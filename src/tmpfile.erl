% Copyright 2017, Noel Cower <ncower@gmail.com>.
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

%% @doc tmpfile is a simple module for handling temporary files and directories
%% in a mostly-Unix-y way. It is not intended as a cross-platform module, so it
%% will only really work on Unix family operating systems (Linux, BSDs, Solaris,
%% etc.).
%%
%% The API surface is primarily `mktemp/0', `mktemp/1', and `mktemp/2' --
%% `mktemp/0' and `mktemp/1' both call `mktemp/2'.
-module(tmpfile).

%% API exports
-export([mktemp/0,
	 mktemp/1,
	 mktemp/2,
	 dir/0
	]).

-define(DEFAULT_TEMP_PREFIX, "erltmp").
-define(DEFAULT_TEMP_DIR_UNIX, "/tmp").

%%====================================================================
%% API functions
%%====================================================================

-spec dir() -> string() | no_return().
%% @doc Returns the directory that temporary files and directories are created
%% under. If called from an unsupported operating system type and TMPDIR is not
%% set, dir/0 throws an `unsupported' error.
dir() ->
	{Family, _} = os:type(),
	dir2(os:getenv("TMPDIR"), Family).

-spec dir2(Tmpdir :: false | string(), Family :: atom()) -> string() | no_return().
%% @doc Returns the directory that temporary files and directories are created
%% under based on the `TMPDIR' environment variable and OS family (as returned
%% by `os:type/0').
dir2(false, Family) ->
	dir2([], Family);
dir2([], unix) ->
	"/tmp";
dir2([], _) ->
	error(unsupported);
dir2(Tmpdir, _) ->
	Tmpdir.

-type modes() :: [file:mode() | touch].

-type file() :: {ok, Path :: string(), File :: file:io_device() | file:fd()}
	| {ok, Path :: string()}.
-type dir() :: {ok, Dir :: string()}.
-type error_return() :: {error, Reason :: any()}.

-define(DEFAULT_FILE_TYPE, {file, [write]}).

-spec mktemp() -> file() | error_return().
%% @doc Calls `mktemp({file, [write]}, [])'.
mktemp() ->
	mktemp(?DEFAULT_FILE_TYPE, []).

-spec mktemp(Type :: file) -> file() | error_return();
	    (Type :: {file, modes()}) -> file() | error_return();
	    (Type :: dir) -> dir() | error_return().
%% @doc Calls `mktemp(Type, [])'.
mktemp(Type) ->
	mktemp(Type, []).

-spec mktemp(Type :: file, Prefix :: string()) -> file() | error_return();
	    (Type :: {file, modes()}, Prefix :: string()) -> file() | error_return();
	    (Type :: dir, Prefix :: string()) -> dir() | error_return().
%% @doc Allocates a new temporary file or directory with the given Prefix
%% (followed by a period). If a conflict is encountered in creating the file or
%% directory -- i.e., it already exists -- it will loop until it gets another
%% error or successfully creates the file or directory.
%%
%% Files can be created by passing file, dir, or `{file, Modes}' for Type.
%% If using `{file, Modes}', the `exclusive' mode is always added to the Modes
%% list. If the file's Modes contains `touch', the file is opened and then
%% closed immediately and only `{ok, Path}' are returned.
%%
%% Type `file' is synonymous with `{file, [write, exclusive]}'.
%%
%% If, somehow, a filename (or directory name) conflicts with an existing file
%% or directory, mktemp will retry, generating a new name and attempting again
%% to create a file. This is only done for eexist file errors -- there is no
%% other handling around errors creating files and directories.
%%
%% The filename is always formatted as `Prefix.Counter.Time.Random'.
mktemp(file, Prefix) ->
	mktemp(?DEFAULT_FILE_TYPE, Prefix);

mktemp({file, Modes}, Prefix) when is_list(Modes) ->
	Path = filename:join([dir(), temp_name(Prefix)]),
	Touch = should_close(Modes),
	case file:open(Path, [exclusive | file_modes(Modes)]) of
		% File already exists -- try opening a file with a different
		% name.
		{error, eexist} ->
			mktemp(file, Prefix);
		{ok, File} when Touch =:= false ->
			{ok, Path, File};
		{ok, File} when Touch =:= true ->
			ok = file:close(File),
			{ok, Path};
		{error, _} = Error ->
			Error
	end;

mktemp(dir, Prefix) ->
	Path = filename:join([dir(), temp_name(Prefix)]),
	case file:make_dir(Path) of
		% Dir already exists -- try a new name.
		{error, eexist} -> mktemp(dir, Prefix);
		% Return either a path or an error
		ok -> {ok, Path};
		{error, _} = Err -> Err
	end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec file_modes(Modes :: modes()) -> [file:mode()].
file_modes(Modes) ->
	file_modes(Modes, []).

-spec file_modes(Modes :: modes(), Acc :: [file:mode()]) -> [file:mode()].
file_modes([], Acc) ->
	Acc;
file_modes([touch|Tail], Acc) ->
	file_modes(Tail, Acc);
file_modes([H|Tail], Acc) ->
	file_modes(Tail, [H|Acc]).

-spec should_close(modes()) -> boolean().
should_close([]) ->
	false;
should_close([touch|_]) ->
	true;
should_close([_|Tail]) ->
	should_close(Tail).

-spec pos_monotonic_time() -> pos_integer().
%% @doc pos_monotonic_time returns a positive monotonic time.
pos_monotonic_time() ->
	1 + (erlang:monotonic_time() - erlang:system_info(start_time)).

-spec temp_integers() ->
	{
	 Counter :: pos_integer(),
	 TimeMS :: pos_integer(),
	 Rand :: pos_integer()
	}.
%% @doc Generates and returns the three integers used by temp_name to uniquely
%% identify temp files. It includes a positive unique integer, a positive native
%% timestamp (the unit is defined by the operating system), and a random,
%% positive 64-bit unsigned integer.
temp_integers() ->
	{
	 erlang:unique_integer([positive]),
	 pos_monotonic_time(),
	 rand:uniform(16#FFFFFFFFFFFFFFF)
	}.

-spec temp_name(Prefix :: string()) -> string().
%% @doc Generates a filename with a given Prefix. If the Prefix is the empty
%% list, then it uses the module's default temp file prefix ("erltmp").
%%
%% The filename is always formed as Prefix.Counter.Time.Random, where Counter,
%% Time, and Random are base-36 integers.
temp_name([]) ->
	temp_name(?DEFAULT_TEMP_PREFIX);
temp_name(Prefix) when is_list(Prefix) ->
	{Counter, Time, RandInt} = temp_integers(),
	lists:append([
		      Prefix,
		      [$.|integer_to_list(Counter, 36)],
		      [$.|integer_to_list(Time, 36)],
		      [$.|integer_to_list(RandInt, 36)]
		     ]).
