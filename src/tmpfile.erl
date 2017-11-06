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

-type file() :: {ok, string(), File :: file:io_device() | file:fd()}.
-type dir() :: {ok, Dir :: string()}.
-type error_return() :: {error, Reason :: any()}.

-spec mktemp() -> file() | error_return().
%% @doc Calls mktemp(file, []).
mktemp() ->
	mktemp(file, []).

-spec mktemp(file) -> file() | error_return();
	    (dir) -> dir() | error_return().
%% @doc Calls mktemp(Type, []).
mktemp(Type) ->
	mktemp(Type, []).

-spec mktemp(Type :: file, string()) -> file() | error_return();
	    (Type :: {file, [file:mode()]}, string()) -> file() | error_return();
	    (Type :: dir, string()) -> dir() | error_return().
%% @doc Allocates a new temporary file or directory with the given Prefix
%% (followed by a period). If a conflict is encountered in creating the file or
%% directory -- i.e., it already exists -- it will loop until it gets another
%% error or successfully creates the file or directory.
%%
%% Files can be created by passing file, dir, or {file, Modes :: [file:mode()]}
%% for Type.  If using {file, Modes} and Modes does not include the exclusive
%% mode, then it is possible for mktemp to overwrite an existing file of the
%% same name. The likelihood of this should be low, but it isn't impossible.
%%
%% Type file is synonymous with {file, [write, exclusive]}.
mktemp(file, Prefix) ->
	mktemp({file, [write, exclusive]}, Prefix);

mktemp({file, Modes}, Prefix) when is_list(Modes) ->
	Path = filename:join([dir(), temp_name(Prefix)]),
	case file:open(Path, [write, exclusive]) of
		% File already exists -- try opening a file with a different
		% name.
		{error, eexist} -> mktemp(file, Prefix);
		{ok, File} -> {ok, Path, File};
		{error, _} = Error -> Error
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
