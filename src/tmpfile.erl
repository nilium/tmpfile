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

%%====================================================================
%% API functions
%%====================================================================

-spec dir() -> string().
%% @doc Returns the directory that temporary files and directories are created
%% under.
dir() ->
	% TODO: Actually determine where to put tempfiles in a better manner.
	% Probably check the os family and then run mktemp or something to get
	% the base directory.
	"/tmp".

-type file() :: {ok, File :: file:io_device()}.
-type dir() :: {ok, Dir :: string()}.
-type error_return() :: {error, Reason :: any()}.

-spec mktemp() -> file() | error_return().
%% @doc Calls mktemp(file, ?DEFAULT_TEMP_PREFIX).
mktemp() ->
	mktemp(file, ?DEFAULT_TEMP_PREFIX).

-spec mktemp(file) -> file() | error_return();
            (dir) -> dir() | error_return().
%% @doc Calls mktemp(Type, ?DEFAULT_TEMP_PREFIX).
mktemp(Type) ->
	mktemp(Type, ?DEFAULT_TEMP_PREFIX).

-spec mktemp(file, string()) -> file() | error_return();
            (dir, string()) -> dir() | error_return().
mktemp(file, Prefix) ->
	Path = filename:join([dir(), temp_name(Prefix)]),
	file:open(Path, [write, exclusive]);

mktemp(dir, Prefix) ->
	Path = filename:join([dir(), temp_name(Prefix)]),
	case file:make_dir(Path) of
		ok -> {ok, Path};
		{error, _} = Err -> Err
	end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec pos_monotonic_time() -> pos_integer().
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
