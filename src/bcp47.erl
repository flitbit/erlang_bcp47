-module(bcp47).
-author('Phillip Clark <phillip@flitbit.com>').

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([start/0, start_link/0, stop/0]).

-export([get_env/2]).

-export([
  describe/1,
  describe_tag/1,
  list/1,
  list/2
  ]).

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

describe(Subtag) ->
  language_server:describe(Subtag).

describe_tag(Tag) ->
  language_server:describe_each(Tag).

list(Kind) when is_atom(Kind)->
  language_server:list(Kind).

list(Kind, Sort) when is_atom(Kind), is_function(Sort, 2) ->
  language_server:list(Kind, Sort).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
  start_common(),
  bcp47_sup:start_link().

%% @spec start() -> ok
%% @doc Start the bcp47 server.
start() ->
  start_common().

%% @spec stop() -> ok
%% @doc Stop the bcp47 server.
stop() ->
  application:stop(etz),
  application:stop(?MODULE).

get_env(Key, Default) ->
  application:get_env(?MODULE, Key, Default).

%% --------------------------------------------------------------------------------------
%% Private Function Definitions
%% --------------------------------------------------------------------------------------

start_common() ->
  ensure_started(etz),
  ensure_started(inets),
  ensure_started(?MODULE).

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.

