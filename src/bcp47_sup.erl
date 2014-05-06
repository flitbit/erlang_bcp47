-module(bcp47_sup).

-behaviour(supervisor).

%% ===================================================================
%% API
%% ===================================================================

-export([start_link/0]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-export([init/1]).


%% ===================================================================
%% Helper macro for declaring children of supervisor
%% ===================================================================

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Config), {I, {I, start_link, [Config]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Defa = config_defaults(),

	CreateMissing = bcp47:get_env(create_missing_paths, proplists:get_value(create_missing_paths, Defa)),
	RegistryPath = bcp47:get_env(registry_path, proplists:get_value(registry_path, Defa)),
	AbsRegistryPath = filename:absname(RegistryPath),

	case CreateMissing of
		true ->
			error_logger:info_msg("~s: starting with registry path: ~s~n", [?MODULE, AbsRegistryPath]),
			%% the last path segment is fake, causes filelib:ensure_dir/1 to create the
			%% directories up to the fake part.
			Fake = filename:join(AbsRegistryPath, "fake"),

			ok = filelib:ensure_dir(Fake);
		_ ->
			error_logger:error_msg("~s: registry path must exist: ~s~n", [?MODULE, AbsRegistryPath]),
			true = filelib:is_dir(AbsRegistryPath)
	end,

	SeedResource = bcp47:get_env(resource, proplists:get_value(resource, Defa)),
	SeedPath = bcp47:get_env(seed_path, proplists:get_value(seed_path, Defa)),

	SelfUpdateIntervalMinutes = bcp47:get_env(self_update_interval_minutes, proplists:get_value(self_update_interval_minutes, Defa)),

	LanguageServer = ?CHILD(language_server, worker, [
				{registry_path, AbsRegistryPath},
				{resource, SeedResource},
				{seed_path, SeedPath},
				{self_update_interval_minutes, SelfUpdateIntervalMinutes}
				]),

	{ok, { {one_for_one, 5, 10}, [LanguageServer]} }.


config_defaults() ->
	[ {create_missing_paths, true},
		{registry_path, "./priv/data/erlang_bcp47/"},
		{resource, "http://www.iana.org/assignments/language-subtag-registry/language-subtag-registry"},
		{seed_path, "./priv/language-subtag-registry.txt"},
		{self_update_interval_minutes, 0}
		].


