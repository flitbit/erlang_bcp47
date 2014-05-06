%% -*- coding: utf-8 -*-

-module(language_server).

-behaviour(gen_server).

-define(SRV, ?MODULE).

-export([start_link/1,
    describe/1,
    describe_each/1,
    list/1,
    list/2
    ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-include("include/language.hrl").

-define(UTC, {'+', 0, 0 }).
-define(SECONDS(Minutes), Minutes * 60).

-record(tables, {
        language,
        extlang,
        script,
        region,
        variant,
        grandfathered,
        redundant,
        housekeeping
        }).

-record(state, {
        tables :: #tables{},
        index,
        resource,
        interval
        }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Props) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Props, []).

describe(Subtag) when is_binary(Subtag) ->
    describe(binary_to_list(Subtag));
describe(Subtag) when is_list(Subtag) ->
    gen_server:call(?SRV,{describe,Subtag}).

describe_each(Subtags) when is_list(Subtags) ->
    gen_server:call(?SRV,{describe_each, Subtags}).

list(Kind) when is_atom(Kind) ->
    gen_server:call(?SRV,{list_kind, Kind, undefined}).

list(Kind, Sort) when is_atom(Kind), is_function(Sort, 2) ->
    gen_server:call(?SRV,{list_kind, Kind, Sort}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Props) ->
    RegistryPath = proplists:get_value(registry_path, Props),
    SeedPath = proplists:get_value(seed_path, Props),
    Resource = proplists:get_value(resource, Props),
    SelfUpdateIntervalMinutes = proplists:get_value(self_update_interval_minutes, Props),

    require_path_exists(RegistryPath, registry_path),

    Tables = #tables{
            language = open_data_file(RegistryPath, bcp47_language, "language.dets"),
            extlang = open_data_file(RegistryPath, bcp47_extlang, "extlang.dets"),
            script = open_data_file(RegistryPath, bcp47_script, "script.dets"),
            region = open_data_file(RegistryPath, bcp47_region, "region.dets"),
            variant = open_data_file(RegistryPath, bcp47_variant, "variant.dets"),
            grandfathered = open_data_file(RegistryPath, bcp47_grandfathered, "grandfathered.dets"),
            redundant = open_data_file(RegistryPath, bcp47_redundant, "redundant.dets"),
            housekeeping = open_data_file(RegistryPath, bcp47_housekeeping, "housekeeping.dets")
            },
    IndexTable =
        ets:new(language_index_table,[set,named_table,protected]),

    {ok, _, {LastCheck,_,_}, _} = check_load_empty_languages(Tables, IndexTable, SeedPath, Resource),
    MostRecent = calendar:datetime_to_gregorian_seconds(LastCheck),
    SelfUpdateIntervalSeconds = ?SECONDS(SelfUpdateIntervalMinutes),
    possibly_schedule_self_update(SelfUpdateIntervalSeconds, MostRecent),
    {ok, #state{
            tables=Tables,
            index=IndexTable,
            resource=Resource,
            interval=SelfUpdateIntervalSeconds
            }}.

handle_call({describe, Subtag}, _From, State) ->
    Reply = query_subtag(
            State#state.tables,
            State#state.index,
            Subtag),
    {reply, Reply, State};
handle_call({describe_each, Subtags}, _From, State) ->
    Reply = query_subtags(
            State#state.tables,
            State#state.index,
            Subtags),
    {reply, Reply, State};
handle_call({list_kind, Kind, Sort}, _From, State) ->
    Reply = list_kind(
            State#state.tables,
            State#state.index,
            Kind,
            Sort),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(self_update_interval, State) ->
    possibly_self_update_from_iana(
        State#state.tables,
        State#state.index,
        State#state.resource),
    erlang:send_after(State#state.interval*1000, self(), self_update_interval),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open_data_file(Path, TableName, FileName) ->
    TablePath = filename:join(Path, FileName),
    case dets:open_file(TableName,[
                {keypos, 2},
                {type, set},
                {file, TablePath}
                ]) of
        {ok, Name} -> Name;
        {error, Reason} ->
            error_logger:info_msg("~s: unable to open data file ~s~n", [?MODULE, TablePath]),
            exit({error, Reason})
    end.

list_kind(Tables, _ndex, Kind, undefined) ->
    Table = table_for(Tables, Kind),
    All = dets:foldl(
            fun(Data, Acc) ->
                    It = language_util:translate_subtag(
                            string:to_lower(element(2,Data)),
                            Data),
                    [It|Acc]
            end, [], Table),
    lists:sort(All);
list_kind(Tables, _ndex, Kind, Sort) ->
    Table = table_for(Tables, Kind),
    All = dets:foldl(
            fun(Data, Acc) ->
                    It = language_util:translate_subtag(
                            string:to_lower(element(2,Data)),
                            Data),
                    [It|Acc]
            end, [], Table),
    lists:sort(Sort, All).

query_subtag(_, _, [C]) when C =:= $$ ->
    empty;
query_subtag(Tables, Index, Subtag) ->
    Lower = string:to_lower(Subtag),
    Look = fun({Kind, Key}) ->
            Table = table_for(Tables, Kind),
            case dets:lookup(Table, Key) of
                [] -> {not_found, Subtag};
                Data -> language_util:translate_subtag(Subtag, Data)
            end
    end,
    case ets:lookup(Index, Lower) of
        [{_,[One]}] ->
            Look(One);
        [{_,Many}] ->
            Subtags = lists:reverse(lists:map(Look, Many)),
            {multiple, Subtag, Subtags};
        [] -> language_util:translate_subtag(Subtag, {not_found, Subtag})
    end.

table_for(Tables,language) -> Tables#tables.language;
table_for(Tables,extlang) -> Tables#tables.extlang;
table_for(Tables,script) -> Tables#tables.script;
table_for(Tables,region) -> Tables#tables.region;
table_for(Tables,variant) -> Tables#tables.variant;
table_for(Tables,grandfathered) -> Tables#tables.grandfathered;
table_for(Tables,redundant) -> Tables#tables.redundant.

query_subtags(Table, Index, Subtags) ->
    Tokens = tokenize_language_tag(Subtags),
    case length(Tokens) of
        1 ->
            [query_subtag(Table, Index, Subtags)];
        N when N < 4 ->
            case query_subtag(Table, Index, Subtags) of
                {not_found, _} ->
                    query_subtags(Table, Index, Tokens, []);
                PossiblyRedundant ->
                    expand_if_redundant(Table, Index, Tokens, PossiblyRedundant)
            end;
        _ ->
            query_subtags(Table, Index, Tokens, [])
    end.

query_subtags(Table, Index, [H|T], Acc) ->
    Subtag = query_subtag(Table, Index, H),
    query_subtags(Table, Index, T, [Subtag | Acc]);
query_subtags(Table, Index, [], Acc) ->
    List = lists:reverse(Acc),
    language_util:post_translate_language_tags(List,
        fun(Subtag) ->
                query_subtag(Table, Index, Subtag)
        end).

expand_if_redundant(Table, Index, Tokens, {redundant, _, _}) ->
    query_subtags(Table, Index, Tokens, []);
expand_if_redundant(_, _, _, NonRedundant) ->
    [NonRedundant].

tokenize_language_tag([$-|_]=Tag) ->
    %% special case, token at front.
    New = string:concat("$", Tag),
    tokenize_language_tag(New);
tokenize_language_tag(Tag) ->
    case lists:last(Tag) of
        $- ->
            %% special case, token at front.
            New = string:concat(Tag, "$"),
            replace_repeating_delimiters(New);
        _ ->
            replace_repeating_delimiters(Tag)
    end.

replace_repeating_delimiters(Tag) ->
    %% markup empty tags since erlang's string:tokens will
    %% simply collapse them... we need to know where they
    %% occur.
    case string:str(Tag, "--") of
        0 -> string:tokens(Tag, "-");
        N ->
            Left = string:left(Tag, N),
            Right = string:sub_string(Tag, N+1),
            New = string:concat(Left, string:concat("$", Right)),
            replace_repeating_delimiters(New)
    end.

possibly_schedule_self_update(IntervalSeconds, _) when IntervalSeconds =< 0 ->
    ok;
possibly_schedule_self_update(IntervalSeconds, MruGregorianSeconds) ->
    NowGregorianSeconds = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    case (NowGregorianSeconds - MruGregorianSeconds) of
        Exp when Exp >= IntervalSeconds ->
            erlang:send_after(0, self(), self_update_interval);
        Elapsed ->
            Rmn = IntervalSeconds - Elapsed,
            erlang:send_after(Rmn * 1000, self(), self_update_interval)
    end.

possibly_self_update_from_iana(Tables, Index, Resource) ->
    {ok, InetsPid} = inets:start(httpc, [{profile, bcp47_self_update}]),
    Res = case on_self_update_head_response(Tables,
            Index,
            Resource,
            httpc:request(head, {Resource,[{"connection", "close"}]}, [], [], InetsPid),
            InetsPid
            ) of
        unneccesary -> ok;
        {ok, FileDate, _, Count} ->
            error_logger:info_msg("~s: language registry updated with ~w records published on ~s~n", [?MODULE, Count, format_date(FileDate)]),
            ok;
        {ok, {{_, HttpStatusCode, ReasonPhrase}, _, _}} ->
            error_logger:info_msg("~s: HTTP request failed: ~s - ~s~n", [?MODULE, HttpStatusCode, ReasonPhrase]),
            {error, {request_failed, Resource, {response, {HttpStatusCode, ReasonPhrase}}}};
        Error ->
            error_logger:info_msg("~s: error while updating language registry: ~p~n", [?MODULE, Error]),
            Error
    end,
    inets:stop(httpc, InetsPid),
    Res.

on_self_update_head_response(Tables, Index, Resource, {ok, {{_, 200, _}, Headers, _}}, InetsPid) ->
    {ok, FileDate} = query_most_recent(Tables#tables.housekeeping, filetime),
    LastMod = proplists:get_value("last-modified", Headers),
    {ResourceDate, _} = httpd_util:convert_request_date(LastMod),
    % Convert the dates to gregorian days, update if the web resource is newer.
    %     BCP 47, Section 3.1, paragraph 8 defines the "File-Date" record
    %     and in my opinion [Phillip] indicates that the registry will not be
    %     updated more than once in the same calendar day; making this
    %     implementation safe.
    FileDays = calendar:date_to_gregorian_days(FileDate),
    ResourceDays = calendar:date_to_gregorian_days(ResourceDate),
    self_update_if_resource_is_newer(FileDays, ResourceDays, Tables, Index, Resource, InetsPid);
on_self_update_head_response(_, _, _, Other, _) ->
    Other.

self_update_if_resource_is_newer(FileDays, ResourceDays, Tables, Index, Resource, InetsPid)
        when ResourceDays > FileDays ->
    error_logger:info_msg("~s: Updates exist on the IANA langage registry: requesting updates~n", [?MODULE]),
    on_self_update_get_response(Tables, Index,
        httpc:request(get, {Resource,[{"connection", "close"}]}, [], [], InetsPid),
        true
        );
self_update_if_resource_is_newer(_, _, _, _, _, _) ->
    unneccesary.

on_self_update_get_response(Tables, Index, {ok, {{_, 200, _}, _, Body}}, DeleteExisting) ->
    %% Body is a `Bytewise` encoded list, translate it to a unicode character list:
    Unibody = unicode:characters_to_list(list_to_binary(Body)),
    update_all_objects_on_successful_parse(Tables, Index, Unibody, DeleteExisting);
on_self_update_get_response(_, _, Other, _) ->
    Other.

add_key_to_index(Index,Table, Id) ->
    Key = string:to_lower(Id),
    case ets:lookup(Index,Key) of
        [{Key, List}] ->
            ets:insert(Index, {Key, [{Table, Id}|List]});
        [] ->
            ets:insert(Index, {Key, [{Table, Id}]})
    end,
    Id.

update_index(Index,Record) ->
    Table = element(1, Record),
    Key = element(2, Record),
    add_key_to_index(Index, Table, Key).

init_index_from_data(Tables, Index) ->
    init_index_from_data_table(Tables, language, Index),
    init_index_from_data_table(Tables, extlang, Index),
    init_index_from_data_table(Tables, script, Index),
    init_index_from_data_table(Tables, region, Index),
    init_index_from_data_table(Tables, variant, Index),
    init_index_from_data_table(Tables, grandfathered, Index),
    init_index_from_data_table(Tables, redundant, Index).

init_index_from_data_table(Tables, Kind, Index) ->
    Table = table_for(Tables, Kind),
    First = dets:first(Table),
    init_index_from_data(Table, Kind, Index,First).

init_index_from_data(_, _, _, '$end_of_table') ->
    ok;
init_index_from_data(Table, Kind, Index, Key) ->
    add_key_to_index(Index, Kind, Key),
    Next = dets:next(Table, Key),
    init_index_from_data(Table, Kind, Index, Next).

update_all_objects_on_successful_parse(Tables, Index, Body, DeleteExisting) ->
    {ok, FileDate, Records} = bcp47_parse:parse_registry(Body),
    case DeleteExisting of
        true -> delete_all_objects(Tables, Index);
        _ -> ok
    end,
    lists:foreach(fun(R) -> insert_record(Tables, Index, R) end, Records),
    ok = insert_most_recent(Tables#tables.housekeeping, filetime, FileDate),
    ResourceCheckTs = etz:now(?UTC),
    ok = insert_most_recent(Tables#tables.housekeeping, resource_check, ResourceCheckTs),
    Count = length(Records),
    {ok, FileDate, ResourceCheckTs, Count}.

insert_record(Tables, Index, R) when is_record(R, language) ->
    dets:insert(Tables#tables.language, R),
    update_index(Index, R);
insert_record(Tables, Index, R) when is_record(R, extlang) ->
    dets:insert(Tables#tables.extlang, R),
    update_index(Index, R);
insert_record(Tables, Index, R) when is_record(R, script) ->
    dets:insert(Tables#tables.script, R),
    update_index(Index, R);
insert_record(Tables, Index, R) when is_record(R, region) ->
    dets:insert(Tables#tables.region, R),
    update_index(Index, R);
insert_record(Tables, Index, R) when is_record(R, variant) ->
    dets:insert(Tables#tables.variant, R),
    update_index(Index, R);
insert_record(Tables, Index, R) when is_record(R, grandfathered) ->
    dets:insert(Tables#tables.grandfathered, R),
    update_index(Index, R);
insert_record(Tables, Index, R) when is_record(R, redundant) ->
    dets:insert(Tables#tables.redundant, R),
    update_index(Index, R).

delete_all_objects(Tables, Index) ->
    dets:delete_all_objects(Tables#tables.language),
    dets:delete_all_objects(Tables#tables.extlang),
    dets:delete_all_objects(Tables#tables.script),
    dets:delete_all_objects(Tables#tables.region),
    dets:delete_all_objects(Tables#tables.variant),
    dets:delete_all_objects(Tables#tables.grandfathered),
    dets:delete_all_objects(Tables#tables.redundant),
    ets:delete_all_objects(Index).

require_path_exists(Path, What) ->
    case filelib:is_dir(Path) of
        true -> ok;
        _ ->
            error_logger:error_msg("~s: required path missing: ~s~n", [?MODULE, What]),
            exit({error, {file_path_missing, {What, Path}}})
    end.

check_load_empty_languages(Tables, IndexTable, SeedPath, Resource) ->
    case dets:info(Tables#tables.language, no_keys) of
        0 ->
            case file:read_file(SeedPath) of
                {ok, Bin} ->
                    error_logger:info_msg("~s: seeding registry data from local file: ~s~n", [?MODULE, SeedPath]),
                    %% file module only reads/returns binary, translate to unicode
                    Unicode = unicode:characters_to_list(Bin),
                    finish_load_from_seed_file(Tables, IndexTable, Unicode);
                _ ->
                    load_iana_data(Tables, IndexTable, Resource)
            end;
        Count when is_integer(Count) ->
            case query_most_recent(Tables#tables.housekeeping, filetime) of
                {ok, FileDate2} ->
                    error_logger:info_msg("~s: registry present with ~w records published on ~s~n", [?MODULE, Count, format_date(FileDate2)]),
                    case query_most_recent(Tables#tables.housekeeping, resource_check) of
                        {ok, ResourceCheckTs2} ->
                            error_logger:info_msg("~s: registry freshness last checked at ~s~n", [?MODULE, etz:iso_format(ResourceCheckTs2)]),
                            init_index_from_data(Tables, IndexTable),
                            {ok, FileDate2, ResourceCheckTs2, Count};
                        _ ->
                            exit({error, {most_recent_file_time_unknown, Tables#tables.housekeeping}})
                    end;
                _ ->
                    exit({error, {most_recent_file_time_unknown, Tables#tables.housekeeping}})
            end
    end.

insert_most_recent(Table, Key, Value) ->
    dets:insert(Table, {most_recent, Key, Value}).

query_most_recent(Table, Key) ->
    case dets:lookup(Table, Key) of
        [{most_recent, Key, Value}] -> {ok, Value};
        _ -> undefined
    end.

finish_load_from_seed_file(Tables, IndexTable, Bin) ->
    case update_all_objects_on_successful_parse(Tables, IndexTable, Bin, false) of
        {ok, FileDate, _, Count} = Ok ->
            error_logger:info_msg("~s: registry seeded with ~w records published on ~s~n", [?MODULE, Count, format_date(FileDate)]),
            Ok;
        {error, Error} ->
            exit({error, Error})
    end.

load_iana_data(Tables, IndexTable, Resource) ->
    {ok, InetsPid} = inets:start(httpc, [{profile, bcp47_self_update}]),
    error_logger:info_msg("~s: requesting IANA language registry over HTTP~n", [?MODULE]),
    case on_self_update_get_response(Tables, IndexTable,
            httpc:request(get, {Resource,[{"connection", "close"}]}, [], [], InetsPid),
            false
            ) of
        {ok, FileDate, _, Count} = Ok ->
            error_logger:info_msg("~s: registry seeded from IANA language registry with ~w records published on ~s~n", [?MODULE, Count, format_date(FileDate)]),
            inets:stop(httpc, InetsPid),
            Ok;
        {ok, {{_, HttpStatusCode, ReasonPrase}, _, _}} ->
            exit({error, {request_iana_data_failed, Resource, {HttpStatusCode, ReasonPrase}}});
        {error, Error} ->
            exit({error, {request_iana_data_failed, Resource, Error}})
    end.

format_date({Y,M,D})
        when is_integer(Y) andalso is_integer(M) andalso is_integer(D) ->
    FmtStr = "~4.10.0B-~2.10.0B-~2.10.0B",
    IsoStr = io_lib:format(FmtStr, [Y, M, D]),
    list_to_binary(IsoStr).



