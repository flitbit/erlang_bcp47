%% -*- coding: utf-8 -*-

-module(bcp47_parse_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

main_test_() ->
	{	foreach,
		fun setup/0,
		fun cleanup/1,
		[{"bcp47_parse:parse_registry/1 can parse downloaded registry file",
				fun() ->
						Resource = "http://www.iana.org/assignments/language-subtag-registry/language-subtag-registry",
						inets:start(),
						{ok, {{_, 200, _}, _, Bytewise}} = httpc:request(get, {Resource,[{"connection", "close"}]}, [], []),
						%% the body is converted to bytewise list of characters, translate to unicode list
						Bin = list_to_binary(Bytewise),
						UnicodeCharList = unicode:characters_to_list(Bin),
						{ok,_,_} = bcp47_parse:parse_registry(UnicodeCharList)
				end}
			]}.

setup() ->
	case etz:start_link() of
		{ok, Pid} -> Pid;
		{error, {Complaint, _}} ->
			Complaint
	end.

cleanup(Pid) when is_pid(Pid)  ->
	MRef = erlang:monitor(process, Pid),
	gen_server:call(Pid, stop),
	receive {'DOWN', MRef, _, _, _} -> ok end;
cleanup(_) ->
	ok.