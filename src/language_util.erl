%% -*- coding: utf-8 -*-

-module(language_util).

-export([
	translate_subtag/2,
	post_translate_language_tags/2,
	value_or/2
	]).

-include("include/language.hrl").

-type langrec() :: #language{}
	| #extlang{}
	| #script{}
	| #region{}
	| #variant{}
	| #grandfathered{}
	| #redundant{}.

-type singleton() :: tuple(
	singleton,
	Singleton :: string()
	).

-type not_found() :: tuple(
	not_found,
	Subtag :: string()
	).

-type unrecognized() :: tuple(
	unrecognized,
	Subtag :: string()
	).

-type registered_subtag() :: tuple(
	Kind :: subtag_or_tag(),
	Lookup :: string(),
	Detail :: tuple(
		Registered :: string(),
		Prefix :: list(string()),
		Scope :: none | scope(),
		PreferredValue :: none | string(),
		Macrolanguage :: none | string(),
		SuppressScript :: none | string(),
		Description :: list(string()),
		Comment :: list(string()),
		Added :: calendar:date(),
		Deprecated :: none | calendar:date()
		)
	).

-type possible_subtag() :: empty
		| singleton()
		| not_found()
		| unrecognized()
		| registered_subtag().

-spec translate_subtag(
		Subtag :: string(),
		Rec :: langrec()) -> possible_subtag().
translate_subtag(Subtag, [Rec]) ->
	translate_subtag(Subtag, Rec);
translate_subtag(Subtag, Rec) when is_record(Rec, language) ->
	{language, Subtag, {
			Rec#language.subtag,
			none,
			value_or(Rec#language.scope, none),
			value_or(Rec#language.preferred_value, none),
			value_or(Rec#language.macrolanguage, none),
			value_or(Rec#language.suppress_script, none),
			Rec#language.description,
			Rec#language.comment,
			Rec#language.added,
			value_or(Rec#language.deprecated, none)
			}};
translate_subtag(Subtag, Rec) when is_record(Rec, extlang) ->
	{extlang, Subtag, {
			Rec#extlang.subtag,
			Rec#extlang.prefix,
			value_or(Rec#extlang.scope, none),
			value_or(Rec#extlang.preferred_value, none),
			value_or(Rec#extlang.macrolanguage, none),
			value_or(Rec#extlang.suppress_script, none),
			Rec#extlang.description,
			Rec#extlang.comment,
			Rec#extlang.added,
			value_or(Rec#extlang.deprecated, none)
			}};
translate_subtag(Subtag, Rec) when is_record(Rec, script) ->
	{script, Subtag, {
			Rec#script.subtag,
			none,
			none,
			value_or(Rec#script.preferred_value, none),
			none,
			none,
			Rec#script.description,
			Rec#script.comment,
			Rec#script.added,
			value_or(Rec#script.deprecated, none)
			}};
translate_subtag(Subtag, Rec) when is_record(Rec, region) ->
	{region, Subtag, {
			Rec#region.subtag,
			none,
			none,
			value_or(Rec#region.preferred_value, none),
			none,
			none,
			Rec#region.description,
			Rec#region.comment,
			Rec#region.added,
			value_or(Rec#region.deprecated, none)
			}};
translate_subtag(Subtag, Rec) when is_record(Rec, variant) ->
	{variant, Subtag, {
			Rec#variant.subtag,
			Rec#variant.prefix,
			none,
			value_or(Rec#variant.preferred_value, none),
			none,
			none,
			Rec#variant.description,
			Rec#variant.comment,
			Rec#variant.added,
			value_or(Rec#variant.deprecated, none)
			}};
translate_subtag(Subtag, Rec) when is_record(Rec, grandfathered) ->
	{grandfathered, Subtag, {
			Rec#grandfathered.tag,
			none,
			none,
			value_or(Rec#grandfathered.preferred_value, none),
			none,
			none,
			Rec#grandfathered.description,
			Rec#grandfathered.comment,
			Rec#grandfathered.added,
			value_or(Rec#grandfathered.deprecated, none)
			}};
translate_subtag(Subtag, Rec) when is_record(Rec, redundant) ->
	{redundant, Subtag, {
			Rec#redundant.tag,
			none,
			none,
			value_or(Rec#redundant.preferred_value, none),
			none,
			none,
			Rec#redundant.description,
			Rec#redundant.comment,
			Rec#redundant.added,
			value_or(Rec#redundant.deprecated, none)
			}};
translate_subtag([_Char]=Singleton, {not_found, _Other}) ->
	{singleton, Singleton};
translate_subtag([], empty) ->
	empty;
translate_subtag(Subtag, {not_found, _Other}) ->
	{not_found, Subtag};
translate_subtag(Subtag, []) ->
	{unrecognized, Subtag}.

post_translate_language_tags(Subtags, Lookup) ->
	post_translate_language_tags(Subtags, Lookup, 0, []).

post_translate_language_tags([{singleton, [I]},H|T]=Subtags, Lookup, N, Acc)
		when N =:= 0, I =:= $i orelse I =:= $I->
	%% Special case for singleton 'i' in first position,
	%%   test whether it is a grandfathered.
	Id = element(2,H),
	Lower = string:to_lower("i-"++Id),
	case Lookup(Lower) of
		{not_found,_} ->
			collapse_extension_parts(Subtags, Lookup, N, Acc);
		Replacement ->
			post_translate_language_tags(T, Lookup, N+1, [Replacement|Acc])
	end;
post_translate_language_tags([{singleton, [X]=Subtag}|T], Lookup, N, Acc)
		when X =:= $x orelse X =:= $X->
	collapse_private_use(T, Lookup, N, Acc, [Subtag]);
post_translate_language_tags([{singleton, _}|_]=Subtags, Lookup, N, Acc) ->
	collapse_extension_parts(Subtags, Lookup, N, Acc);
post_translate_language_tags([{multiple, _, _}=H|T], Lookup, N, Acc) ->
	case choose_subtag_from_multiple(H, Acc) of
		undecided ->
			post_translate_language_tags(T, Lookup, N+1, [H|Acc]);
		Chosen ->
			post_translate_language_tags(T, Lookup, N+1, [Chosen|Acc])
	end;
post_translate_language_tags([H|T], Lookup, N, Acc) ->
	post_translate_language_tags(T, Lookup, N+1, [H|Acc]);
post_translate_language_tags([], _, _, Acc) ->
	lists:reverse(Acc).

collapse_extension_parts([{singleton, X}|T], Lookup, N, Acc) ->
	collapse_extension_parts(T, Lookup, N, Acc, [X]).

collapse_extension_parts([empty|_]=Subtags, Lookup, N, Acc, Ext) ->
	%% extension tag terminated by encountering an empty (invalid) subtag
	Collapsed = string:join(lists:reverse(Ext), "-"),
	post_translate_language_tags(Subtags, Lookup, N+1, [{extension, Collapsed}|Acc]);
collapse_extension_parts([{singleton, _}|_]=Subtags, Lookup, N, Acc, Ext) ->
	%% extension tag terminated by encountering another singleton
	Collapsed = string:join(lists:reverse(Ext), "-"),
	post_translate_language_tags(Subtags, Lookup, N+1, [{extension, Collapsed}|Acc]);
collapse_extension_parts([H|T], Lookup, N, Acc, Ext) ->
	%% The subtag is just a string when it appears in an extension
	Subtag = take_subtag(H),
	collapse_extension_parts(T, Lookup, N, Acc, [Subtag|Ext]);
collapse_extension_parts([], Lookup, N, Acc, Ext) ->
	%% extension tag terminated by running out of subtags
	Collapsed = string:join(lists:reverse(Ext), "-"),
	post_translate_language_tags([], Lookup, N+1, [{extension, Collapsed}|Acc]).

collapse_private_use([empty|_]=Subtags, Lookup, N, Acc, Ext) ->
	Collapsed = string:join(lists:reverse(Ext), "-"),
	post_translate_language_tags(Subtags, Lookup, N+1, [{privateuse, Collapsed}|Acc]);
collapse_private_use([H|T], Lookup, N, Acc, Ext) ->
	%% The subtag is just a string when it appears in an extension
	Subtag = take_subtag(H),
	collapse_private_use(T, Lookup, N, Acc, [Subtag|Ext]);
collapse_private_use([], Lookup, N, Acc, Ext) ->
	Collapsed = string:join(lists:reverse(Ext), "-"),
	post_translate_language_tags([], Lookup, N+1, [{privateuse, Collapsed}|Acc]).

take_subtag({_, Subtag}) ->
	Subtag;
take_subtag({_, Subtag, _}) ->
	Subtag.

choose_subtag_from_multiple({multiple, _, Subtags}, []) ->
	take_first_subtag_or_exact(Subtags, [language,extlang,script,region,variant]);
choose_subtag_from_multiple({multiple, _, Subtags}, Pre) ->
	case hd(Pre) of
		{Lang, _, _} when Lang =:= language orelse Lang =:= extlang ->
			take_first_subtag_or_exact(Subtags, [extlang, script, region, variant, grandfathered, redundant]);
		{script, _, _} ->
			take_first_subtag_or_exact(Subtags, [region, variant, grandfathered, redundant]);
		_ ->
			take_first_subtag_or_exact(Subtags, [variant, grandfathered, redundant])
	end.

first_matching_subtag_or_tag([{_,Subtag,{T,_,_,_,_,_,_,_,_,_}}=H|T]) when T =:= Subtag ->
	H;
first_matching_subtag_or_tag([_|T]) ->
	first_matching_subtag_or_tag(T);
first_matching_subtag_or_tag([]) ->
	undecided.

take_first_subtag_or_exact(Subtags, Kinds) ->
	case take_first_subtag(Subtags, Kinds) of
		undecided ->
			% Don't know based on position, try exact case match...
			first_matching_subtag_or_tag(Subtags);
		Res -> Res
	end.

take_first_subtag(Subtags, [H|T]) ->
	case lists:keyfind(H, 1, Subtags) of
		false -> take_first_subtag(Subtags, T);
		Res -> Res
	end;
take_first_subtag(_, []) ->
	undecided.

value_or(undefined, Defa) ->
	Defa;
value_or([], Defa) ->
	Defa;
value_or(Val, _) ->
	Val.