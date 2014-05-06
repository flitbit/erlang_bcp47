%% -*- coding: utf-8 -*-

-module(bcp47_parse).

-export([
	parse_registry/1
	]).

-include("include/language.hrl").

-define(SCOPE, "Scope").
-define(FILE_DATE, "File-Date").
-define(SUBTAG, "Subtag").
-define(TAG, "Tag").
-define(TYPE, "Type").
-define(MACROLANGUAGE, "Macrolanguage").
-define(SUPPRESS_SCRIPT, "Suppress-Script").
-define(PREFERRED_VALUE, "Preferred-Value").
-define(DESCRIPTION, "Description").
-define(DEPRECATED, "Deprecated").
-define(COMMENT, "Comment").
-define(ADDED, "Added").

-record(kv, {
		key :: string(),
		value :: string()
		}).

-record(tag, {
		fields :: list(#kv{})
		}).

parse_registry(Bin) when is_binary(Bin) ->
	parse_registry(unicode:characters_to_list(Bin));
parse_registry(Body) when is_list(Body) ->
	Lines = string:tokens(Body, "\n"),
	{ok, ParseLines} = cat_continuation_lines(Lines),
	{ok, Subtags} = parse_language_tags(ParseLines),
	make_langrecs(Subtags).

make_langrecs([]) ->
	{error, no_data};
make_langrecs(Subtags) ->
	% File-Date should be present in the first record.
	FileDate = hd(Subtags),
	case take_date_value(FileDate, ?FILE_DATE) of
		undefined ->
			{error, {missing_data, ?FILE_DATE}};
		Date ->
			Recs = make_langrecs(tl(Subtags), []),
			{ok, Date, Recs}
	end.

make_langrecs([H|Rest], Acc) ->
	Type = take_single_value_as_atom(H, ?TYPE),
	Rec = make_record(Type, H),
	make_langrecs(Rest, [Rec| Acc]);
make_langrecs([], Acc) ->
	lists:reverse(Acc).

make_record(language, Data) ->
	Subtag = take_single_value(Data, ?SUBTAG),
	#language{
		subtag = Subtag,
		type = language,
		scope = take_single_value_as_atom(Data, ?SCOPE),
		macrolanguage= take_single_value(Data, ?MACROLANGUAGE),
		suppress_script= take_single_value(Data, ?SUPPRESS_SCRIPT),
		preferred_value= take_single_value(Data, ?PREFERRED_VALUE),
		description = take_multiple_values(Data, ?DESCRIPTION),
		comment = take_multiple_values(Data, ?COMMENT),
		added = take_date_value(Data, ?ADDED),
		deprecated = take_date_value(Data, ?DEPRECATED)
		};
make_record(extlang, Data) ->
	Subtag = take_single_value(Data, ?SUBTAG),
	#extlang{
		subtag = Subtag,
		type = extlang,
		scope = take_single_value_as_atom(Data, ?SCOPE),
		prefix = take_single_value(Data, "Prefix"),
		macrolanguage= take_single_value(Data, ?MACROLANGUAGE),
		suppress_script= take_single_value(Data, ?SUPPRESS_SCRIPT),
		preferred_value= take_single_value(Data, ?PREFERRED_VALUE),
		description = take_multiple_values(Data, ?DESCRIPTION),
		comment = take_multiple_values(Data, ?COMMENT),
		added = take_date_value(Data, ?ADDED),
		deprecated = take_date_value(Data, ?DEPRECATED)
		};
make_record(script, Data) ->
	Subtag = take_single_value(Data, ?SUBTAG),
	#script{
		subtag = Subtag,
		type = script,
		preferred_value= take_single_value(Data, ?PREFERRED_VALUE),
		description = take_multiple_values(Data, ?DESCRIPTION),
		comment = take_multiple_values(Data, ?COMMENT),
		added = take_date_value(Data, ?ADDED),
		deprecated = take_date_value(Data, ?DEPRECATED)
		};
make_record(region, Data) ->
	Subtag = take_single_value(Data, ?SUBTAG),
	#region{
		subtag = Subtag,
		type = region,
		preferred_value= take_single_value(Data, ?PREFERRED_VALUE),
		description = take_multiple_values(Data, ?DESCRIPTION),
		comment = take_multiple_values(Data, ?COMMENT),
		added = take_date_value(Data, ?ADDED),
		deprecated = take_date_value(Data, ?DEPRECATED)
		};
make_record(variant, Data) ->
	Subtag = take_single_value(Data, ?SUBTAG),
	#variant{
		subtag = Subtag,
		type = variant,
		prefix = take_single_value(Data, "Prefix"),
		preferred_value= take_single_value(Data, ?PREFERRED_VALUE),
		description = take_multiple_values(Data, ?DESCRIPTION),
		comment = take_multiple_values(Data, ?COMMENT),
		added = take_date_value(Data, ?ADDED),
		deprecated = take_date_value(Data, ?DEPRECATED)
		};
make_record(grandfathered, Data) ->
	Tag = take_single_value(Data, ?TAG),
	#grandfathered{
		tag = Tag,
		type = grandfathered,
		preferred_value= take_single_value(Data, ?PREFERRED_VALUE),
		description = take_multiple_values(Data, ?DESCRIPTION),
		comment = take_multiple_values(Data, ?COMMENT),
		added = take_date_value(Data, ?ADDED),
		deprecated = take_date_value(Data, ?DEPRECATED)
		};
make_record(redundant, Data) ->
	Tag = take_single_value(Data, ?TAG),
	#redundant{
		tag = Tag,
		type = redundant,
		preferred_value= take_single_value(Data, ?PREFERRED_VALUE),
		description = take_multiple_values(Data, ?DESCRIPTION),
		comment = take_multiple_values(Data, ?COMMENT),
		added = take_date_value(Data, ?ADDED),
		deprecated = take_date_value(Data, ?DEPRECATED)
		}.

take_date_value(Tag, Field) ->
	case take_single_value(Tag, Field) of
		V when is_list(V) ->
			case etz:iso_parse(V) of
				{ok, {{Date, _}, _, _}} -> Date;
				_ -> undefined
			end;
		_ -> undefined
	end.

take_multiple_values(#tag{fields=F}, Field) ->
	Fun = fun(#kv{key=K,value=V}) ->
			case K of
				Field -> {true, V};
				_ -> false
			end
	end,
	lists:filtermap(Fun, F).

take_single_value(#tag{fields=F}, Field) ->
	case lists:keyfind(Field, #kv.key, F) of
		#kv{value=V} -> V;
		_ -> undefined
	end.

take_single_value_as_atom(Tag, Field) ->
	case take_single_value(Tag, Field) of
		V when is_list(V) -> list_to_atom(V);
		_ -> undefined
	end.

parse_language_tags(L) when is_list(L) ->
	{ok, MP} = re:compile(":\\s", [unicode]),
	parse_language_tags(L, MP, []).

parse_language_tags([H|T], MP, Acc) ->
	parse_language_tag([H|T], MP, Acc, #tag{fields=[]});
parse_language_tags([], _, Acc) ->
	{ok, lists:reverse(Acc)}.

parse_language_tag(["%%"|T], MP, Acc, Tag) ->
	Fields = lists:reverse(Tag#tag.fields),
	ResultTag = Tag#tag{fields=Fields},
	parse_language_tags(T, MP, [ResultTag|Acc]);
parse_language_tag([H|T], MP, Acc, Tag) ->
	case re:run(H, MP) of
		{match, [{B, E}]} ->
			{K, Rest} = lists:split(B, H),
			{_, V} = lists:split(E, Rest),
			Field = #kv{key=K,value=V},
			parse_language_tag(T, MP, Acc, Tag#tag{fields=[Field|Tag#tag.fields]});
		_ -> parse_language_tag(T, MP, Acc, Tag)
	end;
parse_language_tag([], MP, Acc, Tag) ->
	Fields = lists:reverse(Tag#tag.fields),
	ResultTag = Tag#tag{fields=Fields},
	parse_language_tags([], MP, [ResultTag|Acc]).

cat_continuation_lines(Lines) ->
	{ok, Desc} = re:compile("^Description:", [unicode]),
	{ok, Com} = re:compile("^Comments:", [unicode]),
	{ok, Sp} = re:compile("^\\s", [unicode]),
	cat_continuation_lines(Lines, Desc, Com, Sp, []).

%% @doc Inspects lines and concats wrapped descriptions and comments into single lines.
cat_continuation_lines([H|T], D, C, S, Acc) ->
	case re:run(H, D) of
		{match, _} ->
			{ok, Rest, NewAcc} = take_continuations(T, [H], S, Acc),
			cat_continuation_lines(Rest, D, C, S, NewAcc);
		nomatch ->
			case re:run(H, C) of
				{match, _} ->
					{ok, Rest, NewAcc} = take_continuations(T, [H], S, Acc),
					cat_continuation_lines(Rest, D, C, S, NewAcc);
				nomatch ->
					cat_continuation_lines(T, D, C, S, [H|Acc])
			end
	end;
cat_continuation_lines([], _, _, _, Acc) ->
	{ok, lists:reverse(Acc)}.

take_continuations([H|T], L, S, Acc) ->
	case re:run(H, S) of
		{match, [{_, E}]} ->
			% replace leading whitespace with one space char...
			{_, Addl} = lists:split(E, H),
			take_continuations(T, [[" "| Addl]|L], S, Acc);
		nomatch ->
			Line = lists:flatten(lists:reverse(L)),
			{ok, [H|T], [Line | Acc]}
	end;
take_continuations([], L, _, Acc) ->
	Line = lists:flatten(lists:reverse(L)),
	{ok, [], [Line | Acc]}.
