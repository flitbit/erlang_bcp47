%% -*- coding: utf-8 -*-

-module(language_server_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

main_test_() ->
	{ setup,
		fun setup/0,
		fun cleanup/1,
		[{"bcp47:describe/1 can describe well-known subtag `en`",
				fun() ->
						{language, SearchSubtag, {RegisteredSubtag, _, _, _, _, _, _, _, _, _}} = bcp47:describe("en"),
						?assertEqual("en", SearchSubtag),
						?assertEqual("en", RegisteredSubtag)
				end},
			{"bcp47:describe/1 can describe well-known subtag when case differs `En`",
				fun() ->
						{language, SearchSubtag, {RegisteredSubtag, _, _, _, _, _, _, _, _, _}} = bcp47:describe("En"),
						?assertEqual("En", SearchSubtag),
						?assertEqual("en", RegisteredSubtag)
				end},

			{"bcp47:describe_tag/1 decodes language tags appearing in the spec's text",
				%% These tags are taken from the text of RFC 5646. I didn't take all of them
				%% since many appear as single subtags or are `irregular` or `regular` productions
				%% tested further below.
				[
					fun() -> ?assertMatch([
									{language, "mn", {"mn",_,_,_,_,_,["Mongolian"],_,_,_}},
									{script, "cyrl", {"Cyrl",_,_,_,_,_,["Cyrillic"],_,_,_}},
									{region, "mn", {"MN",_,_,_,_,_,["Mongolia"],_,_,_}}
									],
								bcp47:describe_tag("mn-cyrl-mn"))
					end,
					fun() -> ?assertMatch([
									{language, "sr", {"sr",_,_,_,_,_,["Serbian"],_,_,_}},
									{script, "Latn", {"Latn",_,_,_,_,_,["Latin"],_,_,_}},
									{region, "RS", {"RS",_,_,_,_,_,["Serbia"],_,_,_}}
									],
								bcp47:describe_tag("sr-Latn-RS"))
					end,
					fun() -> ?assertMatch([
									{language, "az", {"az",_,_,_,_,_,["Azerbaijani"],_,_,_}},
									{script, "Arab", {"Arab",_,_,_,_,_,["Arabic"],_,_,_}},
									{region, "IR", {"IR",_,_,_,_,_,["Islamic Republic of Iran"],_,_,_}}
									],
								bcp47:describe_tag("az-Arab-IR"))
					end,
					fun() -> ?assertMatch([
									{language,"zh",
										{"zh",none,macrolanguage,none,none,none,
											["Chinese"],
											[],
											{2005,10,16},
											none}},
									{script,"Hant",
										{"Hant",none,none,none,none,none,
											["Han (Traditional variant)"],
											[],
											{2005,10,16},
											none}},
									{region,"CN",
										{"CN",none,none,none,none,none,
											["China"],
											[],
											{2005,10,16},
											none}}] ,
								bcp47:describe_tag("zh-Hant-CN"))
					end,
					fun() -> ?assertMatch([
									{language, "en", {"en",_,_,_,_,_,["English"],_,_,_}},
									{region, "CA", {"CA",_,_,_,_,_,["Canada"],_,_,_}},
									{privateuse, "x-ca"}
									],
								bcp47:describe_tag("en-CA-x-ca"))
					end,
					fun() -> ?assertMatch([
									{language, "az", {"az",_,_,_,_,_,["Azerbaijani"],_,_,_}},
									{script, "Latn", {"Latn",_,_,_,_,_,["Latin"],_,_,_}},
									{privateuse, "x-latn"}
									],
								bcp47:describe_tag("az-Latn-x-latn"))
					end,
					fun() -> ?assertMatch([
									{language, "sr", {"sr",_,_,_,_,_,["Serbian"],_,_,_}},
									{script, "Latn", {"Latn",_,_,_,_,_,["Latin"],_,_,_}}
									],
								bcp47:describe_tag("sr-Latn"))
					end,
					fun() -> ?assertMatch([
									{language, "de", {"de",_,_,_,_,_,["German"],_,_,_}},
									{region, "AT", {"AT",_,_,_,_,_,["Austria"],_,_,_}}
									],
								bcp47:describe_tag("de-AT"))
					end,
					fun() -> ?assertMatch([
									{language, "es", {"es",_,_,_,_,_,["Spanish", "Castilian"],_,_,_}},
									{region, "419", {"419",_,_,_,_,_,["Latin America and the Caribbean"],_,_,_}}
									],
								bcp47:describe_tag("es-419"))
					end,
					fun() -> ?assertMatch([
									{language, "de", {"de",_,_,_,_,_,["German"],_,_,_}},
									{region, "DE", {"DE",_,_,_,_,_,["Germany"],_,_,_}},
									{variant, "1901", {"1901",_,_,_,_,_,["Traditional German orthography"],_,_,_}}
									],
								bcp47:describe_tag("de-DE-1901"))
					end,
					fun() -> ?assertMatch([
									{language, "sl", {"sl",_,_,_,_,_,["Slovenian"],_,_,_}},
									{variant, "nedis", {"nedis",_,_,_,_,_,["Natisone dialect", "Nadiza dialect"],_,_,_}}
									],
								bcp47:describe_tag("sl-nedis"))
					end,
					fun() -> ?assertMatch([
									{language, "de", {"de",_,_,_,_,_,["German"],_,_,_}},
									{region, "CH", {"CH",_,_,_,_,_,["Switzerland"],_,_,_}},
									{variant, "1996", {"1996",_,_,_,_,_,["German orthography of 1996"],_,_,_}}
									],
								bcp47:describe_tag("de-CH-1996"))
					end,
					fun() -> ?assertMatch([{extension, "a-value"} ],
								% invalid because it doesn't follow a language tag
								bcp47:describe_tag("a-value"))
					end,
					fun() -> ?assertMatch([
									{language, "de", {"de",_,_,_,_,_,["German"],_,_,_}},
									{extension, "a-value"} ],
								bcp47:describe_tag("de-a-value"))
					end,
					fun() -> ?assertMatch([
									{language, "en", {"en",_,_,_,_,_,["English"],_,_,_}},
									{extension, "a-bbb"},
									{extension, "a-ccc"} ],
								% invalid because the singleton `a` is repeated
								bcp47:describe_tag("en-a-bbb-a-ccc"))
					end,
					fun() -> ?assertMatch([
									{language, "en", {"en",_,_,_,_,_,["English"],_,_,_}},
									{extension, "a-bbb"},
									{privateuse, "x-a-ccc"} ],
								bcp47:describe_tag("en-a-bbb-x-a-ccc"))
					end,
					fun() -> ?assertMatch([
									{language, "tlh", {"tlh",_,_,_,_,_,["Klingon", "tlhIngan-Hol"],_,_,_}},
									{extension, "a"},
									{extension, "b-foo"} ],
								% invalid because the singleton `a` is not followed by an
								% extension subtag (it is immediately followed by singleton `b`)
								bcp47:describe_tag("tlh-a-b-foo"))
					end,
					fun() -> ?assertMatch([
									{language, "fr", {"fr",_,_,_,_,_,["French"],_,_,_}},
									{extension, "a-Latn"}
									],
								bcp47:describe_tag("fr-a-Latn"))
					end,
					fun() -> ?assertMatch([
									{language,"en", {"en",_,_,_,_,"Latn", ["English"], _, _, _}},
									{script,"Latn", {"Latn",_,_,_,_,_, ["Latin"], _, _, _}},
									{region,"GB", {"GB",_,_,_,_,_, ["United Kingdom"], _, _, _}},
									{variant,"boont", {"boont","en",_,_,_,_, ["Boontling"], _, _, _}},
									{extension,"r-extended-sequence"},
									{privateuse,"x-private"}
									],
								bcp47:describe_tag("en-Latn-GB-boont-r-extended-sequence-x-private"))
					end

					]},

			{"bcp47:describe_tag/1 recognizes irregular tags",
				[
					fun() -> ?assertMatch([
									{grandfathered,
										"en-GB-oed",
										{
											"en-GB-oed",
											none,
											none,
											none,
											none,
											none,
											[_],   % Description(s)
											[],
											{2003, 7, 9},
											none
											}}], bcp47:describe_tag("en-GB-oed"))
					end ,
					fun() -> ?assertMatch([
									{grandfathered,
										"i-ami",
										{
											"i-ami",
											none,
											none,
											"ami",  % preferred value
											none,
											none,
											[_],    % Description(s)
											[],
											{1999, 5, 25},
											{2009, 7, 29}
											}}], bcp47:describe_tag("i-ami"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"i-bnn",
										{
											"i-bnn",
											none,
											none,
											"bnn",  % preferred value
											none,
											none,
											[_],    % Description(s)
											[],
											{1999, 5, 25},
											{2009, 7, 29}
											}}], bcp47:describe_tag("i-bnn"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"i-default",
										{
											"i-default",
											none,
											none,
											none,
											none,
											none,
											[_],    % Description(s)
											[],
											{1998, 3, 10},
											none
											}}], bcp47:describe_tag("i-default"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"i-enochian",
										{
											"i-enochian",
											none,
											none,
											none,
											none,
											none,
											[_],    % Description(s)
											[],
											{2002, 7, 3},
											none
											}}], bcp47:describe_tag("i-enochian"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"i-hak",
										{
											"i-hak",
											none,
											none,
											"hak",
											none,
											none,
											[_],    % Description(s)
											[],
											{1999, 1, 31},
											{2000, 1, 10}
											}}], bcp47:describe_tag("i-hak"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"i-klingon",
										{
											"i-klingon",
											none,
											none,
											"tlh",
											none,
											none,
											[_],    % Description(s)
											[],
											{1999, 5, 26},
											{2004, 2, 24}
											}}], bcp47:describe_tag("i-klingon"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"i-lux",
										{
											"i-lux",
											none,
											none,
											"lb",
											none,
											none,
											[_],    % Description(s)
											[],
											{1997, 9, 19},
											{1998, 9, 9}
											}}], bcp47:describe_tag("i-lux"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"i-mingo",
										{
											"i-mingo",
											none,
											none,
											none,
											none,
											none,
											[_],    % Description(s)
											[],
											{1997, 9, 19},
											none
											}}], bcp47:describe_tag("i-mingo"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"i-navajo",
										{
											"i-navajo",
											none,
											none,
											"nv",
											none,
											none,
											[_],    % Description(s)
											[],
											{1997, 9, 19},
											{2000, 2, 18}
											}}], bcp47:describe_tag("i-navajo"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"i-pwn",
										{
											"i-pwn",
											none,
											none,
											"pwn",
											none,
											none,
											[_],    % Description(s)
											[],
											{1999, 5, 25},
											{2009, 7, 29}
											}}], bcp47:describe_tag("i-pwn"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"i-tao",
										{
											"i-tao",
											none,
											none,
											"tao",
											none,
											none,
											[_],    % Description(s)
											[],
											{1999, 5, 25},
											{2009, 7, 29}
											}}], bcp47:describe_tag("i-tao"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"i-tay",
										{
											"i-tay",
											none,
											none,
											"tay",
											none,
											none,
											[_],    % Description(s)
											[],
											{1999, 5, 25},
											{2009, 7, 29}
											}}], bcp47:describe_tag("i-tay"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"i-tsu",
										{
											"i-tsu",
											none,
											none,
											"tsu",
											none,
											none,
											[_],    % Description(s)
											[],
											{1999, 5, 25},
											{2009, 7, 29}
											}}], bcp47:describe_tag("i-tsu"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"sgn-BE-FR",
										{
											"sgn-BE-FR",
											none,
											none,
											"sfb",
											none,
											none,
											[_],    % Description(s)
											[],
											{2001, 11, 11},
											{2009, 7, 29}
											}}], bcp47:describe_tag("sgn-BE-FR"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"sgn-BE-NL",
										{
											"sgn-BE-NL",
											none,
											none,
											"vgt",
											none,
											none,
											[_],    % Description(s)
											[],
											{2001, 11, 11},
											{2009, 7, 29}
											}}], bcp47:describe_tag("sgn-BE-NL"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"sgn-CH-DE",
										{
											"sgn-CH-DE",
											none,
											none,
											"sgg",
											none,
											none,
											[_],    % Description(s)
											[],
											{2001, 11, 11},
											{2009, 7, 29}
											}}], bcp47:describe_tag("sgn-CH-DE"))
					end
					]
				},

			{"bcp47:describe_tag/1 recognizes regular tags",
				[
					fun() -> ?assertMatch([
									{grandfathered,
										"art-lojban",
										{
											"art-lojban",
											none,
											none,
											"jbo",
											none,
											none,
											[_],   % Description(s)
											[],
											{2001, 11, 11},
											{2003, 9, 2}
											}}], bcp47:describe_tag("art-lojban"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"cel-gaulish",
										{
											"cel-gaulish",
											none,
											none,
											none,
											none,
											none,
											[_],    % Description(s)
											[],
											{2001, 5, 25},
											none
											}}], bcp47:describe_tag("cel-gaulish"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"no-bok",
										{
											"no-bok",
											none,
											none,
											"nb",  % preferred value
											none,
											none,
											[_],    % Description(s)
											[],
											{1995, 8, 23},
											{2000, 2, 18}
											}}], bcp47:describe_tag("no-bok"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"no-nyn",
										{
											"no-nyn",
											none,
											none,
											"nn",  % preferred value
											none,
											none,
											[_],    % Description(s)
											[],
											{1995, 8, 23},
											{2000, 2, 18}
											}}], bcp47:describe_tag("no-nyn"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"zh-guoyu",
										{
											"zh-guoyu",
											none,
											none,
											"cmn",
											none,
											none,
											[_],    % Description(s)
											[],
											{1999,12,18},
											{2005,7,15}
											}}], bcp47:describe_tag("zh-guoyu"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"zh-hakka",
										{
											"zh-hakka",
											none,
											none,
											"hak",
											none,
											none,
											[_],    % Description(s)
											[],
											{1999,12,18},
											{2009,7,29}
											}}], bcp47:describe_tag("zh-hakka"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"zh-min",
										{
											"zh-min",
											none,
											none,
											none,
											none,
											none,
											[_],    % Description(s)
											[],
											{1999,12,18},
											{2009,7,29}
											}}], bcp47:describe_tag("zh-min"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"zh-min-nan",
										{
											"zh-min-nan",
											none,
											none,
											"nan",
											none,
											none,
											[_],    % Description(s)
											[],
											{2001, 3, 26},
											{2009, 7, 29}
											}}], bcp47:describe_tag("zh-min-nan"))
					end,
					fun() -> ?assertMatch([
									{grandfathered,
										"zh-xiang",
										{
											"zh-xiang",
											none,
											none,
											"hsn",
											none,
											none,
											[_],    % Description(s)
											[],
											{1999,12,18},
											{2009,7,29}
											}}], bcp47:describe_tag("zh-xiang"))
					end
					]
				}

			]}.

setup() ->
	case bcp47:start_link() of
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