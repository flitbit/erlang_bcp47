-type subtag() :: language | extlang | script | region | variant.
-type tag() :: grandfathered | redundant.
-type subtag_or_tag() :: subtag() | tag().
-type scope() :: macrolanguage | collection | special | 'private-use'.

-record(language, {
		%% applies to language, extlang, script, region, and variant
		subtag :: string(),
		%% langauge, extlang, script, region, variant, grandfathered, redundant
		type :: subtag_or_tag(),
		%% applies to language or extlang; macrolanguage | collection | special | 'private-use'
		scope :: scope(),
		%% For language, contains the primary language preferred.
		preferred_value :: string(),
		%% applies to language or extlang; contains the language that encompasses this subtag
		macrolanguage :: string(),
		%% applies to language or extlang; contains a script subtag that should not be used with the associated subtag.
		suppress_script :: string(),
		description :: list(string()),
		comment :: list(string()),
		added :: calendar:date(),
		deprecated :: calendar:date()
		}).

-record(extlang, {
		%% applies to language, extlang, script, region, and variant
		subtag :: string(),
		%% langauge, extlang, script, region, variant, grandfathered, redundant
		type :: subtag_or_tag(),
		%% applies to extlang and variant
		prefix :: list(string()),
		scope :: scope(),
		%% For extlang, grandfathered, or redundant, contains in order each of the subtags preferred.
		preferred_value :: string(),
		%% applies to language or extlang; contains the language that encompasses this subtag
		macrolanguage :: string(),
		%% applies to language or extlang; contains a script subtag that should not be used with the associated subtag.
		suppress_script :: string(),
		description :: list(string()),
		comment :: list(string()),
		added :: calendar:date(),
		deprecated :: calendar:date()
		}).

-record(script, {
		%% applies to language, extlang, script, region, and variant
		subtag :: string(),
		%% langauge, extlang, script, region, variant, grandfathered, redundant
		type :: subtag_or_tag(),
		%% For script, region or variant, contains the subtag of the same type that is preferred.
		preferred_value :: string(),
		description :: list(string()),
		comment :: list(string()),
		added :: calendar:date(),
		deprecated :: calendar:date()
		}).

-record(region, {
		%% applies to language, extlang, script, region, and variant
		subtag :: string(),
		%% langauge, extlang, script, region, variant, grandfathered, redundant
		type :: subtag_or_tag(),
		%% For script, region or variant, contains the subtag of the same type that is preferred.
		preferred_value :: string(),
		description :: list(string()),
		comment :: list(string()),
		added :: calendar:date(),
		deprecated :: calendar:date()
		}).

-record(variant, {
		%% applies to language, extlang, script, region, and variant
		subtag :: string(),
		%% langauge, extlang, script, region, variant, grandfathered, redundant
		type :: subtag_or_tag(),
		%% applies to extlang and variant
		prefix :: list(string()),
		%% For script, region or variant, contains the subtag of the same type that is preferred.
		preferred_value :: string(),
		description :: list(string()),
		comment :: list(string()),
		added :: calendar:date(),
		deprecated :: calendar:date()
		}).

-record(grandfathered, {
		%% applies to grandfathered or redundant
		tag :: string(),
		%% langauge, extlang, script, region, variant, grandfathered, redundant
		type :: subtag_or_tag(),
		%% For extlang, grandfathered, or redundant, contains in order each of the subtags preferred.
		preferred_value :: string(),
		description :: list(string()),
		comment :: list(string()),
		added :: calendar:date(),
		deprecated :: calendar:date()
		}).

-record(redundant, {
		%% applies to grandfathered or redundant
		tag :: string(),
		%% langauge, extlang, script, region, variant, grandfathered, redundant
		type :: subtag_or_tag(),
		%% For extlang, grandfathered, or redundant, contains in order each of the subtags preferred.
		preferred_value :: string(),
		description :: list(string()),
		comment :: list(string()),
		added :: calendar:date(),
		deprecated :: calendar:date()
		}).
