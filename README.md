erlang_bcp47  [![Build Status](https://travis-ci.org/flitbit/erlang_bcp47.png)](http://travis-ci.org/flitbit/erlang_bcp47)
======

Erlang module for working with languages according to best common practice [BCP 47](http://www.rfc-editor.org/rfc/bcp/bcp47.txt).

## Features

* Self-updating from the IANA language subtag registry (via config setting)
* Describes all registered subtags

## Use

If you just want to try it out then clone this repo, once you have it:

```bash
make && make console
```

The intended public parts are in `bcp47`, which must be started; the `make console` command above will have started it.

You can have the module describe language tags according to the IANA language subtag registry.

### Describe a Tag

In an application that supports internationalization, you may encounter the language tag `de-DE-1901`. You can get a description detailing what it means using:

```erlang
bcp47:describe_tag("de-DE-1901").
```

The resulting description contains all of the important information about the constituent subtags:

```erlang
[{language,"de",
           {"de",none,none,none,none,"Latn",
            ["German"],
            [],
            {2005,10,16},
            none}},
 {region,"DE",
         {"DE",none,none,none,none,none,
          ["Germany"],
          [],
          {2005,10,16},
          none}},
 {variant,"1901",
          {"1901","de",none,none,none,none,
           ["Traditional German orthography"],
           [],
           {2005,10,16},
           none}}]
```

Registered subtags have the following specification:

```erlang
-type subtag() :: language | extlang | script | region | variant.
-type tag() :: grandfathered | redundant.
-type subtag_or_tag() :: subtag() | tag().
-type scope() :: macrolanguage | collection | special | 'private-use'.

-type registered_subtag() :: tuple(
	Type :: subtag_or_tag(),
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
```

The 1st and 3rd elements, `Type` and `Detail` are invariant for matched subtags, however, the second element `Lookup` will have letter casing as specified by the caller.

The definition of elements in the `Detail` tuple correspond to the similarly named fields described in [BCP 47](http://www.rfc-editor.org/rfc/bcp/bcp47.txt). The details enable programs to validate language tags and make appropriate processing decisions. For instance, the language subtag `de` has the value `Latn` in its `SuppressScript` element, so we know if the tag had been `de-Latn-DE-1901`, the script `Latn` is unneccesarily specified. Also note that the `1901` variant is only valid with the prefix `de`, so if we saw `pl-DE-1901`, even though polish was spoken in Germany in 1901, the construction is invalid because the `1901` variant is only applicable to the german language.

BCP 47 indicates that subtags are case-insensitive, even though it incorporates recommendations for character case in many subtag types. Since subtag types are positional, to disambiguate subtags `bcp47:describe_tag/1` will choose subtags based on the position of the subtag and what precedes it, and only resorts to character case matching if position isn't helpful.

### Describe a Subtag

In some cases, you'll want to see all of the possibilities for a particular subtag. Take 'sr' for example:

```erlang
bcp47:describe("sr").
```

The resulting description will be:

```erlang
{multiple,"sr",
          [{language,"sr",
                     {"sr",none,none,none,"sh",none,
                      ["Serbian"],
                      [],
                      {2005,10,16},
                      none}},
           {region,"sr",
                   {"SR",none,none,none,none,none,
                    ["Suriname"],
                    [],
                    {2005,10,16},
                    none}}]}
```

The result shows that `sr` can refer to either the language `Serbian` or the region `Suriname`.

## API

Use the `bcp47` module to describe a language tag or individual subtags.

+ `describe/1` - Describes the specified subtag.
+ `describe_tag/1` - Describes the registered subtags within the specified tag.
+ `list/1` - Lists (in subtag order) the registered subtags of the specified type.

### `describe/1`
+ `Subtag` - a langauge subtag to describe.

Describes the specified subtag.


In an Erlang console type:

```erlang
Klingon = bcp47:describe("tlh").
```

The variable `Klingon` is a 3-tuple such that:

```erlang
{language,"tlh",
          {"tlh",none,none,none,none,none,
           ["Klingon","tlhIngan-Hol"],
           [],
           {2005,10,16},
           none}} = Klingon.
```


`describe_tag/1`
+ `tag` - a langauge tag to describe.

In an Erlang console type:

```erlang
Language = bcp47:describe_tag("de-CH-1996").
```

The variable `Language` is a list of `registered_subtag()` tuples such that:
```erlang
[{language,"de",
           {"de",none,none,none,none,"Latn",
            ["German"],
            [],
            {2005,10,16},
            none}},
 {region,"CH",
         {"CH",none,none,none,none,none,
          ["Switzerland"],
          [],
          {2005,10,16},
          none}},
 {variant,"1996",
          {"1996","de",none,none,none,none,
           ["German orthography of 1996"],
           [],
           {2005,10,16},
           none}}] = Language.
```

## Configuration

`bcp47` can be configured in several ways. Refer to the `priv\app.config` file.

Default configuration settings are defined in `bcp47_sup:config_defaults/0` to be:

```erlang
config_defaults() ->
	[ {create_missing_paths, true},
		{registry_path, "./priv/data/erlang_bcp47/"},
		{resource, "http://www.iana.org/assignments/language-subtag-registry/language-subtag-registry"},
		{seed_path, "./priv/language-subtag-registry.txt"},
		{self_update_interval_minutes, 0}
		].
```

+ `create_missing_paths` - specifies whether the missing parts of `registry_path` should be created.
+ `registry_path` - specifies the directory where `dets` should be stored.
+ `resource` - specifies the URI consulted for registry updates; used to self-update when `self_update_interval_minutes` is not 0 (zero).
+ `seed_path` - specifies a file system path where a copy of the language subtag registry is stored (in text format). Useful if not self-updating.
+ `self_update_interval_minutes` - specifies the number of minutes between self-update attempts.

# More

There are a lot of insightful tests under in `test/language_server_tests.erl`; please have a look.

Run the tests in bash:

```bash
make test
```

# OTP Compatability

A list of supported OTP releases is maintained in `.travis.yml` ([CI Status here](http://travis-ci.org/flitbit/erlang_bcp47)). Be aware that Travis CI doesn't yet support the latest OTP releases so you'll want to verify in your own environment as I do.

## TODO

+ spec and dialyze

## Change Log

*   `0.1` 2014-05-05 - Initial. Feedback, issues, and pull-requests welcome.

## Assist

Since this is brand spanking new on `2014-05-05`, I welcome any feedback, issues, and pull requests (especially those with more tests or integrations).

Peace.
