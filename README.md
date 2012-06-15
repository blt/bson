# `bson` -- an Erlang BSON v1.0 implementation

This library implements a strict BSON encoder and decoder to and from
`bson:document()`s. This library is meant to be included into any project which
requires very explicit BSON serialization; no dependencies on any database
drivers exist. `bson_validate:valid/1` offers checking of Erlang terms for
conformance to the `bson:document()` constraints. `bson:decode/1` will always
return a valid `bson:document()` but `bson:encode/1` does no checking of its
input and assumes compliance with the constraints of `bson:document()`.

The functions `bson:encode/1` and `bson:decode/1` are inverse of one another, as
confirmed in `test/bson_SUITE.erl`. A heuristic encoder is intended future work
but it will not, when composed with `bson:decode/1` be the identity function of
`bson:document()`.

## Quickstart

How do you use this crazy thing? If you don't know the BSON v1.0 specification
please read it [here](http://bsonspec.org/#/specification). Once you've got that
under your belt, the type heirarchy of `include/bson.hrl` _should_ make a fair
bit of sense; I've striven to document this project thoroughly at the source
level.

Building `bson` is a quick `make`. All dependencies will be pulled in and
compiled. Get an Erlang shell using your preferred method. Now, here's a
document that encodes a few well known things:

    > D = {document, [{"motto", {string, "We'll do it live!"}}, {"pi-ish", {double, 3.141}}]}.

Encode it:

    > bson:encode(D).
	<<50,0,0,0,2,109,111,116,116,111,0,18,0,0,0,87,101,39,108,
	  108,32,100,111,32,105,116,32,108,105,...>>
    > io:format("~p~n", [bson:encode(D)]).
	<<50,0,0,0,2,109,111,116,116,111,0,18,0,0,0,87,101,39,108,108,32,100,111,32,
	  105,116,32,108,105,118,101,33,0,1,112,105,45,105,115,104,0,84,227,165,155,
	  196,32,9,64,0>>

You can pick that apart and get back the original document _if you'd like_ but
have the machine do it for you!

    > bson:decode( bson:encode(D) ).
	{document,[{"motto",{string,"We'll do it live!"}},
	           {"pi-ish",{double,3.141}}]}

Happy hacking!

## Caveats

This is early stage software, v0.1.0. It's only been tested against itself and
may not _exactly_ understand BSON as it's implemented in the wild. I'll be
extending the test suite to check against external, common implementations in a
future release.
