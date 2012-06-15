-module(bson_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("../include/test.hrl").
-include_lib("../include/bson.hrl").

-export([all/0]).
%-export([encode_spec_test/1, decode_spec_test/1, validate_spec_test/1]).
-export([decode_and_encode_are_inverses/1]).

all() -> ?CT_REGISTER_TESTS(?MODULE).

% Starting with bson:document() causes PropEr to crash, no idea why. Happily a
% document is just a bson:elements() elaborated. Anything that can't be
% fudged--like the spec tests--simply are not enabled.

id(D) -> bson:decode(bson:encode(D)).
is_valid(Doc) ->
    case bson_validate:valid(Doc) of
	ok -> true;
	_  -> false
    end.

decode_and_encode_are_inverses(_Config) ->
    P = ?FORALL(Elems, [bson:elements()],
		?IMPLIES(
		   begin
		       lists:foldl(fun(E, Bool) ->
					   Bool and is_valid({document, E})
				   end, true, Elems)
		   end,
		   begin
		       lists:foldl(fun(E, Bool) ->
					   Doc = {document, E},
					   Bool and (Doc == id(Doc))
				   end, true, Elems)
		   end
	       )
	       ),
    true = proper:quickcheck(P, [long_result, verbose, {numtests, 1000}]).

%% ====================================================
%% Spec Tests
%% ====================================================

%% encode_spec_test(_Config) ->
%%     true = proper:check_spec({bson, encode, 1}).

%% decode_spec_test(_Config) ->
%%     true = proper:check_spec({bson, decode, 1}).

%% validate_spec_test(_Config) ->
%%     true = proper:check_spec({bson_validate, valid, 1}).
