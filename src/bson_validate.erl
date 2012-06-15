-module(bson_validate).
-export([valid/1]).
-include("bson.hrl").

%%% Determines the validity of the given document.
%%%
%%% A few element types have preconditions that aren't easily expressed outside
%%% of documentation and validation code.
-spec valid(document() | elements()) -> ok | {error, tuple()}.
valid({document, Elements}) ->
    valid(Elements);
valid({array, {document, Elements}}) ->
    valid(Elements);
valid({embedded_doc, {document, Elements}}) ->
    valid(Elements);
valid([{Key, Element} | Rest]) ->
    KBool = ((length(Key) > 0) and valid_string(Key)),
    VE = valid_element(Element),
    if
	not KBool   -> {error, {invalid_key, Key}};
	(VE =/= ok) -> VE;
	true        -> case Rest of
			   [] -> ok;
			   _  -> valid(Rest)
		       end
    end;
valid([]) -> ok.

% ----------------------------------------------------------------------------- %
%				  Private API                                   %
% ----------------------------------------------------------------------------- %

-spec valid_string(string()) -> boolean().
valid_string(Str) ->
    no_null_in_string(Str) and convertable_to_unicode(Str).

-spec convertable_to_unicode(string()) -> boolean().
convertable_to_unicode(Str) ->
    case ?Ctb(Str) of
	List when is_binary(List) -> true;
	_                         -> false
    end.

-spec no_null_in_string(string()) -> boolean().
no_null_in_string([]) ->
    false;
no_null_in_string([C | Rest]) ->
    case C of
	0 -> false;
	_ -> case Rest of
		 [] -> true;
		 _  -> valid_string(Rest)
	     end
    end.

-spec valid_element(base_element()) -> ok | {error, tuple()}.
valid_element({array, Doc}) ->
    valid(Doc);
valid_element({embedded_doc, Doc}) ->
    valid(Doc);
valid_element({object_id, Val}=Orig) ->
    case byte_size(Val) of
	12 -> ok;
	_  -> {error, {bad_length, Orig}}
    end;
valid_element({regex, Pattern, Options}=Orig) ->
    case (valid_string(Pattern) and valid_string(Options)) of
	false ->
	    {error, {pattern_or_options_not_valid_string, Orig}};
	true ->
	    ok
    end;
valid_element({dbpointer, Desc, Val}=Orig) ->
    case valid_string(Desc) of
	false -> {error, {invalid_string, Orig}};
	_ -> case byte_size(Val) of
		 12 -> ok;
		 _  -> {error, {bad_length, Orig}}
	     end
    end;
valid_element({int32, I}=Orig) ->
    if
	I > 2147483648 -> {error, {not_32_bit, Orig}};
	true           -> ok
    end;
valid_element({int64, I}=Orig) ->
    if
	I > 9223372036854775808 -> {error, {not_64_bit, Orig}};
	true                    -> ok
    end;
valid_element({timestamp, T}=Orig) ->
    if
	byte_size(T) =/= 8 -> {error, {bad_length, Orig}};
	true               -> ok
    end;
valid_element({datetime, {{Year, _, _}, _}}=Orig) ->
    if
	Year < 1970 -> {error, {time_before_unix, Orig}};
	true        -> ok
    end;
valid_element({string, Str}=Orig) ->
    case valid_string(Str) of
	false -> {error, {invalid_string, Orig}};
	_     -> ok
    end;
valid_element({javascript, Str}=Orig) ->
    case valid_string(Str) of
	false -> {error, {invalid_string, Orig}};
	_     -> ok
    end;
valid_element({javascript_with_scope, Str}=Orig) ->
    case valid_string(Str) of
	false -> {error, {invalid_string, Orig}};
	_     -> ok
    end;
valid_element({symbol, Str}=Orig) ->
    case valid_string(Str) of
	false -> {error, {invalid_string, Orig}};
	_     -> ok
    end;
valid_element({_, List}=Orig) when is_list(List) ->
    if
	length(List) =:= 0 -> {error, {bad_length, Orig}};
	true               -> ok
    end;
valid_element(_) ->
    ok.

