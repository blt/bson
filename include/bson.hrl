-export_type([document/0, subtype/0, elements/0, element/0, key/0, base_element/0, bson_binary/0]).
%%%
%%% This modules type heirarchy follows a similar form to the BNF above. In
%%% standard use, it will be rather annoyingly pedantic. Further modules may
%%% consume this one to translate a more presumptive encoding into this explicit
%%% form. (You might, for instance, always encode an erlang String as a {string,
%%% S}, assuming always that it's not, possibly, javascript,
%%% javascript_with_scope, or a symbol.
%%%
-type document() :: {document, Elements :: elements()}.
-type subtype() :: generic | function | binary_old | uuid | md5 | user_defined.
-type elements() :: list( element() ).
-type element() :: {key(), base_element()}.
-type key() :: nonempty_string().
-type base_element() :: {double, Double :: float() } |
			{string, String :: nonempty_string() } |
			{embedded_doc, Document :: document()} |
			{array, Array :: document()} |
			{binary_data, SubType :: subtype(), Value :: binary()} |
			undefined |
			{object_id, Value :: binary()} |
			{boolean, Bool :: boolean()} |
			{datetime, DateTime :: calendar:datetime()} |
			null |
			{regex, Pattern :: string(), Options :: string()} |
			{dbpointer, Desc :: string(), Value :: binary()} |
			{javascript, Code :: nonempty_string()} |
			{symbol, Symbol :: nonempty_string()} |
			{javascript_with_scope, Code :: nonempty_string()} |
			{int32, Int :: integer()} |
			{timestamp, Time :: binary()} |
			{int64, Int :: integer()} |
			minkey |
			maxkey.
-type bson_binary() :: binary().

-define(Ctb(Str), unicode:characters_to_binary(Str)).
-define(Ctl(Bin), unicode:characters_to_list(Bin)).
