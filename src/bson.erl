%%% This module implements an encoder and decoder to and from Erlang terms to
%%% BSON v1.0, defined here: http://bsonspec.org/#/specification. The elaboration
%%% below owes much to the BSON specification.
%%%
%%% The BSON language defines the following basic types, acting as terminals in
%%% the grammer to follow:
%%%
%%%     byte     1 byte (8-bits)
%%%     int32    4 bytes (32-bit signed integer)
%%%     int64    8 bytes (64-bit signed integer)
%%%     double   8 bytes (64-bit IEEE 754 floating point)
%%%
%%% Each type must be in little endian order.
%%%
%%% In the grammar which follows consider x01 as the byte 0000 0001. Note this
%%% is a shortening of the C notation used in the official specification, listed
%%% above, which would have the same byte as "\x01". Erlang native 1:8 is not
%%% used for clarity.
%%%
%%% The operator * is to be taken in the same context as in regular
%%% expressions. x01*2 means the bytes x01x01 and the unary operator * means
%%% that the repetition may occur zero or more times. The characters -- are to
%%% be taken as comments, which extend to the end of the production's
%%% line. Footnotes such as "[1]" may appear and denote a paragrap to
%%% immediately follow the grammar. Nil should be taken in its usual,
%%% grammatical sense.
%%%
%%%     document ::= int32 e_list "\x00"		-- BSON Document
%%%     e_list	 ::= element e_list | Nil		-- Sequence of elements
%%%     element	 ::= "\x01" e_name double		-- Floating point
%%%                   | "\x02" e_name string		-- UTF-8 string
%%%                   | "\x03" e_name document		-- Embedded document
%%%                   | "\x04" e_name document		-- Array
%%%                   | "\x05" e_name binary		-- Binary data
%%%                   | "\x06" e_name			-- Undefined — Deprecated
%%%                   | "\x07" e_name (byte*12)		-- ObjectId
%%%                   | "\x08" e_name "\x00"		-- Boolean "false"
%%%                   | "\x08" e_name "\x01"		-- Boolean "true"
%%%                   | "\x09" e_name int64		-- UTC datetime
%%%                   | "\x0A" e_name			-- Null value
%%%                   | "\x0B" e_name cstring cstring	-- Regular expression
%%%                   | "\x0C" e_name string (byte*12)	-- DBPointer — Deprecated
%%%                   | "\x0D" e_name string		-- JavaScript code
%%%                   | "\x0E" e_name string		-- Symbol
%%%                   | "\x0F" e_name code_w_s		-- JavaScript code w/ scope
%%%                   | "\x10" e_name int32		-- 32-bit Integer
%%%                   | "\x11" e_name int64		-- Timestamp
%%%                   | "\x12" e_name int64		-- 64-bit integer
%%%                   | "\xFF" e_name			-- Min key
%%%                   | "\x7F" e_name			-- Max key
%%%     e_name	::= cstring				-- Key name
%%%     string	::= int32 (byte*) "\x00"		-- String (Pascal Style)
%%%     cstring	::= (byte*) "\x00"			-- CString
%%%     binary	::= int32 subtype (byte*)		-- Binary
%%%     subtype	::= "\x00"				-- Binary / Generic
%%%                   | "\x01"				-- Function
%%%                   | "\x02"				-- Binary (Old)
%%%                   | "\x03"				-- UUID
%%%                   | "\x05"				-- MD5
%%%                   | "\x80"				-- User defined
%%%     code_w_s::= int32 string document		-- Code w/ scope
%%%
%%% bson.hrl distributed with this project contains the type heirarchy encoding
%%% this as Erlang terms.
-module(bson).
-export([decode/1, encode/1]).
-include("bson.hrl").

%% element constants
-define(GregEpochSec,   62167219200).
-define(DOUBLE,		<<?Byte(1)>>).
-define(STRING,		<<?Byte(2)>>).
-define(EMBDOC,		<<?Byte(3)>>).
-define(ARRAY ,		<<?Byte(4)>>).
-define(BINDATA,	<<?Byte(5)>>).
-define(UNDEF,		<<?Byte(6)>>).
-define(OBJID,		<<?Byte(7)>>).
-define(BOOLEAN,	<<?Byte(8)>>).
-define(DATETIME,	<<?Byte(9)>>).
-define(NULL,		<<?Byte(10)>>).
-define(REGEX,		<<?Byte(11)>>).
-define(DBPOINTER,	<<?Byte(12)>>).
-define(JSCODE,		<<?Byte(13)>>).
-define(SYMBOL,		<<?Byte(14)>>).
-define(JSCODEWS,	<<?Byte(15)>>).
-define(INT32,		<<?Byte(16)>>).
-define(TIMESTAMP,	<<?Byte(17)>>).
-define(INT64,		<<?Byte(18)>>).
-define(MINKEY,		<<?Byte(255)>>).
-define(MAXKEY,		<<?Byte(127)>>).

%% utility macros
-define(Byte(B), B:8/little).
-define(Null, ?Byte(0)).
-define(Double(D), D:64/little-float).
-define(Int32(I), I:32/little-signed).
-define(Int64(I), I:64/little-signed).
-define(CString(Str), Str,?Null).

%%% Encode a bson:document() into a BSON binary.
%%%
%%% If you've taken the time to construct a bson:document() this function will
%%% pump out a binary that can be passed to any other client which understands
%%% BSON.
-spec encode(document()) -> bson_binary().
encode({document, []}) ->
    <<?Int32(5), ?Null>>;
encode({document, Elements}) when is_list(Elements) ->
    Encoding = encode(Elements, []),
    Size = size(Encoding) + 5, % document overhead is five bytes
    binary:list_to_bin([<<?Int32(Size)>>, Encoding, <<0:8>>]).

%%% Decode a BSON binary into a bson:document().
%%%
%%% Constructs a bson:document() from a BSON binary as defined in the BSON
%%% specification v1.0.
-spec decode(bson_binary()) -> document().
decode(<<_Size:32/little-signed, Elements/binary>>) ->
    {document, d_elements(Elements, [])}.

% ----------------------------------------------------------------------------- %
%				  Private API                                   %
% ----------------------------------------------------------------------------- %

%-spec pstring(string()) -> binary().
pstring(String) when is_list(String) ->
    S = ?Ctb(String),
    Size = byte_size(S) + 1,
    [<<?Int32(Size)>>, S, <<?Null>>].

%%% Counterpart to encode/1.
-spec encode(elements(), iolist()) -> bson_binary().
encode([], Bin) ->
    iolist_to_binary(lists:reverse(Bin));
encode([{K, V} | Rest], Bin) ->
    Key = ?Ctb(K),
    {Type, Ret} = case V of
		      {double, Val} when is_float(Val) ->
			  {?DOUBLE, <<?Double(Val)>>};
		      {string, Val} when is_list(Val) ->
			  {?STRING, pstring(Val)};
		      {embedded_doc, {document, _Elems}=Doc} ->
			  {?EMBDOC, encode(Doc)};
		      {array, {document, _Elems}=Array} ->
			  {?ARRAY, encode(Array)};
		      {binary_data, SubType, Val} when is_binary(Val) ->
			  ST = case SubType of
				   generic -> <<?Byte(0)>>;
				   function -> <<?Byte(1)>>;
				   binary_old -> <<?Byte(2)>>;
				   uuid -> <<?Byte(3)>>;
				   md5 -> <<?Byte(4)>>;
				   user_defined -> <<?Byte(128)>>
			       end,
			  ValSize = byte_size(Val),
			  {?BINDATA, [<<?Int32(ValSize)>>, ST, Val]};
		      undefined ->
			  {?UNDEF, []};
		      {object_id, Val} when is_binary(Val) and (byte_size(Val) =:= 12) ->
			  {?OBJID, Val};
		      {boolean, false} ->
			  {?BOOLEAN, <<?Byte(0)>>};
		      {boolean, true} ->
			  {?BOOLEAN, <<?Byte(1)>>};
		      {datetime, {_Date, _Time}=DT} -> % :: calendar::datetime()
			  % BSON spec has this as milliseconds since Unix epoch
			  GregSeconds = calendar:datetime_to_gregorian_seconds(DT),
			  MS = (GregSeconds - ?GregEpochSec)*1000,
			  {?DATETIME, <<?Int64(MS)>>};
		      null ->
			  {?NULL, []};
		      {regex, Pattern, Options} ->
			  {?REGEX, [[?Ctb(Pattern), <<?Null>>], [?Ctb(Options), <<?Null>>]]};
		      {dbpointer, S, B} when is_binary(B) and (byte_size(B) =:= 12) ->
			  {?DBPOINTER, [pstring(S), B]};
		      {javascript, Code} when is_list(Code) ->
			  {?JSCODE, pstring(Code)};
		      {symbol, Symbol} when is_list(Symbol) ->
			  {?SYMBOL, pstring(Symbol)};
		      {javascript_with_scope, Code} when is_list(Code) ->
			  {?JSCODEWS, pstring(Code)};
		      {int32, I} when is_integer(I) and (I =< 2147483648) -> % 2^31
			  {?INT32, <<?Int32(I)>>};
		      {timestamp, T} when is_binary(T) and (byte_size(T) =:= 8) ->
			  % This is MongoDB specific and not used for
			  % communicating datetimes. See datetime for such a
			  % type.
			  {?TIMESTAMP, T};
		      {int64, I} when is_integer(I) and (I =< 9223372036854775808) -> % 2^63
			  {?INT64, <<?Int64(I)>>};
		      minkey ->
			  {?MINKEY, []};
		      maxkey ->
			  {?MAXKEY, []}
		  end,
    encode(Rest, [[Type, Key, <<?Null>>, Ret] | Bin]).

-define(DoubleConst, 1).
-define(StringConst, 2).
-define(EmbDocConst, 3).
-define(ArrayConst, 4).
-define(BinDataConst, 5).
-define(UndefConst, 6).
-define(ObjIdConst, 7).
-define(BooleanConst, 8).
-define(DateTimeConst, 9).
-define(NullConst, 10).
-define(RegexConst, 11).
-define(DbPointerConst, 12).
-define(JSCodeConst, 13).
-define(SymbolConst, 14).
-define(JSCodeWSConst, 15).
-define(Int32Const, 16).
-define(TimestampConst, 17).
-define(Int64Const, 18).
-define(MinkeyConst, 255).
-define(MaxkeyConst, 127).

%%% Used by decode/1. Decodes an e_list.
-spec d_elements(binary(), elements()) -> elements().
d_elements(<<?Null>>, Accum) ->
    lists:reverse(Accum);
d_elements(<<?Byte(Type), Further/binary>>, Accum) ->
    [Key, Rest] = binary:split(Further, <<?Null>>),
    {Value, Elements} = case Type of
			    ?DoubleConst ->
				<<?Double(Val), Elems/binary>> = Rest,
				{{double, Val}, Elems};
			    ?StringConst ->
				<<?Int32(ValSize), EtAl/binary>> = Rest,
				StrSize = ValSize - 1,
				<<V:StrSize/bytes, ?Null, Elems/binary>> = EtAl,
				{{string, ?Ctl(V)}, Elems};
			    ?EmbDocConst ->
				<<?Int32(DocSize), _/binary>> = Rest,
				<<Doc:DocSize/binary, Elems/binary>> = Rest,
				{{embedded_doc, decode(Doc)}, Elems};
			    ?ArrayConst ->
				<<?Int32(DocSize), _/binary>> = Rest,
				<<Doc:DocSize/binary, Elems/binary>> = Rest,
				{{array, decode(Doc)}, Elems};
			    ?BinDataConst ->
				<<?Int32(BinSize), ?Byte(SubType), Rem/binary>> = Rest,
				<<Bin:BinSize/binary, Elems/binary>> = Rem,
				ST = case SubType of
					 0 -> generic;
					 1 -> function;
					 2 -> binary_old;
					 3 -> uuid;
					 4 -> md5;
					 128 -> user_defined
				     end,
				{{binary_data, ST, Bin}, Elems};
			    ?UndefConst ->
				{undefined, Rest};
			    ?ObjIdConst ->
				<<ObjId:12/binary, Elems/binary>> = Rest,
				{{object_id, ObjId}, Elems};
			    ?BooleanConst ->
				<<?Byte(B), Elems/binary>> = Rest,
				Bool = case B of
					   0 -> false;
					   1 -> true
				       end,
				{{boolean, Bool}, Elems};
			    ?DateTimeConst ->
				<<?Int64(DateTime), Elems/binary>> = Rest,
				GregSec = (DateTime + (?GregEpochSec*1000)) div 1000,
				DT = calendar:gregorian_seconds_to_datetime(GregSec),
				{{datetime, DT}, Elems};
			    ?NullConst ->
				{null, Rest};
			    ?RegexConst ->
				[Pattern, OptsEtAl] = binary:split(Rest, <<?Null>>),
				[Options, Elems] = binary:split(OptsEtAl, <<?Null>>),
				{{regex, ?Ctl(Pattern), ?Ctl(Options)}, Elems};
			    ?DbPointerConst ->
				<<?Int32(DBPSize), Rem/binary>> = Rest,
				StrSize = DBPSize-1,
				<<DBP:StrSize/bytes, ?Null, DBytes:12/bytes, Elems/binary>> = Rem,
				{{dbpointer, ?Ctl(DBP), DBytes}, Elems};
			    ?JSCodeConst ->
				<<?Int32(ValSize), EtAl/binary>> = Rest,
				StrSize = ValSize-1,
				<<JS:StrSize/bytes, ?Null, Elems/binary>> = EtAl,
				{{javascript, ?Ctl(JS)}, Elems};
			    ?SymbolConst ->
				<<?Int32(ValSize), EtAl/binary>> = Rest,
				StrSize = ValSize-1,
				<<Sym:StrSize/bytes, ?Null, Elems/binary>> = EtAl,
				{{symbol, ?Ctl(Sym)}, Elems};
			    ?JSCodeWSConst ->
				<<?Int32(ValSize), EtAl/binary>> = Rest,
				StrSize = ValSize-1,
				<<JS:StrSize/bytes, ?Null, Elems/binary>> = EtAl,
				{{javascript_with_scope, ?Ctl(JS)}, Elems};
			    ?Int32Const ->
				<<?Int32(I), Elems/binary>> = Rest,
				{{int32, I}, Elems};
			    ?TimestampConst ->
				<<?Int64(I), Elems/binary>> = Rest,
				{{timestamp, <<?Int64(I)>>}, Elems};
			    ?Int64Const ->
				<<?Int64(I), Elems/binary>> = Rest,
				{{int64, I}, Elems};
			    ?MinkeyConst ->
				{minkey, Rest};
			    ?MaxkeyConst ->
				{maxkey, Rest}
			end,
    d_elements(Elements, [{?Ctl(Key), Value} | Accum]).
