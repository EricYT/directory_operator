%% @author Eric.yutao
%% @doc @todo Add description to protobuffs_parser.


-module(protobuffs_parser).

%% ====================================================================
%% API functions
%% ====================================================================
-export([parse_file/1]).


-spec parse_file(FileName::string()) -> list().
parse_file(File) when is_list(File) ->
	{ok, FileBinary} = file:read_file("../src/proto_test/repeater.proto"),
	ScanContent = scan(binary_to_list(FileBinary)),
	io:format(">> ~p~n", [{?MODULE, ?LINE, ScanContent}]),
	ok.

scan(File) when is_list(File) ->
	scan(File, [], 1).

scan([${|Rest], Accum, Line) ->
	scan(Rest, [{'{', Line}|Accum], Line);
scan([$}|Rest], Accum, Line) ->
	scan(Rest, [{'}', Line}|Accum], Line);
scan([$[|Rest], Accum, Line) ->
	scan(Rest, [{'[', Line}|Accum], Line);
scan([$]|Rest], Accum, Line) ->
	scan(Rest, [{']', Line}|Accum], Line);
scan([$(|Rest], Accum, Line) ->
	scan(Rest, [{'(', Line}|Accum], Line);
scan([$)|Rest], Accum, Line) ->
	scan(Rest, [{')', Line}|Accum], Line);
scan([$=|Rest], Accum, Line) ->
	scan(Rest, [{'=', Line}|Accum], Line);
scan([$;|Rest], Accum, Line) ->
	scan(Rest, [{';', Line}|Accum], Line);
scan([$,|Rest], Accum, Line) ->
	scan(Rest, [{',', Line}|Accum], Line);
scan([Digit|_Rest]=String, Accum, Line) when
  Digit >= $0, Digit =< $9 ->
	{Number, Rest} = scan_number(String),
	scan(Rest, [{number, Line, Number}|Accum], Line);
scan([$-, Digit|_Rest]=String, Accum, Line) when
  Digit >= $0, Digit =< $9 ->
	{Number, Rest} = scan_number(tl(String)),
	scan(Rest, [{number, Line, -Number}|Accum], Line);
scan([$\n|Rest], Accum, Line) ->
	scan(Rest, Accum, Line+1);
scan([C|Rest], Accum, Line) when
  C =:= 32; C =:= $\t ->
	scan(Rest, Accum, Line);
scan([$/, $/|Rest], Accum, Line) ->
	scan(skip_to_newline(Rest), Accum, Line);
scan([$/, $*|Rest], Accum, Line) ->
	{RestString, Line1}=skip_comment(Rest, Line),
	scan(RestString, Accum, Line1);
scan([$"|_]=String, Accum, Line) ->
	{ResString, Rest, Line1}=scan_string(String, Line),
	scan(Rest, [{string, Line, ResString}|Accum], Line1);
scan([C|_]=String, Accum, Line) when
  C >= $a, C =< $z;
  C >= $A, C =< $Z;
  C =:= $_ ->
	{KeyWordRes, Rest}=scan_identifier(String),
	Token = case get_keyword(KeyWordRes) of
				KeyWord when is_atom(KeyWord) ->
					{KeyWord, Line};
				{bareword, BareWord} ->
					{bareword, Line, BareWord}
			end,
	scan(Rest, [Token|Accum], Line);
scan([C|_], _Accum, Line) ->
    erlang:error({invalid_character, [C], Line}).


scan_number(String) ->
	{A, Result}=scan_integer(String, 0),
	case Result of
		[$.|Fraction] ->
			{B, Rest}=scan_identifier(Fraction),
			{A+list_to_float("0."++B), Rest};
		[$e|Exp] ->
			{B, Rest}=scan_integer(Exp),
			{list_to_float(integer_to_list(A)++".0e"++integer_to_list(B)), Rest};
		[$x|Rest1] when A =:= 0 ->
			{B, Rest}=scan_integer(Rest1),
			{list_to_integer(B, 16), Rest}
	end.

scan_integer(String) ->
	scan_integer(String, 0).

scan_integer([D|Rest], Acc) when
  D >= $0, D =< $9 ->
	scan_integer(Rest, Acc*10+(D - $0));
scan_integer(Rest, Acc) ->
	{Rest, Acc}.


scan_identifier(String) ->
	scan_identifier(String, "").

scan_identifier([C|Rest], Acc) when
	C >= $0, C =< $9;
	C >= $a, C =< $z;
	C >= $A, C =< $Z;
	C =:= $_;
	C =:= $. ->
	scan_identifier(Rest, [C|Acc]);
scan_identifier(Rest, Acc) ->
	{lists:reverse(Acc), Rest}.


skip_to_newline([$\n|Rest]) ->
	Rest;
skip_to_newline([]) ->
	[];
skip_to_newline([_C|Rest]) ->
	skip_to_newline(Rest).

skip_comment([$*, $/|Rest], Line) ->
	{Rest, Line};
skip_comment([$\n|Rest], Line) ->
	skip_comment(Rest, Line+1);
skip_comment([_C|Rest], Line) ->
	skip_comment(Rest, Line).


scan_string([$"|String], Line) ->
    scan_string(String, "", Line).

%% @hidden
scan_string([$"|Rest], Accum, Line) ->
    {lists:reverse(Accum), Rest, Line};
scan_string([$\\, $a|Rest], Accum, Line) ->
    scan_string(Rest, [7|Accum], Line);
scan_string([$\\, $e|Rest], Accum, Line) ->
    scan_string(Rest, [$\e|Accum], Line);
scan_string([$\\, $f|Rest], Accum, Line) ->
    scan_string(Rest, [$\f|Accum], Line);
scan_string([$\\, $n|Rest], Accum, Line) ->
    scan_string(Rest, [$\n|Accum], Line);
scan_string([$\\, $r|Rest], Accum, Line) ->
    scan_string(Rest, [$\r|Accum], Line);
scan_string([$\\, $t|Rest], Accum, Line) ->
    scan_string(Rest, [$\t|Accum], Line);
scan_string([$\\, $v|Rest], Accum, Line) ->
    scan_string(Rest, [$\v|Accum], Line);
scan_string([$\\, D1, D2, D3|Rest], Accum, Line)
  when D1 >= $0, D1 =< $7, D2 >= $0, D2 =< $7, D3 >= $0, D3 =< $7 ->
    scan_string(Rest, [erlang:list_to_integer([D1, D2, D3], 8)|Accum], Line);
scan_string([$\\, $x, H1, H2|Rest], Accum, Line) ->
    scan_string(Rest, [erlang:list_to_integer([H1, H2], 16)|Accum], Line);
scan_string([$\\, Char|Rest], Accum, Line) ->
    scan_string(Rest, [Char|Accum], Line);
scan_string([$\n|Rest], Accum, Line) ->
    scan_string(Rest, [$\n|Accum], Line + 1);
scan_string([Char|Rest], Accum, Line) ->
    scan_string(Rest, [Char|Accum], Line).


get_keyword("import") ->
    import;
get_keyword("package") ->
    package;
get_keyword("option") ->
    option;
get_keyword("message") ->
    message;
get_keyword("group") ->
    group;
get_keyword("enum") ->
    enum;
get_keyword("extend") ->
    extend;
get_keyword("service") ->
    service;
get_keyword("rpc") ->
    rpc;
get_keyword("required") ->
    required;
get_keyword("optional") ->
    optional;
get_keyword("repeated") ->
    repeated;
get_keyword("returns") ->
    returns;
get_keyword("extensions") ->
    extensions;
get_keyword("max") ->
    max;
get_keyword("to") ->
    to;
get_keyword("true") ->
    true;
get_keyword("false") ->
    false;
get_keyword(Bareword) ->
    {bareword, Bareword}.