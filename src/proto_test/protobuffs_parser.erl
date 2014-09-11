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
	todo.

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






