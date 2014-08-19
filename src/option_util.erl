%% @author Eric.yutao
%% @doc @todo Add description to option_util.


-module(option_util).
-author('eric.yutao').

%% ====================================================================
%% API functions
%% ====================================================================
-export([parse/1]).


-spec parse(Options) -> NewOptions when
		  Options :: [Option, ...],
		  NewOptions :: [Option, ...],
		  Option :: {term(), term()}.
parse(Options) when is_list(Options) ->
	parse(Options, []).

parse([], AccOptions) ->
	lists:reverse(AccOptions);
parse([{dir, Dir}|Tail], AccOptions) when is_list(Dir) ->
	parse(Tail, [{directory, Dir}|AccOptions]);
parse([{directory, _Dir}=Option|Tail], AccOptions) ->
	parse(Tail, [Option|AccOptions]);
parse([{cmd, CMD}|Tail], AccOptions) ->
	parse(Tail, [{cmd, CMD}|AccOptions]);
parse([{func, Func}|Tail], AccOptions) when erlang:is_function(Func, 1) ->
	parse(Tail, [{function, Func}|AccOptions]);
parse([{function, Func}|Tail], AccOptions) when erlang:is_function(Func, 1) ->
	parse(Tail, [{function, Func}|AccOptions]);
parse([_Error|_], _AccOptions) ->
	exit("error options").

