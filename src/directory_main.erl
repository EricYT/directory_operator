%% @author Eric.yutao
%% @doc @todo Add description to directory_main.


-module(directory_main).

%% ====================================================================
%% API functions
%% ====================================================================
-export([main/1]).


-spec main(Options) -> no_return() when
		  Options :: [term()].
main([]) ->
	todo;
main(Options) when is_list(Options) ->
	todo.

