%% @author Eric.yutao
%% @doc @todo Add description to directory_operator.


-module(directory_operator).

-define(DIR_LIST, directory_list).


%% ====================================================================
%% API functions
%% ====================================================================
-export([operator/2]).

-compile({inline, [if_do/3]}).
if_do(true, G1, _G2) ->
	G1;
if_do(false, _G1, G2) ->
	G2;
if_do(_Other, _G1, _G2) ->
	exit("error guard").


-spec operator(Directory, Func) -> no_return() when
		  Directory :: string(),
		  Func :: fun().
operator(Directory, Func) when is_function(Func, 1) ->
	case filelib:is_dir(Directory) of
		true ->
			erlang:process_flag(trap_exit, true),
			Self = self(),
			Jobs = lists:seq(1, 5),		%% Get Jobs from options
			Workers = [erlang:spawn_link(?MODULE, operator_worker, [Self, Func])||
						 _Index<-Jobs],
			DirRemoveSlash = string:strip(Directory, right, $/),
			operator_producer(Workers, [DirRemoveSlash]),
			ok;
		false ->
			exit(io_lib:format("Not found Direcotry:~p", [Directory]))
	end;
operator(_Dir, _Func) ->
	exit("Not operator func").


operator_producer([], []) ->
	down;
operator_producer(Workers, Targets) ->
	receive
		{directory, Dirs} ->
			FilterDirs = merge_dirs(Dirs, Targets),
			operator_producer(Workers, FilterDirs);
		{next, Pid} ->
			case Targets of
				[] -> Pid ! empty;
				[Directory|Rest] ->
					Pid ! {directory, Directory},
					operator_producer(Workers, Rest)
			end;
		{'EXIT', From, _Reason} ->
			operator_producer(lists:delete(From, Workers), Targets)
	end.


operator_worker(Parent, Func) ->
	Parent ! next,
	receive
		{directory, Directory} ->
			DirContents = file:list_dir(Directory),
			do_func_in_files(DirContents, Func),
			operator_worker(Parent, Func);
		empty ->
			ok
	end.


do_func_in_files([], _Func) ->
	ok;
do_func_in_files([FileOrDir|Rest], Func) ->
	todo.


merge_dirs([Dir|Tail], Targets) ->
	case lists:member(Dir, Targets) of
		true ->
			merge_dirs(Tail, Targets);
		false ->
			merge_dirs(Tail, [Targets|Dir])
	end;
merge_dirs([], Targets) ->
	Targets.

