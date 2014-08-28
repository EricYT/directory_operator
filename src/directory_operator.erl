%% @author Eric.yutao
%% @doc @todo Bad design, producer's behaviour is affected by customer


-module(directory_operator).

-define(DIR_LIST, directory_list).


%% ====================================================================
%% API functions
%% ====================================================================
-export([operator/2, operator_worker/2, trave_directory/1]).

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
			erlang:register(producer, Self),
			Jobs = lists:seq(1, 5),		%% Get Jobs from options
			Workers = [erlang:spawn_link(?MODULE, operator_worker, [Self, Func])||
						 _Index<-Jobs],
			DirRemoveSlash = string:strip(Directory, right, $/),
			operator_producer(Workers, [DirRemoveSlash]);
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
				[] ->
					Pid ! empty,
					Rest = [];
				[Directory|Rest] ->
					Pid ! {directory, Directory}
			end,
			operator_producer(Workers, Rest);
		{'EXIT', From, _Reason} ->
			operator_producer(lists:delete(From, Workers), Targets)
	end.


operator_worker(Parent, Func) ->
	Parent ! {next, self()},
	receive
		{directory, Directory} ->
			{ok, DirContents} = file:list_dir(Directory),
			Dirs = do_func_in_files(DirContents, Directory, Func, []),
			Parent ! {directory, Dirs},
			operator_worker(Parent, Func);
		empty ->
			operator_worker(Parent, Func)
	end.


do_func_in_files([], _ParentDir, _Func, Dirs) ->
	lists:reverse(Dirs);
do_func_in_files([FileOrDir|Rest], ParentDir, Func, Dirs) ->
	FullPathFileOrDir = filename:join(ParentDir, FileOrDir),
	case filelib:is_regular(FullPathFileOrDir) of
		true ->
			Func(io_lib:format("~s~n", [FullPathFileOrDir])),
			do_func_in_files(Rest, ParentDir, Func, Dirs);
		false ->
			case filelib:is_dir(FullPathFileOrDir) of
				true ->
					do_func_in_files(Rest, ParentDir, Func, [FullPathFileOrDir|Dirs]);
				false ->
					do_func_in_files(Rest, ParentDir, Func, Dirs)
			end
	end.


-spec trave_directory(Dir::list()) -> list() | false.
trave_directory(Dir) when is_list(Dir) ->
	case filelib:is_dir(Dir) of
		true ->
			case file:list_dir(Dir) of
				{ok, ContentFiles} ->
					trave_directorys(ContentFiles, Dir, []);
				{error, Error} ->
					Error
			end;
		false ->
			false
	end;
trave_directory(_) ->
	false.


trave_directorys([], _ParentDir, AccFiles) ->
	lists:reverse(AccFiles);
trave_directorys([FileOrDir|Tail], ParentDir, AccFiles) ->
	FullName = filename:join(ParentDir, FileOrDir),
	case filelib:is_regular(FullName) of
		true ->
			trave_directorys(Tail, ParentDir, [FullName|AccFiles]);
		false ->
			{ok, AllContent} = file:list_dir(FullName),
			WalkRes = trave_directorys(AllContent, FullName, []),
			trave_directorys(Tail, ParentDir, WalkRes++AccFiles)
	end.



merge_dirs([Dir|Tail], Targets) ->
	case lists:member(Dir, Targets) of
		true ->
			merge_dirs(Tail, Targets);
		false ->
			merge_dirs(Tail, Targets++[Dir])
	end;
merge_dirs([], Targets) ->
	Targets.
