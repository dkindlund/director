%% Handle starting a Ruby/Rake process following the Rails application layout
%% This application makes a best effort to handle Ruby application that are
%% started in a seperate thread.
%%
-module(director_ruby_process).
-export([start/1,stop/0,status/0]).

-define(KILL_WAIT_TIME,1000).

%% API
start(Config) ->
    spawn_link(fun() ->
		       register(?MODULE,self()),
		       process_flag(trap_exit,true),
		       Script = get_script(Config),
		       error_logger:info_msg("Starting script: ~s~n",[Script]),
		       PidFile = get_pidfile_name(Config),
		       AppDir = get_app_root(Config),
		       Port = open_port({spawn, Script},[nouse_stdio,{cd,AppDir}]),

		       %% Delay to let the process start and write the pidfile
		       timer:sleep(10000),
		       
		       %%error_logger:info_msg("Using Pidfile: ~s~n",[PidFile]),
		       
		       Pid = get_app_pid(PidFile),
		       Msg = "Starting Ruby process with PID: " ++ Pid,
		       director_node_events:event(Msg),
		       loop(Port,Pid,Script)
	       end).

stop() ->
    ?MODULE ! stop.

status() ->
    ?MODULE ! {self(),status},
    receive Data -> Data end.
	

%% Internal code    
loop(Port,Pid,Script) ->
    receive
	{From,status} ->
	    Data = check_status(Pid,Script),
	    From ! Data,
	    loop(Port,Pid,Script);
	stop ->
	    director_node_events:event("Stopping the Ruby process"),
	    kill_pid(Pid,false);
	{'EXIT',_,_Reason} ->
	    %%error_logger:info_msg("Exited..~p~n",[Reason]),
	    exit(eDied)
    end.

get_app_pid(PidFile) ->
    case file:read_file(PidFile) of
	{error,_} ->
	    error_logger:error_msg("Cannot read pid file: ~p~n",[PidFile]),
	    exit(eBadPidFile);
	{ok,Pid} ->
	    binary_to_list(Pid)
    end.

get_script(Config) ->
    %director_helper:build_rake_command(proplists:get_value(appdir,Config),proplists:get_value(cmd,Config)).
    director_helper:build_rake_command(proplists:get_value(cmd,Config)).

get_pidfile_name(Config) ->
    director_helper:pidfile_location(proplists:get_value(appdir,Config),
				   proplists:get_value(pidfile,Config)).

get_app_root(Config) ->
    director_helper:application_root(proplists:get_value(appdir,Config)).

kill_pid(Pid,Force) ->
    Cmd = if Force ->
		  "kill -9";
	     true ->
		  "kill -15"
	  end,
    os:cmd(Cmd ++ " " ++ Pid),
    timer:sleep(?KILL_WAIT_TIME),
    case is_dead(Pid) of
	true ->
	    error_logger:info_msg("Pid killed~n"),
	    ok;
	false ->
	    error_logger:info_msg("Pid NOT killed try again~n"),
	    kill_pid(Pid,true)
    end.
    
is_dead(Pid) ->
    Cmd = "ps up " ++ Pid ++ " | grep -v USER",
    case os:cmd(Cmd) of
	"" -> true;
	_Any -> false
    end.

check_status(Pid,Script) ->
    Cmd = "ps up " ++ Pid ++ " | grep -v USER",
    R = os:cmd(Cmd),
    case string:tokens(R,"\n ") of
	[] -> 
	    [];
	R1 ->
	    %%P = string:tokens(Script," "),
	    %%ShortScript = lists:nth(1,P) ++ " " ++ lists:nth(3,P),
	    [{script,Script},{cpu, lists:nth(3,R1)},{mem,lists:nth(6,R1)}]
    end.
    

