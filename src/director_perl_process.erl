-module(director_perl_process).
-export([start/1,stop/0,status/0]).

-define(KILL_WAIT_TIME,1000).


%% director.yml expects:
%% a configuration for Perl scripts like this:
%% perlscript
%%  basedir: Fullpath to the script
%%  scriptname: "test.pl"
%%  phonehome: "console@..."
%%  type: "perl"
start(Config) ->
    spawn_link(fun() ->
		       register(?MODULE,self()),
		       process_flag(trap_exit,true),
		       {ScriptBaseDir,PerlCmd} = get_dir_and_cmd(Config),

		       Cmd = "perl " ++ lists:flatten(PerlCmd),
		       
		       Port = open_port({spawn, Cmd},
					[{line, 10000},
					 {cd,lists:flatten(ScriptBaseDir)},
					 binary,exit_status,
					 use_stdio]),

		       loop(Port,PerlCmd,-1)
	       end).

stop() ->
    ?MODULE ! stop.

status() ->
    ?MODULE ! {self(),status},
    receive Data -> Data end.

loop(Port,PerlCmd,Pid) ->
    receive
	{Port,{data,{eol,<<"<PID>",Line/binary>>}}} ->
	    P1 = binary_to_list(Line),
	    Info = "Storing Perl process PID: " ++ P1,
	    director_node_events:event(Info),
	    loop(Port,PerlCmd,P1);
	stop ->
	    Info = "Attempting to stop Perl process: " ++ Pid,
	    director_node_events:event(Info),
	    kill_pid(Pid,false);
	{From,status} ->
	    Data = check_status(Pid,PerlCmd),
	    From ! Data,
	    loop(Port,PerlCmd,Pid);
	{Port,{data,{eol,<<"INFO",Line/binary>>}}} ->
	    director_node_events:event(Line),
	    loop(Port,PerlCmd,Pid);
	{Port,{data,{eol,<<"ERROR",Line/binary>>}}} ->
	    director_node_events:event(Line),
	    loop(Port,PerlCmd,Pid);
	{'EXIT',_Port,_Reason} ->
	    exit(eDied)
    end.
    

kill_pid(Pid,Force) ->
    Cmd = if Force ->
		  "kill -9";
	     true ->
		  %% Send SIGINT
		  "kill -2"
	  end,
    os:cmd(Cmd ++ " " ++ Pid),
    timer:sleep(5000),
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
	    [{script,Script},{cpu, lists:nth(3,R1)},{mem,lists:nth(6,R1)}]
    end.

get_dir_and_cmd(Config) ->
    {proplists:get_value(appdir,Config),proplists:get_value(cmd,Config)}.

	
    


    
