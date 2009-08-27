-module(director_node_pinger).

-export([init/2,start/1]).
-export([system_continue/3,system_terminate/4,write_debug/3]).

-define(TIME_OUT,120000). % 2 minutes

start(Console) ->
    proc_lib:start_link(?MODULE, init,[self(),Console]).

init(Parent,Console) ->
    register(?MODULE,self()),
    Debug = sys:debug_options([]),
    proc_lib:init_ack(Parent,{ok,self()}),
    loop(Parent,list_to_atom(Console),Debug).

loop(Parent,Console,Debug) ->
    receive
	{system,From,Request} ->
	    sys:handle_system_msg(Request,From,Parent,?MODULE,Debug,Console)
    after
	?TIME_OUT ->
	    case net_adm:ping(Console) of
		pong -> 
		    ok;
		pang -> 
		    error_logger:error_msg("Pinger failed. Cannot connect to phonehome: ~p.~n",[Console])
	    end,
	    Debug1 = sys:handle_debug(Debug,{?MODULE,write_debug},?MODULE,{done,console_pinger}), 
	    loop(Parent,Console,Debug1)
    end.

system_continue(Parent,Debug,Console) ->
     loop(Parent,Console,Debug).

system_terminate(Reason,_Parent,_Debug,ok) ->
    exit(Reason).

write_debug(Dev,Event,Name) ->
    io:format(Dev, "~p event is ~p~n",[Name,Event]).
