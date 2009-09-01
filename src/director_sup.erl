-module(director_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SLEEP_CYCLE, 5000).
-define(PHONEHOME_TRIES,4).


%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE,[]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    {ok,[[AppDir]]} = init:get_argument(appdir),
    {ok,[[PidFile]]} = init:get_argument(pidfile),
    {ok,[[CmdName]]} = init:get_argument(cmdname),
    {ok,[[ConsoleNode]]} = init:get_argument(phonehome),
    {ok,[[Type]]} = init:get_argument(type),
    {ok,[[RestartTry]]} = init:get_argument(tries),
    {ok,[[RestartSecs]]} = init:get_argument(secs),

    Config = [{appdir,AppDir},{pidfile,PidFile},{cmd,CmdName},{type,Type}],

    error_logger:info_msg("Configuration: ~p~n",[Config]),

    ping_console(list_to_atom(ConsoleNode),?PHONEHOME_TRIES),
        
    EventServer = {director_node_events,{director_node_events,start,[]},permanent,2000,worker,[director_node_events]},
    DirectNode = {director_node,{director_node,start,[Config]},permanent,2000,worker,[director_node]},
    Pinger = {director_node_pinger,{director_node_pinger,start,
				    [ConsoleNode]},permanent,2000,worker,[director_node_pinger]},
    

    error_logger:info_msg("Starting the Supervisor with the following strategy: ~s tries within ~s milliseconds~n",
			  [RestartTry,RestartSecs]),

    {ok,{{one_for_all,list_to_integer(RestartTry),list_to_integer(RestartSecs)}, [EventServer,DirectNode,Pinger]}}.

%%====================================================================
%% Internal functions
%%====================================================================
ping_console(_Node,Tries) when Tries < 1 ->
    halt();

ping_console(Node,Tries) ->
    case net_adm:ping(Node) of
	pong -> 
	    ok;
	pang -> 
	    error_logger:error_msg("Cannot connect to phonehome: ~p. Did you start it?~n",[Node]),
	    error_logger:error_msg("Will exit after ~p more trie(s).~n",[Tries]),
	    timer:sleep(?SLEEP_CYCLE),
	    ping_console(Node,Tries - 1)
    end.
