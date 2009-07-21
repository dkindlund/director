-module(director_node).

-behaviour(gen_server).

%% API
-export([start/1,start_worker/0,stop_worker/0,status/0,list_rails_apps/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%
%% This module is always running and is supervised.  You can stop and restart the worker and queue reader by setting
%% the allow_exit flag to true. Config state will be maintained on stop and used to restart workers if desired.
%% If something dies on it's own, it will be automatically restarted 
%% 

%% allow_exit   : if false restart the connector
%% config       : configuration information
%% queue_pid    : Pid of the queue reader
%% running      : are the workers running?
-record(state, {allow_exit,config,running}).

%%====================================================================
%% API
%%====================================================================

start(Config) ->
    gen_server:start_link({local, ?MODULE},?MODULE,Config,[]).

start_worker() ->
    gen_server:call(?MODULE,{start_worker}).

stop_worker() ->
    gen_server:call(?MODULE,{stop_worker}).

status() ->
    gen_server:call(?MODULE,status).

list_rails_apps() ->
    gen_server:call(?MODULE,list_rails_apps).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Config) ->
    process_flag(trap_exit,true),
    State = #state{config=Config},
    State1 = start_services(State),
    error_logger:info_msg("Node started~n"),
    {ok, State1}.

handle_call({start_worker},_From,State) ->
    case State#state.running of
	true ->
	    error_logger:error_msg("Cannot start the worker. It's already running~n"),
	    {reply,ok,State};
	false ->
	    State1 = start_services(State),
	    {reply,ok,State1}
    end;

handle_call({stop_worker},_From,State) ->
    State1 = stop_services(State),
    {reply,ok,State1};

handle_call(status,_From,State) ->
    Node = atom_to_list(node()),
    Reply = case State#state.running of
		true ->
		    Info = director_ruby_process:status(),
		    {"true",[[{node,Node}|Info]]};
		false ->
		    {"false",[[{node,Node}]]}
	    end,
    {reply,Reply,State};

handle_call(list_rails_apps,_From,State) ->
    List = director_helper:list_rails_apps(),
    {reply,List,State}.
	    
handle_cast(_Msg, State) ->
    {noreply, State}.

%% Trap the exit signal from the connector
%% Need to set the process_flag
handle_info({'EXIT',_Pid,_Reason},State) ->
    %% Kill the connector but keep me running for another day
    case State#state.allow_exit of
	true ->
	    error_logger:info_msg("Controlled Stop...~n");
	false ->
	    error_logger:error_msg("Something crashed...let the sup handle it~n"),
	    exit(eDied)
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

start_services(State) ->
    director_ruby_process:start(State#state.config),
    State#state{allow_exit=false,running=true}.

stop_services(State) ->
    State1 = State#state{allow_exit=true,running=false},
    %% Stop the worker
    director_ruby_process:stop(),
    State1.
    
    

