-module(api_controller,[Env]).

-export([handle_request/2,before_filter/0]).

%% WORK IN PROGRESS


%% GET /api
handle_request("index",[]) ->
    %% Load nodes: Calling fetch_node is flacky right now
    Result = fetch_node_status(),
    error_logger:info_msg("result: ~p~n",[Result]),
    Data = mochijson2:encode(Result),
    {json,200,Data};

%% GET /api/events/worker1234
handle_request("events",[Name]) ->
    Reply = case rpc:call(list_to_atom(Name),director_node_events,get_events,[]) of
		{badrpc,Reason} ->
		    {struct,[{node,Name},{msgs,[Reason]}]};
		Any ->
		    {struct,[{node,Name},{msgs,Any}]}
	    end,
    Data = mochijson2:encode(Reply),
    {json,200,Data};

%% GET /api/node/worker1234/start
handle_request("node",[Name,Command]) ->
    case Command of
	"start" ->
	    error_logger:info_msg("Start the node: ~s!",[Name]),
	    rpc:call(list_to_atom(Name),director_node,start_worker,[]);
	"stop" ->
	    error_logger:info_msg("Stop the node: ~s!",[Name]),
	    rpc:call(list_to_atom(Name),director_node,stop_worker,[])
    end,
    
    {json,200,mochijson2:encode("ok")}.
    
before_filter() ->
    ok.

%% PROBLEM VALUES ARE RAW LISTS IN THE JSON!!!
fetch_node_status() ->
    {Resp,_BadNodes} = rpc:multicall(nodes(),director_node,status,[],5000),
    lists:foldl(fun(E,Acc) ->
			{Value,[Rest]} = E,
			R1 = [{"running",list_to_binary(Value)}|Rest],
			[{struct,R1}|Acc]
		end,[],Resp).
