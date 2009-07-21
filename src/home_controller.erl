%%
%%
-module(home_controller,[Env]).

-export([handle_request/2,before_filter/0]).

handle_request("index",[]) ->
    %% Load nodes: Calling fetch_node is flacky right now
    Result = fetch_node_status(),
    Data = {nodes,Result},
    {render,"home/index.html",[Data]};

handle_request("node",[Name,Command]) ->
    case Command of
	"start" ->
	    error_logger:info_msg("Start the node: ~s!",[Name]),
	    rpc:call(list_to_atom(Name),director_node,start_worker,[]);
	"stop" ->
	    error_logger:info_msg("Stop the node: ~s!",[Name]),
	    rpc:call(list_to_atom(Name),director_node,stop_worker,[])
    end,
    {redirect,"/"}.


%% Ignore for now
before_filter() ->
    ok.


fetch_node_status() ->
    {Resp,_BadNodes} = rpc:multicall(nodes(),director_node,status,[],5000),
    Resp.
    


    
