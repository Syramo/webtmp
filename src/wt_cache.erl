-module(wt_cache).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2]).



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
stop() ->
	gen_server:cast(?MODULE, stop).
	
	
init(Args) ->
    {ok, Args}.
    
    
terminate(_Reason, _State) ->
    ok.
    

handle_info(_Msg, State) ->
    {noreply, State}.
    

%%------------------------------------------------------------------------------
%% action callbacks

handle_call(What, _From, State) ->
	io:format("wt_cache: got call ~p~n",[What]),
	{reply, ok, State}.
	
	
	
handle_cast({prune, Opt}, State) when is_map(Opt) ->
	io:format("pruning cache: ~p~n",[Opt]),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State}.
	

	


%%==============================================================================
%% internals



    
