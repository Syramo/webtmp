-module(wt_dds).
-behaviour(gen_server).
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
    

%%------------------------------------------------------------------------------
%% synchronous calls

handle_call({get_doc,Path,Opt}, _From, State) when is_map(Opt) ->
	io:format("wt_dds:~nDocument:~p~nOptions:~p~n",[Path,Opt]),
	{reply, {ok, ""}, State}.
	


%%------------------------------------------------------------------------------
%% asynchronous calls

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Msg, State) ->
    {noreply, State}.
    
