-module(wt_cache).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2]).

% API

-export([prune/1,get_doc/1,get_doc/2,doc_state/1,doc_state/2,rem_doc/1,rem_doc/2,add_doc/3]).


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
    

%%==========API=================================================================

prune(#{what := _} = Opt) ->
	gen_server:cast(?MODULE,{prune, #{options => Opt}}).

get_doc(Name) ->
	gen_server:call(?MODULE,{get_doc, #{name => Name}}).
	
get_doc(Name,Key) ->
	gen_server:call(?MODULE,{get_doc, #{name => Name, key => Key}}).
	
doc_state(Name) ->
	gen_server:call(?MODULE,{doc_state, #{name => Name}}).
	
doc_state(Name,Key) ->
	gen_server:call(?MODULE,{doc_state, #{name => Name, key => Key}}).
	
rem_doc(Name) ->
	gen_server:cast(?MODULE,{rem_doc, #{name => Name}}).
	
rem_doc(Name, Key) ->
	gen_server:cast(?MODULE,{rem_doc, #{name => Name, key => Key}}).
	
add_doc(Name, Cont, Opt) ->
	gen_server:call(?MODULE,{add_doc, #{name => Name, options => Opt, content => Cont}}).


%%------------------------------------------------------------------------------
%% action callbacks

handle_call({get_doc, Doc}, _From, State) ->
	%%#{name := Name, key := Key} = Doc,
	{reply, {error, na}, State};
	

handle_call({add_doc, Doc}, _From, State) ->
	%%#{name := Name, content := Cont, options => Opt, key => Key} = Doc,
	{reply, ok, State};
	
	
handle_call({doc_state, Doc}, _From, State) ->
	%%#{name := Name, key => Key} = Doc,
	{reply, {error, na}, State}.
	
	
	
handle_cast({prune, #{options := Opt}}, State) when is_map(Opt) ->
	io:format("pruning cache: ~p~n",[Opt]),
    {noreply, State};

    
handle_cast({rem_doc, #{name := Name}}, State) ->
	io:format("removing document from cache: ~p~n",[Name]),
	{noreply, State};


handle_cast(stop, State) ->
    {stop, normal, State}.
	

	


%%==============================================================================
%% internals



    
