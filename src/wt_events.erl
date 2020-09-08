-module(wt_events).
-export([start_link/0, add/2, delete/2]).

-export([emmit/1, emmit/2]).


start_link() ->
    gen_event:start_link({local, ?MODULE}).


add(Mod,Args) -> gen_event:add_sup_handler(?MODULE, Mod, Args).

delete(Mod,Args) -> gen_event:delete_handler(?MODULE,Mod,Args).



emmit(Evt) ->
	emmit(Evt, false).
	
emmit(Evt, Sync) ->
	case Sync of
		true -> gen_event:sync_notify(?MODULE, Evt);
		_	 -> gen_event:notify(?MODULE, Evt)
	end.
	
	
%% more specific API to come below
