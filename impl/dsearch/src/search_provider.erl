-module(search_provider).
-behaviour(gen_server).

-export([create_and_activate/0]).
-export([search/3, invalidate/1, get/2]).

-export([init/1, terminate/2, code_change/3, handle_info/2]).
-export([handle_call/3, handle_cast/2]).

-record(provider_state, {id, parts}).

%%
%% for administration
%%

create_and_activate() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%%
%% for central_server
%%

search(What, In, Pid) ->
	gen_server:call(Pid, {search, What, In}).

invalidate(Pid) ->
	gen_server:cast(Pid, {invalidate}).

%%
%% for another search provider
%%

get(PartName, Pid) ->
	gen_server:call(Pid, {get, PartName}).

%%
%% Callbacks
%%

init([]) ->
	Parts = dict:new(),
	Id = random_id(16),
	State = #provider_state{
		id = Id,
		parts = Parts},
	{ok, do_connecting_to_central_server(State)}.

terminate(normal, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_info(Msg, State) ->
	io:format("Unexpected message: ~p~n", [Msg]),
	{noreply, State}.

handle_call({search, What, In}, _From, State) ->
	not_implemented;
handle_call({get, PartName}, _From, State) ->
	FoundPart = dict:find(PartName, State#provider_state.parts),
	case FoundPart of
		{ok, PartData} ->
			{
		        reply,
		        {ok},
		        PartData
		    };
		error ->
			{
		        reply,
		        {part_not_found},
		        State
		    }
	end.

handle_cast({invalidate}, State) ->
	{
		noreply,
		do_connecting_to_central_server(State)
	}.
	
%%
%% Local Functions
%%

random_id(0) ->
	[];
random_id(Length) ->
	[random:uniform(26) + 64 | random_id(Length - 1)].

collect_missing_data(State, []) ->
	State;
collect_missing_data(State, [UpdateList_H | UpdateList_T]) ->
	NewState =
	case UpdateList_H of
		{as_data, PartName, PartData} ->
			State#provider_state{
				parts = dict:store(PartName, PartData, State#provider_state.parts)
			};
		{from_provider, PartName, ProviderPid} ->
			ProviderResponse = search_provider:get(PartName, ProviderPid),
			case ProviderResponse of
				{ok, PartData} ->
					State#provider_state{
						parts = dict:store(PartName, PartData, State#provider_state.parts)
					};
				_ ->
					State
					% when the data could not be obtained from other provider, this provider connects with the StateDiff
					% which does not contain this data part, so the server suggests him some new provider to obtain the part from
			end
	end,
	collect_missing_data(NewState, UpdateList_T).

do_connecting_to_central_server(State) ->
	ConnectResponse = central_server:connect(State#provider_state.id, dict:new()),
	StateAfterCollected = 
	case ConnectResponse of
		ok ->
			State;
		UpdateList ->
			collect_missing_data(State, UpdateList)
			% TODO: build new StateDiff and connect
	end,
	StateAfterCollected.
