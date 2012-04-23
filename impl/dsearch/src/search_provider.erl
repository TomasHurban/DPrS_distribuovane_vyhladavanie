-module(search_provider).
-behaviour(gen_server).

-export([create_and_activate/0]).
-export([search/3, invalidate/1, get/2]).

-export([init/1, terminate/2, code_change/3, handle_info/2]).
-export([handle_call/3, handle_cast/2]).

-record(provider_state, {id, parts}).
-record(part_info, {part_version, part_data}).

%%
%% for administration
%%

create_and_activate() ->
	log("search provider started ..."),
	gen_server:start_link({global, random_id(20)}, ?MODULE, [], []),

%%
%% for central_server
%%

search(What, In, Pid) ->
	log("search: " ++ What),
	gen_server:call(Pid, {search, What, In}, 200000).

invalidate(Pid) ->
	MsgToLog = "invalidate: " ++ io_lib:format('~p', [Pid]),
	log(MsgToLog),
	gen_server:cast(Pid, {invalidate}).

%%
%% for another search provider
%%

get(PartName, Pid) ->
	MsgToLog = "get: " ++ PartName ++ ", " ++ io_lib:format('~p', [Pid]),
	log(MsgToLog),
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
	State_new = do_connecting_to_central_server(State, dict:new()),
	{ok, State_new}.

terminate(normal, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_info(Msg, State) ->
	io:format("Unexpected message: ~p~n", [Msg]),
	{noreply, State}.

handle_call({search, What, In}, _From, State) ->
	{
        reply,
        {ok, do_search([], What, In, State#provider_state.parts)},
        State
    };
handle_call({get, PartName}, _From, State) ->
	FoundPart = dict:find(PartName, State#provider_state.parts),
	case FoundPart of
		{ok, PartInfo} ->
			{
		        reply,
		        {ok},
		        PartInfo#part_info.part_version,
				PartInfo#part_info.part_data
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
		do_connecting_to_central_server(State, dict:new())
	}.
	
%%
%% Local Functions
%%

random_id(0) ->
	[];
random_id(Length) ->
	random:seed(erlang:now()),
	[random:uniform(26) + 64 | random_id(Length - 1)].

collect_missing_data(CurrentState, CurrentStateDiff, []) ->
	{CurrentState, CurrentStateDiff};
collect_missing_data(CurrentState, CurrentStateDiff, [UpdateList_H | UpdateList_T]) ->
	{NewState, NewStateDiff} =
	case UpdateList_H of
		{as_data, PartName, PartVersion, PartData} ->
			{
				CurrentState#provider_state{
					parts = dict:store(PartName, #part_info{part_data = PartData, part_version = PartVersion}, CurrentState#provider_state.parts)
				},
				dict:store(PartName, PartVersion, CurrentStateDiff)
			};
		{from_provider, PartName, ProviderPid} ->
			ProviderResponse = search_provider:get(PartName, ProviderPid),
			case ProviderResponse of
				{ok, PartVersion, PartData} ->
					{
						CurrentState#provider_state{
							parts = dict:store(PartName, #part_info{part_data = PartData, part_version = PartVersion}, CurrentState#provider_state.parts)
						},
						dict:store(PartName, PartVersion, CurrentStateDiff)
					};
				_ ->
					{CurrentState, CurrentStateDiff}
					% when the data could not be obtained from other provider, this provider connects with the StateDiff
					% which does not contain this data part, so the server suggests him some new provider to obtain the part from
			end
	end,
	collect_missing_data(NewState, NewStateDiff, UpdateList_T).

do_connecting_to_central_server(State, StateDiff) ->
	ConnectResponse = central_server:connect(State#provider_state.id, StateDiff),
	StateAfterCollected = 
	case ConnectResponse of
		ok ->
			State;
		{update, UpdateList} ->
			{NewState, NewStateDiff} = collect_missing_data(State, dict:new(), UpdateList),
			do_connecting_to_central_server(NewState, NewStateDiff),
			NewState
	end,
	StateAfterCollected.

do_search(CurrentResults, _What, [], _Parts) ->
	CurrentResults;
do_search(CurrentResults, What, [In_H | In_T], Parts) ->
	PartInfo = dict:fetch(In_H, Parts),
	Pos = string:str(PartInfo#part_info.part_data, What),
	if
		Pos > 0 ->
			do_search([In_H | CurrentResults], What, In_T, Parts);
		true ->
			do_search(CurrentResults, What, In_T, Parts)
	end.

log(What) ->
	{ok, WriteDescr} = file:open("log.txt", [raw, append]),
	{Date={Year, Month, Day},Time={Hour, Minutes, Seconds}} = erlang:localtime(),
	DateTime = io_lib:format('~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b', [Year, Month, Day, Hour, Minutes, Seconds]),
	Module = "provider",
	Message = "-> " ++ What ++ "\r\n",
	TextToLog = DateTime ++ " [" ++ Module ++ "] " ++ Message,
	file:write(WriteDescr, TextToLog),
	file:close(WriteDescr).
