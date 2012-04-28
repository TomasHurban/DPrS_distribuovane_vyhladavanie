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
	gen_server:start_link({global, util:random_id(20)}, ?MODULE, [], []).

%%
%% for central_server
%%

search(What, In, Pid) ->
	log("search: " ++ What),
	gen_server:call(Pid, {search, What, In}, 200000).

invalidate(Pid) ->
	MsgToLog = "invalidate: " ++ io_lib:format('~p', [Pid]),
	log(MsgToLog),
	gen_server:call(Pid, {invalidate}, 200000).

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
	log("initialization ..."),
	Parts = dict:new(),
	Id = util:random_id(16),
	State = #provider_state{
		id = Id,
		parts = Parts},
	State_new = do_connecting_to_central_server(State, dict:new()),
	{ok, State_new}.

terminate(normal, _State) ->
	log("termination ..."),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_info(Msg, State) ->
	io:format("Unexpected message: ~p~n", [Msg]),
	{noreply, State}.

handle_call({search, What, In}, _From, State) ->
	{
        reply,
        do_search([], dict:new(), What, In, State#provider_state.parts),
        State
    };
handle_call({get, PartName}, _From, State) ->
	FoundPart = dict:find(PartName, State#provider_state.parts),
	case FoundPart of
		{ok, PartInfo} ->
			MsgToLog = "part \"" ++ PartName ++ "\" found ",
			log(MsgToLog),
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
	end;
handle_call({invalidate}, _From, State) ->
	State_new = do_connecting_to_central_server(State, dict:new()),
	{
		reply,
		ok,
		State_new
	}.

handle_cast(_, _State) ->
	not_supported.

%%
%% Local Functions
%%

collect_missing_data(CurrentState, CurrentStateDiff, []) ->
	{CurrentState, CurrentStateDiff};
collect_missing_data(CurrentState, CurrentStateDiff, [UpdateList_H | UpdateList_T]) ->
	log("collect missing data ..."),
	{NewState, NewStateDiff} =
	case UpdateList_H of
		{as_data, PartName, PartVersion, PartData} ->
			log(io_lib:format("updating part ~p to version ~p from data ~p", [PartName, PartVersion, PartData])),
			{
				CurrentState#provider_state{
					parts = dict:store(PartName, #part_info{part_data = PartData, part_version = PartVersion}, CurrentState#provider_state.parts)
				},
				dict:store(PartName, PartVersion, CurrentStateDiff)
			};
		{from_provider, PartName, ProviderPid} ->
			log(io_lib:format("updating part ~p from provider", [PartName])),
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
	log("connecting to central server ..."),
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

do_search(CurrentResults, CurrentTimes, _What, [], _Parts) ->
	{CurrentResults, CurrentTimes};
do_search(CurrentResults, CurrentTimes, What, [In_H | In_T], Parts) ->
	PartInfo = dict:fetch(In_H, Parts),
	TimeBefore = util:get_time(),
	Pos = string:str(PartInfo#part_info.part_data, What),
	TimeAfter = util:get_time(),
	if
		Pos > 0 ->
			do_search([In_H | CurrentResults], dict:store(In_H, TimeAfter - TimeBefore, CurrentTimes) , What, In_T, Parts);
		true ->
			do_search(CurrentResults, dict:store(In_H, TimeAfter - TimeBefore, CurrentTimes), What, In_T, Parts)
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
