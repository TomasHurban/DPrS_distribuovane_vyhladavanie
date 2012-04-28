-module(central_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([update/2, search/1, search_with_retries/1, connect/2, disconnect/1]).
-export([test_remove_waiting_parts/0]).

-export([init/1, terminate/2, code_change/3, handle_info/2]).
-export([handle_call/3, handle_cast/2]).

-export([search_using_provider/5]).

-record(server_state, {providers, parts, connected_providers, waiting_parts}).
-record(part_info, {current_version, providers}).
-record(waiting_part_info, {part_data, copy_requests_to_providers}).

-define(PREFERRED_COPIES_COUNT, 4).

%%
%% for administration
%%

start_link() ->
	log("central server started ..."),
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

update(PartName, PartData) ->
	MsgToLog = "update: " ++ PartName ++ ", " ++ PartData,
	log(MsgToLog),
	gen_server:call({global, ?MODULE}, {update, PartName, PartData}, 200000).

%%
%% for client
%%

search(What) ->
	log("search: " ++ What),
	gen_server:call({global, ?MODULE}, {search, What}, 200000).

search_with_retries(What) ->
	log("search with retry: " ++ What),
	Result = gen_server:call({global, ?MODULE}, {search, What}, 200000),
	case Result of
		{ok, Results} ->
			Results;
		not_all_parts_available ->
			timer:sleep(1000), % maybe some provider providing the missing parts comes
			search_with_retries(What);
		SomethingElse ->
			SomethingElse
	end.

%%
%% for search provider
%%

connect(ProviderId, StateDiff) ->
	MsgToLog = "connected provider ID: " ++ io_lib:format('~p', [ProviderId]),
	log(MsgToLog),
	gen_server:call({global, ?MODULE}, {connect, ProviderId, StateDiff}, 200000).

disconnect(ProviderId) ->
	MsgToLog = "disconnected provider ID: " ++ io_lib:format('~p', [ProviderId]),
	log(MsgToLog),
	gen_server:call({global, ?MODULE}, {disconnect, ProviderId}, 200000).

%%
%% tests
%%

test_remove_waiting_parts() ->
	gen_server:call({global, ?MODULE}, {test_remove_waiting_parts}, 200000).

%%
%% Callbacks
%%

init([]) ->
	log("initialization ..."),
	Providers = dict:new(),
	Parts = dict:new(),
	ConnectedProviders = dict:new(),
	WaitingParts = dict:new(),
	{ok, #server_state{providers = Providers, parts = Parts, connected_providers = ConnectedProviders, waiting_parts = WaitingParts}}.

terminate(normal, _State) ->
	log("termination ..."),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_info(Msg, State) ->
	io:format("Unexpected message: ~p~n", [Msg]),
	{noreply, State}.

handle_call({update, PartName, PartData}, _From, State) ->
	FoundPart = dict:find(PartName, State#server_state.parts),
	{NewVersion, AlreadyInvalidatedProviderIds, ConnectedProviders_New} =
	case FoundPart of
		{ok, PartInfo} ->
			MsgToLog = "updating part \"" ++ PartName ++ "\" to version " ++ io_lib:format('~p', [PartInfo#part_info.current_version + 1]),
			log(MsgToLog),
			{AlreadyInvalidatedProviderIds_tmp, ConnectedProviders_New_tmp} = invalidate_providers_with_part(sets:new(), dict:to_list(PartInfo#part_info.providers), State#server_state.connected_providers),
			{
				PartInfo#part_info.current_version + 1,
				AlreadyInvalidatedProviderIds_tmp,
				ConnectedProviders_New_tmp
			};
		error ->
			log(PartName ++ " version set to 1"),
			{
				1,
				sets:new(),
				State#server_state.connected_providers
			}
	end,
	
	ConnectedProviders_New2 = invalidate_random_providers(AlreadyInvalidatedProviderIds, dict:to_list(State#server_state.connected_providers), ?PREFERRED_COPIES_COUNT - sets:size(AlreadyInvalidatedProviderIds), ConnectedProviders_New),
	
	{
        reply,
        {ok},
        State#server_state{
			parts = dict:store(
				PartName,
				#part_info{
					current_version = NewVersion,
					providers = dict:new()
				},
				State#server_state.parts
			),
	    	waiting_parts = dict:store(
				PartName,
				#waiting_part_info{part_data = PartData, copy_requests_to_providers = dict:new()},
				State#server_state.waiting_parts
			),
			connected_providers = ConnectedProviders_New2
		}
    };
handle_call({search, What}, _From, State) ->
	SearchDistribution = build_search_distribution(dict:new(), dict:to_list(State#server_state.parts), State#server_state.connected_providers),
	InvalidItem = dict:find(not_found, SearchDistribution),
	case InvalidItem of
		{ok, _} ->
			{
		        reply,
		        not_all_parts_available,
				State
		    };
		error ->
			SpawnIds = distribute_search(sets:new(), dict:to_list(SearchDistribution), What),
			{Results, NewParts} = collect_results([], SpawnIds, State#server_state.parts, State#server_state.connected_providers),
			{
		        reply,
		        {ok, Results},
				State#server_state{
					parts = NewParts
				}
		    }
	end;
handle_call({connect, ProviderId, StateDiff}, From, State) ->
	Providers = State#server_state.providers,
	FoundProvider = dict:find(ProviderId, Providers),
	PartsInProvider_new = 
	case FoundProvider of
		{ok, PartsInProvider} ->
			PartsInProvider;
		error ->
			dict:new()
	end,
	PartsInProvider_new2 = merge_parts_in_provider(PartsInProvider_new, dict:to_list(StateDiff)),
	Providers_new = dict:store(ProviderId, PartsInProvider_new2, Providers),
	
	Parts = State#server_state.parts,
	Parts_new = merge_providers_in_parts(Parts, dict:to_list(StateDiff), ProviderId),
	WaitingParts_new = remove_waiting_parts(State#server_state.waiting_parts, dict:to_list(Parts_new)),
	UpdateList = build_update_list([], dict:to_list(Parts_new), PartsInProvider_new2, State#server_state.waiting_parts, State#server_state.connected_providers),
	log(io_lib:format("created update list: ~p", [UpdateList])),
	
	ConnectedProviders_new = 
	case UpdateList of
		[] ->
			{Ref, _} = From,
			dict:store(ProviderId, Ref, State#server_state.connected_providers);
		_ ->
			dict:erase(ProviderId, State#server_state.connected_providers)
	end,

	State_new = State#server_state{
    	providers = Providers_new,
		parts = Parts_new,
		connected_providers = ConnectedProviders_new,
		waiting_parts = WaitingParts_new
	},
	
	case UpdateList of
		[] ->
			log(io_lib:format("provider connected: ~p", [ProviderId])),
			{
		        reply,
		        ok,
		        State_new
		    };
		_ ->
			log(io_lib:format("asking provider to update data: ~p", [ProviderId])),
			{
		        reply,
		        {update, UpdateList},
		        State_new
		    }
	end;
handle_call({disconnect, ProviderId}, _From, State) ->
	{
        reply,
        ok,
        State#server_state{
			connected_providers = dict:erase(ProviderId, State#server_state.connected_providers)
		}
    };	
handle_call({test_remove_waiting_parts}, _From, State) ->
	{
        reply,
        ok,
        State#server_state{
			waiting_parts = dict:new()
		}
    }.

handle_cast(_, _State) ->
	nothing.

%%
%% Process begin functions
%%

search_using_provider(What, SearchIn, ProviderPid, ParentPid, SpawnId) ->
	{SearchResults, Times} = search_provider:search(What, SearchIn, ProviderPid),
	ParentPid ! {SpawnId, SearchResults, Times, ProviderPid}.

%%
%% Local Functions
%%

merge_parts_in_provider(CurrentPartsInProvider, []) ->
	CurrentPartsInProvider;
merge_parts_in_provider(CurrentPartsInProvider, [StateDiffList_H | StateDiffList_T]) ->
	log("merge parts ..."),
	{PartName, PartVersion} = StateDiffList_H,
	
	merge_parts_in_provider(
		dict:store(
		  	PartName,
			PartVersion,
			CurrentPartsInProvider
		),
		StateDiffList_T
	).

merge_providers_in_parts(CurrentParts, [], _ProviderId) ->
	CurrentParts;
merge_providers_in_parts(CurrentParts, [StateDiffList_H | StateDiffList_T], ProviderId) ->
	log("merge providers ..."),
	{PartName, PartVersionFromStateDiff} = StateDiffList_H,
	PartInfo = dict:fetch(PartName, CurrentParts),
	ProvidersInPart = PartInfo#part_info.providers,
	
	CurrentPartVersion = PartInfo#part_info.current_version,
	
	if 
		PartVersionFromStateDiff == CurrentPartVersion ->
			merge_providers_in_parts(
				dict:store(
				  	PartName,
					PartInfo#part_info{
						providers = dict:store(
							ProviderId,
							undefined,
							ProvidersInPart
						)
					},			
					CurrentParts
				),
				StateDiffList_T,
				ProviderId
			);
		true ->
			merge_providers_in_parts(
				CurrentParts,
				StateDiffList_T,
				ProviderId
			)
	end.

remove_waiting_parts(CurrentWaitingParts, []) ->
	CurrentWaitingParts;
remove_waiting_parts(CurrentWaitingParts, [PartsList_H | PartsList_T]) ->
	log("remove waiting parts ..."),
	{PartName, PartInfo} = PartsList_H,
	Providers = PartInfo#part_info.providers,
	CopiesCount = dict:size(Providers),
	if
		CopiesCount >= ?PREFERRED_COPIES_COUNT ->
			FoundPart = dict:find(PartName, CurrentWaitingParts),
			case FoundPart of
				{ok, _} ->
					log(io_lib:format("removing waiting part ~p because of sufficient distribution", [PartName])),
					remove_waiting_parts(dict:erase(PartName, CurrentWaitingParts), PartsList_T);
				error ->
					remove_waiting_parts(CurrentWaitingParts, PartsList_T)
			end;
		true ->
			remove_waiting_parts(CurrentWaitingParts, PartsList_T)
	end.

build_update_list(CurrentUpdateList, [], _PartsInProvider, _WaitingParts, _ConnectedProviders) ->
	CurrentUpdateList;
build_update_list(CurrentUpdateList, [AllPartsList_H | AllPartsList_T], PartsInProvider, WaitingParts, ConnectedProviders) ->
	log("build update list ..."),
	{PartName, PartInfo} = AllPartsList_H,
	CurrentPartVersion = PartInfo#part_info.current_version,
	ProvidersInPart = PartInfo#part_info.providers,
	
	CopiesCount = dict:size(ProvidersInPart),
	PartNotInEnoughProviders = (CopiesCount < ?PREFERRED_COPIES_COUNT),

	FoundPartInProvider = dict:find(PartName, PartsInProvider),
	{LowerPartVersionInProvider, PartAlreadyInProvider} = 
	case FoundPartInProvider of
		{ok, PartVersion} ->
			{PartVersion < CurrentPartVersion, true};
		error ->
			{false, false}
	end,
	
	NewUpdateList = 
	if
		(PartNotInEnoughProviders and (not PartAlreadyInProvider)) or LowerPartVersionInProvider ->
			FoundWaitingPart = dict:find(PartName, WaitingParts),
			case FoundWaitingPart of
				{ok, WaitingPartInfo} ->
					[{as_data, PartName, CurrentPartVersion, WaitingPartInfo#waiting_part_info.part_data} | CurrentUpdateList];
				error ->
					[{
						from_provider,
						PartName,
						random_connected_provider_pid_for_part(dict:to_list(ProvidersInPart), ConnectedProviders)
					} | CurrentUpdateList]
			end;
		true ->
			CurrentUpdateList
	end,
	
	build_update_list(NewUpdateList, AllPartsList_T, PartsInProvider, WaitingParts, ConnectedProviders).

invalidate_providers_with_part(CurrentInvalidatedIds, [], CurrentConnectedProviders) ->
	{CurrentInvalidatedIds, CurrentConnectedProviders};
invalidate_providers_with_part(CurrentInvalidatedIds, [ProvidersInPartList_H | ProvidersInPartList_T], CurrentConnectedProviders) ->
	log("invalidate providers with part ..."),
	{ProviderId, _}  = ProvidersInPartList_H,
	FoundProviderPid = dict:find(ProviderId, CurrentConnectedProviders),
	case FoundProviderPid of
		{ok, Pid} ->
			log(io_lib:format("invalidating provider ~p because it has an old version of a part", [ProviderId])),
			spawn(search_provider, invalidate, [Pid]),
			invalidate_providers_with_part(sets:add_element(ProviderId, CurrentInvalidatedIds), ProvidersInPartList_T, dict:erase(ProviderId, CurrentConnectedProviders));
		error ->
			invalidate_providers_with_part(CurrentInvalidatedIds, ProvidersInPartList_T, CurrentConnectedProviders)
	end.

invalidate_random_providers(_NotThese, [], _Count, CurrentConnectedProviders) ->
	CurrentConnectedProviders;
invalidate_random_providers(_NotThese, _, Count, CurrentConnectedProviders) when not Count > 0 ->
	CurrentConnectedProviders;
invalidate_random_providers(NotThese, [ConnectedProviders_H | ConnectedProviders_T], Count, CurrentConnectedProviders) ->
	{ProviderId, ProviderPid} = ConnectedProviders_H,
	IsIgnored = sets:is_element(ProviderId, NotThese),
	case IsIgnored of
		true ->
			invalidate_random_providers(NotThese, ConnectedProviders_T, Count, CurrentConnectedProviders);
		false ->
			log(io_lib:format("invalidating provider ~p because we need more providers to get a part", [ProviderId])),
			spawn(search_provider, invalidate, [ProviderPid]),
			invalidate_random_providers(NotThese, ConnectedProviders_T, Count - 1, dict:erase(ProviderId, CurrentConnectedProviders))
	end.
	
random_connected_provider_pid_for_part([], _ConnectedProviders) ->
	not_found;
random_connected_provider_pid_for_part(ProvidersInPartList, ConnectedProviders) ->
	Pos = random:uniform(length(ProvidersInPartList)),
	Nth = lists:nth(Pos, ProvidersInPartList),
	{ProviderId, _} = Nth,
	FoundInConnected = dict:find(ProviderId, ConnectedProviders),
	case FoundInConnected of
		{ok, ProviderPid} ->
			ProviderPid;
		error ->
			random_connected_provider_pid_for_part(lists:delete(Nth, ProvidersInPartList), ConnectedProviders)
	end.

build_search_distribution(CurrentSearchDistribution, [], _ConnectedProviders) ->
	CurrentSearchDistribution;
build_search_distribution(CurrentSearchDistribution, [AllPartsList_H | AllPartsList_T], ConnectedProviders) ->
	log("build search distribution ..."),
	{PartName, PartInfo} = AllPartsList_H,
	ProviderPid = random_connected_provider_pid_for_part(dict:to_list(PartInfo#part_info.providers), ConnectedProviders),
	build_search_distribution(
		dict:append(ProviderPid, PartName, CurrentSearchDistribution),
		AllPartsList_T,
		ConnectedProviders
	).

distribute_search(CurrentSpawnIds, [], _What) ->
	CurrentSpawnIds;
distribute_search(CurrentSpawnIds, [SearchDistribution_H | SearchDistribution_T], What) ->
	log("distribute search ..."),
	{ProviderPid, SearchIn} = SearchDistribution_H,
	SpawnId = util:random_id(20),
	spawn_link(central_server, search_using_provider, [What, SearchIn, ProviderPid, self(), SpawnId]),
	distribute_search(sets:add_element(SpawnId, CurrentSpawnIds), SearchDistribution_T, What).

collect_results(CurrentResults, RemainingSpawnIds, CurrentParts, ConnectedProviders) ->
	log("collect results ..."),
	RemainingSpawnIdsCount = sets:size(RemainingSpawnIds),
	case RemainingSpawnIdsCount of
		0 ->
			{CurrentResults, CurrentParts};
		_ ->
		receive
				{SpawnId, SearchResults, SearchTimesFromProvider, ProviderPid} ->
					SpawnIdValid = sets:is_element(SpawnId, RemainingSpawnIds),
					case SpawnIdValid of
						true ->
							collect_results(
								CurrentResults ++ SearchResults,
								sets:del_element(SpawnId, RemainingSpawnIds),
								update_search_times(
									dict:to_list(SearchTimesFromProvider),
									get_provider_id_by_pid(dict:to_list(ConnectedProviders), ProviderPid),
									CurrentParts									
								),
								ConnectedProviders
							);
						false ->
							collect_results(CurrentResults, RemainingSpawnIds, CurrentParts, ConnectedProviders)
					end
		end
	end.

update_search_times([], _ProviderId, CurrentParts) ->
	CurrentParts;
update_search_times([NewSearchTimes_H | NewSearchTimes_T], ProviderId, CurrentParts) ->
	{PartName, SearchTime} = NewSearchTimes_H,
	PartInfo = dict:fetch(PartName, CurrentParts),
	Providers = PartInfo#part_info.providers,
	NewProviders = dict:store(ProviderId, SearchTime, Providers),
	NewPartInfo = PartInfo#part_info{providers = NewProviders},
	update_search_times(NewSearchTimes_T, ProviderId, dict:store(PartName, NewPartInfo, CurrentParts)).

get_provider_id_by_pid([], _ProviderPid) ->
	not_found;
get_provider_id_by_pid([ConnectedProviders_H | ConnectedProviders_T], ProviderPid) ->
	{Id, Pid} = ConnectedProviders_H,
	if
		Pid == ProviderPid ->
			Id;
		true ->
			get_provider_id_by_pid(ConnectedProviders_T, ProviderPid)
	end.

log(What) ->
	{ok, WriteDescr} = file:open("log.txt", [raw, append]),
	{Date={Year, Month, Day},Time={Hour, Minutes, Seconds}} = erlang:localtime(),
	DateTime = io_lib:format('~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b', [Year, Month, Day, Hour, Minutes, Seconds]),
	Module = "server",
	Message = "-> " ++ What ++ "\r\n",
	TextToLog = DateTime ++ " [" ++ Module ++ "]   " ++ Message,
	file:write(WriteDescr, TextToLog),
	file:close(WriteDescr).
