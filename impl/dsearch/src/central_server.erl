-module(central_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([update/2, search/1, connect/2]).

-export([init/1, terminate/2, code_change/3, handle_info/2]).
-export([handle_call/3, handle_cast/2]).

-export([search_using_provider/4]).

-record(server_state, {providers, parts, connected_providers, waiting_parts}).
-record(part_info, {current_version, providers}).
-record(part_copy_info, {part_version, search_time}).
-record(waiting_part_info, {part_data, copy_requests_to_providers}).


%%
%% for administration
%%

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []),
	log("central server started ...").

update(PartName, PartData) ->
	gen_server:call({global, ?MODULE}, {update, PartName, PartData}).

%%
%% for client
%%

search(What) ->
	gen_server:call({global, ?MODULE}, {search, What}, 200000).

%%
%% for search provider
%%

connect(ProviderId, StateDiff) ->
	gen_server:call({global, ?MODULE}, {connect, ProviderId, StateDiff}, 200000).

%%
%% Callbacks
%%

init([]) ->
	Providers = dict:new(),
	Parts = dict:new(),
	ConnectedProviders = dict:new(),
	WaitingParts = dict:new(),
	{ok, #server_state{providers = Providers, parts = Parts, connected_providers = ConnectedProviders, waiting_parts = WaitingParts}}.

terminate(normal, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_info(Msg, State) ->
	io:format("Unexpected message: ~p~n", [Msg]),
	{noreply, State}.

handle_call({update, PartName, PartData}, _From, State) ->
	FoundPart = dict:find(PartName, State#server_state.parts),
	NewVersion =
	case FoundPart of
		{ok, PartInfo} ->
			invalidate_providers_with_part(dict:to_list(PartInfo#part_info.providers), State#server_state.connected_providers),
			PartInfo#part_info.current_version + 1;
		error ->
			1
	end,
	
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
			)
		}
    };
handle_call({search, What}, _From, State) ->
	SearchDistribution = build_search_distribution(dict:new(), dict:to_list(State#server_state.parts), State#server_state.connected_providers),
	distribute_search(dict:to_list(SearchDistribution), What),
	{
        reply,
        {ok, collect_results([], dict:size(SearchDistribution))},
		State
    };
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
			{
		        reply,
		        ok,
		        State_new
		    };
		_ ->
			{
		        reply,
		        {update, UpdateList},
		        State_new
		    }
	end.

handle_cast(_, _State) ->
	nothing.

%%
%% Process begin functions
%%

search_using_provider(What, SearchIn, ProviderPid, ParentPid) ->
	ParentPid ! search_provider:search(What, SearchIn, ProviderPid).

%%
%% Local Functions
%%

merge_parts_in_provider(CurrentPartsInProvider, []) ->
	CurrentPartsInProvider;
merge_parts_in_provider(CurrentPartsInProvider, [StateDiffList_H | StateDiffList_T]) ->
	{PartName, PartVersion} = StateDiffList_H,
	
	merge_parts_in_provider(
		dict:store(
		  	PartName,
			#part_copy_info{
				part_version = PartVersion,
				search_time = undefined
			},
			CurrentPartsInProvider
		),
		StateDiffList_T
	).

merge_providers_in_parts(CurrentParts, [], _ProviderId) ->
	CurrentParts;
merge_providers_in_parts(CurrentParts, [StateDiffList_H | StateDiffList_T], ProviderId) ->
	{PartName, PartVersion} = StateDiffList_H,
	PartInfo = dict:fetch(PartName, CurrentParts),
	ProvidersInPart = PartInfo#part_info.providers,
	
	merge_providers_in_parts(
		dict:store(
		  	PartName,
			PartInfo#part_info{
				providers = dict:store(
					ProviderId,
					#part_copy_info{
						part_version = PartVersion,
						search_time = undefined
					},
					ProvidersInPart
				)
			},			
			CurrentParts
		),
		StateDiffList_T,
		ProviderId
	).

remove_waiting_parts(CurrentWaitingParts, []) ->
	CurrentWaitingParts;
remove_waiting_parts(CurrentWaitingParts, [PartsList_H | PartsList_T]) ->
	{PartName, PartInfo} = PartsList_H,
	Providers = PartInfo#part_info.providers,
	CopiesCount = dict:size(Providers),
	if
		CopiesCount >= 4 -> % TODO: not constant
			remove_waiting_parts(dict:erase(PartName, CurrentWaitingParts), PartsList_T);
		true ->
			remove_waiting_parts(CurrentWaitingParts, PartsList_T)
	end.

build_update_list(CurrentUpdateList, [], _PartsInProvider, _WaitingParts, _ConnectedProviders) ->
	CurrentUpdateList;
build_update_list(CurrentUpdateList, [AllPartsList_H | AllPartsList_T], PartsInProvider, WaitingParts, ConnectedProviders) ->
	{PartName, PartInfo} = AllPartsList_H,
	CurrentPartVersion = PartInfo#part_info.current_version,
	ProvidersInPart = PartInfo#part_info.providers,
	
	CopiesCount = dict:size(ProvidersInPart),
	PartNotInEnoughProviders = (CopiesCount < 1), % TODO: not constant

	FoundPartInProvider = dict:find(PartName, PartsInProvider),
	LowerPartVersionInProvider = 
	case FoundPartInProvider of
		{ok, PartCopyInfo} ->
			PartCopyInfo#part_copy_info.part_version < CurrentPartVersion;
		error ->
			false
	end,
	
	NewUpdateList = 
	if
		PartNotInEnoughProviders or LowerPartVersionInProvider ->
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

invalidate_providers_with_part([], _ConnectedProviders) ->
	ok;
invalidate_providers_with_part([ProvidersInPartList_H | ProvidersInPartList_T], ConnectedProviders) ->
	{ProviderId, _}  = ProvidersInPartList_H,
	FoundProviderPid = dict:find(ProviderId, ConnectedProviders),
	case FoundProviderPid of
		{ok, Pid} ->
			search_provider:invalidate(Pid);
		error ->
			do_nothing
	end,
	invalidate_providers_with_part(ProvidersInPartList_T, ConnectedProviders).

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
	{PartName, PartInfo} = AllPartsList_H,
	ProviderPid = random_connected_provider_pid_for_part(dict:to_list(PartInfo#part_info.providers), ConnectedProviders),
	build_search_distribution(
		dict:append(ProviderPid, PartName, CurrentSearchDistribution),
		AllPartsList_T,
		ConnectedProviders
	).

distribute_search([], _What) ->
	ok;
distribute_search([SearchDistribution_H | SearchDistribution_T], What) ->
	{ProviderPid, SearchIn} = SearchDistribution_H,
	spawn_link(central_server, search_using_provider, [What, SearchIn, ProviderPid, self()]),
	distribute_search(SearchDistribution_T, What).

collect_results(CurrentResults, RemainingCount) when RemainingCount == 0 ->
	CurrentResults;
collect_results(CurrentResults, RemainingCount) ->
	receive
			{ok, Result} -> collect_results(CurrentResults ++ Result, RemainingCount - 1)
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
