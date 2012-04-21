-module(central_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([connect/2]).

-export([init/1, terminate/2, code_change/3, handle_info/2]).
-export([handle_call/3, handle_cast/2]).

-record(server_state, {providers, parts, connected_providers}).
-record(part_info, {current_version, providers}).
-record(part_copy_info, {part_version, search_time}).

-record(key_value_pair, {key, value}).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%%
%% for administration
%%

update(PartName, PartData, PartVersion) ->
	not_implemented.

%%
%% for client
%%
search(What) ->
	not_implemented.

%%
%% for search provider
%%
connect(ProviderId, StateDiff) ->
	gen_server:call({global, ?MODULE}, {connect, ProviderId, StateDiff}).

%%
%% Callbacks
%%

init([]) ->
	Providers = dict:new(),
	Parts = dict:new(),
	ConnectedProviders = dict:new(),
	{ok, #server_state{providers = Providers, parts = Parts, connected_providers = ConnectedProviders}}.

terminate(normal, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_info(Msg, State) ->
	io:format("Unexpected message: ~p~n", [Msg]),
	{noreply, State}.

handle_call({connect, ProviderId, StateDiff}, From, State) ->
	Providers = State#server_state.providers,
	FoundProvider = dict:find(ProviderId, Providers),
	Providers_new = dict:erase(ProviderId, Providers),
	PartsInProvider_new = 
	case FoundProvider of
		{ok, PartsInProvider} ->
			PartsInProvider;
		error ->
			dict:new()
	end,
	PartsInProvider_new2 = merge_parts_in_provider(PartsInProvider_new, dict:to_list(StateDiff)),
	Providers_new2 = dict:store(ProviderId, PartsInProvider_new2, Providers_new),
	
	Parts = State#server_state.parts,
	Parts_new = merge_providers_in_parts(Parts, dict:to_list(StateDiff), ProviderId),
	
	UpdateList = build_update_list([], dict:to_list(Parts_new), PartsInProvider_new2),
	
	ConnectedProviders_new = 
	case UpdateList of
		[] ->
			dict:store(ProviderId, From, State#server_state.connected_providers);
		_ ->
			dict:erase(ProviderId, State#server_state.connected_providers)
	end,

	State_new = State#server_state{
    	providers = Providers_new2,
		parts = Parts_new,
		connected_providers = ConnectedProviders_new
	},	
	
	case UpdateList of
		[] ->
			{
		        reply,
		        {ok},
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
	not_implemented.

%%
%% Local Functions
%%

merge_parts_in_provider(CurrentPartsInProvider, []) ->
	CurrentPartsInProvider;
merge_parts_in_provider(CurrentPartsInProvider, [StateDiffList_H | StateDiffList_T]) ->
	merge_parts_in_provider(
		dict:store(
		  	StateDiffList_H#key_value_pair.key,
			StateDiffList_H#key_value_pair.value,
			dict:erase(StateDiffList_H#key_value_pair.key, CurrentPartsInProvider)
		),
		StateDiffList_T
	).

merge_providers_in_parts(CurrentParts, [], _ProviderId) ->
	CurrentParts;
merge_providers_in_parts(CurrentParts, [StateDiffList_H | StateDiffList_T], ProviderId) ->
	{_CurrentVersion, ProvidersInPart} = dict:fetch(StateDiffList_H, CurrentParts),
	CurrentParts_new = dict:erase(StateDiffList_H, CurrentParts),
	
	ProvidersInPart_new = dict:erase(ProviderId, ProvidersInPart),
	merge_providers_in_parts(
		dict:store(
		  	StateDiffList_H#key_value_pair.key,
			dict:store(
				ProviderId,
				#part_copy_info{
					part_version = StateDiffList_H#key_value_pair.value,
					search_time = undefined
				},
				ProvidersInPart_new
			),
			CurrentParts_new
		),
		StateDiffList_T,
		ProviderId
	).

build_update_list(CurrentUpdateList, [], _PartsInProvider) ->
	CurrentUpdateList;
build_update_list(CurrentUpdateList, [AllPartsList_H | AllPartsList_T], PartsInProvider) ->
	PartName = AllPartsList_H#key_value_pair.key,
	{CurrentPartVersion, _Providers} =  AllPartsList_H#key_value_pair.value,
	build_update_list(CurrentUpdateList, AllPartsList_T, PartsInProvider).
