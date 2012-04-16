-module(central_server).
-behaviour(gen_server).

-record(server_state, {providers, parts}).
-record(part_in_provider, {part_name, version, search_time}).

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

update(PartName, PartData, Version) ->
	not_implemented.

%%
%% for client
%%
search(What) ->
	not_implemented.

%%
%% for search provider
%%
connect(State) ->
	not_implemented.

ready() ->
	not_implemented.

%%
%% Callbacks
%%

init([]) ->
	Providers = dict:new(),
	Parts = dict:new(),
	{ok, #server_state{providers = Providers, parts = Parts}}.

terminate(normal, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_info(Msg, State) ->
	io:format("Unexpected message: ~p~n", [Msg]),
	{noreply, State}.

%%
%% Local Functions
%%

