-module(search_provider).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).

%%
%% for central_server
%%
search(What) ->
	not_implemented.

update(PartName, PartData, Version) ->
	not_implemented.

update_from(PartName, Provider) ->
	not_implemented.

%%
%% for another search provider
%%
get(PartName, Version) ->
	not_implemented.

%%
%% Local Functions
%%

