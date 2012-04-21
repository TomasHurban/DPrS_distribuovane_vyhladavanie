-module(search_provider).

-export([create_and_activate/0]).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).


create_and_activate() ->
	central_server:connect(random_id(16), dict:new()).

%%
%% for central_server
%%
search(What) ->
	not_implemented.

%%
%%update(PartName, PartData, Version) ->
%%	not_implemented.
%%
%%update_from(PartName, Provider) ->
%%	not_implemented.

%%
%% for another search provider
%%
get(PartName, PartVersion) ->
	not_implemented.

%%
%% Local Functions
%%

random_id(0) ->
	[];
random_id(Length) ->
	[random:uniform(26) + 64 | random_id(Length - 1)].
