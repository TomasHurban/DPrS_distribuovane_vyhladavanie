-module(search_provider).

-export([create_and_activate/0, invalidate/1]).

-record(part, {part_name, part_data}).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).


create_and_activate() ->
	AA = central_server:connect(random_id(16), dict:new()),
	io:format(AA).

%%
%% for central_server
%%
search(What) ->
	not_implemented.

invalidate(Pid) ->
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
