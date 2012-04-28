-module(util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([random_id/1]).

random_id_inner(0) ->
	[];
random_id_inner(Length) ->
	[random:uniform(26) + 64 | random_id_inner(Length - 1)].
random_id(Length) ->
	random:seed(erlang:now()),
	random_id_inner(Length).