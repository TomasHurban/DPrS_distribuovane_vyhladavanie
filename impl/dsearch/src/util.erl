-module(util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([random_id/1, get_time/0]).

random_id_inner(0) ->
	[];
random_id_inner(Length) ->
	[random:uniform(26) + 64 | random_id_inner(Length - 1)].
random_id(Length) ->
	random:seed(erlang:now()),
	random_id_inner(Length).

get_time() ->
	{MegaSecs, Secs, MicroSecs} = erlang:now(),
	MicroSecs + Secs * 1000000 + MegaSecs * 1000000 * 1000000.
