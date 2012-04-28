-module(search_provider_supervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link(search_provider_supervisor, []).

init(_Args) ->
	{
	ok,
		{
			{one_for_one, 10, 10},
			[
				{
					search_provider,
					{search_provider, create_and_activate, []},
					permanent,
					brutal_kill,
					worker,
					[ch3]
				}
			]
		}
	}.