-module(search_provider_end_notifier).

-export([start_link_notifier/1, notifier_impl/2]).

% to be called from the search_provider process
start_link_notifier(SearchProviderId) ->
	spawn_link(?MODULE, notifier_impl, [SearchProviderId, self()]),
	receive
		initialized ->
			ok
	end.

notifier_impl(SearchProviderId, ParentPid) ->
	process_flag(trap_exit, true),
	ParentPid ! initialized,
	receive
		{'EXIT', _, _} -> central_server:disconnect(SearchProviderId)
	end.
