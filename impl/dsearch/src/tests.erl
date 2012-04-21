-module(tests).

-include_lib("eunit/include/eunit.hrl").

aaa_test() ->
	central_server:start_link(),
	central_server:update("1", "abc"),
	search_provider:create_and_activate().