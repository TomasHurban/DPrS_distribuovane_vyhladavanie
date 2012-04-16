-module(tests).

-include_lib("eunit/include/eunit.hrl").

aaa_test() ->
	central_server:start_link().