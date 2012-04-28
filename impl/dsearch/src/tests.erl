-module(tests).

-include_lib("eunit/include/eunit.hrl").

data_providers_search__test() ->
	central_server:start_link(),
	central_server:update("part1", "abc"),
	central_server:update("part2", "def"),
	central_server:update("part3", "ghi"),
	central_server:update("part33", "ihg"),
	central_server:update("part4", "jkl"),
	central_server:update("part5", "mno"),
	central_server:update("part6", "pqr"),
	central_server:update("part7", "stu"),
	central_server:update("part8", "vwx"),
	central_server:update("part9", "yz"),
	search_provider_supervisor:start_link(),
	search_provider_supervisor:start_link(),
	search_provider_supervisor:start_link(),
	search_provider_supervisor:start_link(),
	{ok, Results} = central_server:search("g"),
	ok.