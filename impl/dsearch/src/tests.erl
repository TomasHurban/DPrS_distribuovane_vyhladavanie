-module(tests).

-include_lib("eunit/include/eunit.hrl").

-export([data_providers_search/0, providers_data_search/0, update_existing_part/0, get_part_from_other_provider/0]).

data_providers_search() ->
	log("TEST: data_providers_search()"),
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
	log("test end \r\n"),
	ok.

providers_data_search() ->
	log("TEST: providers_data_search_test()"),
	central_server:start_link(),
	search_provider_supervisor:start_link(),
	search_provider_supervisor:start_link(),
	search_provider_supervisor:start_link(),
	search_provider_supervisor:start_link(),
	timer:sleep(1000),
	central_server:update("part1", "abc"),
	timer:sleep(1000),
	central_server:update("part2", "aabc"),
	timer:sleep(1000),
	A = central_server:search("a"),
	log("test end \r\n"),
	ok.

update_existing_part() ->
	log("TEST: update_existing_part()"),
	central_server:start_link(),
	central_server:update("part1", "abc"),
	search_provider_supervisor:start_link(),
	timer:sleep(2000),
	central_server:update("part1", "def"),
	timer:sleep(2000),
	{ok, Results} = central_server:search("def"),
	log("test end \r\n"),
	ok.

get_part_from_other_provider() ->
	central_server:start_link(),
	central_server:update("part1", "abc"),
	search_provider_supervisor:start_link(),
	timer:sleep(1000),
	central_server:test_remove_waiting_parts(),
	search_provider_supervisor:start_link(),
	timer:sleep(2000),
	{ok, Results} = central_server:search("abc"),
	ok.

log(What) ->
	{ok, WriteDescr} = file:open("log.txt", [raw, append]),
	{Date={Year, Month, Day},Time={Hour, Minutes, Seconds}} = erlang:localtime(),
	DateTime = io_lib:format('~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b', [Year, Month, Day, Hour, Minutes, Seconds]),
	Module = "tests",
	Message = "-> " ++ What ++ "\r\n",
	TextToLog = DateTime ++ " [" ++ Module ++ "] " ++ Message,
	file:write(WriteDescr, TextToLog),
	file:close(WriteDescr).
