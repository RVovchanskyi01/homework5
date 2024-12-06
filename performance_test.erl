-module(performance_test).
-export([ets_test/0, mnesia_test/0, map_test/0]).

-record(test_table, {key, value}).

%% ETS
ets_test() ->
    Table = ets:new(test_table, [set, public]),
    {AddTime, _} = timer:tc(fun() -> ets:insert(Table, {key, value}) end),
    {UpdateTime, _} = timer:tc(fun() -> ets:insert(Table, {key, new_value}) end),
    {DeleteTime, _} = timer:tc(fun() -> ets:delete(Table, key) end),
    {ReadTime, _} = timer:tc(fun() -> ets:lookup(Table, key) end),
    io:format("ETS - Add: ~p ms, Update: ~p ms, Delete: ~p ms, Read: ~p ms~n",
              [AddTime, UpdateTime, DeleteTime, ReadTime]).

%% Mnesia
mnesia_test() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(test_table, [{attributes, record_info(fields, test_table)}]),
    {AddTime, _} = timer:tc(fun() -> mnesia:transaction(fun() -> mnesia:write(#test_table{key=key, value=value}) end) end),
    {UpdateTime, _} = timer:tc(fun() -> mnesia:transaction(fun() -> mnesia:write(#test_table{key=key, value=new_value}) end) end),
    {DeleteTime, _} = timer:tc(fun() -> mnesia:transaction(fun() -> mnesia:delete({test_table, key}) end) end),
    {ReadTime, _} = timer:tc(fun() -> mnesia:transaction(fun() -> mnesia:read({test_table, key}) end) end),
    io:format("Mnesia - Add: ~p ms, Update: ~p ms, Delete: ~p ms, Read: ~p ms~n",
              [AddTime, UpdateTime, DeleteTime, ReadTime]),
    mnesia:stop(),
    mnesia:delete_schema([node()]).

%% Map
map_test() ->
    Map = #{},
    {AddTime, UpdatedMap1} = timer:tc(fun() -> maps:put(key, value, Map) end),
    {UpdateTime, UpdatedMap2} = timer:tc(fun() -> maps:put(key, new_value, UpdatedMap1) end),
    {DeleteTime, UpdatedMap3} = timer:tc(fun() -> maps:remove(key, UpdatedMap2) end),
    {ReadTime, _} = timer:tc(fun() -> case maps:find(key, UpdatedMap3) of
                                          error -> undefined;
                                          {ok, Val} -> Val
                                      end end),
    io:format("Map - Add: ~p ms, Update: ~p ms, Delete: ~p ms, Read: ~p ms~n",
              [AddTime, UpdateTime, DeleteTime, ReadTime]).
