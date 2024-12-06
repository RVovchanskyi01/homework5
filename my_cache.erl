-module(my_cache).

-export([create/1, insert/3, insert/4, lookup/2, delete_obsolete/1]).

-define(UNDEFINED, undefined).

%% Створює ETS-таблицю з вказаною назвою
create(TableName) ->
    ets:new(TableName, [named_table, public, set]),
    ok.

%% Додає дані без обмеження часу зберігання
insert(TableName, Key, Value) ->
    insert(TableName, Key, Value, infinity).

%% Додає дані з обмеженням часу зберігання
insert(TableName, Key, Value, TTL) ->
    ExpiryTime = case TTL of
        infinity -> infinity;
        _ -> erlang:system_time(second) + TTL
    end,
    ets:insert(TableName, {Key, Value, ExpiryTime}),
    ok.

%% Повертає значення за ключем або undefined, якщо його немає або воно застаріло
lookup(TableName, Key) ->
    case ets:lookup(TableName, Key) of
        [] -> ?UNDEFINED;
        [{_Key, Value, ExpiryTime}] ->
            case is_expired(ExpiryTime) of
                true -> ?UNDEFINED;
                false -> Value
            end
    end.

%% Видаляє всі застарілі записи
delete_obsolete(TableName) ->
    CurrentTime = erlang:system_time(second),
    Fun = fun({_, _Value, ExpiryTime}) ->
        case ExpiryTime of
            infinity -> false;
            _ -> ExpiryTime =< CurrentTime
        end
    end,
    ets:select_delete(TableName, [{{'$1', '$2', '$3'}, [{Fun, [true]}], ['$1']}]),
    ok.


%% Допоміжна функція для перевірки, чи запис застарів
is_expired(infinity) ->
    false;
is_expired(ExpiryTime) ->
    CurrentTime = erlang:system_time(second),
    ExpiryTime =< CurrentTime.
