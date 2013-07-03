%%
%% @file norm_types.erl
%%  Описание данных вылидатора. Тут расположены функции вида
%%  (fun(string|binary) -> any()).
%%

-module(norm_types).

-export([
    any/1,
    nullable/1,
    allable/1,
    boolean/1,
    atom/1,
    string/1,
    text/1,
    list/1,
    email/1,
    float/1,
    money/1,
    money/2,
    positive/1,
    pfloat/1,
    integer/1,
    unixtime/1,
    unixdatetime/1,
    test/0,
    test/1
]).




allable('*') ->
    all;
allable(all) ->
    all;
allable("*") ->
    all;
allable("all") ->
    all;
allable(<<"*">>) ->
    all;
allable(<<"all">>) ->
    all.

nullable("nil") ->
    null;
nullable(nil) ->
    null;
nullable("null") ->
    io:format(" ~n~n~n                      !Value = ~p ~n~n~n", ["null"]),
    null;
nullable(null) ->
    io:format(" ~n~n~n                      !Value = ~p ~n~n~n", [null]),
    null;
nullable(<<"">>) ->
    null;
nullable(<<"nil">>) ->
    null;
nullable(<<"null">>) ->
    io:format(" ~n~n~n                      !Value = ~p ~n~n~n", [<<"null">>]),
    null.

boolean(true) ->
    true;
boolean(false) ->
    false;
boolean(<<"true">>) ->
    true;
boolean(<<"false">>) ->
    false.

atom(Value) ->
    norm_convert:to_atom(Value).

integer(Value) ->
    norm_convert:to_integer(Value).

float(Value) ->
    norm_convert:to_float(Value).

positive(Value) when (Value >= 0) ->
    Value.

pfloat(Value) ->
    positive(norm_convert:to_float(Value)).

money(Value) ->
    money(Value, 100).

money(Value, Scale) ->
    trunc(norm_convert:to_float(Value) * Scale) / Scale.

unixtime(Value) ->
    {D, _} = norm_convert:to_universal_datetime(norm_convert:to_integer(Value)),
    D.

unixdatetime(Value) ->
    io:format(" ~n~n~n                      !Value = ~p ~n~n~n", [Value]),
    X = norm_convert:to_universal_datetime(norm_convert:to_integer(Value)),
    io:format(" ~n~n~n                      !ValueX = ~p ~n~n~n", [X]),
    X.

%% Email проверка
email(Value) ->
    case re:run(Value, expression_email()) of
        {match, _} ->
            Value;
        nomatch ->
            throw({
                type_error,
                {
                    email,
                    [
                        {value, Value}
                    ]
                }
            })
    end.

string(Value) ->
    Value.

text(Value) ->
    Value.

list(Value) ->
    norm_convert:to_list(Value).

any(Value) ->
    Value.


expression_email()->
    {ok, Re} =
        re:compile(
            "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]+"
        ),
    Re.


test() ->
    ok.

test(speed) ->
    ok.