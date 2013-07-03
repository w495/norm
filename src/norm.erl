%%
%% @file norm.erl
%%
%%     Модуль преобразования строковых данных
%%         к определенным типам.
%%
%%     Сами типы описаны в отдельно модуле (см ?NORM_CONVERTER).
%%     Это сделано с целью разделить
%%         обязанности между модулями.
%%
%%

-module(norm).

-vsn(1.2).
-authors([cff, w495]).


-export([
    norm/2,
    norm/3,
    norm/4,
    to_rule_type/2,
    test/0,
    test/1
]).

-include("norm.hrl").




norm({Data}, Type_rules) ->
    norm(Data, Type_rules);

norm(Data, Type_rules)
        when erlang:is_list(Data)
            andalso erlang:is_list(Type_rules) ->
    norm(Data, Type_rules, #norm{}).

norm(_Data, [], Norm) ->
   Norm;

%%
%% Описание псевдонимов для различный
%% манипуляций с вложенными правилами
%%
norm(Data, [#norm_one{rules=Crules}|Restrules], Norm) ->
    case norm(Data, set_required(Crules, false),  #norm{}) of
        #norm{errors=[], return=[]} ->
            norm(Data, Restrules,
                Norm#norm{
                    errors=[
                        #norm_error{
                            reason  =   param,
                            rule    =   #norm_rule{rules=Crules}
                        } |Norm#norm.errors
                    ]
                }
            );
        #norm{return=[One|_]} = New_norm  ->
            norm(Data, Restrules,
                #norm{
                    errors=lists:append(Norm#norm.errors, New_norm#norm.errors),
                    return=[One|Norm#norm.return]
                }
            );
        New_norm ->
            norm(Data, Restrules,
                #norm{
                    errors=lists:append(Norm#norm.errors, New_norm#norm.errors),
                    return=lists:append(Norm#norm.return, New_norm#norm.return)
                }
            )
    end;

norm(Data, [#norm_at_least_one{rules=Crules, nkey=Nkey, default=Default}|Restrules], Norm) ->
    norm(Data,
        [
            #norm_rule{rules=set_required(Crules, false), required=true, nkey=Nkey, default=Default}
            |Restrules
        ],
        Norm
    );

%%
%% Для обработки вложенных правил
%%
norm(Data, [#norm_rule{rules=[], required=true, key=?UNIQ_UNDEFINED, keys=[]}=Rule|Restrules], Norm) ->
    Norm;

norm(Data, [#norm_rule{rules=[Crule|Crestrules]=Crules, required=true, key=?UNIQ_UNDEFINED, keys=[],  default=?UNIQ_UNDEFINED}=Rule|Restrules], Norm) ->
    case norm(Data, Crules, #norm{}) of
        #norm{errors=[], return=[]} ->
            norm(Data, Restrules,
                Norm#norm{
                    errors=[
                        #norm_error{
                            reason  =   param,
                            rule    =   Rule
                        } |Norm#norm.errors
                    ]
                }
            );
        New_norm ->
            norm(Data, Restrules,
                #norm{
                    errors=lists:append(Norm#norm.errors, New_norm#norm.errors),
                    return=lists:append(Norm#norm.return, New_norm#norm.return)
                }
            )
    end;


norm(Data, [#norm_rule{rules=[Crule|Crestrules]=Crules, required=true, key=?UNIQ_UNDEFINED, keys=[], nkey=Nkey, default=Default}=Rule|Restrules], Norm) ->
    case norm(Data, Crules, #norm{}) of
        #norm{errors=[], return=[]} ->
            norm(Data, Restrules,
                Norm#norm{
                    return=[
                        {norm_convert:to_atom(Nkey), Default}
                        | Norm#norm.return
                    ]
                }
            );
        New_norm ->
            norm(Data, Restrules,
                #norm{
                    errors=lists:append(Norm#norm.errors, New_norm#norm.errors),
                    return=lists:append(Norm#norm.return, New_norm#norm.return)
                }
            )
    end;

norm(Data, [#norm_rule{rules=[Crule|Crestrules]=Crules, key=?UNIQ_UNDEFINED, keys=[]}=Rule|Restrules], Norm) ->
    norm(Data, Restrules, norm(Data, Crules, Norm));
    
%%
%% Для обработки множественных ключей
%%
norm(Data, [#norm_rule{keys=[Rkey|Restrkeys]=Rkeys, key=?UNIQ_UNDEFINED}=Rule|Restrules], Norm) ->
    norm(Data, [Rule|Restrules], Norm, Rkeys);

% 
norm(Data, [#norm_rule{key=Key, nkey=?UNIQ_UNDEFINED}=Rule|Restrules], Norm) ->
    norm(Data, [Rule#norm_rule{nkey=Key}|Restrules], Norm);

norm(Data, [#norm_rule{key=Rkey, nkey=Nkey, types=Types, required=Required, default=Default}=Rule|Restrules], Norm) ->
    Key = case proplists:is_defined(Rkey, Data) of
        true -> Rkey;
        _ -> norm_convert:to_binary(Rkey)
    end,
    case proplists:get_value(Key, Data, Default) of
        ?UNIQ_UNDEFINED ->
            case Required of
                true ->
                    norm(Data, Restrules,
                        Norm#norm{
                            errors=[
                                #norm_error{
                                    reason  =   param,
                                    rule    =   Rule
                                } |Norm#norm.errors
                            ]
                        }
                    );
                _ ->
                    norm(Data, Restrules, Norm)
            end;
        Raw_value ->
            case to_rule_type(Raw_value, Types) of
                {ok, Value} ->
                    norm(Data,Restrules,
                        Norm#norm{
                            return=[
                                {norm_convert:to_atom(Nkey), Value}
                                | Norm#norm.return
                            ]
                        }
                    );
                {predefined, _} ->
                    norm(Data,Restrules,
                        Norm#norm{
                            return=[
                                {norm_convert:to_atom(Nkey), Default}
                                | Norm#norm.return
                            ]
                        }
                    );
                {error, Value} ->
                    norm(Data,Restrules,
                        Norm#norm{errors=[
                            #norm_error{
                                reason  =   types,
                                value   =   Value, 
                                rule    =   Rule
                            } |Norm#norm.errors
                        ]}
                    )
            end
    end.


norm(Data, [#norm_rule{keys=[], key=?UNIQ_UNDEFINED, required=Required}=Rule|Restrules], Norm, Errkeys) ->
    case Required of
        true ->
            norm(Data, Restrules,
                Norm#norm{
                    errors=[
                        #norm_error{
                            reason  =   param,
                            rule    =   Rule#norm_rule{keys=Errkeys}
                        } |Norm#norm.errors
                    ]
                }
            );
        _ ->
            norm(Data, Restrules, Norm)
    end;

norm(Data, [#norm_rule{keys=[Rkey|Restrkeys]=Rkeys, key=?UNIQ_UNDEFINED}=Rule|Restrules], Norm, Errkeys) ->
    case {
            proplists:is_defined(Rkey, Data),
            proplists:is_defined(Rkeyl = norm_convert:to_binary(Rkey), Data)
        } of
            {true, _} ->
                norm(Data, [Rule#norm_rule{key=Rkey}|Restrules], Norm);
            {false, true} ->
                norm(Data, [Rule#norm_rule{key=Rkeyl}|Restrules], Norm);
            {_, _} ->
                norm(Data, [Rule#norm_rule{keys=Restrkeys}|Restrules], Norm, Errkeys)
    end.


set_required(Rules, Required) ->
    lists:map(fun(R)->R#norm_rule{required=Required}end,Rules).

%%
%% @doc
%%
%%     Преобразует значение Value к указанным типам.
%%     Обертка функции to_rule_type/3
%%
to_rule_type(Value, Type_rules) ->
    to_rule_type(?NORM_CONVERTER, Value, Type_rules).

rule_type_done(Converter, Value, Type)
        when erlang:is_atom(Type)->
    io:format("1.1)  = ~p ~n", [{Converter , Type, Value}]),
    {ok, Converter:Type(Value)};

rule_type_done(_converter, _value, Type)
        when erlang:is_function(Type, 0) ->
    io:format("1.1)  = ~p ~n", [{_converter, Type, _value}]),
    {ok, Type()};
    
rule_type_done(_converter, Value, Type)
        when erlang:is_function(Type, 1) ->
    io:format("1.1)  = ~p ~n", [{_converter, Type, Value}]),
    {ok, Type(Value)}.

%%
%% @doc
%%
%%     Преобразует значение Value к указанным типам.
%%         Для работы нужно указать модуль Converter,
%%             где должны быть описаны правила преобразования.
%%
to_rule_type(_converter, Value, predefined) ->
    {predefined, Value};

to_rule_type(_converter, Value, any) ->
    {ok, Value};
    
to_rule_type(_converter, Value, []) ->
    {error, Value};

to_rule_type(_converter, Value, [[]|_rest]) ->
    {ok, Value};

to_rule_type(Converter, Value, [Type|Restrules]) ->
    io:format("1)  = ~p ~n", [{Type, Value, Restrules}]),
    try
        X = rule_type_done(Converter, Value, Type),
         io:format("1.5)  = ~p ~n", [{X}]),
        X
    catch
        throw : {type_error, Error} ->
            throw({type_error, Error});
        _t : _e ->
            io:format("2) _t : _e = ~p ~n~n", [{_t,  _e, Type, Value, Restrules}]),
            
            to_rule_type(Converter, Value, Restrules)
    end.

%%
%% @doc
%%
%%     Преобразует значение Value к указанным типам.
%%         Для работы нужно указать модуль Converter,
%%             где должны быть описаны правила преобразования.
%%
test() ->
    io:format("X = ~p~n", [
        norm:norm(
            [{<<"x">>, <<"12">>}],
            [#norm_rule{key=x, types=[integer]}]
        )
    ]),
    io:format("X = ~p~n", [
        norm:norm(
            [
                {<<"x">>, <<"12">>},
                {<<"y">>, <<"12.0">>}
            ],
            [
                #norm_rule{key=x, types=[integer]},
                #norm_rule{key=y, types=[float]}
            ]
        )
    ]),
    io:format("X = ~p~n", [
        norm:norm(
            [{<<"y">>, <<"12">>}],
            [#norm_rule{key=x, types=[integer]}]
        )
    ]),
    io:format("X = ~p~n", [
        norm:norm(
            [
                {<<"y">>, <<"12">>},
                {<<"z">>, <<"12">>}
            ],
            [#norm_rule{key=x, types=[integer]}]
        )
    ]),
    io:format("X = ~p~n", [
        norm:norm(
            [
             {<<"x">>, <<"12">>}
            ],
            [
                #norm_rule{key=a, types=[integer]},
                #norm_rule{key=b, types=[float]}
            ]
        )
    ]),
    io:format("X = ~p~n", [
        norm:norm(
            [{<<"x">>, <<"as">>}],
            [#norm_rule{key=x, types=[integer]}]
        )
    ]),
    io:format("X = ~p~n", [
        norm:norm(
            [
                {<<"x">>, <<"12">>},
                {<<"y">>, <<"sdsd">>}
            ],
            [
                #norm_rule{key=x, types=[integer]},
                #norm_rule{key=y, types=[float]}
            ]
        )
    ]),
    ok.

test(speed) ->
    ok.