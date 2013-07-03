# erlang-norm (Что это)

Ввлидатор входных данных. Привовит данные к нужному типу.
Порядок проверки типов имеет значение.

Описание данных можно менять. Оно вынесено в `norm_types.erl.`


# Requirements (Что нужно)

* erlang
* rebar

# Example (Пример)

    rr("include/norm.hrl").
    [norm,norm_error,norm_rule]

    norm:norm([{<<"name">>, <<"ivan">>},{<<"age">>, <<"40.1">>}],[
        #norm_rule{key=name, types=[string,nullable]},
        #norm_rule{key=age, types=[float, integer,nullable]}
    ]).

    %%
    %% #norm{return = [{age,40.1},{name,<<"ivan">>}],errors = []}
    %%

    norm:norm([{<<"name">>, <<"ivan">>},{<<"age">>, <<"40">>}],[
        #norm_rule{key=name, types=[string,nullable]},
        #norm_rule{key=age, types=[float, integer,nullable]}
    ]).

    %%
    %% #norm{return = [{age,40},{name,<<"ivan">>}],errors = []}
    %%

    norm:norm([{<<"name">>, <<"null">>},{<<"age">>, <<"null">>}],[
        #norm_rule{key=name, types=[string,nullable]},
        #norm_rule{key=age, types=[float, integer,nullable]}
    ]).

    %%
    %% #norm{return = [{age,null},{name,<<"null">>}],errors = []}
    %%

    norm:norm([{<<"name">>, <<"null">>},{<<"age">>, <<"null">>}],[
        #norm_rule{key=name, types=[nullable,string]},
        #norm_rule{key=age, types=[nullable, float,integer]}
    ]).

    %%
    %% #norm{return = [{age,null},{name,null}],errors = []}
    %%

    norm:norm([{<<"fname">>, <<"null">>},{<<"age">>, <<"null">>}],[
        #norm_rule{key=name, types=[nullable,string]},
        #norm_rule{key=age, types=[nullable, float,integer]}
    ]).

    %% 
    %% #norm{return = [{age,null}],errors = [
    %%     #norm_error{
    %%         reason = param,
    %%         value = 'norm:undefined',
    %%         rule = #norm_rule{
    %%             key = name,
    %%             nkey = name,
    %%             types = [nullable,string],
    %%             default = 'norm:undefined'
    %%         }
    %%     }
    %% ]}
    %% 

# Credis (Кто это натворил)

* Сергей Кожевников (Serge Kozhevnikov aka cff, 2011);
* Илья w-495 Никитин (w-495, 2012).


# TODO

    * Документировать код.
    * Оформить в виде отдельного OTP приложения.
    * Оформить в виде отдельной ноды.
