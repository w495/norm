-ifndef(__NORM_494696146__).
-define(__NORM_494696146__, true).

-define(NORM_CONVERTER, norm_types).
-define(UNIQ_UNDEFINED, 'norm:undefined').

-record(norm, {
    return=[] :: [{atom(), any()}],
    errors=[] :: [{atom(), any()}]
}).

-type norm_key()    :: atom() | binary() .
-type norm_type()   :: atom()|fun((any()) -> any()).

-record(norm_rule, {
        rules =        []               :: [record(norm_rule)],
    %% Один единственный ключ
        key =          ?UNIQ_UNDEFINED  ::  norm_key(),
    %% Множество (список) возможных ключей
        keys =         []               ::  [norm_key()],
    %% Новое имя ключа
        nkey =         ?UNIQ_UNDEFINED  ::  norm_key(),
    %% Множество (список) типов
        types =        []               ::  [norm_type()] ,
    %% Можно ли этот ключ пропустить
        required    = true,
    %% Значение по умолчанию
        default =      ?UNIQ_UNDEFINED  ::  any()
}).

-record(norm_at_least_one, {
        rules =        []               :: [record(norm_rule)],
    %% Новое имя ключа
        nkey =         ?UNIQ_UNDEFINED  ::  norm_key(),
    %% Значение по умолчанию
        default =      ?UNIQ_UNDEFINED  ::  any()
}).

-record(norm_one, {
        rules =        []               :: [record(norm_rule)],
    %% Новое имя ключа
        nkey =         ?UNIQ_UNDEFINED  ::  norm_key(),
    %% Значение по умолчанию
        default =      ?UNIQ_UNDEFINED  ::  any()
}).

-record(norm_error, {
    reason =        ?UNIQ_UNDEFINED :: param | types,
    value =         ?UNIQ_UNDEFINED :: any(),
    rule =          #norm_rule{}    :: record(norm_rule)
}).


-endif. %%% __NORM_494696146__


