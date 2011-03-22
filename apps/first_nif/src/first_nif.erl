-module(first_nif).
-export([say_hi/0]).

-on_load(init/0).

say_hi() ->
    not_loaded(?LINE).

init() ->
    PrivDir = code:priv_dir(?MODULE),
    SoName = filename:join(PrivDir, ?MODULE),
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
