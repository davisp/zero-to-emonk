-module(basic_tests).
-export([same_term/2, cmp/2]).

-define(APPNAME, basic_tests).
-define(LIBNAME, basic_tests).

-on_load(init/0).


same_term(A, B) ->
    not_loaded(?LINE).

cmp(A, B) ->
    not_loaded(?LINE).


init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).


not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
