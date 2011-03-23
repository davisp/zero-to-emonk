-module(nifstate).
-export([curr/0, incr/0, decr/0, swap/1]).
-export([reload/1]).


-define(APPNAME, nifstate).
-define(LIBNAME, nifstate).

-on_load(init/0).

curr() ->
    not_loaded(?LINE).

incr() ->
    not_loaded(?LINE).

decr() ->
    not_loaded(?LINE).

swap(_Val) ->
    not_loaded(?LINE).

reload(Val) ->
    init(Val).

init() ->
    init(0).

init(Init) ->
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
    erlang:load_nif(SoName, Init).


not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
