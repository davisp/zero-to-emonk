-module(termsend).
-export([send/1, send/2]).

-define(APPNAME, termsend).
-define(LIBNAME, termsend).

-on_load(init/0).

send(_Term) ->
    % self() ! Term.
    not_loaded(?LINE).

send(_Pid, _Term) ->
    % Pid ! Term.
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
