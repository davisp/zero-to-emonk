-module(resources).
-export([init/0, update/2, hex/1]).

-on_load(nif_init/0).

init() ->
    not_loaded(?LINE).

update(_Ctx, _Data) ->
    not_loaded(?LINE).

hex(_Ctx) ->
    not_loaded(?LINE).

nif_init() ->
    SoName = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?MODULE]);
                _ ->
                    filename:join([priv, ?MODULE])
            end;
        Dir ->
            filename:join(Dir, ?MODULE)
    end,
    erlang:load_nif(SoName, 0).


not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
