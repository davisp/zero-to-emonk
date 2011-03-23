-module(modname).
-on_load(init/0).

init() ->
    erlang:load_nif("path/to/modname.so", any_term).

% When the NIF loads, it replaces this
% definition.
myfun() ->
    throw({error, not_loaded}).

