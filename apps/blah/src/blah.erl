-module(blah).

-export([sort/1]).

-spec sort(list()) -> list().
sort(List) when is_list(List) ->
    gen_server:call(blaher, {sort, List}).
