-module(blah_sup).

-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
      { {one_for_one, 5, 10},         % strategy
        [ { blaher,                   % child spec
            {blaher, start_link, []},
            permanent,
            5000,
            worker,
            [blaher]
          }
        ]
      }
    }.
