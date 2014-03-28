-module(blaher).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0]).


start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> {ok, {}}.

handle_call({sort, List}, _From, S) ->
  ChunkLen = length(List) div 2,
  {L1, L2} = lists:split(ChunkLen, List),  % pattern matching example 1
 
  MainProcessPID = self(),
 
  PidA = spawn(fun() -> spawnedsort(L1, MainProcessPID, 100) end),
  PidB = spawn(fun() -> spawnedsort(L2, MainProcessPID, 1  ) end),
 
  receive {SortedA, PidA} ->            % pattern matching example 2. No matter
    receive {SortedB, PidB} ->          % if PidA/PidB responded faster, here
      Res = merge(SortedA, SortedB, []), % we can request for L1 result first
      {reply, Res, S}
    end
  end.

handle_info(_Msg, S) -> {noreply, S}.

handle_cast(_Msg, S) -> {noreply, S}.

terminate(_Reason, _S) -> ok.

code_change(_OldVsn, S, [from1to2]) -> {ok, S};
code_change(_OldVsn, S, [from2to1]) -> {ok, S}.
 
 
spawnedsort(List, RemotePID, SleepTime) ->
  timer:sleep(SleepTime),
  ThisProcessPID = self(),
  RemotePID ! { lists:sort(List), ThisProcessPID }.
 
 
% there is lists:merge/2 but for a sake of example let's implement this one too!
merge( [], [], Acc ) ->
  lists:reverse( Acc );                 % common idiom: temporary data
                                        % structure is reversed

merge(List1, [], Acc) ->
  lists:reverse(Acc) ++ List1;

merge([], List2, Acc) ->
  lists:reverse(Acc) ++ List2;

merge( [ Head1 | _ ] = List1,
       [ Head2 | Tail2 ],
       Acc
     ) when Head1 > Head2 ->            % guard with additional constrains
  merge( List1, Tail2, [Head2|Acc] );

                                        % functional idiom: don't loop, but
                                        % rather recurse.
 
merge( [ Head1 | Tail1 ],
       List2,
       Acc
     ) ->                               % no other option, yes?
  merge( Tail1, List2, [Head1|Acc] ).
