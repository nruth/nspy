-module (nspy).
-export ([new/0, spy/1, assert_message_received/2]).
-define (NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").


new() ->
  spawn(nspy, spy, [[]]).

spy (Messages) ->
  receive
    {nspy_list_messages, ReplyTo}  -> 
      ReplyTo ! {nspy_messages, Messages},
      ?debugFmt("Node ~p requested message received list, sending: ~p~n", [ReplyTo, Messages]),
      spy(Messages);
    Message -> 
      ?debugFmt("Spy ~p received message: ~p~n", [self(), Message]),
      spy([Message | Messages])
  end.

assert_message_received(Spy, Expected) ->
  Spy ! {nspy_list_messages, self()},
  receive
    {nspy_messages, Messages} -> 
      io:format("~n[SPY] expected ~p received ~p~n", [Expected, Messages]),
      MessageFound = lists:any(fun(Elem) -> Elem =:= Expected end, Messages),
      ?assert(MessageFound)
  end.

message_received_test() ->
  Spy = nspy:new(),
  Spy ! hi,
  Spy ! ho,
  timer:sleep(200),
  assert_message_received(Spy, hi),
  assert_message_received(Spy, ho).

message_not_received_test() ->
  Spy = nspy:new(),
  Spy ! hi,
  timer:sleep(200),
  ?assertError({assertion_failed, _}, assert_message_received(Spy, ho)).
