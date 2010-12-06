-module (nspy).
-export ([new/1, wrapper/1, mock/0, spy/2, assert_message_received/2]).
-define (NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

% a test double / mock process to use as a non-replying endpoint (null object) for messages
mock() -> new([]).

% a test spy to wrap another process, monitoring the messages sent to that process and relaying them to the intended receiver
% doesn't consider node failures or message synchrony, but you probably shouldn't be relying on that anyway.
wrapper(Target) -> new([Target]).

new(RelayMessagesTo) ->
  spawn(nspy, spy, [[], RelayMessagesTo]).

spy (Messages, RelayMessagesTo) ->
  receive
    {nspy_list_messages, ReplyTo}  -> 
      ReplyTo ! {nspy_messages, Messages},
      ?debugFmt("Node ~p requested message received list, sending: ~p~n", [ReplyTo, Messages]),
      spy(Messages, RelayMessagesTo);
    Message -> 
      ?debugFmt("Spy ~p received message: ~p~n", [self(), Message]),
      BroadcastMessage = fun(Receiver) -> Receiver ! Message end,
      lists:map(BroadcastMessage, RelayMessagesTo),
      spy([Message | Messages], RelayMessagesTo)
  end.

assert_message_received(Spy, Expected) ->
  Spy ! {nspy_list_messages, self()},
  receive
    {nspy_messages, Messages} -> 
      io:format("~n[SPY] expected ~p received ~p~n", [Expected, Messages]),
      MessageFound = lists:any(fun(Elem) -> Elem =:= Expected end, Messages),
      ?assert(MessageFound)
  end.

assert_message_received_success_test() ->
  Spy = nspy:mock(),
  Spy ! hi,
  Spy ! ho,
  timer:sleep(200),
  assert_message_received(Spy, hi),
  assert_message_received(Spy, ho).

assert_message_received_failure_test() ->
  Spy = nspy:mock(),
  Spy ! hi,
  timer:sleep(200),
  ?assertError({assertion_failed, _}, assert_message_received(Spy, ho)).

passthrough_spy_test() ->
  StuntDouble = nspy:mock(),
  PassthroughSpy = nspy:wrapper(StuntDouble),
  PassthroughSpy ! hi,
  assert_message_received(PassthroughSpy, hi),
  assert_message_received(StuntDouble, hi).

