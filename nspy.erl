-module (nspy).
-export ([new/1, wrapper/1, mock/0, spy/2, assert_message_received/2]).
-define (NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

% API
% ===

% a test double / mock process to use as a non-replying endpoint (null object) for messages
mock() -> new([]).


% a test spy to wrap another process, monitoring the messages sent to that process and relaying them to the intended receiver
% doesn't consider node failures or message synchrony, but you probably shouldn't be relying on that anyway.
wrapper(Target) -> new([Target]).

  passthrough_wrapper_spy_test() ->
    StuntDouble = nspy:mock(),
    PassthroughSpy = nspy:wrapper(StuntDouble),
    PassthroughSpy ! hi,
    assert_message_received(PassthroughSpy, hi),
    assert_message_received(StuntDouble, hi).



%spawn a new spy process with no messages and a list of processes to forward messages to
new(RelayMessagesTo) ->
  spawn(nspy, spy, [[], RelayMessagesTo]).



% asks the spy to return its current messages received list
% n.b. assume asynchronous messaging and use appropriate sleep periods to allow message delivery
get_messages_from_spy(Spy) ->
  Spy ! {nspy_list_messages, self()},
  receive {nspy_messages, Messages} -> Messages end.

  get_messages_from_spy_test() ->
    Spy = spawn(nspy, spy, [[a_message], []]),
    ?assertEqual(get_messages_from_spy(Spy), [a_message]). %or timeout failure


% CORE
% ====

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





% ASSERTS
% =======

assert_message_received(Spy, Expected) ->
  Messages = get_messages_from_spy(Spy),
  io:format("~n[SPY] expected ~p received ~p~n", [Expected, Messages]),
  MessageFound = lists:any(fun(Elem) -> Elem =:= Expected end, Messages),
  ?assert(MessageFound).

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



