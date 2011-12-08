-module (nspy).
-include_lib("eunit/include/eunit.hrl").

% API
-export ([
	  new/1, 
	  wrapper/1, 
	  mock/0, 
          add_handler/2,
	  assert_message_received/2, 
	  assert_message_not_received/2, 
	  assert_message_received_n_times/3
	 ]).

-export ([
	  init_spy/1
	 ]).

-define(NODEBUG, false).

%%%% API %%%%

% a test double / mock process to use as a non-replying endpoint 
% (null object) for messages
mock() -> new([]).

add_handler(Spy, {_Match, _Function}=Handler) ->
    Spy ! {nspy_add_handler, Handler}.

% a test spy to wrap another process, monitoring the messages sent to 
% that process and relaying them to the intended receiver
% doesn't consider node failures or message synchrony, but you 
% probably shouldn't be relying on that anyway.
wrapper(Target) -> new([Target]).

% spawn a new spy process with no messages and a list of processes
% to forward messages to
new(RelayMessagesTo) ->
    spawn(nspy, init_spy, [RelayMessagesTo]).

% asks the spy to return its current messages received list
% n.b. assume asynchronous messaging and use appropriate sleep periods
% to allow message delivery
get_messages_from_spy(Spy) ->
    Spy ! {nspy_list_messages, self()},
    receive {nspy_messages, Messages} -> 
	    Messages 
    end.

assert_message_not_received(Spy, Message) ->
    assert_message_received_n_times(Spy, Message, 0).

assert_message_received(Spy, Expected) ->
    Messages = get_messages_from_spy(Spy),
    %% io:format("~n[SPY] expected ~p received ~p~n", [Expected, Messages]),
    MessageFound = lists:any(fun(Elem) -> Elem =:= Expected end, Messages),
    ?assert(MessageFound).

assert_message_received_n_times(Spy, Expected, NTimes) ->
    Messages = get_messages_from_spy(Spy),
    FilteredMessages = [M || M <- Messages, M == Expected],
    ?assertEqual(NTimes, length(FilteredMessages)).


%%%% CORE %%%% 
init_spy(RelayMessagesTo) ->
    process_flag(trap_exit, true),
    [erlang:monitor(process, Receiver) || Receiver <- RelayMessagesTo],
    spy([], RelayMessagesTo, []).

spy(Messages, RelayMessagesTo, MessageHandlers) ->
    receive
	{nspy_add_handler, Handler} ->
	    spy(Messages, RelayMessagesTo, [Handler|MessageHandlers]);
	{nspy_list_messages, ReplyTo}  -> 
	    ReplyTo ! {nspy_messages, Messages},
	    spy(Messages, RelayMessagesTo, MessageHandlers);
	{'DOWN', _Ref, process, _Object, _Info} ->
	    spy(Messages, RelayMessagesTo, MessageHandlers);
	Message -> 
	    %% Check if match for handler
	    BroadcastMessage = fun(Receiver) -> Receiver ! Message end,
	    lists:map(BroadcastMessage, RelayMessagesTo),
	    spy([Message | Messages], RelayMessagesTo, MessageHandlers)
    end.


%%%% TESTS %%%%
passthrough_wrapper_spy_test() ->
    StuntDouble = nspy:mock(),
    PassthroughSpy = nspy:wrapper(StuntDouble),
    PassthroughSpy ! hi,
    assert_message_received(PassthroughSpy, hi),
    assert_message_received(StuntDouble, hi).

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

assert_message_not_received_success_test() -> 
    assert_message_not_received(nspy:mock(), hi).
assert_message_not_received_failure_test() ->
    Spy = nspy:mock(),
    Spy ! hi,
    ?assertError({assertEqual_failed, _}, 
		 assert_message_not_received(Spy, hi)).

assert_message_received_n_times_failure_test() ->
    Spy = nspy:mock(),
    ?assertError({assertEqual_failed, _}, 
		 assert_message_received_n_times(Spy, hi, 1)).

assert_message_received_n_times_success_test() ->
    Spy = nspy:mock(),
    Spy ! hi,
    assert_message_received_n_times(Spy, hi, 1),
    Spy ! hi,
    assert_message_received_n_times(Spy, hi, 2).

assert_message_received_n_times_ignores_other_messages_test() ->
    Spy = nspy:mock(),
    Spy ! hi,
    assert_message_received_n_times(Spy, hi, 1),
    assert_message_received_n_times(Spy, ho, 0),
    Spy ! ho,
    assert_message_received_n_times(Spy, ho, 1),
    assert_message_received_n_times(Spy, hi, 1).
