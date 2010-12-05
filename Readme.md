What
====

Processes which record all messages they receive

Currently available in two flavours:

  * mock process / test double: records any received messages but takes no action
  * passthrough spy: forms a wrapper around another process and upon receiving a message will record it then forward to the wrapped process.

For example usage see the inline EUnit tests.

Why
===

For asserting a process under test called another process in a particular way, currently only exact message matches supported.

How
===

    nspy:assert_message_received(Spy, Expected)

is a helper (function) which will handle messaging the Spy process and receiving the list of messages, then scan the list of messages for the one you want to find.

On failure eunit will print out your stdout, which contains the spy's expected message and actual received message list.

Born of necessity (since there is no RSpec for Erlang), contributions and criticisms welcome.
