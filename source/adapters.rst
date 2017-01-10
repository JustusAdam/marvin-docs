.. _adapters:

Adapters
========

Adapters are the backend of marvin.
The exchangeable part that talks to the chat service itself.

Adapters are not yet exchangeable at runtime.
The bot application is compiled against one adapter.

Adapter polymorphism
--------------------

The capabilties required of an adapter is defined via the typeclass ``IsAdapter`` in ``Marvin.Adapter``.

Users can define their own adapters of course but are strongly encouraged to release generic adapters publicly or `contribute them to marvin <https://github.com/JustusAdam/marvin/pulls>`_.


Currently defined Adapters
--------------------------

Shell
^^^^^

The shell adapter is used mostly for testing purposes.

The adapter id (for including it via the preprocessor) is "shell".
To wire manually import ``ShellAdapter`` from ``Marvin.Adapter.Shell``.

It is recommended to run a shell instance of marvin with stderr piped to a file so that it does not interfere with your interactions with marvin.

The shell adapter supports a persistent history by specifying ``adapters.shell.history-file`` in your config.

Slack Real Time Messaging API
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^




