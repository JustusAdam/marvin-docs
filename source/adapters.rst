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
Therefore you may define generic scripts which will work with any adapter using just the ``IsAdapter`` constraint in the script initializer type signature.

::

    import Marvin

    script :: IsAdapter a => ScriptInit a
    script = defineScript "name" $ do ...

Or if you need capabilities specific to some adapter you can reference the adapter type directly.

::

    import Marvin
    import Marvin.Adapter.Slack.RTM

    script :: ScriptInit (Slack RTM)
    script = defineScript "name" $ do ...

Users can define their own adapters of course but are strongly encouraged to release generic adapters publicly or `contribute them to marvin <https://github.com/JustusAdam/marvin/pulls>`_.


.. _issues: https://github.com/JustusAdam/marvin/issues


Currently defined Adapters
--------------------------

Shell
^^^^^

.. admonition:: Quick info

    +------------+--------------------------+
    | Adapter id | ``shell``                |
    +------------+--------------------------+
    | Module     | ``Marvin.Adapter.Shell`` |
    +------------+--------------------------+
    | Type       | ``ShellAdapter``         |
    +------------+--------------------------+


The simplest of all adapters, the shell adapter is used mostly for testing purposes.

The adapter id (for including it via the preprocessor) is "shell".
To wire manually import ``ShellAdapter`` from ``Marvin.Adapter.Shell``.

It is recommended to run a shell instance of marvin with stderr piped to a file so that it does not interfere with your interactions with marvin.

The shell adapter supports a persistent history by specifying ``adapters.shell.history-file`` in your config.

.. admonition:: Configuration keys

    +--------------+--------+-----------+---------------------------------------------------------------+
    | Name         | Type   | Necessity | Description                                                   |
    +--------------+--------+-----------+---------------------------------------------------------------+
    | history-file | String | optional  | If set the history of entered commands will be persisted here |
    +--------------+--------+-----------+---------------------------------------------------------------+

Slack 
^^^^^

For both of the following adapters you'll have to create a new `bot user <https://api.slack.com/bot-users>`__ for your slack team.

Real Time Messaging API
"""""""""""""""""""""""

.. admonition:: Quick info

    +------------+------------------------------+
    | Adapter id | ``slack-rtm``                |
    +------------+------------------------------+
    | Module     | ``Marvin.Adapter.Slack.RTM`` |
    +------------+------------------------------+
    | Type       | ``SlackAdapter RTM``         |
    +------------+------------------------------+

The adapter for the `slack real time messaging api <https://api.slack.com/rtm>`__ is currently the best supported adapter.

It works by opening a websocket to the slack servers from which it recieves events in real time.

The adapter id is "slack-rtm".
For manual wiring you'll need the ``(SlackAdapter RTM)`` data structure from ``Marvin.Adapter.Slack.RTM``.

.. admonition:: Configuration keys

    +--------------+--------+-----------+---------------------------------------------------------------+
    | Name         | Type   | Necessity | Description                                                   |
    +--------------+--------+-----------+---------------------------------------------------------------+
    | token        | String | required  | Authentication token for the slack API                        |
    +--------------+--------+-----------+---------------------------------------------------------------+

Events API
""""""""""

.. admonition:: Quick info

    +------------+------------------------------------+
    | Adapter id | ``slack-events``                   |
    +------------+------------------------------------+
    | Module     | ``Marvin.Adapter.Slack.EventsAPI`` |
    +------------+------------------------------------+
    | Type       | ``SlackAdapter EventsAPI``         |
    +------------+------------------------------------+

This adapter creates a server, which listens for events from the slack `Events API <https://api.slack.com/events>`__.

In addition to configuring marvin for this adapter you'll also have tell slack the url for this bots server when configuring the bot.

The adapter id is "slack-rtm".
For manual wiring you'll need the ``(SlackAdapter RTM)`` data structure from ``Marvin.Adapter.Slack.RTM``.

.. admonition:: Configuration keys

    +--------------+--------+---------------------------+---------------------------------------------------------------+
    | Name         | Type   | Necessity                 | Description                                                   |
    +--------------+--------+---------------------------+---------------------------------------------------------------+
    | token        | String | required                  | Authentication token for the slack API                        |
    +--------------+--------+---------------------------+---------------------------------------------------------------+
    | use-tls      | Bool   | optional                  | Whether to use TLS encryption, defaults to true               |
    +--------------+--------+---------------------------+---------------------------------------------------------------+
    | certfile     | String | required if TLS is used   | Path to the TLS certificate                                   |
    +--------------+--------+---------------------------+---------------------------------------------------------------+
    | keyfile      | String | required if TLS is used   | Path to the TLS key                                           |
    +--------------+--------+---------------------------+---------------------------------------------------------------+
    | port         | Int    | optional                  | Port on which to run the server                               |
    +--------------+--------+---------------------------+---------------------------------------------------------------+

.. important:: This adapter is not very well tested yet, please report any issues you encounter `here <issues>`_.


Telegram
^^^^^^^^

Both of the following adapters require you to create and register a `telegram bot <https://core.telegram.org/bots#6-botfather>`__

Poll
""""

.. admonition:: Quick info

    +------------+----------------------------------+
    | Adapter id | ``telegram-poll``                |
    +------------+----------------------------------+
    | Module     | ``Marvin.Adapter.Telegram.Poll`` |
    +------------+----------------------------------+
    | Type       | ``TelegramAdapter Poll``         |
    +------------+----------------------------------+

The telegram poll adapter sends long running http requests to the telegram servers to recieve events in near real time.

A unique trait of this adapter is the ``polling-timeout`` configuration key, which governs how long at maximum the polling requests may be kept open if no new event has arrived.

.. admonition:: Configuration keys

    +-----------------+--------+-----------+---------------------------------------------------------------+
    | Name            | Type   | Necessity | Description                                                   |
    +-----------------+--------+-----------+---------------------------------------------------------------+
    | token           | String | required  | Authentication token for the Telegram API                     |
    +-----------------+--------+-----------+---------------------------------------------------------------+
    | polling-timeout | Int    | optional  | Timeout for the polling requests (seconds) defaults to 120    |
    +-----------------+--------+-----------+---------------------------------------------------------------+

.. important:: This adapter is not very well tested yet, please report any issues you encounter `here <issues>`_.

Push
""""

.. admonition:: Quick info

    +------------+----------------------------------+
    | Adapter id | ``telegram-push``                |
    +------------+----------------------------------+
    | Module     | ``Marvin.Adapter.Telegram.Push`` |
    +------------+----------------------------------+
    | Type       | ``TelegramAdapter Push``         |
    +------------+----------------------------------+

The telegram push adapter creates a server and registers a webhook with telegram to receive event updates.

.. admonition:: Configuration keys

    +--------------+--------+---------------------------+---------------------------------------------------------------+
    | Name         | Type   | Necessity                 | Description                                                   |
    +--------------+--------+---------------------------+---------------------------------------------------------------+
    | token        | String | required                  | Authentication token for the slack API                        |
    +--------------+--------+---------------------------+---------------------------------------------------------------+
    | url          | String | required                  | URL on which this server runs                                 |
    +--------------+--------+---------------------------+---------------------------------------------------------------+
    | use-tls      | Bool   | optional                  | Whether to use TLS encryption, defaults to true               |
    +--------------+--------+---------------------------+---------------------------------------------------------------+
    | certfile     | String | required if TLS is used   | Path to the TLS certificate                                   |
    +--------------+--------+---------------------------+---------------------------------------------------------------+
    | keyfile      | String | required if TLS is used   | Path to the TLS key                                           |
    +--------------+--------+---------------------------+---------------------------------------------------------------+
    | port         | Int    | optional                  | Port on which to run the server                               |
    +--------------+--------+---------------------------+---------------------------------------------------------------+

.. important:: This adapter is not very well tested yet, please report any issues you encounter `here <issues>`_.


IRC
^^^

.. admonition:: Quick info

    +------------+------------------------+
    | Adapter id | ``irc`       `         |
    +------------+------------------------+
    | Module     | ``Marvin.Adapter.IRC`` |
    +------------+------------------------+
    | Type       | ``IRCAdapter``         |
    +------------+------------------------+


The irc adapter connects to your IRC server via the `irc-conduit library <https://hackage.haskell.org/package/irc-conduit>`__.

.. admonition:: Configuration keys

    +--------------+--------+-----------+---------------------------------------------------------------+
    | Name         | Type   | Necessity | Description                                                   |
    +--------------+--------+-----------+---------------------------------------------------------------+
    | host         | String | required  | Url for the IRC server                                        |
    +--------------+--------+-----------+---------------------------------------------------------------+
    | port         | Int    | reqired   | Port for the irc server                                       |
    +--------------+--------+-----------+---------------------------------------------------------------+

.. important:: This adapter is not very well tested yet, please report any issues you encounter `here <issues>`_.
