.. _configuration:

Runtime configuration
=====================

Configuration for marvin is written in the `configurator`_ syntax.

.. _configurator: https://hackage.haskell.com/package/configurator

The configuration is read-only, aka the program does not alter the config itself.
However the config is also auto-reload, meaning that the live system can adapt to changes in the config which are made while the system is running.

Therefore it is recommended that scripts using config values do not cache those values if possible, but reread them instead.

Please note that the config refresh interval means that it takes up to a minute until changes to the config are live. 

System config
-------------

Configuration pertaining to the system itself is stored under the "bot" key.
::

    bot {
        name = "my-bot"
        logging = "INFO"
        adapter = "slack-rtm"
    }

+-------------+---------------------------------------+------------------------------------------------------+
| Key         | default                               | Usage                                                |
+=============+=======================================+======================================================+
| ``name``    | ``"marvin"``                          | Bot name                                             |
|             |                                       | (for logging and which string `respond` triggers on) |
+-------------+---------------------------------------+------------------------------------------------------+
| ``logging`` | ``ERROR``                             | Verbosity of the logging                             |
+-------------+---------------------------------------+------------------------------------------------------+
| ``adapter`` | optional, defaults to ``"slack-rtm"`` | Adapter to use in the main file.                     |
|             |                                       | (Only used by preprocesor, see :ref:`marvin-pp`)     |
+-------------+---------------------------------------+------------------------------------------------------+

Script config
-------------

Configuration for scripts is automatically scoped.
Each script has access to a configuration stored under ``script.<script-id>`` with the functions ``getConfigVal`` and ``requireConfigVal``.
And of course these scripts can have nested config groups.
::

    bot {
        name = "my-bot"
    }

    script {
        script-1 {
            some-string = "foo"
            some-int = 1337
            bome-bool = true
        }
        script 2 {
            nested-group {
                val = false
            }
            name = "Trump"
            capable = false
        }
    }


Adapter config
--------------

Configuration pertaining to a particular adapter is stored under ``adapter.<adapter-name>``.
The exact nature of the adapter config depends on the adapter itself.
::

    bot {
        name = "my-bot"
        logging = "INFO"
    }
    adapter {
        slack-rtm {
            token = "eofk"
        }
    }
 