.. _configuration:

Runtime configuration
=====================

Configuration for marvin is written in the `configurator <https://hackage.haskell.com/package/configurator>`_ syntax.

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
 

Example
-------

An example config with all currently available config options (excludes script config as those are user defined).
::

    bot {
        # String, one of WARNING, ERROR, INFO, DEBUG, optional, defaults to WARNING
        # Logging level for the bot
        logging = "WARNING" 

        # String, optional, default to "marvin", name for the bot
        # Also sometimes used to identify whether a given message should be interpreted as a command
        name = "marvin" 
        
        # String, one of the available adapter identifiers, optional, defaults to "slack-rtm"
        # Adapter to use in the main file.
        # Only used by the preprocessor.
        adapter = "slack-rtm"
    }

    adapter {
        shell {
            # String, filepath, optional. 
            # If present records the history in this file
            history-file = ""
        }
        slack-rtm {
            token = "" # String, required. Authentication token for slack api
        }
        slack-events {
            token = "" # String, required. This token is used to confirm recieved messages come from slack

            # boolean, defaults to true. Whether to use TLS for encryption.
            # Note that slack requires a webhook receiver to be tls protected. 
            # Therefore this must be activated unless the server is behind a proxy using tls.
            use-tls = true 
            certfile = "" # String (filepath), required if tls is used. As the server needs to use ssl, a certificate is required.
            keyfile = "" # String (filepath), required if tls is used. As the server needs to use ssl, a certificate is required.

            port = 7000 # Integer, optional. Defaults to 7000. Port on which the server listens for requests.
        }
        telegram-poll {
            token = "" # String, required. Authentication token for telegram api
            polling-timeout = 120 # positive integer, optional. Timeout for long polling requests
        }
        telegram-push {
            token = "" # String, required. Authentication token for telegram api

            url = "https://..." # String, required. Url of this server. (target for the webhook)

            use-tls = false # boolean, defaults to true. Whether to use TLS for encryption.
            certfile = "" # String (filepath), required if tls is used. As the server needs to use ssl, a certificate is required.
            keyfile = "" # String (filepath), required if tls is used. As the server needs to use ssl, a certificate is required.

            port = 7000 # Integer, optional. Defaults to 7000. Port on which the server listens for requests.
        }
    }
