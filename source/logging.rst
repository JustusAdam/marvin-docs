Logging in marvin
=================

Marvin integrates with a logging library `monad-logger`_.

.. _monad-logger: https://hackage.haskell.org/package/monad-logger

This means all marvin monads are an instance of ``MonadLogger``, meaning they already know how to log messages.
You can use all the functions in ``Control.Monad.Logger`` to log messages in marvin and they will be automatically filtered and processed as the config specifies.

Basics of how to log messages
-----------------------------

The monad-logger library exposes some nice functions for logging messages.
For basic logging you should use functions such as ``logWarnN`` and ``logErrorN``.

Marvin will automatically prepend some location information for you i.e. if you log a message in the ``money`` script it will show up in the log with ``script.money``. 
This makes it easier to trace where a logging message came from.
::
    
    {-# LANGUAGE OverloadedStrings #-}

    script = defineScript "hello" $ do
        logDebugN "Starting definition of script"
        
        hear "hello .*" $ do
            logInfoN "Heard a hello"
            send "Hello to you too"

Since the logging functions all use strict ``Text`` as input it is recommended to use marvins strict text interpolator if your messages should contain external strings and data as the interpolator will take care of converting the data for you.
The interpolator for strict text is called ``isT`` and exposed by default if you import ``Marvin.Prelude``.
For more information on how interpolation is used in general see :ref:`interpolation <interpolation>`.
::

    {-# LANGUAGE TemplateHaskell #-}
    {-# LANGUAGE OverloadedStrings #-}
    import Marvin.Interpolate.Text

    script = 
        ...
        
        hear "sudo .*" $ do
            match <- getMatch

            logInfoN $(isT "I'm asked to do %{match !! 1}")

            send "okay"

For more advanced logging `monad-logger`_ offers some template Haskell functions which also record the place in the source code where the message came from.
The functions are called ``logWarning`` and ``logError``.
They require template Haskell to be enabled and must be invoked like so: ``$logWarning "my str"``.
::

    {-# LANGUAGE TemplateHaskell #-}
    {-# LANGUAGE OverloadedStrings #-}
    
    script = 
        ...
        $logDebug "my message"
    

And again it is recommende to use this in conjuction with the interpolator to easily include data in the message.
::

    {-# LANGUAGE TemplateHaskell #-}
    {-# LANGUAGE OverloadedStrings #-}
    
    script = 
        ...
        
        hear "sudo .*" $ do
            match <- getMatch

            $logInfo $(isT "I'm asked to do %{match !! 1}")

            send "okay"

Filtering log messages
----------------------

You can set a lower bound for the level of log messages which are reported.

The config key ``bot.logging`` controls which is the lowest level of log messages which are recorded. 
Available levels are (in ascending order, case insensitive) ``debug``, ``info``, ``warning``, ``error``.
Currently the choice of level is final, meaning changes in the config will not take effect until the program is restarted.
This is likely to change in the future.

Command line parameters can be used to overwrite the logging settings.
Passing ``-v`` to marvin during startup sets the logging level to ``info`` regardles of the config parameters.
Similarly passing ``--debug`` sets it to ``debug``.

Choosing a logging target
-------------------------

.. attention:: Not implemented yet. Currently log messages will always be printed to stderr.
