Welcome to marvin's documentation!
==================================

Marvin is a Haskell framework for creating chat bots, inspired by `Hubot <https://hubot.github.com>`_.
Marvin aims to recreate the ease of use and straightforward API of Hubot, and combine it with the safety guarantees and purity of Haskell and the higher efficiency.

If you are new to marvin you may want to begin with the :ref:`getting started` section or the :ref:`how to script with marvin <scripting>` section to get a sense of how scripting works with marvin.

Links
-----

* `Hackage <https://hackage.haskell.org/package/marvin>`__
* `GitHub Repository <https://github.com/JustusAdam/marvin>`__
* `Bugtracker <https://github.com/JustusAdam/marvin/issues>`__
* `Documentation repository <https://github.com/JustusAdam/marvin-docs>`__ and `bugtracker <https://github.com/JustusAdam/marvin-docs/issues>`__
* `Slack channel <slack-channel>`_ (:ref:`signup instructions <testing-and-talking>`)

.. _slack-channel: https://marvin-bot.slack.com

A quick snippet of code
-----------------------

::

    module MyScript where

    import Marvin.Prelude

    script :: IsAdapter a => ScriptInit a
    script = defineScript "my-script" $ do
        hear "sudo (.+)" $ do
            match <- getMatch

            reply $(isL "All right, i'll do #{match !! 1}")

        respond "repeat" $ do
            message <- getMessage

            send $(isL "You wrote #{message}")

        respond "what is in file ([\\w\\._/-]+)\\??" $ do
            match <- getMatch
            let file = match !! 1

            contents <- liftIO $ readFile file

            send contents

        respond "upload file ([\\w\\._/-]+)" $ do
            [_, filepath] <- getMatch
            chan <- getChannel
            f <- sendFile filepath [chan]
            case res of
                Left err -> send $(isL "Failed to share file: #{err}")
                Right _  -> send "File successfully uploaded"

        enterIn "random" $ do
            user <- getUser
            send $(isL "Hello #{user^.username} welcome to the random channel!")

        fileSharedIn "announcements" $ do
            file <- getFile
            safeFileToDir file "shared-files"


.. _testing-and-talking:

Testing and Talking
-------------------

There's a `slack channel <slack-channel>`_ where you can ask questions or play around with a `test instance of marvin <https://github.com/JustusAdam/marvin/blob/master/test/integration/slack/Script1.hs>`__.

It's currently invite-only, so `send me an email <mailto:dev@justus.science>`_ if you would like to join.


Contents:
---------

.. toctree::
   :maxdepth: 2

   getting-started
   scripting
   abstract-data
   configuration
   files
   external-scripts
   logging
   marvin-pp
   adapters
   strings
   lenses
   interpolation
   breaking-changes
   faq



Indices and tables
==================

* :ref:`genindex`
* :ref:`search`

