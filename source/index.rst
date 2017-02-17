.. marvin documentation master file, created by
   sphinx-quickstart on Mon Dec 12 15:30:58 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

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
        
        respond "what is in file (\\w+)\\??" $ do
            match <- getMatch 
            let file = match !! 1

            contents <- liftIO $ readFile file

            send contents
        
        enterIn "#random" $ do
            user <- getUser
            username <- getUsername user

            send $(isL "Hello #{username} welcome to the random channel!")


Contents:
---------

.. toctree::
   :maxdepth: 2

   getting-started
   scripting
   marvin-pp
   adapters
   configuration
   external-scripts
   logging
   regex
   strings
   interpolation
   lenses
   faq



Indices and tables
==================

* :ref:`genindex`
* :ref:`search`

