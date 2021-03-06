.. _getting started:

Getting started
===============

Marvin projects basically comprise of a collection of individual scripts and a main file which ties them all together.

To start a new project marvin provides an initializer, called ``marvin-init``.
The initializer will set up a new project for you, including a sample script, the main file and the Haskell project configuration to make compiling smooth and easy.

.. note:: Always run ``marvin-init`` in an empty directory, as it will place certain files there and overwrite existing files with the same name.

If you install marvin through `cabal <https://www.haskell.org/cabal/>`__ (``cabal install marvin``) or `stack <https://docs.haskellstack.org>`_ (``stack install marvin``) it will install the initializer as well and add it to your path.
To see the options of the initializer run ``marvin-init --help`` on your command line.
::

    marvin-init ~ make a new marvin project

    Usage: marvin-init BOTNAME [-a|--adapter ID]

    Available options:
    -h,--help                Show this help text
    -a,--adapter ID          id of the adapter to use

Information on Adapters and their id's can be found in the :ref:`adapters` section.

Installing marvin
-----------------

You can get a release version of marvin on `Hackage <https://hackage.haskell.org/package/marvin>`_

However the recommended way to install this package is via `stack`_.
The marvin package is part if the stack lts as of ``lts-8.5``.
You can let stack do the resolving for you if you've added marvin in your ``.cabal`` file you can simply run ``stack solver --update-config`` and it will choose the right versions for you.

After that ``stack build`` will pull and install marvin for you.

.. important:: 
    Marvin uses the ``text-icu`` library for regexes.
    It therefore requires the ``-dev`` version of the ``icu`` C library.

    Linux
        Simply install the ``-dev`` version of the icu library.

        For instance ``apt install libicu-dev`` on Ubuntu.
    
    OSX 
        You also need the icu library.
        If you are using `Homebrew <homebrew>`_ you are looking for the ``icu4c`` package (``brew install icu4c``).
        Because OSX also provides some headers you will also need to link the headers manually.
        If you are using stack to build your projects the easiest way is to add the following lines to ``$HOME/.stack/config.yaml``.

        .. code-block:: yaml

            extra-include-dirs:
                - /usr/local/opt/icu4c/include
            extra-lib-dirs:
                - /usr/local/opt/icu4c/lib
        
        alternatively you can pass the paths via ``--extra-include-dirs`` and ``extra-lib-dirs`` to the ``stack build`` and ``stack install`` command.



Scripts
-------

The functionality for your marvin installation is split into individual parts, called scripts.
Each script is some Haskell structure created with ``defineScript``.
Scripts can be user defined or be included externally.

External scripts
^^^^^^^^^^^^^^^^

You can include external scripts in the form of a library.
To do this you must add the library name to the ``.cabal`` and ``stack.yaml`` file of your project. 

You can find more information on external scripts and an example ``external-scripts.json`` file in the :ref:`external scripts section <external-scripts>`

User defined scripts
^^^^^^^^^^^^^^^^^^^^

You can also write some scripts yourself.
Typically scripts are a Haskell source file which defines a ``script`` value.

As an example, a "hello world" script.
::

    -- file named "HelloWorld.hs" (must be the same as module name + ".hs")
    module HelloWorld where

    import Marvin.Prelude

    -- This type signature is necessary to help the compiler
    script :: IsAdapter a => ScriptInit a
    script = defineScript 
                "hello-world" -- script name (for logging and config) 
                $ do  -- here follows the actual script definition
                    ...


You can find more information on the actual script content in the :ref:`scripting` section.

The main file
-------------

This file (ususally called ``Main.hs``) ties the scripts together and defines the :ref:`adapters` which your marvin project uses.

.. note:: If you use the initializer ``marvin-init`` the main file will already be defined for you and registered in the ``.cabal`` file.

The file must be a Haskell source file i.e. end with ``.hs`` and be mentioned in the ``main-is`` section of your ``.cabal`` file.
It will look someting like this:
::

    -- import marvin runner
    import Marvin.Run
    -- imports chosen adapter
    import Marvin.Adapter.Slack

    -- import all scripts
    import qualified HelloWorld
    import qualified MyScript

    -- list of all scripts to use
    scripts :: [ScriptInit SlackRTMAdapter]
    scripts = [ HelloWorld.script 
              , MyScript.script 
              ]

    main :: IO ()
    main = runMarvin scripts


You can write the main file yourself, but this can get tedious as you add more and more scripts.
To make this easier Marvin includes a utility which allows you to let the main file be generated automatically, called :ref:`marvin-pp`.
``marvin-pp`` creates the main file dynamically at compile time by scanning your project for scripts.
You can add external scripts by adding an :ref:`external-scripts.json <external-scripts>` file and ``marvin-pp`` will add those to your main file then.

To use ``marvin-pp`` simply add an empty main file, except for this line: ``{-# OPTIONS_GHC -F -pgmF marvin-pp -optF --adapter -optF slack-rtm #-}`` (this is what ``marvin-init`` does as well).

.. important:: 
    The ``marvin-pp`` generator is a compile time preprocessor and thus its output is often cached by your build system. As a result you have to run ``cabal clean`` or ``stack clean`` after you added or removed a script to force the build system to regenerate the main file.

