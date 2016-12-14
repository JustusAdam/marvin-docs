.. _marvin-pp:

The marvin preprocessor (marvin-pp)
===================================

The basic usage of the marvin preprocessor is to generate the main file.

To use ``marvin-pp`` add a line like ``{-# OPTIONS_GHC -F -pgmF marvin-pp -optF --adapter -optF slack-rtm #-}`` to the top of your main file.

.. note:: If you use ``marvin-pp`` it will generate the entre file, any previous content of the file is **ignored completely**. 

Explanation of the preprocessor invocation
------------------------------------------

``{-# OPTIONS_GHC -F -pgmF marvin-pp #-}`` tells the Haskell compiler to use ``marvin-pp`` as a preprocessor.

You can pass additional arguments to the preprocessor by prepending ``-optF ARGUMENT`` to the option line.

.. important:: Each argument has to be prefixed with ``-optF``, i.e. to pass ``--adapter slack-rtm`` to the preprocessor you have to add ``-optF --adapter -optF slack-rtm`` to the option line. 

Arguments to ``marvin-pp``
--------------------------

Output from ``marvin-pp --help``:

.. note:: The order of options is irrelevant.
::

    marvin-pp ~ the marvin preprocessor

    Usage: marvin-pp [-a|--adapter ID] NAME PATH PATH [-s|--external-scripts PATH] [-c|--config-location PATH]

    Available options:
    -h,--help                Show this help text
    -a,--adapter ID          adapter to use
    -s,--external-scripts PATH
                            config file of external scripts to
                            load (default: "external-scripts.json")
    -c,--config-location PATH
                            config to use (default: "config.cfg")

+---------------------------+---------------------------------------------------------------------------------------+
| Option                    | Usage                                                                                 |
+===========================+=======================================================================================+
| ``-h,--help``             | Only used for printing the help on the command line                                   |
+---------------------------+---------------------------------------------------------------------------------------+
| ``NAME PATH PATH``        | Arguments. Passed by GHC. (irrelevant for user)                                       |
+---------------------------+---------------------------------------------------------------------------------------+
| ``-a,--adapter``          | identifier for the adapter to use. If omitted will attempt to                         | 
|                           | read from the config (``--config``) at the ``bot.adapter`` key.                       |
+---------------------------+---------------------------------------------------------------------------------------+
| ``-s,--external-scripts`` | Point to an alternative file containting :ref`external scripts <external-scripts>`    |
+---------------------------+---------------------------------------------------------------------------------------+
| ``-c,--config-location``  | Point to an alternate config file. See the :ref:`configuration` section.              | 
|                           | (only used for looking up the adapter to use, see ``--adapter``)                      |
+---------------------------+---------------------------------------------------------------------------------------+