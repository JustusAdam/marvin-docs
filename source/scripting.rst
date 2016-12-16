.. _scripting:

Writing marvin scripts
======================

Each script in marvin is a Haskell module that defines a value ``script`` with the type ``ScriptInit``.
This value contains the code necessary to set up the script and will be run automatically by the marvin runner at startup.
It returns the script.

It does not matter where in the module you define this value, only that it sits at the top level so that the main file can import it.
You can define arbitrary other values in the top level of your script, such as mutable variables and you can import any Haskell library you like including other marvin scripts (for :ref:`data sharing`).

Script boilerplate
------------------

Since each script is a Haskell module the module name and the file name must match. 
I.e. a script module MyScript must be in a file called ``MyScript.hs``.
Furthermore the module and file name may only contain word characters and the underscore ``_`` and must begin with an upper case letter.

.. note:: 
    A file which starts with an underscore ``_`` or dot ``.`` is ignored by the automatic script discovery of the main file.
    This is a way to hide unfinished scripts from being included in the program.  

When you have created your source file you should first import marvins prelude ``Marvin.Prelude`` (something like marvins standard library).
It contains all the marvin related functions you will need.

.. hint:: 
    You dont *have* to use ``Marvin.Prelude``. 
    The prelude is just a convenient `collection of other modules <https://hackage.haskell.org/package/marvin-0.0.3/Marvin-Prelude.html>`_, you can also import just the ones you need directly, but this is only recommended for people experienced with Haskell.

::

    -- File: MyScript.hs
    module MyScript where -- Module definition (must match filename)

    -- import the prelude
    import Marvin.Prelude

    -- import other modules and libraries you need

    script :: IsAdapter a => ScriptInit a
    script = defineScript "my-script" $ do
        -- here follows the actual scripting part


Lastly we define a value called ``script`` with the type signature ``IsAdapter a => ScriptInit a``.
This complicated looking type signature ensures our script will work with any adapter that satisfies the adapter type class (adapter interface).
Here we call the function ``defineScript`` which takes an id string and an initializer block.

The id string is used fo two things

#. Scoping the config, i.e. the config for this script will be stored in the ``scripts.<id-string>`` key.
#. Logging. All logging messages from this script will be prefixed with ``scripts.<id-string>``.

Usually the id string is some variation on the name of the script file and module.

The initializer block is where the actual scripting starts.

The initializer block
---------------------

The initializer block is the code that is run when you start marvin.

First and foremost this block is used to add new :ref:`reactions <reacting>` to your marvin script, which is most likely the main part of your scripts functionality.

But you can do a variety of other things here such as :ref:`define periodic tasks <periodic tasks>`, :ref:`read data <persistence on disc>` and :ref:`define mutable variables <persistence in memory>` for state tracking or data sharing.

.. _reacting:

Reacting
--------

.. _reaction monad:

The reaction Monad
^^^^^^^^^^^^^^^^^^

Reaction functions
^^^^^^^^^^^^^^^^^^

There are several functions for reacting to some event happening in you chat application.
The type of reaction influences the kind of data available in the reaction handler.

The basic structure of a reaction is ``<reaction-type> <matcher> <handler>``.

``<reaction-type>``
    Is one of the reaction functions, like :ref:`hear <fn-hear>` or :ref:`respond <fn-respond>` (more are to follow).

    This also determines the type of data available in the handler.

``<matcher>``
    Is some selection criterium for which events you wish to handle, and also often influences the contents of the data available to the handler.

    For instance for :ref:`hear <fn-hear>` and :ref:`respond <fn-respond>` this is a regex.
    The message will only be handled if the regex matches, and the result of the match, as well as the original message is available to the handler later.

``<handler>``
    Arbitrary code which runs whenever a matched event occurs.

    Has access to message specific data (like a regex match of the message).
    Can communicate with the chat (send messages to people or channels).


There are currently three reaction functions available:

.. _fn-hear:

``hear``
""""""""

::

    hear :: Regex -> BotReacting a MessageReactionData () -> ScriptDefinition a ()
    hear regex handler = ...

``hear`` triggers on any message posted which matches the :ref:`regular expression <regex>`.
The type of Handler is ``BotReacting a MessageReactionData ()``, which means in addition to the :ref`normal reaction capabilities <reaction monad>` it has access to the full message with the :ref:`getMessage <fn-getMessage>` function and to the regex match with :ref:`getMatch <fn-getMatch>`.

Since this is a reaction to a message we additionally have can use the :ref:`send <fn-send>` function in this handler to post a message to the same channel the triggering message was posted to and also the :ref:`reply <fn-reply>` function to send a message to the sender of the original message (also posted to the same channel).

.. _fn-respond:

``respond``
"""""""""""

::

    respond :: Regex -> BotReacting a MessageReactionData () -> ScriptDefinition a ()
    respond regex handler = ...

.. todo:: At some point this needs to support derivations of the name. Maybe make that configurable?

``respond`` triggers only on messages which are directed at the bot itself, i.e. the message starts with the name of the bot.
The *rest* of the message is matched against the provided :ref:`regular expression <regex>` like in :ref:`hear <fn-hear>`.

As with :ref:`hear <fn-hear>` the match and message are available during handler execution via :ref:`getMatch <fn-getMatch>` and :ref:`getMessage <fn-getMessage>`.


Functions for Handlers
^^^^^^^^^^^^^^^^^^^^^^

.. _fn-getMatch:

The ``getMatch`` function
"""""""""""""""""""""""""

::

    getMatch :: HasMatch m => BotReacting a m Match

Retrieves the result of a regex match inside a handler monad whos state supports it.
Examples are the handlers for :ref:`hear <fn-hear>` and :ref:`respond <fn-respond>`.

:ref:`Regex matches <regex match>` are a list of strings. The 0'th index is the full match, the following indexes are matched groups.

.. _fn-getMessage:

The ``getMessage`` function
"""""""""""""""""""""""""""

::

    getMessage :: HasMessage m => BotReacting a m Message

Retrieves the :ref:`respond <fn-respond>` structure for the message this handler is reacting to inside a handler monad whos state supports it.
Examples are the handlers for :ref:`hear <fn-hear>` and :ref:`respond <fn-respond>`.


.. _fn-send:

The ``send`` function
"""""""""""""""""""""

::

    send :: (IsAdapter a, HasMessage m) => String -> BotReacting a m ()
    send msg = ...

The ``send``function is used to post messages to the same channel to which the message that triggered this handler was posted to.


Explanation of the type signature:

``IsAdapter a``
    We require the saved ``a`` in ``BotReacting`` to be an adapter. 
    This means this function actually interacts with the chat service (sends a message in this case).

``HasMessage m`` 
    The data in the monad must have a ``Message``in it somewhere, from which we can find the channel it was posted to.

.. _fn-reply:

The ``reply`` function 
""""""""""""""""""""""

::

    reply :: (IsAdapter a, HasMessage m) => String -> BotReacting a m ()
    reply msg = ...

Reply is similar to :ref:`send <fn-send>`. It posts back to the same channel the original message came from, but it also references the author of the original message.


Types for Handlers
^^^^^^^^^^^^^^^^^^

.. _type-Message:

The ``Message`` data type
"""""""""""""""""""""""""

:: 

    data Message = Message
        { sender    :: User
        , channel   :: Channel
        , content   :: String
        , timestamp :: TimeStamp
        }

Represents the information associated with a chat message that was posted.


Persistence
-----------

.. _persistence in memory:

In memory
^^^^^^^^^

.. _persistence on disc:

On disk
^^^^^^^

.. _periodic tasks:

Periodic tasks
--------------

.. _data sharing:

Data sharing
------------
