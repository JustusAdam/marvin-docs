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
    The prelude is just a convenient `collection of other modules <http://hackage.haskell.org/package/marvin-0.2.0/docs/Marvin-Prelude.html>`_, you can also import just the ones you need directly, but this is only recommended for people experienced with Haskell.

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

.. _reaction monad:

The reaction Monad
------------------

::

    data BotReacting a d r = ... deriving (Monad, MonadIO, MonadReader (BotActionState a d)
                                          , MonadLogger, MonadLoggerIO)


The reaction monad offers basically four different capabilities.

#. ``MonadIO`` allows the user to execute arbitrary ``IO`` actions by lifting them with ``liftIO``.
    This can be things such as performing HTTP requests, reading files etc.

#. ``MonadReader (BotActionState a d)`` allows read access to the data carried by the monad.
    In general you dont need to use this directly as functions such as :ref:`getUser <fn-getUser>` are much more convenient to use.
    However the readable data you get by using ``ask`` contains not only the payload which is of type ``d`` and different depending on each handler function,
    but also access to the adapter, the config and script id. And is therefore capable of

#. Accessing the adapter.
    This enables the handler to communicate.
    Functions such as :ref:`send <fn-send>` and :ref:`messageChannel <fn-messageChannel>` can be used to send messages to the chat application.

#. ``MonadLogger(IO)`` Allows you to write log messages using functions from the `monad-logger`_ package by importing ``Control.Monad.Logging``.

.. _monad-logger: https://hackage.haskell.org/package/monad-logger


Reaction functions
------------------

There are several functions for reacting to some event happening in you chat application.
The type of reaction influences the kind of data available in the reaction handler.
The data available in the handler can be seen listed in a tuple in the ``BotReacting`` monad.
For instance ``BotReacting a (User' a, Channel' a, Message, Match, TimeStamp) ()`` will have access to a user, a channel, a message and so on.
Functions for getting access to this data are listed in :ref:`functions for handlers`

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


Reacting to messages
--------------------

There are two ways to react to a text message.
A reaction defined with ``hear`` will trigger on any incoming message which matches its defined pattern (a regular expresion).
By contrast reactions defined with ``respond`` will only trigger if the bot itself is being adressed.
How one adresses the bot depends on the concrete adapter.
However typically prefixing the message with the bots name or sending a direct message (if the adapter supports this) to the bot ususally trigger these reactions.

In the handler that is being attached you have access to the match groups of the regex with ``getMatch``, the user who sent the message (``getUser``), the full text of the message (``getMessage``), the channel to which the message was posted (``getChannel``) and a timestamp for when the message arrived (``getTimeStamp``).

The type signature for both is the same.

::

    hear, respond :: Regex -> BotReacting a (User' a, Channel' a, Match, Message, TimeStamp) () -> ScriptDefinition a ()
    hear regex handler = ...
    respond regex handler = ...

A working example could be something like this:

::

    defineScript "test" $ do

        hear "\\bmarvin\\b" $ do
            user <- getUser

            send $(isL "Yes #{user^.username}, that is my name")

        respond "^\\bsudo\\b(.+)" $ do
            match <- getMatch
            send #(isL "I will do #{match !! 1} immediately!"

        hear ".*" $ do
            channel <- getChannel
            unless (channel^.name == "#nsa") $ do
                message <- getMessage
                messageChannel "#nsa" $(isL "Psst, this message was just posted in #{channel^.name}: #{message}")


Reacting to the topic
---------------------

You can react to changes in the topic in two different ways.
Using ``topic`` the handler will trigger whenever the topic in any channel changes.
Using ``topicIn`` you can provide the name of a channel which you wish to watch for changes in the topic and the handler will only be run for changes to the topic in the specified channel.

In the handler you have access to the user which triggered the change (``getUser``), the channel in which the topic was changed (``getChannel``), the new topic (``getTopic``) and a timestamp for when this change occurred (``getTimeStamp``).

::

    topic :: BotReacting a (User' a, Channel' a, Topic, TimeStamp) () -> ScriptDefinition a ()
    topic handler = ...

    topicIn :: Text -> BotReacting a (User' a, Channel' a, Topic, TimeStamp) () -> ScriptDefinition a ()
    topicIn channelName handler = ...

.. note:: The ``Topic`` type is just for readability, it is just an alternate name for ``Text``.

Reacting to changes in channel participants
-------------------------------------------

Marvin can react both to people joining and leaving channels.
``enter`` triggers when a user enters **any** channel in which the bot is also participating.
``enterIn`` takes as an argument the name of a channel and ony triggers if a user joins **that** specific channel.
``exit`` triggers when a user leaves **any** channel in which the bot is also participating.
``exitFrom`` takes as an argument the name of a channel and ony triggers if a user leaves **that** specific channel.

All of these handlers have access to the channel which the user joined/left (``getChannel``), the user that joined/left (``getUser``) and a timestamp for when this occurred (``getTimeStamp``)

::

    enter :: BotReacting a (User' a, Channel' a, TimeStamp) () -> ScriptDefinition a ()
    enter handler = ...

    enterIn :: Text -> BotReacting a (User' a, Channel' a, TimeStamp) () -> ScriptDefinition a ()
    enterIn channelName handler = ...

    exit :: BotReacting a (User' a, Channel' a, TimeStamp) () -> ScriptDefinition a ()
    exit handler = ...

    exitFrom :: Text -> BotReacting a (User' a, Channel' a, TimeStamp) () -> ScriptDefinition a ()
    exitFrom channelName handler = ...

Reacting to files
-----------------

The ``fileShared`` handler is invoked any time a file is shared in **any** channel the bot is participating in.
By contrast the ``fileSharedIn`` handler takes as its first argument a channel name and only reacts to files being shared in that channel.

The handlers provide access to the user who shared the file (``getUser``), the channel in which the file was shared (``getChannel``), the ``RemoteFile`` object, containing information about the file being shared (``getRemoteFile``) and a timestamp for when the file was shared ``getTimeStamp``).

::

    fileShared :: BotReacting a (User' a, Channel' a, TimeStamp) () -> ScriptDefinition a ()
    fileShared handler = ...

    fileSharedFrom :: Text -> BotReacting a (User' a, Channel' a, TimeStamp) () -> ScriptDefinition a ()
    fileSharedFrom channelName handler = ...


Generic functions for handlers
------------------------------

.. _fn-send:

The ``send`` function
^^^^^^^^^^^^^^^^^^^^^

::

    send :: (IsAdapter a, Get m (Channel' a)) => Text -> BotReacting a m ()
    send msg = ...

The ``send`` function is used to post messages to the same channel from which the event that triggered the handler came.


Explanation of the type signature:

``IsAdapter a``
    We require the saved ``a`` in ``BotReacting`` to be an adapter.
    This means this function actually interacts with the chat service (sends a message in this case).

``Get m (Channel' a)``
    The data in the monad must have an originating ``Channel`` in it somewhere to which the message will be posted.
    This is true for most handler functions, for instance :ref:`hear <fn-hear>`, :ref:`respond <fn-respond>`, :ref:`enter <fn-enter>` all :ref:`enter <fn-enter>`, :ref:`exit <fn-exit>` and :ref:`topic <fn-topic>` handlers.


.. _fn-reply:

The ``reply`` function
^^^^^^^^^^^^^^^^^^^^^^

::

    reply :: (IsAdapter a, Get m (User' a), Get m (Channel' a)) => Text -> BotReacting a m ()
    reply msg = ...

Reply is similar to :ref:`send <fn-send>`. It posts back to the same channel the original message came from, but it also references the author of the original message.


.. _fn-messageChannel:

The ``messageChannel`` function
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    messageChannel :: (HasConfigAccess m, AccessAdapter m, IsAdapter (AdapterT m)) => L.Text -> L.Text -> m ()
    messageChannel channelName message = ...

Similar to :ref:`send <fn-send>` and :ref:`reply <fn-reply>` this functions sends a message to the channel with the (human readable) ``channelName``. If instead of a name you have a ``Channel a`` object, you can use :ref:`messageChannel' <fn-messageChannel'>`.

.. _fn-messageChannel':

The ``messageChannel'`` function
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    messageChannel' :: (HasConfigAccess m, AccessAdapter m, IsAdapter (AdapterT m), MonadIO m) => Channel (AdapterT m) -> L.Text -> m ()
    messageChannel' channel message = ...

Like :ref:`messgeChannel <fn-messageChannel>` but references the channel by channel object, rather than name.


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

    getMessage :: Get m (Message a) => BotReacting a m (Message a)

Retrieves the :ref:`respond <fn-respond>` structure for the message this handler is reacting to inside a handler monad whos state supports it.
Examples are the handlers for :ref:`hear <fn-hear>` and :ref:`respond <fn-respond>`.


.. _fn-getTopic:

The ``getTopic`` function
"""""""""""""""""""""""""

::

    getTopic :: HasTopic m => BotReacting a m Topic

This function is usable in handlers which react to changes of the topic of a channel.
It returns the *new* topic.

.. note:: The ``Topic`` type is just for readability, it is just an alternate name for ``Text``.


.. _fn-getChannel:

The ``getChannel`` function
"""""""""""""""""""""""""""

::

    getChannel :: Get m (Channel' a) => BotReacting a m (Channel a)

Usable in most handler functions, this function returns the channel in which some event occurred.


.. _fn-getUser:


The ``getUser`` function
"""""""""""""""""""""""""""

::

    getUser :: Get m (User' a) => BotReacting a m User

Usable in all handler functions which involve an acting user (most).
Returns the user who triggered an event.


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
