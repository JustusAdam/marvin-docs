.. _adapters:

Adapters
========

Adapters are the backend of marvin.
The exchangeable part that talks to the chat service itself.

Adapters are not yet exchangeable at runtime.
The bot application is compiled against one adapter.

Adapter polymorphism
^^^^^^^^^^^^^^^^^^^^

The capabilties required of an adapter is defined via the typeclass ``IsAdapter`` in ``Marvin.Adapter``.

Currently only one adapter is defined, which uses the the slack real time messaging api.
A planned next adapter will be using the push notifications of the slack api.

Users can define their own adapters of course but are strongly encouraged to release generic adapters publicly or `contribute them to marvin <https://github.com/JustusAdam/marvin/pulls>`_.
