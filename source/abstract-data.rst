The abstract data model of marvin
=================================

The data model of marvin is that for many data types such as a ``User`` or a ``RemoteFile`` marvin leaves the concrete representation of the data structure to the used adapter.
This is the reason these structures always contain a type variable for the adapter, like ``User a``, ``Channel a`` or ``RemoteFile a``.
Adapters define these types as part of the implementation of the ``IsAdapter`` or ``HasFiles`` typeclasses.

The concrete representation of these types of course depends on the adapters and as such we do not know what the structure looks like.
However to ensure some basic interactions marvins ``isAdapter`` and ``HasFiles`` typeclass place constraints on the data types int he form of lens class superclasses.

Generally a ``Has<field> <stucture> <field-type>`` class means that *structure* has a reachable *field* of *field-type*.
For more thorough information on lenses see the :ref:`lenses` section, but for just some basics of lenses we can use the operators ``.^`` to access the field with the lens and ``.~`` to set the field at the lens.

::

    let user1 = ... :: User SomeAdapter
        username = ... :: Lens' (User SomeAdapter) Text

    x^.username -- returns the username

    let y = x & username .~ "new_name"

    y^.username - now returns "new_name"
