.. _lenses:

Lens quickstart
===============

The ``Lens'`` type can be used to manipulate a field in a data structure.
For example a lens ``foo :: Lens' Bar Int`` pertains to a field of type ``Int`` in a data structure called ``Bar``.

Getting
-------

The operator ``^.`` is used to access the contents of a field.
``x ^. foo`` accesses the ``foo`` field in the ``x`` value.

These lenses are composable.
If we have a lens ``bar :: Lens' Baz Bar`` and a value ``y :: Baz`` we can access the nested ``foo`` value with ``y ^. bar . foo``.

Setting
-------

The same lenses can also be used to modify the contents of the referenced field.
``foo .~ value`` creates a function which sets the ``foo`` field to ``value``.
Often this is combined with the revers application operator ``&`` to write code such as ``x & foo .~ value`` which sets ``foo`` in ``x`` to ``value``.
Using ``&`` we can also chain modifications like so ``x & foo .~ value & anotherField .~ anotherValue``.
This does not modify the original ``x`` but instead returns a new value of type ``Bar`` which is identical to ``x`` except for the contents of the ``foo`` field.

Another operator for modification is ``%~`` where ``foo %~ f`` modifies the content of the ``foo`` field with the function ``f``.

Lenses in modification operations are also composable.
For instance to set the nested ``foo`` field in ``y`` we can say ``y & baz . foo .~ value``.
