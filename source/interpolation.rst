.. _interpolation:

marvin-interpolate, A simple string interpolation library
=========================================================

.. note:: The marvin interpolation library, with no dependencies on marvin itself, is separately available on `hackage <https://hackage.haskell.org/package/marvin-interpolate>`_.

.. caution:: There is currently a bug in the Haskell parser for interpolated
             expressions. For versions of this library compiled against
             ``haskell-src-meta >= 0.8,2`` operator precedence is no longer
             respected. This prevents mixing of non-parenthesized infix
             operators. For instance ``$(isT "My Text
             #{object^.field.otherField}")`` won't compile properly. This can be
             fixed with explicit parentheses, i.e. ``$(isT "My Text
             #{object^.(field.otherField)}")``

             The tracking issue is `#4 https://github.com/JustusAdam/marvin-interpolate/issues/4`_.

The marvin string interpolation library is an attempt to make it easy for the user to write text with some generated data in it.
The design is very similar to the string interpolation in Scala and CoffeeScript, in that the hard work happens at compile time (no parsing overhead at runtime) and any valid Haskell expression can be interpolated.

TLDR and ``Marvin.Prelude`` specifics
-------------------------------------

By default ``Marvin.Prelude`` exposes two interpolators ``isL`` for composing messages which can be sent to the chat (produces lazy ``Text``) and ``isT`` for composing log messages (produces **strict** ``Text``).

Both require `Template Haskell`_ and `Overloaded Strings`_ which is enabled by adding the lines ``{-# LANGUAGE TemplateHaskell #-}`` and ``{-# LANGUAGE OverloadedStrings #-}`` at the beginning of your script file.

.. _Template Haskell: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell

.. _Overloaded Strings: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#overloaded-string-literals

Example:
::

    {-# LANGUAGE TemplateHaskell #-}
    {-# LANGUAGE OverloadedStrings #-}

    myStr = let x = "data" in $(isL "some string with #{x}: #{ 1 + 1 }")
    -- "some string with data: 2"

The syntax is ``$(interpolator "interpolated string")`` where interpolator is either ``isL`` or ``isT``.
As in CoffeeScript you can use ``#{}`` to interpolate an expression.
Any valid Haskell expression can be interpolated, it can reference both local and global bindings.

The result of the expression must either be a type of string or be convertible to one via ``Show`` or ``ShowL`` or ``ShowT`` respectivley which is true for most basic data types.
More information on conversion can be found :ref:`here <interpolation and conversion>`

How to interpolate
------------------

The library uses the builtin Haskell compiler extension in the form of *QuasiQuoters* (`QuasiQuotes <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell-quasi-quotation>`_ language extension) and /or *splices* (`Template Haskell`_ language extension)

Some examples to start with:

::

    {-# LANGUAGE QuasiQuotes #-}

    import Marvin.Interpolate

    str1 = [iq|some string #{show $ map succ [1,2,3]} and data|]
    -- "some string [2,3,4] and data"

    str2 =
        let
            x = "multiple"
            y = "can"
            z = "local scope"
        in [iq|We #{y} interpolate #{x} bindings from #{z}|]
    -- "We can interpolate multiple bindings from local scope"

    str2 =
        let
            x = ["haskell", "expression"]
            y = " can be"
        in [iq|Any #{intercalate ' ' x ++ y} interpolated|]
    -- "Any haskell expression can be interpolated"


Alternatively the interpolators are available as splices
::

    {-# LANGUAGE TemplateHaskell #-}

    import Marvin.Interpolate

    str1 = $(is "some string #{show $ map succ [1,2,3]} and data")
    -- "some string [2,3,4] and data"


It basically transforms the interpolated string, which is ``[iq|interpolated string|]`` or in splices ``$(is "interpolated string")`` into a concatenation of all string bits and the expressions in ``#{}``.
Therefore it is not limited to ``String`` alone, rather it produces a literal at compile time, which can either be interpreted as ``String`` or, using the `Overloaded Strings`_ extension, as ``Text`` or ``ByteString`` or any other string type.

.. _interpolation and conversion:

Interpolators and conversion
----------------------------

``iq`` (for *interpolate quoter*) and ``is`` (for *interpolate splice*) is the basic interpolator, which inserts the expressions verbatim. Hence when using ``iq`` or ``is`` all expressions must return the desired string type, otherwise the compiler will raise a type error.

There are specialized interpolators, which also perform automatic conversion of non-string types into the desired string type.
As an example, from earlier, if we use a specialized interpolator we dont need the call to ``show``.
::

    str1 = [iq|some string #{show $ map succ [1,2,3]} and data|]
    -- "some string [2,3,4] and data"

    -- is the same as
    str2 = [iqS|some string #{map succ [1,2,3]} and data|]

    -- ('iqS' is the specialized interpolator for 'String')

These specialized interpolators each have an associated typeclass, which converts string types (``String``, ``Text`` and lazy ``Text``) to the target type, but leaves the contents unchanged and calls `show` on all other types before converting.
This last instance, which is based on the ``Show`` typeclass, can be overlapped by specifying a custom instance for your type, allowing the user to define the conversion.

The naming scheme of the interpolators in general is ``i<splice|quoter><pecialization?>``.
I. e. ``isS`` expands to *interpolate splice to String* and ``iqL`` to *interpolate quoter to Lazy Text*.

- ``iqS`` and ``isS`` in ``Marvin.Interpolate.String`` converts to ``String`` via the ``ShowStr`` typeclass
- ``iqT`` and ``isT`` in ``Marvin.Interpolate.Text`` converts to ``Text`` via the ``ShowT`` typeclass
- ``iqL`` and ``isL`` in ``Marvin.Interpolate.Text.Lazy`` converts to lazy ``Text`` via the ``ShowLT`` typeclass

To import all interpolators, import ``Marvin.Interpolate.All``.


Syntax for the interpolated String
----------------------------------

Interpolation uses the `quasi quoter sytax <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell-quasi-quotation>`_, which starts with ``[interpolator_name|`` and ends with ``|]``.
Anything in between is interpreted by the library.

The format string in between uses the syntax ``#{expression}``.
Any valid Haskell expression can be used inside the braces.
Anything outside the braces is interpreted as literal string.
And all names which are in scope can be used, like so.
::

    let x = 5 in [iqS|x equals #{x}|] -- > "x equals 5"

.. _escape sequences:

Escape sequences
^^^^^^^^^^^^^^^^

::

    str3 = [iq|Two escape sequences allow us to write literal ##{ and |#] inside expressions"}|]
    -- "Two escape sequence allow us to write literal #{, |] and } inside expressions"

There are two escape sequences to allow literal ``#{`` and ``|]``

+--------+--------+
| Input  | Output |
+--------+--------+
| ``#]`` | ``]``  |
+--------+--------+
| ``##`` | ``#``  |
+--------+--------+

As a result the sequence ``##{`` will show up as a literal ``#{`` in the output and ``|#]`` results in a literal ``|]``.


Differences between QuasiQuotes and splices
"""""""""""""""""""""""""""""""""""""""""""

When using QuasiQuotes (``[i|interpolated string|]``) any character between is interpreted as literal, including this such as tabs and newlines.
No escaping like ``\n``, ``\t`` or ``\\`` is required.

In splices the input is interpreted as a Haskell String, therefore no newlines are allowed for instance and escape sequences such as ``\n``, ``\t`` and ``\\`` are necessary.
Furthermore literal ``"`` must be escaped also, as ``\"``.

.. note:: The library internal :ref:`escape sequences` are identical in QuasiQuotes and splices


Differences to/Advantages over other libraries
----------------------------------------------

There are a few advantages this libary has over other string formatting options.

#. The hard work happens at compile time

    Unlike libraries like `text-format <https://hackage.haskell.org/package/text-format>`_ and the `Text.Printf <https://www.stackage.org/haddock/lts-7.14/base-4.9.0.0/Text-Printf.html>`_ module parsing the format string, producing the string fragments and interleaving data and strings happens all at compile time.
    At runtime a single fusable string concatenation expression is produced.

    Furthermore all errors, like missing identifiers happen at compile time, not at runtime.

#. Type Polymorphism

    The created, interpolated string has no type.
    It can be interpreted as any string type, so long as there is an `IsString <https://www.stackage.org/haddock/lts-7.14/base-4.9.0.0/Data-String.html#t:IsString>`_ instance and the expressions inside return the appropriate type.

    This is different format string libraries like `text-format <https://hackage.haskell.org/package/text-format>`_ and the `Text.Printf <https://www.stackage.org/haddock/lts-7.14/base-4.9.0.0/Text-Printf.html>`_ module which always produce strings of a particular type and interpolation libraries like `interpolate <http://hackage.haskell.org/package/interpolate>`_ and `interpol <http://hackage.haskell.org/package/interpol>`_ which require instances of ``Show``.

#. Simple API and full Haskell support

    The interpolated expressions are just plain Haskell expressions, no extra syntax, beyond the interpolation braces ``#{}``.
    Also all Haskell expressions, including infix expressions, are fully supported.

    This is different from `Interpolation <http://hackage.haskell.org/package/Interpolation>`__ which introduces additional syntax and does not fully support infix expressions.
