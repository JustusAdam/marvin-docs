Strings and String conversions (in Haskell)
===========================================

Representation of Strings is a sore spot in Haskell, unfortunately.

The fundamental problem is that the 'default' `String <https://www.stackage.org/haddock/lts-7.13/base-4.9.0.0/Data-String.html#t:String>`_, the ``String`` type from the standard library, is a *linked list* of characters.
Nicely enough it is unicode capeable and handles special characters nicely, however using linked lists as strings is very inefficient.

Therefore marvin uses a more efficient string type called ``Text``. 
To be precise the ``Text`` type in ``Data.Text.Lazy`` from the `text <https://hackage.haskell.org/package/text>`_ library.

Functions exposed by the marvin library generally ALL deal with this string type, to make it as easy as possible for the user.

However when you interact with other libraries you might encounter other string types, such as ``ByteString`` (often the result of HTTP requests or input for JSON decoding) and ``String`` from the standard library, often in the form of ``FilePath`` as name for files and directories.

Furthermore both ``Text`` and ``ByteString`` have a **strict** and a **lazy** variant.
It is not really necessary to know the difference between the lazy and strict variants of these strings, suffice to say they are not the same thing.
If you need to convert theses values to and from lazy ``Text`` you can use ``unpack`` (converts *to* ``String``) and ``pack`` (convertes *from* ``String``) in:


If you need to convert between the strict and lazy variants of these strings there is a ``fromStrict`` (convert *to* lazy) and ``toStrict``

If you want to know which String type you have, look at the module.

- strict ``Text`` comes from either ``Data.Text`` or ``Data.Text.Internal``
- lazy ``Text`` comes from either ``Data.Text.Lazy`` or ``Data.Text.Internal.Lazy``

``ByteString`` uses the same naming scheme, just replace ``Text`` with ``ByteString``.
