Strings and String conversions (in Haskell)
===========================================

Representation of Strings is a sore spot in Haskell, unfortunately.

The fundamental problem is that the 'default' ```String`` <https://www.stackage.org/haddock/lts-7.13/base-4.9.0.0/Data-String.html#t:String>`, the ``String`` type from the standard library, is a *linked list* of characters.
Nicely enough it is unicode capeable and handles special characters nicely, however using linked lists as strings is very inefficient.

Despite this marvin uses the default ``String``, because it is easier to use for beginners.

However when you interact with other libraries you might encounter other string types, such as ``ByteString`` (often the result of HTTP requests or input for JSON decoding) and ``Text``.
Both of theses also have a **strict** and a **lazy** variant.
It is not really necessary to know the difference between the lazy and strict variants of these strings, suffice to say they are not the same thing.
If you need to convert theses values to and from ``String`` you can use ``unpack`` (converts *to* ``String``) and ``unpack`` (convertes *from* ``String``) in:

* ```Data.Text`` <https://www.stackage.org/haddock/lts-7.13/text-1.2.2.1/Data-Text.html>` for strict ``Text`` 
* ```Data.Text.Lazy`` <https://www.stackage.org/haddock/lts-7.13/text-1.2.2.1/Data-Text-Lazy.html>` for lazy ``Text``
* ```Data.ByteString.Char8`` <https://www.stackage.org/haddock/lts-7.13/bytestring-0.10.8.1/Data-ByteString-Char8.html>` for strict ``ByteString``
* ```Data.ByteString.Lazy.Char8`` <https://www.stackage.org/haddock/lts-7.13/bytestring-0.10.8.1/Data-ByteString-Lazy-Char8.html>` for lazy ``ByteString``

If you need to convert between the strict and lazy variants of these strings there is a ``fromStrict`` (convert *to* lazy) and ``toStrict``

If you want to know which String type you have, look at the module.

- strict ``Text`` comes from either ``Data.Text`` or ``Data.Text.Internal``
- lazy ``Text`` comes from either ``Data.Text.Lazy`` or ``Data.Text.Internal.Lazy``

``ByteString`` uses the same naming scheme, just replace ``Text`` with ``ByteString``.
