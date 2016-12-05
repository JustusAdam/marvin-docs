Representation of Strings is a sore spot in Haskell, unfortunately.

The fundamental problem is that the 'default' `String`, the `String` type from the standard library, is a linked list of characters.
Nicely enough it is unicode capeable and handles special characters nicely, however using strings as linked lists is very inefficient.

Despite this marvin uses the default `String`, because it is easier to use for beginners.

However when you interact with other libraries you might encounter other string types, such as `ByteString` (often the result of HTTP requests or input for JSON decoding) and `Text`.
Both of theses also have a strict and a lazy variant.
If you need to convert theses values to and from `String` you can use `pack` and `unpack` in:

- `Data.Text` for strict `Text` 
- `Data.Text.Lazy` for lazy `Text`
- `Data.ByteString.Char8` for strict `ByteString`
- `Data.ByteString.Lazy.Char8` for lazy `ByteString`

If you want to know which String type you have, look at the module.

- strict `Text` is something like `Data.Text.(Internal.)?.Text`
- lazy `Text` is something like `Data.Text.(Internal.)?.Lazy.Text`

`ByteString` uses the same naming scheme, just replace `Text` with `ByteString`.
