API breaking changes
====================


Since version 0.3
-----------------

The slack adapter no longer automatically prepends a ``"#"`` to channel names.
This means channel resolution functions such as ``resolveChannel`` now work on the channel name directly.
Example: to resolve the channel ``#random`` use ``resolveChannel "random"``.

This also affects channel referencing handlers and functions such as ``enterIn`` and ``messageChannel``.
Example: what was previously the handler ``enterIn "#random"`` is now ``enterIn "random"``.
