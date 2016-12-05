
Defining scripts is very easy.

Create a new Haskell source file like "MyScript.hs" and import marvins prelude `Marvin.Prelude`.
This provides you with all the tools you need to interact with marvin.
For more information why this is necessary see section [Why no prelude?](#why-no-prelude).

Now you can start to define your script with `defineScript` which produces a script initializer.
If you wish to use marvins automatic script discovery your script initializer should be named `script`  

```Haskell
{-# LANGUAGE NoImplicitPrelude #-}
module MyScript where

import Marvin.Prelude

script :: IsAdapter a => ScriptInit a
script = defineScript "my-script" $ do
    ...
```

The script id, "my-script" in this case, is the name used for this script when repoting loggin messages as well as the key for this scripts configuration, see [configuration](#configuration).

In the define script block you can have marvin react to certain events with `hear` and `respond`.
More information on those in the section [reacting](#reacting)

Finally after you have defined your scripts you have to tie them together.
You can do this [manually](#wiring-manually) or you can have marvin create the boilerplate code for you.

To do this simply place a main file (this is the file you'll be compiling later) in the same directory the scripts are placed in.
Leave the file empty except for this line at the top `{-# OPTIONS_GHC -F -pgmF marvin-pp #-}`.
When you compile the file marvin will look for any other ".hs" and ".lhs" files in the same directory, import them and define a server which runs with the `script` from each.
If you wish to hide a file from the auto discovery either place it in a different directory or prefix it with "." or "_".
