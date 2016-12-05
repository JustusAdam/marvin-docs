Marvin projects basically comprise of a collection of individual scripts and a main file which ties them all together.

To start a new project marvin procvides an initializer, called `marvin-init`.
The initializer will set up a new project for you, including a sample script, the main file and the Haskell project configuration to make compiling smooth and easy.

If you install marvin through cabal (`cabal install marvin`) or stack (`stack install marvin`) it will install the initializer as well and add it to your path.
To see the options of the initializer run `marvin-init --help` on your command line.

## Install marvin

You can get a release version of marvin on [Hackage](https://hackage.haskell.org/package/marvin).

However this library is still a very early stage so you might want to get updates quicker. 
You can do so by using [stack](https://docs.haskellstack.org) and adding a recent commit of this repository to your `stack.yaml` file.
Stack will take care of downloading and building it for you.

Your `stack.yaml` should look something like this:

```Yaml
# stack.yaml
packages:
- '.'
- location:
    git: https://github.com/JustusAdam/marvin
    commit: 46cd98d179be6f8fc4e385a76f9ea38340a687ad
```

## Scripts and the main file

The functionality for your marvin installation is split into individual parts, called scripts.
Each script is some Haskell structure created with `defineScript`.
Scripts can be user defined or be included externally.

### External scripts

You can include external scripts in the form of a library.
To do this you must add the library name to the `.cabal` and `stack.yaml` file of your project. 

<!-- TODO add example -->

### User defined scripts

You can also write some scripts yourself.
Typically scripts are a Haskell source file which defines a `script` value.

As an example, a "hello world" script.

```Haskell
-- file named "HelloWorld.hs" (must be the same as module name + ".hs")
module HelloWorld where

import Marvin.Prelude

-- This type signature is necessary to help the compiler
script :: IsAdapter a => ScriptInit a
script = defineScript 
            "hello-world" -- script name (for logging and config) 
            $ do  -- here follows the actual script definition
                ...
```

You can find more information on the actual script content in the [scripting](../scripting) section.

## The main file

This file (ususally called `Main.hs`) ties the scripts together and defines the [adapter](../adapters) which your marvin project uses.

The main file looks someting like this:

```Haskell
-- import marvin runner
import Marvin.Run
-- imports chosen adapter
import Marvin.Adapter.Slack

-- import all scripts
import qualified HelloWorld
import qualified MyScript

-- list of all scripts to use
scripts :: [ScriptInit SlackRTMAdapter]
scripts = [ HelloWorld.script 
          , MyScript.script 
          ]

main :: IO ()
main = runMarvin scripts
```

You can write the main file yourself, but this can get tedious as you add more and more scripts.
To make this easier Marvin includes a utility which allows you to let the main file be generated automatically, called `marvin-pp`.
`marvin-pp` creates the main file dynamically at compile time by scanning your project for scripts.
You can add external scripts by adding an `external-scripts.json` file and `marvin-pp` will add those to your main file then.

**Important**: The `marvin-pp` generator is a compile time preprocessor and thus its output is often cached by your build system. As a result you have to run `cabal clean` or `stack clean` after you added a new script or removed one to force the build system to regenerate the main file.

