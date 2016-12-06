Each script in marvin is a Haskell module that defines a value `script` with the type `ScriptInit`.
It does not matter where in the module you define this value, only that it sits at the top level so that the main file can import it.
You can define arbitrary other values in the top level of your script, such as mutable variables and you can import any Haskell library you like including other marvin scripts (for [data sharing](#data-sharing)).

## Script boilerplate

Since each script is a Haskell module the module name and the file name must match. 
I.e. a script module MyScript must be in a file called `MyScript.hs`.
Furthermore the module and file name may only contain word characters and the underscore `_` and must begin with an upper case letter.

**Note:** A file which starts with an underscore `_` or dot `.` is ignored by the automatic script discovery of the main file.
This is a way to hide unfinished scripts from being included in the program.  

When you have created your source file you should first import marvins prelude `Marvin.Prelude` (something like marvins standard library).
It contains all the marvin related functions you will need.

*Aside:* You dont have to use `Marvin.Prelude`. The prelude is just a collection of other modules, you can also import just the ones you need directly, but this is only recommended for people experienced with Haskell.

```Haskell
-- File: MyScript.hs
module MyScript where -- Module definition (must match filename)

-- import the prelude
import Marvin.Prelude

-- import other modules and libraries you need

script :: IsAdapter a => ScriptInit a
script = defineScript "my-script" $ do
    -- here follows the actual scripting part
```

Lastly we define a value called `script` with the type signature `IsAdapter a => ScriptInit a`.
This complicated looking type signature ensures our script will work with any adapter that satisfies the adapter type class (adapter interface).
Here we call the function `defineScript` which takes an id string and an initializer block.

The id string is used fo two things

1. Scoping the config, i.e. the config for this script will be stored in the `scripts.<id-string>` key.
2. Logging. All logging messages from this script will be prefixed with `scripts.<id-string>`.

The initializer block is where the actual scripting starts.

## The initializer block

The initializer block is the code that is run when you start marvin.

First and foremost this block is used to add new [reactions](#reacting) to your marvin script, which is most likely the main part of your scripts funcitonality.

But you can do a variety of other things here such as [define periodic tasks](#periodic-tasks), [read-data](#on-disk) and [define mutable variables](#in-memory).

## Reacting

## Persistence

### In memory

### On disk

## Data sharing
