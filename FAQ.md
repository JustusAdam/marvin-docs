## I added a new script, why is the functionality not present?

### If you are using the automatic main file

You have to force the main file to be recompiled after adding or removing a script.
You can do this by running `stack clean` (if you use stack for building) or `cabal clean` (if you use cabal for building).

If that didn't fix:

- **For user scripts:** make sure the new script is in the script directory and the file does not start with `.` or `_` (those are ignored by `marvin-pp`). See [marvin-pp](../marvin-pp)
- **For external scripts:** make sure the script is mentioned in the `external-scripts.json` file. See [external scripts](external-scripts).

### If you are defining the main script manually

Make sure you imported the script module in your main file and added the script to the list of scripts for the call to `runMarvin`.

This will look something like this:

```Haskell
import qualified MyScript

main = runMarvin [MyScript.script]
````
