.. _external-scripts:

External scripts
================

Since marvin scripts are just Haskell values adding external scripts is as easy as importing a library.

Assuming you use cabal or stack for building your project you need to add the library to your ``.cabal`` file.
Each library may define multiple scripts.

As with user scripts you need to wire the scripts into the main file.

If you manually create the main file, you add the script like a user script by importing the module and adding the scripts to the list of scripts.

If you use the automatic main file you can add external scripts by listing the modules to import in the ``external-scripts.json`` file.
Currently the ``external-scripts.json`` only supports listing modules.
This means each external script must be in its own module and be named ``script``.


.. code-block:: json

    [ 
        "Marvin.Script.SomeScript",
        "Marvin.Script.AnotherScript",
        "SomeUserScript",
        "Some.Library.Script"
    ]


.. note:: The API around `external-scripts.json` is not stable and it will probably change in the future, although we might preserve backward compatibility.

    You can join the discussion around its design on `GitHub <https://github.com/JustusAdam/marvin/issues/8>`__.
