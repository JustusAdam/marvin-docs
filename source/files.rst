.. _files:

Files
=====

Some adapters support up- and download of files.
Not all adapters support this and the individual interactions are different between each of the adapters.

API 
---

Marvin encodes its file interaction API in a separate typeclass ``SupportsFiles``.
Adapters can opt to implement this class to support file interactions.
The concrete structure of local and remote files and metadata differs between each adapter.
However the API enforces certain basic rules on the data.

Each adapter may implement additional custom functionality, but if you wish to be adapter agnostic, for instance when implementing a library of reactions, you can rely on the API interface.

Remote Files
^^^^^^^^^^^^

Remote files must have

* An optional *name*, available through the ``name`` lens
* An optional *url*, from which the file can be downloaded, available through the ``url`` lens
* An optional *file type*, available through the ``fileType`` lens
* A *creation date*, available through the ``creationDate`` lens
* and a *size*, available through the ``size`` lens

All optional fields use lenses which return ``Maybe`` values.

Content of remote files can be downloaded using ``readTextFile`` or ``readFileBytes``.

Local Files
^^^^^^^^^^^

Local files must have

* A *name*, available through the ``name`` lens
* *content*, which is either in-memory or on-disk, available through the ``content`` lens
* and an optional *file type*, available through the ``fileType`` lens

Local files can be created using the ``newLocalFile`` function and uploaded using ``shareFile``.

Adapters which support the ``HasFiles`` class can emit the ``FileSharedEvent``.
This event can be handled with the ``fileShared`` and ``fileSharedIn`` triggers.

