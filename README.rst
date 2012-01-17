=================
Rolling Directory
=================

RollingDirectory is a daemon which monitors changes in directory contents 
limiting it's size to a given value.
The directory with all it's subdirectories get monitored.


TODO
----

* Use better datastructures for the file list (something presorted).

* Don't rescan the whole structure on every change, only apply changes

* Check if the INotify *Create* is enough or if we also have to trigger on *Modify*,
  perhaps make it configurable with a cmdline option.
  
* Add different filesize units in cmdline (MB, GB, TB)  