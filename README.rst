=================
Rolling Directory
=================

RollingDirectory is a daemon which monitors changes in directory contents 
limiting it's size to a given value.
The directory with all it's subdirectories get monitored.

Usage
-----

::

  $> RollingDirectory -h
    -h                     --help                       Print this helping text
    -D start|stop|restart  --daemon=start|stop|restart  Start the program in daemon mode
    -d directory           --dir=directory              The directory to monitor and process
    -s SIZE [MB|GB|TB]     --size=SIZE [MB|GB|TB]       The maximum size of the directory in KB,
                                                        the extensions 'MB', 'GB' and 'TB' are supported.

