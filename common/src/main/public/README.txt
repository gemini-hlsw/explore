The "conf.json" and "staging.conf.json" files are deprecated. They
are only here so that old serviceWorkers won't fail when they can't
find the config file they are expecting.

"staging.conf.json" is only needed by serviceWorkers that were loaded
during a short transition of 1 day on 19 June 2023 and is probably only 
needed by developers and Andy. It can be deleted soon.

"conf.json" is expected by serviceWorkers that stretch back for many
months so probably should be kept longer. 
