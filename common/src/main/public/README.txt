The "conf.json" files is deprecated. It is only here so that old serviceWorkers 
won't fail when they can't find the config file they are expecting. The working
assumption is that everyone who uses staging has updated in the 5 months
since the change was made to "environments.conf.json", so the only issue will be
with production.

Eventually, this file and conf.json should be deleted.
