#
# Regular cron jobs for the master-slave-detector package
#
0 4	* * *	root	[ -x /usr/bin/master-slave-detector_maintenance ] && /usr/bin/master-slave-detector_maintenance
