#!/bin/sh
CUR_PWD=$PWD
cd /var/lib/msdetector/
/var/lib/msdetector/bin/master $1
RET=$?
cd $CUR_PWD
exit $RET
