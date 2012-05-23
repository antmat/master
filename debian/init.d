#!/bin/sh
# -*- tab-width:4;indent-tabs-mode:nil -*-
# ex: ts=4 sw=4 et

CUR_PWD=$PWD
cd /var/lib/msdetector/
/var/lib/msdetector/bin/master $1
RET=$?
cd $CUR_PWD

exit $RET

RUNNER_BASE_DIR=/var/lib/msdetector
RUNNER_ETC_DIR=/etc/msdetector
RUNNER_LOG_DIR=/var/log/msdetector
# Note the trailing slash on $PIPE_DIR/
PIPE_DIR=/tmp/msdetector/
RUNNER_USER=msdetector

# Make sure log directory exists
mkdir -p $RUNNER_LOG_DIR
chown -R msdetector:root $RUNNER_LOG_DIR
# Make sure this script is running as the appropriate user
#if [ ! -z "$RUNNER_USER" ] && [ `whoami` != "$RUNNER_USER" ]; then
#    exec sudo -u $RUNNER_USER -i
#fi

CUR_PWD=`pwd`
# Make sure CWD is set to runner base dir
cd $RUNNER_BASE_DIR

# Identify the script name
SCRIPT=`basename $0`

# Parse out release and erts info
START_ERL=`cat $RUNNER_BASE_DIR/releases/start_erl.data`
ERTS_VSN=${START_ERL% *}
APP_VSN=${START_ERL#* }

VMARGS_PATH="$RUNNER_ETC_DIR/vm.args"
echo "$VMARGS_PATH"

CONFIG_PATH="$RUNNER_ETC_DIR/app.config"
echo "$CONFIG_PATH"
# Extract the target node name from node.args
NAME_ARG=`egrep '^-s?name' $VMARGS_PATH`
if [ -z "$NAME_ARG" ]; then
    echo "vm.args needs to have either -name or -sname parameter."
    exit 1
fi

# Extract the target cookie
COOKIE_ARG=`grep '^-setcookie' $VMARGS_PATH`
if [ -z "$COOKIE_ARG" ]; then
    echo "vm.args needs to have a -setcookie parameter."
    exit 1
fi

# Add ERTS bin dir to our path
ERTS_PATH=$RUNNER_BASE_DIR/erts-$ERTS_VSN/bin

# Setup command to control the node
NODETOOL="$ERTS_PATH/escript $ERTS_PATH/nodetool $NAME_ARG $COOKIE_ARG"

# Check the first argument for instructions
case "$1" in
    start)
        # Make sure there is not already a node running
        RES=`$NODETOOL ping`
        echo "$NODETOOL"
        if [ "$RES" = "pong" ]; then
            echo "Node is already running!"
            exit 1
        fi
        HEART_COMMAND="/etc/init.d/$SCRIPT start"
        echo "HEART_COMMAND: $HEART_COMMAND"
        export HEART_COMMAND
        mkdir -p $PIPE_DIR
        echo "PIPE_DIR: $PIPE_DIR"
        shift # remove $1
        $ERTS_PATH/run_erl -daemon $PIPE_DIR $RUNNER_LOG_DIR "exec /etc/init.d/$SCRIPT console $@" 2>&1
        ;;

    stop)
        # Wait for the node to completely stop...
        case `uname -s` in
            Linux|Darwin|FreeBSD|DragonFly|NetBSD|OpenBSD)
                # PID COMMAND
                echo "ps ax -o pid= -o command=|\
                                    grep "$RUNNER_BASE_DIR/.*/[b]eam"|awk '{print $1}'"
                PID=`ps ax -o pid= -o command=|\
                    grep "$RUNNER_BASE_DIR/.*/[b]eam"|awk '{print $1}'`
                ;;
            SunOS)
                # PID COMMAND
                PID=`ps -ef -o pid= -o args=|\
                    grep "$RUNNER_BASE_DIR/.*/[b]eam"|awk '{print $1}'`
                ;;
            CYGWIN*)
                # UID PID PPID TTY STIME COMMAND
                PID=`ps -efW|grep "$RUNNER_BASE_DIR/.*/[b]eam"|awk '{print $2}'`
                ;;
        esac
        $NODETOOL stop
#        ES=$?
#       if [ "$ES" -ne 0 ]; then
#            exit $ES
#        fi
        if [ "$PID" != "" ]
        then
            while `kill -0 $PID 2>/dev/null`;
            do
                sleep 1
            done
        fi
        ;;

    restart)
        ## Restart the VM without exiting the process
        $NODETOOL restart
        ES=$?
        if [ "$ES" -ne 0 ]; then
            exit $ES
        fi
        ;;

    reboot)
        ## Restart the VM completely (uses heart to restart it)
        $NODETOOL reboot
        ES=$?
        if [ "$ES" -ne 0 ]; then
            exit $ES
        fi
        ;;

    ping)
        ## See if the VM is alive
        $NODETOOL ping
        ES=$?
        if [ "$ES" -ne 0 ]; then
            exit $ES
        fi
        ;;

    status)
        ## See if the VM is alive
        $NODETOOL status
        ES=$?
        if [ "$ES" -ne 0 ]; then
            exit $ES
        fi
        ;;


    attach)
        # Make sure a node IS running
        RES=`$NODETOOL ping`
        ES=$?
        if [ "$ES" -ne 0 ]; then
            echo "Node is not running!"
            exit $ES
        fi

        shift
        exec $ERTS_PATH/to_erl $PIPE_DIR
        ;;

    console|console_clean)
        # .boot file typically just $SCRIPT (ie, the app name)
        # however, for debugging, sometimes start_clean.boot is useful:
        case "$1" in
            console)        BOOTFILE=$SCRIPT ;;
            console_clean)  BOOTFILE=start_clean ;;
        esac
        # Setup beam-required vars
        ROOTDIR=$RUNNER_BASE_DIR
        BINDIR=$ROOTDIR/erts-$ERTS_VSN/bin
        EMU=beam
        PROGNAME=`echo $0 | sed 's/.*\\///'`
        CMD="$BINDIR/erlexec -boot $RUNNER_BASE_DIR/releases/$APP_VSN/$BOOTFILE -mode embedded -config $CONFIG_PATH -args_file $VMARGS_PATH -- ${1+"$@"}"
        export EMU
        export ROOTDIR
        export BINDIR
        export PROGNAME

        # Dump environment info for logging purposes
        echo "Exec: $CMD"
        echo "Root: $ROOTDIR"

        # Log the startup
        logger -t "$SCRIPT[$$]" "Starting up"

        # Start the VM
        exec $CMD
        ;;

    *)
        echo "Usage: $SCRIPT {start|stop|restart|reboot|ping|console|console_clean|attach|status}"
        exit 1
        ;;
esac

cd  $CUR_PWD

exit 0
