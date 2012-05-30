master-slave
============

Simple tool for master-slave configure. use Erlang R15B

configure
---------

### rel/files/sys.config 
you must set:
minimal       - minimal nodes for live cluster,
path          - path for temp file with nodes list,
master_script - script execute on master node
slave_script  - script execute on slave node
dead_script   - script execute when cluster is dead

### rel/files/vm.args 
change name and setcookie here
name must be like "master@host.example.org"

compile
-------

./rebar compile ./rebar generate

usage
-----

get node in rel/master

for start: master/bin/master start

for stop: master/bin/master stop

get status: master/bin/master status
