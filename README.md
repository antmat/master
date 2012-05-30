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

after compile you can find conf files in

master/releases/1/vm.args

master/releases/1/sys.config

compile
-------

./rebar compile ./rebar generate

usage
-----

copy bottle ./rel/master to every host in cluster

set name for every host in master/releases/1/vm.args

### in bottle
start application: ./bin/master start

stop application: ./bin/master stop

When node was started you can connect node to cluster

./bin/master connect master@host1.example.org

get node status: ./bin/master who_am_i          -> slave|master

get master node: ./bin/master who_is_master     -> 'nodename'|none ( if cluster dead)

get all slaves:  ./bin/master who_is_slaves     -> ['nodename']

get live nodes:  ./bin/master who_is_live       -> ['nodename']

get dead nodes:  ./bin/master who_is_dead       -> ['nodename']

get cluster status: ./bin/master cluster_status -> work|dead

