Simple tool for master-slave configure.
use Erlang R15B

for configure change:
rel/files/sys.config
{live,['host1','host2']} - host1, host2 - live node
{master_script,"/tmp/script.sh"}
{slave_script, "/tmp/sscript.sh"}

rel/files/vm.args
change name and setcookie here 

compile:

./rebar get-deps 
./rebar compile
./rebar generate

get node in rel/master

for start:
master/bin/master start

for stop:
master/bin/master stop

get status:
master/bin/master status
