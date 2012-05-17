Simple tool for master-slave configure.
use Erlang R15B

for configure change
rel/files/sys.config
rel/files/vm.args

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
