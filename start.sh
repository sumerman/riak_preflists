#!/bin/bash

N=$1
[ $N ] || N=1
[ $2 ] &&  SUFFIX=@$2
NODE=riak_plist$N$SUFFIX

HTTP_PORT=$(( N + 8880 ))
HANDOFF_PORT=$(( N + 8100 ))
ROUTING_PORT=$(( N + 8090 ))
PB_PORT=$(( N + 8070 ))
DATA_DIR="./data${N}"
RING_STATE_DIR="${DATA_DIR}/ring"

export ERL_MAX_PORTS=64000
export ERL_MAX_ETS_TABLES=256000
export ERL_FULLSWEEP_AFTER=0
erl +K true +P 1000000 -name $NODE -setcookie dev -pa ebin -pa deps/*/ebin \
	-boot start_sasl \
   	-config ./app.config \
	-s riak_preflists_app \
	-riak_preflists port $ROUTING_PORT \
	-riak_core handoff_port $HANDOFF_PORT \
	-riak_core platform_data_dir \"$DATA_DIR\" \
	-riak_core ring_state_dir \"$RING_STATE_DIR\" \
	-riak_core http '[ { {0,0,0,0},'$HTTP_PORT'} ]' \
	-riak_kv pb_port $PB_PORT 

