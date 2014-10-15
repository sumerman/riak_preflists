# Riak Preflists

This app is a distilled example of an approach used in a couple of riak-based applicaitons I've seen.
As you may know Riak allows to contact any node with any request. Although client is greatly simplified this way, there are cases when you want to contact "the right" node for a key. This simple application on being injected into Riak node exposes routing information through a simple HTTP API.

## API

All API endpoints piggybacks on Riak's HTTP interface, so host and port are the same.

### Ring status

`GET /riak_preflists/ringstatus`
Returns a json hash table of hosts and what part of the continuum they own.

For example:  
`curl http://localhost:8881/riak_preflists/ringstatus`  
returns something like:

```
{"0":"riak@127.0.0.1",
 "22835963083295358096932575511191922182123945984":"riak@127.0.0.1",
 "45671926166590716193865151022383844364247891968":"riak@127.0.0.1",
   ...
  "1415829711164312202009819681693899175291684651008":"riak@127.0.0.1",
  "1438665674247607560106752257205091097473808596992":"riak@127.0.0.1"
}
```

### Nodelists

`GET /riak_preflists/nodelists/buckets/Bucket/keys/Key`
Returns a list of hostnames for the `Bucket` and key `Key` in the form of JSON array.

For example:  
`curl http://localhost:8881/riak_preflists/nodelists/buckets/foo/keys/bar`  
returns `["sumerman-mbpr.local"]` on my machine :)

### Ring update notifications

Having to ask nodelist for every request may defeat the purpose of exposing it. But if one wants to cache it, (s)he needs a way to invalidate the cache. So this endpoint provides such a way.

`GET /riak_preflists/notifications[?ping=N]`
Returns an endless chunked stream of either `ping\n` or `update\n`. Pings are there as a means of health-check while an `update` is a sign to invalidate a cache of nodelists. The optional parameter allows to specify an interval in seconds between pings (1 sec by default).

## Build

- To build for injection just issue `make`. 
- If you want to play with it in a standalone mode issue `make standalone`. But beware, it will download and compile almost whole riak :)

### Run standalone

Open the first console and cd into the app's directory, then`$ ./start.sh`,
wait a bit until everything starts up, then (in the erlang console) issue `node()`

Open the second one and do the`$ ./start.sh 2`, wait a bit again, then type `riak_core:join('first_node@name')` where `first_node@name` is the thing returned by `node()` in the first console.

You can join up to 9 nodes in the same way.

API ports for the nodes are `8881`, `8882`, ... respectively.

### Inject into Riak

Just add 

```
-pa /path/too/riak_preflists/ebin
-s riak_preflists_app
```
to your `vm.args` file

You should see in the log something like

```
Application riak_preflists started on node 'riak@127.0.0.1'
```

When running into Riak, the REST API will be available under the same port as
Riak's one, so `8098` by default.