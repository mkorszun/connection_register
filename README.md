connection_register
===================

Build:

~~~bash
$ make compile
~~~

Setup 3 node cluster:

By default 'node1@localhost' is discovery master for rest of the nodes. You can change it by setting application env variable: `conntact_node` on each of the nodes.

On node 1:
~~~bash
$ make run NODE=node1 PORT=1111
~~~

On node 2:
~~~bash
$ make run NODE=node2 PORT=2222
$ (node2@localhost)3> connection_register:join().
~~~

On node 3:
~~~bash
$ make run NODE=node3 PORT=3333
$ (node3@localhost)3> connection_register:join().
~~~

On node 1 (or any other node):
~~~bash
$ (node1@localhost)3> connection_register:init_cluster(all_visible_nodes).
~~~

Check mnesia status on each of the nodes to see if cluster is properly set up:

~~~bash
$ (node1@localhost)3> mnesia:info().
~~~

Should look similar to this:

~~~bash
running db nodes   = [node1@localhost,node3@localhost,node2@localhost]
stopped db nodes   = []
master node tables = []
remote             = []
ram_copies         = [conn,schema]
disc_copies        = []
disc_only_copies   = []
[{node1@localhost,ram_copies},
 {node2@localhost,ram_copies},
 {node3@localhost,ram_copies}] = [schema,conn]
~~~

Start multiple tcp connections to all of the cluster nodes. Check on any of erlang node list of active connections:

~~~bash
$ (node1@localhost)3> connection_register:list_connections().
~~~

Kill some client connections and list connections again to see if closed one where removed from database.


