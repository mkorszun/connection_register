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
$ make run NODE=node1
~~~

On node 2:
~~~bash
$ make run NODE=node2
$ (node2@localhost)3> connection_register:join().
~~~

On node 3:
~~~bash
$ make run NODE=node2
$ (node3@localhost)3> connection_register:join().
~~~

On node 1:
~~~bash
$ (node1@localhost)3> connection_register:init_cluster([node()|nodes()]).
~~~

Start tcp connections and Enjoy!!!

