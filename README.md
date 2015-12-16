# riak-mesos-erlang
Top-level application forming the erlang-based Mesos Framework for running Riak

### Building

```
make rel
```

Alternatively `make stage` then `make recompile` when changes are made to `*.erl`.

### Testing

#### Run the riak_mesos application

```
./rel/riak_mesos/bin/riak_mesos start
```

#### Clique Based CLI

Get the status

```
./rel/riak_mesos/bin/riak-mesos-admin status
```

```
{"status":"Initial Status"}
```

Set the status

```
./rel/riak_mesos/bin/riak_mesos-admin status -s Hello There
```

```
{"status":"Hello There"}
```

#### Webmachine HTTP Resource

Get the status

```
curl http://localhost:9090/api/v1/status
```

```
{"status":"Hello There"}
```

Set the status

```
curl -i -XPUT -H "Content-Type: application/json" \
    'http://localhost:9090/api/v1/status'
    -d '{"status": "Hi There"}'
```

```
HTTP/1.1 204 No Content
Server: MochiWeb/1.1 WebMachine/1.10.8 (that head fake, tho)
Date: Mon, 27 Jul 2015 20:12:17 GMT
Content-Type: application/json
Content-Length: 0
```
