# File Sender

The project is started for training purposes in Erlang programming. It is under developing,
see **TODO** section of planned further features.

Sending files over network from one node (server) to others (clients).

### Building

```shell
rebar3 compile
```

### Running tests

```shell
rebar3 eunit
```

### Usage example

Starting server node:

```shell
cd _build/default/lib/file_sender/ebin
erl -sname server -setcookie 'MY_FANTASTIC_COOKIE'
application:ensure_started(file_sender).
```

On the client node:

```shell
cd _build/default/lib/file_sender/ebin
erl -sname client -setcookie 'MY_FANTASTIC_COOKIE'
net_kernel:connect_node(server@myhost).
application:ensure_started(file_sender).
```

Check current server state:

```shell
filer:print_state().
```

Set source dir on the server:

```shell
filer:set_source_dir("/tmp/1").
```

Get first file from the source dir:

```shell
filer:get_file().
```

Get 3 files from the source dir:

```shell
filer:get_file(3).
```

Unset the source dir:

```shell
filer:unset_source_dir().
```

# TODO

- check is file was sent successfully
- set_target_dir
- cover test
- performance test of writing/sending files
- separate state for nodes: source dirs, files and so on
- use ETS instead of keeping files list in state?
- working with remote nodes
- termination of the app
- registering nodes
- connection setting (port number) should be set up during init