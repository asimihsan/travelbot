# tcpproxy

## Introduction

tcpproxy exposes a TCP interface that allows access to:
-	workers bound to an AMQP queue. This allows distributed queue-based RPC calls.
-	concurrent RPC calls. You can push in as much RPC calls as you want and get responses in arbitrary order.

It is also intended to allow access to:
-	streaming RPC responses from workers over a long period of time.
-	bi-directional access to some data, for example a stream of JSON messages that indicate chat activity.

## How to run tests

-	Install dependencies:
	-	build-essentials
	-	git
	-	erlang
	-	rabbitmq
	-	python
	-	`git clone …`
	-	`mkvirtualenv travelbot`
	-	`pip install -r src/tcpproxy/requirements.txt`

-	Execute RabbitMQ in the background:

```
rabbitmq-server -detached
```
		
-	Execute the Celery workers that execute RPC calls via the AMQP broker:

```
cd src/tcpproxy/test/
celery worker --app=test_celery -l info
```
		
-	Execute the Celery transducer. This:
	-	binds to exchange `tcpproxy` with on queue `celery_transducer` with routing key `task`,
	-	launches the correct Celery task for each JSON request.
	-	waits for the result for a given task.
	-	publish task results to exchange `tcpproxy` with routing key `task_result_<<UUID>>`, where UUID is parsed from the initial request.

```
cd src/tcpproxy/test/
./test_consume_queue.py
```
			
-	Execute the tcpproxy itself. This:
	-	listens on a TCP port and pushes JSON-serialized tasks onto exchange `tcpproxy` with routing key `task`.
	-	subscribes indefinitely to exchange `tcpproxy` on queue `task_result_<<UUID>>`, where UUID is generated earlier. Routing key is the same as the queue name.
	-	when a task result comes on push it out on the TCP socket and delete the AMQP queue.
	
```
cd src/tcpproxy/
make
make build_plt # this will take ~30 minutes.
make start
```

-	Execute the tests

```
cd src/tcpproxy/test
nosetests -x -v test_tcp_to_amqp_protocol.py
```
