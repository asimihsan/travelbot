#!/usr/bin/env python

import os
import sys
import pika

def main():
    connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
    channel = connection.channel()
    channel.basic_qos(prefetch_count=1)
    #channel.exchange_declare(exchange = '', type='direct')
    channel.queue_declare(queue = 'addition', durable=True)
    #channel.queue_bind(exchange = '',
    #                   queue = 'addition',
    #                   routing_key = 'addition')

    def callback(ch, method, properties, body):
        print "got: %s" % body

    channel.basic_consume(callback,
                          queue = 'addition',
                          no_ack = True)
    channel.start_consuming()

if __name__ == "__main__":
    main()
