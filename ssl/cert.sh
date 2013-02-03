#!/bin/bash
openssl genrsa -out key.pem 1024
openssl req -new -key key.pem -out req.pem
openssl x509 -req -in req.pem -signkey key.pem -out cert.pem
#openssl pkcs12 -export -in cert.pem -inkey key.pem -name gert -out gert.p12
