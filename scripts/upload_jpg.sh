#!/bin/sh

curl -v -X POST -H "content-type: image/jpg" --upload-file erlang http://localhost:8080/images

