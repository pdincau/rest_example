#!/bin/sh

curl -v -X POST -H "content-type: image/png" --upload-file erlang_logo http://localhost:8080/images?user_token=myusertoken

