#!/bin/sh

curl -v -X DELETE http://localhost:8080/images/$1\?user_token\=myusertoken

