erl -pa deps/*/ebin apps/*/ebin -sasl errlog_type error -s vchat_app -config etc/app.config -detached -setcookie curlybob -sname webrtcexample
