erl -pa deps\cowboy\ebin deps\cowlib\ebin deps\goldrush\ebin deps\gproc\ebin deps\lager\ebin deps\ranch\ebin apps\signaler\ebin -sasl errlog_type error -s signaler_app -config etc\app.config

