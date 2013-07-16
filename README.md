WebRTC Example
=============

Video conference, web chat, video 'chat rouelette' using Erlang, JavaScript and WebRTC.

### Dependencies:

  - Erlang (http://www.erlang.org/download.html) 
  - Rebar (http://github.com/basho/rebar)

### Get the code:

    git clone https://github.com/fycth/webrtcexample.git

### Build

    cd webrtcexample 
    rebar get-deps 
    rebar compile

### Configuration

    vi etc/app.config

UI  

  listen_host - your server's domain name or IP address 
  listen_port - TCP port to listen on

Signaler  

  signaler_listen_host - your server's domain name or IP address 
  signaler_listen_port - TCP port to listen on

    vi www/js/rtc_lib.js

  Find this line "connection = new WebSocket('ws://www.webrtcexample.com:8087/s');" - around line 42  
  and change URL to [signaler_listen_host] and [signaler_listen_port]

### Run

    ./start.sh - to start in console 
    ./start_detached.sh - to start detached (in background)

Now open URL

    http://listen_host/

NGINX
---

    server {  
        listen 80;  
        server_name www.your_server_name.com  
        location /chat {  
            proxy_pass http://listen_host:listen_port;  
        }  
    }  


