WebRTC Example
=============

This is a web application that allows you to video chat with someone else.
Just create a virtual room, share the link with someone and start a video chat.
The application also supports traditional web chat using text messages.

Written using JavaScript and Erlang.

### Examples of using

  - http://www.oslikas.com built using this code

### Tested with browsers

  - Firefox version 22 or later
  - Google Chrome version 23 or later

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

  signaler_listen_port - TCP port to listen on

    vi www/js/rtc_lib.js

  Edit first line  

  var wsURI = 'ws://www.webrtcexample.com/s';

  and change URL to [signaler_listen_host]

### Nginx

You should use NGINX >= 1.4 to make it work  
Edit your nginx configuraton file and add these lines into the website configuration:

    location /s { 
    proxy_http_version 1.1; 
    proxy_set_header Upgrade $http_upgrade; 
    proxy_set_header Connection "upgrade"; 
    } 

### Run

    ./start.sh - to start in console 
    ./start_detached.sh - to start detached (in background)

Now configure your web server, point them on [www] folder as a 'root dir', and open your new web site.

