Wall
===

Simple app written using MongoDB, Webmachine (Erlang+MochiWeb), Backbone.js, jQuery and Handlebars

### Dependencies:

  - MongoDB (http://www.mongodb.org/downloads)
  - Erlang (http://www.erlang.org/download.html)

### Get the code:

    git clone https://github.com/tegioz/wall.git

### Build:

    cd wall

Fetch dependencies:

    ./rebar get-deps

Compile: 
    
    ./rebar compile

### Configuration: 

Update Webmachine static resource root path:

    vi apps/rest_api/priv/dispatch.conf

and change this line with the correct path:

    {['*'], rest_api_static, [{root, "/YOUR_PATH_OF_INSTALLATION/wall/apps/rest_api/priv/www"}]}.

Configure your application editing the file app.config (defaults values are OK).

### Run:

Launch MongoDB:

    mkdir walldata
    mongod --dbpath=walldata

Launch application server:

    ./start.sh

Now open this URL in your browser:

    http://localhost:8000/wall/index.html

and you're done ;)

NGINX
---

Use this config if you want to use NGINX to serve the static content, acting as a reverse proxy to Webmachine RESTful API.

    server {
        listen       80; 
        server_name  www.your_server_name.com;
        access_log   /var/log/nginx/your_server_name.com.access.log  main;
        root    /YOUR_PATH_OF_INSTALLATION/wall/apps/rest_api/priv/www/;
 
        location /api { 
            proxy_pass      http://127.0.0.1:8000;
        }   
    }
