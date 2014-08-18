---
title: CommaFeed, a self-hosted RSS reader
author: David Sferruzza
lang: en
tags: rss, supervisor, debian, java
description: How to install it on Debian
published: 2014-08-18 18:04:00+02:00
---

## Introduction

A RSS reader (or aggregator) is a software that allows you to read syndicated web content (like news, blog posts, ...) in one unique place.
This is very useful because:

- you don't have to visit every website/blog you follow to read new articles
- you can mark an article as *read* or *unread* (like you would do with an email in your inbox)
- you can read the content directly inside the reader (which is faster), or click on a link and open it in your browser (which gives you the full experience)
- you can let the reader opened all day (my CommaFeed tab in Firefox is pinned, and a little white dot appears on it when there are new articles)

CommaFeed is a *"Google Reader inspired self-hosted RSS reader"*.
You can get the source code on [GitHub](https://github.com/Athou/commafeed), and there is a free official instance [here](https://www.commafeed.com/).

![A (quite old) preview screenshot](https://raw.githubusercontent.com/Athou/commafeed/master/src/main/app/images/preview.jpg)

As CommaFeed is a web application, it doesn't have to run on the same computer you use to read news.
You can host it on a server, so that you can get your unread news and mark them as read no matter where you are.

I will explain how to install you own instance.

## Requirements

First, you need a server.
A real server, a VPS, or a local virtual machine will do.
In this document, I will assume you have [Debian](http://www.debian.org/) installed on it, but it's not a requirement.

You will need to have the following packages installed:

- git
- openjdk-7-jdk
- maven (3.x)
- supervisor

Finally, you will need a database server.
Here I will assume you have a MariaDB (or MySQL) server running on localhost, with a `commafeed:commafeed` user that has write access on a `commafeed` database (you can also use PostgreSQL or SQLServer).

## Installation

First, we create a user and its home:

```bash
useradd -d /opt/commafeed -s /bin/bash commafeed
mkdir /opt/commafeed
chown commafeed. /opt/commafeed
su - commafeed
```

Let's clone the repo:

```bash
git clone https://github.com/Athou/commafeed.git .
cd commafeed
```

Build the project (it can take around 5 minutes):

```bash
mvn clean package
```

Create the config file:

```bash
cp config.yml.example config.yml
```

Now, you need to edit `config.yml`.
Get to the `database` section, and put:

```yaml
database:
  driverClass: com.mysql.jdbc.Driver
  url: jdbc:mysql://localhost/commafeed?autoReconnect=true&failOverReadOnly=false&maxReconnects=20&rewriteBatchedStatements=true
  user: commafeed
  password: commafeed
```

I also recommend to enable two other options:

- `pubsubhubbub`: to automatically use the [PubSubHubbub](https://code.google.com/p/pubsubhubbub/) protocol on compatible feeds
- `imageProxyEnabled`: images in the news will be served through CommaFeed and not directly by the website that published the content

Let's try to launch it!

```bash
java -jar target/commafeed.jar server config.yml
```

You should see some log messages in the console while CommaFeed is launching and setting up the database.
Then you can go to `http://myserver:8082` with your web browser (where `myserver` is your server's IP/domain).
You should see the welcome page.
Type `Ctrl + C` in the console to kill CommaFeed.

Now, let's configure [Supervisor](http://supervisord.org/) to *"daemonize"* CommaFeed, so that we can launch it automatically on system startup.

Create a `/etc/supervisor/conf.d/commafeed.conf` file (as root) with this content:

```ini
[program:commafeed]
directory=/opt/commafeed
command=java -jar target/commafeed.jar server config.yml
process_name=commafeed
user=commafeed
autostart=true
autorestart=true
```

Reload Supervisor's config and check if CommaFeed is running:

```bash
supervisorctl reload
supervisorctl status
```

You should see that CommaFeed is running, and it will keep running even if you close your console.
You can go to `http://myserver:8082`, use `admin:admin` to log in, change `admin` password (or disable `admin` user), create a new user and start using it!

## Using a custom HTTP frontend

CommaFeed works, but you might want more, like accessing it on port 80 or 443 (with SSL) using a virtual host (so you can host other apps reachable via port 80/443).

In this document, I will use Apache2, but you can setup a similar configuration with any decent HTTP server.

First, install the `apache2` package.

If you want SSL, you need to add the following line in `/etc/apache2/ports.conf`:

```
NameVirtualHost *:443
```

It allows Apache to use virtual hosts on port 443[^1].
Then, let's enable some mods:

```bash
a2enmod proxy proxy_http ssl
```

For SSL, I will assume that you have already created a certificate and its key[^2] in a `/etc/commafeed` directory (it can be anywhere as long as the web server's user can read it).

Now we can create a virtual host in `/etc/apache2/sites-available/commafeed`:

```
<VirtualHost *:443>
    ServerName rss.mydomain.com

    <IfModule mod_ssl.c>
        SSLEngine on
        SSLCertificateFile /etc/commafeed/commafeed.pem
        SSLCertificateKeyFile /etc/commafeed/commafeed.key
    </IfModule>

    <IfModule mod_proxy.c>
        ProxyRequests off
        ProxyVia on
        ProxyPass / http://localhost:8082/
        ProxyPassReverse / http://localhost:8082/
        <Proxy *>
            Order deny,allow
            Allow from all
        </Proxy>
    </IfModule>
</VirtualHost>
```

Or, if you don't want SSL:

```
<VirtualHost *:80>
    ServerName rss.mydomain.com

    <IfModule mod_proxy.c>
        ProxyRequests off
        ProxyVia on
        ProxyPass / http://localhost:8082/
        ProxyPassReverse / http://localhost:8082/
        <Proxy *>
            Order deny,allow
            Allow from all
        </Proxy>
    </IfModule>
</VirtualHost>
```

Now, we can enable it, and restart Apache:

```bash
a2ensite commafeed
service apache restart
```

Last thing, we need to update the `app.publicUrl` key in CommaFeed's config:

```yaml
app:
  # url used to access commafeed
  publicUrl: https://rss.mydomain.com
```

Restart CommaFeed:

```bash
supervisorctl restart commafeed
```

And head over to `https://rss.mydomain.com`!

## Update

If you want to update CommaFeed, here is a simple script:

```bash
#!/bin/sh

cd /opt/commafeed
su commafeed -c "git pull"
su commafeed -c "mvn clean package"
supervisorctl restart commafeed
supervisorctl tail -f commafeed
```

Pay attention to the list of modified files displayed when the `git pull` command is executed: if `config.yml.example` has changed, you might need to take a look and edit `config.yml`.

## Share the love

If (like me) you think CommaFeed is a great software, share the love by:

- using it
- telling your friends about it
- giving some [feedback](https://github.com/Athou/commafeed/issues)
- translating it
- improving it
- giving it a [Flattr](https://flattr.com/thing/1409652/Athoucommafeed-on-GitHub)

*I'm not affiliated with this project, but I'm using it everyday for a while (and sometimes reporting bugs).*

[^1]: See <https://httpd.apache.org/docs/2.2/mod/core.html#NameVirtualHost> for more details
[^2]: Have a look to this guide: <https://wiki.debian.org/Self-Signed_Certificate>
