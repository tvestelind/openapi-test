# rss-dummy-backend
Start the daemon by either:
- running it in background, `docker run -d --rm -p8080:8080 tvestelind/rss-dummy-backend` or;
- running it in foreground (which lets you see logging), `docker run -it --rm -p8080:8080 tvestelind/rss-dummy-backend`

If you want to mount your own directory that contains your own RSS XML files, then run:
`docker run -d --rm -p8080:8008 -v ~/rssfiles/on/host:/tmp/myrssfiles tvestelind/rss-dummy-backend --feedsdir /tmp/myrssfiles`

You should hopefully see some documentation by going to:
`http://localhost:8080/docs`
You can also query data directly from the documentation or by doing:
`curl localhost:8080/feeds`
`curl localhost:8080/feeds/{channeluuid}`

So far the daemon only support RSS2.0 XML-files.
