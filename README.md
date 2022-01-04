# GeoplotRgui

Run GeoplotR from a web browser.

# Docker deployment

To get GeoplotRgui running on port 3820:

```sh
docker pull timband/geoplotr:beta
docker run --name geoplotr -p 3820:80 -d timband/geoplotr:beta
```

# Rebuilding docker

Build it:

```sh
docker build -t timband/geoplotr:beta .
```

Run it to test it with the same line as above:

```sh
docker stop geoplotr
docker rm geoplotr
docker run --name geoplotr -p 3820:80 -d timband/geoplotr:beta
```

Upload it to Docker Hub.

```sh
docker push timband/geoplotr:beta
```

# Nginx stanza

If reverse-proxying to GeoplotRgui on a subpath, you need to
enable upgrading (to allow the websocket connection through)
and to set the `SCRIPT_NAME` header to the subpath:

```
location /geoplotr/ {
    proxy_pass http://127.0.0.1:3820/; # the trailing / is important!
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "Upgrade";
    proxy_set_header Host $host;
    proxy_set_header SCRIPT_NAME /geoplotr; # matches the subpath (without trailing /)
}
```

# github deployment

You can deploy directly from github, using the same nginx configuration
as above.

## install dependencies

GeoplotRgui depends on R. We will be 

```sh
sudo apt-get install nginx git r-base r-base-dev
sudo -Hu wwwrunner Rscript -e "install.packages('remotes', '~/R')"
```

## create system user (if needed and not already done)

You can begin with a system user; you can create one (here called
`wwwrunner`) like this:

```sh
sudo adduser --system wwwrunner
```

This will enable GeoplotRgui to run with minimal privileges, so that in the
event of a security breach via GeoplotRgui, the damage that can be done is
limited to the damage a normal user can do.

## systemd configuration

Copy the following into a new file `/etc/systemd/system/geoplotr.service`:

```
[Unit]
Description=GeoplotR
After=network.target

[Service]
Type=simple
User=wwwrunner
ExecStart=/usr/bin/Rscript -e "GeoplotRgui::GeoplotR(host='127.0.0.1',port=3820, daemonize=TRUE)"
Restart=always

[Install]
WantedBy=multi-user.target
```

Reload and enable:

```sh
sudo systemctl daemon-reload
sudo systemctl enable geoplotr
```

## make an update script

Copy the following into a new file `/usr/local/sbin/updateGeoplotR.sh`:

```sh
sudo -Hu wwwrunner Rscript -e \
"remotes::install_github(repo=c('pvermees/GeoplotR', 'tim-band/GeoplotRgui'),force=TRUE,lib='~/R')"
sudo systemctl restart geoplotr
```

Now make it executable and run it (it will take a while to run):

```sh
sudo chmod a+x /usr/local/sbin/updateGeoplotR.sh
sudo updateGeoplotR.sh
```

Check it is running with `sudo systemctl status geoplotr`.

### update via cron

Edit `root`'s cron schedule (not `wwwrunner`'s):

```sh
sudo crontab -e
```

Adding a line like the following (for a daily 3am update in this example):

```
0 3 * * * /usr/local/sbin/updateGeoplotR.sh
```

The `0` and `3` are the minutes and hours. The third `*` is the day of
the week, so change that to a number from `0` (Sunday) to `6`
(Saturday) to change it to a weekly update.
