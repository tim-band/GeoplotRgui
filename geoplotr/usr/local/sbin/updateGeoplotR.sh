sudo -Hu wwwrunner Rscript -e "remotes::install_github(repo=c('tim-band/shinylight','pvermees/GeoplotR','pvermees/GeoplotRgui'),force=TRUE,lib='~/R')"
# We use sudo here so that the user only gets asked for a password once
sudo /usr/local/sbin/geoplotrctl restart
