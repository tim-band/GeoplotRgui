sudo -Hu wwwrunner Rscript -e \
"remotes::install_github(repo=c('pvermees/GeoplotR','tim-band/shinylight','tim-band/GeoplotRgui'),force=TRUE,lib='~/R')"
sudo systemctl restart geoplotr
