FROM alpine:3.15

RUN apk add build-base xfce4-dev-tools R R-dev ttf-liberation

WORKDIR /app

RUN Rscript --vanilla -e \
    "install.packages(c('remotes','later','jsonlite','httpuv'), \
    repos='https://cloud.r-project.org')"
RUN Rscript --vanilla -e \
    "remotes::install_github('tim-band/GeoplotR@beta')"
RUN Rscript --vanilla -e \
    "remotes::install_github('tim-band/GeoplotRgui')"
RUN Rscript --vanilla -e \
    "remotes::install_github('tim-band/shinylight@v0.5')"

COPY DESCRIPTION /app/DESCRIPTION
COPY NAMESPACE /app/NAMESPACE
COPY R /app/R
COPY inst /app/inst
COPY build/start-gui.R /app/build/start-gui.R

CMD ["Rscript", "--vanilla", "build/start-gui.R", "0.0.0.0:80"]
