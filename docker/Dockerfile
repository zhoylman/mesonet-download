# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny-verse

# copy the app to the image
RUN mkdir /root/apps

COPY app-deployment /root/apps

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/apps')"]
