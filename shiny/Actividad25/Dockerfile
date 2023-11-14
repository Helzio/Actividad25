# Utiliza una imagen base oficial de R con Shiny preinstalado
FROM rocker/shiny:latest

# Instala los paquetes de R que necesitas
RUN R -e "install.packages(c('ggplot2', 'dplyr', 'shiny'), repos='http://cran.rstudio.com/')"

# Copia los archivos de tu aplicación en la imagen
COPY . /srv/shiny-server/myapp

# Expone el puerto en el que se ejecuta Shiny
EXPOSE 3838

# Ejecuta la aplicación
CMD ["/usr/bin/shiny-server"]
