# Speed Dating Analysis Shiny App

## Descripción
Este repositorio contiene una aplicación Shiny para analizar un conjunto de datos de citas rápidas. La aplicación permite a los usuarios explorar visualizaciones interactivas y obtener información sobre preferencias y patrones de citas.

## Características
- Visualizaciones interactivas de atributos de citas.
- Análisis del porcentaje de coincidencias (matches).
- Comparación de auto-percepción vs. percepción de los demás.
- Influencia de intereses compartidos en la decisión de segunda cita.

## Requisitos Previos
Para ejecutar esta aplicación, necesitarás Docker y acceso a una terminal de línea de comandos.

## Cómo Ejecutar la Aplicación
1. Clona este repositorio en tu máquina local usando:

```git clone https://github.com/Helzio/Actividad25.git```

2. Navega al directorio del proyecto:

```cd tu_repositorio```

3. Construye la imagen de Docker con:

```docker build -t mi-app-shiny .```

4. Una vez construida la imagen, ejecuta la aplicación con:

```docker run -p 3838:3838 mi-app-shiny```

5. Abre un navegador y ve a `http://localhost:3838/myapp/` para acceder a la aplicación.

## Estructura del Repositorio
- `app.R`: El script principal de la aplicación Shiny.
- `data/`: Directorio que contiene el conjunto de datos utilizado por la aplicación.
- `Dockerfile`: Archivo necesario para construir la imagen de Docker de la aplicación.
- `README.md`: Este archivo, que proporciona una descripción general y cómo usar el repositorio.

## Contribuir
Si deseas contribuir a este proyecto, por favor realiza un "fork" del repositorio y envía tus "pull requests".

