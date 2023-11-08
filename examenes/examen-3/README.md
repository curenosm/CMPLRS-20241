# Instrucciónes de compilación de los documentos con extensión `.tex`

1. Tener instalado Docker.
2. Dirigirse a la carpeta donde esté el archivo.
3. Ejecutar los siguientes comandos.

```powershell
docker build -t "custom-latex" .
docker run --rm -v ${pwd}:/data custom-latex
```

Ya se habrá generado el archivo `main.pdf`

## Instrucciones para agregar dependencias

1. Si se trata de una dependencia del sistema operativo, agregarla en el Dockerfile
2. Si se trata de una dependencia de latex agregar en el archivo `resources/entrypoint.sh`


## Instrucciones para agregar referencias

1. Abrir el archivo `resources/main.bib` y agregar la referencia en formato bibtex.
