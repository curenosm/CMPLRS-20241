# Compiladores

## Comandos utiles

NOTA: tienes que estar dentro de cualquier practica para ejecutar las pruebas, por ejemplo:

```bash
cd practica1/
```

abrir contenedor de haskell en bash, desde linux:
```bash
docker run -it --rm -v $(pwd):/app -w /app haskell:9 /bin/bash
```

o bien, desde windows (powershell)
```powershell
docker run -it --rm -v ${pwd}:/app -w /app haskell:9 /bin/bash
```

una vez dentro del contenedor así se pueden ejecutar las pruebas
```bash
cabal update
cabal build
cabal run runtests
```

o en una sola linea
```bash
cabal update && cabal build && cabal run runtests
```

en un solo comando desde el directorio de la práctica, por ejemplo:
```bash
docker run -it --rm -v $(pwd):/app -w /app haskell:9 /bin/bash -c "cabal update && cabal build && cabal run runtests"
```
