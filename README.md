# Lenguaje Imperativo Simple en Haskell

Este proyecto es una implementación minimalista de un lenguaje imperativo, basada en el libro _Theories of Programming Languages_ de John C. Reynolds. Toda la semántica está dirigida por la sintaxis, usando Haskell y su estilo funcional puro.

---

## 🚀 ¿Qué incluye?

- Expresiones enteras (`+`, `-`, `*`, `/`, `rem`, variables, negación)
- Expresiones booleanas (`true`, `false`, negación, comparaciones, operadores lógicos)
- Comandos: `skip`, asignaciones, secuencias (`;`), condicionales (`if`), bucles `while`, definición de variables locales (`newvar`)
- Entrada y salida (`read`, `print`)
- Manejo de fallos con `fail`
- Un intérprete funcional: sin efectos, sin trampas, con control total

---

## 🚀 Cómo correr el programa

Desde la terminal, en el directorio del proyecto:

```bash
ghci Main.hs
``` 

Una vez dentro de GHCi, puedes:
- Ejecutar el programa principal con:
```bash
main
``` 
el cual correra los dos test:
- testok: recive un int de input e imprime una secuencia hasta el 0 terminando con 999
- testfail: falla al intentar leer un input vacio

o se pueden ejecutar los test por separado llamandolos con su nombre