# Bitácora de desarrollo – Compilador en Haskell  
**Basado en _Theories of Programming Languages_ de John C. Reynolds**

---

## Etapa inicial

Ya tenía instalado GHC, por lo que comencé directamente con la construcción del proyecto. Mi primer paso fue dividir el sistema en módulos bien definidos, lo cual me permitiría avanzar ordenadamente y escalar el lenguaje progresivamente.

---

## División en archivos

Separé el código en cuatro partes principales, de acuerdo con los tipos de expresiones y estructuras básicas:

- **Expresiones booleanas (`BoolExp`)**
- **Expresiones enteras (`IntExp`)**
- **Comandos (`Comm`)**
- **Estado (`State`)**

Esta estructura modular permite trabajar con claridad sobre cada componente por separado, y facilita la extensión del sistema más adelante.

---

## Diseño de gramáticas e intérpretes

Me basé en las gramáticas presentadas en el capítulo 2 del libro para definir cada una de las estructuras:

- **Expresiones enteras**: Incluyen constantes, variables, negaciones y operaciones binarias como suma, resta, multiplicación, división y resto. Las expresiones están representadas como tipos algebraicos y se interpretan mediante una función que evalúa su resultado dado un estado.

- **Expresiones booleanas**: Incorporan constantes lógicas (`true`, `false`), negación, operadores lógicos (conjunción, disyunción, implicación, equivalencia) y comparaciones entre expresiones enteras. Implementé un intérprete que evalúa una expresión booleana en un estado.

- **Comandos**: Incluyen las construcciones básicas de un lenguaje imperativo: `skip`, asignaciones, secuencias, condicionales, definición de variables locales (`newvar`) y bucles `while`. Por el momento decidí no implementar `fail`, entrada/salida ni excepciones para poder establecer primero una semántica sólida.

- **Estado**: Implementé una representación funcional del estado como una función que asigna a cada variable su valor actual. También construí una función auxiliar para actualizar ese estado de forma inmutable.

- **Decision de Diseño**: Modifiqué las firmas y definiciones de todos los intérpretes para que el State se pase como último parámetro. Esta convención es más idiomática en Haskell, facilita la currificación y permite una composición de funciones más natural. Afectó a los módulos IntExp, BoolExp y Comm.

---

## Implementación del intérprete

Para cada estructura sintáctica definí su correspondiente función de interpretación. Estas funciones aplican la semántica del lenguaje sobre una estructura y un estado, devolviendo el valor o estado resultante.

---

## Consideraciones sobre el bucle `while`

Al principio intenté representarlo en términos del punto fijo, como sugiere el libro, pero encontré que en Haskell era mucho más natural y claro usar una definición recursiva. Esta solución es más directa, fácil de implementar y mantiene la misma semántica funcional.

Reemplacé la definición recursiva directa del comando while en interpComm por una función auxiliar local evalWhile, de modo que el intérprete de comandos queda más claramente guiado por la sintaxis. Esta reescritura mantiene el estilo funcional, pero facilita la comprensión del flujo de ejecución de bucles y prepara el terreno para futuras extensiones (como seguimiento de pasos, conteo de iteraciones o límites de ejecución).

---

## Entrada y salida (read / print)

Decidí extender el lenguaje con mecanismos simples de entrada y salida. La idea es seguir manteniendo una semántica dirigida por la sintaxis, así que implementé estas operaciones sin cambiar de paradigma.

Para esto, modifiqué la firma de interpComm para que, además de recibir un estado, también tome una entrada (lista de enteros) y devuelva como resultado un Maybe con el nuevo estado, la salida generada (como lista de strings) y la entrada restante. Esto me permite modelar tanto lectura como escritura de forma inmutable y funcional.

Agregué dos nuevos comandos:

Read Var: consume un valor de la entrada y lo asigna a una variable. Si no hay valores disponibles, la ejecución falla con Nothing.

Print IntExp: evalúa una expresión y agrega su resultado como string a la lista de salida.

Para mantener el estilo claro y local, extendí el patrón recursivo que ya venía usando, haciendo que la semántica de cada comando se exprese directamente por su forma sintáctica. Las salidas se concatenan progresivamente (como listas de strings) y la entrada se va “consumiendo” comando a comando.

Con estos cambios, el lenguaje gana capacidad expresiva sin perder simplicidad. Además, deja abierta la puerta a incorporar más adelante una semántica de continuaciones o fallos más refinados, si hiciera falta.

---

## Implementación de la función `run`

Agregué la función run para facilitar la ejecución de un comando desde IO, 
imprimiendo la salida producida o un mensaje de error si la ejecución falla.
Esto debido al problema de imprimir estados, por lo que fue necesatio modificarlo con el run.

---

## Ejemplos y tests incluidos

Para probar el intérprete, implementé dos programas:

- progOK: Cuenta regresiva desde un número leído por consola hasta 1, luego imprime 999.
- progFail: Programa que falla porque intenta leer sin entrada disponible.

Se definieron dos tests para estos programas:

testok = run progOK initialState [3] ejecuta el programa con entrada [3].
testfail = run progFail initialState [] ejecuta el programa sin entrada para provocar fallo.

Finalmente, el main ejecuta ambos tests en orden, mostrando mensajes para diferenciar cada ejecución.

---

## Bitácora técnica resumida

- GHC instalado y configurado
- Módulos independientes: BoolExp, IntExp, Comm, State
- Gramáticas basadas en capítulos 1 y 2 del libro
- Intérpretes implementados para cada módulo
- Estado pasado como último parámetro (convención Haskell)
- Inicio sin fail ni E/S para base sólida
- while implementado recursivamente
- Extensión con comandos read y print
- interpComm retorna Maybe (State, Output, Input)
- Función run para ejecución e impresión desde IO
- Tests: programa correcto y programa con fallo
- main para ejecutar tests con mensajes

