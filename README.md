# Lab 1 Paradigmas de Programación Famaf

## Preguntas

### ¿Por qué están separadas las funcionalidades en los módulos indicados?

Pensamos que las razones/beneficios para separar las funcionalidades en distintos módulos son las siguientes:

- **El código es menos complejo**: cada módulo tiene una responsabilidad clara y no se mezclan las cosas. Por ejemplo, el módulo `Dibujo` se encarga de la sintaxis del lenguaje, mientras que el módulo `Interp` se encarga de la semántica/interpretación del lenguaje y luego la implementación de cada figura básica (utilizando Gloss) se encuentra en el módulo `Dibujos/<dibujo_en_cuestión>`.
- **El código es más fácil de entender y testear**: Se facilita la comprensión del código y la posibilidad de testearlo de forma aislada. Si no hubieramos separado la implementación de la sintaxis (Dibujo.hs) de la semántica (Interp.hs) no hubiéramos podido testear la sintaxis de manera aislada, y en caso de error al realizar los dibujos habría costado más identificar si el error estaba en la sintaxis (Dibujo.hs) o en la semántica (Interp.hs).
- **Facilita el mantenimiento**: Además de ser más fácil para corregir errores, se puede modificar la implementación de un módulo sin tener que modificar el resto del código. Cada módulo es independiente del resto y comparte una interfaz bien definida la cual no deberiamos modificar, pero si se puede modificar la implementación interna del módulo. Por ejemplo, pudimos hacer el punto estrella de permitir el manejo de animaciones solamente modificando el modulo `Interp` (y modificando levemente el modulo `Dibujos/<dibujo_en_cuestión>` para aprovechar esta nueva funcionalidad) y no tuvimos que modificar el resto de los módulos de código.
- **Facilita la reutilización de código**: Al modularizar la implementación de la función `foldDib`, podemos importar el módulo `Dibujo` y utilizar la función `foldDib` en `Interp` y `Pred` sin tener que copiar y pegar el código en cada archivo.

#### Explicar detalladamente la responsabilidad de cada módulo

- **Internals/Dibujo**: Definir el tipo de datos `Dibujo` y las funciones basicas que permiten construir dibujos.
- **Dibujo**: Terminar de implementar la sintaxis del lenguaje comenzada en `Internals/Dibujo`. Esto significa definir las funciones más complejas que permiten construir, modificar y trabajar con los dibujos.
- **Pred**: Extender la sintaxis del lenguaje para poder trabajar con predicados sobre los dibujos.
- **Interp**: Implementar la semántica del lenguaje. Esto significa tomar un dibujo y devolver su interpretación, en este caso mediante el uso de la biblioteca de gráficos Gloss (Se interpreta como un grafico bi-dimensional usando vectores).
- **FloatingPic**: Proporcionar funciones auxiliares que permiten trabajar con los dibujos y los vectores utilizados para representarlos.
- **Grilla**: TODO
- **Dibujos/Escher**: Implementar la semántica/interpretación de las figuras básicas usadas en el dibujo "Escher", definición de combinadores y generación del dibujo de Escher.
- **Main**: Solicitar al usuario que ingrese el nombre de un dibujo y mostrar el resultado de interpretar el dibujo ingresado (si es que existe).
- **Dibujos/\<Dibujo fulanito\>**: Implementar la semántica/interpretación de las figuras básicas usadas en el dibujo fulanito y generación del código correspondiente a realizar el dibujo fulanito.

### ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez es un parámetro del tipo?

Porque existe una cantidad infinita de figuras básicas, y cada una seguramente tendrá una interpretación diferente. Por ejemplo, una figura básica puede ser un círculo, un hexagono, un triángulo, una letra F, una figura vacia, etc.

Como a priori no sabemos que figuras básicas tendremos, por eso se utiliza un tipo abstracto `<Dibujo>` que es parámetrico en `<Fig>`, porque cada programador define su propia figura básica y la implementación de esa figura básica.

### ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?

La única ventaja que nuestro grupo encontró en utilizar una función de `fold` sobre hacer pattern-matching directo es la posibilidad de generar un código más legible y corto, incrementando la abstracción. Por ejemplo, en el módulo `Pred` implementamos la función `anyDib` de la siguiente manera:

```haskell
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p = foldDib p id id id (\_ _ x y -> x || y) (\_ _ x y -> x || y) (||)
```

Utilizando `foldDib` podemos escribir la función `anyDib` en una sola línea, mientras que si hubiéramos utilizado pattern-matching directo, el código hubiera sido más largo y menos legible:

```haskell
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p (Figura a) = p a
anyDib p (Rotar d) = anyDib p d
anyDib p (Espejar d) = anyDib p d
anyDib p (Rot45 d) = anyDib p d
anyDib p (Apilar _ _ d1 d2) = anyDib p d1 || anyDib p d2
anyDib p (Juntar _ _ d1 d2) = anyDib p d1 || anyDib p d2
anyDib p (Encimar d1 d2) = anyDib p d1 || anyDib p d2
```

Si no hubiesemos implementado `foldDib` no podríamos habernos abstraido de los constructores y hubiese sido necesario utilizar pattern-matching. Aquellas funciones donde se lo utiliza `foldDib` hubiesen tenido (cómo mínimo) 7 lineas de código, lo cual hubiese hecho el código menos legible y más complejo de entender.

Sin embargo, existen casos en los que utilizar una función de `fold` sobre hacer pattern-matching directo no es conveniente porque el código se hace más complejo y menos legible (Por ejemplo, si queremos detectar si dos figuras que fueron apiladas están ambas inmediatamente rotadas 45 grados). Además, para programadores novatos que no están familiarizados con el concepto de `fold` puede ser más difícil de entender y preferirían una implementación usando pattern-matching. Cada enfoque tiene sus pros y sus contras y depende de la situación y del programador elegir una estrategia u otra.

## Decisiones de diseño y detalles de implementación

- **División en Dibujo y internals/Dibujo**: Se decidió dividir la implementación de la sintaxis en 2 modulos, `Internals/Dibujo` y `Dibujo`. Nos basamos en la siguiente respuesta de stack overflow: <https://stackoverflow.com/a/14379426> y decidimos poner los miembros privados del modulo en Internals.Dibujo para poder testear comodamente sin perder la encapsulacion.
- Como hicimos el punto estrella de las proporciones
- Los otros puntos estrella que hicimos
- makefile
COMPLETAR

## Nuestra experiencia con el laboratorio

COMPLETAR