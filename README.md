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
- **FloatingPic**: Proporcionar funciones auxiliares que permiten trabajar con los dibujos y con los vectores utilizados para representarlos.
- **Grilla**: Implementa la función `grilla` utilizada en los dibujos Grilla y Feo. La función `grilla` recibe una lista de listas de dibujos, que representan las filas de una grilla, y construye la grilla completa de esos dibujos.
- **Dibujos/Escher**: Implementar la semántica/interpretación de las figuras básicas usadas en el dibujo "Escher", definición de combinadores y generación del dibujo de Escher.
- **Main**: Solicitar al usuario que ingrese el nombre de un dibujo y mostrar el resultado de interpretar el dibujo ingresado (si es que existe). También permite solicitar una lista de dibujos y seleccionar uno de ellos para mostrarlo.
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
- **Las proporciones usadas en Juntar y Apilar están invertidas**: No sabemos muy bien porque pero al implementar estas funciones en el interp se nos invertieron las proporciones. Por ejemplo, si queremos juntar dos dibujos y que el primero sea el doble que el segundo debemos escribir `juntar 1 2 d1 d2`. Como todos los dibujos que hicimos estaban hechos teniendo en cuenta las proporciones invertidas, decidimos dejarlo así, pero puede llegar a ser confuso para quien no esta acostumbrado.
- **Makefile**: Se implementó un makefile para facilitar la ejecución de ciertos dibujos. La elección de que dibujos se agregaron al archivo makefile fue arbitraria y dependia de las necesidades de cada integrante del grupo mientras se realizaba la implementación de los distintos dibujos. Para poder ejecutar el dibujo Escher se debe ejecutar el comando `make esch`.
- **Puntos estrella realizados:**
  - **Operador para modificar proporción de las figuras:** Se implementó el operador `Modificar Float Float (Dibujo a)` para modificar la proporción de las figuras. Este operador recibe 2 floats que representan la proporción horizontal y vertical de la figura. Las proporciones se amplian de manera tal que la figura quede centrada respecto a su posición anterior. Por ejemplo, si tenemos una figura de 1x1 y creamos una copia ampliada a 2x2, la figura de 1x1 quedará centrada y dentro de la figura 2x2 (Solo aplica a figuras que son medianamente simétricas respecto a su centro). Se puede ver el uso de este operador en el dibujo `PruebaModificar`.
  - **Al listar los dibujos te permite seleccionar cual imprimir**: Si uno pasa `--lista` como argumento al programa, se le muestra una lista de los dibujos disponibles y se le pide que ingrese cual quiere ejecutar. Esto se hizo para facilitar la ejecución de los dibujos.
  - **Rama con animaciones**: Se implementó una nueva rama en el repositorio llamada `animaciones` que contiene una versión del programa que permite visualizar los dibujos animados. La animación solo se aplica a los dibujos que se construyen de manera recursiva y la animación se basa en como se realiza la construcción del dibujo en cada paso. Evitar revisar de manera exhaustiva esta rama ya que fue creada como copia de la rama `master` cuando la rama `master` aún no estaba finalizada.
  - **Dibujos extra**:
    - **Escher2**: Similar a Escher pero con una "grilla" de 4x4 en vez de 3x3. La única diferencia con el Escher original es en la función noneto, que ahora recibe 16 dibujos como input en vez de 9.
    - **EscherEfe**: Similar a Escher pero utilizando la figura `Efe` en vez de `Triangulo`. La idea vino cuando estaba corrigiendo un error en la función noneto del dibujo Escher (debido a que las proporciones usadas en Juntar y Apilar están invertidas) y para depurar el dibujo utilicé la figura Efe en vez de Triangulo, ya que me era más cómodo para encontrar los errores. Como me gustó el resultado, decidí implementar el dibujo EscherEfe.
    - **Aureo**: Dibujo de la espiral aurea. Se construye de manera recursiva, juntando una figura aurea junto con la rotación de la llamada recursiva a la función. Se utiliza la razón aurea en las proporciones al juntar las 2 figuras para que queden cuadrados y no rectangulares. La parte más dificil de implementar fue el cuarto de circunferencia, ya que se complicaba el tema de usar la función `arc` de Gloss porque no sabía como hacer para que el arco se dibujara desde el punto de inicio hasta el punto de fin que yo necesitaba. La formula final que se utiliza para graficar el cuarto de circunferencia fue una mezcla entre respuestas de ChatGPT y un poco de análisis matemático 1.
    - **PruebaModificar**: Dibujo que prueba el operador `Modificar`. Para graficar se utiliza una función recursiva que dibuja una figura y luego se vuelve a llamar a la misma función pero con las proporciones de la figura levente modificadas (Se multiplican por 0.9).
    - **Sierpinski**: Imagen fractal que crea el triangulo de Sierpinski. Se construye de manera recursiva, el caso base imprime 3 triangulos, 2 en la mitad de abajo y uno en la mitad de arriba, el caso recursivo se llama 3 veces, una para cada triangulo de la mitad de abajo y una para el triangulo de la mitad de arriba. Se utilizaron figuras `Vacias` para que el triangulo de arriba quede correctamente centrado. La idea de este dibujo vino de la siguiente página: <https://jaalonso.github.io/cursos/i1m/temas/tema-26.html>, aunque no se utilizo como referencia para el código ya que en esa página utilizan Gloss sin tanta abstracción como hacemos nosotros.

## Nuestra experiencia en el proyecto

En general, nos pareció un proyecto muy interesante y divertido. Nos gustó mucho la idea de poder crear dibujos de manera recursiva y poder visualizarlos en una interfaz gráfica como la que provee Gloss.

Nos costó un poco entender el paradigma de programación funcional, pero una vez que lo entendimos nos gustó mucho la forma elegante en la cual se pueden resolver los problemas en este paradigma. También estuvo bueno aprender sobre la creación de lenguajes específicos de dominio y cómo se pueden usar para resolver problemas concretos, en este caso la creación de dibujos. Otra cosa que también nos llevamos es un refuerzo de los temas vistos en las clases teoricas de Paradigmas de Programación, como la diferencia entre sintaxis y semántica. También nos sirvió para reforzar el uso de Git, una herramienta de suma importancia para cualquier programador.

Para comunicarnos estuvimos principalmente utilizando Telegram y mediante ese medio pudimos separar las tareas de manera bastante equitativa y comunicarnos bien para poder avanzar en el proyecto y terminarlo a tiempo.
