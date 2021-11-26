## Descripción del problema

### Motivación

Todo delito trae consigo una serie de inconvenientes o sufrimiento para una o más personas. Por esto, y más, siempre ha sido de interés para el humano el estudio de diversos delitos. El ideal sería poder predecirlos para evitar estos inconvenientes. Lamentablemente esto no es posible, puesto a la gran aleatoriedad de los actos de las personas, pero creemos que tal vez podemos aportar al detectar algún patrón espacial. 

Es por esto que buscamos analizar las densidades urbanas de la población en comunas de la región metropolitana y la cantidad de delitos en cada una de ellas para así responder inicialmente a preguntas como:
¿Se correlaciona la densidad urbana con los delitos? o ¿Existirán patrones identificables de los tipos de delitos según la densidad urbana y la comuna?

Además, dado que tenemos información espacial ¿Podremos predecir futuros lugares conflictivos según la evidencia de evolución temporal? o ¿Existirán relaciones en los resultados alcanzados entre las unidades vecinales (comunas)?

### Metodologías a usar.

Como todo análisis de datos, iniciaremos nuestra investigación con un análisis de visualización para observar posibles patrones, asociaciones y correlaciones. Puesto que tenemos datos de área y georreferenciados, realizaremos variogramas, correlogramas y algunos test de asociación espacial.

Una vez analizadas las asociaciones, construiremos modelos acordes a nuestros datos, como de series de tiempo, cluster y SAR o CAR (según lo indiquen nuestros datos) y llevaremos a cabo ciertas predicciones espaciales (kriging).