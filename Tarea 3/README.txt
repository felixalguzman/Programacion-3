
Basado en las reglas de gramatica de Tiger descritas en el libro, agregando directivas de precedencia a algunas de las keywords del lenguaje para eliminar los conflictos de shift/reduce que se presentaban, una de estas directivas es:

%nonassoc ELSE

Le decimos al ML-yacc que este terminal se une con menor cercanía que los operadores booleanos y aritmeticos, de manera que la expresion:

IF exp THEN exp ELSE exp

se pueda parsear de manera no ambigua. El mismo uso le fue dado a las otras directivas de precedencia.