# Análise léxica da linguagem CC-2021-1

UFSC - CTC - INE5426 - Construção de compiladores - Julho 2021

Professor: Alvaro Junio Pereira Franco

Aluno: Henrique da Cunha Buss

## Tecnologia

O trabalho foi feito utilizando a linguagem [Elm](https://elm-lang.org/), uma
linguagem puramente funcional. Por conta disso, alguns algoritmos e estruturas
de dados podem ser diferentes dos tradicionais, mas o resultado final deve ser
equivalente.

## Estruturação

A estrutura do programa até agora foi dividida em duas partes: Sintaxe e Parser,
que serão discutidos em seguida.

### Sintaxe

Módulos (arquivos) dentro da pasta `src/Syntax` definem o formato das estruturas
da linguagem (um programa pode ser apenas um *Statement*, ou pode ser uma lista
de funções. Um *Statement* pode ser uma declaração de variável, uma atribuição,
um comando *print*, etc.).

Aqui, temos 4 módulos, que foram divididos de forma que possamos usar pequenos
blocos (expressões), para criar blocos maiores (*statements*), e finalmente os
últimos blocos (programa e tabela de símbolos). Com um sistema de tipos
semelhante ao de [Haskell](https://www.haskell.org/), Elm nos possibilita fazer
uma modelagem de dados extremamente precisa, que imita fielmente as definições
da linguagem. De modo geral, as estruturas sintáticas se organizam em forma de
árvore, muitas vezes recursiva.

O módulo de expressões (`Syntax.Expression`) é o mais baixo-nível, e é
responsável por representar expressões numéricas, booleanas e com strings. Nele
o uso de árvores recursivas é bastante aparente. Por exemplo, um termo é
definido recursivamente. Seu caso base é um único termo (uma expressão unária,
ou seja, um único fator, com ou sem sinal), e seu caso recursivo é a definição
de múltiplos termos, que é uma expressão unária, um operador (`*`, `/` ou `%`),
e outro termo (recursivamente).

O módulo de *statements* (`Syntax.Statement`) é construído com base nas
expressões. Nele, definimos quais tipos de variáveis podem existir (`int`,
`float` e `string`), assim como as estruturas de declaração de variável,
atribuição de variável, laços de repetição *for*, blocos *if*, etc.

O módulo de programa (`Syntax.Program`) é a junção de tudo, que representa o
programa como um todo, e usa as definições do módulo de *statements* para tal.

O módulo `Syntax.Symbol` é responsável por, a partir de um `Program`, gerar a
tabela de símbolos. Ou seja, primeiro fazemos todo o *parsing* do código fonte,
estruturamos os dados no formato de um `Program`, e apenas depois analisamos o
`Program` resultante para construir a tabela de símbolos. Isso é,
principalmente, devido ao fato de Elm ser uma linguagem puramente funcional, sem
*side-effects*, ou seja, se fossemos construir a tabela de símbolos ao mesmo
tempo que lemos o código fonte, teríamos que passar como argumento para todas as
funções uma instância da tabela de símbolos. Portanto, gerar a tabela de
símbolos com o `Program` já identificado é mais fácil e gera um código (do
compilador) mais legível. Como um `Program` é uma árvore, construir a tabela de
símbolos significa percorrer a árvore, coletando informações importantes. Como
estamos percorrendo uma árvore, também temos noção de escopo, o que quer dizer
que naturalmente temos variáveis que podem ser específicas para uma função, ou
um bloco de código. A tabela de símbolos tem 3 tipos de entradas:

- Declaração de variável. Uma declaração de variável é algo como `int x[1];`, e
temos todos os dados dessa declaração disponível, mas usamos o nome da variável
(`x`), o tipo (`int`), e as dimensões (`[1]`);
- Declaração de função. Um exemplo é `def funcA(int x) {...}`. Podemos guardar
o nome da função (`funcA`), o tipo (`function`), e os tipos de argumentos
(`int`);
- Parâmetro de função. Na definição de função acima, seria o `int x`. Podemos
salvar seu nome (`x`) e seu tipo (`int`). A gramática não permite que argumentos
tenham dimensões, como na declaração de variáveis.

### Parsing

Os arquivos da pasta `src/Parser` são responsáveis em transformar o código fonte
em *statements*, expressões e programas, e eles definem os *tokens* da
linguagem. Elm possui uma excelent biblioteca de *parsing*,
[elm/parser](https://package.elm-lang.org/packages/elm/parser/latest/), que é
baseada em gramáticas livres de contexto. Em seguida, falaremos dos *tokens*
relacionados a expressões.

- `variableName`. Representa o nome de uma variável. É qualquer sequência de
caracteres que comece com uma letra, e é formado por letras ou `_`. São
excluidas as palavras reservadas da linguagem, como `def`, `print`, etc.
- `variableAccessor`. É o token utilizado para acessar uma variável, podendo ou
não ter acesso a índices (por exemplo, `x[3][2][1]`).
- `factor`. É o token mais básico, e representa um número (sem sinal, podendo
ser `int` ou `float`), uma `string`, `null`, `variableAccessor`, ou uma
expressão entre parênteses.
- `unaryExpression`. Um `factor` que pode ser precedido por `+` ou `-`.
- `termOperator`. Representa uma operação de `*`, `/` ou `%`.
- `term`. Definido recursivamente, pode ser simplesmente uma `unaryExpression`,
ou uma `unaryExpression` seguida de um `termOperator`, seguida por um `term`.
- `numericalOperator`. Representa uma operação de `+` ou `-`.
- `numericalExpression`. Representa uma expressão numérica. Similarmente ao
`term`, é definida recursivamente, e pode ser um simples `term`, ou um `term`
seguido de um `numericalOperator` seguido de outra `numericalExpression`.
- `comparator`. Representa os comparadores booleanos, como `<=`, `>`, `==`, etc.
- `expression`. Representa uma expressão inteira, é definido como uma única
`numericalExpression`, ou duas `numericalExpressions` com um `comparator` entre
elas.

Também temos um módulo específico para o *parsing* de *statements*,
`Parser.Statement`. Em seguida, são apresentados os *tokens* relacionados a
*statements*, que usam os *tokens* de expressões descritos acima.

- `variableType`. Faz *parsing* de um tipo de variável, isto é, `int`, `float`
ou `string`.
- `declaration`. Representa a declaração de uma variável. Começa com um
`variableType`, seguida por uma `variableName`, e uma sequência (que pode ser
vazia) de índices (como em `int x[1][2][3]`)
- `functionCall`. Representa a chamada de uma função. É definida como uma
`variableName`, seguida por uma lista de parâmetros entre parênteses, separados
por vírgula.
- `allocation`. Responsável por ler a alocação de memória. Começa com a
*keyword* `new`, seguida por um `variableType`, e um ou mais índices, como em
`new x[1][2][3]`.
- `attribution`. Representa a atribuição de um valor a uma variável. Começa com
um `variableAccessor`, seguido por `=`, e `functionCall` ou `expression` ou
`allocation`.
- `print`. Simplesmente a *keyword* `print` seguida por uma `expression`.
- `read`. Simplesmente a *keyword* `read` seguida por um `variableAccessor`.
- `return`. Simplesmente a *keyword* `return`.
- `if`. Representa um *if statement*. Começa com a *keyword* `if`, seguida
por uma `expression` entre parentêses, e um `statement`. Opcionalmente, pode ter
a *keyword* `else` seguida por um `statement`.
- `for`. Representa um laço *for*. Começa com a *keyword* `for`, seguida
por uma `attribution`, uma `expression` e outra `attribution`, separados com `;`
e entre parênteses, seguidos de um `statement`.
- `statementList`. Simplesmente uma lista de `statement`s entre `{` e `}`.
- `break`. Simplesmente a *keyword* `break;`.
- `;`. Simplesmente o *token* `;`.
- `statement`. Qualquer um dos descritos acima.
- `functionParameter`. Um `variableType` seguido por um `variableName`.
- `functionDeclaration`. A *keyword* `def`, seguida por um `variableName`, uma
lista de `functionParameter`s separados por vírgula e entre parênteses, e um
`statementList`.

Abaixo são apresentadas as expressões regulares que definem cada *token*:

```regex
alpha -> A | B | ... | Z | a | b | ... | z
digit -> 0 | 1 | ... | 9
null -> null
variableName -> alpha(alpha | _)*
variableAccessor -> variableName([numericalExpresion])*
factor -> digit+(.digit+)? | "(alpha*)" | null | variableAccessor
unaryExpression -> (+ | -)?factor
termOperator -> * | / | %
term -> unaryExpression | unaryExpression termOperator term
numericalOperator -> + | -
numericalExpression -> term | term numericalOperator numericalExpression
comparator -> <= | >= | < | > | == | !=
expression -> numericalExpression | numericalExpression comparator numericalExpression
variableType -> int | float | string
declaration -> variableType variableName([digit])*
functionCall -> variableName\((variableName (, variableName)*(,)?)?\)
new -> new
allocation -> new variableType([numericalExpression])*
attribution -> variableAccessor = (functionCall | expression | allocation)
print -> print expression
read -> read variableAccessor
return -> return
if -> if \(expression\) statement (else statement)?
for -> for \(attribution; expression; attribution\) statement
statementList -> { statement* }
break -> break;
semicolon -> ;
statement -> declaration; | attribution; | print; | read; | return; | if | for | statementList | break | semicolon
functionParameter -> variableType variableName
functionDeclaration -> def variableName\((functionParameter(, functionParameter)*(,)?)?\)
```

Os diagramas de transição de cada um desses *tokens* estão em anexo.
