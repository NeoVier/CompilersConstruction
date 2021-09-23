# Compilador para linguagem CC-2021-1

UFSC - CTC - INE5426 - Construção de Compiladores

Professor: Alvaro Junio Pereira Franco

Aluno: Henrique da Cunha Buss

## Rodando o projeto

O projeto é construído com a linguagem [Elm](https://elm-lang.org), que é
compilada para JavaScript. Portanto, precisamos do
node.js para instalar dependências e rodar o programa. Para verificar se já o
possui instalado, basta rodar `node -v`. Se ainda não tiver instalado, o
programa pode ser obtido através do [site oficial](https://nodejs.org).

### Instalando dependências

Este projeto possui dependências (o compilador de Elm), que podem ser obtidos
através do gerenciador de pacotes do node, o [npm](https://www.npmjs.com), que
é instalado junto com o node. Para instalar as dependências, basta rodar
`make setup`. Este comando invocará o comando `npm install`, que baixa os
pacotes necessários do banco de dados do npm.

### Executando

Além do comando `make setup`, o Makefile também disponibiliza o comando `run`,
que compila o projeto e, em seguida, o executa. Como precisamos de um arquivo de
entrada e um de saída, basta rodar o seguinte em um terminal:

```bash
make run input=examples/guessingGame.lcc output=output/guessingGame.int
```

Onde `examples/guessingGame.lcc` pode ser qualquer caminho para um arquivo da
linguagem CC-2021-1, a pasta `output` **já existe**, e o código intermediário
gerado será escrito no arquivo `output/guessingGame.int`.
