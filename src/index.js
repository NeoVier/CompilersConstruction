/* UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   June 2021
*/

const Elm = require('./elm.js').Elm;
const fs = require('fs');

const cliArgs = process.argv.slice(2);

// This is our main entry point. We want to keep as much logic as possible on
// Elm, and as little logic as possible on JS, so we don't care what we get as
// arguments on the JS side, and let Elm deal with that
const main = Elm.Main.init({ flags: cliArgs });

main.ports.printError.subscribe((error) => {
    console.error(error)
});

main.ports.requestFile.subscribe((filename) => {
    fs.readFile(filename, 'utf8', (err, fileContents) => {
        if (err) {
            main.ports.getFile.send({ error: err });
            return;
        }

        main.ports.getFile.send({ fileContents });
    });
});
