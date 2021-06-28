const Elm = require('./elm.js').Elm;
const fs = require('fs');

const cliArgs = process.argv.slice(2);

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
