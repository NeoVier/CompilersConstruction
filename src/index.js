const Elm = require('./elm.js').Elm;
const fs = require('fs');

const main = Elm.Main.init();

main.ports.requestFile.subscribe((filename) => {
    fs.readFile(filename, 'utf8', (err, fileContents) => {
        if (err) {
            main.ports.getFile.send({ error: err })
            return
        }

        main.ports.getFile.send({ fileContents })
    });
});
