const Elm = require('./elm.js').Elm;

const main = Elm.Main.init();

console.log(main.ports)

main.ports.printSomething.subscribe((what) => {
    console.log(what)
})

setInterval(() => {
    main.ports.getSomething.send('who')
}, 300)

