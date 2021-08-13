const initialProductions = {
    'PROGRAM': ['STATEMENT', 'FUNCLIST', ''],
    'FUNCLIST': ['FUNCDEF FUNCLIST', 'FUNCDEF'],
    'FUNCDEF': ['def ident(PARAMLIST) { STATELIST }'],
    'PARAMLIST': [
        'int ident, PARAMLIST',
        'float ident, PARAMLIST',
        'string ident, PARAMLIST',
        'int ident',
        'float ident',
        'string ident',
        ''
    ],
    'STATEMENT': [
        'VARDECL;',
        'ATRIBSTAT;',
        'PRINTSTAT;',
        'READSTAT;',
        'RETURNSTAT;',
        'IFSTAT',
        'FORSTAT',
        '{STATELIST}',
        'break;',
        ';'
    ],
    'VARDECL': ['int ident VARDECL\'', 'float ident VARDECL\'', 'string ident VARDECL\''],
    'VARDECL\'': ['[int_constant] VARDECL\'', ''],
    'ATRIBSTAT': ['LVALUE = EXPRESSION', 'LVALUE = ALLOCEXPRESSION', 'LVALUE = FUNCCALL'],
    'FUNCCALL': ['ident(PARAMLISTCALL)'],
    'PARAMLISTCALL': ['ident, PARAMLISTCALL', 'ident', ''],
    'PRINTSTAT': ['print EXPRESSION'],
    'READSTAT': ['read LVALUE'],
    'RETURNSTAT': ['return'],
    'IFSTAT': ['if (EXPRESSION) STATEMENT else STATEMENT', 'if (EXPRESSION) STATEMENT'],
    'FORSTAT': ['for(ATRIBSTAT; EXPRESSION; ATRIBSTAT) STATEMENT'],
    'STATELIST': ['STATEMENT', 'STATEMENT STATELIST'],
    'ALLOCEXPRESSION': [
        'new int ALLOCEXPRESSION\'',
        'new float ALLOCEXPRESSION\'',
        'new string ALLOCEXPRESSION\''
    ],
    'ALLOCEXPRESSION\'': ['[NUMEXPRESSION]', '[NUMEXPRESSION] ALLOCEXPRESSION\''],
    'EXPRESSION': [
        'NUMEXPRESSION',
        'NUMEXPRESSION < NUMEXPRESSION',
        'NUMEXPRESSION > NUMEXPRESSION',
        'NUMEXPRESSION <= NUMEXPRESSION',
        'NUMEXPRESSION >= NUMEXPRESSION',
        'NUMEXPRESSION == NUMEXPRESSION',
        'NUMEXPRESSION != NUMEXPRESSION'
    ],
    'NUMEXPRESSION': ['TERM NUMEXPRESSION\''],
    'NUMEXPRESSION\'': ['+ TERM NUMEXPRESSION\'', '- TERM NUMEXPRESSION\'', ''],
    'TERM': ['UNARYEXPR TERM\''],
    'TERM\'': ['* UNARYEXPR TERM\'', '/ UNARYEXPR TERM\'', '% UNARYEXPR TERM\'', ''],
    'UNARYEXPR': ['+ FACTOR', '- FACTOR', 'FACTOR'],
    'FACTOR': [
        'int_constant',
        'float_constant',
        'string_constant',
        'null',
        'LVALUE',
        '(NUMEXPRESSION)'
    ],
    'LVALUE': ['ident LVALUE\''],
    'LVALUE\'': ['[NUMEXPRESSION] LVALUE\'', '']
}

const factoredProductions = {
    'PROGRAM': ['STATEMENT', 'FUNCLIST', ''],
    'FUNCLIST': ['FUNCDEF FUNCLIST\''],
    'FUNCLIST\'': ['FUNCLIST', ''],
    'FUNCDEF': ['def ident(PARAMLIST) { STATELIST }'],
    'PARAMLIST': [
        'int ident PARAMLIST\'',
        'float ident PARAMLIST\'',
        'string ident PARAMLIST\'',
        ''
    ],
    'PARAMLIST\'': [', PARAMLIST', ''],
    'STATEMENT': [
        'VARDECL;',
        'ATRIBSTAT;',
        'PRINTSTAT;',
        'READSTAT;',
        'RETURNSTAT;',
        'IFSTAT',
        'FORSTAT',
        '{STATELIST}',
        'break;',
        ';'
    ],
    'VARDECL': ['int ident VARDECL\'', 'float ident VARDECL\'', 'string ident VARDECL\''],
    'VARDECL\'': ['[int_constant] VARDECL\'', ''],
    'ATRIBSTAT': ['LVALUE = ATRIBSTAT\''],
    'ATRIBSTAT\'': ['EXPRESSION', 'ALLOCEXPRESSION', 'FUNCCALL'],
    'FUNCCALL': ['ident(PARAMLISTCALL)'],
    'PARAMLISTCALL': ['ident PARAMLISTCALL\'', ''],
    'PARAMLISTCALL\'': [', PARAMLISTCALL', ''],
    'PRINTSTAT': ['print EXPRESSION'],
    'READSTAT': ['read LVALUE'],
    'RETURNSTAT': ['return'],
    'IFSTAT': ['if (EXPRESSION) STATEMENT IFSTAT\''],
    'IFSTAT\'': ['else STATEMENT', ''],
    'FORSTAT': ['for(ATRIBSTAT; EXPRESSION; ATRIBSTAT) STATEMENT'],
    'STATELIST': ['STATEMENT STATELIST\''],
    'STATELIST\'': ['STATELIST', ''],
    'ALLOCEXPRESSION': ['new ALLOCEXPRESSION\''],
    'ALLOCEXPRESSION\'': [
        'int ALLOCEXPRESSION\'\'',
        'float ALLOCEXPRESSION\'\'',
        'string ALLOCEXPRESSION\'\''
    ],
    'ALLOCEXPRESSION\'\'': ['[NUMEXPRESSION] ALLOCEXPRESSION\'\'\''],
    'ALLOCEXPRESSION\'\'\'': ['ALLOCEXPRESSION\'\'', ''],
    'EXPRESSION': ['NUMEXPRESSION EXPRESSION\'',],
    'EXPRESSION\'': ['< EXPRESSION\'\'', '> EXPRESSION\'\'', '== NUMEXPRESSION', '!= NUMEXPRESSION', ''],
    'EXPRESSION\'\'': ['NUMEXPRESSION', '= NUMEXPRESSION'],
    'NUMEXPRESSION': ['TERM NUMEXPRESSION\''],
    'NUMEXPRESSION\'': ['+ TERM NUMEXPRESSION\'', '- TERM NUMEXPRESSION\'', ''],
    'TERM': ['UNARYEXPR TERM\''],
    'TERM\'': ['* UNARYEXPR TERM\'', '/ UNARYEXPR TERM\'', '% UNARYEXPR TERM\'', ''],
    'UNARYEXPR': ['+ FACTOR', '- FACTOR', 'FACTOR'],
    'FACTOR': [
        'int_constant',
        'float_constant',
        'string_constant',
        'null',
        'LVALUE',
        '(NUMEXPRESSION)'
    ],
    'LVALUE': ['ident LVALUE\''],
    'LVALUE\'': ['[NUMEXPRESSION] LVALUE\'', '']
}

const firsts = {
    'PROGRAM': [
        'int', 'float', 'string',
        'ident',
        'print',
        'read',
        'return',
        'if',
        'for',
        '{',
        'break',
        ';',
        'def',
        ''
    ],
    'FUNCLIST': ['def'],
    'FUNCLIST\'': ['def', ''],
    'FUNCDEF': ['def'],
    'PARAMLIST': ['int', 'float', 'string', ''],
    'PARAMLIST\'': [',', ''],
    'STATEMENT': [
        'int', 'float', 'string',
        'ident',
        'print',
        'read',
        'return',
        'if',
        'for',
        '{',
        'break',
        ';'
    ],
    'VARDECL': ['int', 'float', 'string'],
    'VARDECL\'': ['[', ''],
    'ATRIBSTAT': ['ident'],
    'ATRIBSTAT\'': [
        '+', '-', 'int_constant', 'float_constant', 'string_constant', 'null', 'ident', '(',
        'new'
    ],
    'FUNCCALL': ['ident'],
    'PARAMLISTCALL': ['ident', ''],
    'PARAMLISTCALL\'': [',', ''],
    'PRINTSTAT': ['print'],
    'READSTAT': ['read'],
    'RETURNSTAT': ['return'],
    'IFSTAT': ['if'],
    'IFSTAT\'': ['else', ''],
    'FORSTAT': ['for'],
    'STATELIST': [
        'int', 'float', 'string',
        'ident',
        'print',
        'read',
        'return',
        'if',
        'for',
        '{',
        'break',
        ';'
    ],
    'STATELIST\'': [
        'int', 'float', 'string',
        'ident',
        'print',
        'read',
        'return',
        'if',
        'for',
        '{',
        'break',
        ';',
        ''
    ],
    'ALLOCEXPRESSION': ['new'],
    'ALLOCEXPRESSION\'': ['int', 'float', 'string'],
    'ALLOCEXPRESSION\'\'': ['['],
    'ALLOCEXPRESSION\'\'\'': ['[', ''],
    'EXPRESSION': ['+', '-', 'int_constant', 'float_constant', 'string_constant', 'null', 'ident', '('],
    'EXPRESSION\'': ['<', '>', '=', '!'],
    'EXPRESSION\'\'': ['=', '+', '-', 'int_constant', 'float_constant', 'string_constant', 'null', 'ident', '('],
    'NUMEXPRESSION': ['+', '-', 'int_constant', 'float_constant', 'string_constant', 'null', 'ident', '('],
    'NUMEXPRESSION\'': ['+', '-', ''],
    'TERM': ['+', '-', 'int_constant', 'float_constant', 'string_constant', 'null', 'ident', '('],
    'TERM\'': ['*', '/', '%', ''],
    'UNARYEXPR': ['+', '-', 'int_constant', 'float_constant', 'string_constant', 'null', 'ident', '('],
    'FACTOR': [
        'int_constant',
        'float_constant',
        'string_constant',
        'null',
        'ident',
        '('
    ],
    'LVALUE': ['ident'],
    'LVALUE\'': ['[', '']
}

const follows = {
    'PROGRAM': ['$'],
    'FUNCLIST': ['$'],
    'FUNCLIST\'': ['$'],
    'FUNCDEF': ['def', '$'],
    'PARAMLIST': [')'],
    'PARAMLIST\'': [')'],
    'STATEMENT': [
        '$', 'else',
        'int',
        'float',
        'string',
        'ident',
        'print',
        'read',
        'return',
        'if',
        'for',
        '{',
        'break',
        ';',
        '}'
    ],
    'VARDECL': [';'],
    'VARDECL\'': [';'],
    'ATRIBSTAT': [';', ')'],
    'ATRIBSTAT\'': [';', ')'],
    'FUNCCALL': [';', ')'],
    'PARAMLISTCALL': [')'],
    'PARAMLISTCALL\'': [')'],
    'PRINTSTAT': [';'],
    'READSTAT': [';'],
    'RETURNSTAT': [';'],
    'IFSTAT': [
        '$', 'else',
        'int',
        'float',
        'string',
        'ident',
        'print',
        'read',
        'return',
        'if',
        'for',
        '{',
        'break',
        ';',
        '}'
    ],
    'IFSTAT\'': [
        '$', 'else',
        'int',
        'float',
        'string',
        'ident',
        'print',
        'read',
        'return',
        'if',
        'for',
        '{',
        'break',
        ';',
        '}'
    ],
    'FORSTAT': [
        '$', 'else',
        'int',
        'float',
        'string',
        'ident',
        'print',
        'read',
        'return',
        'if',
        'for',
        '{',
        'break',
        ';',
        '}'
    ],
    'STATELIST': ['}'],
    'STATELIST\'': ['}'],
    'ALLOCEXPRESSION': [';', ')'],
    'ALLOCEXPRESSION\'': [';', ')'],
    'ALLOCEXPRESSION\'\'': [';', ')'],
    'ALLOCEXPRESSION\'\'\'': [';', ')'],
    'EXPRESSION': [';', ')'],
    'EXPRESSION\'': [';', ')'],
    'EXPRESSION\'\'': [';', ')'],
    'NUMEXPRESSION': [']', ')', '<', '>', '=', '!', ';'],
    'NUMEXPRESSION\'': [']', ')', '<', '>', '=', '!', ';'],
    'TERM': ['+', '-'],
    'TERM\'': ['+', '-'],
    'UNARYEXPR': ['*', '/', '%', '+', '-'],
    'FACTOR': ['*', '/', '%', '+', '-'],
    'LVALUE': ['=', ';', '*', '/', '%', '+', '-'],
    'LVALUE\'': ['=', ';', '*', '/', '%', '+', '-']
}

const populateProductions = ({ containerId, productions, type }) => {
    const targetContainer = document.getElementById(containerId)

    for (const production in productions) {
        if (Object.hasOwnProperty.call(productions, production)) {
            const productionContainer = document.createElement('p')
            productionContainer.className = 'bg-white px-2'
            if (type === 'production') {
                productionContainer.innerText = production
            } else if (type === 'first') {
                productionContainer.innerText = `First(${production})`
            } else if (type === 'follow') {
                productionContainer.innerText = `Follow(${production})`
            }

            const arrowContainer = document.createElement('p')
            arrowContainer.className = 'bg-white'
            arrowContainer.innerText = type === 'production' ? '→' : '='

            const productionsContainer = document.createElement('div')
            const containers = productions[production].forEach((productionBody) => {
                const productionBodyContainer = document.createElement('p')
                const bodyText = productionBody === '' ? 'ε' : productionBody
                const bodyContent = bodyText.split('').forEach((letter) => {
                    productionBodyContainer.innerHTML += `<span ${(letter.toLowerCase() === letter && letter !== '\'') ? 'class=\"text-red\"' : ''}>${letter}</span>`
                })
                productionsContainer.appendChild(productionBodyContainer)
            })
            productionsContainer.className = 'flex flex-wrap divide-x space-x-4 bg-white'

            targetContainer.appendChild(productionContainer)
            targetContainer.appendChild(arrowContainer)
            targetContainer.appendChild(productionsContainer)
        }
    }
}

const main = () => {
    populateProductions({ containerId: 'first-target-container', productions: initialProductions, type: 'production' })
    populateProductions({ containerId: 'second-target-container', productions: factoredProductions, type: 'production' })
    populateProductions({ containerId: 'firsts-target-container', productions: firsts, type: 'first' })
    populateProductions({ containerId: 'follows-target-container', productions: firsts, type: 'follow' })
}

window.addEventListener('load', () => { main() })
