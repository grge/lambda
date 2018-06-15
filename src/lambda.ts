import _ = require("underscore");
import $ = require("jquery");

/* 
 *
 * Errors
 *
 */

var ParseErrorUnknownTerm = function(term) { 
    return {
        name: "ParseErrorUnknownTerm",
        message: "Not a term: " + term
    }
}

var ParseErrorUnexpectedSymbol = function(sym) {
    return {
        name: "ParseErrorUnexpectedSymbol",
        message: "Unexpected symbol: " + sym
    }
};

var ParseErrorUnexpectedEOF = function() {
    return {
        name: "ParseErrorUnexpectedEOF",
        message: "Unexpected EOF"
    }
};

var SubstituteError = function(sub, term) { 
    return {
        name: "SubstituteError",
        message: "Couldn't substitute " + sub + " in " + to_text(term)
    }
}

var BetaReductionError = function(term) {
    return {
        name: "BetaReductionError",
        message: "Can't do Beta Reduction on " + to_text(term)
    }
}

var EtaConversionError = function(term) {
    return {
        name: "EtaConversionError",
        message: "Can't do Eta Convesion on " + to_text(term)
    }
}

var AlphaConversionError = function(term) {
    return {
        name: "AlphaConversionError",
        message: "Can't do Alpha Convesion on " + to_text(term)
    }
}

/* 
 *
 * AST Data Structures
 *
 */


type Variable = {
    kind: "Variable";
    name: string;
}

type Lambda = {
    kind: "Lambda";
    bind: Variable;
    body: Term;
}

type Application = {
    kind: "Application";
    lambda: Term;
    argument: Term;
}

type Term = Lambda | Application | Variable;

// Check if a given variable is free in a given term
// To be free, the variable must be present in the term, but not bound to 
// an outer lambda.
function is_free_in(variable:Variable, term:Term): boolean {
    switch (term.kind) {
        case 'Variable':
            return variable.name == term.name;
        case 'Lambda':
            return (variable.name != term.bind.name) && is_free_in(variable, term.body)
        case 'Application':
            return is_free_in(variable, term.lambda) || is_free_in(variable, term.argument)
    }
}

// Look for instances of 'match' in 'term', and replace with 'replace'
function substitute(term:Term, match:Variable, replace:Term): Term {
    switch (term.kind) {
        case 'Variable':
            return term.name == match.name ? replace : term;
        case 'Lambda':
            // if the lambda bind clobbers the match, don't substitute
            if (match.name == term.bind.name) {
                return term;
            } else {
                let body = substitute(term.body, match, replace)
                let lambda:Lambda = {kind:'Lambda', bind: term.bind, body:body}
                return lambda
            }
        case 'Application':
            let lambda = substitute(term.lambda, match, replace)
            let argument = substitute(term.argument, match, replace)
            let application:Application = {kind:"Application", lambda:lambda, argument:argument}
    }
}

function alpha_conversion(lambda:Lambda, replace:Term):Lambda {
    return <Lambda>{kind:"Lambda", bind:replace, body:substitute(lambda.body, lambda.bind, replace)};
}


function beta_reduction(application:Application):Term {
    if (application.lambda.kind != 'Lambda') {
        throw BetaReductionError(application);
    }
    return substitute(application.lambda.body, application.lambda.bind, application.argument)
}

function eta_conversion(lambda:Lambda):Term {
    if (lambda.body.kind != 'Application' || lambda.body.argument != lambda.bind) {
        throw EtaConversionError(lambda);
    }
    return lambda.body.lambda
}

function reduce(term:Term):Term {
    switch (term.kind) {
        case 'Lambda':
            return <Lambda>{kind: "Lambda", bind:term.bind, body:reduce(term.body)};
        case 'Application':
            switch (term.lambda.kind) {
                case 'Lambda':
                    var result = beta_reduction(term);
                    print_line(to_text(result))
                    if (to_text(result) == to_text(term)) {
                        print_line("Loop detected... halting");
                        return result;
                    } 
                    else {
                        return reduce(result);
                    }
                case 'Application':
                    return reduce(<Application>{kind: 'Application', lambda:reduce(term.lambda), argument:term.argument});
                case 'Variable':
                    return <Application>{kind: "Application", lambda:term.lambda, argument:reduce(term.argument)};
            }
        case 'Variable':
            return term;
    }
}

function to_json(term:Term):any {
    return JSON.stringify(term, undefined, 2);
}

function to_text(term:Term):string {
    switch (term.kind) {
        case 'Variable':
            return term.name;
        case 'Lambda':
            return 'λ' + to_text(term.bind) + '.' + to_text(term.body);
        case 'Application':
            return [term.lambda, term.argument].map((term:Term):string => {
                return (term.kind) == 'Variable' ? to_text(term) : "(" + to_text(term) + ")";
            }).join(' ');
    }
}

function to_boolean(term:Term):boolean {
    let x:Variable = {kind:'Variable', name:'x'};
    let y:Variable = {kind:'Variable', name:'y'};
    return reduce(<Application>{kind: 'Application', lambda: <Application>{kind:'Application', lambda:term, argument:x}, argument:y}) == x;
}

function print_line(text:string) {
    var el = $('#output');
    var existing = el.text()
    el.text(existing + text + "\n\n");
}


/* 
 *
 * Parser
 *
 */

var from_text = function(str:string):Term {
    // TODO: Change tokenizer to give position markers to the parser.
    var tokenize = function(str) {
        return str.match(/[.λ\(\)]|(\w|\'+)/g);
    }

    var parse = function(tokens) {
        var token = null;

        var next = function() { 
            if (tokens.length) {
                token = tokens.shift();
            }
            else {
                throw ParseErrorUnexpectedEOF();
            }
        }

        var peek = function() {
            return tokens.length ? tokens[0] : null;
        };

        var parse_term = function() {
            next();
            let left:Term = parse_simple_term();
            while (peek() != ")" && peek() != null) {
                next();
                left = <Application>{lambda:left, argument:parse_simple_term()};
            }
            return left;
        }

        var parse_simple_term = function() {
            switch(token) {
                case 'λ':
                    return parse_lambda();
                case '(':
                    var t = parse_term();
                    next();
                    return t;
                case '.':
                case ')':
                    throw ParseErrorUnexpectedSymbol(token);
                default:
                    return parse_variable();
            }
        }

        var parse_variable = function() {
            return <Variable>{name:token};
        }

        var parse_lambda = function() {
            next();
            var bind = <Variable>{name:token};
            next();
            if (token != ".") {
                throw ParseErrorUnexpectedSymbol(token);
            }
            return <Lambda>{bind:bind, body:parse_term()};
        }

        var result = parse_term();

        if (peek()) {
            next();    
            throw ParseErrorUnexpectedSymbol(token);
        }

        return result;
    }
    return parse(tokenize(str));
}

var parse_input = function() {
    try {
        $('#output').text("");
        var term:Term = from_text($('#input').text());
        print_line(to_text(term));
        // $('#diagram').html(draw_lines(term));
        var term = reduce(term);
        print_line(to_text(term));
        $('#parse-error-box').text('')
    }
    catch (e) {
        $('#parse-error-box').text(e.message)
    }
}


// var draw_lines = function(term:Term, deep) {
//     var incr = function(x) { return x + 1 };
//     var deep = deep ? _.object(_.keys(deep), _.map(_.values(deep), incr)) : {};

//     switch (typeof term) {
//         case 'Variable':
//             var depth = deep[term.name];
//             var h = (depth || 1) * 10 + "px";
//             var t = (1 + (depth || 1))* -10 + "px";
//             console.log(term.name, h, t);
//             return $('<div id="variable">').css({'height':h, 'top':t});
//         case 'Lambda':
//             deep[term.bind.name] = 1;
//             return $('<div id="lambda">').append(draw_lines(term.body, deep));
//         case 'Application':
//             var arg = draw_lines(term.argument, deep);
//             var lambda = draw_lines(term.lambda, deep);
//             return $('<div id="application">').append(lambda).append(arg);

//     }
// }

var create_input = function() {
    var el = $('#input').attr('contentEditable', true);

    var insert = function(insert_text) {
        var range = window.getSelection().getRangeAt(0)
        range.deleteContents();
        range.insertNode(document.createTextNode(insert_text));
        range.collapse(false);
        return false;
    }

    el.keypress(function(e) {
        if (e.key == '\\') {
            insert('λ');
            return false
        }
        return true;
    });
    el.keyup(parse_input);

}

create_input();
let term:Term = {
    kind: "Application",
    lambda: {
        kind: "Lambda",
        bind: {kind:'Variable', name:'x'},
        body: {kind:'Variable', name:'x'}
    },
    argument: {kind:'Variable', name:'y'}
}

$('#input').text(to_text(term));
parse_input();

