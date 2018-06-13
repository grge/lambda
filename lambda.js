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

var ParseErrorUnexpectedEOF = function(sym) {
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

var variable = function(x) {
    return {
        type: 'variable',
        name: x
    };
};    

var lambda = function(bind, body) {
    return {
        type: 'lambda',
        bind: bind,
        body: body
    }
}

var application = function(lambda, arg) {
    return {
        type: 'application',
        lambda: lambda, 
        arg: arg
    };
}

var type = function(term) {
    var s = typeof term;
    if (s === 'object') {
        return term.type
    }
    else {
        throw ParseErrorUnknownTerm(JSON.stringify(term));
    }
}

/* 
 *
 * Reduction 
 *
 */

var is_free_in = function(variable, term) {
    var is_free_in = function(term) {
        switch (type(term)) {
            case 'variable':
                return (variable.name == term.name);
            case 'lambda':
                return (variable.name != term.bind.name) && is_free_in(term.body);
            case 'application':
                return is_free_in(term.lambda) || is_free_in(term.arg);
        }
    }
    return is_free_in(term);
}

var substitute = function(term, match, replace) {
    var sub = function(term) {
        switch (type(term)) {
            case 'variable':
                return term.name == match.name ? replace : term;
            case 'lambda':
                if (match == term.bind) {
                    return term;
                } else {
                    var t = add_free_term(term);
                    return lambda(t.bind, sub(t.body));
                }
            case 'application':
                return application(sub(term.lambda), sub(term.arg));
        }
    };
    return sub(term);
}

var add_free_term = function(term, replace) {
    var t;
    while(is_free_in(term.bind, replace)) {
        t = alpha_conversion(term, variable(term.bind.name + "'"));
    }
    return t;
}

var alpha_conversion = function(term, replace) {
    if (type(term) != 'lambda') throw AlphaConversionError(term);
    return lambda(replace, substitute(term.body, term.bind, replace));
}

var beta_reduction = function(term) {
    if (type(term) != 'application' ||
       (type(term.lambda) != 'lambda')) {
           throw BetaReductionError(term);
    }
    return substitute(term.lambda.body, term.lambda.bind, term.arg);
}

var eta_conversion = function(term) {
    if (type(term) != 'lambda' ||
        type(term.body) != 'application' ||
        term.body.arg != term.bind ) {
            throw EtaConversionError(term);
    }
    return term.body.lambda;
}

var reduce = function(term) {
    switch (type(term)) {
        case 'application':
            switch (type(term.lambda)) {
                case 'lambda':
                    var result = beta_reduction(term);
                    print(to_text(result))
                    if (to_text(result) == to_text(term)) {
                        print("Loop detected... halting");
                        return result;
                    } 
                    else {
                        return reduce(result);
                    }
                case 'application':    
                    return reduce(application(reduce(term.lambda), term.arg));
                case 'variable':
                    return application(term.lambda, reduce(term.arg));
                default:
                    console.log(term);
                    throw {name:"wtf"};
            }
        case 'lambda':
            return lambda(term.bind, reduce(term.body))
        case 'variable':
            return term;
        default:
            throw {name: "wtf"};
    }
}

var to_json = function(term) {
    return JSON.stringify(term, undefined, 2);
}

var to_text = function(term) {
    switch (type(term)) {
        case 'variable':
            return term.name;
        case 'lambda':
            return 'λ' + to_text(term.bind) + '.' + to_text(term.body);
        case 'application':
            return _.map([term.lambda, term.arg], function(e) {
                return type(e) == 'variable' ? to_text(e) : "(" + to_text(e) + ")";
            }).join(' ')
    }
}

var to_boolean = function(term) {
    var x = variable('x');
    var y = variable('y');
    return reduce(application(application(term, x), y)) == x;
}

var print = function(text) {
    var el = $('#output');
    var existing = el.text()
    el.text(existing + text + "\n\n");
}


/* 
 *
 * Parser
 *
 */

var from_text = function(str) {
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
            var left = parse_simple_term();
            while (peek() != ")" && peek() != null) {
                next();
                left = application(left, parse_simple_term());
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
            return variable(token);
        }

        var parse_lambda = function() {
            next();
            var bind = variable(token);
            next();
            if (token != ".") {
                throw ParseErrorUnexpectedSymbol(token);
            }
            return lambda(bind, parse_term());
        }

        var result = parse_term(tokens);

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
        var term = from_text($('#input').text());
        print(to_text(term));
        $('#diagram').html(draw_lines(term));
        var term = reduce(term);
        print(to_text(term));
        $('#parse-error-box').text('')
    }
    catch (e) {
        $('#parse-error-box').text(e.message)
    }
}


var draw_lines = function(term, deep) {
    var incr = function(x) { return x + 1 };
    var deep = deep ? _.object(_.keys(deep), _.map(_.values(deep), incr)) : {};

    switch (type(term)) {
        case 'variable':
            var depth = deep[term.name];
            var h = (depth || 1) * 10 + "px";
            var t = (1 + (depth || 1))* -10 + "px";
            console.log(term.name, h, t);
            return $('<div id="variable">').css({'height':h, 'top':t});
        case 'lambda':
            deep[term.bind.name] = 1;
            return $('<div id="lambda">').append(draw_lines(term.body, deep));
        case 'application':
            var arg = draw_lines(term.arg, deep);
            var lambda = draw_lines(term.lambda, deep);
            return $('<div id="application">').append(lambda).append(arg);

    }
}

var tie_binds = function(term) {
    switch (type(term)) {
        case 'variable':
        case 'lambda':
        case 'application':
    }
}

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
$('#input').text(to_text(application(lambda(variable('x'), variable('x')), variable('y'))));
parse_input();

