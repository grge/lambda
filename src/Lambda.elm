module Lambda exposing (Term(..), Var, Lambda, Application, toText,
                        alphaConvert, reduce, substitute, isFreeIn)

type alias Var = { name : String }
type alias Lambda = { bind : Var, body : Term }
type alias Application = { lambda : Term, argument : Term }

type Term = VarTerm Var
          | LambdaTerm Lambda
          | ApplicationTerm Application


isFreeIn : String -> Term -> Bool
isFreeIn v t = case t of
    VarTerm v2 -> 
        v == v2.name
    LambdaTerm l -> 
        v /= l.bind.name && isFreeIn v l.body
    ApplicationTerm a -> 
        isFreeIn v a.lambda || isFreeIn v a.argument


substitute : Term -> Var -> Term -> Term
substitute term match replace =
    let 
        sub t = substitute t match replace
    in
        case term of
            VarTerm v ->
                if v == match then replace else term
            ApplicationTerm a -> 
                ApplicationTerm {lambda = sub a.lambda, argument = sub a.argument} 
            LambdaTerm l -> 
                if match == l.bind then
                    term
                else
                    LambdaTerm {bind = l.bind, body = sub l.body}


alphaConvert : Lambda -> Var -> Lambda
alphaConvert lambda replace =
    Lambda replace (substitute lambda.body lambda.bind (VarTerm replace))


betaReduce : Application -> Term
betaReduce a =
    case a.lambda of
        LambdaTerm l ->
            substitute l.body l.bind a.argument
        _ ->
            ApplicationTerm a


reduce : Term -> Term
reduce term =
    case term of
        VarTerm v -> 
            VarTerm v
        LambdaTerm l -> 
            LambdaTerm {bind = l.bind, body = reduce l.body}
        ApplicationTerm a ->
            case a.lambda of
                LambdaTerm l ->
                    let out = betaReduce a in 
                        if out == term then out else reduce out
                ApplicationTerm a2 ->
                    reduce (ApplicationTerm {lambda = reduce(a.lambda), argument=a.argument})
                VarTerm v ->
                    ApplicationTerm {lambda = a.lambda, argument=reduce a.argument}


toText : Term -> String
toText term =
    case term of
        VarTerm v -> v.name
        LambdaTerm l -> "λ" ++ l.bind.name ++ "." ++ toText l.body
        ApplicationTerm a -> "(" ++ toText a.lambda ++ ")(" ++ toText a.argument ++ ")"

