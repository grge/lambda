module Lambda exposing (Term(..), Var, Lambda, Application, toText,
                        alphaConvert, reduce, substitute, isFreeIn, getFreshVar)

type alias Var = { name : String }
type alias Lambda = { bind : Var, body : Term }
type alias Application = { lambda : Term, argument : Term }

type Term = VarTerm Var
          | LambdaTerm Lambda
          | ApplicationTerm Application


isFreeIn : Var -> Term -> Bool
isFreeIn v t = 
    case t of
        VarTerm v2 -> 
            v == v2
        LambdaTerm l -> 
            v /= l.bind && isFreeIn v l.body
        ApplicationTerm a -> 
            isFreeIn v a.lambda || isFreeIn v a.argument

isFreshIn : Var -> Term -> Bool
isFreshIn v t = 
    case t of
        VarTerm v2 ->
            v /= v2
        LambdaTerm l ->
            v == l.bind || isFreshIn v l.body
        ApplicationTerm a ->
            isFreshIn v a.lambda && isFreshIn v a.argument


getFreshVar : Var -> Term -> Var
getFreshVar var term =
    if isFreshIn var term then
        var
    else
        getFreshVar (Var (var.name ++ "'")) term


substitute : Var -> Term -> Term -> Term
substitute match replace term =
    let 
        sub = substitute match replace 
    in
        case term of
            VarTerm v ->
                if v == match then 
                    replace -- x[x:t] => t
                else 
                    term -- y[x:t] => y
            ApplicationTerm a -> 
                ApplicationTerm {lambda = sub a.lambda, argument = sub a.argument} 
            LambdaTerm l -> 
                if match == l.bind then
                    term -- (\x.t)[x:=s] => \x.t 
                else
                    let
                        freshVar = getFreshVar l.bind replace
                        freshBody = substitute l.bind (VarTerm freshVar) l.body
                    in
                        LambdaTerm { bind = freshVar, body = sub freshBody }


alphaConvert : Lambda -> Var -> Lambda
alphaConvert lambda replace =
    Lambda replace (substitute lambda.bind (VarTerm replace) lambda.body)


betaReduce : Application -> Term
betaReduce a =
    case a.lambda of
        LambdaTerm l ->
            substitute l.bind a.argument l.body
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

