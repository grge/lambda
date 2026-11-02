module Lambda exposing (Term(..), Var, Lambda, Application, toText,
                        alphaConvert, betaReduce, reduce, reduceWithin, substitute, isFreeIn, getFreshVar)

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
                else if isFreeIn l.bind replace then
                    let
                        avoid = ApplicationTerm { lambda = replace, argument = l.body }
                        freshVar = getFreshVar l.bind avoid
                        freshBody = substitute l.bind (VarTerm freshVar) l.body
                    in
                        LambdaTerm { bind = freshVar, body = sub freshBody }
                else
                    LambdaTerm { bind = l.bind, body = sub l.body }


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


step : Term -> Maybe Term
step term =
    case term of
        VarTerm _ ->
            Nothing
        LambdaTerm l ->
            Maybe.map (\body -> LambdaTerm { bind = l.bind, body = body }) (step l.body)
        ApplicationTerm a ->
            case a.lambda of
                LambdaTerm _ ->
                    Just (betaReduce a)
                _ ->
                    case step a.lambda of
                        Just lambda ->
                            Just (ApplicationTerm { a | lambda = lambda })
                        Nothing ->
                            Maybe.map (\argument -> ApplicationTerm { a | argument = argument }) (step a.argument)


size : Term -> Int
size term =
    case term of
        VarTerm _ -> 1
        LambdaTerm l -> 1 + size l.body
        ApplicationTerm a -> 1 + size a.lambda + size a.argument


maxSteps : Int
maxSteps = 2000


maxSize : Int
maxSize = 10000


-- Normal order reduction. Err holds the term reached on giving up: not every term has a normal form.
reduceWithin : Int -> Term -> Result Term Term
reduceWithin steps term =
    case step term of
        Nothing ->
            Ok term
        Just next ->
            if steps <= 0 || size next > maxSize then
                Err next
            else
                reduceWithin (steps - 1) next


reduce : Term -> Result Term Term
reduce = reduceWithin maxSteps


toText : Term -> String
toText term =
    case term of
        VarTerm v -> v.name
        LambdaTerm l -> "λ" ++ l.bind.name ++ "." ++ toText l.body
        ApplicationTerm a -> lambdaText a.lambda ++ " " ++ argumentText a.argument


lambdaText : Term -> String
lambdaText term =
    case term of
        LambdaTerm _ -> "(" ++ toText term ++ ")"
        _ -> toText term


argumentText : Term -> String
argumentText term =
    case term of
        VarTerm _ -> toText term
        _ -> "(" ++ toText term ++ ")"
