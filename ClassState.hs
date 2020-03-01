module ClassState
where

-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq)

-- TODO - Trebuie definit ClassState
data Pos = Variable String String | Function String String [String]

type ClassState = [Pos]

initEmptyClass :: ClassState
initEmptyClass = []

insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState

insertIntoClass clasa Var param = (Variable x y) : clasa
                                where [x, y] = param

insertIntoClass clasa Func param = (Function x y list) : clasa
                                where list = drop 2 param 
                                      [x, y] = take 2 param


getValues :: ClassState -> InstrType -> [[String]]
getValues [] Var = []
getValues [] Func = []

getValues ((Variable simbol tip) : clasa) Var = [simbol, tip] : getValues clasa Var
getValues ((Function simbol tip param) : clasa) Var = getValues clasa Var

getValues((Function simbol tip param) : clasa) Func = (simbol: tip: param) : getValues clasa Func
getValues((Variable simbol tip) : clasa) Func = getValues clasa Func


