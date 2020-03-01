module Parser
where

import Util
import Data.Maybe
import InferenceDataType
import ClassState

-- Definire Program
--                            Nume    PARINTE--
data Clasa = Clasa ClassState String String
type Program = [Clasa]
type Instruction = [String]

initEmptyProgram :: Program
initEmptyProgram = [Clasa initEmptyClass "Global" "Global"]


getVars :: Program -> [[String]]
getVars [] = getValues [] Var
getVars ((Clasa cls nume parinte) : program) = (getValues cls Var) ++ (getVars program)

getClasses :: Program -> [String]
getClasses [] = []
getClasses ((Clasa cls nume parinte) : prog) = nume : (getClasses prog)

getParentClass :: String -> Program -> String
getParentClass name [] = []
getParentClass name ((Clasa cls nume parinte) : prog) = if (name == nume) then parinte
                                                        else getParentClass name prog

getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass clsCautare [] = getValues [] Func
getFuncsForClass clsCautare ((Clasa cls nume parinte) : program) = if (clsCautare == nume) then (getValues cls Func)
                                                                   else getFuncsForClass clsCautare program
-- Instruction poate fi ce considerați voi

{-In parse mai intai impart textul dupa linii (split dupa \n) si imi rezulta o lista de linii, iar apoi splituiesc fiecare linie in cuvinte (instructiuni) -}
parse :: String -> [Instruction]
parse text = map (takeInstr) lin
            where lin = (parsareCuv text [] "\n" [])

{-Preiau instructiunile din fiecare linie -}
takeInstr :: String -> Instruction
takeInstr line = parsareCuv line [] " =:()," []

isEmpty :: String -> Bool
isEmpty y = y == []

{- Impart cuvantul in functie de ce se afla in delimit (string cu elemente " ()=:") si apoi construiesc in acumulatorul y instructiunea
 - Verific caracter cu caracter daca char-ul curent este unul din delim cu ajutorul functiei implicite elem
 -}
parsareCuv :: String -> String -> String -> [String] -> [String]
parsareCuv [] [] delimit list = list
parsareCuv [] y delimit list = list ++ [y]
parsareCuv (x:xs) [] delimit list = if (x `elem` delimit) then (parsareCuv xs [] delimit list)
                         else (parsareCuv xs [x] delimit list)
parsareCuv (x:xs) y delimit list = if (x `elem` delimit) then (parsareCuv xs [] delimit (list++[y]))
                         else parsareCuv xs (y++[x]) delimit list

{- In interpret transmit instructiunile ce au fost splituite in functia parse si verific
 - Verific in functie de prima instructiune daca este clasa sau variabila , in caz contrar este functie
 - Dupa verificare incep inserare . La clase am 2 cazuri :- primul , cand clasa actuala nu extinde alta clasa definita deja (length < 4) => apartine Global
 -                                                        - al 2-lea , cand clasa actuala extinde o alta clasa (are instructiunea extends => nr de elem din instructiuni == 4)
 - Apoi verific , in functie de caz,daca este deja definita clasa de construit si daca este definita si clasa ce o extinde (decat pt length == 4)
 - Daca este deja definita , nu mai are rost sa o reconstruiesc

 -- Pt variabile verific daca exista type-ul ca si clasa si , in caz afirmativ, inserez variabila in clasa Global cu tipul si cu numele date ca instructiuni
 -- In caz negativ , nu inserez variabila

 -- Pentru functii verific daca fiecare instructiune data este o clasa, mai putin numele
 -- (take 2 instr) + (drop 3 instr) => dau skip la numele functiei
-}

interpret :: Instruction -> Program -> Program
interpret instr prog 
{-Inserez clasele-}
        
        | ((instr!!0) == "class") && (length instr < 4) && (isDefined (instr!!1) prog == False) 
                                        = (Clasa initEmptyClass (instr!!1) "Global") : prog
        | ((instr!!0) == "class") && (length instr < 4) && (isDefined (instr!!1) prog) = prog

        | ((instr!!0) == "class") && (length instr == 4) && ( (isDefined (instr!!3) prog) == False)
                                && (isDefined (instr!!1) prog == False) = 
                                (Clasa initEmptyClass (instr!!1) "Global") : prog

        | ((instr!!0) == "class") && (length instr == 4) && (isDefined (instr!!3) prog)
                                && (isDefined (instr!!1) prog == False) = 
                                (Clasa initEmptyClass (instr!!1) (instr!!3)) : prog
                                

        | ((instr!!0) == "class") && (length instr == 4) && (isDefined (instr!!3) prog)
                                  && (isDefined (instr!!1) prog) = prog

        | ((instr!!0) == "class") && (length instr == 4) && ((isDefined (instr!!3) prog) == False)
                                  && (isDefined (instr!!1) prog) = prog


{-Inserez variabilele-}
        | ((instr!!0) == "newvar") && (isDefined (instr!!2) prog) = addNewVar [(instr!!1), (instr!!2)] prog
        | ((instr!!0) == "newvar") && (isDefined (instr!!2) prog == False) = prog

{-Inserez functiile-}
        | ((instr!!0) /= "newvar" && (instr!!0) /= "class") && (verifyFunction ((take 2 instr) ++ (drop 3 instr)) prog ) = 
                        addNewFunc ((instr!!2) : (instr!!0) : (drop 3 instr)) (instr!!1) prog
                        {-(instr!!2) : (instr!!0) : (drop 3 instr) ==
                        numele functiei:tipulfunctiei:restul de parametrii (fara nume, tip si clasa) -}
        | otherwise = prog

{-Parcurg parametrii si verific daca sunt definiti ca si clase
-}
verifyFunction :: [String] -> Program -> Bool
verifyFunction [] program = True
verifyFunction (nume : param) program = isDefined nume program && verifyFunction param program

{-Parcurg programul (lista de clase) si inserez in clasa al carei nume corespunde cu cel dat ca parametru
-}
addNewFunc :: [String] -> String -> Program -> Program
addNewFunc param numeClasa ((Clasa cls nume parinte) : program)
        | nume == numeClasa = (Clasa (insertIntoClass cls Func param) nume parinte) : program
        | otherwise = (Clasa cls nume parinte) : addNewFunc param numeClasa program

{-Adaug variabila in clasa Global . Mai intai o caut prin program si , dupa ce o gasesc , fac inserarea in clasa a variabilei
-}
addNewVar :: [String] -> Program -> Program
addNewVar param ((Clasa cls nume parinte) : program)
        | nume == "Global" = (Clasa (insertIntoClass cls Var param) nume parinte) : program
        | otherwise  = (Clasa cls nume parinte) : addNewVar param program

{-Verific daca o clasa exista in program-}
isDefined :: String ->Program -> Bool
isDefined clsCautare [] = False
isDefined clsCautare ((Clasa cls nume parinte) : program) = clsCautare == nume || isDefined clsCautare program


infer :: Expr -> Program -> Maybe String
infer expr program = Nothing


