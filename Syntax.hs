module Main where
import Grammar
import Tokens
import Data.Map (Map)
import qualified Data.Map as Map

data AsnType = AsnName | AsnVal deriving (Eq, Ord, Show)

data Address = Val Int 
	| Name String 
	deriving (Show)

data Code = Add Address Address Address {- arit -}
	| Sub Address Address Address 
	| Mul Address Address Address 
	| Divv Address Address Address 
	| Asn Address Address 
	| Grt Address Address Address {- bools -}
	| Less Address Address Address
	| GrtEq Address Address Address
	| LessEq Address Address Address
	| Eq Address Address Address
	| Dif Address Address Address
	| Andd Address Address Address
	| Orr Address Address Address
	| If_False Address Code {- cmd -}
	| At Address Address Address
	| Goto Address
	| Label Address
	deriving (Show)

emptyMap = Map.empty

removeMain :: Prog -> Cmd
removeMain (Main s) = s

vars :: Cmd -> Map String Type -> Map String Type
vars (Seq a b) m = Map.union (vars a m) (vars b m)
vars (DeclareIntVar v) m = (Map.insert (v) (Int) m)
vars (DeclareBoolVar v) m = (Map.insert (v) (Bool) m) 
vars _ m = emptyMap

symToData :: Map String Type -> String
symToData x = computeList (Map.toList (x))

computeList :: [(String, Type)] -> String
computeList [] = ".data\n"
computeList ((a,Int):xs) = computeList xs ++ (stringEscape a ++ ":" ++ " .space 4\n")
computeList ((a,Bool):xs) = computeList xs ++ (stringEscape a ++ ":" ++ " .space 1\n")

compile_cmd :: Cmd -> Int -> (Address, [Code], Int)
compile_cmd (Seq a b) i = 
	let 
		(a1, c1, i1) = compile_cmd a i
		(a2, c2, i2) = compile_cmd b i1
		(a3, i3) = new_var i2
	in (a3, c1 ++ c2, i3)
compile_cmd (Attrib a b) i = 
	let
		(a1, c1, i1) = compile_exp (Var a) i
		(a2, c2, i2) = compile_exp b i1
		(a3, i3) = new_var i2
	in (a3, c1 ++ c2 ++ [At a3 a1 a2], i3)
compile_cmd (IfElse a b c) i = 
	let
		(a1, c1, i1) = compile_exp a i
		(a2, c2, i2) = compile_cmd b i1
		(a3, c3, i3) = compile_cmd c i2
		(a4, i4) = new_var i3
	in (a4, c1 ++ [If_False a1 (Goto a3)] ++ [Goto a4] ++ [Label a3] ++ c3 ++ [Label a4], i4)
compile_cmd (If a b) i = 
	let
		(a1, c1, i1) = compile_exp a i
		(a2, c2, i2) = compile_cmd b i1
		(a3, i3) = new_var i2
	in (a3, c1 ++ [If_False a1 (Goto a3)] ++ c2 ++ [Label a3], i3)
compile_cmd (While a b) i = 
	let
		(a1, c1, i1) = compile_exp a i
		(a2, c2, i2) = compile_cmd b i1
		(a3, i3) = new_var i2
	in (a3, [Label a1] ++ c1 ++ [If_False a1 (Goto a3)] ++ c2 ++ [Goto a1] ++ [Label a3], i3)
compile_cmd (DeclareIntVar _) i = (Name "a0", [], i)
compile_cmd (DeclareBoolVar _) i = (Name "a0", [], i)

compile_exp :: Exp -> Int -> (Address, [Code], Int)
compile_exp (Plus e1 e2) i = 
	let 
		(a1, c1, i1) = compile_exp e1 i
		(a2, c2, i2) = compile_exp e2 i1
		(a3, i3) = new_var i2
	in (a3, c1 ++ c2 ++ [Add a3 a1 a2], i3)
compile_exp (Minus e1 e2) i = 
	let 
		(a1, c1, i1) = compile_exp e1 i
		(a2, c2, i2) = compile_exp e2 i1
		(a3, i3) = new_var i2
	in (a3, c1 ++ c2 ++ [Sub a3 a1 a2], i3)
compile_exp (Times e1 e2) i = 
	let 
		(a1, c1, i1) = compile_exp e1 i
		(a2, c2, i2) = compile_exp e2 i1
		(a3, i3) = new_var i2
	in (a3, c1 ++ c2 ++ [Mul a3 a1 a2], i3)
compile_exp (Div e1 e2) i = 
	let 
		(a1, c1, i1) = compile_exp e1 i
		(a2, c2, i2) = compile_exp e2 i1
		(a3, i3) = new_var i2
	in (a3, c1 ++ c2 ++ [Divv a3 a1 a2], i3)
compile_exp (Num n) i = 
	let
		(a1, x) = new_var i
	in (a1, [Asn a1 (Val n)], x)
compile_exp (Var v) i = 
	let
		(a1, x) = new_var i
	in (a1, [Asn a1 (Name v)], x)
compile_exp (Bracketed e1) i = compile_exp e1 i
compile_exp (Greater e1 e2) i = 
	let
		(a1, c1, i1) = compile_exp e1 i
		(a2, c2, i2) = compile_exp e2 i1
		(a3, i3) = new_var i2
	in (a3, c1 ++ c2 ++ [Grt a3 a1 a2], i3)
compile_exp (Lesser e1 e2) i = 
	let
		(a1, c1, i1) = compile_exp e1 i
		(a2, c2, i2) = compile_exp e2 i1
		(a3, i3) = new_var i2
	in (a3, c1 ++ c2 ++ [Less a3 a1 a2], i3)
compile_exp (GreaterEqual e1 e2) i = 
	let
		(a1, c1, i1) = compile_exp e1 i
		(a2, c2, i2) = compile_exp e2 i1
		(a3, i3) = new_var i2
	in (a3, c1 ++ c2 ++ [GrtEq a3 a1 a2], i3)
compile_exp (LesserEqual e1 e2) i = 
	let
		(a1, c1, i1) = compile_exp e1 i
		(a2, c2, i2) = compile_exp e2 i1
		(a3, i3) = new_var i2
	in (a3, c1 ++ c2 ++ [LessEq a3 a1 a2], i3)
compile_exp (Equals e1 e2) i = 
	let
		(a1, c1, i1) = compile_exp e1 i
		(a2, c2, i2) = compile_exp e2 i1
		(a3, i3) = new_var i2
	in (a3, c1 ++ c2 ++ [Eq a3 a1 a2], i3)
compile_exp (Different e1 e2) i = 
	let
		(a1, c1, i1) = compile_exp e1 i
		(a2, c2, i2) = compile_exp e2 i1
		(a3, i3) = new_var i2
	in (a3, c1 ++ c2 ++ [Dif a3 a1 a2], i3)
compile_exp (Or e1 e2) i = 
	let
		(a1, c1, i1) = compile_exp e1 i
		(a2, c2, i2) = compile_exp e2 i1
		(a3, i3) = new_var i2
	in (a3, c1 ++ c2 ++ [Orr a3 a1 a2], i3)
compile_exp (And e1 e2) i = 
	let
		(a1, c1, i1) = compile_exp e1 i
		(a2, c2, i2) = compile_exp e2 i1
		(a3, i3) = new_var i2
	in (a3, c1 ++ c2 ++ [Andd a3 a1 a2], i3)

new_var :: Int -> (Address, Int)
new_var n = (Name ("t" ++ show n), n+1)

getCode :: (Address, [Code], Int) -> [Code]
getCode (_,b,_) = b

stringEscape :: String -> String --removes quotes from show
stringEscape s = filter (/='"') (show s)

loadValueFromAddress :: Address -> Address -> String
loadValueFromAddress (Name a) (Name b) = "lw $" ++ stringEscape b ++ ", 0($" ++ stringEscape a ++ ")\n"

valOrNameTable :: [Code] -> [(Address, AsnType)] --table with type of Asn
valOrNameTable [] = []
valOrNameTable ((Asn a (Name _)):xs) = [(a, AsnName)] ++ valOrNameTable xs 
valOrNameTable ((Asn a (Val _)):xs) = [(a, AsnVal)] ++ valOrNameTable xs 
valOrNameTable (_:xs) = valOrNameTable xs

valOrNameTableLookup :: Address -> [(Address, AsnType)] -> AsnType
valOrNameTableLookup (Name a) ((Name b,t):xs) = if (a == b) then t else valOrNameTableLookup (Name a) xs

processTAC :: [Code] -> [(Address, AsnType)] -> String
processTAC [] _ = "li $v0, 10\nsyscall\n" --end program 
processTAC ((Asn (Name a) (Name b)):xs) t = "la $" ++ stringEscape a ++ ", " ++ stringEscape b ++ "\n" ++ processTAC xs t 
processTAC ((Asn (Name a) (Val b)):xs) t = "li $" ++ stringEscape a ++ ", " ++ stringEscape (show b) ++ "\n" ++ processTAC xs t 
processTAC ((At (Name a) (Name b) (Name c)):xs) t = "sw $" ++ stringEscape c ++ ", 0($" ++ stringEscape b ++ ")\n" ++ processTAC xs t 
processTAC ((Less (Name a) (Name b) (Name c)):xs) t --need to check whether our registers contain a pointer to a variable in memory (data) or an actual number
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnVal = "lw $s0, 0($" ++ stringEscape b ++ ")\nslt $" ++ stringEscape a ++ ", $s0, $" ++ stringEscape c ++ "\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnName = "lw $s0, 0($" ++ stringEscape b ++ ")\n" ++ "lw $s1, 0($" ++ stringEscape c ++ ")\nslt $" ++ stringEscape a ++ ", $s0, $s1\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnVal && valOrNameTableLookup (Name c) t == AsnName = "lw $s1, 0($" ++ stringEscape c ++ ")\nslt $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $s1\n" ++ processTAC xs t
	| otherwise = "slt $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $" ++ stringEscape c ++ "\n" ++ processTAC xs t
processTAC ((Grt (Name a) (Name b) (Name c)):xs) t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnVal = "lw $s0, 0($" ++ stringEscape b ++ ")\nsgt $" ++ stringEscape a ++ ", $s0, $" ++ stringEscape c ++ "\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnName = "lw $s0, 0($" ++ stringEscape b ++ ")\n" ++ "lw $s1, 0($" ++ stringEscape c ++ ")\nsgt $" ++ stringEscape a ++ ", $s0, $s1\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnVal && valOrNameTableLookup (Name c) t == AsnName = "lw $s1, 0($" ++ stringEscape c ++ ")\nsgt $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $s1\n" ++ processTAC xs t
	| otherwise = "sgt $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $" ++ stringEscape c ++ "\n" ++ processTAC xs t
processTAC ((LessEq (Name a) (Name b) (Name c)):xs) t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnVal = "lw $s0, 0($" ++ stringEscape b ++ ")\nsle $" ++ stringEscape a ++ ", $s0, $" ++ stringEscape c ++ "\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnName = "lw $s0, 0($" ++ stringEscape b ++ ")\n" ++ "lw $s1, 0($" ++ stringEscape c ++ ")\nsle $" ++ stringEscape a ++ ", $s0, $s1\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnVal && valOrNameTableLookup (Name c) t == AsnName = "lw $s1, 0($" ++ stringEscape c ++ ")\nsle $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $s1\n" ++ processTAC xs t
	| otherwise = "sle $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $" ++ stringEscape c ++ "\n" ++ processTAC xs t
processTAC ((GrtEq (Name a) (Name b) (Name c)):xs) t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnVal = "lw $s0, 0($" ++ stringEscape b ++ ")\nsge $" ++ stringEscape a ++ ", $s0, $" ++ stringEscape c ++ "\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnName = "lw $s0, 0($" ++ stringEscape b ++ ")\n" ++ "lw $s1, 0($" ++ stringEscape c ++ ")\nsge $" ++ stringEscape a ++ ", $s0, $s1\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnVal && valOrNameTableLookup (Name c) t == AsnName = "lw $s1, 0($" ++ stringEscape c ++ ")\nsge $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $s1\n" ++ processTAC xs t
	| otherwise = "sge $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $" ++ stringEscape c ++ "\n" ++ processTAC xs t
processTAC ((Eq (Name a) (Name b) (Name c)):xs) t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnVal = "lw $s0, 0($" ++ stringEscape b ++ ")\nseq $" ++ stringEscape a ++ ", $s0, $" ++ stringEscape c ++ "\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnName = "lw $s0, 0($" ++ stringEscape b ++ ")\n" ++ "lw $s1, 0($" ++ stringEscape c ++ ")\nseq $" ++ stringEscape a ++ ", $s0, $s1\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnVal && valOrNameTableLookup (Name c) t == AsnName = "lw $s1, 0($" ++ stringEscape c ++ ")\nseq $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $s1\n" ++ processTAC xs t
	| otherwise = "seq $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $" ++ stringEscape c ++ "\n" ++ processTAC xs t
processTAC ((Dif (Name a) (Name b) (Name c)):xs) t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnVal = "lw $s0, 0($" ++ stringEscape b ++ ")\nseq $" ++ stringEscape a ++ ", $s0, $" ++ stringEscape c ++ "\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnName = "lw $s0, 0($" ++ stringEscape b ++ ")\n" ++ "lw $s1, 0($" ++ stringEscape c ++ ")\nseq $" ++ stringEscape a ++ ", $s0, $s1\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnVal && valOrNameTableLookup (Name c) t == AsnName = "lw $s1, 0($" ++ stringEscape c ++ ")\nseq $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $s1\n" ++ processTAC xs t
	| otherwise = "seq $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $" ++ stringEscape c ++ "\n" ++ processTAC xs t
processTAC ((Label (Name l)):xs) t = stringEscape l ++ ":\n" ++ processTAC xs t
processTAC ((Goto (Name a)):xs) t = "j " ++ stringEscape a ++ "\n" ++ processTAC xs t
processTAC ((If_False (Name a) (Goto (Name b))):xs) t = "beq $" ++ stringEscape a ++ ", $zero, " ++ stringEscape b ++ "\n" ++ processTAC xs t
processTAC ((Add (Name a) (Name b) (Name c)):xs) t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnVal = "lw $s0, 0($" ++ stringEscape b ++ ")\nadd $" ++ stringEscape a ++ ", $s0, $" ++ stringEscape c ++ "\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnName = "lw $s0, 0($" ++ stringEscape b ++ ")\n" ++ "lw $s1, 0($" ++ stringEscape c ++ ")\nadd $" ++ stringEscape a ++ ", $s0, $s1\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnVal && valOrNameTableLookup (Name c) t == AsnName = "lw $s1, 0($" ++ stringEscape c ++ ")\nadd $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $s1\n" ++ processTAC xs t
	| otherwise = "add $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $" ++ stringEscape c ++ "\n" ++ processTAC xs t
processTAC ((Sub (Name a) (Name b) (Name c)):xs) t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnVal = "lw $s0, 0($" ++ stringEscape b ++ ")\nsub $" ++ stringEscape a ++ ", $s0, $" ++ stringEscape c ++ "\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnName = "lw $s0, 0($" ++ stringEscape b ++ ")\n" ++ "lw $s1, 0($" ++ stringEscape c ++ ")\nsub $" ++ stringEscape a ++ ", $s0, $s1\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnVal && valOrNameTableLookup (Name c) t == AsnName = "lw $s1, 0($" ++ stringEscape c ++ ")\nsub $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $s1\n" ++ processTAC xs t
	| otherwise = "sub $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $" ++ stringEscape c ++ "\n" ++ processTAC xs t
processTAC ((Mul (Name a) (Name b) (Name c)):xs) t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnVal = "lw $s0, 0($" ++ stringEscape b ++ ")\nmulo $" ++ stringEscape a ++ ", $s0, $" ++ stringEscape c ++ "\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnName = "lw $s0, 0($" ++ stringEscape b ++ ")\n" ++ "lw $s1, 0($" ++ stringEscape c ++ ")\nmulo $" ++ stringEscape a ++ ", $s0, $s1\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnVal && valOrNameTableLookup (Name c) t == AsnName = "lw $s1, 0($" ++ stringEscape c ++ ")\nmulo $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $s1\n" ++ processTAC xs t
	| otherwise = "mulo $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $" ++ stringEscape c ++ "\n" ++ processTAC xs t
processTAC ((Divv (Name a) (Name b) (Name c)):xs) t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnVal = "lw $s0, 0($" ++ stringEscape b ++ ")\ndiv $" ++ stringEscape a ++ ", $s0, $" ++ stringEscape c ++ "\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnName && valOrNameTableLookup (Name c) t == AsnName = "lw $s0, 0($" ++ stringEscape b ++ ")\n" ++ "lw $s1, 0($" ++ stringEscape c ++ ")\ndiv $" ++ stringEscape a ++ ", $s0, $s1\n" ++ processTAC xs t
	| valOrNameTableLookup (Name b) t == AsnVal && valOrNameTableLookup (Name c) t == AsnName = "lw $s1, 0($" ++ stringEscape c ++ ")\ndiv $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $s1\n" ++ processTAC xs t
	| otherwise = "div $" ++ stringEscape a ++ ", $" ++ stringEscape b ++ ", $" ++ stringEscape c ++ "\n" ++ processTAC xs t

main = do
	s <- getContents
	putStrLn "--------"
	putStrLn "Abstract tree"
	putStrLn "--------"
	print $ parse $ scanTokens s
	putStrLn "--------"
	putStrLn "Symbol table"
	putStrLn "--------"
	print (vars (removeMain $ parse $ scanTokens s) emptyMap)
	putStrLn "--------"
	putStrLn "Three Address Code"
	putStrLn "--------"
	print $ compile_cmd (removeMain $ parse $ scanTokens s) 0
	putStrLn "--------"
	putStrLn "MIPS Code (outputting to out.s)"
	putStrLn "--------"
	putStr $ symToData (vars (removeMain $ parse $ scanTokens s) emptyMap)
	putStr ".text\n"
	putStr $ (processTAC (getCode (compile_cmd (removeMain $ parse $ scanTokens s) 0)) (valOrNameTable (getCode (compile_cmd (removeMain $ parse $ scanTokens s) 0))))
	writeFile "out.s" (symToData (vars (removeMain $ parse $ scanTokens s) emptyMap) ++ ".text\n" ++ (processTAC (getCode (compile_cmd (removeMain $ parse $ scanTokens s) 0)) (valOrNameTable (getCode (compile_cmd (removeMain $ parse $ scanTokens s) 0)))))