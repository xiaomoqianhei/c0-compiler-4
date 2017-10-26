{
module Grammar where
import Tokens
import Data.Char
}

%name parse
%tokentype {Token}
%error {parseError}

%token

int {TokenIntKeyword}
bool {TokenBoolKeyword}
if {TokenIfKeyword}
else {TokenElseKeyword}
while {TokenWhileKeyword}
true {TokenTrue}
false {TokenFalse}
var {TokenSym $$}
'+'	{TokenPlus}
'-'	{TokenMinus}
num	{TokenInt $$}
'*'	{TokenTimes}
'/'	{TokenDiv}
'%' {TokenMod}
'=' {TokenAt}
'('	{TokenOPar}
')'	{TokenCPar}
'!' {TokenNeg}
'||' {TokenOr}
'&&' {TokenAnd}
'!=' {TokenDif}
'==' {TokenEq}
'>' {TokenGrt}
'<' {TokenLess}
'>=' {TokenGrtEq}
'<=' {TokenLessEq}
';' {TokenSeparator}
'{' {TokenOpen}
'}' {TokenClose}
'NULL' {TokenNullObject}
main {TokenMain}

%nonassoc '!=' '==' '>' '<' '>=' '<=' '!'
%left '||'
%left '&&'
%left '+' '-'
%left '*' '/' '%'
%left NEG

%%
Prog : main '(' ')' '{' Cmd '}' {Main $5}

Cmd : if '(' Exp ')' Cmd {If $3 $5}
	| if '(' Exp ')' Cmd else Cmd {IfElse $3 $5 $7}
	| while '(' Exp ')' Cmd {While $3 $5}
	| '{' Cmd ';' Cmd '}' {Seq $2 $4}
	| '{' Cmd ';' '}' {$2}
	| Cmd ';' {$1}
	| Cmd ';' Cmd {Seq $1 $3}
	| var '=' Exp {Attrib $1 $3}
	| int var {DeclareIntVar $2}
	| int var '=' Exp {Seq (DeclareIntVar $2) (Attrib $2 $4)}
	| bool var {DeclareBoolVar $2}
	| bool var '=' Exp {Seq (DeclareBoolVar $2) (Attrib $2 $4)}

Exp	: Exp '+' Exp	{Plus $1 $3}
	| Exp '-' Exp	{Minus $1 $3}
	| Exp '*' Exp	{Times $1 $3}
	| Exp '/' Exp	{Div $1 $3}
	| Exp '%' Exp	{Mod $1 $3}
	| '(' Exp ')'	{Bracketed $2}
	| '-' Exp %prec NEG {Negative $2}
	| num 	{Num $1}
	| var 	{Var $1}
	| Exp '>' Exp	{Greater $1 $3}
	| Exp '<' Exp {Lesser $1 $3}
	| Exp '>=' Exp {GreaterEqual $1 $3}
	| Exp '<=' Exp {LesserEqual $1 $3}
	| '!' Exp {Negate $2}
	| Exp '||' Exp {Or $1 $3}
	| Exp '&&' Exp {And $1 $3}
	| Exp '==' Exp {Equals $1 $3}
	| Exp '!=' Exp {Different $1 $3}
	| true {TrueBool}
	| false {FalseBool}
	| 'NULL' {NullObject}

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Prog = Main Cmd
	deriving (Show)

data Exp = Num Int
	| Plus Exp Exp
	| Minus Exp Exp
	| Times Exp Exp
	| Div Exp Exp
	| Mod Exp Exp
	| Negative Exp
	| Bracketed Exp
	| Var String
	| TrueBool
	| FalseBool
	| Or Exp Exp
	| And Exp Exp
	| Greater Exp Exp
	| Lesser Exp Exp
	| GreaterEqual Exp Exp
	| LesserEqual Exp Exp
	| Negate Exp
	| Equals Exp Exp
	| Different Exp Exp
	| NullObject
	deriving (Show)

data Cmd = If Exp Cmd
	| IfElse Exp Cmd Cmd
	| While Exp Cmd
	| Seq Cmd Cmd
	| DeclareBoolVar String
	| DeclareIntVar String
	| Attrib String Exp
	deriving (Show)

data Type = Int | Bool deriving (Eq, Ord, Show)
}