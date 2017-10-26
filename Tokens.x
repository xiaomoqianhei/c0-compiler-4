{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
@number = $digit+
@whitespace = $white+
$minus = \-

tokens :-
	@whitespace	; --skip whitespaces
	@number	{\s -> TokenInt (read s)} --1 digit or more consecutively
	\+	{\s -> TokenPlus}
	\-	{\s -> TokenMinus}
	\/	{\s -> TokenDiv}
	\*	{\s -> TokenTimes}
	\%	{\s -> TokenMod}
	\=	{\s -> TokenAt}
	\(	{\s -> TokenOPar}
	\)	{\s -> TokenCPar}
	\{ 	{\s -> TokenOpen}
	\}	{\s -> TokenClose}
	int	{\s -> TokenIntKeyword}
	bool {\s -> TokenBoolKeyword}
	if {\s -> TokenIfKeyword}
	else {\s -> TokenElseKeyword}
	while {\s -> TokenWhileKeyword}
	true {\s -> TokenTrue}
	false {\s -> TokenFalse}
	NULL {\s -> TokenNullObject}
	\! {\s -> TokenNeg}
	"||" {\s -> TokenOr}
	"&&" {\s -> TokenAnd}
	\> {\s -> TokenGrt}
	\< {\s -> TokenLess}
	"!=" {\s -> TokenDif}
	"==" {\s -> TokenEq}
	">=" {\s -> TokenGrtEq}
	"<=" {\s -> TokenLessEq}
	main {\s -> TokenMain}
	";" {\s -> TokenSeparator}
  	$alpha [$alpha $digit \_]*	{\s -> TokenSym s}
	
{
data Token
	= TokenInt Int
	| TokenPlus
	| TokenMinus
	| TokenTimes
	| TokenDiv
	| TokenMod
	| TokenIntKeyword
	| TokenSym String
	| TokenAt
	| TokenOPar
	| TokenCPar
	| TokenOpen
	| TokenClose
	| TokenBoolKeyword
	| TokenMain
	| TokenTrue
	| TokenFalse
	| TokenNeg
	| TokenOr
	| TokenAnd
	| TokenGrt
	| TokenLess
	| TokenDif
	| TokenEq
	| TokenGrtEq
	| TokenLessEq
	| TokenIfKeyword
	| TokenElseKeyword
	| TokenWhileKeyword
	| TokenNullObject
	| TokenSeparator
	deriving (Show)

scanTokens = alexScanTokens
}