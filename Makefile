all: Syntax

Tokens.hs: Tokens.x
	alex Tokens.x

Grammar.hs: Grammar.y
	happy Grammar.y

Syntax: Tokens.hs Grammar.hs Syntax.hs
	ghc --make Syntax 

clean:
	rm -f Calc Grammar.hs Tokens.hs *.o *.hi