SOURCES = syntax.ml w5lexer.mll w5parser.mly w5interpreter.ml w5printer.ml w5.ml
RESULT = w5
TRASH = *~ *.output
OCAMLFLAGS = "-g"
## menhirを使用する場合は以下をアンコメントする
# OCAMLYACC=menhir 

## .output（menhirを使った場合は.automatonと.conflicts）
## を生成する場合は以下をアンコメント
 YFLAGS = -v 

all: byte-code 

-include OCamlMakefile
