SOURCES = syntax.ml w4lexer.mll w4parser.mly w4interpreter.ml w4.ml
RESULT = w4
TRASH = *~
CFLAGS = -g
## menhirを使用する場合は以下をアンコメントする
# OCAMLYACC=menhir 

## .output（menhirを使った場合は.automatonと.conflicts）
## を生成する場合は以下をアンコメント
# YFLAGS = -v 

all: byte-code 

-include OCamlMakefile
