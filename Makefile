SOURCES = util.ml syntax.ml parser.mly lexer.mll \
          eval_direct.ml zam_list.ml zam_array.ml \
          zam_fast1.ml zam_pure.ml zam_pure2.ml main.ml
RESULT  = minicaml
LIBS = unix

include OCamlMakefile

