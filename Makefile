OCAMLMAKEFILE = OCamlMakefile

INCDIRS=../poclow ../extlib-1.3 ../xml-light2.1 ../lua-ml
LIBS=poclow xml-light extLib str unix lua-std

SOURCES = locales/locale.ml locales/fr.ml generic.ml rect.ml file.ml config.ml video.ml audio.ml event_manager.ml resource.ml vfs.ml olua.ml oxml.ml cache.ml drawing.ml medias.ml music.ml anim.ml cursors.ml otype.ml properties.ml tree.ml stage.ml dijkstra.ml pathfinding.ml action.ml main.ml

OCAMLOPT=ocamlopt.opt

RESULT  = poccore

#THREADS=yes

# PREDS="str unix xml-light extLib"
 
all : ncl

#VERSION=0.12
#game_version.ml: Makefile
#	echo "let version = \""$(VERSION)"\"" > game_version.ml
#	echo "let date = \""`date`"\"" >> game_version.ml



include $(OCAMLMAKEFILE)