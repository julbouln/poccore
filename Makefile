OCAMLMAKEFILE = OCamlMakefile

BINDING=poclow_binding.ml

INCDIRS=../poclow ../extlib-1.3 ../xml-light2.1 ../lua-ml
LIBS=poclow xml-light extLib str unix lua-std

SOURCES = locales/locale.ml locales/fr.ml generic.ml rect.ml file.ml config.ml audio.ml timer.ml olua.ml oxml.ml oval.ml cache.ml event.ml font.ml drawing.ml binding.ml video.ml medias.ml graphic.ml music.ml anim.ml cursors.ml otype.ml properties.ml tree.ml stage.ml dijkstra.ml pathfinding.ml action.ml core_xml.ml main.ml

OCAMLOPT=ocamlopt.opt

RESULT  = poccore

#THREADS=yes

# PREDS="str unix xml-light extLib"
 
all : binding.ml ncl

binding.ml:
	cp $(BINDING) binding.ml



#VERSION=0.12
#game_version.ml: Makefile
#	echo "let version = \""$(VERSION)"\"" > game_version.ml
#	echo "let date = \""`date`"\"" >> game_version.ml



include $(OCAMLMAKEFILE)