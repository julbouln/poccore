OCAMLMAKEFILE = OCamlMakefile

BINDING=poclow_binding.ml

INCDIRS=../xml-light2.1 ../lua-ml
LIBS=xml-light str unix lua-std

PACKS=extlib poclow

SOURCES = core_generic.ml core_rect.ml core_file.ml core_timer.ml olua.ml oxml.ml oval.ml core_cache.ml core_event.ml core_font.ml core_drawing.ml binding.ml core_video.ml core_medias.ml core_graphic.ml core_anim.ml core_cursor.ml core_type.ml core_stage.ml dijkstra.ml pathfinding.ml core_action.ml core_xml.ml core_main.ml

#OCAMLOPT=ocamlopt.opt

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