OCAMLMAKEFILE = OCamlMakefile

BINDING=poclow_binding.ml

#INCDIRS=../lua-ml
#LIBS=xml-light lua-std

PACKS=str poclow unix extlib lua-ml xml-light unix

LIBINSTALL_FILES=*.cmi *.cmx *.a poccore.cmxa
#LIB_PACK_NAME=poccore

SOURCES = core_generic.ml core_rect.ml core_file.ml core_timer.ml olua.ml oxml.ml oval.ml core_cache.ml core_event.ml core_font.ml core_drawing.ml binding.ml core_video.ml core_medias.ml core_graphic.ml core_anim.ml core_cursor.ml core_type.ml core_stage.ml dijkstra.ml pathfinding.ml core_action.ml core_xml.ml core_main.ml

RESULT  = poccore

#THREADS=yes

# PREDS="str unix xml-light extLib"
 
all : binding.ml ncl

binding.ml:
	cp $(BINDING) binding.ml


include $(OCAMLMAKEFILE)