OCAMLMAKEFILE = OCamlMakefile

BINDING=poclow_binding.ml

PACKS=str poclow unix extlib lua-ml xml-light unix

LIBINSTALL_FILES=*.cmi *.cmo *.cmx *.a poccore.cma poccore.cmxa

SOURCES = ocommon.ml olua.ml oxml.ml oval.ml core_rect.ml core_file.ml core_timer.ml core_val.ml core_cache.ml core_event.ml core_font.ml core_drawing.ml binding.ml core_video.ml core_medias.ml core_graphic.ml core_anim.ml core_cursor.ml core_type.ml core_stage.ml core_action.ml core_sprite.ml core_xml.ml core_main.ml

RESULT  = poccore

OCAMLDOC=ocamlfind ocamldoc -package "$(PACKS)"
DOC_FILES=$(SOURCES)

all : binding.ml ncl bcl

binding.ml:
	cp $(BINDING) binding.ml


include $(OCAMLMAKEFILE)