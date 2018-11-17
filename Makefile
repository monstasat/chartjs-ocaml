BINDINGS 		= chartjs_types.mli \
							chartjs_scales.mli \
							chartjs_options.mli \
							chart.mli
SOURCES 		= axes.ml chartjs.ml

LIB_NAME 		= chartjs

CC					= ocamlc
PACKAGES		= -package gen_js_api -package js_of_ocaml

ML_FILE			= $(patsubst %.mli, %.ml, $(BINDINGS)) $(SOURCES)
CMI_FILE		= $(patsubst %.ml, %.cmi, $(ML_FILE))
CMO_FILE		= $(patsubst %.ml, %.cmo, $(ML_FILE))
CMA_FILE		= $(LIB_NAME).cma
INSTALL			= \
	META \
	chartjs.cmi \
	chartjs_types.cmi \
	$(CMA_FILE) \

%.ml: %.mli
	ocamlfind gen_js_api/gen_js_api $<
	ocamlfind $(CC) -c $(PACKAGES) $<

%.cmi: %.mli
	ocamlfind $(CC) -c $(PACKAGES) $<

%.cmi: %.ml
	ocamlfind $(CC) -c $(PACKAGES) $<

%.cmo: %.ml
	ocamlfind $(CC) -c $(PACKAGES) $<

build: $(ML_FILE) $(CMI_FILE) $(CMO_FILE)
	ocamlfind $(CC) -a $(CMO_FILE) -o $(CMA_FILE) -no-check-prims -package gen_js_api -linkpkg

install: build
	ocamlfind install $(LIB_NAME) $(INSTALL)

remove:
	ocamlfind remove $(LIB_NAME)

clean:
	rm -f $(BINDINGS:.mli=.ml) *.cm*

re: clean all
