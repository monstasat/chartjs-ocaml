LIB_NAME 		= chartjs

CC					= ocamlc
PACKAGES		= -package gen_js_api -package js_of_ocaml -package js_of_ocaml-ppx

ML_FILE			= chartjs_types.ml chartjs_array.ml chartjs_scales.ml chartjs_options.ml \
							chart.ml line.ml pie.ml axes.ml chartjs.ml
CMI_FILE		= $(patsubst %.ml, %.cmi, $(ML_FILE))
CMO_FILE		= $(patsubst %.ml, %.cmo, $(ML_FILE))
CMA_FILE		= $(LIB_NAME).cma
INSTALL			= \
	META \
	chartjs.cmi \
	chartjs_types.cmi \
	chartjs_array.cmi \
	$(CMA_FILE) \

%.ml: %.mli
	ocamlfind gen_js_api/gen_js_api $<

%.cmi: %.mli
	ocamlfind $(CC) -c $(PACKAGES) $<

%.cmi: %.ml
	ocamlfind $(CC) -c $(PACKAGES) $<

%.cmo: %.ml
	ocamlfind $(CC) -c $(PACKAGES) $<

build: $(ML_FILE) $(CMI_FILE) $(CMO_FILE)
	ocamlfind $(CC) -a -o $(CMA_FILE) -package gen_js_api -linkpkg $(CMO_FILE)

install: build
	ocamlfind install $(LIB_NAME) $(INSTALL)

remove:
	ocamlfind remove $(LIB_NAME)

clean:
	rm -f  *.cm* chartjs_types.ml chartjs_scales.ml chartjs_options.ml chart.ml pie.ml line.ml

re: clean all
