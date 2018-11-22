LIB_NAME 		= chartjs

CC					= ocamlc
PACKAGES		= -package gen_js_api.ppx \
							-package js_of_ocaml \
							-package js_of_ocaml-ppx \
							-package ptime

BINDINGS    = chartjs_types.ml chartjs_scales_raw.ml chartjs_data.ml chartjs_options.ml \
							chart.ml line.ml bar.ml pie.ml

ML_FILE			= chartjs_types.ml chartjs_array.ml chartjs_scales_raw.ml chartjs_data.ml \
							chartjs_scales.ml chartjs_options.ml chartjs_option_types.ml chart.ml \
							line.ml bar.ml pie.ml chartjs.ml
CMI_FILE		= $(patsubst %.ml, %.cmi, $(ML_FILE))
CMO_FILE		= $(patsubst %.ml, %.cmo, $(ML_FILE))
CMA_FILE		= $(LIB_NAME).cma
INSTALL			= \
	META \
	chartjs.cmi \
	chartjs_types.cmi \
	chartjs_option_types.cmi \
	chartjs_array.cmi \
	chartjs_data.cmi \
	chartjs_scales.cmi \
	$(CMA_FILE) \

all: build

$(BINDINGS): %.ml : %.mli
	ocamlfind gen_js_api/gen_js_api $<

%.cmi: %.mli
	ocamlfind $(CC) -c $(PACKAGES) $<

%.cmi: %.ml
	ocamlfind $(CC) -c $(PACKAGES) $<

%.cmo: %.ml
	ocamlfind $(CC) -c $(PACKAGES) $<

build: $(CMI_FILE) $(CMO_FILE)
	ocamlfind $(CC) -a -o $(CMA_FILE) -package gen_js_api -linkpkg $(CMO_FILE)

install: build
	ocamlfind install $(LIB_NAME) $(INSTALL)

remove:
	ocamlfind remove $(LIB_NAME)

clean:
	rm -f  *.cm* $(BINDINGS)

re: clean all

reinstall: remove install
