# ecl_root -- dir where the host-system's native ECL is installed,
# including `cmp' module
sdk_ver = 4.2
ecl_root = /opt/ecl-ios
host_ecl = $(ecl_root)/host

module = eclffi
sources = "util.lisp" "$(module).lisp"

# Note: the trailing slash on the :ecl-root keyword arg to `build-all'
# is required for Lisp pathnames to work right
lib$(module).a: $(module).lisp
	f=`mktemp -t build`; \
	echo '(build-all "$(module)" \
                         (list $(sources)) \
                         :sdk-ver "$(sdk_ver)" \
                         :ecl-root "$(ecl_root)/")' > $$f; \
	cat $$f; \
	DYLD_LIBRARY_PATH=$(host_ecl)/lib \
	$(host_ecl)/bin/ecl \
		-load util.lisp \
		-load build.lisp \
		-shell $$f; \
	rm -f $$f

clean:
	rm -f *.data $(module).[cho] util.[cho] lib$(module)*.a
