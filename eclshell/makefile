# ecl_root -- dir where the host-system's native ECL is installed,
# including `cmp' module
sdk_ver = 4.1
ecl_root = /opt/ios/ecl-10.4.1-sdk4.1
host_ecl = $(ecl_root)/host

module = eclffi
sources = "$(module).lisp"

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
	$(host_ecl)/bin/ecl -load build.lisp -shell $$f; \
	rm -f $$f

clean:
	rm -f $(module).data $(module).[cho] lib$(module)*.a
