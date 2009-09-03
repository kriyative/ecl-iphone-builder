# ecl_root -- dir where the host-system's native ECL is installed,
# including `cmp' module
ecl_root = /opt/ecl
ecl = $(ecl_root)/bin/ecl

libeclffi.a: eclffi.lisp
	DYLD_LIBRARY_PATH=$(ecl_root)/lib $(ecl) -shell build.lisp

clean:
	rm -f eclffi.data eclffi.[cho] libeclffi*.a
