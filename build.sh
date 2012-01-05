#!/bin/sh
##
## script for building statically linked ecl libraries for the iPhone
## simulator and device. The universal option will build fat libs from
## the simulator and device builds.
##
install_root=
target=simulator
iphone_sdk_ver=4.3
clean=no
build_debug=no
enable_threads=yes
devtools_root=/Developer
host_config_opts="\
	--disable-longdouble \
	--enable-unicode \
	--enable-threads=$enable_threads \
	--with-asdf=yes \
	--with-bytecmp=builtin \
	--with-cmp=builtin \
	--with-clx=no \
	--with-debug-cflags=no \
	--with-defsystem=no \
	--with-fpe=yes \
	--with-profile=no \
	--with-rt=no \
	--with-serve-event=yes \
	--with-tcp=yes \
	--with-x=no \
	--with-dffi=no"
base_config_opts="\
	--disable-c99complex \
	--disable-longdouble \
	--disable-soname \
    	--disable-shared \
	--enable-unicode \
	--enable-threads=$enable_threads \
	--with-asdf=no \
	--with-bytecmp=builtin \
	--with-cmp=no \
	--with-clx=no \
	--with-debug-cflags=no \
	--with-defsystem=no \
	--with-fpe=yes \
	--with-profile=no \
	--with-rt=no \
	--with-serve-event=builtin \
	--with-tcp=builtin \
	--with-x=no \
	--with-dffi=no"
CC_bin=gcc
LD_bin=gcc

configure()
{
    prefix=$1
    config_opts=$2
    echo ./configure --prefix=$prefix $config_opts
    if [ ! -f Makefile ]; then
	./configure --prefix=$prefix $config_opts
    fi
}

build()
{
    prefix=$1
    cd build
    make < /dev/null
    make || exit 1
    make install || exit 1
    cp -p *.a $prefix/lib
    cd ..
}

distclean()
{
    make distclean || echo "Nothing to clean."
    rm -f Makefile
}

set_build_dir()
{
    if [ "$build_debug" = "yes" ]; then
	build_dir=$1
	[ -d $build_dir ] || mkdir -p $build_dir
	rm -f build
	ln -s $build_dir build
    else
	[ -d build ] || mkdir -p build
    fi
}

ensure_build_dir()
{
    build_dir=$1
    set_build_dir $build_dir
    if [ "$clean" = "yes" ]; then
	distclean
	set_build_dir $build_dir
    fi
}

host()
{
    ensure_build_dir build.host
    export CC="$SDK/usr/bin/$CC_bin"
    export CFLAGS="-g -m32"
    export LD="$SDK/usr/bin/$LD_bin"
    export LDFLAGS="-m32"
    configure $install_root/host "${host_config_opts}"
    build $install_root/host
    chmod +x $install_root/host/lib/ecl*/dpp \
    	$install_root/host/lib/ecl*/ecl_min
}

simulator()
{
    ensure_build_dir build.simulator
    export SDK=${devtools_root}/Platforms/iPhoneSimulator.platform/Developer
    export SDKROOT=$SDK/SDKs/iPhoneSimulator${iphone_sdk_ver}.sdk
    export CC="$SDK/usr/bin/$CC_bin"
    int_sdk_ver=$(echo "(${iphone_sdk_ver} * 100)/1"|bc)
    export CFLAGS=$(echo -g -arch i386 -I$SDKROOT/usr/include \
	-fmessage-length=0 \
	-pipe \
	-std=c99 \
	-Wno-trigraphs \
	-fpascal-strings \
	-fasm-blocks \
	-O0 \
	-Wreturn-type \
	-Wunused-variable \
    	-isysroot $SDKROOT \
	-fexceptions \
	-fvisibility=hidden \
	-mmacosx-version-min=10.6 \
	-gdwarf-2 \
	-fobjc-abi-version=2 \
	-D__IPHONE_OS_VERSION_MIN_REQUIRED=30000 \
	-D_DARWIN_USE_64_BIT_INODE \
	-DAPPLE -DIPHONE -DIPHONE_SIMULATOR \
	-DIPHONE_SDK_VER=${int_sdk_ver})
    export LD="$SDK/usr/bin/$LD_bin"
    export LDFLAGS=$(echo -arch i386 \
    	-isysroot $SDKROOT -mmacosx-version-min=10.6 \
	-Xlinker -objc_abi_version -Xlinker 2)
    # the following two definitions are required to force the
    # simulator config.h to match the device config.h
    export ac_cv_header_ffi_ffi_h="no"
    export dynamic_ffi="no"
    configure $install_root/simulator "${base_config_opts}"
    {
	echo "#define HAVE_NATIVE_mpn_add_n 1";
	echo "#define HAVE_NATIVE_mpn_sub_n 1";
    } >> build/gmp/config.h
    orig_DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH
    DYLD_LIBRARY_PATH=$SDKROOT/usr/lib:$SDKROOT/usr/lib/system
    export DYLD_LIBRARY_PATH
    build $install_root/simulator
    DYLD_LIBRARY_PATH=${orig_DYLD_LIBRARY_PATH}
    chmod +x $install_root/simulator/lib/ecl*/dpp \
    	$install_root/simulator/lib/ecl*/ecl_min
}

cross_config()
{
    ecl_to_run=$1
    echo "###
### YOU ARE TRYING TO CROSS COMPILE ECL.
### PLEASE FOLLOW THESE INSTRUCTIONS:
###
### 1) Vital information cannot be determined at configuration time
### because we are not able to run test programs. A file called
###		
### has been created, that you will have to fill out. Please do
### it before invoking \"configure\" again.

### 1.1) Direction of growth of the stack
ECL_STACK_DIR=up
### 1.2) Choose an integer datatype which is large enough to host a pointer
CL_FIXNUM_TYPE=int
CL_FIXNUM_BITS=32
CL_FIXNUM_MAX=536870911L
CL_FIXNUM_MIN=-536870912L
CL_INT_BITS=32
CL_LONG_BITS=32

### 1.3) Order of bytes within a word
ECL_BIGENDIAN=no

### 1.4) What characters signal an end of line. May be LF (Linefeed)
###      CR (Carriage return), and CRLF (CR followed by LF).
ECL_NEWLINE=LF

### 1.5) Can we guess how many characters are available for reading from
###      the FILE structure?
###          0 = no
###          1 = (f)->_IO_read_end - (f)->_IO_read_ptr
###          2 = (f)->_r
###          3 = (f)->_cnt
ECL_FILE_CNT=0

### 1.6) How many bits constitute a long long?
ECL_LONG_LONG_BITS=64
ECL_STDINT_HEADER=\"#include <stdint.h>\"
ECL_UINT8_T=uint8_t
ECL_UINT16_T=uint16_t
ECL_UINT32_T=uint32_t
ECL_UINT64_T=no
ECL_INT8_T=int8_t
ECL_INT16_T=int16_t
ECL_INT32_T=int32_t
ECL_INT64_T=no

ECL_WORKING_SEM_INIT=yes
ECL_WORKING_ENVIRON=yes

### 2) To cross-compile ECL so that it runs on the system
###		arm-apple-darwin
### you need to first compile ECL on the system in which you are building
### the cross-compiled files, that is
###		i686-apple-darwin9.6.0
### By default we assume that ECL can be accessed from some directory in
### the path.
ECL_TO_RUN=${ecl_to_run}
"
}

device()
{
    arch=$1
    ensure_build_dir build.$arch
    prefix=$install_root/${arch}
    ecl_root=$install_root/host
    export SDK=${devtools_root}/Platforms/iPhoneOS.platform/Developer
    export SDKROOT=$SDK/SDKs/iPhoneOS${iphone_sdk_ver}.sdk
    export CC="$SDK/usr/bin/$CC_bin"
    export CFLAGS=$(echo -g -arch ${arch} -I$SDKROOT/usr/include \
    	-isysroot $SDKROOT -DAPPLE -DIPHONE)
    export CPP="$SDK/usr/bin/cpp"
    export LD="$SDK/usr/bin/$LD_bin"
    export LDFLAGS="-arch ${arch} -isysroot $SDKROOT"
    cross_config "$ecl_root/bin/ecl" > build/cross_config
    configure $prefix "${base_config_opts} --host=arm-apple-darwin"
    build $prefix
}

lipo()
{
    arm6lib=$1
    arm7lib=$2
    i386lib=$3
    lipolib=$4
    export SDK=${devtools_root}/Platforms/iPhoneOS.platform/Developer
    $SDK/usr/bin/lipo -arch armv6 $arm6lib -arch armv7 $arm7lib -arch i386 $i386lib -create -output $lipolib
}

universal()
{
    prefix=$install_root
    mkdir -p $prefix/universal/lib
    for lib in bytecmp ecl eclgc eclgmp serve-event sockets; do
	rm -f $prefix/universal/lib/lib${lib}.a
	lipo \
	    $prefix/armv6/lib/lib${lib}.a \
	    $prefix/armv7/lib/lib${lib}.a \
	    $prefix/simulator/lib/lib${lib}.a \
	    $prefix/universal/lib/lib${lib}.a
    done
    (cd $prefix/universal; ln -fs ../armv6/include .)
    (cd $prefix; mkdir -p etc; cd etc; rm -f ucd.dat; ln -s ../host/lib/ecl-*/ucd.dat .)
}

usage()
{
    echo "Usage: `basename $0` [-a arch] [-d <dir>] [-t <target>] [-c] [-v <sdk-ver>] [-r <dir>]"
    echo ""
    echo " -a arch     -- one of: armv6, armv7, when target == device"
    echo " -c          -- force a 'make distclean' before building target"
    echo " -d dir      -- prefix directory where ecl will be installed"
    echo " -r dir      -- the install root of the developer tools [$devtools_root]"
    echo " -t target   -- one of: host, simulator, device [$target]"
    echo " -v sdk-ver  -- the sdk version to use [$iphone_sdk_ver]"
    echo ""
}

while getopts 'a:d:v:t:r:cx' o; do
case "$o" in
    a) arch="$OPTARG";;
    d) install_root="$OPTARG";;
    v) iphone_sdk_ver="$OPTARG";;
    t) target="$OPTARG";;
    c) clean=yes;;
    x) build_debug=yes;;
    r) devtools_root="$OPTARG";;
    ?) usage
    esac
done
shift $(($OPTIND - 1))

if [ "$install_root" = "" ]; then
    usage
    exit 1
else
    echo "Installing in $install_root"
    [ -d $install_root ] || mkdir -p $install_root
fi

case "$target" in
    device)
	if [ "" = "${arch}" ]; then
	    echo "Please specify CPU architecture (armv6, armv7)."
	    exit 1
	fi
	device $arch;;
    host|simulator|universal)
	$target;;
    all)
	clean="yes"
	host || exit 1
	simulator || exit 1
	device armv6 || exit 1
	device armv7 || exit 1
	universal;;
    *)
	echo "Unknown target $target";
	usage;
	exit 1;;
esac
