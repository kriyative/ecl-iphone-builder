#!/bin/sh
##
## script for building statically linked ecl libraries for the iPhone
## simulator and device. The universal option will build fat libs from
## the simulator and device builds.
##
install_root=/opt/iphone
iphone_sdk_ver=3.0
base_config_opts="--disable-c99complex \
	--disable-longdouble \
	--disable-soname \
	--disable-unicode \
    	--disable-shared \
	--enable-threads \
	--with-asdf=no \
	--with-bytecmp=builtin \
	--with-cmp=no \
	--with-clx=no \
	--with-defsystem=no \
	--with-fpe=yes \
	--with-profile=no \
	--with-rt=no \
	--with-serve-event=builtin \
	--with-tcp=builtin \
	--with-x=no \
	--with-dffi=no"

configure()
{
    prefix=$1
    config_opts=$2
    echo ./configure --prefix=$prefix $config_opts $base_config_opts
    if [ ! -f Makefile ]; then
	./configure --prefix=$prefix $config_opts $base_config_opts
    fi
}

build()
{
    prefix=$1
    make < /dev/null
    make || exit 1
    make install || exit 1
    cp -p build/*.a $prefix/lib
}

simulator()
{
    export SDK=/Developer/Platforms/iPhoneSimulator.platform/Developer
    export SDKROOT=$SDK/SDKs/iPhoneSimulator${iphone_sdk_ver}.sdk
    export CC="$SDK/usr/bin/gcc-4.2"
    export CFLAGS="-g -arch i386 -I$SDKROOT/usr/include -isysroot $SDKROOT -DAPPLE -DIPHONE -mmacosx-version-min=10.5"
    export LDFLAGS="-arch i386 -isysroot $SDKROOT -mmacosx-version-min=10.5"
    # the following two definitions are required to force the
    # simulator config.h to match the device config.h
    export ac_cv_header_ffi_ffi_h="no"
    export dynamic_ffi="no"
    configure $install_root/simulator
    build $install_root/simulator
    chmod +x $install_root/simulator/lib/ecl*/dpp $install_root/simulator/lib/ecl*/ecl_min
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

### 1.4) What characters signal an end of line. May be LF (Linefeed or \\n)
###      CR (Carriage return or \\r), and CRLF (CR followed by LF).
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
    prefix=$install_root/device
    mkdir build
    ecl_root=$install_root/simulator
    cross_config "$ecl_root/bin/ecl" > build/cross_config
    export SDK=/Developer/Platforms/iPhoneOS.platform/Developer
    export SDKROOT=$SDK/SDKs/iPhoneOS${iphone_sdk_ver}.sdk
    export CC="$SDK/usr/bin/gcc-4.2"
    export CFLAGS="-g -arch armv6 -I$SDKROOT/usr/include -isysroot $SDKROOT -DAPPLE -DIPHONE"
    export CPP="$SDK/usr/bin/cpp"
    export LDFLAGS="-arch armv6 -isysroot $SDKROOT"
    configure $prefix "--host=arm-apple-darwin --target=arm-apple-darwin"
    build $prefix
}

lipo()
{
    armlib=$1
    i386lib=$2
    lipolib=$3
    export SDK=/Developer/Platforms/iPhoneOS.platform/Developer
    $SDK/usr/bin/lipo -arch arm $armlib -arch i386 $i386lib -create -output $lipolib
}

universal()
{
    prefix=$install_root
    mkdir -p $prefix/universal/lib
    for lib in bytecmp ecl eclgc eclgmp serve-event sockets; do
	rm -f $prefix/universal/lib/lib${lib}.a
	lipo $prefix/device/lib/lib${lib}.a $prefix/simulator/lib/lib${lib}.a $prefix/universal/lib/lib${lib}.a
    done
    (cd $prefix/universal; ln -s ../device/include .)
}

optsp="1"
while [ "$optsp" == "1" ]; do
    case "$1" in
	"--install")
	    shift;
	    install_root=$1;
	    shift;;
	"--*")
	    echo "unknown option: $1";
	    shift;;
	*)
	    optsp=0;;
    esac
done

[ -d $install_root ] || mkdir -p $install_root
install_root=$(cd $install_root; pwd)
echo "Installing $1 in $install_root"

case "$1" in
    "")
	make distclean;
	simulator || exit 1;
	make distclean;
	device || exit 1;
	universal || exit 1;;
    simulator|device|universal) $1;;
    *) echo "usage: $0 {simulator|device|universal}"; exit 1;;
esac
