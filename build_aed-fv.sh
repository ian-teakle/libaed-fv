#!/bin/bash -e

export EXTERNAL_LIBS=shared
export SINGLE=false
export PRECISION=1
export DEBUG=false

while [ $# -gt 0 ] ; do
  case $1 in
    --debug)
      export DEBUG=true
      ;;
    --fence)
      export FENCE=true
      ;;
    --single)
      export SINGLE=true
      ;;
    --no-ben)
      export NO_BEN=true
      ;;
    --no-demo)
      export NO_DEMO=true
      ;;
    *)
      ;;
  esac
  shift
done

# The default is MacPorts : uncomment the next line if you use "homebrew"
#export HOMEBREW=true

export CURDIR=`pwd`

export DAEDWATDIR=${CURDIR}/../libaed-water
export DAEDBENDIR=${CURDIR}/../libaed-benthic
export DAEDDMODIR=${CURDIR}/../libaed-demo

if [ ! -d ${DAEDDMODIR} ] ; then
  export NO_DEMO=true
fi
if [ ! -d ${DAEDBENDIR} ] ; then
  export NO_BEN=true
fi

if [ "$FC" = "" ] ; then
  export FC=ifort
fi

if [ "$FC" = "ifort" ] ; then
  if [ -d /opt/intel/bin ] ; then
    . /opt/intel/bin/compilervars.sh intel64
  fi
  which ifort >& /dev/null
  if [ $? != 0 ] ; then
    echo ifort compiler requested, but not found
    exit 1
  fi
fi

export F77=$FC
export F90=$FC
export F95=$FC

cd ${DAEDWATDIR}
echo making in ${DAEDWATDIR}
make || exit 1
export PARAMS=""
if [ "${NO_BEN}" != "true" ] ; then
   cd ${DAEDBENDIR}
   echo making in ${DAEDBENDIR}
   make || exit 1
   export PARAMS="${PARAMS} AEDBENDIR=${DAEDBENDIR}"
fi
if [ "${NO_DEMO}" != "true" ] ; then
   cd ${DAEDDMODIR}
   echo making in ${DAEDDMODIR}
   make || exit 1
   export PARAMS="${PARAMS} AEDDMODIR=${DAEDDMODIR}"
fi
cd ${CURDIR}
#make distclean
echo make ${PARAMS}
make ${PARAMS} || exit 1

exit 0
