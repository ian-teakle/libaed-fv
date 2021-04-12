#
# Makefile to build the aed water quality library
# with hydrodynamic driver wrapper
#

VERS=3.0.0

objdir=obj
srcdir=src
libdir=lib
moddir=mod

OSTYPE=$(shell uname -s)

LIBAEDFV=aed-fv

ifeq ($(AEDWATDIR),)
  AEDWATDIR=../libaed-water
endif
INCLUDES+=-I${AEDWATDIR}/include
INCLUDES+=-I${AEDWATDIR}/mod

OUTLIB=libtuflowfv_external_wq

ifeq ($(OSTYPE),Darwin)
  SHARED=-dynamiclib -undefined dynamic_lookup
  so_ext=dylib
else
  SHARED=-shared
  so_ext=so
endif

ifeq ($(F90),ifort)
  INCLUDES+=-I/opt/intel/include
  DEBUG_FFLAGS=-g -traceback
  OPT_FFLAGS=-O3 -qopenmp
  FFLAGS=-fpp -warn all -module ${moddir} -static-intel -mp1 -warn nounused $(DEFINES)
  ifeq ($(WITH_CHECKS),true)
    FFLAGS+=-check
  endif
  FFLAGS+=-real-size 64
else ifeq ($(F90),flang)
  INCLUDES+=-I../flang_extra/mod
  DEBUG_FFLAGS=-g
  OPT_FFLAGS=-O3
  FFLAGS=-fPIC -module ${moddir} $(DEFINES) $(INCLUDES)
  ifeq ($(WITH_CHECKS),true)
    FFLAGS+=-Mbounds
  endif
  FFLAGS+=-r8
endif

FFLAGS+=-fPIC

LIBWATAED=aed-water
SOFLAGS = ${libdir}/lib${LIBAEDFV}.a ${AEDWATDIR}/lib/lib${LIBWATAED}.a

EXTFLAG=
ifneq ($(AEDBENDIR),)
  LIBBENAED=aed-benthic
  SOFLAGS+=${AEDBENDIR}/lib/lib${LIBBENAED}.a
endif
ifneq ($(AEDRIPDIR),)
  LIBRIPAED=aed-riparian
  SOFLAGS+=${AEDRIPDIR}/lib/lib${LIBRIPAED}.a
else
  EXTFLAG+=-DNO_RIPARIAN
endif
ifneq ($(AEDDMODIR),)
  LIBDMOAED=aed-demo
  SOFLAGS+=${AEDDMODIR}/lib/lib${LIBDMOAED}.a
endif
ifneq ($(AEDDEVDIR),)
  LIBDEVAED=aed-dev
  SOFLAGS+=${AEDDEVDIR}/lib/lib${LIBDEVAED}.a
else
  EXTFLAG+=-DNO_DEV
endif

FFLAGS+=$(OPT_FFLAGS)

ifeq ($(DEBUG),true)
  FFLAGS+=$(DEBUG_FFLAGS)
endif

ifeq ($(PRECISION),1)
  TFFLAGS += -D_PRECISION=1
else ifeq ($(PRECISION),2)
  TFFLAGS += -D_PRECISION=2
else
  TFFLAGS += -D_PRECISION=1
endif

TFFLAGS += -g -DAED -DEXTERNAL_WQ=2
INCLUDES += -I${moddir}


FVOBJECTS=${objdir}/fv_zones.o ${objdir}/fv_aed.o
OBJECTS=${objdir}/tuflowfv_external_wq_aed.o ${objdir}/aed_external.o

ifeq ($(EXTERNAL_LIBS),shared)
  TARGET = ${libdir}/$(OUTLIB).${so_ext}
else
  TARGET = ${libdir}/$(OUTLIB).a
endif
FFLAGS+=-Dtuflowfv_external_wq_aed=tuflowfv_external_wq

all: ${TARGET}

${libdir}/lib${LIBAEDFV}.a: ${objdir} ${moddir} ${libdir} ${FVOBJECTS}
	ar -rv $@ ${FVOBJECTS}
	ranlib $@

${libdir}/${OUTLIB}.a: ${libdir}/lib${LIBAEDFV}.a ${OBJECTS}
	# ar -rv $@ ${OBJECTS}
	# The old way built just the fv library and require the linking of dependant
	#  aed libs at final link stage.  This ugly haque combines the necessary aed libs
	#  into the external wq lib so there is less change to the tfv makefile
	( cd ${objdir} ; ar -x ${AEDWATDIR}/lib/lib${LIBWATAED}.a )
ifneq ($(AEDBENDIR),)
	( cd ${objdir} ; ar -x ${AEDBENDIR}/lib/lib${LIBBENAED}.a )
endif
ifneq ($(AEDRIPDIR),)
	( cd ${objdir} ; ar -x ${AEDRIPDIR}/lib/lib${LIBRIPAED}.a )
endif
ifneq ($(AEDDMODIR),)
	( cd ${objdir} ; ar -x ${AEDDMODIR}/lib/lib${LIBDMOAED}.a )
endif
ifneq ($(AEDDEVDIR),)
	( cd ${objdir} ; ar -x ${AEDDEVDIR}/lib/lib${LIBDEVAED}.a )
endif
	( cd ${objdir} ; ar -rv ../$@ *.o )
	ranlib $@

${libdir}/${OUTLIB}.${so_ext}: ${libdir}/lib${LIBAEDFV}.a ${OBJECTS}
	$(FC) ${SHARED} -o $@.${VERS} ${OBJECTS} ${LDFLAGS} ${SOFLAGS}
	ln -sf ${OUTLIB}.${so_ext}.${VERS} $@

${objdir}/%.o: ${srcdir}/%.F90 ${AEDWATDIR}/include/aed.h
	$(F90) ${FFLAGS} ${INCLUDES} -g -c $< -o $@

${objdir}/tuflowfv_external_wq_aed.o: tuflowfv_external_wq/tuflowfv_external_wq_aed.F90
	$(FC) ${FFLAGS} ${TFFLAGS} ${INCLUDES} -Ituflowfv_external_wq -c $< -o $@

${objdir}/aed_external.o: $(AEDWATDIR)/${srcdir}/aed_external.F90
	$(F90) $(FFLAGS) $(EXTFLAG) ${INCLUDES} -g -c $< -o $@

${objdir}:
	@mkdir ${objdir}

${moddir}:
	@mkdir ${moddir}

${libdir}:
	@mkdir ${libdir}

clean:
	/bin/rm -f *.i90
	/bin/rm -f ${objdir}/*.o
	/bin/rm -f ${moddir}/*.mod
	/bin/rm -f ${libdir}/*.a
	/bin/rm -f ${libdir}/*.${so_ext}*

distclean: clean
	/bin/rm -rf ${libdir} ${moddir} ${objdir} mod_s
