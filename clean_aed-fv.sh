#!/bin/bash

CWD=`pwd`
for i in water benthic riparian demo dev ; do
   echo clean libaed-$i
   if [ -d ../libaed-$i ] ; then
     cd  ../libaed-$i
     make distclean
     cd ${CWD}
   fi
done

make distclean
