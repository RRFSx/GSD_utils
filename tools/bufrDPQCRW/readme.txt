1.compile:
   Before compile the src, need to compile baselib. here is an example:
      /gpfs/fs1/p/ral/jntp/mhu/code/GSD_UTL/tools/baselib/build
   Then compile code use cmake and make

2.run:
   Need to copy a HRRR forecast to run directroy if use the foreacst RW to do dealiasing.
   Need to setup the namelist.input. Set the right cycle time and point to the wrf file.

3.There are BUFR file before and after dealiasing.

4. The GSI needs to turn of VAD QC.


