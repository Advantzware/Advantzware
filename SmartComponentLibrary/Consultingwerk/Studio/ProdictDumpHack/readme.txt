Use the contents in this directory at your own risk! Consultingwerk Ltd. (CW)
provides these files without warranty of any kind - expressed or implied!!!

This directory contains a modified version of the OpenEdge 11.3.2 data 
dictionary dump routine. The routine is modified in the way, that tables
that contain LOB fields now export the LOB fields using constant file names.
Rather than using a sequence counter (causing new file names each time you 
dump again) this routine builds the file name for the LOB files based on 
the records ROWID. This causes, that the file name of the LOB files stay
constant until the database is dumped and loaded again.

This makes versioning of .d files that contain LOB's much easier!

This routine is compatible with the data dictionary load data routine.

To use the modified routine, add Consultingwerk/Studio/ProdictDumpHack/src 
to your propath when running the data dictionary. As .p files starting with 
_ in the file name are typically cached by default, you may have to restart
your session.
 