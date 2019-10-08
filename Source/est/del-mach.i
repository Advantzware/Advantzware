/* del-mach.i */

 for each mstd of mach:
   for each mmty of mstd:
       delete mmty.
   end.
   for each mmtx of mstd:
       delete mmtx.
   end.
   delete mstd.
 end.  
