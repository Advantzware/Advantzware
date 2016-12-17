/* fgrep/r-agewip.w */

{methods/defines/hndldefs.i}
{methods/prgsecur.i} 

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DO TRANSACTION:
   {sys/inc/selrptcol.i "IL15" }
END.

/*IF SelectRptColumn-log THEN RUN fgrep/r-agewipN.w.
ELSE RUN fgrep/r-ageinvA.w.     
*/
RUN fgrep/r-agewipN.w PERSISTENT.