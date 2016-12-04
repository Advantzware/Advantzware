/* salrep/r-cusitm.w */

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
   {sys/inc/selrptcol.i "HZ" }
END.

IF SelectRptColumn-log THEN RUN salrep/r-cusitmN.w PERSISTENT.
ELSE RUN salrep/r-cusitmA.w PERSISTENT.