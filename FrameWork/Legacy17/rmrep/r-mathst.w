/* rmrep/r-mathst.w */

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
   {sys/inc/selrptcol.i "MR7" }
END.

IF SelectRptColumn-log THEN RUN rmrep/r-mathsN.w PERSISTENT.
ELSE RUN rmrep/r-mathsA.w PERSISTENT.