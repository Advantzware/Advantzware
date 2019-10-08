/* arrep/r-chist.w */

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
   {sys/inc/selrptcol.i "AL1" }
END.

IF SelectRptColumn-log THEN RUN arrep/r-chistN.w PERSISTENT.
ELSE RUN arrep/r-chistA.w PERSISTENT.