/* rmrep/r-aitem.w */

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
   {sys/inc/selrptcol.i "MR4" }
END.

IF SelectRptColumn-log THEN RUN rmrep/r-aitemN.w PERSISTENT.
ELSE RUN rmrep/r-aitemA.w PERSISTENT.