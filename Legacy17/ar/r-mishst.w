/* ar/r-mishst.w */

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
   {sys/inc/selrptcol.i "AL2" }
END.

IF SelectRptColumn-log THEN RUN ar/r-mishstN.w PERSISTENT.
ELSE RUN ar/r-mishstA.w PERSISTENT.