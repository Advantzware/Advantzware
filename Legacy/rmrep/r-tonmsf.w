/* rmrep/r-tonmsf.w */

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
   {sys/inc/selrptcol.i "MR11" }
END.

IF SelectRptColumn-log THEN RUN rmrep/r-tonmsfN.w PERSISTENT.
ELSE RUN rmrep/r-tonmsfA.w PERSISTENT.