/* rmrep/r-ibtag.w */

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
   {sys/inc/selrptcol.i "MR3" }
END.

IF SelectRptColumn-log THEN RUN rmrep/r-ibtagN.w PERSISTENT.
ELSE RUN rmrep/r-ibtagA.w PERSISTENT.