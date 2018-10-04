/* rmrep/r-stkrol.w */

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
   {sys/inc/selrptcol.i "MR9" }
END.

IF SelectRptColumn-log THEN RUN rmrep/r-stkrolN.w PERSISTENT.
ELSE RUN rmrep/r-stkrolA.w PERSISTENT.