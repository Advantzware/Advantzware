/* rmrep/r-inkmch.w */

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
   {sys/inc/selrptcol.i "MR5" }
END.

IF SelectRptColumn-log THEN RUN rmrep/r-transN.w PERSISTENT.
ELSE RUN rmrep/r-transA.w PERSISTENT.