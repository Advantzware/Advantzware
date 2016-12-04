/* rmrep/r-rmroll.w */

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
   {sys/inc/selrptcol.i "MR13" }
END.

IF SelectRptColumn-log THEN RUN rmrep/r-rmrolN.w PERSISTENT.
ELSE RUN rmrep/r-rmrolA.w PERSISTENT.