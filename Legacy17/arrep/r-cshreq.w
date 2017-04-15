/* arrep/r-cshreq.w */

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
   {sys/inc/selrptcol.i "AR14" }
END.

IF SelectRptColumn-log THEN RUN arrep/r-cshreqN.w PERSISTENT.
ELSE RUN arrep/r-cshreqA.w PERSISTENT.