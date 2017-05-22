/* arrep/r-cashs2.w */

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
   {sys/inc/selrptcol.i "AR16" }
END.

IF SelectRptColumn-log THEN RUN arrep/r-cashs2N.w PERSISTENT.
ELSE RUN arrep/r-cashs2A.w PERSISTENT.