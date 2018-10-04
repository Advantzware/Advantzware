/* porep/r-sonord.w */

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
   {sys/inc/selrptcol.i "PR1" }
END.

IF SelectRptColumn-log THEN RUN porep/r-sonordN.w PERSISTENT.
ELSE RUN porep/r-sonordA.w PERSISTENT.