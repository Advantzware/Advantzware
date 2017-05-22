/* aprep/r-cashrq.w */

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
   {sys/inc/selrptcol.i "VR2" }
END.

IF SelectRptColumn-log THEN RUN aprep/r-cashqN.w PERSISTENT.
ELSE RUN aprep/r-cashqA.w PERSISTENT.

