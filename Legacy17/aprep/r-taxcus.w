/* aprep/r-taxcus.w */

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
   {sys/inc/selrptcol.i "VR9" }
END.

IF SelectRptColumn-log THEN RUN aprep/r-taxcusN.w PERSISTENT.
ELSE RUN aprep/r-taxcusA.w PERSISTENT.
