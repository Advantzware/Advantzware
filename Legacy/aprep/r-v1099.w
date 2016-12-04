/* aprep/r-v1099.w */

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
   {sys/inc/selrptcol.i "VR10" }
END.

IF SelectRptColumn-log THEN RUN aprep/r-v1099N.w PERSISTENT.
ELSE RUN aprep/r-v1099A.w PERSISTENT.