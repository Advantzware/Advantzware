/* aprep/r-mtdsub.w */

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
   {sys/inc/selrptcol.i "VR8" }
END.

IF SelectRptColumn-log THEN RUN aprep/r-mtdsubN.w PERSISTENT.
ELSE RUN aprep/r-mtdsubA.w PERSISTENT.
