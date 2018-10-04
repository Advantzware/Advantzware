/* aprep/r-vendan.w */

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
   {sys/inc/selrptcol.i "VR3" }
END.

IF SelectRptColumn-log THEN RUN aprep/r-vendnN.w PERSISTENT.
ELSE RUN aprep/r-vendnA.w PERSISTENT.