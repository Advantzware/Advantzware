/* arrep/r-depsit.w */

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
   {sys/inc/selrptcol.i "AR8" }
END.

IF SelectRptColumn-log THEN RUN arrep/r-depsitN.w PERSISTENT.
ELSE RUN arrep/r-depsitA.w PERSISTENT.