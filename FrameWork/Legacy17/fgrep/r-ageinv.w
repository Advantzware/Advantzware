/* fgrep/r-ageinv.w */

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
   {sys/inc/selrptcol.i "IR12" }
END.

IF SelectRptColumn-log THEN RUN fgrep/r-ageinN.w PERSISTENT.
ELSE RUN fgrep/r-ageinA.w PERSISTENT.