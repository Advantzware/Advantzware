/* jcrep/r-wipsum.w */

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
   {sys/inc/selrptcol.i "JR2" }
END.

IF SelectRptColumn-log THEN RUN jcrep/r-wipsumN.w PERSISTENT.
ELSE RUN jcrep/r-wipsumA.w PERSISTENT.