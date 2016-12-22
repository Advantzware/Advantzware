/* arrep/r-mail.w */

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
   {sys/inc/selrptcol.i "AR7" }
END.

IF SelectRptColumn-log THEN RUN arrep/r-mailN.w PERSISTENT.
ELSE RUN arrep/r-mailA.w PERSISTENT.