/* oerep/r-commcr.w */

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
   {sys/inc/selrptcol.i "AR15" }
END.

IF SelectRptColumn-log THEN RUN oerep/r-commrN.w PERSISTENT.
ELSE RUN oerep/r-commrA.w PERSISTENT.