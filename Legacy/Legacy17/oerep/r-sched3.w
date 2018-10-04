/* oerep/r-sched3.w */

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
   {sys/inc/selrptcol.i "OZ4" }
END.

IF SelectRptColumn-log THEN RUN oerep/r-shed3N.w PERSISTENT.
ELSE RUN oerep/r-shed3A.w PERSISTENT.