/* oerep/r-sched.w */

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
   {sys/inc/selrptcol.i "OR2" }
END.

IF SelectRptColumn-log THEN RUN oerep/r-schedN.w PERSISTENT.
ELSE RUN oerep/r-schedA.w PERSISTENT.