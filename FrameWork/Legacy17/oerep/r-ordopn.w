/* oerep/r-ordopn.w */

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
   {sys/inc/selrptcol.i "OR16" }
END.

IF SelectRptColumn-log THEN RUN oerep/r-ordonN.w PERSISTENT.
ELSE RUN oerep/r-ordonA.w PERSISTENT.