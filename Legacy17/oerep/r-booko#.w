/* oerep/r-booko#.w */

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
   {sys/inc/selrptcol.i "OR11" }
END.

IF SelectRptColumn-log THEN RUN oerep/r-booko#N.w PERSISTENT.
ELSE RUN oerep/r-booko#A.w PERSISTENT.