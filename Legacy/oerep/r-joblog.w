/* oerep/r-joblog.w */

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
   {sys/inc/selrptcol.i "OR3" }
END.

IF SelectRptColumn-log THEN RUN oerep/r-joblgN.w.
ELSE RUN oerep/r-joblgA.w.     



