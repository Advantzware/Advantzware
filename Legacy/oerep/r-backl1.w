/* oerep/r-backl1.w */

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
   {sys/inc/selrptcol.i "OR13" }
END.

IF SelectRptColumn-log THEN RUN oerep/r-back1N.w.
ELSE RUN oerep/r-back1A.w.     



