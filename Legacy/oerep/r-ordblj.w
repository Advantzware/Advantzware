/* oerep/r-ordblj.w */

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
   {sys/inc/selrptcol.i "OZ9" }
END.

IF SelectRptColumn-log THEN RUN oerep/r-ordbjN.w.
ELSE RUN oerep/r-ordbjA.w.     



