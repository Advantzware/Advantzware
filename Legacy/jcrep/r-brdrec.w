/* jcrep/r-brdrec.w */

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
   {sys/inc/selrptcol.i "JR8" }
END.

IF SelectRptColumn-log THEN RUN jcrep/r-brdreN.w.
ELSE RUN jcrep/r-brdreA.w.     
