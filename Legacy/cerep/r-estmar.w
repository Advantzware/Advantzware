/* cerep/r-estmar.w */

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
   {sys/inc/selrptcol.i "ER10" }
END.

IF SelectRptColumn-log THEN RUN cerep/r-estmarN.w PERSISTENT.
ELSE RUN cerep/r-estmarA.w PERSISTENT.