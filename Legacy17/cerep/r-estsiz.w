/* cerep/r-estsiz.w */

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
   {sys/inc/selrptcol.i "ER8" }
END.

IF SelectRptColumn-log THEN RUN cerep/r-estsizN.w PERSISTENT.
ELSE RUN cerep/r-estsizA.w PERSISTENT.