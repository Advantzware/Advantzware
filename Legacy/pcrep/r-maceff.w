/* pcrep/r-maceff.w */

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
   {sys/inc/selrptcol.i "DR1" }
END.

IF SelectRptColumn-log THEN RUN pcrep/r-maceffN.w PERSISTENT.
ELSE RUN pcrep/r-maceffA.w PERSISTENT.