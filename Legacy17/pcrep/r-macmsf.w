/* pcrep/r-macmsf.w */

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
   {sys/inc/selrptcol.i "JL4" }
END.

IF SelectRptColumn-log THEN RUN pcrep/r-macmsfN.w PERSISTENT.
ELSE RUN pcrep/r-macmsfA.w PERSISTENT.