/* pcrep/r-scrapm.w */

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
   {sys/inc/selrptcol.i "DR12" }
END.

IF SelectRptColumn-log THEN RUN pcrep/r-scrapmN.w PERSISTENT.
ELSE RUN pcrep/r-scrapmA.w PERSISTENT.