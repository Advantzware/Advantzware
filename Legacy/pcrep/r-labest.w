/* pcrep/r-labest.w */

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
   {sys/inc/selrptcol.i "DR7" }
END.

IF SelectRptColumn-log THEN RUN pcrep/r-labestN.w PERSISTENT.
ELSE RUN pcrep/r-labestA.w PERSISTENT.