/* pcrep/r-jbbym.w */

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
   {sys/inc/selrptcol.i "DR13" }
END.

IF SelectRptColumn-log THEN RUN pcrep/r-jbbymN.w PERSISTENT.
ELSE RUN pcrep/r-jbbymA.w PERSISTENT.