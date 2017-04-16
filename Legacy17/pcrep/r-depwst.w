/* pcrep/r-depwst.w */

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
   {sys/inc/selrptcol.i "DR5" }
END.

IF SelectRptColumn-log THEN RUN pcrep/r-depwstN.w PERSISTENT.
ELSE RUN pcrep/r-depwstA.w PERSISTENT.