/* pcrep/r-wiplst.w */

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
   {sys/inc/selrptcol.i "DR8" }
END.

IF SelectRptColumn-log THEN RUN pcrep/r-wiplstN.w.
ELSE RUN pcrep/r-wiplstA.w.     





