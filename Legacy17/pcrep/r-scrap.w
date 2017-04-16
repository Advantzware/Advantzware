/* pcrep/r-scrap.w */

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
   {sys/inc/selrptcol.i "DR11" }
END.

IF SelectRptColumn-log THEN RUN pcrep/r-scrapN.w PERSISTENT.
ELSE RUN pcrep/r-scrapA.w PERSISTENT.