/* rmrep/r-rmcost.w */

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
   {sys/inc/selrptcol.i "MR1" }
END.

IF SelectRptColumn-log THEN RUN rmrep/r-rmcostN.w.
ELSE RUN rmrep/r-rmcostA.w.     
        











