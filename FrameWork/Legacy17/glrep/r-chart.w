/* glrep/r-chart.w */

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
   {sys/inc/selrptcol.i "GR1" }
END.

IF SelectRptColumn-log THEN RUN glrep/r-chartN.w PERSISTENT.
ELSE RUN glrep/r-chartA.w PERSISTENT.