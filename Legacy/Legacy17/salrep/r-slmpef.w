/* salrep/r-slmpef.w */

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
   {sys/inc/selrptcol.i "HT" }
END.

IF SelectRptColumn-log THEN RUN salrep/r-slmpefN.w PERSISTENT.
ELSE RUN salrep/r-slmpefA.w PERSISTENT.