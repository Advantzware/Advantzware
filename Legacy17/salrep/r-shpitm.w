/* salrep/r-shpitm.w */

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
   {sys/inc/selrptcol.i "HR6" }
END.

IF SelectRptColumn-log THEN RUN salrep/r-shpitmN.w PERSISTENT.
ELSE RUN salrep/r-shpitmA.w PERSISTENT.