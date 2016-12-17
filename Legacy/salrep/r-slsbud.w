/* salrep/r-slsbud.w */

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
   {sys/inc/selrptcol.i "HS" }
END.

IF SelectRptColumn-log THEN RUN salrep/r-slsbudN.w PERSISTENT.
ELSE RUN salrep/r-slsbudA.w PERSISTENT.