/* salrep/r-yeavol.w */

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
   {sys/inc/selrptcol.i "HY" }
END.

IF SelectRptColumn-log THEN RUN salrep/r-yeavolN.w PERSISTENT.
ELSE RUN salrep/r-yeavolA.w PERSISTENT.