/* salrep/r-lastvs.w */

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
   {sys/inc/selrptcol.i "HL" }
END.

IF SelectRptColumn-log THEN RUN salrep/r-lastvsN.w PERSISTENT.
ELSE RUN salrep/r-lastvsA.w PERSISTENT.