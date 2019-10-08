/* salrep/r-scomsr.w */

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
   {sys/inc/selrptcol.i "HR16" }
END.

IF SelectRptColumn-log THEN RUN salrep/r-scomsrN.w PERSISTENT.
ELSE RUN salrep/r-scomsrA.w PERSISTENT.