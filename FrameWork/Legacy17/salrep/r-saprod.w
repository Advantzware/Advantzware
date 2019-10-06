/* salrep/r-saprod.w */

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
   {sys/inc/selrptcol.i "HF" }
END.

IF SelectRptColumn-log THEN RUN salrep/r-saprodN.w PERSISTENT.
ELSE RUN salrep/r-saprodA.w PERSISTENT.