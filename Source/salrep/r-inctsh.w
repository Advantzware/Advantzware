/* salrep/r-inctsh.w */

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
   {sys/inc/selrptcol.i "HR13" }
END.

IF SelectRptColumn-log THEN RUN salrep/r-inctshN.w.
ELSE RUN salrep/r-inctshA.w.
