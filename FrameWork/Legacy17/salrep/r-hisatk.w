/* salrep/r-hisatk.w */

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
   {sys/inc/selrptcol.i "HR1" }
END.

IF SelectRptColumn-log THEN RUN salrep/r-hisatkN.w PERSISTENT.
ELSE RUN salrep/r-hisatkA.w PERSISTENT.