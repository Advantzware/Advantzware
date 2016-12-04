/* fgrep/r-valjob.w */

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
   {sys/inc/selrptcol.i "IR13" }
END.

IF SelectRptColumn-log THEN RUN fgrep/r-valjbN.w PERSISTENT.
ELSE RUN fgrep/r-valjbA.w PERSISTENT.