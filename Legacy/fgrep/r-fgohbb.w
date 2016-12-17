/* fgrep/r-fgohbb.w */

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
   {sys/inc/selrptcol.i "IR2" }
END.

IF SelectRptColumn-log THEN RUN fgrep/r-fgohbN.w PERSISTENT.
ELSE RUN fgrep/r-fgohbA.w PERSISTENT.