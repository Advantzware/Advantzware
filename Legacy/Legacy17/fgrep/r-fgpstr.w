/* fgrep/r-fgpstr.w */

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
   {sys/inc/selrptcol.i "IL6" }
END.

IF SelectRptColumn-log THEN RUN fgrep/r-fgpstrN.w PERSISTENT.
ELSE RUN fgrep/r-fgpstrA.w PERSISTENT.