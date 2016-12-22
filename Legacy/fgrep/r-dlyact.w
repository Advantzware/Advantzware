/* fgrep/r-dlyact.w */

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
   {sys/inc/selrptcol.i "IL7" }
END.

IF SelectRptColumn-log THEN RUN fgrep/r-dlyactN.w PERSISTENT.
ELSE RUN fgrep/r-dlyactA.w PERSISTENT.