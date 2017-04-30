/* fgrep/r-itmlst.w */

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
   {sys/inc/selrptcol.i "IR9" }
END.

IF SelectRptColumn-log THEN RUN fgrep/r-itmltN.w PERSISTENT.
ELSE RUN fgrep/r-itmltA.w PERSISTENT.