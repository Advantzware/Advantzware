/* fgrep/r-qtyval.w */

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
   {sys/inc/selrptcol.i "IR4" }
END.

IF SelectRptColumn-log THEN RUN fgrep/r-qtyvlN.w PERSISTENT.
ELSE RUN fgrep/r-qtyvlA.w PERSISTENT.