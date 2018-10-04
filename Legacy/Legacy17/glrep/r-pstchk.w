/* glrep/r-pstchk.w */

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
   {sys/inc/selrptcol.i "GR8" }
END.

IF SelectRptColumn-log THEN RUN glrep/r-pstchN.w PERSISTENT.
ELSE RUN glrep/r-pstchA.w PERSISTENT.