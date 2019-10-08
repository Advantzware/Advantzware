/* glrep/r-invsum.w */

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
   {sys/inc/selrptcol.i "GR4" }
END.

IF SelectRptColumn-log THEN RUN glrep/r-invsuN.w PERSISTENT.
ELSE RUN glrep/r-invsuA.w PERSISTENT.