/* salrep/r-prfinv.w */

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
   {sys/inc/selrptcol.i "GR5" }
END.

IF SelectRptColumn-log THEN RUN glrep/r-glhstN.w PERSISTENT.
ELSE RUN glrep/r-glhstA.w PERSISTENT.