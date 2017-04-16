/* jcrep/r-msfkic.w */

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
   {sys/inc/selrptcol.i "JR11" }
END.

IF SelectRptColumn-log THEN RUN jcrep/r-msfkicN.w PERSISTENT.
ELSE RUN jcrep/r-msfkicA.w PERSISTENT.