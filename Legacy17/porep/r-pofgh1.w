/* porep/r-pofgh1.w */

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
   {sys/inc/selrptcol.i "PR9" }
END.

IF SelectRptColumn-log THEN RUN porep/r-pofgh1N.w PERSISTENT.
ELSE RUN porep/r-pofgh1A.w PERSISTENT.