/* arrep/r-cashsm.w */

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
   {sys/inc/selrptcol.i "AR9" }
END.

IF SelectRptColumn-log THEN RUN arrep/r-cashsmN.w PERSISTENT.
ELSE RUN arrep/r-cashsmA.w PERSISTENT.