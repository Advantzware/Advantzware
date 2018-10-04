/* fgrep/r-stajob.w */

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
   {sys/inc/selrptcol.i "AR12" }
END.

IF SelectRptColumn-log THEN RUN arrep/r-custtN.w PERSISTENT.
ELSE RUN arrep/r-custtA.w PERSISTENT.