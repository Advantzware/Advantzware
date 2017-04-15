/* porep/r-notvou.w */

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
   {sys/inc/selrptcol.i "PR7" }
END.

IF SelectRptColumn-log THEN RUN porep/r-notvouN.w PERSISTENT.
ELSE RUN porep/r-notvouA.w PERSISTENT.