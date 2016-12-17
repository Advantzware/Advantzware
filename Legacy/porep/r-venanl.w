/* porep/r-venanl.w */

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
   {sys/inc/selrptcol.i "PR6" }
END.

IF SelectRptColumn-log THEN RUN porep/r-venanlN.w PERSISTENT.
ELSE RUN porep/r-venanlA.w PERSISTENT.