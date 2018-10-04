/* porep/r-voufgc.w */

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
   {sys/inc/selrptcol.i "PR11" }
END.

IF SelectRptColumn-log THEN RUN porep/r-voufgcN.w PERSISTENT.
ELSE RUN porep/r-voufgcA.w PERSISTENT.