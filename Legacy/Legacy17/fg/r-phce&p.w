/* fg\r-phce&p.w */

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
   {sys/inc/selrptcol.i "IC3" }
END.

IF SelectRptColumn-log THEN RUN fg\r-phce&pN.w PERSISTENT.
ELSE RUN fg\r-phce&pA.w PERSISTENT.