/* cerep/r-prpmat.w */

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
   {sys/inc/selrptcol.i "ER7" }
END.

IF SelectRptColumn-log THEN RUN cerep/r-prpmatN.w PERSISTENT.
ELSE RUN cerep/r-prpmatA.w PERSISTENT.