/* pcrep/r-purvar.w */

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
   {sys/inc/selrptcol.i "PR5" }
END.

IF SelectRptColumn-log THEN RUN porep/r-purvaN.w.
ELSE RUN porep/r-purvaA.w.     
        
