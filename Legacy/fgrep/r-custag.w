/* fgrep/r-custag.w */

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
   {sys/inc/selrptcol.i "IR7" }
END.

IF SelectRptColumn-log THEN RUN fgrep/r-custgN.w.
ELSE RUN fgrep/r-custgA.w.     
        






