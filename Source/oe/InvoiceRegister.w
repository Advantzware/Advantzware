/* oe/r-inve&r.w */

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
   {sys/inc/runAOAVer.i "OB4"}
END.

cAOAFile = SEARCH("AOA/InvoiceRegister.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/InvoiceRegister.p.
ELSE RUN oe/InvoiceRegisterN.w PERSISTENT.
