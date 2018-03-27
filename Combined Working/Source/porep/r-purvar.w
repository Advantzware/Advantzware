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
   {sys/inc/runAOAVer.i "PR5" }
END.

cAOAFile = SEARCH("AOA/r-purvar.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-purvar.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN porep/r-purvaN.w PERSISTENT.
ELSE RUN porep/r-purvaA.w PERSISTENT.
