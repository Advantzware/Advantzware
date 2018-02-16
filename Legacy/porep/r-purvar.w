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

cAOAFile = SEARCH("AOA/r-purvar.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-purvar.p.
ELSE RUN porep/r-purvaN.w PERSISTENT.
