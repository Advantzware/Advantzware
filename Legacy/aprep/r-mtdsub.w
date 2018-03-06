/* aprep/r-mtdsub.w */

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
   {sys/inc/runAOAVer.i "VR8" }
END.

cAOAFile = SEARCH("AOA/r-mtdsub.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-mtdsub.p.
ELSE RUN aprep/r-mtdsubN.w PERSISTENT.
