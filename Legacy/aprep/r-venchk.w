/* aprep/r-venchk.w */

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
   {sys/inc/runAOAVer.i "VR13" }
END.

cAOAFile = SEARCH("AOA/r-venchk.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-venchk.p.
ELSE RUN aprep/r-venchkN.w PERSISTENT.
