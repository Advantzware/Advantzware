/* aprep/r-vaging.w */

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
   {sys/inc/runAOAVer.i "VR1" }
END.

cAOAFile = SEARCH("AOA/r-vaging.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-vaging.p.
ELSE RUN aprep/r-vagingN.w PERSISTENT.
