/* aprep/r-v1099.w */

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
   {sys/inc/runAOAVer.i "VR10" }
END.

cAOAFile = SEARCH("AOA/r-v1099.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-v1099.p.
ELSE RUN aprep/r-v1099N.w PERSISTENT.
