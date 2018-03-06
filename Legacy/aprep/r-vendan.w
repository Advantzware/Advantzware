/* aprep/r-vendan.w */

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
   {sys/inc/runAOAVer.i "VR3" }
END.

cAOAFile = SEARCH("AOA/r-vendan.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-vendan.p.
ELSE RUN aprep/r-vendnN.w PERSISTENT.
