/* rmrep/r-inkglu.w */

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
   {sys/inc/runAOAVer.i "MR12" }
END.

cAOAFile = SEARCH("AOA/r-inkglu.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-inkglu.p.
ELSE RUN rmrep/r-inkgluN.w PERSISTENT.
