/* jcrep/r-wipsum.w */

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
   {sys/inc/runAOAVer.i "JR2" }
END.

cAOAFile = SEARCH("AOA/r-wipsum.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-wipsum.p.
ELSE RUN jcrep/r-wipsumN.w PERSISTENT.
