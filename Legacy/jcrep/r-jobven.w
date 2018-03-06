/* jcrep/r-jobven.w */

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
   {sys/inc/runAOAVer.i "JR9" }
END.

cAOAFile = SEARCH("AOA/r-jobven.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-jobven.p.
ELSE RUN jcrep/r-jobvenN.w PERSISTENT.
