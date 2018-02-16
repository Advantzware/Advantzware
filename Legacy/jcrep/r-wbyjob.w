/* jcrep/r-wbyjob.w */

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
   {sys/inc/runAOAVer.i "JR4" }
END.

cAOAFile = SEARCH("AOA/r-wbyjob.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-wbyjob.p.
ELSE RUN jcrep/r-wbyjobN.w PERSISTENT.
