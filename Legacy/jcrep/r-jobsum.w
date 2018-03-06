/* jcrep/r-jobsum.w */

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
   {sys/inc/runAOAVer.i "JR3" }
END.

cAOAFile = SEARCH("AOA/r-jobsum.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-jobsum.p.
ELSE RUN jcrep/r-jobsumN.w PERSISTENT.
