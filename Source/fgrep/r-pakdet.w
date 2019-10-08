/* fgrep/r-pakdet.w */

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
   {sys/inc/runAOAVer.i "IL11" }
END.

cAOAFile = SEARCH("AOA/r-pakdet.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-pakdet.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN fgrep/r-pakdetN.w PERSISTENT.
ELSE RUN fgrep/r-pakdetA.w PERSISTENT.
