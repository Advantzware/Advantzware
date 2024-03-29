/* fgrep/r-fgohbb.w */

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
   {sys/inc/runAOAVer.i "IR2" }
END.

cAOAFile = SEARCH("AOA/r-fgohbb.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-fgohbb.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN fgrep/r-fgohbN.w PERSISTENT.
ELSE RUN fgrep/r-fgohbA.w PERSISTENT.
