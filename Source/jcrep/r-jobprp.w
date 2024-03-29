/* jcrep/r-jobprp.w */

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
   {sys/inc/runAOAVer.i "JL5" }
END.

cAOAFile = SEARCH("AOA/r-jobprp.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-jobprp.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN jcrep/r-jobprpN.w PERSISTENT.
ELSE RUN jcrep/r-jobprpA.w PERSISTENT.
