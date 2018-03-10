/* arrep/r-carr.w */

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
   {sys/inc/runAOAVer.i "AR2" }
END.

cAOAFile = SEARCH("AOA/r-carr.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-carr.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN arrep/r-carrN.w PERSISTENT.
ELSE RUN arrep/r-carrA.w PERSISTENT.
