/* rmrep/r-inkmch.w */

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
   {sys/inc/runAOAVer.i "MR5" }
END.

cAOAFile = SEARCH("AOA/r-trans.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-trans.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN rmrep/r-transN.w PERSISTENT.
ELSE RUN rmrep/r-transA.w PERSISTENT.
