/* pcrep/r-jbbym.w */

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
   {sys/inc/runAOAVer.i "DR13" }
END.

cAOAFile = SEARCH("AOA/r-jbbym.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-jbbym.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN pcrep/r-jbbymN.w PERSISTENT.
ELSE RUN pcrep/r-jbbymA.w PERSISTENT.
