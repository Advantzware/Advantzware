/* jcrep/r-qbprpt.w */

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
   {sys/inc/runAOAVer.i "PR13" }
END.

cAOAFile = SEARCH("AOA/r-qbprpt.p").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-qbprpt.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN jcrep/r-qbprptN.w PERSISTENT.
ELSE RUN jcrep/r-qbprptA.w PERSISTENT.
