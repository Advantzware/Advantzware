/* porep/r-sonord.w */

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
   {sys/inc/runAOAVer.i "PR1" }
END.

cAOAFile = SEARCH("AOA/r-sonord.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-sonord.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN porep/r-sonordN.w PERSISTENT.
ELSE RUN porep/r-sonordA.w PERSISTENT.
