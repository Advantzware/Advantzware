/* aprep/r-taxdis.w */

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
   {sys/inc/runAOAVer.i "VR7" }
END.

cAOAFile = SEARCH("AOA/r-taxdis.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-taxdis.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN aprep/r-taxdisN.w PERSISTENT.
ELSE RUN aprep/r-taxdisA.w PERSISTENT.
