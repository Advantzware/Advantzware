/* rmrep/r-reordr.w */

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
   {sys/inc/runAOAVer.i "MR2" }
END.

cAOAFile = SEARCH("AOA/r-reordr.p").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-reordr.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN rmrep/r-reordrN.w PERSISTENT.
ELSE RUN rmrep/r-reordrA.w PERSISTENT.
