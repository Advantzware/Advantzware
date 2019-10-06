/* oerep/r-comprm.w */

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
   {sys/inc/runAOAVer.i "OZ5" }
END.

cAOAFile = SEARCH("AOA/r-comprm.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-comprm.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN oerep/r-comprN.w PERSISTENT.
ELSE RUN oerep/r-comprA.w PERSISTENT.
