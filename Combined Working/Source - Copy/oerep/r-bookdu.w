/* oerep/r-bookdu.w */

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
   {sys/inc/runAOAVer.i "OR10" }
END.

cAOAFile = SEARCH("AOA/r-bookdu.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-bookdu.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN oerep/r-bokduN.w PERSISTENT.
ELSE RUN oerep/r-bokduA.w PERSISTENT.
