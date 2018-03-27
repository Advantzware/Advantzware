/* oerep/r-booko#.w */

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
   {sys/inc/runAOAVer.i "OR11" }
END.

cAOAFile = SEARCH("AOA/r-booko#.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-booko#.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN oerep/r-booko#N.w PERSISTENT.
ELSE RUN oerep/r-booko#A.w PERSISTENT.
