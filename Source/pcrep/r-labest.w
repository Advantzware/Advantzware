/* pcrep/r-labest.w */

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
   {sys/inc/runAOAVer.i "DR7" }
END.

cAOAFile = SEARCH("AOA/r-labest.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-labest.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN pcrep/r-labestN.w PERSISTENT.
ELSE RUN pcrep/r-labestA.w PERSISTENT.
