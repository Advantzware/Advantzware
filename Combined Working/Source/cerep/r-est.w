
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
   {sys/inc/runAOAVer.i "ER5" }
END.

cAOAFile = SEARCH("AOA/r-est.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-est.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN cerep/r-estN.w PERSISTENT.
ELSE RUN cerep/r-estA.w PERSISTENT.
