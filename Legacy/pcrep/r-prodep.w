/* pcrep/r-prodep.w */

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
   {sys/inc/runAOAVer.i "DR3" }
END.

cAOAFile = SEARCH("AOA/r-prodep.p").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-prodep.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN pcrep/r-prodepN.w PERSISTENT.
ELSE RUN pcrep/r-prodepA.w PERSISTENT.
