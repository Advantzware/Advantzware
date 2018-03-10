/* pcrep/r-prodlys.w */

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
   {sys/inc/runAOAVer.i "DE2"}
END.

cAOAFile = SEARCH("AOA/r-prodlys.p").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-prodlys.p.
ELSE RUN pcrep/r-prodlyN.w PERSISTENT.
