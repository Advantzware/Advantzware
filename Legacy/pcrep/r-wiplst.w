/* pcrep/r-wiplst.w */

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
   {sys/inc/runAOAVer.i "DR8" }
END.

cAOAFile = SEARCH("AOA/r-wiplst.p").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-wiplst.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN pcrep/r-wiplstN.w PERSISTENT.
ELSE RUN pcrep/r-wiplstA.w PERSISTENT.
