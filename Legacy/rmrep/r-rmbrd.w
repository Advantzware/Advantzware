/* rmrep/r-rmbrd.w */

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
   {sys/inc/runAOAVer.i "MR14" }
END.

cAOAFile = SEARCH("AOA/r-rmbrd.p").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-rmbrd.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN rmrep/r-rmbrdN.w PERSISTENT.
ELSE RUN rmrep/r-rmbrdA.w PERSISTENT.
