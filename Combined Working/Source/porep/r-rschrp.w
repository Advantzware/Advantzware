/* porep/r-rschrp.w */

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
   {sys/inc/runAOAVer.i "PR14" }
END.

cAOAFile = SEARCH("AOA/r-rschrp.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-rschrp.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN porep/r-rschrpN.w PERSISTENT.
ELSE RUN porep/r-rschrpA.w PERSISTENT.
