/* jcrep/r-jcost.w */

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
   {sys/inc/runAOAVer.i "JL6" }
END.

cAOAFile = SEARCH("AOA/r-jcost.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-jcost.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN jcrep/r-jcostN.w PERSISTENT.
ELSE RUN jcrep/r-jcostA.w PERSISTENT.
