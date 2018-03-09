/* jcrep/r-totwsa.w */

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
   {sys/inc/runAOAVer.i "JL3" }
END.

cAOAFile = SEARCH("AOA/r-totwsa.p").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-totwsa.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN jcrep/r-totwsaN.w PERSISTENT.
ELSE RUN jcrep/r-totwsaA.w PERSISTENT.
