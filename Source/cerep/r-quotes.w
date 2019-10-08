/* cerep/r-quotes.w */

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
   {sys/inc/runAOAVer.i "ER6" }
END.

cAOAFile = SEARCH("AOA/r-quotes.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-quotes.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN cerep/r-quotesN.w PERSISTENT.
ELSE RUN cerep/r-quotesA.w PERSISTENT.
