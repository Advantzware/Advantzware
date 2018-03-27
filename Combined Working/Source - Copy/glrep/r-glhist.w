/* salrep/r-prfinv.w */

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
   {sys/inc/runAOAVer.i "GR5" }
END.

cAOAFile = SEARCH("AOA/r-glhist.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-glhist.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN glrep/r-glhstN.w PERSISTENT.
ELSE RUN glrep/r-glhstA.w PERSISTENT.
