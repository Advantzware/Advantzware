/* cerep/r-quolst.w */

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
   {sys/inc/runAOAVer.i "ER9" }
END.

cAOAFile = SEARCH("AOA/r-quolst.p").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-quolst.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN cerep/r-quolstN.w PERSISTENT.
ELSE RUN cerep/r-quolstA.w PERSISTENT.
