/* salrep/r-detjnl.w */

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
   {sys/inc/runAOAVer.i "HB" }
END.

cAOAFile = SEARCH("AOA/r-detjnl.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-detjnl.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN salrep/r-detjnlN.w PERSISTENT.
ELSE RUN salrep/r-detjnlA.w PERSISTENT.
