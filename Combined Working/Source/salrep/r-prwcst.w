/* salrep/r-prwcst.w */

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
   {sys/inc/runAOAVer.i "HR8" }
END.

cAOAFile = SEARCH("AOA/r-prwcst.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-prwcst.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN salrep/r-prwcstN.w PERSISTENT.
ELSE RUN salrep/r-prwcstA.w PERSISTENT.
