/* fgrep/r-itmlst.w */

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
   {sys/inc/runAOAVer.i "IR9" }
END.

cAOAFile = SEARCH("AOA/r-itmlst.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-itmlst.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN fgrep/r-itmltN.w PERSISTENT.
ELSE RUN fgrep/r-itmltA.w PERSISTENT.
