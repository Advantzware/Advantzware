/* fgrep/r-qtytot.w */

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
   {sys/inc/runAOAVer.i "IL1" }
END.

cAOAFile = SEARCH("AOA/r-qtytot.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-qtytot.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN fgrep/r-qtytotN.w PERSISTENT.
ELSE RUN fgrep/r-qtytotA.w PERSISTENT.
