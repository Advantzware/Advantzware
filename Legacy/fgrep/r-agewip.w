/* fgrep/r-agewip.w */

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
   {sys/inc/runAOAVer.i "IL15" }
END.

cAOAFile = SEARCH("AOA/r-agewip.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-agewip.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN fgrep/r-agewipN.w.
ELSE RUN fgrep/r-agewipN.w PERSISTENT. /*No found old classical file in old version only selectable file exist.*/
