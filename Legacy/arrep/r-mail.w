/* arrep/r-mail.w */

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
   {sys/inc/runAOAVer.i "AR7" }
END.

cAOAFile = SEARCH("AOA/r-mail.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-mail.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN arrep/r-mailN.w PERSISTENT.
ELSE RUN arrep/r-mailA.w PERSISTENT.
