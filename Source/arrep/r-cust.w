/* arrep/r-cust.w */

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
   {sys/inc/runAOAVer.i "AR1" }
END.

cAOAFile = SEARCH("AOA/r-cust.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-cust.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN arrep/r-custN.w PERSISTENT.
ELSE RUN arrep/r-custA.w PERSISTENT.
