/* arrep/r-pstdue.w */

{methods/defines/hndldefs.i}
{methods/prgsecur.i} 

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN 
    cocode = gcompany
    locode = gloc
    .

DO TRANSACTION:
    {sys/inc/runAOAVer.i "AR11" }
END.

cAOAFile = SEARCH("AOA/r-pstdue.r").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/r-pstdue.p.
ELSE IF RunAOAVersion1-log NE NO AND RunAOAVersion2-log THEN RUN arrep/r-pstdueN.w PERSISTENT.
ELSE RUN arrep/r-pstdueA.w PERSISTENT.
