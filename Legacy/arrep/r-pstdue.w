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

cAOAFile = SEARCH("AOA/r-pstdue.p").
IF RunAOAVersion-log AND cAOAFile NE ? THEN RUN AOA/r-pstdue.p.
ELSE RUN arrep/r-pstdueN.w PERSISTENT.
