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
    {sys/inc/selrptcol.i "AR11" }
END.

IF SelectRptColumn-log THEN RUN arrep/r-pstdueN.w PERSISTENT.
ELSE RUN arrep/r-pstdueA.w PERSISTENT.
