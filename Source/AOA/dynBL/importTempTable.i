/* importTempTable.i */

{aoa/tempTable/{1}.i}

DEFINE OUTPUT PARAMETER TABLE FOR {1}.
{aoa/includes/aoaInputDefParams.i}

DEFINE BUFFER b{1} FOR {1}.
    
cTmpFile = SEARCH("AOA.{1}."
        + ipcCompany       + "."
        + STRING(ipiBatch) + "."
        + ipcUserID        + ".dat"
        ).
IF cTmpFile NE ? THEN DO:
    CREATE b{1}.
    INPUT FROM VALUE(cTmpFile).
    REPEAT:
        IMPORT b{1}.
        CREATE {1}.
        BUFFER-COPY b{1} TO {1}.
    END. /* repeat */
    INPUT CLOSE.
END. /* cTmpFile ne ? */
