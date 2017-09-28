/* exportTempTable.i */

/* export sub-report data */
cTmpFile = "AOA.{1}."
        + ipcCompany       + "."
        + STRING(ipiBatch) + "."
        + ipcUserID        + ".dat"
        .
OUTPUT TO VALUE(cTmpFile).
/* create dummy last record to avoid duplicate triggered by metadata record */
CREATE {1}.
FOR EACH {1}:
    EXPORT {1}.
END. /* each {1} */
OUTPUT CLOSE.
