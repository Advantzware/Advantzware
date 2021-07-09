/* exportDynTempTable.i */

/* export sub-report data */
cTmpFile = "AOA.{1}."
         + cCompany + "."
         + STRING(dynParamValue.paramValueID) + "."
         + dynParamValue.user-id + ".dat"
         .
OUTPUT TO VALUE(cTmpFile).
/* create dummy last record to avoid duplicate triggered by metadata record */
CREATE {1}.
FOR EACH {1}:
    EXPORT {1}.
END. /* each {1} */
OUTPUT CLOSE.
