/*   sys/ref/excelextend.p   */
DEFINE INPUT PARAMETER ipFile as CHARACTER NO-UNDO .
DEFINE OUTPUT PARAMETER opcFileName as CHARACTER NO-UNDO .
DEFIN VARIABLE cTempPath AS CHARACTER NO-UNDO .
DEFINE VARIABLE lCreated AS LOGICAL NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO .

opcFileName  =   SUBSTRING(ipFile,1,INDEX(ipFile,".") - 1) .
opcFileName  = opcFileName + "_" + STRING(year(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + "_" + STRING(TIME) + ".csv" .
 
 cTempPath =  SUBSTRING(ipFile,1,R-INDEX(ipFile,"\") - 1) .
 
 /* Create output directory if not available */
  RUN FileSys_CreateDirectory(
      INPUT  cTempPath,
      OUTPUT lCreated,
      OUTPUT cMessage
      ) NO-ERROR.
  IF NOT lCreated THEN DO:
      MESSAGE "Unable to find report path '" + cTempPath + "' to export report file"
          VIEW-AS ALERT-BOX ERROR.
      RETURN.
  END.