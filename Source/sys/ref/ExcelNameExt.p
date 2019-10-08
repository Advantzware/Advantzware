/*   sys/ref/excelextend.p   */
DEFINE INPUT PARAMETER ipFile as CHARACTER NO-UNDO .
DEFINE OUTPUT PARAMETER opcFileName as CHARACTER NO-UNDO .

opcFileName  =   SUBSTRING(ipFile,1,INDEX(ipFile,".") - 1) .
opcFileName  = opcFileName + "_" + STRING(year(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + "_" + STRING(TIME) + ".csv" .
