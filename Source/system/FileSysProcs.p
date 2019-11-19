
 /*------------------------------------------------------------------------
     File        : FileSysProcs.p
     Purpose     : Mulltiple Procs and Functions for processing Folder and files

     Syntax      :

     Description : 

     Author(s)   : Sewa Singh
     Created     : Mon Nov 18  2019
     Notes       :
   ----------------------------------------------------------------------*/

 /* ***************************  Definitions  ************************** */


 /* ********************  Preprocessor Definitions  ******************** */

 /* ************************  Function Prototypes ********************** */


 /* ***************************  Main Block  *************************** */



 /* **********************  Internal Procedures  *********************** */

 PROCEDURE pCheckFolder:
     /*------------------------------------------------------------------------------
      Purpose: Check folder path and folder exist or not 
      Notes:  
     ------------------------------------------------------------------------------*/
     DEFINE INPUT PARAMETER ipcFolderPath AS CHARACTER NO-UNDO.
     DEFINE OUTPUT PARAMETER oplExist AS LOGICAL NO-UNDO.

     FILE-INFO:FILE-NAME = ipcFolderPath .
    IF ipcFolderPath <> "" AND search(FILE-INFO:FULL-PATHNAME) eq ? THEN
        ASSIGN oplExist = NO .
    ELSE oplExist = YES .

 END PROCEDURE.

 PROCEDURE pCreateFolder:
     /*------------------------------------------------------------------------------
      Purpose: create folder if it not exist 
      Notes:
     ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFolderPath AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreate AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lCheckPathExist AS LOGICAL NO-UNDO .
    
    RUN pCheckFolder(ipcFolderPath, OUTPUT lCheckPathExist) .

    IF ipcFolderPath NE "" AND NOT lCheckPathExist THEN DO:
        OS-CREATE-DIR VALUE(ipcFolderPath).
        oplCreate = YES .
    END.

 END PROCEDURE.

 PROCEDURE pFindFileFullPath:
     /*------------------------------------------------------------------------------
      Purpose: return file full path 
      Notes:  
     ------------------------------------------------------------------------------*/
     DEFINE INPUT PARAMETER ipcFilePath AS CHARACTER NO-UNDO.
     DEFINE OUTPUT PARAMETER opcFullFilePath AS CHARACTER NO-UNDO.

     FILE-INFO:FILE-NAME = ipcFilePath   .

     ASSIGN opcFullFilePath = FILE-INFO:FULL-PATHNAME  .

 END PROCEDURE.

 PROCEDURE pFindFilePath:
     /*------------------------------------------------------------------------------
      Purpose: check file is exist or not and given a message if it not esixt 
      Notes:  
     ------------------------------------------------------------------------------*/
     DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO.
     DEFINE INPUT PARAMETER ipcNK1Name AS CHARACTER NO-UNDO.
     DEFINE INPUT PARAMETER iplMessage AS LOGICAL NO-UNDO.
     DEFINE OUTPUT PARAMETER opcFullFilePath AS CHARACTER NO-UNDO.
     DEFINE VARIABLE lCheckPathExist AS LOGICAL NO-UNDO .
     
     RUN pCheckFolder(ipcFileName, OUTPUT lCheckPathExist) .

     RUN pFindFileFullPath(ipcFileName, OUTPUT opcFullFilePath) .

     IF NOT lCheckPathExist AND ipcFileName NE "" AND iplMessage THEN
         MESSAGE  "The N-K-1 setting for " ipcNK1Name " is set to '" ipcFileName "' and that file is not there. " SKIP
         " Resolve by:" SKIP
         "1) Add the logo image to that location" SKIP 
         "2) Have your system administrator change the setting" SKIP 
         "3) Take a screen shot and submit this to Advantzware help Desk" 
         VIEW-AS ALERT-BOX INFO .


 END PROCEDURE.


 /* ************************  Function Implementations ***************** */



