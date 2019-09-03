
/*------------------------------------------------------------------------
    File        : FileSysProcs.p
    Purpose     : Mulltiple Procs and Functions for processing Folder and files

    Syntax      :

    Description : 

    Author(s)   : Sewa Singh
    Created     : tue Sep 03  2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE CheckFolder:
    /*------------------------------------------------------------------------------
     Purpose: Check folder path and folder exist  
     Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFolderPath AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplExist AS LOGICAL NO-UNDO.

    FILE-INFO:FILE-NAME = ipcFolderPath .
   IF ipcFolderPath <> "" AND FILE-INFO:FILE-type eq ? THEN
       ASSIGN oplExist = NO .
   ELSE oplExist = YES .
    
END PROCEDURE.

PROCEDURE CreateFolder:
    /*------------------------------------------------------------------------------
     Purpose: Propagates a single note from one rec_key to another
     Notes:
    ------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ipcFolderPath AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oplCreate AS LOGICAL NO-UNDO.

   FILE-INFO:FILE-NAME = ipcFolderPath .
   IF ipcFolderPath NE "" AND FILE-INFO:FILE-type eq ? THEN DO:
       OS-CREATE-DIR VALUE(ipcFolderPath).
       oplCreate = YES .
   END.

END PROCEDURE.

PROCEDURE FindFile:
    /*------------------------------------------------------------------------------
     Purpose: Copies all notes from one source record (rec_key) to another 
     Notes:  Filter the scope of notes by type or code (commas separated or left blank for all)
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFilePath AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFullFilePath AS CHARACTER NO-UNDO.

    FILE-INFO:FILE-NAME = ipcFilePath + "\" + ipcFileName  .
    
    ASSIGN opcFullFilePath = FILE-INFO:FULL-PATHNAME  .
    
END PROCEDURE.

PROCEDURE FindFilePath:
    /*------------------------------------------------------------------------------
     Purpose: Copies all notes from one source record (rec_key) to another 
     Notes:  Filter the scope of notes by type or code (commas separated or left blank for all)
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplMessage AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFullFilePath AS CHARACTER NO-UNDO.

    FILE-INFO:FILE-NAME = ipcFileName  .
    ASSIGN opcFullFilePath = FILE-INFO:FULL-PATHNAME  .

    IF opcFullFilePath EQ ? AND ipcFileName NE "" AND iplMessage THEN
        MESSAGE  "The N-K-1 setting for BusinessFormLogo is set to " ipcFileName " and that file is not there. " SKIP
        " Resolve by:" SKIP
        "1) Add the logo image to that location" SKIP 
        "2) Have your system administrator change the setting" SKIP 
        "3) Take a screen shot and submit this to Advantzware help Desk" 
        VIEW-AS ALERT-BOX INFO .

    
END PROCEDURE.


/* ************************  Function Implementations ***************** */



