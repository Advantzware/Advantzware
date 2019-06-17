/*------------------------------------------------------------------------
    File        :   getFileFullPathName.p
    Purpose     :   Returns the fully qualified path name of any file located
                    somewhere in the propath, or -if provided with a fully
                    qualified OPSYS filename - returns that filename (if found)
    Syntax      :   RUN getFileFullPathName (INPUT file-to-locate, OUTPUT fully-qualified-name)
    Description : 
    Author(s)   :   MYT 
    Created     :   Fri Apr 19 16:24:16 EDT 2019
    Notes       :   Also handles error messaging if file not found and returns null value (?)
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT PARAMETER ipcFileShortName AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER opcFileFullPathName AS CHAR NO-UNDO.

DEF VAR cTestFileName AS CHAR NO-UNDO.
DEF VAR cTestLongName AS CHAR NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
ASSIGN 
    cTestFileName = SEARCH(ipcFileShortName)
    FILE-INFO:FILE-NAME = cTestFileName
    cTestLongName = FILE-INFO:FULL-PATHNAME
    opcFileFullPathName = cTestLongName.    

IF cTestLongName EQ ? THEN MESSAGE 
    "The file " + cTestFileName + " cannot be found."
    VIEW-AS ALERT-BOX ERROR.
        
        
    
    