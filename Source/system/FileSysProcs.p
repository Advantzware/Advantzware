/*------------------------------------------------------------------------

  File: system/FileSysProcs.p

  Description: Procedures related to the File System

  Input Parameters:

  Output Parameters:
      <none>

  Author: Porandla Mithun

  Created: 11/25/2019
------------------------------------------------------------------------*/
&SCOPED-DEFINE WTRUE 1
&SCOPED-DEFINE WFALSE 0

DEFINE VARIABLE cFileTypeFile      AS CHARACTER NO-UNDO INITIAL "F".
DEFINE VARIABLE cFileTypeDirectory AS CHARACTER NO-UNDO INITIAL "D".
DEFINE VARIABLE cBackwardSlash     AS CHARACTER NO-UNDO INITIAL "\".
DEFINE VARIABLE cForwardSlash      AS CHARACTER NO-UNDO INITIAL "/".

FUNCTION fFormatFilePath RETURNS CHARACTER
    ( ipcFilePath AS CHARACTER ) FORWARD.

FUNCTION get64BitValue RETURNS DECIMAL
    ( INPUT m64 AS MEMPTR ) FORWARD.

PROCEDURE GetDiskFreeSpaceExA EXTERNAL "kernel32.dll" :
    DEFINE  INPUT  PARAMETER  lpDirectoryName        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT  PARAMETER  FreeBytesAvailable     AS MEMPTR    NO-UNDO.
    DEFINE OUTPUT  PARAMETER  TotalNumberOfBytes     AS MEMPTR    NO-UNDO.
    DEFINE OUTPUT  PARAMETER  TotalNumberOfFreeBytes AS MEMPTR    NO-UNDO.
    DEFINE RETURN  PARAMETER  iReturnVal                 AS LONG      NO-UNDO.
END PROCEDURE.



/* **********************  Internal Procedures  *********************** */


PROCEDURE FileSys_GetTempDirectory:
    /*------------------------------------------------------------------------------
     Purpose: Public wrapper procedure to fetch the temporary directory
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcPath AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lValid   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

    /* Fetch the user Report Path from the users table */
    RUN pGetUserReportDirectory (
        OUTPUT opcPath
        ) NO-ERROR.

    /* Validate the Report path */
    RUN pValidateDirectory (
        INPUT  opcPath,
        OUTPUT lValid,
        OUTPUT cMessage
        ) NO-ERROR.

    /* If Report path is not valid then get the temporary directory from SESSION handle */
    IF NOT lValid THEN
        RUN pGetSessionTempDirectory (
            OUTPUT opcPath
            ) NO-ERROR.
END.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FileSys_GetDiskSpace C-Win 
PROCEDURE FileSys_GetDiskSpace :
/*------------------------------------------------------------------------------
     Purpose:   Returns total/free disk space on a disk
     Notes:     If ipcDrive is blank, uses disk of current directory
                ipcDrive can be a disk/map letter or UNC directory 
                Input ipcUnit can be variants of KB,MB,GB - if blank will return number of bytes
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcDrive   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUnit    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDiskFreeSpace    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDiskTotalSpace   AS DECIMAL   NO-UNDO.
    
    DEF VAR cDivisor AS INT NO-UNDO.
    DEF VAR iMem1 AS MEMPTR NO-UNDO.
    DEF VAR iMem2 AS MEMPTR NO-UNDO.
    DEF VAR iMem3 AS MEMPTR NO-UNDO.
    DEF VAR iReturnVal AS INT NO-UNDO.
    DEF VAR iDiskFreeSpace AS DECIMAL NO-UNDO.
    DEF VAR iDiskTotalSpace AS DECIMAL NO-UNDO.

    IF CAN-DO("KB,Kilo,Kilobyte,Kilobytes", ipcUnit)
        THEN cDivisor = 1024.
    ELSE
        IF CAN-DO("MB,Mega,Megabyte,Megabytes", ipcUnit)
            THEN cDivisor = 1024 * 1024.
        ELSE
            IF CAN-DO("GB,Giga,Gigabyte,Gigabytes", ipcUnit)
                THEN cDivisor = 1024 * 1024 * 1024.
            ELSE cDivisor = 1.
 
    /* No directory specified? Then use the current directory */
    IF (ipcDrive = "") OR (ipcDrive=?) THEN 
    DO:
        FILE-INFO:FILE-NAME = ".".
        ipcDrive = FILE-INFO:FULL-PATHNAME.
    END.
 
    /* If a UNC name was specified, make sure it ends with a backslash ( \\drive\share\dir\ )
       This won't hurt for a mapped drive too */
    IF SUBSTR(ipcDrive, LENGTH(ipcDrive), 1) NE "\"
        THEN ipcDrive = ipcDrive + "\".
 
    SET-SIZE(iMem1) = 8.  /* 64 bit integer! */
    SET-SIZE(iMem2) = 8.
    SET-SIZE(iMem3) = 8.
 
    RUN GetDiskFreeSpaceExA ( ipcDrive + CHR(0),
        OUTPUT iMem1,
        OUTPUT iMem2,
        OUTPUT iMem3,
        OUTPUT iReturnVal  ).
    IF iReturnVal NE {&WTRUE} THEN 
    DO:
        iDiskFreeSpace = ?.
        iDiskTotalSpace = ?.
    END.
    ELSE 
    DO:
        ASSIGN
            iDiskFreeSpace  = TRUNC( get64BitValue(iMem3) / cDivisor, 3)
            iDiskTotalSpace = TRUNC( get64BitValue(iMem2) / cDivisor, 3)
            opdDiskFreeSpace = iDiskFreeSpace
            opdDiskTotalSpace = iDiskTotalSpace.
    END.
 
    SET-SIZE(iMem1) = 0.
    SET-SIZE(iMem2) = 0.
    SET-SIZE(iMem3) = 0.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

PROCEDURE FileSys_GetFilePath:
    /*------------------------------------------------------------------------------
     Purpose: Public wrapper procedure to return the absolute path of the input file name
     Notes: This procedure will take the absolute or relative file name as input and 
            returns the absolute file path of the input file
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcFilePathName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFilePath     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid        AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.

    RUN pGetFilePath (
        INPUT  ipcFilePathName,
        OUTPUT opcFilePath,
        OUTPUT oplValid,
        OUTPUT opcMessage
        ) NO-ERROR.
END PROCEDURE.

PROCEDURE FileSys_CreateDirectory:
    /*------------------------------------------------------------------------------
     Purpose: Public wrapper procedure to create directory. 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcPath    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    RUN pCreateDirectory (
        INPUT  ipcPath,
        OUTPUT oplCreated,
        OUTPUT opcMessage
        ) NO-ERROR.
END PROCEDURE.

PROCEDURE FileSys_GetUniqueFileName:
/*------------------------------------------------------------------------------
 Purpose: Returns a unique file name in a given directory 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcFullFilePath          AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER iplAutoIncrementFileName AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFullFilePath          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess               AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage               AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFilePath AS CHARACTER NO-UNDO.    
    
    ASSIGN 
        cFileName = fFormatFilePath(ipcFullFilePath)
        cFilePath = cFileName
        .
    
    RUN pGetFilePath (
        INPUT  ipcFullFilePath,
        OUTPUT cFilePath,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
    
    /* Below code gets the file name from file path name */
    cFileName = ENTRY(NUM-ENTRIES(cFileName,cBackwardSlash),cFileName,cBackwardSlash) NO-ERROR.
         
    IF NOT iplAutoIncrementFileName THEN    
        opcFullFilePath = ipcFullFilePath.     
    ELSE       
        RUN pGetUniqueFileName (
            INPUT  cFilePath,
            INPUT  cFileName,
            INPUT  TRUE,    /* Create directory */
            INPUT  FALSE,   /* Create file */  
            INPUT  " (",    /* File count prefix */
            INPUT  ")",     /* File count suffix */
            OUTPUT opcFullFilePath,
            OUTPUT oplSuccess,
            OUTPUT opcMessage 
            ).

END PROCEDURE.

PROCEDURE FileSys_ValidateDirectory:
    /*------------------------------------------------------------------------------
     Purpose: Public wrapper procedure to validate the input directory
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcPath    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    RUN pValidateDirectory (
        INPUT  ipcPath,
        OUTPUT oplValid,
        OUTPUT opcMessage
        ) NO-ERROR.
END PROCEDURE.

PROCEDURE FileSys_ValidateFile:
    /*------------------------------------------------------------------------------
     Purpose: Public wrapper procedure to validate the input file
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcFileName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.

    RUN pValidateFile (
        INPUT  ipcFileName,
        OUTPUT oplValid,
        OUTPUT opcMessage
        ) NO-ERROR.
END PROCEDURE.

PROCEDURE FileSys_CreateFile:
    /*------------------------------------------------------------------------------
     Purpose: Public wrapper procedure to validate the input file
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcFileName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.

    RUN pCreateFile (
        INPUT  ipcFileName,
        INPUT  TRUE,    /* Create directory if not available */
        OUTPUT oplValid,
        OUTPUT opcMessage
        ) NO-ERROR.
END PROCEDURE.

PROCEDURE pGetSessionTempDirectory PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch the TEMP-DIRECTORY attribute from SESSION handle
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcPath AS CHARACTER NO-UNDO.

    FILE-INFO:FILE-NAME = SESSION:TEMP-DIRECTORY.

    opcPath = fFormatFilePath (SESSION:TEMP-DIRECTORY).
END.

PROCEDURE pGetUniqueFileName:
/*------------------------------------------------------------------------------
 Purpose: Returns a unique file name 
 Notes:
    INPUT  ipcFilePath        - Path of the file name. Mandatory. Non-empty
    INPUT  ipcFileName        - File name. Mandatory. Non-empty
    INPUT  iplCreateDir       - Create directory? TRUE/FALSE
    INPUT  iplCreateFile      - Create file? TRUE/FALSE
    INPUT  ipcFileCountPrefix - Prefix to the file count, if an input file already exists
    INPUT  ipcFileCountSuffix - Suffix to the file count, if an input file already exists
    OUTPUT opcFullFilePath    - Full file path
    OUTPUT oplSuccess         - Success flag
    OUTPUT opcMessage         - Error messages
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcFilePath        AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER ipcFileName        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplCreateDir       AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplCreateFile      AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFileCountPrefix AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFileCountSuffix AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFullFilePath    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage         AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFilePath     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFileName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFileNameExt  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFullFilePath AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iFileCount    AS INTEGER   NO-UNDO INITIAL 1.
    
    /* Validate empty file path */
    IF ipcFilePath EQ "" THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Empty file path"
            .
        RETURN.     
    END.

    /* Validate empty file name */
    IF ipcFileName EQ "" THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Empty file name"
            .
        RETURN.     
    END.    

    /* If iplCreateDir is false and file path does not exist return with error */
    IF NOT iplCreateDir THEN DO:
        RUN pValidateDirectory (
            INPUT  ipcFilePath,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
        
        IF NOT oplSuccess THEN
            RETURN.
    END.

    ASSIGN
        cFilePath = IF SUBSTRING(ipcFilePath, LENGTH(ipcFilePath), 1) EQ "/" OR
                       SUBSTRING(ipcFilePath, LENGTH(ipcFilePath), 1) EQ "\" THEN
                        SUBSTRING(ipcFilePath, 1, LENGTH(ipcFilePath) - 1)
                    ELSE
                        ipcFilePath
        cFileName = ipcFileName
        .

    /* No file exists with given file name in file path */
    IF SEARCH(cFilePath + "\" + cFileName) EQ ? THEN
        cFullFilePath = cFilePath + "\" + cFileName.
    ELSE DO:
        /* Extract the file name extension */
        IF INDEX(ipcFileName, ".") GT 0 THEN
            cFileNameExt = "." + ENTRY(NUM-ENTRIES(ipcFileName, "."), ipcFileName, ".").
        
        /* Remove the file name extension from file name. Will be appended later */
        IF cFileNameExt NE "" THEN
            cFileName = REPLACE(cFileName, cFileNameExt, "").

        cFullFilePath = cFilePath + "\" + cFileName 
                      + ipcFileCountPrefix + STRING(iFileCount) + ipcFileCountSuffix
                      + cFileNameExt.

        /* Search if the given file exists, if already available increment the file count */
        DO WHILE SEARCH(cFullFilePath) NE ?:
            ASSIGN
                iFileCount    = iFileCount + 1
                cFullFilePath = cFilePath + "\" + cFileName 
                              + ipcFileCountPrefix + STRING(iFileCount) + ipcFileCountSuffix
                              + cFileNameExt
                .            
        END.
    END.

    opcFullFilePath = cFullFilePath.
    
    /* pCreateFile procedure will automatically create the directory with file,
       so verify if iplCreateFile is false to avoid a procedure call */
    IF iplCreateDir AND NOT iplCreateFile THEN DO:
        RUN pCreateDirectory (
            INPUT  cFilePath,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
        
        IF NOT oplSuccess THEN
            RETURN.
    END.

    /* Create file. This will reserve the file for use as well */
    IF iplCreateFile THEN
        RUN pCreateFile (
            INPUT  opcFullFilePath,
            INPUT  TRUE,            /* Create directory */
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
    ELSE
        ASSIGN
            oplSuccess = TRUE
            opcMessage = "Success"
            .
END PROCEDURE.

PROCEDURE pGetUserReportDirectory PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch the user's Reports Path from users table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcPath AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-users FOR users.
    
    FIND FIRST bf-users
         WHERE bf-users.user_id EQ USERID("ASI")
         NO-LOCK NO-ERROR.
    IF AVAILABLE bf-users AND bf-users.user_program[2] NE "" THEN
        opcPath = bf-users.user_program[2].

    opcPath = fFormatFilePath (opcPath).

    RELEASE bf-users.
END.

PROCEDURE pGetUserDocumentDirectory PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch the user's Document Path from users table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcPath AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-users FOR users.
    
    FIND FIRST bf-users
         WHERE bf-users.user_id EQ USERID("ASI")
         NO-LOCK NO-ERROR.
    IF AVAILABLE bf-users AND bf-users.user_program[3] NE "" THEN
        opcPath = bf-users.user_program[3].

    opcPath = fFormatFilePath (opcPath).

    RELEASE bf-users.
END.

PROCEDURE pGetFilePath PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch absolute path name of the given input file name
     Notes: Eg. INPUT file path name = "C:\Tmp\foo.txt", OUTPUT path name = "C:\Tmp"
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcFilePathName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFilePath     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid        AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.

    /* Format the file path name */
    opcFilePath = fFormatFilePath(ipcFilePathName).

    /* Validate if the input file path name is a directory */
    RUN pValidateDirectory (
        INPUT  opcFilePath,
        OUTPUT oplValid,
        OUTPUT opcMessage
        ) NO-ERROR.
    /* If valid directory then return */
    IF oplValid THEN DO:
        opcMessage  = "Valid path".
        RETURN.
    END.

    /* Extract path name from the given input file */
    IF NUM-ENTRIES(opcFilePath, cBackwardSlash) GT 1 THEN DO:
        /* Replace the file name portion from the input file */
        opcFilePath = REPLACE(opcFilePath, ENTRY(NUM-ENTRIES(opcFilePath, cBackwardSlash), opcFilePath, cBackwardSlash), "").

        /* Validate if the extracted path is a directory */
        RUN pValidateDirectory (
            INPUT  opcFilePath,
            OUTPUT oplValid,
            OUTPUT opcMessage
            ) NO-ERROR.

        IF NOT oplValid THEN
            RETURN.
    END.

    ASSIGN
        oplValid    = TRUE
        opcMessage  = "Valid path"
        opcFilePath = FILE-INFO:FULL-PATHNAME
        .
END PROCEDURE.

PROCEDURE pValidateFile PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to validate the input file
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcFileName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.

    /* Validate if input file name is not empty */
    IF ipcFileName EQ "" THEN DO:
        ASSIGN
            oplValid   = FALSE
            opcMessage = "Empty File"
            .
        RETURN.
    END.

    ASSIGN
        oplValid   = FALSE
        opcMessage = "Invalid File"
        .

    FILE-INFO:FILE-NAME = ipcFileName.
    /* Validate if file name is valid and of file type */
    IF FILE-INFO:FULL-PATHNAME NE ? AND FILE-INFO:FILE-TYPE BEGINS cFileTypeFile THEN
        ASSIGN
            oplValid   = TRUE
            opcMessage = "Valid"
            .
END PROCEDURE.

PROCEDURE pCreateFile PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to create an empty file with the input file name
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcFileName  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplCreateDir AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFilePath  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValidPath AS LOGICAL   NO-UNDO.

    /* Validate if the input file already exists */
    RUN pValidateFile (
        INPUT  ipcFileName,
        OUTPUT oplCreated,
        OUTPUT opcMessage
        ) NO-ERROR.
    IF oplCreated THEN
        RETURN.

    /* Fetch the absolute path of the input file name */
    RUN pGetFilePath (
        INPUT  ipcFileName,
        OUTPUT cFilePath,
        OUTPUT lValidPath,
        OUTPUT opcMessage
        ) NO-ERROR.

    /* If not a valid path, auto create directory if iplCreateDir is TRUE */
    IF NOT lValidPath AND iplCreateDir THEN
        RUN pCreateDirectory (
            INPUT  cFilePath,
            OUTPUT lValidPath,
            OUTPUT opcMessage
            ) NO-ERROR.

    /* If a valid path is available, create an empty file */
    IF lValidPath THEN DO:
        OUTPUT TO VALUE(ipcFileName).
        OUTPUT CLOSE.
    END.
    
    /* Validate if file is created */
    RUN pValidateFile (
        INPUT  ipcFileName,
        OUTPUT oplCreated,
        OUTPUT opcMessage
        ) NO-ERROR.
    IF oplCreated THEN
        RETURN.  
END PROCEDURE.

PROCEDURE pValidateDirectory PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to validate directory
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcPath    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    ASSIGN
        oplValid   = FALSE
        opcMessage = "Invalid directory"
        .
    /* Validate if input path is empty */
    IF ipcPath EQ "" THEN
        RETURN.

    FILE-INFO:FILE-NAME = ipcPath.
    /* Validate if directory is valid and of type directory */
    IF FILE-INFO:FULL-PATHNAME NE ? AND FILE-INFO:FILE-TYPE BEGINS cFileTypeDirectory THEN
        ASSIGN
            oplValid   = TRUE
            opcMessage = "Valid path"
            .
END PROCEDURE.

PROCEDURE pCreateDirectory PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to create directory for the given input
     Notes: This procedure should also allow to create multi level deep directories 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcPath    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cPath1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPath2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER   NO-UNDO.
    
    /* Validate if given directory already exists, if one exists then return */
    RUN pValidateDirectory (
        INPUT  ipcPath,
        OUTPUT oplCreated,
        OUTPUT opcMessage
        ) NO-ERROR.

    IF oplCreated THEN
        RETURN.

    cPath1 = fFormatFilePath(ipcPath).
    
    /* Fetch the initial directory of the input file path. This validation is required
       as we need initial directory to be always available.
       Eg. INPUT file path = "C:\Tmp\foo", OUTPUT = "C:". */
    FILE-INFO:FILE-NAME = ENTRY(1, cPath1, cBackwardSlash).
    
    /* If initial directory is not available then return, as we cannot create any further directories
       in the path */
    IF FILE-INFO:FULL-PATHNAME EQ ? OR NOT FILE-INFO:FILE-TYPE BEGINS cFileTypeDirectory THEN DO:
        ASSIGN
            oplCreated = FALSE
            opcMessage = "Invalid initial directory"
            .
        RETURN.
    END.
    
    /* iterate through all the directories in the input file path */
    DO iCount = 1 TO NUM-ENTRIES(cPath1,cBackwardSlash):
        cPath2 = cPath2 + ENTRY(iCount, cPath1, cBackwardSlash) + cBackwardSlash.
        
        /* Validate if any of the parent directories are not available.
           If not available then create */
        FILE-INFO:FILE-NAME = cPath2.

        IF FILE-INFO:FULL-PATHNAME EQ ? THEN
            OS-CREATE-DIR VALUE(cPath2).

        /* Valiadte if directory is created successfully, return in any error */
        FILE-INFO:FILE-NAME = cPath2.

        IF FILE-INFO:FULL-PATHNAME EQ ? THEN DO:
            ASSIGN
                oplCreated = FALSE
                opcMessage = "Error creating directory"
                .
            RETURN.
        END.
    END.

    /* Validate if directory is created */
    IF FILE-INFO:FULL-PATHNAME NE ? THEN
        ASSIGN
            oplCreated = TRUE
            opcMessage = "Directory " + ipcPath + " created"
            .
END PROCEDURE.

FUNCTION fFormatFilePath RETURNS CHARACTER
  ( ipcFilePath AS CHARACTER ) :
    /*------------------------------------------------------------------------------
     Purpose: Function to format the file path name.
     Notes: This function will replace the forward slashes with backward slashes and
            will remove the last backward slash if a valid file path is provided
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFormattedFilePath AS CHARACTER NO-UNDO.

    cFormattedFilePath = REPLACE(ipcFilePath, cForwardSlash, cBackwardSlash).
    
    /* Validate the directory and remove any additional back slash */
    FILE-INFO:FILE-NAME = cFormattedFilePath.
    
    IF FILE-INFO:FULL-PATHNAME NE ? THEN
        cFormattedFilePath = FILE-INFO:FULL-PATHNAME.

    RETURN cFormattedFilePath.
END FUNCTION.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get64BitValue C-Win 
FUNCTION get64BitValue RETURNS DECIMAL
    ( INPUT m64 AS MEMPTR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* constant 2^32 */
    &SCOPED-DEFINE BigInt 4294967296
 
    DEFINE VARIABLE d1 AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE d2 AS DECIMAL    NO-UNDO.
 
    d1 = GET-LONG(m64, 1).
    IF d1 < 0 
        THEN d1 = d1 + {&BigInt}.
 
    d2 = GET-LONG(m64, 5).
    IF d2 < 0 
        THEN d2 = d2 + {&BigInt}.
 
    IF d2 GT 0
        THEN d1 = d1 + (d2 * {&BigInt}).
 
    RETURN d1.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

