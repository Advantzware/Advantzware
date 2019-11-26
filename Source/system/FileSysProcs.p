/*------------------------------------------------------------------------

  File: system/FileSysProcs.p

  Description: Procedures related to the File System

  Input Parameters:

  Output Parameters:
      <none>

  Author: Porandla Mithun

  Created: 11/25/2019
------------------------------------------------------------------------*/
DEFINE VARIABLE cFileTypeFile      AS CHARACTER NO-UNDO INITIAL "F".
DEFINE VARIABLE cFileTypeDirectory AS CHARACTER NO-UNDO INITIAL "D".
DEFINE VARIABLE cBackwardSlash     AS CHARACTER NO-UNDO INITIAL "\".
DEFINE VARIABLE cForwardSlash      AS CHARACTER NO-UNDO INITIAL "/".

FUNCTION fFormatFilePath RETURNS CHARACTER PRIVATE
    ( ipcFilePath AS CHARACTER ) FORWARD.

PROCEDURE FileSys_GetTempDirectory:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch the temporary directory from SESSION handle
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcPath AS CHARACTER NO-UNDO.

    RUN pGetTempDirectory (
        OUTPUT opcPath
        ) NO-ERROR.
END.

PROCEDURE FileSys_GetFilePath:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to return the absolute path of the input file name
     Notes:
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
     Purpose: Procedure to create directory. This will also allow to create directory
              with multiple levels
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

PROCEDURE FileSys_ValidateDirectory:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to validate the input directory
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

PROCEDURE pGetTempDirectory PRIVATE:
    DEFINE OUTPUT PARAMETER opcPath AS CHARACTER NO-UNDO.

    FILE-INFO:FILE-NAME = SESSION:TEMP-DIRECTORY.

    IF FILE-INFO:FULL-PATHNAME NE ? AND FILE-INFO:FILE-TYPE BEGINS cFileTypeDirectory THEN
        opcPath = FILE-INFO:FULL-PATHNAME.
END.

PROCEDURE pGetFilePath PRIVATE:
    DEFINE INPUT  PARAMETER ipcFilePathName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFilePath     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid        AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.

    IF ipcFilePathName  EQ "" THEN DO:
        ASSIGN
            oplValid   = FALSE
            opcMessage = "Empty file name"
            .
        RETURN.
    END.

    /* Replace all forward slashes with backward slash */
    ipcFilePathName = fFormatFilePath(ipcFilePathName).

    FILE-INFO:FILE-NAME = ipcFilePathName.

    /* Validate if the given output file is a directory */
    IF FILE-INFO:FILE-TYPE BEGINS cFileTypeDirectory THEN DO:
        ASSIGN
            oplValid    = TRUE
            opcMessage  = "Valid path"
            opcFilePath = FILE-INFO:FULL-PATHNAME
            .
        RETURN.
    END.

    /* Validate if the output file's path is valid */
    IF NUM-ENTRIES(ipcFilePathName, cBackwardSlash) GT 1 THEN DO:
        /* Extract output path from the given output file */
        opcFilePath = REPLACE(ipcFilePathName, ENTRY(NUM-ENTRIES(ipcFilePathName, cBackwardSlash), ipcFilePathName, cBackwardSlash), "").

        FILE-INFO:FILE-NAME = opcFilePath.

        IF FILE-INFO:FULL-PATHNAME EQ ? THEN DO:
            ASSIGN
                oplValid   = FALSE
                opcMessage = "Output file path is not valid"
                .
            RETURN.
        END.
    END.

    ASSIGN
        oplValid    = TRUE
        opcMessage  = "Valid path"
        opcFilePath = FILE-INFO:FULL-PATHNAME
        .
END PROCEDURE.

PROCEDURE pValidateFile PRIVATE:
    DEFINE INPUT  PARAMETER ipcFileName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.

    IF ipcFileName EQ "" THEN DO:
        ASSIGN
            oplValid   = FALSE
            opcMessage = "Empty File"
            .
        RETURN.
    END.

    FILE-INFO:FILE-NAME = ipcFileName.

    IF FILE-INFO:FULL-PATHNAME NE ? AND FILE-INFO:FILE-TYPE BEGINS cFileTypeFile THEN
        ASSIGN
            oplValid   = TRUE
            opcMessage = "Valid"
            .
END PROCEDURE.

PROCEDURE pCreateFile PRIVATE:
    DEFINE INPUT  PARAMETER ipcFileName  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplCreateDir AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFilePath  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValidPath AS LOGICAL   NO-UNDO.

    RUN pValidateFile (
        INPUT  ipcFileName,
        OUTPUT oplCreated
        ) NO-ERROR.
    IF oplCreated THEN
        RETURN.

    RUN pGetFilePath (
        INPUT  ipcFileName,
        OUTPUT cFilePath,
        OUTPUT lValidPath,
        OUTPUT opcMessage
        ) NO-ERROR.

    IF NOT lValidPath AND iplCreateDir THEN
        RUN pCreateDirectory (
            INPUT  cFilePath,
            OUTPUT lValidPath,
            OUTPUT opcMessage
            ) NO-ERROR.

    IF lValidPath THEN DO:
        OUTPUT TO VALUE(ipcFileName).
        OUTPUT CLOSE.
    END.
END PROCEDURE.

PROCEDURE pValidateDirectory PRIVATE:
    DEFINE INPUT  PARAMETER ipcPath    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    ASSIGN
        oplValid   = FALSE
        opcMessage = "Invalid directory"
        .

    IF ipcPath EQ "" THEN
        RETURN.

    FILE-INFO:FILE-NAME = ipcPath.

    IF FILE-INFO:FULL-PATHNAME NE ? AND FILE-INFO:FILE-TYPE BEGINS cFileTypeDirectory THEN
        ASSIGN
            oplValid   = TRUE
            opcMessage = "Valid path"
            .
END PROCEDURE.

PROCEDURE pCreateDirectory PRIVATE:
    DEFINE INPUT  PARAMETER ipcPath    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cPath1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPath2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER   NO-UNDO.

    RUN pValidateDirectory (
        INPUT  ipcPath,
        OUTPUT oplCreated,
        OUTPUT opcMessage
        ) NO-ERROR.

    IF oplCreated THEN
        RETURN.

    cPath1 = fFormatFilePath(ipcPath).

    FILE-INFO:FILE-NAME = ENTRY(1, cPath1, cBackwardSlash).

    IF FILE-INFO:FULL-PATHNAME EQ ? OR NOT FILE-INFO:FILE-TYPE BEGINS cFileTypeDirectory THEN DO:
        ASSIGN
            oplCreated = FALSE
            opcMessage = "Invalid initial directory"
            .
        RETURN.
    END.

    DO iCount = 1 TO NUM-ENTRIES(cPath1,cBackwardSlash):
        cPath2 = cPath2 + ENTRY(iCount, cPath1, cBackwardSlash) + cBackwardSlash.

        FILE-INFO:FILE-NAME = cPath2.

        IF FILE-INFO:FULL-PATHNAME EQ ? THEN
            OS-CREATE-DIR VALUE(cPath2).

        FILE-INFO:FILE-NAME = cPath2.

        IF FILE-INFO:FULL-PATHNAME EQ ? THEN DO:
            ASSIGN
                oplCreated = FALSE
                opcMessage = "Error creating directory"
                .
            RETURN.
        END.
    END.

    IF FILE-INFO:FULL-PATHNAME NE ? THEN
        ASSIGN
            oplCreated = TRUE
            opcMessage = "Directory " + ipcPath + " created"
            .
END PROCEDURE.

FUNCTION fFormatFilePath RETURNS CHARACTER PRIVATE
  ( ipcFilePath AS CHARACTER ) :
    DEFINE VARIABLE cFormattedFilePath AS CHARACTER NO-UNDO.

    cFormattedFilePath = REPLACE(ipcFilePath, cForwardSlash, cBackwardSlash).

    RETURN cFormattedFilePath.
END FUNCTION.