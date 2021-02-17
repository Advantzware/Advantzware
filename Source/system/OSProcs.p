/*------------------------------------------------------------------------
    File        : api\OSProcs.p
    Purpose     : Procedures related call OS-COMMAND function

    Syntax      :

    Description : Procedures related to Inbound API

    Author(s)   : Mithun Porandla
    Created     : Fri November 15 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* **********************  Internal Procedures  *********************** */


PROCEDURE OS_GetFullFileName:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT-OUTPUT PARAMETER iopcFileName AS CHAR NO-UNDO.
    DEF VAR cTempFileName AS CHAR NO-UNDO.
    
    ASSIGN 
        cTempFileName = iopcFileName
        FILE-INFO:FILE-NAME = cTempFileName
        cTempFileName = FILE-INFO:FULL-PATHNAME.
    IF cTempFileName = ? 
    OR cTempFileName = "" THEN ASSIGN 
        cTempFileName = SEARCH(cTempFileName).
    IF cTempFileName = ? THEN ASSIGN 
        cTempFileName = "".
    ASSIGN 
        iopcFileName = cTempFileName.

END PROCEDURE.

PROCEDURE OS_GetIPAddress:
/*------------------------------------------------------------------------------
 Purpose: Procedure to return the current machines IP Address
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcIPAddress AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cCommand    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOutputFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLine       AS CHARACTER NO-UNDO.

    /* Code to fetch the temporary directory */    
    RUN FileSys_GetTempDirectory(
        OUTPUT cOutputFile
        ) NO-ERROR.

    /* Run ipconfig command and filter the IPv4 address line to output file */
    ASSIGN
        cOutputFile = cOutputFile + "\OSCommand" + STRING(MTIME)
        cCommand    = 'ipconfig | find "IPv4" > ' 
                    + cOutputFile
        .
    
    RUN pRunOSCommandWithSilent(
        INPUT cCommand
        ).
    
    /* Read the first line in the output file */
    INPUT FROM VALUE(cOutputFile).
        IMPORT UNFORMATTED cLine.
    INPUT CLOSE. 

    ASSIGN
        opcIPAddress = ENTRY(2, cLine, ":")
        opcIPAddress = TRIM(opcIPAddress)
        .
                
    /* Delete the temporary file once command is run successfully */
    OS-DELETE VALUE(cOutputFile).    
END PROCEDURE.

PROCEDURE OS_RunCommand:
    DEFINE INPUT  PARAMETER ipcCommand    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplSilent     AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplNoWait     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage    AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cCommand            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOutputFilePath     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOutputSuppressFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValidPath          AS LOGICAL   NO-UNDO.

    /* Command validation */
    IF ipcCommand EQ "" THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Command cannot be empty"
            .
        RETURN.
    END.

    /* Output file validation */
    RUN FileSys_GetFilePath(
        INPUT  ipcOutputFile,
        OUTPUT cOutputFilePath,
        OUTPUT lValidPath,
        OUTPUT opcMessage
        ) NO-ERROR.
    IF NOT lValidPath THEN DO:
        oplSuccess = FALSE.
        RETURN.
    END.
    
    /* If output file path is valid, add output file in command to direct any output from command to file */
    cCommand = ipcCommand
             + (IF ipcOutputFile NE "" THEN " > " + ipcOutputFile ELSE "").

    /* Temporary file to suppress the output from OS-COMMAND */
    RUN FileSys_GetTempDirectory(
        OUTPUT cOutputSuppressFile
        ) NO-ERROR.

    cOutputSuppressFile = cOutputSuppressFile + "\OSCommand" + STRING(MTIME).

    /* Suppress any output from the OS-COMMAND to a temporary file - This is needed for commands like cURL
       which force the output on console, even when SILENT option is used in OS-COMMAND */
    IF iplSilent THEN
        OUTPUT TO VALUE(cOutputSuppressFile).

    /* Run OS-COMMAND with NO-WAIT option first, as SILENT AND NO-WAIT cannot run together */
    IF iplNoWait THEN
        RUN pRunOSCommandWithNoWait (
            INPUT cCommand
            ) NO-ERROR.
    ELSE IF iplSilent THEN
        RUN pRunOSCommandWithSilent (
            INPUT cCommand
            ) NO-ERROR.
    ELSE
        RUN pRunOSCommand (
            INPUT cCommand
            ) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = ERROR-STATUS:GET-MESSAGE(1)
            .

        IF iplSilent THEN DO:
            OUTPUT CLOSE.

            /* Delete the temporary file once command is run successfully */
            OS-DELETE VALUE(cOutputSuppressFile).
        END.

        RETURN.
    END.

    IF iplSilent THEN DO:
        OUTPUT CLOSE.

        /* Delete the temporary file once command is run successfully */
        OS-DELETE VALUE(cOutputSuppressFile).
    END.

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

PROCEDURE OS_RunFile:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to open a file with windows default option from OS-COMMAND.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcFile    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cCommand AS CHARACTER NO-UNDO.
    
    ipcFile = TRIM(ipcFile, ' ').
    ipcFile = TRIM(ipcFile, '"').
    
    RUN FileSys_ValidateFile(
        INPUT  ipcFile,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
        
    IF NOT oplSuccess THEN
        RETURN.
    
    ipcFile = DYNAMIC-FUNCTION (
                  "fFormatFilePath",
                  INPUT ipcFile
                  ).
    
    cCommand = '"' + ipcFile + '"'.
    
    RUN OS_RunCommand (
        INPUT  cCommand,             /* Command string to run */
        INPUT  "",                   /* File name to write the command output */
        INPUT  FALSE,                /* Run with SILENT option */
        INPUT  TRUE,                 /* Run with NO-WAIT option */
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
        
END PROCEDURE.

PROCEDURE OS_PlaySound:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to open a file with windows default option from OS-COMMAND.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcFile   AS CHARACTER.
    DEFINE OUTPUT PARAMETER opcStatus AS  LONGCHAR.
    
    DEFINE VARIABLE oplSuccess AS LOGICAL NO-UNDO.
    DEFINE VARIABLE opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE  loStatus AS LOGICAL NO-UNDO.
    
    ipcFile = TRIM(ipcFile, ' ').
    ipcFile = TRIM(ipcFile, '"').
    
    RUN FileSys_ValidateFile(
        INPUT  ipcFile,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
        
    IF NOT oplSuccess THEN
        RETURN.   
    
    RUN sndPlaySoundA (
        INPUT ipcFile, 
        INPUT 2,
        OUTPUT loStatus
        ) NO-ERROR.
        
    opcStatus = STRING(loStatus).

END PROCEDURE.

PROCEDURE pRunOSCommand PRIVATE:
    /* Procedure to run OS-COMMAND */
    DEFINE INPUT  PARAMETER ipcCommand    AS CHARACTER NO-UNDO.

    OS-COMMAND VALUE(ipcCommand).
END PROCEDURE.

PROCEDURE pRunOSCommandWithSilent PRIVATE:
    /* Procedure to run OS-COMMAND with SILENT option. This will suppress the console window from popping up*/
    DEFINE INPUT  PARAMETER ipcCommand    AS CHARACTER NO-UNDO.

    OS-COMMAND SILENT VALUE(ipcCommand).
END PROCEDURE.

PROCEDURE pRunOSCommandWithNoWait PRIVATE:
    /* In a multi-tasking environment, causes the AVM to immediately pass control back to next
       statement after the OS-COMMAND without waiting for the operating system command to terminate. */
    DEFINE INPUT  PARAMETER ipcCommand    AS CHARACTER NO-UNDO.

    OS-COMMAND NO-WAIT VALUE(ipcCommand).
END PROCEDURE.

PROCEDURE sndPlaySoundA EXTERNAL "winmm.dll" :
    DEFINE INPUT  PARAMETER ic  AS CHARACTER.
    DEFINE INPUT  PARAMETER ish AS LONG.
    DEFINE OUTPUT PARAMETER osh AS LONG.
END PROCEDURE.
