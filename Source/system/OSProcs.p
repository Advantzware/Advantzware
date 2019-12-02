/*------------------------------------------------------------------------
    File        : api\OSProcs.p
    Purpose     : Procedures related call OS-COMMAND function

    Syntax      :

    Description : Procedures related to Inbound API

    Author(s)   : Mithun Porandla
    Created     : Fri November 15 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE VARIABLE hdFileSysProcs AS HANDLE NO-UNDO.

RUN system/FileSysProcs.p PERSISTENT SET hdFileSysProcs.

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
    RUN FileSys_GetFilePath IN hdFileSysProcs (
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
    RUN FileSys_GetTempDirectory IN hdFileSysProcs (
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
