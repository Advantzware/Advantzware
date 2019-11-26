/*------------------------------------------------------------------------

  File: testers/FileSysProcsTester.p

  Description: Tester for FileSysProcs.p

  Input Parameters:

  Output Parameters:
      <none>

  Author: Porandla Mithun

  Created: 11/26/2019
------------------------------------------------------------------------*/
DEFINE VARIABLE lValid    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilePath AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdFileSysProcs AS HANDLE NO-UNDO.

RUN system\FileSysProcs.p PERSISTENT SET hdFileSysProcs.

/* Fetch the temporary directoy of the session */
RUN FileSys_GetTempDirectory IN hdFileSysProcs (
    OUTPUT cfilepath
    ).

MESSAGE "Procedure: FileSys_GetTempDirectory" SKIP
    "Temporary Directory:" cFilePath VIEW-AS ALERT-BOX.

/* Fetch the path name for a given file */
RUN FileSys_GetFilePath IN hdFileSysProcs (
    INPUT  "api\CreateAPIOutboundEvent.p",
    OUTPUT cFilePath,
    OUTPUT lValid,
    OUTPUT cMessage
    ).

MESSAGE "Procedure: FileSys_GetFilePath" SKIP
    "File Path:" cFilepath SKIP
    "Valid:" lValid SKIP
    "Message:" cMessage VIEW-AS ALERT-BOX.

/* Validate the directory */
RUN FileSys_ValidateDirectory IN hdFileSysProcs (
    INPUT  "api",
    OUTPUT lValid,
    OUTPUT cMessage
    ).

MESSAGE "Procedure: FileSys_ValidateDirectory" SKIP
    "Valid:" lValid SKIP
    "Message:" cMessage VIEW-AS ALERT-BOX.

RUN FileSys_CreateDirectory IN hdFileSysProcs (
    INPUT  "C:\Tmp\",
    OUTPUT lValid,
    OUTPUT cMessage
    ).

MESSAGE "Procedure: FileSys_CreateDirectory" SKIP
    "Valid:" lValid SKIP
    "Message:" cMessage VIEW-AS ALERT-BOX.

IF VALID-HANDLE(hdFileSysProcs) THEN
    DELETE PROCEDURE hdFileSysProcs.
