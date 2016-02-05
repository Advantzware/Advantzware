&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : DocList.p
    Purpose     : CMP Version of Doc List.

    Syntax      :return-value

    Description : Controls Printing of all Quote,Dec,Notice.

    Author(s)   :
    Created     :
    Notes       : Added Loss Control, uses output9.
    
    Modified    : May 20, 2008
    Modified By : Dave Olsen
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

    /* Make this procedure available to everyone */
    SESSION:ADD-SUPER-PROCEDURE(THIS-PROCEDURE).

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE RVal (IF RETURN-VALUE <> ? THEN RETURN-VALUE ELSE "")

{ CMsrc/Common/TableDefs/InBatchRequest.i }
{ CMsrc/Common/TableDefs/InBatchReqOutput.i }
{ CMsrc/Common/TableDefs/InDocList.i      }

DEFINE TEMP-TABLE ttFormsControl NO-UNDO LIKE FormsControl
    FIELD PrintStatus AS CHARACTER FORMAT "x(1)"
    FIELD ListStatus  AS CHARACTER FORMAT "x(8)"
    FIELD DeclOn      AS INTEGER   FORMAT "->>9".

/* ***************************  Parameters  ************************** */

&IF DEFINED(UIB_is_Running) NE 0 &THEN
  RUN BuildTestData IN THIS-PROCEDURE.
&ELSE
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttBatchRequest.
&ENDIF

/* ***************************  Variables **************************** */
  DEFINE VARIABLE hDocMethods      AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hSharedMethods   AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hDocListMethods  AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hDocListMethods2 AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hCommonMethods1  AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hCommonMethods2  AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hData            AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iDocSequence     AS INTEGER    NO-UNDO.
  
  DEFINE VARIABLE lPolicy          AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lPolicyRecords   AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lQuote           AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lQuoteRecords    AS LOGICAL    NO-UNDO.
  
  DEFINE VARIABLE lDebugDocList    AS LOGICAL    NO-UNDO.
  
  DEFINE VARIABLE cErrors          AS CHARACTER  NO-UNDO.
  
  DEFINE VARIABLE iCurrentPage     AS INTEGER    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-BoldOff) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BoldOff Procedure 
FUNCTION BoldOff RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BoldOn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BoldOn Procedure 
FUNCTION BoldOn RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkDb) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkDb Procedure 
FUNCTION checkDb RETURNS CHARACTER
    ( pcDbName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CheckFile Procedure 
FUNCTION CheckFile RETURNS CHARACTER
    ( pcFileName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetErrors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetErrors Procedure 
FUNCTION GetErrors RETURNS CHARACTER
  ( pcErrorMsg as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LineFeed) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LineFeed Procedure 
FUNCTION LineFeed RETURNS CHARACTER
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RemoveSpaces) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RemoveSpaces Procedure 
FUNCTION RemoveSpaces RETURNS CHARACTER
  ( pcText AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReturnAnchor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReturnAnchor Procedure 
FUNCTION ReturnAnchor RETURNS CHARACTER
  (piLineNumber AS INTEGER, phProgram AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 17.62
         WIDTH              = 49.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE objWriteError AS CLASS PSsrc.cls.writeErrorClass NO-UNDO.

DO:

    RUN PolicyQuote IN THIS-PROCEDURE NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
    DO:
      cErrors = RETURN-VALUE.
      
      RUN CleanUp NO-ERROR.

      IF checkDb("insweb") = "yes" THEN
      DO:
        objWriteError = new PSsrc.cls.writeErrorClass().
        ASSIGN cErrors = objWriteError:saveError(cErrors, "") NO-ERROR.
      END.


      RETURN ERROR cErrors.
      
    END.

    RUN LoadMethods IN THIS-PROCEDURE NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
    DO:
      cErrors = RETURN-VALUE.
      
      RUN CleanUp NO-ERROR.

      IF checkDb("insweb") = "yes" THEN
      DO:
        objWriteError = new PSsrc.cls.writeErrorClass().
        ASSIGN cErrors = objWriteError:saveError(cErrors, "") NO-ERROR.
      END.

      
      RETURN ERROR cErrors.
      
    END.
    
    RUN CreateDocList IN THIS-PROCEDURE NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
      cErrors = RETURN-VALUE.
      
      RUN CleanUp NO-ERROR.

      IF checkDb("insweb") = "yes" THEN
      DO:
        objWriteError = new PSsrc.cls.writeErrorClass().
        ASSIGN cErrors = objWriteError:saveError(cErrors, "") NO-ERROR.
      END.

      
      RETURN ERROR cErrors.
      
    END.

    RUN CleanUp NO-ERROR.
    
END. /* DO: */

FINALLY:
  IF CONNECTED("insweb") THEN
  DO:
    DISCONNECT insweb NO-ERROR.
  END.
  IF objWriteError <> ? THEN
    DELETE OBJECT objWriteError.
  objWriteError = ?.
END FINALLY.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BuildTestData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildTestData Procedure 
PROCEDURE BuildTestData :
/*------------------------------------------------------------------------------
  Purpose:     Creates a stand alone test temp table record.
  Parameters:  <none>
  Notes:       Will not compile into source code only available
               when run from the app builder.
------------------------------------------------------------------------------*/
&IF DEFINED(UIB_is_Running) NE 0  &THEN

    DEFINE VARIABLE cTime       AS CHARACTER FORMAT "99:99:99"     NO-UNDO.
    DEFINE VARIABLE cDebugFile  AS CHARACTER                       NO-UNDO.
    DEFINE VARIABLE dDate       AS DATE                            NO-UNDO.

    RUN PSsrc/Cm/Decs/Common/Procs/BatchTimeW.w (INPUT-OUTPUT cTime,
                                                 INPUT-OUTPUT dDate,
                                                 OUTPUT cDebugFile) NO-ERROR.

    IF cTime <> "" AND dDate <> ? THEN
    DO:
      
      FIND LAST BatchRequest WHERE 
        STRING(Batchrequest.RequestTime,"HH:MM:SS") = cTime AND
        Batchrequest.RequestDate = dDate
        NO-LOCK NO-ERROR.
    
    END.
    ELSE 
      
      FIND FIRST BatchRequest NO-LOCK NO-ERROR.

    IF AVAILABLE batchrequest THEN
    DO:
        CREATE ttbatchrequest.
        BUFFER-COPY batchrequest TO ttbatchrequest
          ASSIGN
          ttBatchRequest.DebugFileName  = cDebugFile.
    END.

    ASSIGN ldebugdoclist = FALSE.

    MESSAGE "Do you want to step through this procedure?"
        VIEW-AS ALERT-BOX
        QUESTION
        BUTTONS YES-NO
        UPDATE ldebugdoclist.

&ENDIF

END PROCEDURE.   /* BuildTestData */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CleanUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CleanUp Procedure 
PROCEDURE CleanUp :
/*------------------------------------------------------------------------------
  Purpose:    Remove Super Procedures. 
  Parameters:  <none>
  Notes:      Should not remove and ADE or ADM2 Super Procs.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hProc       AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hDelProc    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cTemp       AS CHARACTER  NO-UNDO.
  
  
  /* Clean Up the internal temp table */

  
  EMPTY TEMP-TABLE ttDocList      NO-ERROR.
  EMPTY TEMP-TABLE ttFormsControl NO-ERROR.


  /* Clean Up the Data Store */
  IF VALID-HANDLE(hData) THEN
  DO:
    RUN CleanUp IN hData NO-ERROR.
    DELETE PROCEDURE hData NO-ERROR.
    
  END.
  /* Clean up the App Server Programs. */

  ASSIGN hProc = SESSION:FIRST-PROCEDURE.
        
  DO WHILE hProc <> ?:
  
    IF LOOKUP("DelProcHandle",hProc:INTERNAL-ENTRIES) > 0 THEN 
       RUN DelProcHandle IN hProc (INPUT "$ALL$") NO-ERROR.
            
   
            
    ASSIGN hProc = hProc:NEXT-SIBLING.
  
  END.   /* DO WHILE hProc <> ? */


  ASSIGN hProc = SESSION:FIRST-PROCEDURE.
  
  /* Clean up the App Server */

  DO WHILE hProc <> ?:
  
    IF hProc:FILE-NAME = "PSsrc/Common/Procs/AsUtilsCustom.p" AND 
       LOOKUP("AppServerDisconnect",hProc:INTERNAL-ENTRIES) > 0 THEN 
       RUN AppServerDisconnect IN hProc (INPUT "insserver") NO-ERROR.
            
   
            
    ASSIGN hProc = hProc:NEXT-SIBLING.
  
  END.   /* DO WHILE hProc <> ? */


  ASSIGN
  cTemp = DYNAMIC-FUNCTION('StopSuperProcedure',"PSsrc/Common/Procs/AsUtilsCustom.p")  NO-ERROR.

  

  ASSIGN hProc = SESSION:FIRST-PROCEDURE.

  FILEBLK:
  DO WHILE hProc <> ?:

    IF hProc = THIS-PROCEDURE THEN 
    DO:

      ASSIGN hProc = hProc:NEXT-SIBLING.
      NEXT FILEBLK.

    END.   /* IF hProc = THIS-PROCEDURE */

    IF CAN-DO(SESSION:SUPER-PROCEDURES,STRING(hProc)) AND
       NOT CAN-DO(hProc:INTERNAL-ENTRIES, "ADEPersistent") THEN 
    DO:

      hDelProc = hProc.
      hProc    = hProc:NEXT-SIBLING.

      SESSION:REMOVE-SUPER-PROCEDURE(hDelProc).
      DELETE PROCEDURE hDelProc.

      NEXT FILEBLK.

    END.   /* IF CAN-DO(SESSION: etc... */

    ASSIGN hProc = hProc:NEXT-SIBLING.

  END.   /* FILEBLK */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateDocList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateDocList Procedure 
PROCEDURE CreateDocList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  RUN SetOutFiles IN THIS-PROCEDURE  NO-ERROR.
  IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.

  RUN DocLoop IN THIS-PROCEDURE  NO-ERROR.
  IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.

  /* debug code */    

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
      ASSIGN ldebugdoclist = FALSE.

      MESSAGE "Do you want a document-level break?" 
      VIEW-AS ALERT-BOX QUESTION  
      BUTTONS YES-NO 
      UPDATE ldebugdoclist.

      IF ldebugdoclist THEN 
      DO: 

        FOR EACH ttdoclist
          BY Sequence:
              
            ASSIGN ldebugdoclist = FALSE.
      
            MESSAGE "do you want to process "  + ttdoclist.docname + 
                    " for copy "           + ttdoclist.doccopy     + 
                    "?"    
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO 
            UPDATE ldebugdoclist.
      
            IF NOT ldebugdoclist THEN 
               DELETE ttdoclist.
                  
            ELSE 
              ASSIGN ttdoclist.DebugMode = "debug".

        END.   /* FOR EACH ttdoclist */

      END.   /* IF ldebugdoclist */

      ELSE 
      DO: 
            
        FOR EACH ttdoclist:
            ttdoclist.DebugMode = "Ask".

        END.   /* FOR EACH ttdoclist */

      END.   /* Else Do - IF ldebugdoclist */

  &ENDIF
  
  /* end debug code */

  SESSION:REMOVE-SUPER-PROCEDURE(hDocListMethods).
  DELETE PROCEDURE hDocListMethods  NO-ERROR.

  IF ERROR-STATUS:ERROR THEN 
     RETURN ERROR DYNAMIC-FUNCTION("ErrorTrap").
  
  RUN PSsrc/Cm/Decs/Common/Procs/DocPrint.p (INPUT-OUTPUT TABLE ttBatchRequest,
                                             INPUT-OUTPUT TABLE ttBatchReqOutput,
                                             INPUT-OUTPUT TABLE ttDocList,
                                             hDocMethods)  NO-ERROR.

  IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.


END PROCEDURE.   /* CreateDocList */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DocLoop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DocLoop Procedure 
PROCEDURE DocLoop :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:       CMP has Output that can go to Company, Agent, Insured, 
               Addl Insured, Mortgagee, Correspondence, and Internal 
               Docs, and Loss Control in this order on the Close out screens.
  ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCopy         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cCurrentEntry AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cList         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFormName     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cTempName     AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE iBeginLoop    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iEntry        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iEntry2       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iCopy         AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iIndex        AS INTEGER    NO-UNDO.

    DEFINE VARIABLE lAppend       AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cFillInKey    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cStockKey     AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE statusMsg        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE hBatchRequest    AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hBatchReqOutput  AS HANDLE     NO-UNDO.
    DEFINE VARIABLE objBatchRequest  AS CLASS PSsrc.cls.batchRequestClass NO-UNDO.


    IF NOT AVAILABLE ttBatchRequest THEN 
       RETURN ERROR "The ttBatchRequest record was not available.".

    EMPTY TEMP-TABLE ttBatchReqOutput.
    objBatchRequest = NEW PSsrc.cls.batchRequestClass().
    hBatchRequest   = TEMP-TABLE ttBatchRequest:HANDLE.
    hBatchReqOutput = TEMP-TABLE ttBatchReqOutput:HANDLE.
    
    statusMsg = objBatchRequest:convertBatchRequest(INPUT hBatchRequest,
                                                    INPUT hBatchReqOutput).
    DELETE OBJECT objBatchRequest.
    objBatchRequest = ?.

    IF statusMsg <> "ok" THEN
      RETURN ERROR statusMsg.

    REPEAT iCopy = 1 TO 10:
      CASE iCopy:
        WHEN 1 THEN
          ASSIGN
          cList   = ttBatchRequest.DocListCompany
          cCopy   = "Company"
          lAppend = FALSE.
        WHEN 2 THEN 
          ASSIGN
          cList   = ttBatchRequest.DocListAgent
          cCopy   = "Agent"
          lAppend = FALSE.
        WHEN 3 THEN 
          ASSIGN
          cList   = ttBatchRequest.DocListInsured
          cCopy   = "Insured"
          lAppend = FALSE.
        WHEN 4 THEN 
          ASSIGN
          cList   = ttBatchRequest.DocListAdditionalInsured
          cCopy   = "AdditionalInsured"
          lAppend = FALSE.
        WHEN 5 THEN 
          ASSIGN
          cList   = ttBatchRequest.DocListMortgagee
          cCopy   = "Mortgagee"
          lAppend = FALSE.
        WHEN 6 THEN 
          ASSIGN
          cList   = ttBatchRequest.DocListLossPayee
          cCopy   = "LossPayee"
          lAppend = FALSE.
        WHEN 7 THEN 
          ASSIGN
          cList   = ttBatchRequest.DocListCorrespondence
          cCopy   = "Correspondence"
          lAppend = FALSE.
        WHEN 8 THEN 
          ASSIGN
          cList   = ttBatchRequest.DocListInternalDocs
          cCopy   = "InternalDocs"
          lAppend = FALSE.
        WHEN 9 THEN
          ASSIGN
          cList   = ttBatchRequest.DocListAudit
          cCopy   = "LossControl"
          lAppend = FALSE.
        WHEN 10 THEN
        DO:
          FIND FIRST ttBatchReqOutput WHERE
            ttBatchReqOutput.copyType = "Forms"
            NO-ERROR.
          IF AVAILABLE ttBatchReqOutput THEN
            ASSIGN
              cList   = ttBatchReqOutput.docList
              cCopy   = ttBatchReqOutput.copyType
              lAppend = FALSE.
          ELSE
            ASSIGN
              cList   = ""
              cCopy   = ""
              lAppend = FALSE.
        END.  /* WHEN 10 */

      END CASE.


    REPEAT iEntry = 1 TO NUM-ENTRIES(cList,CHR(1)):
      
      ASSIGN cCurrentEntry = ENTRY(iEntry,cList,CHR(1))  NO-ERROR.

      IF ERROR-STATUS:ERROR THEN
         RETURN ERROR DYNAMIC-FUNCTION("ErrorTrap").

      IF cCurrentEntry = "NONE" THEN  NEXT.
        
      REPEAT iEntry2 = 1 TO NUM-ENTRIES(cCurrentEntry):

        IF lDebugDoclist THEN
        DO:

          MESSAGE "debug:doclist:docloop" 
                   SKIP
                   ENTRY(ientry2,cCurrentEntry) 
          VIEW-AS ALERT-BOX.

          DEBUGGER:INITIATE().
          DEBUGGER:SET-BREAK().

        END.   /* IF lDebugDoclist */

        ASSIGN cFormName = ENTRY(iEntry2,cCurrentEntry).
            
        IF cFormName = "FillInNone"    OR
           cFormName = "FillInAll"     OR
           cFormName = "FillInChanged" OR
           cFormName = "StockNone"     OR
           cFormName = "StockAll"      OR
           cFormName = "StockChanged"  THEN 
        DO:
                
          ASSIGN 
          cFillInKey = "NONE"
          cStockKey  = "NONE".

          REPEAT iIndex = 1 TO NUM-ENTRIES(cList,CHR(1)):
                    
            ASSIGN cTempName = ENTRY(iIndex,cList,CHR(1)).
            CASE cTempName:
               WHEN "FillInNone" THEN cFillInKey = "NONE".
               WHEN "FillInAll" THEN cFillInKey = "ALL".
               WHEN "FillInChanged" THEN cFillInKey = "CHANGED".
               WHEN "StockNone" THEN cStockKey = "NONE".
               WHEN "StockAll" THEN cStockKey = "ALL".
               WHEN "StockChanged" THEN cStockKey = "CHANGED".
            END CASE.

          END.   /* REPEAT iIndex = 1 TO NUM-ENTRIES(cList,CHR(1)) */

          RUN FormsNew IN hDocListMethods
                      (INPUT cFormName,
                       INPUT cCopy,
                       INPUT lAppend,
                       INPUT-OUTPUT TABLE ttDocList,
                       INPUT-OUTPUT iDocSequence)  NO-ERROR.
          IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.
  
          RUN GetCombinedForms IN hDocListMethods
                              (INPUT "GetCombinedForms",
                               INPUT cCopy,
                               INPUT lAppend,
                               INPUT cFillInKey,
                               INPUT cStockKey,
                               INPUT-OUTPUT TABLE ttDocList,
                               INPUT-OUTPUT iDocSequence) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.
                    
        END.   /* IF cFormName = "FillInNone" */

        ELSE
          IF cCopy = "Correspondence" OR cCopy = "InternalDocs" THEN 
          DO:
                
            IF LOOKUP(cFormName,hDocListMethods2:INTERNAL-ENTRIES) > 0 THEN 
            DO:

              RUN RunProcedure IN hDocListMethods2
                              (INPUT cFormName,
                               INPUT cCopy,
                               INPUT lAppend,
                               INPUT-OUTPUT TABLE ttDocList,
                               INPUT-OUTPUT iDocSequence)  NO-ERROR.
  
              IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.
  
            END.   /* IF LOOKUP(cFormName,hDocListMethods:INTERNAL-ENTRIES) > 0 */

            ELSE 
              IF LOOKUP("Default",hDocListMethods2:INTERNAL-ENTRIES) > 0 THEN 
              DO:
  
                RUN RunProcedure IN hDocListMethods2
                                (INPUT cFormName,
                                 INPUT cCopy,
                                 INPUT lAppend,
                                 INPUT-OUTPUT TABLE ttDocList,
                                 INPUT-OUTPUT iDocSequence)  NO-ERROR.
  
                IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.
  
              END.   /* ELSE IF LOOKUP("Default",hDocListMethods:INTERNAL-ENTRIES) > 0 */

              ELSE 
              DO:
  
                RETURN ERROR "No Procedure Found For " + cFormName.

              END.   /* ELSE */
               
          END.   /* IF cCopy = "Correspondence" */

          ELSE 
          DO:

            IF LOOKUP(cFormName,hDocListMethods:INTERNAL-ENTRIES) > 0 THEN 
            DO:
  
              RUN RunProcedure IN hDocListMethods
                              (INPUT cFormName,
                               INPUT cCopy,
                               INPUT lAppend,
                               INPUT-OUTPUT TABLE ttDocList,
                               INPUT-OUTPUT iDocSequence)  NO-ERROR.
  
              IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.
  
            END. /* IF LOOKUP(cFormName,hDocListMethods:INTERNAL-ENTRIES) > 0 */

            ELSE 
              IF LOOKUP(cFormName,hDocListMethods2:INTERNAL-ENTRIES) > 0 THEN 
              DO:
  
                RUN RunProcedure IN hDocListMethods2
                                (INPUT cFormName,
                                 INPUT cCopy,
                                 INPUT lAppend,
                                 INPUT-OUTPUT TABLE ttDocList,
                                 INPUT-OUTPUT iDocSequence)  NO-ERROR.
  
                IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.
  
              END. /* IF LOOKUP(cFormName,hDocListMethods2:INTERNAL-ENTRIES) > 0 */

              ELSE 
                IF LOOKUP(cFormName,THIS-PROCEDURE:INTERNAL-ENTRIES) > 0 THEN 
                DO:
  
                  RUN VALUE(cFormName) IN THIS-PROCEDURE
                                      (INPUT cFormName,
                                       INPUT cCopy,
                                       INPUT lAppend)  NO-ERROR.
  
                  IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.
  
                END.   /* if cFormName in internal procedures */

                ELSE 
                  IF LOOKUP("Default",hDocListMethods:INTERNAL-ENTRIES) > 0 THEN 
                  DO:
  
                    RUN RunProcedure IN hDocListMethods
                                    (INPUT cFormName,
                                     INPUT cCopy,
                                     INPUT lAppend,
                                     INPUT-OUTPUT TABLE ttDocList,
                                     INPUT-OUTPUT iDocSequence)  NO-ERROR.
  
                    IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.
  
                  END.   /* ELSE IF LOOKUP("Default",hDocListMethods:INTERNAL-ENTRIES) > 0 */

                  ELSE 
                  DO:
  
                    RETURN ERROR "No Procedure Found For " + cFormName.
  
                  END.  /* ELSE */
  
            ASSIGN lAppend = TRUE  NO-ERROR.
  
            IF ERROR-STATUS:ERROR THEN
               RETURN ERROR DYNAMIC-FUNCTION("ErrorTrap").
            
          END.   /* Else Do - IF cCopy = "Correspondence" */

      END.   /* REPEAT iEntry2 = 1 TO NUM-ENTRIES(cCurrentEntry) */

    END.   /* REPEAT iEntry = 1 TO NUM-ENTRIES(cList,CHR(1)) */
    
    IF NUM-ENTRIES(cList,CHR(1)) > 0 THEN DO:
    
        /* This is not true in CMP; All these, plus LossControl are in a CMP Quote */
      cTempName = ENTRY(1,cList,CHR(1)) NO-ERROR.

      IF (cCopy = "Insured" OR cCopy = "Agent" OR cCopy = "Company" OR cCopy = "LossControl") AND 
         cTempName = "DECL" THEN DO:
            
          RUN AddIndex IN hDocListMethods
                         (INPUT "AddIndex",
                          INPUT cCopy,
                          INPUT lAppend,
                          INPUT-OUTPUT TABLE ttDocList,
                          INPUT-OUTPUT iDocSequence) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.
      END.
      
    END.

  END. /* REPEAT iCopy = 1 TO 10 */

END PROCEDURE.   /* DocLoop */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadMethods) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadMethods Procedure 
PROCEDURE LoadMethods :
/*------------------------------------------------------------------------------
  Purpose:     This procedure is to start the general method libraries
               that are used throughout the system.  

          
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCmpPath      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cCommPath     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cMethodLib    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cControlLib   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cControlLib2  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cCommonLib    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cCommonLib2   AS CHARACTER   NO-UNDO.
   
    ASSIGN
    cCmpPath     = "PSsrc/Cm/Decs/Cmp/Methods/"
    cCommPath    = "PSsrc/Cm/Decs/Common/Methods/"
    cMethodLib   = cCmpPath + "Cmp" + 
                  (IF NOT lQuote AND NOT lPolicy THEN 
                      "N"
                   ELSE 
                      "") + ".p"
    cControlLib  = cCmpPath + "CmpDL" + 
                   (IF lQuote OR lPolicy THEN 
                       "P" 
                    ELSE 
                       "N") + ".p"
    cControlLib2 = cCmpPath + "CmpDLC.p"
    cCommonLib   = cCommPath + 
                   (IF lQuote OR lPolicy THEN 
                       "Base"
                    ELSE 
                       "Notice") + "Methods.p"
    cCommonLib2  = cCommPath + "DocMethods.p"
    NO-ERROR.

    IF ERROR-STATUS:ERROR THEN 
       RETURN ERROR DYNAMIC-FUNCTION("ErrorTrap").

    /*
    ** Load the Data Store.
    */
    RUN VALUE(cCommPath + "DataStore.p") 
        PERSISTENT SET hData NO-ERROR.

    IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.
    /*
    ** Load the Policy Method Lib. 
    */

     
    RUN VALUE(cMethodLib) PERSISTENT SET hDocMethods 
      (INPUT-OUTPUT TABLE ttBatchRequest) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.
     
    
    /*
    ** Load the Program Flow Method Lib.
    */
    RUN VALUE(cControlLib) PERSISTENT SET hDocListMethods
      (INPUT-OUTPUT TABLE ttBatchRequest, hDocMethods) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.
   
    /*
    ** Load the Program Flow Method Lib. for Correspondence
    */
    RUN VALUE(cControlLib2) PERSISTENT SET hDocListMethods2
      (INPUT-OUTPUT TABLE ttBatchRequest, hDocMethods) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.

    /*
    ** Load the Print Type Common Method Lib.
    */
    RUN VALUE(cCommonLib) PERSISTENT SET hCommonMethods1
      (INPUT-OUTPUT TABLE ttBatchRequest) NO-ERROR.
 
    IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.

     /*
    ** Load Common Doc Methods Method Lib.
    */
    RUN VALUE(cCommonLib2) PERSISTENT SET hCommonMethods2
      (INPUT-OUTPUT TABLE ttBatchRequest) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.

    FIND FIRST ttBatchRequest NO-ERROR.

    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PolicyQuote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PolicyQuote Procedure 
PROCEDURE PolicyQuote :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cKeyPrefix AS CHARACTER  NO-UNDO.

  ASSIGN
  lQuote  = FALSE
  lPolicy = FALSE
  NO-ERROR.

  IF ERROR-STATUS:ERROR THEN 
     RETURN ERROR DYNAMIC-FUNCTION("ErrorTrap").

  FIND FIRST ttBatchRequest NO-ERROR.
  IF ERROR-STATUS:ERROR OR NOT AVAILABLE ttBatchRequest THEN 
     RETURN ERROR DYNAMIC-FUNCTION("ErrorTrap").

  ASSIGN cKeyPrefix = SUBSTRING(ttBatchRequest.Parameter1,4,3).

  IF ttBatchRequest.Branch = "" THEN 
    ASSIGN ttBatchRequest.Branch = "HOME"  NO-ERROR.

  IF ERROR-STATUS:ERROR THEN 
     RETURN ERROR DYNAMIC-FUNCTION("ErrorTrap").

  IF  LOOKUP ("DECL",ttBatchRequest.DocListCompany,CHR(1)) > 0           OR 
      LOOKUP ("DECL",ttBatchRequest.DocListAgent,CHR(1)) > 0             OR 
      LOOKUP ("DECL",ttBatchRequest.DocListAudit,CHR(1)) > 0             OR
      LOOKUP ("DECL",ttBatchRequest.DocListInsured,CHR(1)) > 0           OR
      LOOKUP ("DECL",ttBatchRequest.DocListAdditionalInsured,CHR(1)) > 0 OR
      LOOKUP ("DECL",ttBatchRequest.DocListMortgagee,CHR(1)) > 0         OR 
      LOOKUP ("DECL",ttBatchRequest.DocListLossPayee,CHR(1)) > 0         THEN 
      
      ASSIGN lPolicy = TRUE.

  IF  LOOKUP ("DeclChanged",ttBatchRequest.DocListCompany,CHR(1)) > 0           OR 
      LOOKUP ("DeclChanged",ttBatchRequest.DocListAgent,CHR(1)) > 0             OR
      LOOKUP ("DeclChanged",ttBatchRequest.DocListAudit,CHR(1)) > 0             OR
      LOOKUP ("DeclChanged",ttBatchRequest.DocListInsured,CHR(1)) > 0           OR
      LOOKUP ("DeclChanged",ttBatchRequest.DocListAdditionalInsured,CHR(1)) > 0 OR
      LOOKUP ("DeclChanged",ttBatchRequest.DocListMortgagee,CHR(1)) > 0         OR 
      LOOKUP ("DeclChanged",ttBatchRequest.DocListLossPayee,CHR(1)) > 0         THEN 
          
      ASSIGN lPolicy = TRUE.

  IF  LOOKUP ("DeclNONE",ttBatchRequest.DocListCompany,CHR(1)) > 0           OR 
      LOOKUP ("DeclNONE",ttBatchRequest.DocListAgent,CHR(1)) > 0             OR 
      LOOKUP ("DeclNONE",ttBatchRequest.DocListAudit,CHR(1)) > 0             OR 
      LOOKUP ("DeclNONE",ttBatchRequest.DocListInsured,CHR(1)) > 0           OR
      LOOKUP ("DeclNONE",ttBatchRequest.DocListAdditionalInsured,CHR(1)) > 0 OR
      LOOKUP ("DeclNONE",ttBatchRequest.DocListMortgagee,CHR(1)) > 0         OR 
      LOOKUP ("DeclNONE",ttBatchRequest.DocListLossPayee,CHR(1)) > 0         THEN 
      
      ASSIGN lPolicy = TRUE.
  
  IF  LOOKUP ("QUOTE",ttBatchRequest.DocListCompany,CHR(1)) > 0           OR 
      LOOKUP ("QUOTE",ttBatchRequest.DocListAgent,CHR(1))   > 0           OR 
      LOOKUP ("QUOTE",ttBatchRequest.DocListInsured,CHR(1)) > 0           OR
      LOOKUP ("QUOTE",ttBatchRequest.DocListAudit,CHR(1))   > 0           OR 
      LOOKUP ("QuoteNone",ttBatchRequest.DocListCompany,CHR(1)) > 0       OR 
      LOOKUP ("QuoteNone",ttBatchRequest.DocListAgent,CHR(1))   > 0       OR 
      LOOKUP ("QuoteNone",ttBatchRequest.DocListInsured,CHR(1)) > 0       OR
      LOOKUP ("QuoteNone",ttBatchRequest.DocListAudit,CHR(1))   > 0       THEN
      
      ASSIGN lQuote = TRUE.


  /* Need to load the correct methods in LoadMethods */
  IF NOT lPolicy AND NOT lQuote THEN DO:

    /* Check for Correspondence and Internal Docs if no lPolicy and no lQuote */
    IF (ttBatchRequest.DocListCorrespondence <> ?   AND 
        ttBatchRequest.DocListCorrespondence <> "") OR  
       (ttBatchRequest.DocListInternalDocs   <> ?   AND 
        ttBatchRequest.DocListInternalDocs   <> "") THEN DO:
      
      IF ttBatchRequest.QuoteFlag OR 
         LOOKUP(SUBSTRING(ttBatchRequest.Parameter1,4,1),"Q,R,E" ) > 0 THEN 
        lQuote = TRUE.
      ELSE
        lPolicy = TRUE.

    END.

    /* Special Logic for printing outside the dec logic but by using the dec Logic */
    IF ENTRY(1,ttBatchRequest.DocListInsured,CHR(1))   = "PolicySelectRun" OR 
       ENTRY(1,ttBatchRequest.DocListCompany,CHR(1))   = "PolicySelectRun" OR
       ENTRY(1,ttBatchRequest.DocListBranch,CHR(1))    = "PolicySelectRun" OR
       ENTRY(1,ttBatchRequest.DocListAgent,CHR(1))     = "PolicySelectRun" OR
       ENTRY(1,ttBatchRequest.DocListLossPayee,CHR(1)) = "PolicySelectRun" THEN 
  
       ASSIGN lPolicy = TRUE.
  
    IF ENTRY(1,ttBatchRequest.DocListInsured,CHR(1))   = "QuoteSelectRun" OR 
       ENTRY(1,ttBatchRequest.DocListCompany,CHR(1))   = "QuoteSelectRun" OR
       ENTRY(1,ttBatchRequest.DocListBranch,CHR(1))    = "QuoteSelectRun" OR
       ENTRY(1,ttBatchRequest.DocListAgent,CHR(1))     = "QuoteSelectRun" OR
       ENTRY(1,ttBatchRequest.DocListLossPayee,CHR(1)) = "QuoteSelectRun" THEN 
  
       ASSIGN lQuote = TRUE.
  
    IF ENTRY(1,ttBatchRequest.DocListInsured,CHR(1))   = "NoticeSelectRun" OR 
       ENTRY(1,ttBatchRequest.DocListCompany,CHR(1))   = "NoticeSelectRun" OR
       ENTRY(1,ttBatchRequest.DocListBranch,CHR(1))    = "NoticeSelectRun" OR
       ENTRY(1,ttBatchRequest.DocListAgent,CHR(1))     = "NoticeSelectRun" OR
       ENTRY(1,ttBatchRequest.DocListLossPayee,CHR(1)) = "NoticeSelectRun" THEN 
  
      ASSIGN 
      lQuote  = FALSE
      lPolicy = FALSE.

  END.  /* IF NOT lPolicy AND NOT lQuote */

  IF lPolicy AND lQuote THEN 
      RETURN ERROR "The batch request cannot ask for both a policy and quote".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrintErrorLog) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintErrorLog Procedure 
PROCEDURE PrintErrorLog :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pcErrorList AS CHAR NO-UNDO.


    FIND FIRST ttBatchRequest NO-ERROR.
    IF ERROR-STATUS:ERROR OR NOT AVAILABLE ttBatchRequest THEN 
       RETURN ERROR DYNAMIC-FUNCTION("ErrorTrap").

    EMPTY TEMP-TABLE ttDocList NO-ERROR.

    IF ERROR-STATUS:ERROR THEN 
       RETURN ERROR DYNAMIC-FUNCTION("ErrorTrap").

    FIND FIRST DocumentControl WHERE 
        DocumentControl.DocKey = "ErrorLog" AND 
        DocumentControl.Company = ttBatchRequest.Company
        NO-LOCK NO-ERROR.

    IF ERROR-STATUS:ERROR OR NOT AVAILABLE DocumentControl THEN 
       RETURN ERROR DYNAMIC-FUNCTION("ErrorTrap").

    CREATE ttDocList.
    ASSIGN
    ttDocList.Sequence        = iDocSequence
    ttDocList.PrintSequence   = iDocSequence
    ttDocList.DocName         = DocumentControl.DocName
    ttDocList.DocType         = DocumentControl.DocType
    ttDocList.ProcedureValue  = DocumentControl.ProcedureValue
    ttDocList.RTFValue        = DocumentControl.RTFValue
    ttDocList.FdfValue        = DocumentControl.FdfValue
    ttDocList.FinalOutputFile = ttBatchRequest.OutFileInsured
    ttDocList.DocCopy         = "Insured"
    ttDocList.AppendFlag      = FALSE
    ttDocList.Key1            = pcErrorList
    iDocSequence              = iDocSequence + 1
    NO-ERROR.

    IF ERROR-STATUS:ERROR THEN 
       RETURN ERROR DYNAMIC-FUNCTION("ErrorTrap").

    IF SEARCH("PSsrc/Cm/Decs/Common/Procs/DocPrint.r") <> ? THEN
    DO:
        RUN PSsrc/Cm/Decs/Common/Procs/DocPrint.r (INPUT-OUTPUT TABLE ttBatchRequest,
                                                   INPUT-OUTPUT TABLE ttDocList,
                                                   hDocMethods)
                                                   NO-ERROR.

        IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.

    END. /* IF SEARCH("PSsrc/Cm/Decs/Common/Procs/DocPrint.r") <> ? THEN */
    ELSE
    DO:

        RUN PSsrc/Cm/Decs/Common/Procs/DocPrint.p (INPUT-OUTPUT TABLE ttBatchRequest,
                                                   INPUT-OUTPUT TABLE ttBatchReqOutput,
                                                   INPUT-OUTPUT TABLE ttDocList,
                                                   hDocMethods)
                                                   NO-ERROR.

        IF ERROR-STATUS:ERROR THEN  RETURN ERROR RETURN-VALUE.

    END. /* ELSE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetOutFiles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetOutFiles Procedure 
PROCEDURE SetOutFiles :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cSequence AS CHARACTER  NO-UNDO.

    ASSIGN
    cSequence = TRIM(STRING(NEXT-VALUE(sDocSequence))) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN 
       RETURN ERROR DYNAMIC-FUNCTION("ErrorTrap").

    IF NOT AVAILABLE ttBatchRequest THEN 
       FIND FIRST ttBatchRequest.

    IF ERROR-STATUS:ERROR OR NOT AVAILABLE ttBatchRequest THEN 
       RETURN ERROR DYNAMIC-FUNCTION("ErrorTrap").

    IF ttBatchRequest.OutFileCompany = "" THEN 
        ttBatchRequest.OutFileCompany = "CO" + cSequence.

    IF ttBatchRequest.OutFileAgent = "" THEN 
        ttBatchRequest.OutFileAgent = "AG" + cSequence.

    IF ttBatchRequest.OutFileAudit = "" THEN 
        ttBatchRequest.OutFileAudit = "LC" + cSequence. /* Loss Control */

    IF ttBatchRequest.OutFileInsured = "" THEN 
        ttBatchRequest.OutFileInsured = "IN" + cSequence.

    IF ttBatchRequest.OutFileAdditionalInsured = "" THEN 
        ttBatchRequest.OutFileAdditionalInsured = "AI" + cSequence.
                                                              
    IF ttBatchRequest.OutFileMortgagee = "" THEN 
        ttBatchRequest.OutFileMortgagee = "MG" + cSequence.
    
    IF ttBatchRequest.OutFileCorrespondence = "" THEN
        ttBatchRequest.OutFileCorrespondence = "CP" + cSequence.

    IF ttBatchRequest.OutFileInternalDocs = "" THEN
        ttBatchRequest.OutFileInternalDocs = "ID" + cSequence.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-BoldOff) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BoldOff Procedure 
FUNCTION BoldOff RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN CHR(125) + CHR(123) + CHR(92) + "b0".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BoldOn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BoldOn Procedure 
FUNCTION BoldOn RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN CHR(125) + CHR(123) + CHR(92) + "b ".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkDb) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkDb Procedure 
FUNCTION checkDb RETURNS CHARACTER
    ( pcDbName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Check to see if a database is connected.  If not it is connected.
   Author:  
------------------------------------------------------------------------------*/
  
DEFINE VARIABLE objEnv     AS CLASS PSsrc.cls.environmentClass NO-UNDO.
DEFINE VARIABLE cEnv       AS CHARACTER NO-UNDO.

  IF NOT CONNECTED(pcDbName) THEN
  DO:
    objEnv = new PSsrc.cls.environmentClass().
    cEnv = objEnv:getEnvironment() NO-ERROR.
    FIND FIRST environmentData WHERE 
            environmentData.environmentName = cEnv AND
            environmentData.fieldName = pcDbName NO-LOCK NO-ERROR.
    IF AVAILABLE environmentData THEN
    DO:
      CONNECT environmentData.fieldValue NO-ERROR.
      
      IF ERROR-STATUS:ERROR THEN RETURN "no".
    END.
                 
  END.
  ELSE
    RETURN "yes".

  RETURN "yes".

  

  FINALLY:
    IF objEnv <> ? THEN
      DELETE OBJECT objEnv.
    ASSIGN objEnv = ?.
  END FINALLY.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CheckFile Procedure 
FUNCTION CheckFile RETURNS CHARACTER
    ( pcFileName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Check to see if a file exists.
   Author:  
------------------------------------------------------------------------------*/
  
  FILE-INFO:FILE-NAME = pcFileName.

  IF FILE-INFO:FULL-PATHNAME = "" OR FILE-INFO:FULL-PATHNAME = ? THEN
  DO:

    FILE-INFO:FILE-NAME = REPLACE(REPLACE(pcFileName,".p",".r"),".w",".r").

  END.

  RETURN FILE-INFO:FULL-PATHNAME.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetErrors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetErrors Procedure 
FUNCTION GetErrors RETURNS CHARACTER
  ( pcErrorMsg as char ) :
/*------------------------------------------------------------------------------
  Purpose:  GetErrors is defined in PSsrc/Pl/Decs/Common/Methods/DocMethods.p.
            This procedure is called at any time before we have run that
            method library persistent.  This function is run dynamically
            (DYNAMIC-FUNCTION ("GetErrors"...)) in the appropriate procedures.
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iErrCount    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cErrMessage  AS CHARACTER  NO-UNDO  INITIAL "" .

  IF pcErrorMsg <> ?  THEN
    cErrMessage = pcErrorMsg.

  DO iErrCount = 1 TO ERROR-STATUS:NUM-MESSAGES:
    ASSIGN
    cErrMessage = cErrMessage +
                 (IF cErrMessage <> "" THEN (CHR(13) + "\par ") ELSE "") +
                 ERROR-STATUS:GET-MESSAGE(iErrCount).

  END.   /* DO iErrCount = 1 TO ERROR-STATUS:NUM-MESSAGES */

  IF AVAILABLE ttBatchRequest THEN
     ASSIGN
       ttBatchRequest.StatusMessage = cErrMessage.

  RETURN cErrMessage.   /* Function return value. */

END FUNCTION.   /* GetErrors */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LineFeed) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LineFeed Procedure 
FUNCTION LineFeed RETURNS CHARACTER
  ( ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  RETURN CHR(13) + "\par ".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RemoveSpaces) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RemoveSpaces Procedure 
FUNCTION RemoveSpaces RETURNS CHARACTER
  ( pcText AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  ASSIGN pcText = REPLACE(pcText," ","").

  RETURN pcText.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReturnAnchor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReturnAnchor Procedure 
FUNCTION ReturnAnchor RETURNS CHARACTER
  (piLineNumber AS INTEGER, phProgram AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cAnchor             AS CHAR NO-UNDO.
    DEFINE VARIABLE cErrorRow           AS CHAR NO-UNDO.
    DEFINE VARIABLE cInternalEntity     AS CHAR NO-UNDO.
    DEFINE VARIABLE cInternalEntityName AS CHAR NO-UNDO.
    DEFINE VARIABLE cParameter          AS CHAR NO-UNDO.
    DEFINE VARIABLE cProgram            AS CHAR NO-UNDO.
    DEFINE VARIABLE cProgramLine        AS CHAR NO-UNDO.
    DEFINE VARIABLE cSignature          AS CHAR NO-UNDO.

    DEFINE VARIABLE i                   AS INTE NO-UNDO.

    DEFINE VARIABLE lInternalEntity     AS LOGI NO-UNDO.
    DEFINE VARIABLE lParameter          AS LOGI NO-UNDO.
    DEFINE VARIABLE lProcedure          AS LOGI NO-UNDO.

    ASSIGN
        cErrorRow = STRING(piLineNumber)
        cProgramLine = PROGRAM-NAME(2)
        cProgram = THIS-PROCEDURE:FILE-NAME
        /* ENTRY(NUM-ENTRIES(cProgramLine," "),cProgramLine," ") */
        cInternalEntityName = ENTRY(1,cProgramLine," ")
        cSignature = phProgram:GET-SIGNATURE(cInternalEntityName)
        lInternalEntity = (IF NUM-ENTRIES(cProgramLine," ") > 1
                            THEN TRUE
                            ELSE FALSE)
        lProcedure = (IF lInternalEntity AND ENTRY(1,cSignature) = "PROCEDURE"
                        THEN TRUE
                        ELSE FALSE)
        cInternalEntity = (IF NOT lInternalEntity
                            THEN ""
                            ELSE (IF lProcedure
                                THEN "internal procedure "
                                ELSE "function "))
        lParameter = (IF NUM-ENTRIES(cSignature) > 2
                        THEN (IF ENTRY(3,cSignature) <> ""
                            THEN TRUE
                            ELSE FALSE)
                        ELSE FALSE)
/*        cAnchor = "An error occured around line number "
 *                     + DYNAMIC-FUNCTION("BoldOn")
 *                     + cErrorRow
 *                     + DYNAMIC-FUNCTION("BoldOff")*/
        cAnchor = "An error occured in "
                    + (IF lInternalEntity
                        THEN "the "
                            + cInternalEntity
                            + DYNAMIC-FUNCTION("BoldOn")
                            + cInternalEntityName
                            + DYNAMIC-FUNCTION("BoldOff")
                            + "  of "
                        ELSE "")
                    + "the program "
                    + DYNAMIC-FUNCTION("BoldOn")
                    + cProgram
                    + DYNAMIC-FUNCTION("BoldOff")
                    + ".  "
                    + DYNAMIC-FUNCTION("LineFeed").

    IF NOT lParameter
        THEN ASSIGN
            cAnchor = cAnchor
                        + "There are no parameters required for "
                        + DYNAMIC-FUNCTION("BoldOn")
                        + cInternalEntityName
                        + DYNAMIC-FUNCTION("BoldOff")
                        + "  in "
                        + DYNAMIC-FUNCTION("BoldOn")
                        + cProgram
                        + DYNAMIC-FUNCTION("BoldOff")
                        + ".  "
                        + DYNAMIC-FUNCTION("LineFeed").
        ELSE
        DO:
            ASSIGN
                cAnchor = cAnchor
                            + "The parameters for "
                            + DYNAMIC-FUNCTION("BoldOn")
                            + cInternalEntityName
                            + DYNAMIC-FUNCTION("BoldOff")
                            + "  in "
                            + DYNAMIC-FUNCTION("BoldOn")
                            + cProgram
                            + DYNAMIC-FUNCTION("BoldOff")
                            + "  are:"
                            + DYNAMIC-FUNCTION("LineFeed").

            REPEAT i = 3 TO NUM-ENTRIES(cSignature):

                ASSIGN
                    cParameter = ENTRY(i,cSignature)
                    cAnchor = cAnchor
                                + DYNAMIC-FUNCTION("TabStop",1)
                                + DYNAMIC-FUNCTION("BoldOn")
                                + ENTRY(2,cParameter," ")
                                + DYNAMIC-FUNCTION("BoldOff")
                                + "  has a mode of "
                                + DYNAMIC-FUNCTION("BoldOn")
                                + ENTRY(1,cParameter," ")
                                + DYNAMIC-FUNCTION("BoldOff")
                                + "  and a data type of "
                                + DYNAMIC-FUNCTION("BoldOn")
                                + ENTRY(3,cParameter," ")
                                + DYNAMIC-FUNCTION("BoldOff")
                                + ".  "
                                + DYNAMIC-FUNCTION("LineFeed").

            END. /* REPEAT i = 3 TO NUM-ENTRIES(cSignature): */
        END. /* ELSE */

    RETURN cAnchor.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

