&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : cerep/jobcboxQANotes.p
    Purpose     : Pop-up notes for QA purposes when printing Job card

    Syntax      : run cerep/jobcboxQANotes.p (input cocode,
                                              input itemfg.i-no,
                                              input job.create-date).

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcItemNo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdtJobDate AS DATE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15.05
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEFINE BUFFER bf-itemfg FOR itemfg.
DEFINE BUFFER bf-notes FOR notes.
DEFINE BUFFER bf-job FOR job.

DEFINE VARIABLE lAlert AS LOGICAL     NO-UNDO.

FIND FIRST bf-itemfg 
    WHERE bf-itemfg.company EQ ipcCompany
      AND bf-itemfg.i-no EQ ipcItemNo
    NO-LOCK NO-ERROR.
IF AVAIL bf-itemfg THEN DO:
    FOR EACH bf-notes
        WHERE bf-notes.rec_key EQ bf-itemfg.rec_key
          AND bf-notes.note_code EQ 'QA'
        NO-LOCK:
        RUN SetAlert(INPUT bf-itemfg.company,
                     INPUT bf-itemfg.i-no,
                     INPUT ipdtJobDate,
                     INPUT bf-notes.note_date,
                     OUTPUT lAlert).
        RUN DisplayMessage(BUFFER bf-notes,
                           INPUT bf-itemfg.i-no,
                           INPUT lAlert).
    END. /*each notes*/
END. /*avail itemfg*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-DisplayMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayMessage Procedure 
PROCEDURE DisplayMessage :
/*------------------------------------------------------------------------------
  Purpose:     Displays the note message
  Parameters:  Notes buffer, alert vs. warning
  Notes:       
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipbf-notes FOR notes.
DEFINE INPUT PARAMETER ipcINo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplAlert AS LOGICAL NO-UNDO.

DEFINE VARIABLE cTitle AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNoteText LIKE notes.note_text   NO-UNDO.

IF AVAIL ipbf-notes THEN DO:
    cNoteText = ipbf-notes.note_text.
    IF iplAlert THEN DO:
    
        ASSIGN 
            cTitle = 'QA Alert'
            cMessage = 'Alert: Please Verify That Previous Quality Defect Has Been Resolved'
            .
        MESSAGE cMessage SKIP(1)
                "Item: " ipcINo SKIP
                "Note: " cNoteText
            VIEW-AS ALERT-BOX WARNING BUTTONS OK TITLE cTitle.
    END.
    ELSE DO:
        ASSIGN
            cTitle = 'QA Warning'
            cMessage = 'Warning: Quality Defect on Previous Run'
            .
        MESSAGE cMessage SKIP(1)
                "Item: " ipcINo SKIP
                "Note: " cNoteText
            VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE cTitle.
    END.

END. /*avail notes*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetAlert) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetAlert Procedure 
PROCEDURE SetAlert :
/*------------------------------------------------------------------------------
  Purpose:  Determines if message should be alert or warning   
  Parameters:  date of the note, date of the curent job, output Alert (yes) or Warning (no)
  Notes: Logic is that if there is a job that is older than current job but newer
         than the QA note, -> Alert.  Otherwise it should be the first job displaying
         this note so it is just a warning
         
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER ipcComp AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcINo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdtJobCreateDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipdtNoteDate AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER oplAlert AS LOGICAL NO-UNDO.

DEFINE BUFFER bf-job FOR job. 
DEFINE BUFFER bf-job-hdr FOR job-hdr.

IF ipdtJobCreateDate NE ? THEN
FOR EACH bf-job-hdr
    WHERE bf-job-hdr.company EQ ipcComp
      AND bf-job-hdr.i-no EQ ipcINo
    NO-LOCK,
    FIRST bf-job 
        WHERE bf-job.company EQ bf-job-hdr.company
          AND bf-job.job EQ bf-job-hdr.job
          AND bf-job.job-no EQ bf-job-hdr.job-no
          AND bf-job.job-no2 EQ bf-job-hdr.job-no2
          AND bf-job.create-date LT ipdtJobCreateDate
    NO-LOCK
    BY bf-job.create-date DESC:
    IF bf-job.create-date GE ipdtNoteDate THEN DO:
        oplAlert = YES.
        LEAVE.
    END. /* job date test*/
END. /*each bf-job-hdr*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

