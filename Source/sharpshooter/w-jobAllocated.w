&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: sharpshooter\job-details.w

  Description: Displays Job Header, Materials and Machine Details

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */     
USING system.SharedConfig.

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/* Required for run_link.i */
DEFINE VARIABLE char-hdl            AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle             AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdJobProcs          AS HANDLE    NO-UNDO.
DEFINE VARIABLE cJobNo2ListItems    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount              AS INTEGER   NO-UNDO.
DEFINE VARIABLE cFormattedJobno     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation           AS CHARACTER NO-UNDO.
DEFINE VARIABLE giFormNo            AS INTEGER   NO-UNDO.
DEFINE VARIABLE giBlankNo           AS INTEGER   NO-UNDO.
DEFINE VARIABLE gcEstNo             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cScheduleBoardNotes AS CHARACTER NO-UNDO.

DEFINE VARIABLE scInstance AS CLASS system.SharedConfig NO-UNDO.
DEFINE VARIABLE oJobHeader AS jc.JobHeader NO-UNDO.

RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Record-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES job
&Scoped-define FIRST-EXTERNAL-TABLE job


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR job.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btExit btnFirst btnLast btnNext btnPrevious ~
btCopy btDelete btAdd btUpdate btAllocate btnExitText btnViewRM btClear ~
btnClearText imJobLookup 
&Scoped-Define DISPLAYED-OBJECTS fiStatusLabel fiStatus fiCreatedLabel ~
fiCreated fiDueLabel fiDue fiCSRLabel fiCSR fiLastRunLabel fiLastRun ~
fiLastJobLabel fiLastJob fiAllocatedLabel fiJobLabel fiJob fiJobQtyLabel ~
fiJobQty btnExitText statusMessage btnViewRM btnClearText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_adjustwindowsize AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-job-mat AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-job-mat-last-all AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_viewrminquiry AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btAdd  NO-FOCUS
     LABEL "Add New Material" 
     SIZE 34 BY 1.52
     FONT 17.

DEFINE BUTTON btAllocate  NO-FOCUS
     LABEL "Allocate" 
     SIZE 16 BY 1.52
     FONT 17.

DEFINE BUTTON btClear 
     IMAGE-UP FILE "Graphics/32x32/back_white.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/back_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btCopy  NO-FOCUS
     LABEL "Copy" 
     SIZE 20 BY 1.52
     FONT 17.

DEFINE BUTTON btDelete  NO-FOCUS
     LABEL "Delete" 
     SIZE 13 BY 1.52
     FONT 17.

DEFINE BUTTON btExit 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btnFirst 
     IMAGE-UP FILE "Graphics/32x32/first.png":U NO-FOCUS FLAT-BUTTON
     LABEL "First" 
     SIZE 8 BY 1.91 TOOLTIP "First".

DEFINE BUTTON btnLast 
     IMAGE-UP FILE "Graphics/32x32/last.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Last" 
     SIZE 8 BY 1.91 TOOLTIP "Last".

DEFINE BUTTON btnNext 
     IMAGE-UP FILE "Graphics/32x32/next.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Next" 
     SIZE 8 BY 1.91 TOOLTIP "Next".

DEFINE BUTTON btnPrevious 
     IMAGE-UP FILE "Graphics/32x32/previous.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Prev" 
     SIZE 8 BY 1.91 TOOLTIP "Previous".

DEFINE BUTTON btnSBNotes  NO-FOCUS
     LABEL "SB Notes" 
     SIZE 19 BY 1.52 TOOLTIP "Schedule Board Notes"
     BGCOLOR 8 FONT 17.

DEFINE BUTTON btUpdate  NO-FOCUS
     LABEL "Update" 
     SIZE 14 BY 1.52
     FONT 17.

DEFINE VARIABLE btnClearText AS CHARACTER FORMAT "X(256)":U INITIAL "RESET" 
      VIEW-AS TEXT 
     SIZE 12 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnExitText AS CHARACTER FORMAT "X(256)":U INITIAL "EXIT" 
      VIEW-AS TEXT 
     SIZE 9 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnViewRM AS CHARACTER FORMAT "X(256)":U INITIAL "RM INQUIRY" 
      VIEW-AS TEXT 
     SIZE 21 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE fiAllocatedLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Allocate Material:" 
     VIEW-AS FILL-IN 
     SIZE 40.4 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiCreated AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21.8 BY 1.43
     FONT 38 NO-UNDO.

DEFINE VARIABLE fiCreatedLabel AS CHARACTER FORMAT "X(256)":U INITIAL "CREATED:" 
     VIEW-AS FILL-IN 
     SIZE 18.2 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiCSR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.43
     FONT 38 NO-UNDO.

DEFINE VARIABLE fiCSRLabel AS CHARACTER FORMAT "X(256)":U INITIAL "CSR:" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiDue AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21.8 BY 1.43
     FONT 38 NO-UNDO.

DEFINE VARIABLE fiDueLabel AS CHARACTER FORMAT "X(256)":U INITIAL "DUE:" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiJob AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.43
     FONT 38 NO-UNDO.

DEFINE VARIABLE fiJobLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Job:" 
     VIEW-AS FILL-IN 
     SIZE 14.4 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiJobQty AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.43
     FONT 38 NO-UNDO.

DEFINE VARIABLE fiJobQtyLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Job Qty:" 
     VIEW-AS FILL-IN 
     SIZE 19.4 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiLastJob AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.43
     FONT 38 NO-UNDO.

DEFINE VARIABLE fiLastJobLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Job:" 
     VIEW-AS FILL-IN 
     SIZE 15.4 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiLastRun AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.43
     FONT 38 NO-UNDO.

DEFINE VARIABLE fiLastRunLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Last Run:" 
     VIEW-AS FILL-IN 
     SIZE 18.4 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiStatus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.43
     FONT 38 NO-UNDO.

DEFINE VARIABLE fiStatusLabel AS CHARACTER FORMAT "X(256)":U INITIAL "STATUS:" 
     VIEW-AS FILL-IN 
     SIZE 15.4 BY 1.38 NO-UNDO.

DEFINE VARIABLE statusMessage AS CHARACTER FORMAT "X(256)":U INITIAL "STATUS MESSAGE" 
      VIEW-AS TEXT 
     SIZE 102 BY 1.43 NO-UNDO.

DEFINE IMAGE imJobLookup
     FILENAME "Graphics/32x32/search_new.png":U
     STRETCH-TO-FIT RETAIN-SHAPE
     SIZE 6.4 BY 1.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnSBNotes AT ROW 32.67 COL 105
     btExit AT ROW 1 COL 197 WIDGET-ID 126
     btnFirst AT ROW 10.67 COL 197 WIDGET-ID 44
     btnLast AT ROW 16.38 COL 197 WIDGET-ID 46
     btnNext AT ROW 14.48 COL 197 WIDGET-ID 42
     btnPrevious AT ROW 12.57 COL 197 WIDGET-ID 40
     fiStatusLabel AT ROW 5.52 COL 2 NO-LABEL WIDGET-ID 94
     fiStatus AT ROW 5.52 COL 15.6 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     fiCreatedLabel AT ROW 5.52 COL 46 NO-LABEL WIDGET-ID 112
     fiCreated AT ROW 5.52 COL 62.2 COLON-ALIGNED NO-LABEL WIDGET-ID 110
     fiDueLabel AT ROW 5.52 COL 122 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     fiDue AT ROW 5.52 COL 131.6 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     fiCSRLabel AT ROW 5.52 COL 85 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     fiCSR AT ROW 5.52 COL 94.2 COLON-ALIGNED NO-LABEL WIDGET-ID 100
     fiLastRunLabel AT ROW 7.52 COL 2 NO-LABEL
     fiLastRun AT ROW 7.52 COL 17 COLON-ALIGNED NO-LABEL
     fiLastJobLabel AT ROW 7.52 COL 62.2 NO-LABEL
     fiLastJob AT ROW 7.52 COL 68.6 COLON-ALIGNED NO-LABEL
     fiAllocatedLabel AT ROW 19.71 COL 4 NO-LABEL
     fiJobLabel AT ROW 19.71 COL 52 NO-LABEL
     fiJob AT ROW 19.71 COL 60 COLON-ALIGNED NO-LABEL
     fiJobQtyLabel AT ROW 19.71 COL 122 NO-LABEL
     fiJobQty AT ROW 19.71 COL 136 COLON-ALIGNED NO-LABEL
     btCopy AT ROW 7.33 COL 125 WIDGET-ID 118
     btDelete AT ROW 32.67 COL 125
     btAdd AT ROW 32.67 COL 139
     btUpdate AT ROW 32.67 COL 191
     btAllocate AT ROW 32.67 COL 174
     btnExitText AT ROW 1.24 COL 187 NO-LABEL WIDGET-ID 24
     statusMessage AT ROW 32.67 COL 2 NO-LABEL WIDGET-ID 28
     btnViewRM AT ROW 2.91 COL 138.2 COLON-ALIGNED NO-LABEL WIDGET-ID 138
     btClear AT ROW 3.14 COL 197 WIDGET-ID 146
     btnClearText AT ROW 3.33 COL 184.2 NO-LABEL WIDGET-ID 148
     imJobLookup AT ROW 7.52 COL 100.6 WIDGET-ID 182
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 204.8 BY 36.19
         BGCOLOR 21 FGCOLOR 15 FONT 38 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: ASI.job
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Allocated Material"
         HEIGHT             = 32.81
         WIDTH              = 204.8
         MAX-HEIGHT         = 48.76
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 48.76
         VIRTUAL-WIDTH      = 273.2
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN btnClearText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN btnExitText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR BUTTON btnSBNotes IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnSBNotes:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiAllocatedLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiCreated IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCreatedLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiCSR IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCSRLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDue IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDueLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiJob IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiJobLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiJobQty IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiJobQtyLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiLastJob IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiLastJobLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiLastRun IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiLastRunLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiStatus IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiStatusLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN statusMessage IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Allocated Material */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Allocated Material */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  IF VALID-HANDLE(hdJobProcs) THEN
  DELETE PROCEDURE hdJobProcs.
  
  IF VALID-OBJECT(scInstance) THEN
  DELETE OBJECT scInstance NO-ERROR.
        
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAdd W-Win
ON CHOOSE OF btAdd IN FRAME F-Main /* Add New Material */
DO:
     DEFINE VARIABLE iForm AS INTEGER INIT 1 NO-UNDO.
     DEFINE VARIABLE iBlank AS INTEGER INIT 1 NO-UNDO.
     DEFINE VARIABLE cRmItem AS CHARACTER NO-UNDO.
     DEFINE VARIABLE cRmItemDesc AS CHARACTER NO-UNDO.
     DEFINE VARIABLE dAllocation AS DECIMAL NO-UNDO.
     DEFINE VARIABLE dAvailQty AS DECIMAL NO-UNDO.
     DEFINE VARIABLE lCreated AS LOGICAL NO-UNDO. 
     DEFINE VARIABLE rwRowid AS ROWID NO-UNDO.
     
     RUN pStatusMessage ("", 0).
     
     IF AVAIL job THEN
     DO:      
         RUN jc/dJobMat.w ("Add", cCompany, ROWID(job), INPUT-OUTPUT iForm, INPUT-OUTPUT iBlank, INPUT-OUTPUT cRmItem, INPUT-OUTPUT cRmItemDesc, INPUT-OUTPUT dAllocation, INPUT-OUTPUT dAvailQty, OUTPUT lCreated). 
         IF lCreated THEN
         DO:        
            RUN job_AllocationJobMaterial IN hdJobProcs ("Add", cCompany, rowid(job), iForm, iBlank, cRmItem, dAllocation, INPUT-OUTPUT rwRowid).            
            RUN pReOpenQuery IN h_b-job-mat(rwRowid). 
         END.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAllocate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAllocate W-Win
ON CHOOSE OF btAllocate IN FRAME F-Main /* Allocate */
DO:
     DEFINE VARIABLE cStatusMessage     AS CHARACTER NO-UNDO.
     RUN pStatusMessage("","0").
     IF AVAIL job THEN
     RUN pRunAlloc IN h_b-job-mat (YES, OUTPUT cStatusMessage).
     IF cStatusMessage NE "" THEN
     RUN pStatusMessage (cStatusMessage, "3").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btClear W-Win
ON CHOOSE OF btClear IN FRAME F-Main /* Reset */
DO:
    {methods/run_link.i "JOB-SOURCE" "SetJob" "(?,?,?,?)"}
    RUN pJobScan.

    {methods/run_link.i "JOB-SOURCE" "Reset"}
    
    {methods/run_link.i "JOB-SOURCE" "Set-Focus"}
    
    RUN pStatusMessage ("", 0).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCopy W-Win
ON CHOOSE OF btCopy IN FRAME F-Main /* Copy */
DO:
   DEFINE VARIABLE lComplete AS LOGICAL NO-UNDO.
   DEFINE VARIABLE rwRowid AS ROWID NO-UNDO.
   
   RUN pStatusMessage ("", 0).
                
   IF NOT AVAIL job THEN
   FIND FIRST job NO-LOCK
        WHERE job.company EQ cCompany
          AND job.job-no  EQ SUBSTRING(fiJob:SCREEN-VALUE,1,6)
          AND job.job-no2 EQ INTEGER(SUBSTRING(fiJob:SCREEN-VALUE,8,2))
        NO-ERROR.       
   IF AVAILABLE job THEN DO:
       RUN pCopyJob IN h_b-job-mat-last-all (job.company, ROWID(job), OUTPUT lComplete).
       RUN pSBNotes (YES).
   END. // if avail
   IF lComplete THEN
   RUN pStatusMessage ("Copy Complete", 2). 
   RUN pReOpenQuery IN h_b-job-mat(rwRowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelete W-Win
ON CHOOSE OF btDelete IN FRAME F-Main /* Delete */
DO:
     DEFINE VARIABLE cStatusMessage     AS CHARACTER NO-UNDO.
     DEFINE VARIABLE iForm AS INTEGER NO-UNDO.
     DEFINE VARIABLE iBlank AS INTEGER NO-UNDO.
     DEFINE VARIABLE cRmItem AS CHARACTER NO-UNDO.
     DEFINE VARIABLE cRmItemDesc AS CHARACTER NO-UNDO.
     DEFINE VARIABLE dAllocation AS DECIMAL NO-UNDO.
     DEFINE VARIABLE dAvailQty AS DECIMAL NO-UNDO.
     DEFINE VARIABLE rwRowid AS ROWID NO-UNDO.
     DEFINE VARIABLE ll AS LOGICAL NO-UNDO.
     
     RUN pStatusMessage("","0").      
     RUN pGetMaterial IN h_b-job-mat (OUTPUT rwRowid, OUTPUT iForm, OUTPUT iBlank, OUTPUT cRmItem, OUTPUT cRmItemDesc, OUTPUT dAllocation, OUTPUT dAvailQty).   
     
     RUN sharpshooter\messageDialog.w("Delete this material ?",
            YES,
            YES,
            NO,
            OUTPUT ll
            ).
     IF ll THEN 
     DO:
         RUN job_Delete_JobMaterial IN hdJobProcs(rwRowid).
         RUN pReOpenQuery IN h_b-job-mat(rwRowid).
     END.
             
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit W-Win
ON CHOOSE OF btExit IN FRAME F-Main
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    
    RETURN.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearText W-Win
ON MOUSE-SELECT-CLICK OF btnClearText IN FRAME F-Main
DO:
    APPLY "CHOOSE" TO btClear.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExitText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExitText W-Win
ON MOUSE-SELECT-CLICK OF btnExitText IN FRAME F-Main
DO:
    RUN dispatch ("exit").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst W-Win
ON CHOOSE OF btnFirst IN FRAME F-Main /* First */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLast W-Win
ON CHOOSE OF btnLast IN FRAME F-Main /* Last */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext W-Win
ON CHOOSE OF btnNext IN FRAME F-Main /* Next */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevious W-Win
ON CHOOSE OF btnPrevious IN FRAME F-Main /* Prev */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSBNotes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSBNotes W-Win
ON CHOOSE OF btnSBNotes IN FRAME F-Main /* SB Notes */
DO:
    RUN pSBNotes (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnViewRM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnViewRM W-Win
ON MOUSE-SELECT-CLICK OF btnViewRM IN FRAME F-Main
DO:
    RUN ChooseBtViewRMInquiry IN h_viewrminquiry.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdate W-Win
ON CHOOSE OF btUpdate IN FRAME F-Main /* Update */
DO:
     DEFINE VARIABLE iForm AS INTEGER NO-UNDO.
     DEFINE VARIABLE iBlank AS INTEGER NO-UNDO.
     DEFINE VARIABLE cRmItem AS CHARACTER NO-UNDO.
     DEFINE VARIABLE cRmItemDesc AS CHARACTER NO-UNDO.
     DEFINE VARIABLE dAllocation AS DECIMAL NO-UNDO.
     DEFINE VARIABLE dAvailQty AS DECIMAL NO-UNDO.
     DEFINE VARIABLE lUpdated AS LOGICAL NO-UNDO.
     DEFINE VARIABLE rwRowid AS ROWID NO-UNDO. 
     
     RUN pStatusMessage ("", 0).
     
     IF AVAIL job THEN
     DO:   
         RUN pGetMaterial IN h_b-job-mat (OUTPUT rwRowid, OUTPUT iForm, OUTPUT iBlank, OUTPUT cRmItem, OUTPUT cRmItemDesc, OUTPUT dAllocation, OUTPUT dAvailQty).   
         RUN jc/dJobMat.w("Update", cCompany, ROWID(job), INPUT-OUTPUT iForm, INPUT-OUTPUT iBlank, INPUT-OUTPUT cRmItem, INPUT-OUTPUT cRmItemDesc, INPUT-OUTPUT dAllocation, INPUT-OUTPUT dAvailQty, OUTPUT lUpdated). 
         
         IF lUpdated THEN
         DO:        
           RUN job_AllocationJobMaterial IN hdJobProcs("Update", cCompany, rowid(job), iForm, iBlank, cRmItem, dAllocation, INPUT-OUTPUT rwRowid). 
           RUN pReOpenQuery IN h_b-job-mat(rwRowid).                  
         END.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME imJobLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL imJobLookup W-Win
ON MOUSE-SELECT-CLICK OF imJobLookup IN FRAME F-Main
DO:
    DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.
    DEFINE VARIABLE cPerJob       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPreJob2      AS INTEGER   NO-UNDO.
    IF gcEstNo NE "" THEN
    DO:
        scInstance = SharedConfig:instance.
        scInstance:SetValue("ShowOnlyEstimateJob",gcEstNo).

        RUN windows/l-jobpo.w (
            INPUT  cCompany, 
            INPUT  fiLastJob:SCREEN-VALUE,  
            OUTPUT cFoundValue,   
            OUTPUT recFoundRecID
            ).
        
        IF cFoundValue NE "" THEN do:
            ASSIGN
            fiLastJob:SCREEN-VALUE = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', ENTRY(1,cFoundValue), ENTRY(2,cFoundValue)))
            cPerJob = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', ENTRY(1,cFoundValue))) 
            iPreJob2 =  INTEGER(ENTRY(2,cFoundValue)).        
            
           {methods\run_link.i "LastAll-SOURCE" "OpenQuery" "(cCompany,cPerJob,iPreJob2,giFormNo,giBlankNo)"}
        END.
            
        scInstance:DeleteValue(INPUT "ShowOnlyEstimateJob").  
    END.                               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

{sharpshooter/pStatusMessage.i}
{sharpshooter/ChangeWindowSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/adjustwindowsize.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_adjustwindowsize ).
       RUN set-position IN h_adjustwindowsize ( 1.00 , 154.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/jobfilter.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_jobfilter ).
       /* Position in AB:  ( 2.76 , 4.40 ) */
       /* Size in UIB:  ( 2.05 , 143.40 ) */

       /* Links to  h_jobfilter. */
       RUN add-link IN adm-broker-hdl ( h_jobfilter , 'JOB':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_jobfilter , 'State':U , THIS-PROCEDURE ).

    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/viewrminquiry.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_viewrminquiry ).
       RUN set-position IN h_viewrminquiry ( 2.71 , 161.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-job-mat-last-all.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-job-mat-last-all ).
       RUN set-position IN h_b-job-mat-last-all ( 9.29 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-job-mat-last-all ( 10.48 , 195.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-job-mat-all.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-job-mat ).
       RUN set-position IN h_b-job-mat ( 21.29 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-job-mat ( 10.48 , 195.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-job-mat-last-all. */
       RUN add-link IN adm-broker-hdl ( h_b-job-mat-last-all , 'LastAll':U , THIS-PROCEDURE ).

       /* Links to SmartBrowser h_b-job-mat. */
       RUN add-link IN adm-broker-hdl ( h_viewrminquiry , 'RMInq':U , h_b-job-mat ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'PAGE_1':U , h_b-job-mat ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'RMInq':U , h_b-job-mat ).
       RUN add-link IN adm-broker-hdl ( h_b-job-mat , 'Record':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-job-mat ,
             h_b-job-mat-last-all , 'AFTER':U ).
    END. /* Page 1 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "job"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "job"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fiStatusLabel fiStatus fiCreatedLabel fiCreated fiDueLabel fiDue 
          fiCSRLabel fiCSR fiLastRunLabel fiLastRun fiLastJobLabel fiLastJob 
          fiAllocatedLabel fiJobLabel fiJob fiJobQtyLabel fiJobQty btnExitText 
          statusMessage btnViewRM btnClearText 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btExit btnFirst btnLast btnNext btnPrevious btCopy btDelete btAdd 
         btUpdate btAllocate btnExitText btnViewRM btClear btnClearText 
         imJobLookup 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page AS INTEGER NO-UNDO.
  DEFINE VARIABLE dCol             AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dColTmp          AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dRow             AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dHeight          AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dWidth           AS DECIMAL NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).
        
  DO WITH FRAME {&FRAME-NAME}:
     
  END. /* with frame */  
              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy W-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF VALID-HANDLE(hdJobProcs) THEN
  DELETE PROCEDURE hdJobProcs.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    RUN pWinReSize.

    RUN spGetSessionParam("Company", OUTPUT cCompany).
    RUN spGetSessionParam("Location", OUTPUT cLocation).

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .
  
    /* Code placed here will execute AFTER standard behavior.    */
    FIND FIRST company NO-LOCK 
         WHERE company.company EQ cCompany NO-ERROR .
    IF AVAILABLE company THEN            
        {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " - " + DYNAMIC-FUNCTION("sfVersion") + " - " 
                             + STRING(company.name) + " - " + cLocation.

    //btnIssueQtyText:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    
    RUN pStatusMessage ("", 0).
    
    {methods/run_link.i "JOB-SOURCE" "HideJobDetails"}
    {methods/run_link.i "JOB-SOURCE" "Set-Focus"}
    {methods/run_link.i "JOB-SOURCE" "ValidateSameJobScan" "(FALSE)"}    
    {methods/run_link.i "JOB-SOURCE" "AllowEmptyFormAndBlank"}

    RUN spGetSettingByName ("ScheduleBoardNotes", OUTPUT cScheduleBoardNotes).
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            btnSBNotes:HIDDEN    = cScheduleBoardNotes EQ "NO"
            btnSBNotes:SENSITIVE = cScheduleBoardNotes EQ "YES"
            . 
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetJobQty W-Win 
PROCEDURE pGetJobQty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ipcJobNo    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ipiJobNo2   AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER ipiForm     AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER ipiBlank    AS INTEGER   NO-UNDO.
   DEFINE OUTPUT PARAMETER opiJobQty  AS INTEGER   NO-UNDO.
   
   DEFINE BUFFER bf-job-hdr FOR job-hdr.  
   
   FOR EACH bf-job-hdr NO-LOCK 
             WHERE bf-job-hdr.company EQ ipcCompany
             AND bf-job-hdr.job-no EQ ipcJobNo
             AND bf-job-hdr.job-no2 EQ ipiJobNo2
             AND (bf-job-hdr.frm EQ ipiForm OR ipiForm EQ ?)
             AND (bf-job-hdr.blank-no EQ ipiBlank OR ipiBlank EQ ? ):
             
        opiJobQty = opiJobQty + bf-job-hdr.qty .     
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetPrevoiusJob W-Win 
PROCEDURE pGetPrevoiusJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ipcJobNo    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ipiJobNo2   AS INTEGER   NO-UNDO.
   DEFINE OUTPUT PARAMETER opcPreJob   AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER opiPreJob2  AS INTEGER   NO-UNDO.
   DEFINE OUTPUT PARAMETER opiLastRun  AS INTEGER   NO-UNDO.
   DEFINE OUTPUT PARAMETER opcEstNo    AS CHARACTER NO-UNDO.
   
   DEFINE BUFFER bf-job-hdr FOR job-hdr.
   DEFINE BUFFER bf-job FOR job.
   
   FIND FIRST bf-job NO-LOCK
        WHERE bf-job.company EQ ipcCompany
        AND bf-job.job-no  EQ ipcJobNo 
        AND bf-job.job-no2 EQ (ipiJobNo2 - 1) NO-ERROR.
        
   IF NOT AVAIL bf-job THEN
      FIND LAST bf-job NO-LOCK
        WHERE bf-job.company EQ ipcCompany
        AND bf-job.est-no EQ job.est-no 
        AND rowid(bf-job) NE ROWID(job) NO-ERROR.
   
   IF AVAIL bf-job THEN
   DO:
        ASSIGN
        opcPreJob = bf-job.job-no
        opiPreJob2 = bf-job.job-no2
        opcEstNo = bf-job.est-no.
        FOR EACH job-mch NO-LOCK 
             WHERE job-mch.company EQ bf-job.company
             AND job-mch.job-no EQ bf-job.job-no
             AND job-mch.job-no2 EQ bf-job.job-no2 :
            opiLastRun = opiLastRun + (IF AVAIL job-mch THEN job-mch.run-qty ELSE 0).  
        END.
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJobError W-Win 
PROCEDURE pJobError :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStatusMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStatusMessageType AS INTEGER   NO-UNDO.
    
    {methods/run_link.i "JOB-SOURCE" "GetMessageAndType" "(OUTPUT cStatusMessage, OUTPUT iStatusMessageType)"}
                                 
    RUN pStatusMessage (cStatusMessage, iStatusMessageType).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJobScan W-Win 
PROCEDURE pJobScan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cJobNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFormNo  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBlankNo AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iJobQty  AS INTEGER   NO-UNDO.
    
    {methods/run_link.i "JOB-SOURCE" "GetJob" "(OUTPUT cJobNo, OUTPUT iJobNo2, OUTPUT iFormNo, OUTPUT iBlankNo)"}

    RUN pUpdateBrowse (
        INPUT cCompany,
        INPUT cJobNo,
        INPUT iJobNo2,
        INPUT iFormNo,
        INPUT iBlankNo
        ) NO-ERROR.
                     
    
    RUN select-page(1).
    DO WITH FRAME {&FRAME-NAME}:
       IF AVAILABLE job THEN do:  
          ASSIGN
            fiStatus:SCREEN-VALUE  = IF job.stat EQ "C" OR job.stat EQ "Z" THEN
                                         "Closed"
                                     ELSE IF job.stat EQ "H" THEN
                                         "Hold"
                                     ELSE
                                         "Open"
            fiCreated:SCREEN-VALUE = IF job.create-date EQ ? THEN
                                         ""
                                     ELSE
                                         STRING(job.create-date)
            fiDue:SCREEN-VALUE     = IF job.due-date EQ ? THEN
                                         ""
                                     ELSE
                                         STRING(job.due-date)
            fiCSR:SCREEN-VALUE     = csrUser_id
            fiJob:SCREEN-VALUE     = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', job.job-no, job.job-no2)).              
       END.
       ELSE DO:
            fiJob:SCREEN-VALUE     = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', cJobNo, iJobNo2)).
            fiStatus:SCREEN-VALUE  = "".
       END.
       
       RUN pGetJobQty(INPUT cCompany, INPUT cJobNo, INPUT iJobNo2, INPUT iFormNo, INPUT iBlankNo, OUTPUT iJobQty). 
       fiJobQty:SCREEN-VALUE = STRING(iJobQty). 
       assign
        giFormNo = iFormNo
        giBlankNo = iBlankNo
        .               
    
    END.    
    RUN select-page(1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNavigate W-Win 
PROCEDURE pNavigate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphNavPanel AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE hdCurrentBrowse AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cHandle         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLink           AS CHARACTER NO-UNDO.

    RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).   

    cLink = "PAGE_" + STRING(RETURN-VALUE) + "-TARGET".
 
    RUN get-link-handle IN adm-broker-hdl (
        INPUT THIS-PROCEDURE,
        INPUT cLink,
        OUTPUT cHandle
        ).
  
    IF NUM-ENTRIES(cHandle) LT 2 THEN
        hdCurrentBrowse = WIDGET-HANDLE(cHandle).
   
    IF VALID-HANDLE(hdCurrentBrowse) THEN
        RUN dispatch IN hdCurrentBrowse ("get-" + iphNavPanel:LABEL).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSBNotes W-Win 
PROCEDURE pSBNotes :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplCopy AS LOGICAL NO-UNDO.

    DEFINE VARIABLE iForm       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBlank      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cRmItem     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRmItemDesc AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dAllocation AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dAvailQty   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lUpdated    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE rwRowid     AS ROWID     NO-UNDO. 

    DEFINE BUFFER bsbNote FOR sbNote.

    IF AVAILABLE job THEN DO WITH FRAME {&FRAME-NAME}:
        CASE iplCopy:
            WHEN YES THEN DO:
                FOR EACH sbNote NO-LOCK
                    WHERE sbNote.company EQ cCompany
                      AND sbNote.job-no  EQ ENTRY(1,fiLastJob:SCREEN-VALUE,"-")
                      AND sbNote.job-no2 EQ INTEGER(ENTRY(2,fiLastJob:SCREEN-VALUE,"-"))
                      AND CAN-FIND(FIRST job-mch
                                   WHERE job-mch.company EQ sbNote.company
                                     AND job-mch.m-code  EQ sbNote.m-code
                                     AND job-mch.job-no  EQ job.job-no
                                     AND job-mch.job-no2 EQ job.job-no2
                                     AND job-mch.frm     EQ sbNote.frm)
                    :
                    CREATE bsbNote.
                    BUFFER-COPY sbNote EXCEPT job-no job-no2 rec_key TO bsbNote
                        ASSIGN
                            bsbNote.job-no  = job.job-no
                            bsbNote.job-no2 = job.job-no2
                            .
                END. // each sbnote
            END. // YES
            WHEN NO THEN DO: 
                RUN pGetMaterial IN h_b-job-mat (
                    OUTPUT rwRowid,
                    OUTPUT iForm,
                    OUTPUT iBlank,
                    OUTPUT cRmItem,
                    OUTPUT cRmItemDesc,
                    OUTPUT dAllocation,
                    OUTPUT dAvailQty
                    ).   
                IF rwRowID NE ? THEN DO:
                    FIND FIRST job-mat NO-LOCK
                         WHERE ROWID(job-mat) EQ rwRowID
                         NO-ERROR.
                    IF AVAILABLE job-mat THEN
                        RUN schedule/objects/prompts/externalNotes.w (
                            job-mat.company,
                            job-mat.job-no,
                            job-mat.job-no2,
                            job-mat.frm
                            ).
                END. // if rwrowid ne ?
            END. // NO
        END CASE.
    END. // if avail job

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pStatusClear W-Win 
PROCEDURE pStatusClear :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   RUN pStatusMessage("","0").
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateBrowse W-Win 
PROCEDURE pUpdateBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo  AS INTEGER   NO-UNDO.

    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE cPerJob  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPreJob2 AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dLastRun AS DECIMAL   NO-UNDO.

    gcEstNo = "".
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
   
    {methods\run_link.i "Record-SOURCE" "OpenQuery" "(ipcCompany,ipcJobNo,ipiJobNo2,ipiFormNo,ipiBlankNo)"}
    
    RUN pGetPrevoiusJob(ipcCompany, ipcJobNo, ipiJobNo2, OUTPUT cPerJob, OUTPUT iPreJob2, OUTPUT dLastRun, OUTPUT gcEstNo).
    
    IF cPerJob NE "" THEN
    ASSIGN
        fiLastRun:SCREEN-VALUE = string(dLastRun)
        fiLastJob:SCREEN-VALUE =  STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', cPerJob, iPreJob2)).
     ELSE 
     ASSIGN
     fiLastRun:SCREEN-VALUE = ""
     fiLastJob:SCREEN-VALUE = "" .
                    
    {methods\run_link.i "LastAll-SOURCE" "OpenQuery" "(ipcCompany,cPerJob,iPreJob2,ipiFormNo,ipiBlankNo)"}

    RUN pStatusMessage ("", 0).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateJobMaterial W-Win 
PROCEDURE pUpdateJobMaterial :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER iprwRowid     AS ROWID   NO-UNDO.
   DEFINE INPUT PARAMETER ipiForm       AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER ipiBlank      AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER ipcRmItem     AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ipdAllocation AS DECIMAL NO-UNDO.
   
   
   DEFINE BUFFER bf-job-mat FOR job-mat.
   
   FIND FIRST bf-job-mat EXCLUSIVE-LOCK
        WHERE bf-job-mat.company EQ cCompany
        AND ROWID(bf-job-mat) EQ iprwRowid NO-ERROR.
   IF avail bf-job-mat THEN     
    ASSIGN            
       bf-job-mat.rm-i-no  = ipcRmItem        
       bf-job-mat.qty-all  = ipdAllocation       
       .     
   RELEASE bf-job-mat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize W-Win 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dCol    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dColTmp AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dRow    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dHeight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dWidth  AS DECIMAL NO-UNDO.

    SESSION:SET-WAIT-STATE("General").
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            statusMessage:ROW    = {&WINDOW-NAME}:HEIGHT - .86
            statusMessage:SCREEN-VALUE = "test"
            dCol                 = {&WINDOW-NAME}:WIDTH  - 8
            btExit:COL           = dCol
            btnFirst:COL         = dCol - 1
            btnPrevious:COL      = dCol - 1
            btnNext:COL          = dCol - 1
            btnLast:COL          = dCol - 1
            btnExitText:COL      = dCol - 9
            btnClearText:COL     = dCol - 13
            btClear:COL          = dCol - 1           
            //btnPrintJobText:COL  = dCol - btnPrintJobText:WIDTH - 1
            btDelete:ROW         = {&WINDOW-NAME}:HEIGHT - .96
            btAllocate:ROW       = {&WINDOW-NAME}:HEIGHT - .96
            btAdd:ROW            = {&WINDOW-NAME}:HEIGHT - .96
            btUpdate:ROW         = {&WINDOW-NAME}:HEIGHT - .96
            btnSBnotes:ROW       = {&WINDOW-NAME}:HEIGHT - .96
            dRow                 = {&WINDOW-NAME}:HEIGHT - 1
            .        
        RUN get-position IN h_b-job-mat ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
        RUN get-size IN h_b-job-mat ( OUTPUT dHeight , OUTPUT dWidth ) NO-ERROR.
        ASSIGN
            dHeight = {&WINDOW-NAME}:HEIGHT - dRow - 1.33
            dWidth  = dCol - 3
            .      
        RUN set-size IN h_b-job-mat ( dHeight , dWidth ) NO-ERROR.
        RUN set-size IN h_b-job-mat-last-all ( 10.48 , dWidth ) NO-ERROR.
        RUN set-position IN h_adjustwindowsize ( 1.00 , dCol - 45 ) NO-ERROR.
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScanJob W-Win 
PROCEDURE ScanJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo  AS INTEGER   NO-UNDO.

    SESSION:SET-WAIT-STATE("GENERAL").   
    
    {methods/run_link.i "JOB-SOURCE" "SetJob" "(ipcJobNo,ipiJobNo2,?,?)"}
    
    {methods/run_link.i "JOB-SOURCE" "DisableAll"}
            
    SESSION:SET-WAIT-STATE("").            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "job"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.    
    
    CASE p-state:
        WHEN "job-valid" THEN
            RUN pJobScan.
        WHEN "job-error" THEN
            RUN pJobError.
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

