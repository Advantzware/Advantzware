&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: jobDetail.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 5.9.2004

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{schedule/scopDir.i}
{{&includes}/defBoard.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{{&includes}/ttblJob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define DISPLAYED-OBJECTS job jobSequence startDate startTime ~
origStartDate origStartTime dueDate dueTime resource resourceSequence ~
endDate endTime origEndDate origEndTime timeSpan downtimeSpan 

/* Custom List Definitions                                              */
/* ttblJobFields,timeFields,List-3,List-4,List-5,List-6                 */
&Scoped-define ttblJobFields job jobSequence startDate origStartDate ~
dueDate resource resourceSequence endDate origEndDate 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE downtimeSpan AS CHARACTER FORMAT "X(256)":U 
     LABEL "Downtime Span" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE dueDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Due" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE dueTime AS CHARACTER FORMAT "X(256)":U 
     LABEL "@" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE endDate AS DATE FORMAT "99/99/9999":U 
     LABEL "End" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE endTime AS CHARACTER FORMAT "X(256)":U 
     LABEL "@" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE job AS CHARACTER FORMAT "X(256)":U 
     LABEL "Job" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE jobSequence AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Job Sequence" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE origEndDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Orig End" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE origEndTime AS CHARACTER FORMAT "X(256)":U 
     LABEL "@" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE origStartDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Orig Start" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE origStartTime AS CHARACTER FORMAT "X(256)":U 
     LABEL "@" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE resource AS CHARACTER FORMAT "X(256)":U 
     LABEL "Resource" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE resourceSequence AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Resource Seq" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE startDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Start" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE startTime AS CHARACTER FORMAT "X(256)":U 
     LABEL "@" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE timeSpan AS CHARACTER FORMAT "X(256)":U 
     LABEL "Time Span" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE IMAGE lockImage
     FILENAME "adeicon/blank":U
     SIZE 2.8 BY .67.

DEFINE IMAGE noteImage
     FILENAME "schedule/images/notetack.bmp":U
     SIZE 3.6 BY .86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     job AT ROW 1.24 COL 11 COLON-ALIGNED
     jobSequence AT ROW 1.24 COL 43 COLON-ALIGNED
     startDate AT ROW 1.24 COL 54 COLON-ALIGNED
     startTime AT ROW 1.24 COL 74 COLON-ALIGNED
     origStartDate AT ROW 1.24 COL 98 COLON-ALIGNED
     origStartTime AT ROW 1.24 COL 118 COLON-ALIGNED
     dueDate AT ROW 1.24 COL 143 COLON-ALIGNED
     dueTime AT ROW 1.24 COL 163 COLON-ALIGNED
     resource AT ROW 2.43 COL 11 COLON-ALIGNED
     resourceSequence AT ROW 2.43 COL 43 COLON-ALIGNED
     endDate AT ROW 2.43 COL 54 COLON-ALIGNED
     endTime AT ROW 2.43 COL 74 COLON-ALIGNED
     origEndDate AT ROW 2.43 COL 98 COLON-ALIGNED
     origEndTime AT ROW 2.43 COL 118 COLON-ALIGNED
     timeSpan AT ROW 2.43 COL 143 COLON-ALIGNED
     downtimeSpan AT ROW 2.43 COL 168 COLON-ALIGNED
     lockImage AT ROW 1.48 COL 5
     noteImage AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 178.4 BY 2.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW sObject ASSIGN
         HEIGHT             = 2.67
         WIDTH              = 178.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB sObject 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW sObject
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN downtimeSpan IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dueDate IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN dueTime IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endDate IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN endTime IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN job IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN jobSequence IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR IMAGE lockImage IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE noteImage IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN origEndDate IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN origEndTime IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN origStartDate IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN origStartTime IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN resource IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN resourceSequence IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN startDate IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN startTime IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN timeSpan IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK sObject 


/* ***************************  Main Block  *************************** */

{{&includes}/{&Board}/detailProc.i}

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI sObject  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFields sObject 
PROCEDURE displayFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.

  DEFINE VARIABLE lockImageName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE noteRowID AS ROWID NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    FIND ttblJob NO-LOCK WHERE ROWID(ttblJob) EQ ipRowID NO-ERROR.
    {{&viewers}/includes/jobDetail.i ttblJob}
    ELSE DO:
      FIND pendingJob NO-LOCK WHERE ROWID(pendingJob) EQ ipRowID NO-ERROR.
      {{&viewers}/includes/jobDetail.i pendingJob}
    END.
    lockImage:LOAD-IMAGE(lockImageName).
    DISPLAY {&displayed-objects}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

