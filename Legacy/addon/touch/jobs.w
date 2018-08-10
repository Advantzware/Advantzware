&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: touch/jobs.w

  Description: from SMART.W - Template for basic SmartObject

  Author: Ron Stark
  Created: 4.18.2000

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE v-autopo-sec AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-access-close AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-access-list AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSort AS CHARACTER NO-UNDO INITIAL "Job".

/* Check if authorized enter job */
RUN methods/prgsecur.p
    (INPUT "TSEntJob",
     INPUT "ACCESS",
     INPUT YES,
     INPUT YES,
     INPUT YES,
     OUTPUT v-autopo-sec,
     OUTPUT v-access-close,
     OUTPUT v-access-list).

&SCOPED-DEFINE PageNo 9
{touch/touchdef.i}

&Scoped-define BUTTON-INCLUDE JOBS

DEFINE TEMP-TABLE tt-job NO-UNDO
    FIELD job-no  LIKE job.job-no
    FIELD job-no2 LIKE job.job-no2
    FIELD sortKey   AS CHARACTER 
    INDEX sortKey IS PRIMARY sortKey
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_Button-1 Btn_Button-6 ~
Btn_Button-11 Btn_Button-16 Btn_Button-21 Btn_Button-26 Btn_Button-2 ~
Btn_Button-7 Btn_Button-12 Btn_Button-17 Btn_Button-22 Btn_Button-27 ~
Btn_Button-3 Btn_Button-8 Btn_Button-13 Btn_Button-18 Btn_Button-23 ~
Btn_Button-28 Btn_Button-4 Btn_Button-9 Btn_Button-14 Btn_Button-19 ~
Btn_Button-24 Btn_Button-29 Btn_Button-5 Btn_Button-10 Btn_Button-15 ~
Btn_Button-20 Btn_Button-25 Btn_Button-30 
&Scoped-Define DISPLAYED-OBJECTS sortBy 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Btn_Button-1 Btn_Button-6 Btn_Button-11 Btn_Button-16 ~
Btn_Button-21 Btn_Button-26 Btn_Button-2 Btn_Button-7 Btn_Button-12 ~
Btn_Button-17 Btn_Button-22 Btn_Button-27 Btn_Button-3 Btn_Button-8 ~
Btn_Button-13 Btn_Button-18 Btn_Button-23 Btn_Button-28 Btn_Button-4 ~
Btn_Button-9 Btn_Button-14 Btn_Button-19 Btn_Button-24 Btn_Button-29 ~
Btn_Button-5 Btn_Button-10 Btn_Button-15 Btn_Button-20 Btn_Button-25 ~
Btn_Button-30 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Button-1 
     LABEL "BUTTON1" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-10 
     LABEL "BUTTON10" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-11 
     LABEL "BUTTON11" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-12 
     LABEL "BUTTON12" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-13 
     LABEL "BUTTON13" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-14 
     LABEL "BUTTON14" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-15 
     LABEL "BUTTON15" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-16 
     LABEL "BUTTON16" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-17 
     LABEL "BUTTON17" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-18 
     LABEL "BUTTON18" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-19 
     LABEL "BUTTON19" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-2 
     LABEL "BUTTON2" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-20 
     LABEL "BUTTON20" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-21 
     LABEL "BUTTON21" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-22 
     LABEL "BUTTON22" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-23 
     LABEL "BUTTON23" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-24 
     LABEL "BUTTON24" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-25 
     LABEL "BUTTON25" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-26 
     LABEL "BUTTON26" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-27 
     LABEL "BUTTON27" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-28 
     LABEL "BUTTON28" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-29 
     LABEL "BUTTON29" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-3 
     LABEL "BUTTON3" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-30 
     LABEL "BUTTON30" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-4 
     LABEL "BUTTON4" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-5 
     LABEL "BUTTON5" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-6 
     LABEL "BUTTON6" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-7 
     LABEL "BUTTON7" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-8 
     LABEL "BUTTON8" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Button-9 
     LABEL "BUTTON9" 
     SIZE 14 BY 2.38.

DEFINE VARIABLE sortBy AS CHARACTER FORMAT "X(256)":U INITIAL "Sorted by Jobs" 
      VIEW-AS TEXT 
     SIZE 18.4 BY .52
     BGCOLOR 14  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124 BY 12.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_Button-1 AT ROW 1.71 COL 2
     Btn_Button-6 AT ROW 1.71 COL 16
     Btn_Button-11 AT ROW 1.71 COL 30
     Btn_Button-16 AT ROW 1.71 COL 44
     Btn_Button-21 AT ROW 1.71 COL 58
     Btn_Button-26 AT ROW 1.71 COL 72
     Btn_Button-2 AT ROW 4.1 COL 2
     Btn_Button-7 AT ROW 4.1 COL 16
     Btn_Button-12 AT ROW 4.1 COL 30
     Btn_Button-17 AT ROW 4.1 COL 44
     Btn_Button-22 AT ROW 4.1 COL 58
     Btn_Button-27 AT ROW 4.1 COL 72
     Btn_Button-3 AT ROW 6.48 COL 2
     Btn_Button-8 AT ROW 6.48 COL 16
     Btn_Button-13 AT ROW 6.48 COL 30
     Btn_Button-18 AT ROW 6.48 COL 44
     Btn_Button-23 AT ROW 6.48 COL 58
     Btn_Button-28 AT ROW 6.48 COL 72
     Btn_Button-4 AT ROW 8.86 COL 2
     Btn_Button-9 AT ROW 8.86 COL 16
     Btn_Button-14 AT ROW 8.86 COL 30
     Btn_Button-19 AT ROW 8.86 COL 44
     Btn_Button-24 AT ROW 8.86 COL 58
     Btn_Button-29 AT ROW 8.86 COL 72
     Btn_Button-5 AT ROW 11.24 COL 2
     Btn_Button-10 AT ROW 11.24 COL 16
     Btn_Button-15 AT ROW 11.24 COL 30
     Btn_Button-20 AT ROW 11.24 COL 44
     Btn_Button-25 AT ROW 11.24 COL 58
     Btn_Button-30 AT ROW 11.24 COL 72
     sortBy AT ROW 1.24 COL 65 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     "JOBS" VIEW-AS TEXT
          SIZE 8 BY .52 AT ROW 1.24 COL 3 WIDGET-ID 2
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FONT 6.


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
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 12.95
         WIDTH              = 124.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON Btn_Button-1 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-10 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-11 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-12 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-13 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-14 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-15 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-16 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-17 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-18 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-19 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-2 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-20 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-21 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-22 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-23 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-24 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-25 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-26 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-27 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-28 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-29 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-3 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-30 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-4 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-5 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-6 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-7 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-8 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-9 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN sortBy IN FRAME F-Main
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_Button-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-1 s-object
ON CHOOSE OF Btn_Button-1 IN FRAME F-Main /* BUTTON1 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-10 s-object
ON CHOOSE OF Btn_Button-10 IN FRAME F-Main /* BUTTON10 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-11 s-object
ON CHOOSE OF Btn_Button-11 IN FRAME F-Main /* BUTTON11 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-12 s-object
ON CHOOSE OF Btn_Button-12 IN FRAME F-Main /* BUTTON12 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-13 s-object
ON CHOOSE OF Btn_Button-13 IN FRAME F-Main /* BUTTON13 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-14 s-object
ON CHOOSE OF Btn_Button-14 IN FRAME F-Main /* BUTTON14 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-15 s-object
ON CHOOSE OF Btn_Button-15 IN FRAME F-Main /* BUTTON15 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-16 s-object
ON CHOOSE OF Btn_Button-16 IN FRAME F-Main /* BUTTON16 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-17 s-object
ON CHOOSE OF Btn_Button-17 IN FRAME F-Main /* BUTTON17 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-18 s-object
ON CHOOSE OF Btn_Button-18 IN FRAME F-Main /* BUTTON18 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-19 s-object
ON CHOOSE OF Btn_Button-19 IN FRAME F-Main /* BUTTON19 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-2 s-object
ON CHOOSE OF Btn_Button-2 IN FRAME F-Main /* BUTTON2 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-20 s-object
ON CHOOSE OF Btn_Button-20 IN FRAME F-Main /* BUTTON20 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-21 s-object
ON CHOOSE OF Btn_Button-21 IN FRAME F-Main /* BUTTON21 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-22
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-22 s-object
ON CHOOSE OF Btn_Button-22 IN FRAME F-Main /* BUTTON22 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-23
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-23 s-object
ON CHOOSE OF Btn_Button-23 IN FRAME F-Main /* BUTTON23 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-24
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-24 s-object
ON CHOOSE OF Btn_Button-24 IN FRAME F-Main /* BUTTON24 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-25
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-25 s-object
ON CHOOSE OF Btn_Button-25 IN FRAME F-Main /* BUTTON25 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-26
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-26 s-object
ON CHOOSE OF Btn_Button-26 IN FRAME F-Main /* BUTTON26 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-27
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-27 s-object
ON CHOOSE OF Btn_Button-27 IN FRAME F-Main /* BUTTON27 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-28
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-28 s-object
ON CHOOSE OF Btn_Button-28 IN FRAME F-Main /* BUTTON28 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-29
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-29 s-object
ON CHOOSE OF Btn_Button-29 IN FRAME F-Main /* BUTTON29 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-3 s-object
ON CHOOSE OF Btn_Button-3 IN FRAME F-Main /* BUTTON3 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-30
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-30 s-object
ON CHOOSE OF Btn_Button-30 IN FRAME F-Main /* BUTTON30 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-4 s-object
ON CHOOSE OF Btn_Button-4 IN FRAME F-Main /* BUTTON4 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-5 s-object
ON CHOOSE OF Btn_Button-5 IN FRAME F-Main /* BUTTON5 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-6 s-object
ON CHOOSE OF Btn_Button-6 IN FRAME F-Main /* BUTTON6 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-7 s-object
ON CHOOSE OF Btn_Button-7 IN FRAME F-Main /* BUTTON7 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-8 s-object
ON CHOOSE OF Btn_Button-8 IN FRAME F-Main /* BUTTON8 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-9 s-object
ON CHOOSE OF Btn_Button-9 IN FRAME F-Main /* BUTTON9 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */
 
/* If testing in the UIB, initialize the SmartObject. */ 
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF
  
{touch/pCreateINIObjects.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Button_Labels s-object 
PROCEDURE Button_Labels :
/*------------------------------------------------------------------------------
  Purpose:     place values on button labels
  Parameters:  Input current button item
  Notes:       
------------------------------------------------------------------------------*/
  {touch/btnlabel.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-job-status s-object 
PROCEDURE check-job-status :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-gang-jobs AS LOG NO-UNDO.

  find first job where job.company = company_code and
                       job.job-no =  SUBSTR(job#,1,INDEX(job#,'-') - 1) and
                       job.job-no2 = integer(SUBSTR(job#,INDEX(job#,'-') + 1))       
                        
                       no-lock no-error.                                
  if avail job and job.stat = "H" then do:
     message "JOB ON HOLD. DO NOT PROCEED!" 
              VIEW-AS alert-box.
     return error.
  end.
  /* task# 08260515  don't allow to do any operation for other job if the machine is running for a job */
  DEF BUFFER bf-machtran FOR machtran.
  DEF BUFFER bf-mach FOR mach.

  {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('machine_code',OUTPUT machine_code)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('job_number',OUTPUT job_number)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('job_sub',OUTPUT job_sub)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('machine_list',OUTPUT machine_list)"}
  /* task# 10110517 allow duplicate time if gang jobs is yes*/
  FIND FIRST bf-mach WHERE
       bf-mach.company EQ company_code AND
       bf-mach.m-code  EQ machine_code
       NO-LOCK NO-ERROR.

  v-gang-jobs = IF AVAIL bf-mach THEN bf-mach.gang-jobs ELSE NO.

  FIND FIRST bf-machtran NO-LOCK WHERE bf-machtran.company = company_code 
                       AND (bf-machtran.machine = machine_code OR
                            LOOKUP(bf-machtran.machine,machine_list) > 0 )
                       AND (bf-machtran.job_number NE job_number
                           OR bf-machtran.job_sub NE int(job_sub) )
                       AND bf-machtran.END_date = ?
                       AND bf-machtran.end_time = 0 
                       AND bf-machtran.TOTAL_time = 0
                       NO-ERROR.

  IF AVAIL bf-machtran AND NOT v-gang-jobs THEN do:
     MESSAGE "Job " + trim(bf-machtran.job_number) + "-" + TRIM(string(bf-machtran.job_sub,"99")) +
             " has data collection transaction started. You must end that job's operation before selecting a new job."
             VIEW-AS ALERT-BOX ERROR.
     RETURN error.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Jobs s-object 
PROCEDURE Get_Jobs :
/*------------------------------------------------------------------------------
  Purpose:     populate selection list with job numbers
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-sort-by AS cha NO-UNDO.
  
  {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('machine_code',OUTPUT machine_code)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('machine_list',OUTPUT machine_list)"}
  ASSIGN
    itemlist = ''
    button_item = 1.
    
  def var lv-stat like job.stat extent 2 no-undo.
  def var i as int no-undo.
  def var b as int no-undo.
  DEF VAR lv-form-completed AS LOG NO-UNDO.
  DEF BUFFER bf-jobmch FOR job-mch.
  DEFINE VARIABLE cSortKey AS CHARACTER NO-UNDO.

  EMPTY TEMP-TABLE tt-job.

IF ip-sort-by = "JOB" THEN
  do i = 1 to 2:
     assign
        lv-stat[1] = lv-stat[2]
        lv-stat[2] = if i eq 1 then "C" else "Z".

     for each job
         where job.company eq company_code /*'001'*/
           and job.stat    gt lv-stat[1]
           and job.stat    lt lv-stat[2]  
           AND CAN-FIND(FIRST job-hdr WHERE job-hdr.company = job.company 
                        AND job-hdr.job = job.job AND job-hdr.job-no = job.job-no
                        AND job-hdr.job-no2 = job.job-no2)
           no-lock
           BREAK BY job.job-no BY job.job-no2:
         
        IF FIRST-OF(job.job-no2) THEN DO:
           FOR EACH job-mch OF job NO-LOCK
               WHERE (job-mch.m-code EQ machine_code
                  OR LOOKUP(job-mch.m-code,machine_list) GT 0)
                 AND job-mch.run-complete EQ NO
               BREAK BY job-mch.frm BY job-mch.blank-no:

              IF FIRST-OF(job-mch.blank-no) THEN DO:
                 lv-form-completed = YES.
                 FOR EACH bf-jobmch OF job NO-LOCK where (bf-jobmch.m-code eq machine_code OR 
                                                   LOOKUP(bf-jobmch.m-code,machine_list) > 0):
                     lv-form-completed = IF NOT bf-jobmch.run-complete THEN NO ELSE lv-form-completed.
                 END.                 
                 IF NOT CAN-FIND(FIRST cmpltjob WHERE cmpltjob.company = company_code
                                  AND cmpltjob.machine = machine_code
                                  AND cmpltjob.job_number = job-mch.job-no
                                  AND cmpltjob.job_sub = job-mch.job-no2
                                  AND cmpltjob.FORM_number = job-mch.frm
                                  AND cmpltjob.blank_number = job-mch.blank-no)                    
                    AND NOT lv-form-completed
                 THEN DO:
                      IF NOT CAN-FIND(FIRST tt-job
                                      WHERE tt-job.job-no  EQ job.job-no
                                        AND tt-job.job-no2 EQ job.job-no2) THEN DO: 
                         create tt-job.
                         assign tt-job.job-no  = job.job-no
                                tt-job.job-no2 = job.job-no2
                                tt-job.sortKey = job.job-no + STRING(job.job-no2,"99")
                                .
                      END.
                 END.
              END.
           END. /*job-mch*/
        END. /* first-of(job-no2) */
     end.  /* for each job */
  end.

 ELSE /* start-date */
  do i = 1 to 2:
     assign
        lv-stat[1] = lv-stat[2]
        lv-stat[2] = if i eq 1 then "C" else "Z".

     for each job no-lock
         where job.company eq company_code /*'001'*/
           and job.stat    gt lv-stat[1]
           and job.stat    lt lv-stat[2]
           AND CAN-FIND(FIRST job-hdr WHERE job-hdr.company = job.company 
                        AND job-hdr.job = job.job AND job-hdr.job-no = job.job-no
                        AND job-hdr.job-no2 = job.job-no2)
           BREAK BY job.start-date BY job.job-no BY job.job-no2:
         
        IF FIRST-OF(job.job-no2) THEN DO:
           FOR EACH job-mch OF job NO-LOCK
               WHERE job-mch.m-code EQ machine_code
                 AND job-mch.run-complete EQ NO
               BREAK BY job-mch.frm BY job-mch.blank-no:
              IF FIRST-OF(job-mch.blank-no) THEN DO:
                 lv-form-completed = YES.
                 FOR EACH bf-jobmch OF job NO-LOCK where (bf-jobmch.m-code eq machine_code OR 
                                                   LOOKUP(bf-jobmch.m-code,machine_list) > 0):
                     lv-form-completed = IF NOT bf-jobmch.run-complete THEN NO ELSE lv-form-completed.
                 END.
                 IF NOT CAN-FIND(FIRST cmpltjob WHERE cmpltjob.company = company_code
                                  AND cmpltjob.machine = machine_code
                                  AND cmpltjob.job_number = job-mch.job-no
                                  AND cmpltjob.job_sub = job-mch.job-no2
                                  AND cmpltjob.FORM_number = job-mch.frm
                                  AND cmpltjob.blank_number = job-mch.blank-no)
/*                    AND NOT lv-form-completed*/
                 THEN DO:
                      IF NOT CAN-FIND(FIRST tt-job
                                      WHERE tt-job.job-no  EQ job.job-no
                                        AND tt-job.job-no2 EQ job.job-no2) THEN DO: 
                         create tt-job.
                         assign tt-job.job-no  = job.job-no
                                tt-job.job-no2 = job.job-no2
                                tt-job.sortKey = IF job-mch.start-date NE ? AND
                                                    job-mch.start-date GE TODAY - 365 THEN
                                                 STRING(YEAR(job-mch.start-date),"9999")
                                               + STRING(MONTH(job-mch.start-date),"99")
                                               + STRING(DAY(job-mch.start-date),"99")
                                               + STRING(job-mch.start-time,"99999")
                                                 ELSE "9999999999999" + job.job-no + STRING(job.job-no2,"99")
                                .
                      END.
                 END.
              END.
           END. /*job-mch*/
        END. /* first-of(job-no2) */
     end.  /* for each job */
  end.

  for each tt-job BY tt-job.sortKey:
      itemlist = itemlist + tt-job.job-no + '-' + STRING(tt-job.job-no2) + '@'.
  end.
  itemlist = TRIM(itemlist,'@').

  RUN Button_Labels (INPUT-OUTPUT button_item).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize s-object 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DISPLAY sortBy WITH FRAME {&FRAME-NAME}.
  RUN pCreateINIObjects
    ("First,Last,HomeSmall,JobList,EnterJob,PageUpSmall,PageDownSmall,Schedule,Sort,BackSmall").
  
  RUN pSetSensitive ("EnterJob",v-autopo-sec).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view s-object 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {touch/localview.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClick s-object 
PROCEDURE pClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcClick AS CHARACTER NO-UNDO.
    
    CASE ipcClick:
        WHEN "HomeSmall" THEN DO:
            {methods/run_link.i "CONTAINER" "Change_Page" "(2)"}
        END.
        WHEN "PageUpSmall" THEN DO:
            button_item = button_item - 60.
            RUN Button_Labels (INPUT-OUTPUT button_item).
        END.
        WHEN "PageDownSmall" THEN DO:
            RUN Button_Labels (INPUT-OUTPUT button_item).
        END.
        WHEN "First" THEN DO:
            button_item = 1.
            RUN Button_Labels (INPUT-OUTPUT button_item).
        END.
        WHEN "Last" THEN DO:
            button_item = NUM-ENTRIES(itemlist,'@') + 1.
            RUN Button_Labels (INPUT-OUTPUT button_item).
        END.
        WHEN "BackSmall" THEN DO:
            {methods/run_link.i "CONTAINER" "Change_Page" "(5)"}
        END.
        WHEN "JobList" THEN DO:
            {methods/run_link.i "CONTAINER" "Change_Page" "(15)"}
        END.
        WHEN "EnterJob" THEN DO:
            {methods/run_link.i "CONTAINER" "Change_Page" "(16)"}
        END.
        WHEN "Schedule" THEN DO:
            RUN schedule-proc.
        END.
        WHEN "Sort" THEN DO WITH FRAME {&FRAME-NAME}:
            cSort = IF cSort EQ "Job" THEN "Start" ELSE "Job".
            sortBy:SCREEN-VALUE = "Sorted by " + cSort.
            RUN Get_jobs (cSort).
            {touch/localview.i}
        END.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE schedule-proc s-object 
PROCEDURE schedule-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF LOOKUP(PROPATH,"../") EQ 0 THEN
    PROPATH = "../," + PROPATH.

  FIND FIRST sys-ctrl NO-LOCK
       WHERE sys-ctrl.company EQ company_code
         AND sys-ctrl.name    EQ "SCHEDULE"
       NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = company_code
     sys-ctrl.name     = "SCHEDULE"
     sys-ctrl.descrip  = "Update Order Due date and Promise date via Scheduled Job Start Date?"
     sys-ctrl.char-fld = "None"
     .
    FIND CURRENT sys-ctrl NO-LOCK.
  END.
  IF sys-ctrl.log-fld THEN RUN VALUE(SEARCH('schedule\sbView.r')).
  ELSE
  MESSAGE "Scheduler View Not Enabled - Please Refer to System Administrator."
  VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed s-object 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

