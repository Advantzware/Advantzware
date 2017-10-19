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
 DEFINE VARIABLE v-access-close AS LOGICAL.
 DEFINE VARIABLE v-access-list AS CHARACTER.
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
  
{touch/touchdef.i}

&Scoped-define BUTTON-INCLUDE JOBS

def temp-table tt-job field job-no like job.job-no
                      field job-no2 like job.job-no2
                      /*index job is primary job-no*/ .

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
Btn_Button-11 Btn_Button-16 Btn_Button-21 Btn_Button-26 Btn_schedule ~
Btn_Close Btn_Button-2 Btn_Button-7 Btn_Button-12 Btn_Button-17 ~
Btn_Button-22 Btn_Button-27 Btn_Job_List /*Btn_Enter_Job*/ Btn_Page_Up ~
Btn_Page_Down Btn_Button-3 Btn_Button-8 Btn_Button-13 Btn_Button-18 ~
Btn_Button-23 Btn_Button-28 Btn_First Btn_Last Btn_Button-4 Btn_Button-9 ~
Btn_Button-14 Btn_Button-19 Btn_Button-24 Btn_Button-29 Btn_sort ~
Btn_Button-5 Btn_Button-10 Btn_Button-15 Btn_Button-20 Btn_Button-25 ~
Btn_Button-30 Btn_Cancel 

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
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-10 
     LABEL "BUTTON10" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-11 
     LABEL "BUTTON11" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-12 
     LABEL "BUTTON12" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-13 
     LABEL "BUTTON13" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-14 
     LABEL "BUTTON14" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-15 
     LABEL "BUTTON15" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-16 
     LABEL "BUTTON16" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-17 
     LABEL "BUTTON17" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-18 
     LABEL "BUTTON18" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-19 
     LABEL "BUTTON19" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-2 
     LABEL "BUTTON2" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-20 
     LABEL "BUTTON20" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-21 
     LABEL "BUTTON21" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-22 
     LABEL "BUTTON22" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-23 
     LABEL "BUTTON23" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-24 
     LABEL "BUTTON24" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-25 
     LABEL "BUTTON25" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-26 
     LABEL "BUTTON26" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-27 
     LABEL "BUTTON27" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-28 
     LABEL "BUTTON28" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-29 
     LABEL "BUTTON29" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-3 
     LABEL "BUTTON3" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-30 
     LABEL "BUTTON30" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-4 
     LABEL "BUTTON4" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-5 
     LABEL "BUTTON5" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-6 
     LABEL "BUTTON6" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-7 
     LABEL "BUTTON7" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-8 
     LABEL "BUTTON8" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-9 
     LABEL "BUTTON9" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Cancel 
     LABEL "CANCEL" 
     SIZE 39 BY 1.67 TOOLTIP "CANCEL".

DEFINE BUTTON Btn_Close 
     IMAGE-UP FILE "images\exit-au":U
     LABEL "CLOSE" 
     SIZE 10 BY 2.38.

DEFINE BUTTON Btn_Enter_Job 
     LABEL "ENTER JOB" 
     SIZE 20 BY 2.14 TOOLTIP "ENTER JOB".

DEFINE BUTTON Btn_First 
     LABEL "FIRST JOB" 
     SIZE 19 BY 1.91 TOOLTIP "FIRST JOB".

DEFINE BUTTON Btn_Job_List 
     LABEL "JOB LIST" 
     SIZE 19 BY 2.14 TOOLTIP "JOB LIST".

DEFINE BUTTON Btn_Last 
     LABEL "LAST JOB" 
     SIZE 20 BY 1.91 TOOLTIP "LAST JOB".

DEFINE BUTTON Btn_Page_Down 
     IMAGE-UP FILE "images\pagedown":U
     LABEL "Page Down" 
     SIZE 20 BY 2.38 TOOLTIP "PAGE DOWN".

DEFINE BUTTON Btn_Page_Up 
     IMAGE-UP FILE "images\pageup":U
     LABEL "Page Up" 
     SIZE 19 BY 2.38 TOOLTIP "PAGE UP".

DEFINE BUTTON Btn_schedule 
     LABEL "SCHEDULE" 
     SIZE 29 BY 2.38 TOOLTIP "SCHEDULE".

DEFINE BUTTON Btn_sort 
     LABEL "SORT / JOB" 
     SIZE 39 BY 1.67 TOOLTIP "SORT / Job".

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
     Btn_schedule AT ROW 1.71 COL 86
     Btn_Close AT ROW 1.71 COL 115
     Btn_Button-2 AT ROW 4.1 COL 2
     Btn_Button-7 AT ROW 4.1 COL 16
     Btn_Button-12 AT ROW 4.1 COL 30
     Btn_Button-17 AT ROW 4.1 COL 44
     Btn_Button-22 AT ROW 4.1 COL 58
     Btn_Button-27 AT ROW 4.1 COL 72
     Btn_Job_List AT ROW 4.1 COL 86
     Btn_Enter_Job AT ROW 4.1 COL 105
     Btn_Page_Up AT ROW 6.24 COL 86
     Btn_Page_Down AT ROW 6.24 COL 105
     Btn_Button-3 AT ROW 6.48 COL 2
     Btn_Button-8 AT ROW 6.48 COL 16
     Btn_Button-13 AT ROW 6.48 COL 30
     Btn_Button-18 AT ROW 6.48 COL 44
     Btn_Button-23 AT ROW 6.48 COL 58
     Btn_Button-28 AT ROW 6.48 COL 72
     Btn_First AT ROW 8.62 COL 86
     Btn_Last AT ROW 8.62 COL 105
     Btn_Button-4 AT ROW 8.86 COL 2
     Btn_Button-9 AT ROW 8.86 COL 16
     Btn_Button-14 AT ROW 8.86 COL 30
     Btn_Button-19 AT ROW 8.86 COL 44
     Btn_Button-24 AT ROW 8.86 COL 58
     Btn_Button-29 AT ROW 8.86 COL 72
     Btn_sort AT ROW 10.52 COL 86
     Btn_Button-5 AT ROW 11.24 COL 2
     Btn_Button-10 AT ROW 11.24 COL 16
     Btn_Button-15 AT ROW 11.24 COL 30
     Btn_Button-20 AT ROW 11.24 COL 44
     Btn_Button-25 AT ROW 11.24 COL 58
     Btn_Button-30 AT ROW 11.24 COL 72
     Btn_Cancel AT ROW 12.19 COL 86
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 7 FGCOLOR 15 FONT 6.


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
ASSIGN 
       Btn_Cancel:PRIVATE-DATA IN FRAME F-Main     = 
                "CANCEL".

ASSIGN 
       Btn_Enter_Job:PRIVATE-DATA IN FRAME F-Main     = 
                "ENTER JOB".

ASSIGN 
       Btn_First:PRIVATE-DATA IN FRAME F-Main     = 
                "FIRST JOB".

ASSIGN 
       Btn_Job_List:PRIVATE-DATA IN FRAME F-Main     = 
                "JOB LIST".

ASSIGN 
       Btn_Last:PRIVATE-DATA IN FRAME F-Main     = 
                "LAST JOB".

ASSIGN 
       Btn_Page_Down:PRIVATE-DATA IN FRAME F-Main     = 
                "images\pagedown.bmp".

ASSIGN 
       Btn_Page_Up:PRIVATE-DATA IN FRAME F-Main     = 
                "images\pageup.bmp".

ASSIGN 
       Btn_schedule:PRIVATE-DATA IN FRAME F-Main     = 
                "SCHEDULE".

ASSIGN 
       Btn_sort:PRIVATE-DATA IN FRAME F-Main     = 
                "SORT / JOB".


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


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel s-object
ON CHOOSE OF Btn_Cancel IN FRAME F-Main /* CANCEL */
DO:
  {methods/run_link.i "CONTAINER" "Change_Page" "(5)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close s-object
ON CHOOSE OF Btn_Close IN FRAME F-Main /* CLOSE */
DO:
  {methods/run_link.i "CONTAINER" "Change_Page" "(2)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Enter_Job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Enter_Job s-object
ON CHOOSE OF Btn_Enter_Job IN FRAME F-Main /* ENTER JOB */
DO:
  {methods/run_link.i "CONTAINER" "Change_Page" "(16)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_First
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_First s-object
ON CHOOSE OF Btn_First IN FRAME F-Main /* FIRST JOB */
DO:
  button_item = 1.
  RUN Button_Labels (INPUT-OUTPUT button_item).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Job_List
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Job_List s-object
ON CHOOSE OF Btn_Job_List IN FRAME F-Main /* JOB LIST */
DO:
  {methods/run_link.i "CONTAINER" "Change_Page" "(15)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Last
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Last s-object
ON CHOOSE OF Btn_Last IN FRAME F-Main /* LAST JOB */
DO:
  button_item = NUM-ENTRIES(itemlist,'@') + 1.
  RUN Button_Labels (INPUT-OUTPUT button_item).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Page_Down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Page_Down s-object
ON CHOOSE OF Btn_Page_Down IN FRAME F-Main /* Page Down */
DO:
  RUN Button_Labels (INPUT-OUTPUT button_item).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Page_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Page_Up s-object
ON CHOOSE OF Btn_Page_Up IN FRAME F-Main /* Page Up */
DO:
  button_item = button_item - 60.
  RUN Button_Labels (INPUT-OUTPUT button_item).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_schedule
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_schedule s-object
ON CHOOSE OF Btn_schedule IN FRAME F-Main /* SCHEDULE */
DO:
  RUN schedule-proc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_sort s-object
ON CHOOSE OF Btn_sort IN FRAME F-Main /* SORT / JOB */
DO:
  IF SELF:TOOLTIP = "SORT / JOB" THEN DO:
     SELF:TOOLTIP = "SORT / START".
     RUN Get_jobs ("Start").       
  END.
  ELSE DO:
     SELF:TOOLTIP = "SORT / JOB".
     RUN Get_jobs ("Job").       
  END.
  ASSIGN
    SELF:LABEL = SELF:TOOLTIP
    SELF:PRIVATE-DATA = SELF:TOOLTIP.
  {touch/localview.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */
 
IF v-autopo-sec THEN do:
    ASSIGN
        Btn_Enter_Job:SENSITIVE IN FRAME F-Main         = YES .
END.
ELSE
    ASSIGN 
        Btn_Enter_Job:SENSITIVE IN FRAME F-Main         = NO .


/* If testing in the UIB, initialize the SmartObject. */ 
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF
  
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

  for each tt-job:
      delete tt-job.
  end.

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
           /* schedule machine task# 08030510*/
           /*DEF VAR lv-sch-mach AS cha NO-UNDO.
           DEF VAR lv-mach-list AS cha NO-UNDO.
           FIND FIRST mach NO-LOCK WHERE mach.company = company_code
                                     AND mach.m-code = machine_code NO-ERROR.
           FIND FIRST reftable {&where-sch-m-code} NO-LOCK NO-ERROR.
           lv-sch-mach = IF AVAIL reftable THEN reftable.code2 ELSE "".
           IF lv-sch-mach <> "" THEN DO:
              lv-mach-list = "".
              FOR EACH reftable NO-LOCK WHERE reftable.reftable = "mach.sch-m-code"
                                       AND reftable.company  = mach.company
                                       AND reftable.loc      = mach.loc
                                       AND reftable.code2     = lv-sch-mach:
                  lv-mach-list = lv-mach-list + reftable.CODE + ",".
              END.
           END.
           */
           /* end of task 08030510*/
           FOR each job-mch of job  NO-LOCK where (job-mch.m-code eq machine_code OR 
                                                   LOOKUP(job-mch.m-code,machine_list) > 0)
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
                      FIND FIRST tt-job WHERE tt-job.job-no = job.job-no
                                          AND tt-job.job-no2 = job.job-no2 NO-ERROR.
                      IF NOT AVAIL tt-job THEN DO: 
                         create tt-job.
                         assign tt-job.job-no = job.job-no
                                tt-job.job-no2 = job.job-no2.
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

     for each job
         where job.company eq company_code /*'001'*/
           and job.stat    gt lv-stat[1]
           and job.stat    lt lv-stat[2]       no-lock
           BREAK BY job.start-date BY job.job-no BY job.job-no2:
         
        IF FIRST-OF(job.job-no2) THEN DO:
           FOR each job-mch of job  where job-mch.m-code eq machine_code NO-LOCK
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
                      FIND FIRST tt-job WHERE tt-job.job-no = job.job-no
                                          AND tt-job.job-no2 = job.job-no2 NO-ERROR.
                      IF NOT AVAIL tt-job THEN DO: 
                         create tt-job.
                         assign tt-job.job-no = job.job-no
                                tt-job.job-no2 = job.job-no2.
                      END.
                 END.
              END.
           END. /*job-mch*/
        END. /* first-of(job-no2) */
     end.  /* for each job */
  end.

  for each tt-job:
      itemlist = IF itemlist = '' THEN tt-job.job-no + '-' + STRING(tt-job.job-no2)
                 ELSE itemlist + '@' + tt-job.job-no + '-' + STRING(tt-job.job-no2).
  end.

  RUN Button_Labels (INPUT-OUTPUT button_item).
  
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

