&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

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

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

DEFINE VARIABLE rec_key_value AS CHARACTER NO-UNDO.
DEFINE VARIABLE header_value AS CHARACTER NO-UNDO.

DEFINE VARIABLE misc_rec_key_value AS CHARACTER NO-UNDO.
DEFINE VARIABLE misc_header_value AS CHARACTER NO-UNDO.

DEFINE VARIABLE h-XTab AS HANDLE NO-UNDO.
DEFINE VARIABLE h-Bf-tab AS HANDLE NO-UNDO.
DEFINE VARIABLE h-Qry AS HANDLE     NO-UNDO.
DEF VAR job-rowid AS ROWID NO-UNDO.

DEF TEMP-TABLE tt_date
    FIELD s-seq AS cha
    FIELD s-DATE AS DATE
    FIELD s-time AS INT
    INDEX s-date IS PRIMARY s-date.

DEF TEMP-TABLE tt-tab
    FIELD m-code LIKE mach.m-code
    FIELD s-date AS DATE FORM "99/99/9999"
    FIELD s-time AS INT FORM ">>>>>>>>>9".

DEF TEMP-TABLE tt-sch
    FIELD s-seq AS INT
    FIELD m-code LIKE mach.m-code
    FIELD s-date AS DATE EXTENT 7
    FIELD s-time AS INT EXTENT 24
    FIELD tot-time AS INT EXTENT 24
    FIELD job-list AS cha FORM "(10)" EXTENT 168 /* 7X24 */
    FIELD job-no AS cha EXTENT 168
    FIELD in-out AS LOG EXTENT 168
    .

DEF VAR v-record-position AS ROWID NO-UNDO.  /* record for default highlighted line */

DEF STREAM st-out.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Record-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-2

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES job
&Scoped-define FIRST-EXTERNAL-TABLE job


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR job.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-sch

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-sch.m-code disp-job-list(1) @ tt-sch.job-list[1] /*COLUMN-FONT 16*/ disp-job-list(2) @ tt-sch.job-list[2] /*COLUMN-FONT 16*/ disp-job-list(3) @ tt-sch.job-list[3] /*COLUMN-FONT 16*/ disp-job-list(4) @ tt-sch.job-list[4] /*COLUMN-FONT 16*/ disp-job-list(5) @ tt-sch.job-list[5] /*COLUMN-FONT 16*/ disp-job-list(6) @ tt-sch.job-list[6] /*COLUMN-FONT 16*/ disp-job-list(7) @ tt-sch.job-list[7] /*COLUMN-FONT 16*/ /*disp-job-list(8) @ tt-sch.job-list[8] */ tt-sch.job-list[1] tt-sch.job-list[2] tt-sch.job-list[3] tt-sch.job-list[4] tt-sch.job-list[5] tt-sch.job-list[6] tt-sch.job-list[7] /*== /* need to specify every element to   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 it or can use [1 for 10] for display only */ ~
 /* do 2 times to display bars instead of job#*/ FILL( CHR(150),LENGTH(tt-sch.job-list[1]) ) @ tt-sch.job-list[1] FORM "x(10)"  FILL(CHR(150),LENGTH(tt-sch.job-list[2])) @ tt-sch.job-list[2]  FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[3])) @ tt-sch.job-list[3]  FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[4])) @ tt-sch.job-list[4]  FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[5])) @ tt-sch.job-list[5]  FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[6])) @ tt-sch.job-list[6] FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[7])) @ tt-sch.job-list[7] FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[8])) @ tt-sch.job-list[8] FORM "x(10)"  tt-sch.job-list[1] FORMAT "x(10)" ~
 tt-sch.job-list[2] FORMAT "x(10)"   tt-sch.job-list[3] FORMAT "x(10)"   tt-sch.job-list[4] FORMAT "x(10)"   tt-sch.job-list[5] FORMAT "x(10)"   tt-sch.job-list[6] FORMAT "x(10)"   tt-sch.job-list[7] FORMAT "x(10)"   tt-sch.job-list[8] FORMAT "x(10)"  FILL( CHR(150),LENGTH(tt-sch.job-list[1]) ) @ tt-sch.job-list[1] FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[2])) @ tt-sch.job-list[2]  FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[3])) @ tt-sch.job-list[3]  FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[4])) @ tt-sch.job-list[4]  FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[5])) @ tt-sch.job-list[5]  FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[6])) @ tt-sch.job-list[6] FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[7])) @ tt-sch.job-list[7] FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[8])) @ tt-sch.job-list[8] FORM "x(10)"  /*ENABLE tt-sch.job-list[1] tt-sch.job-list[2] */ ===*/  disp-job-list(1) @ tt-sch.job-list[1] FORM "x(61)"  disp-job-list(2) @ tt-sch.job-list[2] FORM "x(61)"  disp-job-list(3) @ tt-sch.job-list[3] FORM "x(61)"  disp-job-list(4) @ tt-sch.job-list[4] FORM "x(61)"  disp-job-list(5) @ tt-sch.job-list[5] FORM "x(61)"  disp-job-list(6) @ tt-sch.job-list[6] FORM "x(61)"  disp-job-list(7) @ tt-sch.job-list[7] FORM "x(61)"  /*disp-job-list(8) @ tt-sch.job-list[8] FORM "x(61)"*/ ENABLE tt-sch.job-list[1] tt-sch.job-list[2] tt-sch.job-list[3] ~
tt-sch.job-list[4] tt-sch.job-list[5] tt-sch.job-list[6] tt-sch.job-list[7]   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 CHR(150) LENGTH(tt-sch ~
tt-sch FILL(CHR(150) LENGTH(tt-sch FILL(CHR(150) LENGTH(tt-sch ~
FILL(CHR(150) LENGTH(tt-sch FILL(CHR(150) LENGTH(tt-sch FILL(CHR(150) ~
LENGTH(tt-sch FILL(CHR(150) LENGTH(tt-sch FILL(CHR(150) LENGTH(tt-sch ~
CHR(150) LENGTH(tt-sch FILL(CHR(150) LENGTH(tt-sch FILL(CHR(150) ~
LENGTH(tt-sch FILL(CHR(150) LENGTH(tt-sch FILL(CHR(150) LENGTH(tt-sch ~
FILL(CHR(150) LENGTH(tt-sch FILL(CHR(150) LENGTH(tt-sch FILL(CHR(150) ~
LENGTH(tt-sch
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 CHR(150)
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-BROWSE-2 LENGTH(tt-sch
&Scoped-define THIRD-ENABLED-TABLE-IN-QUERY-BROWSE-2 tt-sch
&Scoped-define FOURTH-ENABLED-TABLE-IN-QUERY-BROWSE-2 FILL(CHR(150)
&Scoped-define FIFTH-ENABLED-TABLE-IN-QUERY-BROWSE-2 LENGTH(tt-sch
&Scoped-define SIXTH-ENABLED-TABLE-IN-QUERY-BROWSE-2 FILL(CHR(150)
&Scoped-define SEVENTH-ENABLED-TABLE-IN-QUERY-BROWSE-2 LENGTH(tt-sch
&Scoped-define EIGHTH-ENABLED-TABLE-IN-QUERY-BROWSE-2 FILL(CHR(150)
&Scoped-define NINTH-ENABLED-TABLE-IN-QUERY-BROWSE-2 LENGTH(tt-sch
&Scoped-define TENTH-ENABLED-TABLE-IN-QUERY-BROWSE-2 FILL(CHR(150)
&Scoped-define ELEVENTH-ENABLED-TABLE-IN-QUERY-BROWSE-2 LENGTH(tt-sch
&Scoped-define TWELFTH-ENABLED-TABLE-IN-QUERY-BROWSE-2 FILL(CHR(150)
&Scoped-define THIRTEENTH-ENABLED-TABLE-IN-QUERY-BROWSE-2 LENGTH(tt-sch
&Scoped-define FOURTEENTH-ENABLED-TABLE-IN-QUERY-BROWSE-2 FILL(CHR(150)
&Scoped-define FIFTEENTH-ENABLED-TABLE-IN-QUERY-BROWSE-2 LENGTH(tt-sch
&Scoped-define SIXTEENTH-ENABLED-TABLE-IN-QUERY-BROWSE-2 FILL(CHR(150)
&Scoped-define SEVENTEENTH-ENABLED-TABLE-IN-QUERY-BROWSE-2 LENGTH(tt-sch
&Scoped-define EIGHTEENTH-ENABLED-TABLE-IN-QUERY-BROWSE-2 CHR(150)
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-sch
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-sch.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-sch
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-sch


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD disp-job-list W-Win 
FUNCTION disp-job-list RETURNS CHARACTER
  ( INPUT  ip-element AS int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-jobhdr AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-job AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE v-help AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 128 BY .95
     BGCOLOR 11  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-sch SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 W-Win _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-sch.m-code FORM "x(15)"
          disp-job-list(1) @ tt-sch.job-list[1] FORM "x(61)" COLUMN-LABEL "Label!1" /*COLUMN-FONT 16*/ WIDTH 61 
  disp-job-list(2) @ tt-sch.job-list[2] FORM "x(61)"      COLUMN-LABEL "Label!2"  /*COLUMN-FONT 16*/  WIDTH 61 
  disp-job-list(3) @ tt-sch.job-list[3] FORM "x(61)"      COLUMN-LABEL "Label!3" /*COLUMN-FONT 16*/  WIDTH 61 
  disp-job-list(4) @ tt-sch.job-list[4] FORM "x(61)"      COLUMN-LABEL "Label!4" /*COLUMN-FONT 16*/  WIDTH 61 
  disp-job-list(5) @ tt-sch.job-list[5] FORM "x(61)"      COLUMN-LABEL "Label!5" /*COLUMN-FONT 16*/ WIDTH 61 
  disp-job-list(6) @ tt-sch.job-list[6] FORM "x(61)"      COLUMN-LABEL "Label!6" /*COLUMN-FONT 16*/ WIDTH 61 
  disp-job-list(7) @ tt-sch.job-list[7] FORM "x(61)"      COLUMN-LABEL "Label!7" /*COLUMN-FONT 16*/ WIDTH 61 
  /*disp-job-list(8) @ tt-sch.job-list[8] FORM "x(61)"      COLUMN-LABEL "Label!8" COLUMN-FONT 16 WIDTH 61 */
          tt-sch.job-list[1] tt-sch.job-list[2] tt-sch.job-list[3]
      tt-sch.job-list[4] tt-sch.job-list[5] tt-sch.job-list[6] tt-sch.job-list[7]
/*==
 /* need to specify every element to enable it  or can use [1 for 10] for display only */    
    /* do 2 times to display bars instead of job#*/
 FILL( CHR(150),LENGTH(tt-sch.job-list[1]) ) @ tt-sch.job-list[1] FORM "x(10)" 
 FILL(CHR(150),LENGTH(tt-sch.job-list[2])) @ tt-sch.job-list[2]   FORM "x(10)"
 FILL(CHR(150),LENGTH(tt-sch.job-list[3])) @ tt-sch.job-list[3]   FORM "x(10)"
 FILL(CHR(150),LENGTH(tt-sch.job-list[4])) @ tt-sch.job-list[4]   FORM "x(10)"
 FILL(CHR(150),LENGTH(tt-sch.job-list[5])) @ tt-sch.job-list[5]   FORM "x(10)"
 FILL(CHR(150),LENGTH(tt-sch.job-list[6])) @ tt-sch.job-list[6]  FORM "x(10)"
 FILL(CHR(150),LENGTH(tt-sch.job-list[7])) @ tt-sch.job-list[7]  FORM "x(10)"
 FILL(CHR(150),LENGTH(tt-sch.job-list[8])) @ tt-sch.job-list[8]  FORM "x(10)"

 tt-sch.job-list[1] FORMAT "x(10)"   
     tt-sch.job-list[2] FORMAT "x(10)"
     tt-sch.job-list[3] FORMAT "x(10)"
     tt-sch.job-list[4] FORMAT "x(10)"
     tt-sch.job-list[5] FORMAT "x(10)"
     tt-sch.job-list[6] FORMAT "x(10)"
     tt-sch.job-list[7] FORMAT "x(10)"
     tt-sch.job-list[8] FORMAT "x(10)"
 
 FILL( CHR(150),LENGTH(tt-sch.job-list[1]) ) @ tt-sch.job-list[1] FORM "x(10)"
 FILL(CHR(150),LENGTH(tt-sch.job-list[2])) @ tt-sch.job-list[2]   FORM "x(10)"
 FILL(CHR(150),LENGTH(tt-sch.job-list[3])) @ tt-sch.job-list[3]   FORM "x(10)"
 FILL(CHR(150),LENGTH(tt-sch.job-list[4])) @ tt-sch.job-list[4]   FORM "x(10)"
 FILL(CHR(150),LENGTH(tt-sch.job-list[5])) @ tt-sch.job-list[5]   FORM "x(10)"
 FILL(CHR(150),LENGTH(tt-sch.job-list[6])) @ tt-sch.job-list[6]  FORM "x(10)"
 FILL(CHR(150),LENGTH(tt-sch.job-list[7])) @ tt-sch.job-list[7]  FORM "x(10)"
 FILL(CHR(150),LENGTH(tt-sch.job-list[8])) @ tt-sch.job-list[8]  FORM "x(10)"
 
/*ENABLE tt-sch.job-list[1] tt-sch.job-list[2] */

===*/
  disp-job-list(1) @ tt-sch.job-list[1] FORM "x(61)"
  disp-job-list(2) @ tt-sch.job-list[2] FORM "x(61)"
  disp-job-list(3) @ tt-sch.job-list[3] FORM "x(61)"
  disp-job-list(4) @ tt-sch.job-list[4] FORM "x(61)"
  disp-job-list(5) @ tt-sch.job-list[5] FORM "x(61)"
  disp-job-list(6) @ tt-sch.job-list[6] FORM "x(61)"
  disp-job-list(7) @ tt-sch.job-list[7] FORM "x(61)"
  /*disp-job-list(8) @ tt-sch.job-list[8] FORM "x(61)"*/

ENABLE tt-sch.job-list[1] tt-sch.job-list[2] tt-sch.job-list[3]
      tt-sch.job-list[4] tt-sch.job-list[5] tt-sch.job-list[6] tt-sch.job-list[7]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 142 BY 11.43 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BROWSE-2 AT ROW 1.24 COL 2
     v-help AT ROW 11.71 COL 2 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 155.6 BY 26.05.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: asi.job
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Machine Schedule"
         HEIGHT             = 26.05
         WIDTH              = 155.6
         MAX-HEIGHT         = 26.05
         MAX-WIDTH          = 155.6
         VIRTUAL-HEIGHT     = 26.05
         VIRTUAL-WIDTH      = 155.6
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 1 F-Main */
/* SETTINGS FOR FILL-IN v-help IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       v-help:HIDDEN IN FRAME F-Main           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-sch
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Machine Schedule */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Machine Schedule */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 W-Win
ON ROW-DISPLAY OF BROWSE-2 IN FRAME F-Main
DO:
  
  /*  ASSIGN tt-sch.m-code:BGCOLOR IN BROWSE {&browse-name} = ?
              tt-sch.job-list[1]:BGCOLOR IN BROWSE {&browse-name} = ?
              tt-sch.job-list[2]:BGCOLOR IN BROWSE {&browse-name} = ?
              tt-sch.job-list[3]:BGCOLOR IN BROWSE {&browse-name} = ?
              tt-sch.job-list[4]:BGCOLOR IN BROWSE {&browse-name} = ?
              tt-sch.job-list[5]:BGCOLOR IN BROWSE {&browse-name} = ?
              tt-sch.job-list[6]:BGCOLOR IN BROWSE {&browse-name} = ?
              tt-sch.job-list[7]:BGCOLOR IN BROWSE {&browse-name} = ?
           /* tt-sch.job-list[8]:BGCOLOR IN BROWSE {&browse-name} = ? */
              .

       ASSIGN tt-sch.job-list[1]:FGCOLOR IN BROWSE {&browse-name} = 2
              tt-sch.job-list[2]:FGCOLOR IN BROWSE {&browse-name} = 3
              tt-sch.job-list[3]:FGCOLOR IN BROWSE {&browse-name} = 4
              tt-sch.job-list[4]:FGCOLOR IN BROWSE {&browse-name} = 5
              tt-sch.job-list[5]:FGCOLOR IN BROWSE {&browse-name} = 6
              tt-sch.job-list[6]:FGCOLOR IN BROWSE {&browse-name} = 7
              tt-sch.job-list[7]:FGCOLOR IN BROWSE {&browse-name} = 8
              /*-sch.job-list[8]:FGCOLOR IN BROWSE {&browse-name} = 9*/
              .
   */           
    ASSIGN tt-sch.job-list[1]:BGCOLOR IN BROWSE {&browse-name} = 2
              tt-sch.job-list[2]:BGCOLOR IN BROWSE {&browse-name} = 3
              tt-sch.job-list[3]:BGCOLOR IN BROWSE {&browse-name} = 4
              tt-sch.job-list[4]:BGCOLOR IN BROWSE {&browse-name} = 5
              tt-sch.job-list[5]:BGCOLOR IN BROWSE {&browse-name} = 6
              tt-sch.job-list[6]:BGCOLOR IN BROWSE {&browse-name} = 7
              tt-sch.job-list[7]:BGCOLOR IN BROWSE {&browse-name} = 8
              /*-sch.job-list[8]:FGCOLOR IN BROWSE {&browse-name} = 9*/
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

ON 'mouse-select-dblclick':U OF tt-sch.job-list[1] IN BROWSE {&browse-name}
    OR 'mouse-select-dblclick':U OF tt-sch.job-list[2] IN BROWSE {&browse-name}
    OR 'mouse-select-dblclick':U OF tt-sch.job-list[3] IN BROWSE {&browse-name}
    OR 'mouse-select-dblclick':U OF tt-sch.job-list[4] IN BROWSE {&browse-name}
    OR 'mouse-select-dblclick':U OF tt-sch.job-list[5] IN BROWSE {&browse-name}
    OR 'mouse-select-dblclick':U OF tt-sch.job-list[6] IN BROWSE {&browse-name}
    OR 'mouse-select-dblclick':U OF tt-sch.job-list[7] IN BROWSE {&browse-name}
DO:
     RUN jc/w-jobcst.w.
END.


ON 'mouse-select-click':U OF tt-sch.job-list[1] IN BROWSE {&browse-name}
    OR 'mouse-select-click':U OF tt-sch.job-list[2] IN BROWSE {&browse-name}
    OR 'mouse-select-click':U OF tt-sch.job-list[3] IN BROWSE {&browse-name}
    OR 'mouse-select-click':U OF tt-sch.job-list[4] IN BROWSE {&browse-name}
    OR 'mouse-select-click':U OF tt-sch.job-list[5] IN BROWSE {&browse-name}
    OR 'mouse-select-click':U OF tt-sch.job-list[6] IN BROWSE {&browse-name}
    OR 'mouse-select-click':U OF tt-sch.job-list[7] IN BROWSE {&browse-name}
DO:

/*  IF tt-sch.s-seq = 1 THEN RETURN NO-APPLY.*/

   /* DEFINE FRAME f-tab.
  
    CREATE WIDGET-POOL "w-tab" PERSISTENT NO-ERROR. 
    ASSIGN FRAME f-tab:X = tt-sch.job-list[1]:X IN BROWSE {&browse-name}
           FRAME f-tab:Y = tt-sch.job-list[1]:y IN BROWSE {&browse-name}  
           FRAME f-tab:VISIBLE = YES
           .
           
   DISPLAY tt-sch.job-list[1] WITH FRAME f-tab OVERLAY TOP-ONLY NO-LABELS WIDTH 35.
   */
    
    
    v-help = tt-sch.job-list[FOCUS:INDEX].
    ASSIGN  v-help:X IN FRAME {&FRAME-NAME} = /*tt-sch.job-list[1]:X IN BROWSE {&browse-name} */
                                              FRAME {&FRAME-NAME}:X + 30
           v-help:Y = tt-sch.job-list[1]:Y IN BROWSE {&browse-name} + 15 
           v-help:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-sch.job-list[FOCUS:INDEX]
           v-help:VISIBLE = YES.


    IF NUM-ENTRIES(tt-sch.job-no[FOCUS:INDEX]) = 1 THEN DO:
        DEF VAR char-hdl AS cha NO-UNDO.
       FIND FIRST job WHERE job.company = g_company
                         AND job.job-no = SUBSTRING(tt-sch.job-no[FOCUS:INDEX],1,INDEX(tt-sch.job-no[FOCUS:INDEX],"-") - 1)
                         AND job.job-no2 = INT(SUBSTRING(tt-sch.job-no[FOCUS:INDEX],INDEX(tt-sch.job-no[FOCUS:INDEX],"-") + 1,2))
                         NO-LOCK NO-ERROR.

     /*   MESSAGE AVAIL job "comp:" g_company
             SUBSTRING(tt-sch.job-no[FOCUS:INDEX],1,INDEX(tt-sch.job-no[FOCUS:INDEX],"-") - 1)
             length(SUBSTRING(tt-sch.job-no[FOCUS:INDEX],1,INDEX(tt-sch.job-no[FOCUS:INDEX],"-") - 1))
            SUBSTRING(tt-sch.job-no[FOCUS:INDEX],INDEX(tt-sch.job-no[FOCUS:INDEX],"-") + 1,2)
            length(SUBSTRING(tt-sch.job-no[FOCUS:INDEX],INDEX(tt-sch.job-no[FOCUS:INDEX],"-") + 1,2))
            VIEW-AS ALERT-BOX.
     */
        IF AVAIL job THEN  do:
            RUN set-attribute-list ("JOBID = " + STRING(ROWID(job)) ).
            RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-target",OUTPUT char-hdl).
            RUN dispatch IN WIDGET-HANDLE(char-hdl) ('open-query').
        END.
    END.

    RETURN.
END.

ON 'leave':U OF tt-sch.job-list[1] IN BROWSE {&browse-name}
    Or 'leave':U OF tt-sch.job-list[2] IN BROWSE {&browse-name}
    OR 'leave':U OF tt-sch.job-list[3] IN BROWSE {&browse-name}
    OR 'leave':U OF tt-sch.job-list[4] IN BROWSE {&browse-name}
    OR 'leave':U OF tt-sch.job-list[5] IN BROWSE {&browse-name}
    OR 'leave':U OF tt-sch.job-list[6] IN BROWSE {&browse-name}
    OR 'leave':U OF tt-sch.job-list[7] IN BROWSE {&browse-name}
DO:
    v-help:VISIBLE IN FRAME {&FRAME-NAME} = NO.
    RETURN.
END.

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

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
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Job Detail|Estimate' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 12.91 , 2.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 14.05 , 148.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             v-help:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job ).
       RUN set-position IN h_v-job ( 14.57 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/b-jobhdr.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jobhdr ).
       RUN set-position IN h_b-jobhdr ( 17.76 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-jobhdr ( 9.29 , 145.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcsch/q-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-job ).
       RUN set-position IN h_q-job ( 12.67 , 62.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Links to SmartViewer h_v-job. */
       RUN add-link IN adm-broker-hdl ( h_q-job , 'Record':U , h_v-job ).

       /* Links to SmartNavBrowser h_b-jobhdr. */
       RUN add-link IN adm-broker-hdl ( h_v-job , 'Record':U , h_b-jobhdr ).

       /* Links to SmartQuery h_q-job. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Record':U , h_q-job ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jobhdr ,
             h_v-job , 'AFTER':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table W-Win 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-date AS DATE NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR j AS INT NO-UNDO.
  DEF VAR v-time AS INT NO-UNDO.
  DEF VAR v-cnt AS INT NO-UNDO.
  DEFINE VARIABLE h-Field AS HANDLE     NO-UNDO.
  DEF VAR v-job AS cha NO-UNDO.
  DEF VAR lh-tab AS HANDLE NO-UNDO.
  DEF VAR lv-num-of-col AS INT NO-UNDO.
  DEF VAR lv-tsch-rowid AS ROWID NO-UNDO.
  DEF VAR lv-sch-io AS LOG NO-UNDO. /* mach-calendar start-time check */
  DEF VAR v-tot-time AS INT NO-UNDO.

  lv-num-of-col = 50.

  FOR EACH tt_date:
      DELETE tt_date.
  END.
  FOR EACH tt-tab:
      DELETE tt-tab.
  END.
  FOR EACH tt-sch:
      DELETE tt-sch.
  END.
  /*
  DEFINE VARIABLE h-XTab AS HANDLE NO-UNDO.
  DEFINE VARIABLE h-Bf-tab AS HANDLE NO-UNDO.
  DEFINE VARIABLE h-Qry AS HANDLE     NO-UNDO.
  */


  v-date = 01/01/2004.
  v-cnt = 0.
  /*
  DO i = 1 TO 10.
     DO j = 1 TO 240:
        v-cnt = v-cnt + 1.
        CREATE tt_date.
        ASSIGN tt_date.s-seq = STRING(v-cnt)
               tt_date.s-date = v-date
               tt_date.s-time = j.
     END.
     v-date = v-date + 1.
  END.
  */
  i = 0.

  FOR EACH job-mch NO-LOCK  ,
      FIRST mach-calendar WHERE mach-calendar.company = job-mch.company
                                 AND mach-calendar.m-code = job-mch.m-code 
                                 AND mach-calendar.m-date = job-mch.start-date NO-LOCK
       BREAK BY job-mch.start-date DESC  BY job-mch.m-code BY start-time:
  
      IF FIRST-OF(job-mch.start-date) THEN DO:
         v-cnt = v-cnt + 1.
         CREATE tt_date.
         ASSIGN tt_date.s-seq = STRING(v-cnt)
                tt_date.s-date = job-mch.start-date
                tt_date.s-time = job-mch.start-time.
         
      END.
      IF v-cnt > 7 THEN LEAVE. /* one week */
  END.
  v-cnt = 0.

  FOR EACH tt_date,
      EACH job-mch WHERE job-mch.start-date = tt_date.s-date NO-LOCK,
      FIRST mach-calendar WHERE mach-calendar.company = job-mch.company
                                 AND mach-calendar.m-code = job-mch.m-code 
                                 AND mach-calendar.m-date = job-mch.start-date NO-LOCK
      BREAK BY job-mch.m-code BY job-mch.start-date BY job-mch.start-time:
  
      IF FIRST-OF(job-mch.m-code) THEN DO:
         CREATE tt-tab.
         ASSIGN tt-tab.m-code = job-mch.m-code
                tt-tab.s-date = job-mch.start-date
                tt-tab.s-time = v-time.         
         CREATE tt-sch.
         ASSIGN tt-sch.m-code = job-mch.m-code.
         lv-tsch-rowid = ROWID(tt-sch).
         
      END.
      IF FIRST-OF(job-mch.start-date) THEN do:
         v-cnt = v-cnt + 1.
         /*FIND FIRST mach-calendar WHERE mach-calendar.company = job-mch.company
                                    AND mach-calendar.m-code = job-mch.m-code 
                                    AND mach-calendar.m-date = job-mch.start-date NO-LOCK NO-ERROR.
        */
      END.
      IF NOT AVAIL tt-sch THEN FIND tt-sch WHERE ROWID(tt-sch) = lv-tsch-rowid.
      
      IF v-cnt >= 1 AND v-cnt <= 7 THEN
         tt-sch.s-date[v-cnt] = job-mch.start-date.

      lv-sch-io = NO.
      v-tot-time = 0.
      IF /*AVAIL mach-calendar and*/
         job-mch.start-time >= mach-calendar.start-time THEN DO:
         j = int(SUBSTRING(STRING(job-mch.start-time,"HH:MM"),1,2)).
         j = j + IF int(SUBSTRING(STRING(job-mch.start-time,"HH:MM"),4,2)) > 0 THEN 1 ELSE 0 .
         v-tot-time = job-mch.end-time - job-mch.start-time.
         IF v-tot-time < 0 THEN v-tot-time = 0.
         lv-sch-io = YES.
      END.
      
      IF j = 0 THEN j = 1.


      IF j >= 1 AND j <= 24 THEN /* time - 24 hrs and 10 cells per hour */
          ASSIGN tt-sch.s-time[v-cnt] = j
                 tt-sch.tot-time[v-cnt] = v-tot-time   
                 tt-sch.job-list[v-cnt] = tt-sch.job-list[v-cnt] +
                                      trim(STRING(job-mch.job-no)) + "-" +
                                      STRING(job-mch.job-no2,">9") + " (" +
                                      STRING(job-mch.start-time,"hh:mm") + "-" +
                                      STRING(job-mch.end-time,"hh:mm") + ") "
                 tt-sch.job-no[v-cnt] =   ( IF tt-sch.job-no[v-cnt] <> "" THEN 
                                              tt-sch.job-no[v-cnt] + "," 
                                             ELSE tt-sch.job-no[v-cnt] ) +
                                          STRING(job-mch.job-no) + "-" +
                                           STRING(job-mch.job-no2,">9") 
                 tt-sch.in-out[v-cnt] = lv-sch-io. 

     

    IF i > 300 THEN LEAVE.

  END.
  
/*
  v-time = 0.
  FOR EACH tt_date ,
      EACH mach BREAK BY tt_date.s-date.
      IF FIRST-OF(tt_date.s-date) THEN v-time = 0.

      v-time = v-time + 100.
      CREATE tt-tab.
      ASSIGN tt-tab.m-code = mach.m-code
             tt-tab.s-date = tt_date.s-date
             tt-tab.s-time = v-time.
     
  END.

  FOR EACH tt-tab BREAK BY tt-tab.m-code BY tt-tab.s-date BY tt-tab.s-time:
     
      IF FIRST-OF(tt-tab.m-code) THEN DO:
         CREATE tt-sch.
         ASSIGN tt-sch.m-code = tt-tab.m-code.
         i = 1.
      END.
      IF FIRST-OF(tt-tab.s-date) THEN DO:
         IF i <= 7 THEN tt-sch.s-date[i] = tt-tab.s-date.
         i = i + 1.
         j = 1.
      END.
      IF FIRST-OF(tt-tab.s-time) THEN j = 1.
      
      j = int(SUBSTRING(STRING(tt-tab.s-time,"HH:MM"),1,2)).
      j = j + IF int(SUBSTRING(STRING(tt-tab.s-time,"HH:MM"),4,2)) > 0 THEN 1 ELSE 0 .
      tt-sch.s-time[j] = j. 
      /* time - 24 hrs and 10 cells per hour */
      tt-sch.job-list[j] = tt-sch.job-list[j] + STRING(tt-tab.s-time).

      
  END.
*/

  

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
  ENABLE BROWSE-2 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-g_rec_key W-Win 
PROCEDURE get-g_rec_key :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF OUTPUT PARAMETER op-rec_key AS cha .

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN build-table.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 DEF VAR hB AS HANDLE NO-UNDO.
  DEF VAR hC AS HANDLE NO-UNDO.
  DEF VAR vi AS INTEGER NO-UNDO.
  DEF VAR v-stime AS cha FORM "x(50)" NO-UNDO.

  v-stime = "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24".

  hB = BROWSE {&BROWSE-NAME}:HANDLE.

  hC = hB:FIRST-COLUMN.
  vi = 1.
  FIND FIRST tt_date USE-INDEX s-date NO-LOCK NO-ERROR.
  DO WHILE VALID-HANDLE( hC ) :
     /* 
        IF( hC:NAME = "job-list" ) THEN
            hC:LABEL = STRING(hc:SCREEN-VALUE) + "!" + STRING(vi).
        ELSE DO:
            vi = vi + 1.
            hC:LABEL = STRING(hc:SCREEN-VALUE) + "!" + STRING(vi).
        END.
     */


      IF( hC:NAME = "job-list" ) THEN DO:
          hC:LABEL = IF AVAIL tt_date THEN "                                        " + STRING(tt_date.s-date) + "!" + v-stime
                     ELSE STRING(hc:SCREEN-VALUE) + "!" + v-stime.
                     .
          FIND NEXT tt_date USE-INDEX s-date NO-LOCK NO-ERROR.
      END.
      ELSE
          hc:LABEL = "Machine\Date!      Time".

        vi = vi + 1.
        hC = hC:NEXT-COLUMN.
        
  END.
 SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mf-message W-Win 
PROCEDURE mf-message :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER ip-misc-flds AS LOGICAL NO-UNDO.

/*  RUN Show-MF-Message IN h_smartmsg (ip-misc-flds).*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE notes-message W-Win 
PROCEDURE notes-message :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-notes AS LOGICAL NO-UNDO.

  /*RUN Show-Notes-Message IN h_smartmsg (ip-notes). */
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
  {src/adm/template/snd-list.i "tt-sch"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Misc-Rec-Key_Header W-Win 
PROCEDURE Set-Misc-Rec-Key_Header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec_key AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ip-header AS CHARACTER NO-UNDO.

   ASSIGN
      misc_rec_key_value = ip-rec_key
      misc_header_value = ip-header.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-rec-key_header W-Win 
PROCEDURE set-rec-key_header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER ip-rec_key AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-header AS CHARACTER NO-UNDO.

  ASSIGN
    rec_key_value = ip-rec_key
    header_value = ip-header.

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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION disp-job-list W-Win 
FUNCTION disp-job-list RETURNS CHARACTER
  ( INPUT  ip-element AS int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR lv-fillcha AS cha NO-UNDO.
     /* chr(150) - thin bar in default font, 
        chr(103) - thick bar in font 16*/
  lv-fillcha = /* to have a bar - font 16 
                FILL( CHR(103),LENGTH(tt-sch.job-list[ip-element]) )*/

              /*FILL(CHR(103), int(tt-sch.s-time[ip-element])) + 
               tt-sch.job-list[ip-element] +
              IF LENGTH(tt-sch.job-list[ip-element]) < 24 THEN 
                  FILL(CHR(103), 24 - LENGTH(tt-sch.job-list[ip-element]))  
               ELSE ""  */
       
              FILL(" ", int(tt-sch.s-time[ip-element])) + 
               tt-sch.job-list[ip-element] +
              IF LENGTH(tt-sch.job-list[ip-element]) < 24 THEN 
                  FILL(" ", 24 - LENGTH(tt-sch.job-list[ip-element]))  
               ELSE ""  .

  RETURN lv-fillcha.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

