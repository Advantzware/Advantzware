&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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

DEFINE VARIABLE h-XTab AS HANDLE NO-UNDO.
DEFINE VARIABLE h-Bf-tab AS HANDLE NO-UNDO.
DEFINE VARIABLE h-Qry AS HANDLE     NO-UNDO.
 
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
    FIELD job-list AS cha FORM "(10)" EXTENT 168 /* 7X24 */
    .

DEF VAR v-record-position AS ROWID NO-UNDO.  /* record for default highlighted line */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-sch

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-sch.m-code disp-job-list(1) @ tt-sch.job-list[1] disp-job-list(2) @ tt-sch.job-list[2] disp-job-list(3) @ tt-sch.job-list[3] disp-job-list(4) @ tt-sch.job-list[4] disp-job-list(5) @ tt-sch.job-list[5] disp-job-list(6) @ tt-sch.job-list[6] disp-job-list(7) @ tt-sch.job-list[7] disp-job-list(8) @ tt-sch.job-list[8] tt-sch.job-list[1] /*== /* need to specify every element to   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 it or can use [1 for 10] for display only */ ~
 /* do 2 times to display bars instead of job#*/ FILL( CHR(150),LENGTH(tt-sch.job-list[1]) ) @ tt-sch.job-list[1] FORM "x(10)"  FILL(CHR(150),LENGTH(tt-sch.job-list[2])) @ tt-sch.job-list[2]  FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[3])) @ tt-sch.job-list[3]  FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[4])) @ tt-sch.job-list[4]  FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[5])) @ tt-sch.job-list[5]  FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[6])) @ tt-sch.job-list[6] FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[7])) @ tt-sch.job-list[7] FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[8])) @ tt-sch.job-list[8] FORM "x(10)"  tt-sch.job-list[1] FORMAT "x(10)" ~
 tt-sch.job-list[2] FORMAT "x(10)"   tt-sch.job-list[3] FORMAT "x(10)"   tt-sch.job-list[4] FORMAT "x(10)"   tt-sch.job-list[5] FORMAT "x(10)"   tt-sch.job-list[6] FORMAT "x(10)"   tt-sch.job-list[7] FORMAT "x(10)"   tt-sch.job-list[8] FORMAT "x(10)"  FILL( CHR(150),LENGTH(tt-sch.job-list[1]) ) @ tt-sch.job-list[1] FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[2])) @ tt-sch.job-list[2]  FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[3])) @ tt-sch.job-list[3]  FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[4])) @ tt-sch.job-list[4]  FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[5])) @ tt-sch.job-list[5]  FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[6])) @ tt-sch.job-list[6] FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[7])) @ tt-sch.job-list[7] FORM "x(10)" FILL(CHR(150),LENGTH(tt-sch.job-list[8])) @ tt-sch.job-list[8] FORM "x(10)"  /*ENABLE tt-sch.job-list[1] tt-sch.job-list[2] */ ===*/  disp-job-list(1) @ tt-sch.job-list[1] FORM "x(61)"  disp-job-list(2) @ tt-sch.job-list[2] FORM "x(61)"  disp-job-list(3) @ tt-sch.job-list[3] FORM "x(61)"  disp-job-list(4) @ tt-sch.job-list[4] FORM "x(61)"  disp-job-list(5) @ tt-sch.job-list[5] FORM "x(61)"  disp-job-list(6) @ tt-sch.job-list[6] FORM "x(61)"  disp-job-list(7) @ tt-sch.job-list[7] FORM "x(61)"  disp-job-list(8) @ tt-sch.job-list[8] FORM "x(61)" ENABLE tt-sch.job-list[1]   
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


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD disp-job-list wWin 
FUNCTION disp-job-list RETURNS CHARACTER
  ( INPUT  ip-element AS int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE v-help AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 63 BY .95
     BGCOLOR 11  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-sch SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 wWin _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-sch.m-code FORM "x(15)"
          disp-job-list(1) @ tt-sch.job-list[1] FORM "x(61)" COLUMN-LABEL "Label!1"
  disp-job-list(2) @ tt-sch.job-list[2] FORM "x(61)"      COLUMN-LABEL "Label!2"
  disp-job-list(3) @ tt-sch.job-list[3] FORM "x(61)"      COLUMN-LABEL "Label!3"
  disp-job-list(4) @ tt-sch.job-list[4] FORM "x(61)"      COLUMN-LABEL "Label!4"
  disp-job-list(5) @ tt-sch.job-list[5] FORM "x(61)"      COLUMN-LABEL "Label!5"
  disp-job-list(6) @ tt-sch.job-list[6] FORM "x(61)"      COLUMN-LABEL "Label!6"
  disp-job-list(7) @ tt-sch.job-list[7] FORM "x(61)"      COLUMN-LABEL "Label!7"
  disp-job-list(8) @ tt-sch.job-list[8] FORM "x(61)"      COLUMN-LABEL "Label!8"
      tt-sch.job-list[1]
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
  disp-job-list(8) @ tt-sch.job-list[8] FORM "x(61)"

ENABLE tt-sch.job-list[1]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 129 BY 16.91 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BROWSE-2 AT ROW 1.24 COL 2
     v-help AT ROW 18.38 COL 8 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 132.8 BY 19.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Machine Schedule"
         HEIGHT             = 19
         WIDTH              = 132.8
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
                                                                        */
/* BROWSE-TAB BROWSE-2 1 fMain */
/* SETTINGS FOR FILL-IN v-help IN FRAME fMain
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       v-help:HIDDEN IN FRAME fMain           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

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

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Machine Schedule */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Machine Schedule */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 wWin
ON ROW-DISPLAY OF BROWSE-2 IN FRAME fMain
DO:
  
    ASSIGN tt-sch.m-code:BGCOLOR IN BROWSE {&browse-name} = ?
              tt-sch.job-list[1]:BGCOLOR IN BROWSE {&browse-name} = ?
              tt-sch.job-list[2]:BGCOLOR IN BROWSE {&browse-name} = ?
              tt-sch.job-list[3]:BGCOLOR IN BROWSE {&browse-name} = ?
              tt-sch.job-list[4]:BGCOLOR IN BROWSE {&browse-name} = ?
              tt-sch.job-list[5]:BGCOLOR IN BROWSE {&browse-name} = ?
              tt-sch.job-list[6]:BGCOLOR IN BROWSE {&browse-name} = ?
              tt-sch.job-list[7]:BGCOLOR IN BROWSE {&browse-name} = ?
              tt-sch.job-list[8]:BGCOLOR IN BROWSE {&browse-name} = ?
              .

       ASSIGN tt-sch.job-list[1]:FGCOLOR IN BROWSE {&browse-name} = 2
              tt-sch.job-list[2]:FGCOLOR IN BROWSE {&browse-name} = 3
              tt-sch.job-list[3]:FGCOLOR IN BROWSE {&browse-name} = 4
              tt-sch.job-list[4]:FGCOLOR IN BROWSE {&browse-name} = 5
              tt-sch.job-list[5]:FGCOLOR IN BROWSE {&browse-name} = 6
              tt-sch.job-list[6]:FGCOLOR IN BROWSE {&browse-name} = 7
              tt-sch.job-list[7]:FGCOLOR IN BROWSE {&browse-name} = 8
              tt-sch.job-list[8]:FGCOLOR IN BROWSE {&browse-name} = 9.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */
ON 'mouse-select-click':U OF tt-sch.job-list[1] IN BROWSE {&browse-name}
DO:
    IF tt-sch.s-seq = 1 THEN RETURN NO-APPLY.

   /* DEFINE FRAME f-tab.
  
    CREATE WIDGET-POOL "w-tab" PERSISTENT NO-ERROR. 
    ASSIGN FRAME f-tab:X = tt-sch.job-list[1]:X IN BROWSE {&browse-name}
           FRAME f-tab:Y = tt-sch.job-list[1]:y IN BROWSE {&browse-name}  
           FRAME f-tab:VISIBLE = YES
           .
           
   DISPLAY tt-sch.job-list[1] WITH FRAME f-tab OVERLAY TOP-ONLY NO-LABELS WIDTH 35.
   */
    v-help = tt-sch.job-list[1].
    ASSIGN v-help:X IN FRAME {&FRAME-NAME} = tt-sch.job-list[1]:X IN BROWSE {&browse-name}
           v-help:Y = tt-sch.job-list[1]:y + 12
           v-help:SCREEN-VALUE = tt-sch.job-list[1]
           v-help:VISIBLE = YES.

    RETURN.
END.
ON 'leave':U OF tt-sch.job-list[1] IN BROWSE {&browse-name}
DO:
    v-help:VISIBLE IN FRAME {&FRAME-NAME} = NO.
    RETURN.
END.
/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table wWin 
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
  DO i = 1 TO 10.
        v-cnt = v-cnt + 1.
        CREATE tt_date.
        ASSIGN tt_date.s-seq = STRING(v-cnt)
               tt_date.s-date = v-date
               tt_date.s-time = j.
        v-date = v-date + 1.
  END.

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
/*
  CREATE TEMP-TABLE h-xtab.
  h-xtab:ADD-LIKE-FIELD("m-code","machine.m-code").
  FOR EACH tt_date:
    h-xtab:ADD-NEW-FIELD(tt_date.s-seq,"cha",0,"x(5)","s-seq").
  END.

  h-XTab:TEMP-TABLE-PREPARE("CrossTab").
  h-Bf-tab = h-XTab:DEFAULT-BUFFER-HANDLE.

  DO v-cnt = 2 TO h-bf-tab:NUM-FIELDS:
    h-Field = h-Bf-tab:BUFFER-FIELD(v-cnt).
    FIND tt_date NO-LOCK WHERE tt_date.s-seq = h-Field:NAME NO-ERROR.
    IF AVAILABLE(tt_date) THEN
      h-Field:LABEL = string(tt_date.s-date,"99/99/9999") + "!" +
                     STRING(tt_date.s-time).
  END.


  FOR EACH tt-tab BREAK BY tt-tab.m-code BY tt-tab.s-date:
      IF FIRST-OF(tt-tab.m-code) THEN
      DO:
         h-Bf-tab:BUFFER-CREATE().
         h-Field = h-Bf-tab:BUFFER-FIELD("m-code").
         h-Field:BUFFER-VALUE = tt-tab.m-code.
      END.
      IF FIRST-OF(tt-tab.s-date) THEN v-job = "100".

      h-Field = h-Bf-tab:BUFFER-FIELD(tt-tab.s-date).
      /* Set the amount */
      hField:BUFFER-VALUE = v-job + "X".


     IF LAST-OF(tt_Summary.State) THEN
     DO:
         h-Bf-tab:BUFFER-RELEASE().
     END.

  END.
  ========*/
  v-record-position = ?.

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
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN build-table.

  RUN SUPER.

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION disp-job-list wWin 
FUNCTION disp-job-list RETURNS CHARACTER
  ( INPUT  ip-element AS int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR lv-fillcha AS cha NO-UNDO.

  lv-fillcha = IF tt-sch.s-seq = 1 THEN tt-sch.job-list[ip-element]
                ELSE FILL( CHR(150),LENGTH(tt-sch.job-list[ip-element]) ).

  RETURN lv-fillcha.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

