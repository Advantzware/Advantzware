&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/<table>.w

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
&scoped-define enable-proc proc-enable

DEF VAR lv-save-rpt LIKE gl-rpt.rpt NO-UNDO.
DEF BUFFER bf-rpt FOR gl-rpt.

{sys/inc/VAR.i NEW SHARED}
{gl/gl-rptsv.i NEW SHARED}
ASSIGN cocode = g_company
       locode = g_loc.

{gl/gl-fs.i NEW}

DEF VAR lv-gl-type AS INT NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.

&SCOPED-DEFINE where-pct-subtotal                               ~
    WHERE reftable.reftable EQ "gl-rpt.pct-subtotal"            ~
      AND reftable.company  EQ gl-rpt.company                   ~
      AND reftable.loc      EQ ""                               ~
      AND reftable.code     EQ gl-rpt.rpt                       ~
      AND reftable.code2    EQ STRING(gl-rpt.line,"9999999999")

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES gl-rpt
&Scoped-define FIRST-EXTERNAL-TABLE gl-rpt


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR gl-rpt.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS gl-rpt.line gl-rpt.dscr gl-rpt.acrange[1] ~
gl-rpt.acrange[2] gl-rpt.acrange1[1] gl-rpt.acrange1[2] gl-rpt.acrange1[3] ~
gl-rpt.acrange1[4] gl-rpt.acrange1[5] gl-rpt.acrange1[6] gl-rpt.acrange1[7] ~
gl-rpt.acrange1[8] 
&Scoped-define ENABLED-TABLES gl-rpt
&Scoped-define FIRST-ENABLED-TABLE gl-rpt
&Scoped-Define ENABLED-OBJECTS RECT-2 
&Scoped-Define DISPLAYED-FIELDS gl-rpt.rpt gl-rpt.line gl-rpt.dscr ~
gl-rpt.acrange[1] gl-rpt.acrange[2] gl-rpt.acrange1[1] gl-rpt.acrange1[2] ~
gl-rpt.acrange1[3] gl-rpt.acrange1[4] gl-rpt.acrange1[5] gl-rpt.acrange1[6] ~
gl-rpt.acrange1[7] gl-rpt.acrange1[8] 
&Scoped-define DISPLAYED-TABLES gl-rpt
&Scoped-define FIRST-DISPLAYED-TABLE gl-rpt
&Scoped-Define DISPLAYED-OBJECTS lv-d-type lv-break lv-pct-subtotal ~
lv-s-dscr-1 lv-s-dscr-2 lv-s-dscr-3 lv-s-dscr-4 lv-s-dscr-5 lv-s-dscr-6 ~
lv-s-dscr-7 lv-s-dscr-8 lv-s-dscr-9 lv-s-dscr-10 lv-s-dscr-11 lv-s-dscr-12 ~
lv-s-yn-1 lv-s-yn-2 lv-s-yn-3 lv-s-yn-4 lv-s-yn-5 lv-s-yn-6 lv-s-yn-7 ~
lv-s-yn-8 lv-s-yn-9 lv-s-yn-10 lv-s-yn-11 lv-s-yn-12 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define List-5 lv-d-type lv-break lv-pct-subtotal lv-s-dscr-1 ~
lv-s-dscr-2 lv-s-dscr-3 lv-s-dscr-4 lv-s-dscr-5 lv-s-dscr-6 lv-s-dscr-7 ~
lv-s-dscr-8 lv-s-dscr-9 lv-s-dscr-10 lv-s-dscr-11 lv-s-dscr-12 lv-s-yn-1 ~
lv-s-yn-2 lv-s-yn-3 lv-s-yn-4 lv-s-yn-5 lv-s-yn-6 lv-s-yn-7 lv-s-yn-8 ~
lv-s-yn-9 lv-s-yn-10 lv-s-yn-11 lv-s-yn-12 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE lv-d-type AS CHARACTER FORMAT "X(25)":U 
     LABEL "Type" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-dscr-1 AS CHARACTER FORMAT "X(13)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-dscr-10 AS CHARACTER FORMAT "X(13)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-dscr-11 AS CHARACTER FORMAT "X(13)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-dscr-12 AS CHARACTER FORMAT "X(13)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-dscr-2 AS CHARACTER FORMAT "X(13)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-dscr-3 AS CHARACTER FORMAT "X(13)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-dscr-4 AS CHARACTER FORMAT "X(13)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-dscr-5 AS CHARACTER FORMAT "X(13)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-dscr-6 AS CHARACTER FORMAT "X(13)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-dscr-7 AS CHARACTER FORMAT "X(13)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-dscr-8 AS CHARACTER FORMAT "X(13)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-dscr-9 AS CHARACTER FORMAT "X(13)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 17.14.

DEFINE VARIABLE lv-break AS LOGICAL INITIAL no 
     LABEL "Break Page?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE lv-pct-subtotal AS LOGICAL INITIAL no 
     LABEL "Print % of Subtotals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE lv-s-yn-1 AS LOGICAL INITIAL no 
     LABEL "Toggle 1" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-yn-10 AS LOGICAL INITIAL no 
     LABEL "lv s yn 10" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-yn-11 AS LOGICAL INITIAL no 
     LABEL "lv s yn 10" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-yn-12 AS LOGICAL INITIAL no 
     LABEL "lv s yn 10" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-yn-2 AS LOGICAL INITIAL no 
     LABEL "lv s yn 2" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-yn-3 AS LOGICAL INITIAL no 
     LABEL "lv s yn 3" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-yn-4 AS LOGICAL INITIAL no 
     LABEL "lv s yn 4" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-yn-5 AS LOGICAL INITIAL no 
     LABEL "lv s yn 5" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-yn-6 AS LOGICAL INITIAL no 
     LABEL "lv s yn 6" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-yn-7 AS LOGICAL INITIAL no 
     LABEL "lv s yn 7" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-yn-8 AS LOGICAL INITIAL no 
     LABEL "lv s yn 8" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-s-yn-9 AS LOGICAL INITIAL no 
     LABEL "lv s yn 9" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     gl-rpt.rpt AT ROW 1.24 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     gl-rpt.line AT ROW 2.19 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     lv-d-type AT ROW 2.19 COL 41 COLON-ALIGNED
     lv-break AT ROW 1.24 COL 38
     lv-pct-subtotal AT ROW 1.24 COL 61
     gl-rpt.dscr AT ROW 3.14 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
     gl-rpt.acrange[1] AT ROW 5.52 COL 14 COLON-ALIGNED
          LABEL "Fr Acct1"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     gl-rpt.acrange[2] AT ROW 6.48 COL 14 COLON-ALIGNED
          LABEL "To Acct1"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     gl-rpt.acrange1[1] AT ROW 7.52 COL 14 COLON-ALIGNED
          LABEL "Fr Acct2"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     gl-rpt.acrange1[2] AT ROW 8.52 COL 14 COLON-ALIGNED
          LABEL "To Acct2"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     gl-rpt.acrange1[3] AT ROW 9.52 COL 14 COLON-ALIGNED
          LABEL "Fr Acct3"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     gl-rpt.acrange1[4] AT ROW 10.52 COL 14 COLON-ALIGNED
          LABEL "To Acct3"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     gl-rpt.acrange1[5] AT ROW 11.52 COL 14 COLON-ALIGNED
          LABEL "Fr Acct4"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     gl-rpt.acrange1[6] AT ROW 12.52 COL 14 COLON-ALIGNED
          LABEL "To Acct4"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     gl-rpt.acrange1[7] AT ROW 13.52 COL 14 COLON-ALIGNED
          LABEL "Fr Acct5"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     gl-rpt.acrange1[8] AT ROW 14.52 COL 14 COLON-ALIGNED
          LABEL "To Acct5"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     lv-s-dscr-1 AT ROW 5.52 COL 56 COLON-ALIGNED NO-LABEL
     lv-s-dscr-2 AT ROW 6.48 COL 56 COLON-ALIGNED NO-LABEL
     lv-s-dscr-3 AT ROW 7.52 COL 56 COLON-ALIGNED NO-LABEL
     lv-s-dscr-4 AT ROW 8.52 COL 56 COLON-ALIGNED NO-LABEL
     lv-s-dscr-5 AT ROW 9.52 COL 56 COLON-ALIGNED NO-LABEL
     lv-s-dscr-6 AT ROW 10.52 COL 56 COLON-ALIGNED NO-LABEL
     lv-s-dscr-7 AT ROW 11.52 COL 56 COLON-ALIGNED NO-LABEL
     lv-s-dscr-8 AT ROW 12.52 COL 56 COLON-ALIGNED NO-LABEL
     lv-s-dscr-9 AT ROW 13.52 COL 56 COLON-ALIGNED NO-LABEL
     lv-s-dscr-10 AT ROW 14.57 COL 56 COLON-ALIGNED NO-LABEL
     lv-s-dscr-11 AT ROW 15.52 COL 56 COLON-ALIGNED NO-LABEL
     lv-s-dscr-12 AT ROW 16.48 COL 56 COLON-ALIGNED NO-LABEL
     lv-s-yn-1 AT ROW 5.52 COL 83
     lv-s-yn-2 AT ROW 6.71 COL 83
     lv-s-yn-3 AT ROW 7.67 COL 83
     lv-s-yn-4 AT ROW 8.62 COL 83
     lv-s-yn-5 AT ROW 9.81 COL 83
     lv-s-yn-6 AT ROW 10.76 COL 83
     lv-s-yn-7 AT ROW 11.71 COL 83
     lv-s-yn-8 AT ROW 12.67 COL 83
     lv-s-yn-9 AT ROW 13.62 COL 83
     lv-s-yn-10 AT ROW 14.57 COL 83
     lv-s-yn-11 AT ROW 15.52 COL 83
     lv-s-yn-12 AT ROW 16.48 COL 83
     RECT-2 AT ROW 1 COL 1
     "From/To Accounts" VIEW-AS TEXT
          SIZE 23 BY .86 AT ROW 4.57 COL 21
     "Subtotal List" VIEW-AS TEXT
          SIZE 16 BY .86 AT ROW 4.57 COL 65
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.gl-rpt
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 17.14
         WIDTH              = 144.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN gl-rpt.acrange1[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN gl-rpt.acrange1[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN gl-rpt.acrange1[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN gl-rpt.acrange1[4] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN gl-rpt.acrange1[5] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN gl-rpt.acrange1[6] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN gl-rpt.acrange1[7] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN gl-rpt.acrange1[8] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN gl-rpt.acrange[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN gl-rpt.acrange[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX lv-break IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lv-d-type IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX lv-pct-subtotal IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lv-s-dscr-1 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lv-s-dscr-10 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lv-s-dscr-11 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lv-s-dscr-12 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lv-s-dscr-2 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lv-s-dscr-3 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lv-s-dscr-4 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lv-s-dscr-5 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lv-s-dscr-6 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lv-s-dscr-7 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lv-s-dscr-8 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lv-s-dscr-9 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX lv-s-yn-1 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX lv-s-yn-10 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX lv-s-yn-11 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX lv-s-yn-12 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX lv-s-yn-2 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX lv-s-yn-3 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX lv-s-yn-4 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX lv-s-yn-5 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX lv-s-yn-6 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX lv-s-yn-7 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX lv-s-yn-8 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX lv-s-yn-9 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN gl-rpt.rpt IN FRAME F-Main
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
    DEF VAR char-val AS cha NO-UNDO.


    CASE FOCUS:NAME :
        WHEN "acrange" OR WHEN "acrange1" OR WHEN "actnum" THEN DO:
             RUN windows/l-acct3.w (g_company,"T",focus:screen-value,OUTPUT char-val).
             IF char-val <> "" THEN DO:
                ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
                /*CASE FOCUS:INDEX:
                    WHEN 1 THEN 
                END CASE.
                */
             END.
        END.
        WHEN "lv-d-type" THEN DO:
            RUN gl/l-dtypes.w (OUTPUT char-val).
            IF char-val <> "" THEN 
               ASSIGN lv-gl-type = INT(ENTRY(1,char-val))
                      FOCUS:SCREEN-VALUE = ENTRY(2,char-val).
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gl-rpt.acrange1[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-rpt.acrange1[1] V-table-Win
ON LEAVE OF gl-rpt.acrange1[1] IN FRAME F-Main /* Fr Acct2 */
DO:
  IF LASTKEY = -1  THEN RETURN.
  RUN validate-acct (gl-rpt.acrange1[1]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  {&methods/lValidateError.i YES}
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.acrange1[1]. 
     RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gl-rpt.acrange1[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-rpt.acrange1[2] V-table-Win
ON LEAVE OF gl-rpt.acrange1[2] IN FRAME F-Main /* To Acct2 */
DO:
  IF LASTKEY = -1  THEN RETURN.
  RUN validate-acct (gl-rpt.acrange1[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  {&methods/lValidateError.i YES}
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.acrange1[2]. 
     RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gl-rpt.acrange1[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-rpt.acrange1[3] V-table-Win
ON LEAVE OF gl-rpt.acrange1[3] IN FRAME F-Main /* Fr Acct3 */
DO:
    IF LASTKEY = -1 THEN RETURN.
    RUN validate-acct (gl-rpt.acrange1[3]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
    {&methods/lValidateError.i YES}
    IF ERROR-STATUS:ERROR THEN do:
       APPLY "entry" TO gl-rpt.acrange1[3]. 
       RETURN NO-APPLY.
    END.
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gl-rpt.acrange1[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-rpt.acrange1[4] V-table-Win
ON LEAVE OF gl-rpt.acrange1[4] IN FRAME F-Main /* To Acct3 */
DO:
   IF LASTKEY = -1 THEN RETURN.
    RUN validate-acct (gl-rpt.acrange1[4]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
    {&methods/lValidateError.i YES}
    IF ERROR-STATUS:ERROR THEN do:
       APPLY "entry" TO gl-rpt.acrange1[4]. 
       RETURN NO-APPLY.
    END.
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gl-rpt.acrange1[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-rpt.acrange1[5] V-table-Win
ON LEAVE OF gl-rpt.acrange1[5] IN FRAME F-Main /* Fr Acct4 */
DO:
   IF LASTKEY = -1 THEN RETURN.
    RUN validate-acct (gl-rpt.acrange1[5]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
    {&methods/lValidateError.i YES}
    IF ERROR-STATUS:ERROR THEN do:
       APPLY "entry" TO gl-rpt.acrange1[5].
       RETURN NO-APPLY.
    END.
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gl-rpt.acrange1[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-rpt.acrange1[6] V-table-Win
ON LEAVE OF gl-rpt.acrange1[6] IN FRAME F-Main /* To Acct4 */
DO:
   IF LASTKEY = -1 THEN RETURN.
    RUN validate-acct (gl-rpt.acrange1[6]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
    {&methods/lValidateError.i YES}
    IF ERROR-STATUS:ERROR THEN do:
       APPLY "entry" TO gl-rpt.acrange1[6]. 
       RETURN NO-APPLY.
    END.
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gl-rpt.acrange1[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-rpt.acrange1[7] V-table-Win
ON LEAVE OF gl-rpt.acrange1[7] IN FRAME F-Main /* Fr Acct5 */
DO:
   IF LASTKEY = -1 THEN RETURN.
    RUN validate-acct (gl-rpt.acrange1[7]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
    {&methods/lValidateError.i YES}
    IF ERROR-STATUS:ERROR THEN do:
       APPLY "entry" TO gl-rpt.acrange1[7]. 
       RETURN NO-APPLY.
    END.
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gl-rpt.acrange1[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-rpt.acrange1[8] V-table-Win
ON LEAVE OF gl-rpt.acrange1[8] IN FRAME F-Main /* To Acct5 */
DO:
   IF LASTKEY = -1 THEN RETURN.
    RUN validate-acct (gl-rpt.acrange1[8]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
    {&methods/lValidateError.i YES}
    IF ERROR-STATUS:ERROR THEN do:
       APPLY "entry" TO gl-rpt.acrange1[8]. 
       RETURN NO-APPLY.
    END.
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gl-rpt.acrange[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-rpt.acrange[1] V-table-Win
ON LEAVE OF gl-rpt.acrange[1] IN FRAME F-Main /* Fr Acct1 */
DO:
  IF LASTKEY = -1  THEN RETURN.
  RUN validate-acct (gl-rpt.acrange[1]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  {&methods/lValidateError.i YES}
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.acrange[1]. 
     RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gl-rpt.acrange[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-rpt.acrange[2] V-table-Win
ON LEAVE OF gl-rpt.acrange[2] IN FRAME F-Main /* To Acct1 */
DO:
  IF LASTKEY = -1  THEN RETURN.
  RUN validate-acct (gl-rpt.acrange[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  {&methods/lValidateError.i YES}
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.acrange[2]. 
     RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gl-rpt.line
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-rpt.line V-table-Win
ON LEAVE OF gl-rpt.line IN FRAME F-Main /* Line */
DO:
   IF LASTKEY = -1 THEN RETURN.

   RUN validate-line#  NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-d-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-d-type V-table-Win
ON LEAVE OF lv-d-type IN FRAME F-Main /* Type */
DO:
    IF LASTKEY = -1 THEN RETURN.
    {&methods/lValidateError.i YES}

    IF LOOKUP(string(lv-gl-type,"99"),"21,22,23,24,60,61,71,73,90") = 0 THEN do:
        MESSAGE "Invalid Type. " VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    {&methods/lValidateError.i NO}

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
SESSION:DATA-ENTRY-RETURN = YES.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "gl-rpt"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "gl-rpt"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-correct-gl-rpt V-table-Win 
PROCEDURE get-correct-gl-rpt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  DEF VAR char-hdl AS CHAR NO-UNDO.


  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    RUN repo-query IN WIDGET-HANDLE(char-hdl) (ip-rowid).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if gl-rpt.type gt 100 then gl-rpt.type / 10.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-break lv-pct-subtotal lv-d-type
     lv-s-yn-1 lv-s-yn-2 lv-s-yn-3 lv-s-yn-4
     lv-s-yn-5 lv-s-yn-6 lv-s-yn-7 lv-s-yn-8
     lv-s-yn-9 lv-s-yn-10 lv-s-yn-11 lv-s-yn-12.
  END.

  IF lv-gl-type NE 0 THEN gl-rpt.type = lv-gl-type.

  SUBSTR(gl-rpt.flag,01,1) = IF lv-s-yn-1  THEN "Y" ELSE "N".
  SUBSTR(gl-rpt.flag,02,1) = IF lv-s-yn-2  THEN "Y" ELSE "N".
  SUBSTR(gl-rpt.flag,03,1) = IF lv-s-yn-3  THEN "Y" ELSE "N".
  SUBSTR(gl-rpt.flag,04,1) = IF lv-s-yn-4  THEN "Y" ELSE "N".
  SUBSTR(gl-rpt.flag,05,1) = IF lv-s-yn-5  THEN "Y" ELSE "N".
  SUBSTR(gl-rpt.flag,06,1) = IF lv-s-yn-6  THEN "Y" ELSE "N".
  SUBSTR(gl-rpt.flag,07,1) = IF lv-s-yn-7  THEN "Y" ELSE "N".
  SUBSTR(gl-rpt.flag,08,1) = IF lv-s-yn-8  THEN "Y" ELSE "N".
  SUBSTR(gl-rpt.flag,09,1) = IF lv-s-yn-9  THEN "Y" ELSE "N".
  SUBSTR(gl-rpt.flag,10,1) = IF lv-s-yn-10 THEN "Y" ELSE "N".
  SUBSTR(gl-rpt.flag,11,1) = IF lv-s-yn-11 THEN "Y" ELSE "N".
  SUBSTR(gl-rpt.flag,12,1) = IF lv-s-yn-12 THEN "Y" ELSE "N".

  IF gl-rpt.type EQ 60 OR
     gl-rpt.type EQ 61 THEN
  DO i = 1 TO 12:
    IF SUBSTR(gl-rpt.flag,i,1) = "Y" THEN DO:
      gl-rpt.level = i.
      LEAVE.
    END.
  END.

  IF lv-break THEN gl-rpt.type = gl-rpt.type * 10.

  RUN reftable-values (NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-correct-gl-rpt (lv-rowid).

  DISABLE lv-d-type lv-break lv-pct-subtotal
          lv-s-yn-1 lv-s-yn-2 lv-s-yn-3 lv-s-yn-4
          lv-s-yn-5 lv-s-yn-6 lv-s-yn-7 lv-s-yn-8
          lv-s-yn-9 lv-s-yn-10 lv-s-yn-11 lv-s-yn-12
          WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li-next AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT AVAIL gl-rpt THEN DO:
    RUN GET-ATTRIBUTE IN adm-broker-hdl ("GLRPT-NUM").
    IF RETURN-VALUE NE "" THEN lv-save-rpt = RETURN-VALUE.
    lv-rowid = ?.
  END.
  ELSE
    ASSIGN
     lv-save-rpt = gl-rpt.rpt
     lv-rowid    = ROWID(gl-rpt).

  FIND LAST bf-rpt
      WHERE bf-rpt.company EQ g_company
        AND bf-rpt.rpt     EQ lv-save-rpt
      NO-LOCK NO-ERROR.

  li-next = IF AVAIL bf-rpt AND bf-rpt.line LT 100 THEN 100 
            ELSE
            IF AVAIL bf-rpt AND bf-rpt.line GE 100 THEN bf-rpt.line + 10
            ELSE 100.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   gl-rpt.company = g_company
   gl-rpt.rpt     = lv-save-rpt
   gl-rpt.type    = 22
   gl-rpt.flag    = "NNNNNNNNNNNN"
   gl-rpt.line    = li-next.

  RUN dispatch ('display-fields').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN lv-s-yn-1 = NO
         lv-s-yn-2 = NO
         lv-s-yn-3 = NO
         lv-s-yn-4 = NO
         lv-s-yn-5 = NO
         lv-s-yn-6 = NO
         lv-s-yn-7 = NO
         lv-s-yn-8 = NO
         lv-s-yn-9 = NO
         lv-s-yn-10 = NO
         lv-s-yn-11 = NO
         lv-s-yn-12 = NO.

  IF AVAIL gl-rpt THEN DO:
     DO i = 1 TO 12:
        IF SUBSTRING(gl-rpt.flag,i,1) = "Y" THEN CASE i:
            WHEN 1 THEN lv-s-yn-1 = YES.
            WHEN 2 THEN lv-s-yn-2 = YES.
            WHEN 3 THEN lv-s-yn-3 = YES.
            WHEN 4 THEN lv-s-yn-4 = YES.
            WHEN 5 THEN lv-s-yn-5 = YES.
            WHEN 6 THEN lv-s-yn-6 = YES.
            WHEN 7 THEN lv-s-yn-7 = YES.
            WHEN 8 THEN lv-s-yn-8 = YES.
            WHEN 9 THEN lv-s-yn-9 = YES.
            WHEN 10 THEN lv-s-yn-10 = YES.                       
            WHEN 11 THEN lv-s-yn-11 = YES.                       
            WHEN 12 THEN lv-s-yn-12 = YES.                       
        END.
     END.
     DO i = 1 TO v-n-types:
        IF gl-rpt.type = {gl/gl-type.i v-type-no[i]} THEN lv-d-type = v-type[i].
     END.

     run gl/gl-rptg.p (input recid(gl-rpt), input no).
     ASSIGN lv-s-dscr-1 = v-sub[1]
          lv-s-dscr-2 = v-sub[2]
          lv-s-dscr-3 = v-sub[3]
          lv-s-dscr-4 = v-sub[4]
          lv-s-dscr-5 = v-sub[5]
          lv-s-dscr-6 = v-sub[6]
          lv-s-dscr-7 = v-sub[7]
          lv-s-dscr-8 = v-sub[8]
          lv-s-dscr-9 = v-sub[9]
          lv-s-dscr-10 = v-sub[10]
          lv-s-dscr-11 = v-sub[11]            
          lv-s-dscr-12 = v-sub[12]            
          .
      lv-break = gl-rpt.type > 100.
      lv-gl-type = gl-rpt.type / (IF lv-break THEN 10 ELSE 1).
  END.

  IF AVAIL gl-rpt AND NOT adm-new-record THEN RUN reftable-values (YES).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-tmp-new AS LOG NO-UNDO.
  DEF VAR hld-pct-subtotal LIKE lv-pct-subtotal NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
   hld-pct-subtotal = lv-pct-subtotal
   ll-tmp-new       = adm-new-record.

  RUN validate-line#  NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN validate-record NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  RUN validate-acct (gl-rpt.acrange[1]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.acrange[1]. 
     RETURN.
  END.
  RUN validate-acct (gl-rpt.acrange[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.acrange[2]. 
     RETURN.
  END.
  RUN validate-acct (gl-rpt.acrange1[1]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.acrange1[1]. 
     RETURN.
  END.
  RUN validate-acct (gl-rpt.acrange1[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.acrange1[2]. RETURN.
  END.
  RUN validate-acct (gl-rpt.acrange1[3]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.acrange1[3]. 
     RETURN.
  END.
  RUN validate-acct (gl-rpt.acrange1[4]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.acrange1[4]. 
     RETURN.
  END.
  RUN validate-acct (gl-rpt.acrange1[5]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.acrange1[5]. 
     RETURN.
  END.
  RUN validate-acct (gl-rpt.acrange1[6]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.acrange1[6]. 
     RETURN.
  END.
  RUN validate-acct (gl-rpt.acrange1[7]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.acrange1[7]. 
     RETURN.
  END.
  RUN validate-acct (gl-rpt.acrange1[8]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.acrange1[8]. 
     RETURN.
  END.
  /*RUN validate-acct (gl-rpt.actnum[1]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.actnum[1]. 
     RETURN.
  END.
  RUN validate-acct (gl-rpt.actnum[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.actnum[2]. 
     RETURN.
  END.
   RUN validate-acct (gl-rpt.actnum[3]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.actnum[3]. 
     RETURN.
  END.
   RUN validate-acct (gl-rpt.actnum[4]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.actnum[4]. 
     RETURN.
  END.
   RUN validate-acct (gl-rpt.actnum[5]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.actnum[5]. 
     RETURN.
  END.
   RUN validate-acct (gl-rpt.actnum[6]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.actnum[6]. 
     RETURN.
  END.
   RUN validate-acct (gl-rpt.actnum[7]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.actnum[7]. 
     RETURN.
  END.
   RUN validate-acct (gl-rpt.actnum[8]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.actnum[8]. 
     RETURN.
  END.
   RUN validate-acct (gl-rpt.actnum[9]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.actnum[9]. 
     RETURN.
  END.
   RUN validate-acct (gl-rpt.actnum[10]:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN do:
     APPLY "entry" TO gl-rpt.actnum[10]. 
     RETURN.
  END.*/

  lv-rowid = ROWID(gl-rpt).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-correct-gl-rpt (lv-rowid).

  DISABLE lv-d-type lv-break lv-pct-subtotal
          lv-s-yn-1 lv-s-yn-2 lv-s-yn-3 lv-s-yn-4
          lv-s-yn-5 lv-s-yn-6 lv-s-yn-7 lv-s-yn-8
          lv-s-yn-9 lv-s-yn-10 lv-s-yn-11 lv-s-yn-12
          WITH FRAME {&FRAME-NAME}.

  IF lv-pct-subtotal NE hld-pct-subtotal THEN DO:
    ll = NO.
    MESSAGE "Update all lines to " + (IF lv-pct-subtotal THEN "" ELSE "NOT ") +
            TRIM(lv-pct-subtotal:LABEL)
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll.

    IF ll THEN DO:
      FIND bf-rpt WHERE ROWID(bf-rpt) EQ lv-rowid NO-LOCK NO-ERROR.

      IF AVAIL bf-rpt THEN
      FOR EACH gl-rpt
          WHERE gl-rpt.company EQ bf-rpt.company
            AND gl-rpt.rpt     EQ bf-rpt.rpt
            AND bf-rpt.line    GT 99
          TRANSACTION:
        RUN reftable-values (NO).
      END.

      FIND gl-rpt WHERE ROWID(gl-rpt) EQ lv-rowid NO-LOCK NO-ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-enable V-table-Win 
PROCEDURE proc-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ENABLE lv-d-type lv-break lv-pct-subtotal
         lv-s-yn-1 lv-s-yn-2 lv-s-yn-3 lv-s-yn-4
         lv-s-yn-5 lv-s-yn-6 lv-s-yn-7 lv-s-yn-8
         lv-s-yn-9 lv-s-yn-10 lv-s-yn-11 lv-s-yn-12
         WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reftable-values V-table-Win 
PROCEDURE reftable-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-display AS LOG NO-UNDO.


  IF AVAIL gl-rpt THEN DO:
    FIND FIRST reftable {&where-pct-subtotal} NO-ERROR.
    IF NOT AVAIL reftable THEN DO:
      CREATE reftable.
      ASSIGN
       reftable.reftable = "gl-rpt.pct-subtotal"
       reftable.company  = gl-rpt.company
       reftable.loc      = ""
       reftable.code     = gl-rpt.rpt
       reftable.code2    = STRING(gl-rpt.line,"9999999999").
    END.
    IF ip-display THEN
      lv-pct-subtotal = reftable.val[1] GT 0.
    ELSE
      reftable.val[1] = INT(lv-pct-subtotal).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "gl-rpt"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-acct V-table-Win 
PROCEDURE validate-acct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-account LIKE account.actnum.

  {methods/lValidateError.i YES}
  FIND FIRST account WHERE account.company = g_company
                         AND account.type <> "T" AND
                             account.actnum =     ip-account NO-LOCK NO-ERROR.
  IF NOT AVAIL account AND ip-account <> "" THEN DO:
      MESSAGE "Invalid Account Number. Try Help." VIEW-AS ALERT-BOX.      
      RETURN ERROR.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-line# V-table-Win 
PROCEDURE validate-line# :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-glrpt FOR gl-rpt.
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
     IF INT(gl-rpt.LINE:SCREEN-VALUE) < 100 THEN DO:
        MESSAGE "Line# can not be less than 100. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO gl-rpt.LINE.
        RETURN ERROR.
     END.
     IF CAN-FIND(FIRST bf-glrpt WHERE bf-glrpt.company = gl-rpt.company
                               AND bf-glrpt.rpt = gl-rpt.rpt
                               AND bf-glrpt.LINE = INT(gl-rpt.LINE:SCREEN-VALUE)
                               AND RECID(bf-glrpt) <> RECID(gl-rpt)
              )
     THEN DO:
        MESSAGE "Line# already exists. Try other number. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO gl-rpt.LINE.
        RETURN ERROR.
     END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-record V-table-Win 
PROCEDURE validate-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
    IF LOOKUP(string(lv-gl-type,"99"),"21,22,23,24,60,61,71,73,90") = 0 THEN do:
        MESSAGE "Invalid Type. Type must be 22 or 24 or 73 or 90. " VIEW-AS ALERT-BOX.
        APPLY "entry" TO lv-d-type IN FRAME {&FRAME-NAME}.
        RETURN ERROR.
    END.


  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

