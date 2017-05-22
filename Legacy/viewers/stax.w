&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/stax.w

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

{custom/gcompany.i}
{custom/gloc.i}

{sys/inc/VAR.i NEW SHARED}

DEFINE SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

DEF BUFFER b-stax FOR stax.
DEFINE TEMP-TABLE tt-accounts NO-UNDO
    FIELD i-extent AS INTEGER
    FIELD c-code   AS CHARACTER
    FIELD c-acct   AS CHARACTER
    FIELD h-acct   AS HANDLE
    INDEX KEY AS PRIMARY UNIQUE i-extent.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES stax
&Scoped-define FIRST-EXTERNAL-TABLE stax


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR stax.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS stax.tax-code1[1] stax.tax-dscr1[1] ~
stax.tax-rate1[1] stax.tax-frt1[1] stax.tax-acc1[1] stax.tax-code1[2] ~
stax.tax-dscr1[2] stax.tax-rate1[2] stax.tax-frt1[2] stax.tax-acc1[2] ~
stax.tax-code1[3] stax.tax-dscr1[3] stax.tax-rate1[3] stax.tax-frt1[3] ~
stax.tax-acc1[3] stax.tax-code1[4] stax.tax-dscr1[4] stax.tax-rate1[4] ~
stax.tax-frt1[4] stax.tax-acc1[4] stax.tax-code1[5] stax.tax-dscr1[5] ~
stax.tax-rate1[5] stax.tax-frt1[5] stax.tax-acc1[5] stax.accum-tax 
&Scoped-define ENABLED-TABLES stax
&Scoped-define FIRST-ENABLED-TABLE stax
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS stax.tax-group stax.tax-code1[1] ~
stax.tax-dscr1[1] stax.tax-rate1[1] stax.tax-frt1[1] stax.tax-acc1[1] ~
stax.tax-code1[2] stax.tax-dscr1[2] stax.tax-rate1[2] stax.tax-frt1[2] ~
stax.tax-acc1[2] stax.tax-code1[3] stax.tax-dscr1[3] stax.tax-rate1[3] ~
stax.tax-frt1[3] stax.tax-acc1[3] stax.tax-code1[4] stax.tax-dscr1[4] ~
stax.tax-rate1[4] stax.tax-frt1[4] stax.tax-acc1[4] stax.tax-code1[5] ~
stax.tax-dscr1[5] stax.tax-rate1[5] stax.tax-frt1[5] stax.tax-acc1[5] ~
stax.accum-tax 
&Scoped-define DISPLAYED-TABLES stax
&Scoped-define FIRST-DISPLAYED-TABLE stax
&Scoped-Define DISPLAYED-OBJECTS F1 F-2 F-3 F-5 F-4 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS stax.tax-group 
&Scoped-define ADM-ASSIGN-FIELDS stax.tax-group 
&Scoped-define F1 F1 F-2 F-3 F-5 F-4 

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
DEFINE VARIABLE F-2 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-3 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-4 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-5 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 122 BY 9.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     stax.tax-group AT ROW 2.91 COL 3 COLON-ALIGNED NO-LABEL FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 FONT 4
     stax.tax-code1[1] AT ROW 2.91 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 14 FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     stax.tax-dscr1[1] AT ROW 2.91 COL 28.4 COLON-ALIGNED NO-LABEL WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     stax.tax-rate1[1] AT ROW 2.91 COL 63 COLON-ALIGNED NO-LABEL WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     stax.tax-frt1[1] AT ROW 2.91 COL 80 WIDGET-ID 34
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 3 BY .81
     stax.tax-acc1[1] AT ROW 2.91 COL 89 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 27.2 BY 1
     stax.tax-code1[2] AT ROW 4.19 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 16 FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     stax.tax-dscr1[2] AT ROW 4.19 COL 28.4 COLON-ALIGNED NO-LABEL WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     stax.tax-rate1[2] AT ROW 4.19 COL 63 COLON-ALIGNED NO-LABEL WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     stax.tax-frt1[2] AT ROW 4.19 COL 80 WIDGET-ID 36
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 4 BY .81
     stax.tax-acc1[2] AT ROW 4.19 COL 89 COLON-ALIGNED NO-LABEL WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 27.2 BY 1
     stax.tax-code1[3] AT ROW 5.62 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 18 FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     stax.tax-dscr1[3] AT ROW 5.62 COL 28.4 COLON-ALIGNED NO-LABEL WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     stax.tax-rate1[3] AT ROW 5.62 COL 63 COLON-ALIGNED NO-LABEL WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     stax.tax-frt1[3] AT ROW 5.62 COL 80 WIDGET-ID 38
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 2 BY .81
     stax.tax-acc1[3] AT ROW 5.62 COL 89 COLON-ALIGNED NO-LABEL WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 27.2 BY 1
     stax.tax-code1[4] AT ROW 6.86 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 20 FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     stax.tax-dscr1[4] AT ROW 6.86 COL 28.4 COLON-ALIGNED NO-LABEL WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     stax.tax-rate1[4] AT ROW 6.86 COL 63 COLON-ALIGNED NO-LABEL WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     stax.tax-frt1[4] AT ROW 6.86 COL 80 WIDGET-ID 40
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 2 BY .81
     stax.tax-acc1[4] AT ROW 6.86 COL 89 COLON-ALIGNED NO-LABEL WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 27.2 BY 1
     stax.tax-code1[5] AT ROW 8.14 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 22 FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     stax.tax-dscr1[5] AT ROW 8.14 COL 28.4 COLON-ALIGNED NO-LABEL WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     stax.tax-rate1[5] AT ROW 8.14 COL 63 COLON-ALIGNED NO-LABEL WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     stax.tax-frt1[5] AT ROW 8.14 COL 80 WIDGET-ID 42
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 2 BY .81
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     stax.tax-acc1[5] AT ROW 8.14 COL 89 COLON-ALIGNED NO-LABEL WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 27.2 BY 1
     stax.accum-tax AT ROW 9.57 COL 19
          LABEL "Tax on Tax?"
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81
     F1 AT ROW 2.91 COL 118 NO-LABEL
     F-2 AT ROW 4.19 COL 118 NO-LABEL
     F-3 AT ROW 5.62 COL 118 NO-LABEL
     F-5 AT ROW 6.86 COL 118.2 NO-LABEL WIDGET-ID 56
     F-4 AT ROW 8.14 COL 118 NO-LABEL WIDGET-ID 54
     "Freight?" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.95 COL 78
     "Tax Group" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.95 COL 4
     "Sales Tax Account" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 1.48 COL 95
     "Tax" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.24 COL 78
     "Tax" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.24 COL 19
     "Description" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 1.48 COL 42
     "Sales" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.24 COL 4
     "Code" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.95 COL 19
     "Tax" VIEW-AS TEXT
          SIZE 6 BY .71 AT ROW 1.24 COL 66
     "Rate" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.95 COL 66
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.stax
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
         HEIGHT             = 9.67
         WIDTH              = 122.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX stax.accum-tax IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN F-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-3 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-3:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-4 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-4:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-5 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-5:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN stax.tax-code1[1] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN stax.tax-code1[2] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN stax.tax-code1[3] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN stax.tax-code1[4] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN stax.tax-code1[5] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR TOGGLE-BOX stax.tax-frt1[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX stax.tax-frt1[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX stax.tax-frt1[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX stax.tax-frt1[4] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX stax.tax-frt1[5] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN stax.tax-group IN FRAME F-Main
   NO-ENABLE 1 2 EXP-LABEL EXP-FORMAT                                   */
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

&Scoped-define SELF-NAME stax.tax-acc1[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[1] V-table-Win
ON HELP OF stax.tax-acc1[1] IN FRAME F-Main /* Sales Tax Account[1] */
DO:
  def var char-val as cha no-undo.
  run windows/l-acct.w (gcompany,"",self:screen-value,output char-val).
  if char-val <> "" then self:screen-value = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[1] V-table-Win
ON LEAVE OF stax.tax-acc1[1] IN FRAME F-Main /* Sales Tax Account[1] */
DO:
  {&methods/lValidateError.i YES}
  if lastkey <> -1 and stax.tax-code1[1]:screen-value <> "" then do:
     if not can-find(first account where account.company = gcompany and
                                         account.type <> "T" and
                                         account.actnum BEGINS self:screen-value)
     then do:
         message "Invalid Account. Account Type must not be 'T'. " view-as alert-box error.
         return no-apply.
     end.
  end.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-acc1[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[2] V-table-Win
ON HELP OF stax.tax-acc1[2] IN FRAME F-Main /* Sales Tax Account[2] */
DO:
  def var char-val as cha no-undo.
  run windows/l-acct.w (gcompany,"",self:screen-value,output char-val).
  if char-val <> "" then self:screen-value = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[2] V-table-Win
ON LEAVE OF stax.tax-acc1[2] IN FRAME F-Main /* Sales Tax Account[2] */
DO:
  {&methods/lValidateError.i YES}
  if lastkey <> -1 and stax.tax-code1[2]:screen-value <> "" then do:
     if not can-find(first account where account.company = gcompany and
                                         account.type <> "T" and
                                         account.actnum BEGINS self:screen-value)
     then do:
         message "Invalid Account. Account Type must not be 'T'. " view-as alert-box error.
         return no-apply.
     end.
  end.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-acc1[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[3] V-table-Win
ON HELP OF stax.tax-acc1[3] IN FRAME F-Main /* Sales Tax Account[3] */
DO:
  def var char-val as cha no-undo.
  run windows/l-acct.w (gcompany,"",self:screen-value,output char-val).
  if char-val <> "" then self:screen-value = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[3] V-table-Win
ON LEAVE OF stax.tax-acc1[3] IN FRAME F-Main /* Sales Tax Account[3] */
DO:
  {&methods/lValidateError.i YES}
  if lastkey <> -1 and stax.tax-code1[3]:screen-value <> "" then do:
     if not can-find(first account where account.company = gcompany and
                                         account.type <> "T" and
                                         account.actnum BEGINS self:screen-value)
     then do:
         message "Invalid Account. Account Type must not be 'T'. " view-as alert-box error.
         return no-apply.
     end.
  end.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-acc1[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[4] V-table-Win
ON HELP OF stax.tax-acc1[4] IN FRAME F-Main /* Sales Tax Account[4] */
DO:
  def var char-val as cha no-undo.
  run windows/l-acct.w (gcompany,"",self:screen-value,output char-val).
  if char-val <> "" then self:screen-value = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[4] V-table-Win
ON LEAVE OF stax.tax-acc1[4] IN FRAME F-Main /* Sales Tax Account[4] */
DO:
  {&methods/lValidateError.i YES}
  if lastkey <> -1 and stax.tax-code1[4]:screen-value <> "" then do:
     if not can-find(first account where account.company = gcompany and
                                         account.type <> "T" and
                                         account.actnum BEGINS self:screen-value)
     then do:
         message "Invalid Account. Account Type must not be 'T'. " view-as alert-box error.
         return no-apply.
     end.
  end.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-acc1[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[5] V-table-Win
ON HELP OF stax.tax-acc1[5] IN FRAME F-Main /* Sales Tax Account[5] */
DO:
  def var char-val as cha no-undo.
  run windows/l-acct.w (gcompany,"",self:screen-value,output char-val).
  if char-val <> "" then self:screen-value = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[5] V-table-Win
ON LEAVE OF stax.tax-acc1[5] IN FRAME F-Main /* Sales Tax Account[5] */
DO:
  {&methods/lValidateError.i YES}
  if lastkey <> -1 and stax.tax-code1[5]:screen-value <> "" then do:
     if not can-find(first account where account.company = gcompany and
                                         account.type <> "T" and
                                         account.actnum BEGINS self:screen-value)
     then do:
         message "Invalid Account. Account Type must not be 'T'. " view-as alert-box error.
         return no-apply.
     end.
  end.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-code1[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[1] V-table-Win
ON HELP OF stax.tax-code1[1] IN FRAME F-Main /* Tax Code */
DO:
  {viewers/stax2.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[1] V-table-Win
ON LEAVE OF stax.tax-code1[1] IN FRAME F-Main /* Tax Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-code IN THIS-PROCEDURE (SELF:SCREEN-VALUE, SELF) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[1] V-table-Win
ON VALUE-CHANGED OF stax.tax-code1[1] IN FRAME F-Main /* Tax Code */
DO:
  {viewers/stax.i 1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-code1[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[2] V-table-Win
ON HELP OF stax.tax-code1[2] IN FRAME F-Main /* Tax Code */
DO:
  {viewers/stax2.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[2] V-table-Win
ON LEAVE OF stax.tax-code1[2] IN FRAME F-Main /* Tax Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-code IN THIS-PROCEDURE (SELF:SCREEN-VALUE, SELF) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[2] V-table-Win
ON VALUE-CHANGED OF stax.tax-code1[2] IN FRAME F-Main /* Tax Code */
DO:
  {viewers/stax.i 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-code1[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[3] V-table-Win
ON HELP OF stax.tax-code1[3] IN FRAME F-Main /* Tax Code */
DO:
  {viewers/stax2.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[3] V-table-Win
ON LEAVE OF stax.tax-code1[3] IN FRAME F-Main /* Tax Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-code IN THIS-PROCEDURE (SELF:SCREEN-VALUE, SELF) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[3] V-table-Win
ON VALUE-CHANGED OF stax.tax-code1[3] IN FRAME F-Main /* Tax Code */
DO:
  {viewers/stax.i 3}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-code1[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[4] V-table-Win
ON HELP OF stax.tax-code1[4] IN FRAME F-Main /* Tax Code */
DO:
  {viewers/stax2.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[4] V-table-Win
ON LEAVE OF stax.tax-code1[4] IN FRAME F-Main /* Tax Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-code IN THIS-PROCEDURE (SELF:SCREEN-VALUE, SELF) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[4] V-table-Win
ON VALUE-CHANGED OF stax.tax-code1[4] IN FRAME F-Main /* Tax Code */
DO:
  {viewers/stax.i 4}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-code1[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[5] V-table-Win
ON HELP OF stax.tax-code1[5] IN FRAME F-Main /* Tax Code */
DO:
  {viewers/stax2.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[5] V-table-Win
ON LEAVE OF stax.tax-code1[5] IN FRAME F-Main /* Tax Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-code IN THIS-PROCEDURE (SELF:SCREEN-VALUE, SELF) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[5] V-table-Win
ON VALUE-CHANGED OF stax.tax-code1[5] IN FRAME F-Main /* Tax Code */
DO:
  {viewers/stax.i 5}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-group
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-group V-table-Win
ON LEAVE OF stax.tax-group IN FRAME F-Main /* Sales Tax Group */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-group NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}

ASSIGN
 cocode = gcompany
 locode = gloc.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF         

/************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item V-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN dispatch ('add-record').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "stax"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "stax"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var ls-tax-group as cha no-undo.
  /* Code placed here will execute PRIOR to standard behavior. */
   ls-tax-group = stax.tax-group:screen-value in frame {&frame-name}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 /* if adm-new-record then 
     stax.tax-group = string(gcompany,"x(10)") + trim(ls-tax-group).
 */ 

   find first stax-group where stax-group.company = stax.company and
                               stax-group.tax-group = stax.tax-group
                               no-lock no-error.
   if not avail stax-group then do:
      create stax-group.
      assign stax-group.company = stax.company
             stax-group.tax-group = stax.tax-group
             stax-group.tax-dscr = stax.tax-group.

   end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/create/stax.i}  /* assign stax.company = gcompany */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE frame-handle AS HANDLE NO-UNDO.
  DEFINE VARIABLE group-handle AS HANDLE NO-UNDO.
  DEFINE VARIABLE field-handle AS HANDLE NO-UNDO.

  ASSIGN frame-handle = FRAME {&FRAME-NAME}:HANDLE
         group-handle = frame-handle:FIRST-CHILD
         field-handle = group-handle:FIRST-CHILD.
  EMPTY TEMP-TABLE tt-accounts.

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-tax-group NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /* validate tax codes and accounts */
  DO WHILE VALID-HANDLE(field-handle):
      IF field-handle:NAME MATCHES "*tax-code1*" THEN DO:
          RUN valid-tax-code IN THIS-PROCEDURE (field-handle:SCREEN-VALUE, field-handle) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
      END.
      /* setup for validating accounts against tax codes */
      IF field-handle:INDEX > 0 AND CAN-DO("*tax-code1*,*tax-acc1*",field-handle:NAME) THEN DO:
          FIND FIRST tt-accounts WHERE tt-accounts.i-extent = field-handle:INDEX NO-ERROR.
          IF NOT AVAILABLE tt-accounts THEN DO:
              CREATE tt-accounts.
              ASSIGN tt-accounts.i-extent = field-handle:INDEX.
          END.
          IF field-handle:NAME MATCHES "*tax-code1*" THEN ASSIGN
             c-code = field-handle:SCREEN-VALUE.
          IF field-handle:NAME MATCHES "*tax-acc1*" THEN ASSIGN
              c-acct = field-handle:SCREEN-VALUE.
              h-acct = field-handle:HANDLE.
      END. /* indexed fields */
      field-handle = field-handle:NEXT-SIBLING.
  END.
  {&methods/lValidateError.i YES}
  /* now check the accounts against the tax codes */
  FOR EACH tt-accounts WHERE tt-accounts.c-code <> "":
      if not can-find(first account where account.company = gcompany and
                                          account.type <> "T" and
                                          account.actnum = tt-accounts.c-acct)
      then do:
          message "Invalid Account." view-as alert-box error.
          apply "entry" to tt-accounts.h-acct.
          return no-apply.
      end.
  END.
  {&methods/lValidateError.i NO}
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  {src/adm/template/snd-list.i "stax"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tax-code V-table-Win 
PROCEDURE valid-tax-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-value  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-widget AS HANDLE    NO-UNDO.

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:

    ip-widget:SCREEN-VALUE = CAPS(ip-value).

    IF  ip-value NE "" 
    AND ip-value NE stax.tax-group:SCREEN-VALUE 
    AND NOT CAN-FIND(FIRST b-stax
                  WHERE b-stax.company   EQ cocode
                    AND b-stax.tax-group EQ ip-value
                    AND ROWID(b-stax)    NE ROWID(stax))          
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-widget.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tax-group V-table-Win 
PROCEDURE valid-tax-group :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/ 
  DEFINE BUFFER b-stax FOR stax.

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    stax.tax-group:SCREEN-VALUE = CAPS(stax.tax-group:SCREEN-VALUE).

    FOR EACH b-stax NO-LOCK
       WHERE b-stax.company EQ cocode
         AND b-stax.tax-group EQ stax.tax-group:SCREEN-VALUE:
        IF ROWID(b-stax) <> ROWID(stax)
        OR (NEW stax AND ROWID(stax) = ?) THEN DO:
            MESSAGE "Sorry, Tax Group already exists..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO stax.tax-group.
            RETURN ERROR.
        END.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

