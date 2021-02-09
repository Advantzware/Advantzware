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
    
DEFINE VARIABLE hGLProcs AS HANDLE NO-UNDO.

RUN system/GLProcs.p PERSISTENT SET hGLProcs.    

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
stax.tax-rate1[1] stax.tax-acc1[1] stax.taxableLimit stax.tax-frt1[1] ~
stax.tax-code1[2] stax.tax-dscr1[2] stax.tax-rate1[2] stax.tax-acc1[2] ~
stax.tax-code1[3] stax.tax-dscr1[3] stax.tax-rate1[3] stax.tax-acc1[3] ~
stax.tax-code1[4] stax.tax-dscr1[4] stax.tax-rate1[4] stax.tax-acc1[4] ~
stax.tax-code1[5] stax.tax-dscr1[5] stax.tax-rate1[5] stax.tax-acc1[5] ~
stax.tax-code1[6] stax.tax-dscr1[6] stax.tax-rate1[6] stax.tax-acc1[6] ~
stax.tax-code1[7] stax.tax-dscr1[7] stax.tax-rate1[7] stax.tax-acc1[7] ~
stax.tax-code1[8] stax.tax-dscr1[8] stax.tax-rate1[8] stax.tax-acc1[8] ~
stax.tax-rate1[9] stax.tax-code1[9] stax.tax-dscr1[9] stax.tax-acc1[9] ~
stax.tax-code1[10] stax.tax-dscr1[10] stax.tax-rate1[10] stax.tax-acc1[10] ~
stax.accum-tax stax.inactive 
&Scoped-define ENABLED-TABLES stax
&Scoped-define FIRST-ENABLED-TABLE stax
&Scoped-Define DISPLAYED-FIELDS stax.tax-group stax.tax-code1[1] ~
stax.tax-dscr1[1] stax.tax-rate1[1] stax.tax-acc1[1] stax.taxableLimit ~
stax.tax-frt1[1] stax.tax-code1[2] stax.tax-dscr1[2] stax.tax-rate1[2] ~
stax.tax-acc1[2] stax.tax-code1[3] stax.tax-dscr1[3] stax.tax-rate1[3] ~
stax.tax-acc1[3] stax.tax-code1[4] stax.tax-dscr1[4] stax.tax-rate1[4] ~
stax.tax-acc1[4] stax.tax-code1[5] stax.tax-dscr1[5] stax.tax-rate1[5] ~
stax.tax-acc1[5] stax.tax-code1[6] stax.tax-dscr1[6] stax.tax-rate1[6] ~
stax.tax-acc1[6] stax.tax-code1[7] stax.tax-dscr1[7] stax.tax-rate1[7] ~
stax.tax-acc1[7] stax.tax-code1[8] stax.tax-dscr1[8] stax.tax-rate1[8] ~
stax.tax-acc1[8] stax.tax-rate1[9] stax.tax-code1[9] stax.tax-dscr1[9] ~
stax.tax-acc1[9] stax.tax-code1[10] stax.tax-dscr1[10] stax.tax-rate1[10] ~
stax.tax-acc1[10] stax.accum-tax stax.inactive 
&Scoped-define DISPLAYED-TABLES stax
&Scoped-define FIRST-DISPLAYED-TABLE stax


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS stax.tax-group 
&Scoped-define ADM-ASSIGN-FIELDS stax.tax-group 

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
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 142 BY 15.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     stax.tax-group AT ROW 2.91 COL 3 COLON-ALIGNED NO-LABEL FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 FONT 4
     stax.tax-code1[1] AT ROW 2.91 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 14 FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 
     stax.tax-dscr1[1] AT ROW 2.91 COL 28.4 COLON-ALIGNED NO-LABEL WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     stax.tax-rate1[1] AT ROW 2.91 COL 63 COLON-ALIGNED NO-LABEL WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     stax.tax-acc1[1] AT ROW 2.91 COL 76 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 27.2 BY 1
          BGCOLOR 15 
     stax.taxableLimit AT ROW 2.91 COL 121.2 COLON-ALIGNED NO-LABEL WIDGET-ID 62
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
          BGCOLOR 15 
     stax.tax-frt1[1] AT ROW 3 COL 114 WIDGET-ID 34
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 4 BY .81
     stax.tax-code1[2] AT ROW 4.19 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 16 FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 
     stax.tax-dscr1[2] AT ROW 4.19 COL 28.4 COLON-ALIGNED NO-LABEL WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     stax.tax-rate1[2] AT ROW 4.19 COL 63 COLON-ALIGNED NO-LABEL WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     stax.tax-acc1[2] AT ROW 4.19 COL 76 COLON-ALIGNED NO-LABEL WIDGET-ID 96
          VIEW-AS FILL-IN 
          SIZE 27.2 BY 1
          BGCOLOR 15 
     stax.tax-code1[3] AT ROW 5.29 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 18 FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 
     stax.tax-dscr1[3] AT ROW 5.29 COL 28.4 COLON-ALIGNED NO-LABEL WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     stax.tax-rate1[3] AT ROW 5.29 COL 63 COLON-ALIGNED NO-LABEL WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     stax.tax-acc1[3] AT ROW 5.29 COL 76 COLON-ALIGNED NO-LABEL WIDGET-ID 98
          VIEW-AS FILL-IN 
          SIZE 27.2 BY 1
          BGCOLOR 15 
     stax.tax-code1[4] AT ROW 6.52 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 20 FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 
     stax.tax-dscr1[4] AT ROW 6.52 COL 28.4 COLON-ALIGNED NO-LABEL WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     stax.tax-rate1[4] AT ROW 6.52 COL 63 COLON-ALIGNED NO-LABEL WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     stax.tax-acc1[4] AT ROW 6.52 COL 76 COLON-ALIGNED NO-LABEL WIDGET-ID 100
          VIEW-AS FILL-IN 
          SIZE 27.2 BY 1
          BGCOLOR 15 
     stax.tax-code1[5] AT ROW 7.81 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 22 FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 
     stax.tax-dscr1[5] AT ROW 7.81 COL 28.4 COLON-ALIGNED NO-LABEL WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     stax.tax-rate1[5] AT ROW 7.81 COL 63 COLON-ALIGNED NO-LABEL WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     stax.tax-acc1[5] AT ROW 7.81 COL 76 COLON-ALIGNED NO-LABEL WIDGET-ID 102
          VIEW-AS FILL-IN 
          SIZE 27.2 BY 1
          BGCOLOR 15 
     stax.tax-code1[6] AT ROW 9.05 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 68
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 
     stax.tax-dscr1[6] AT ROW 9.1 COL 28.4 COLON-ALIGNED NO-LABEL WIDGET-ID 78
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     stax.tax-rate1[6] AT ROW 9.1 COL 63 COLON-ALIGNED NO-LABEL WIDGET-ID 88
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     stax.tax-acc1[6] AT ROW 9.1 COL 76 COLON-ALIGNED NO-LABEL WIDGET-ID 104
          VIEW-AS FILL-IN 
          SIZE 27.2 BY 1
          BGCOLOR 15 
     stax.tax-code1[7] AT ROW 10.29 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 70
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 
     stax.tax-dscr1[7] AT ROW 10.29 COL 28.6 COLON-ALIGNED NO-LABEL WIDGET-ID 80
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     stax.tax-rate1[7] AT ROW 10.29 COL 63 COLON-ALIGNED NO-LABEL WIDGET-ID 90
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     stax.tax-acc1[7] AT ROW 10.29 COL 76 COLON-ALIGNED NO-LABEL WIDGET-ID 106
          VIEW-AS FILL-IN 
          SIZE 27.2 BY 1
          BGCOLOR 15 
     stax.tax-code1[8] AT ROW 11.52 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 72
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 
     stax.tax-dscr1[8] AT ROW 11.52 COL 28.6 COLON-ALIGNED NO-LABEL WIDGET-ID 82
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     stax.tax-rate1[8] AT ROW 11.52 COL 63 COLON-ALIGNED NO-LABEL WIDGET-ID 92
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     stax.tax-acc1[8] AT ROW 11.52 COL 76 COLON-ALIGNED NO-LABEL WIDGET-ID 108
          VIEW-AS FILL-IN 
          SIZE 27.2 BY 1
          BGCOLOR 15 
     stax.tax-rate1[9] AT ROW 12.71 COL 63 COLON-ALIGNED NO-LABEL WIDGET-ID 94
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     stax.tax-code1[9] AT ROW 12.76 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 74
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 
     stax.tax-dscr1[9] AT ROW 12.76 COL 28.6 COLON-ALIGNED NO-LABEL WIDGET-ID 84
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     stax.tax-acc1[9] AT ROW 12.76 COL 76 COLON-ALIGNED NO-LABEL WIDGET-ID 110
          VIEW-AS FILL-IN 
          SIZE 27.2 BY 1
          BGCOLOR 15 
     stax.tax-code1[10] AT ROW 13.95 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 66
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 
     stax.tax-dscr1[10] AT ROW 14 COL 28.6 COLON-ALIGNED NO-LABEL WIDGET-ID 76
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     stax.tax-rate1[10] AT ROW 14 COL 63 COLON-ALIGNED NO-LABEL WIDGET-ID 86
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     stax.tax-acc1[10] AT ROW 14 COL 76 COLON-ALIGNED NO-LABEL WIDGET-ID 112
          VIEW-AS FILL-IN 
          SIZE 27.2 BY 1
          BGCOLOR 15 
     stax.accum-tax AT ROW 15.29 COL 19
          LABEL "Tax on Tax?"
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81
     stax.inactive AT ROW 15.29 COL 78 WIDGET-ID 60
          VIEW-AS TOGGLE-BOX
          SIZE 13.2 BY .81
     "Freight?" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 2.05 COL 112
     "Sales" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.24 COL 4
     "Tax" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.33 COL 112
     "$ Limit" VIEW-AS TEXT
          SIZE 8.8 BY .62 AT ROW 1.48 COL 123.2 WIDGET-ID 64
     "Tax Group" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.95 COL 4
     "Tax" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.24 COL 19
     "Description" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 1.48 COL 42
     "GL Account" VIEW-AS TEXT
          SIZE 14.8 BY .62 AT ROW 1.48 COL 83.2
     "Tax" VIEW-AS TEXT
          SIZE 6 BY .71 AT ROW 1.24 COL 66
     "Code" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.95 COL 19
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
         HEIGHT             = 15.24
         WIDTH              = 142.
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
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME stax.tax-acc1[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[10] V-table-Win
ON LEAVE OF stax.tax-acc1[10] IN FRAME F-Main /* Sales Tax Account[10] */
DO:
    DEFINE VARIABLE lError    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lInactive AS LOGICAL NO-UNDO.
    
    {&methods/lValidateError.i YES}
    IF LASTKEY EQ -1 THEN RETURN.
  
    IF stax.tax-code1[10]:SCREEN-VALUE NE "" THEN DO:
        RUN pvalidGLAccount(INPUT stax.tax-acc1[10]:SCREEN-VALUE, OUTPUT lError, OUTPUT lInactive).
        
        IF lError THEN DO:            
            RUN presetColor(INPUT stax.tax-acc1[10]:HANDLE).
            IF lInactive EQ YES THEN 
                RUN pchangeColor(INPUT INPUT stax.tax-acc1[10]:HANDLE) NO-ERROR.
            APPLY "ENTRY" TO stax.tax-acc1[10].
            RETURN NO-APPLY.
        END.    
    END.           
    ELSE DO:
      IF stax.tax-acc1[10]:SCREEN-VALUE NE "" THEN DO: 
          MESSAGE "First enter corresponding tax-code." VIEW-AS ALERT-BOX INFORMATION.
          SELF:SCREEN-VALUE = "".
          APPLY "ENTRY" TO stax.tax-code1[10].
          RETURN NO-APPLY.         
      END.    
    END. 
    RUN presetColor(INPUT stax.tax-acc1[10]:HANDLE).
   {&methods/lValidateError.i NO}
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-acc1[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[1] V-table-Win
ON LEAVE OF stax.tax-acc1[1] IN FRAME F-Main /* Sales Tax Account[1] */
DO:
    DEFINE VARIABLE lError    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lInactive AS LOGICAL NO-UNDO.
    
    {&methods/lValidateError.i YES}
    IF LASTKEY EQ -1 THEN RETURN.
  
    IF stax.tax-code1[1]:SCREEN-VALUE NE "" THEN DO:
        RUN pvalidGLAccount(INPUT stax.tax-acc1[1]:SCREEN-VALUE, OUTPUT lError, OUTPUT lInactive).
      
        IF lError THEN DO:            
            RUN presetColor(INPUT stax.tax-acc1[1]:HANDLE).
            IF lInactive EQ YES THEN 
                RUN pchangeColor(INPUT stax.tax-acc1[1]:HANDLE) NO-ERROR.
            APPLY "ENTRY" TO stax.tax-acc1[1].
            RETURN NO-APPLY.
        END.    
    END.           
    ELSE DO:
      IF stax.tax-acc1[1]:SCREEN-VALUE NE "" THEN DO: 
          MESSAGE "First enter corresponding tax-code." VIEW-AS ALERT-BOX INFORMATION.
          SELF:SCREEN-VALUE = "".
          APPLY "ENTRY" TO stax.tax-code1[1].
          RETURN NO-APPLY.         
      END.    
    END. 
    RUN presetColor(INPUT stax.tax-acc1[1]:HANDLE). 
   {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-acc1[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[2] V-table-Win
ON LEAVE OF stax.tax-acc1[2] IN FRAME F-Main /* Sales Tax Account[2] */
DO:
    DEFINE VARIABLE lError    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lInactive AS LOGICAL NO-UNDO.
    
    {&methods/lValidateError.i YES}
    IF LASTKEY EQ -1 THEN RETURN.
  
    IF stax.tax-code1[2]:SCREEN-VALUE NE "" THEN DO:
        RUN pvalidGLAccount(INPUT stax.tax-acc1[2]:SCREEN-VALUE, OUTPUT lError, OUTPUT lInactive).
       
        IF lError THEN DO:            
            RUN presetColor(INPUT stax.tax-acc1[2]:HANDLE).
            IF lInactive EQ YES THEN 
                RUN pchangeColor(INPUT stax.tax-acc1[2]:HANDLE) NO-ERROR.
            APPLY "ENTRY" TO stax.tax-acc1[2].
            RETURN NO-APPLY.
        END.    
    END.           
    ELSE DO:
      IF stax.tax-acc1[2]:SCREEN-VALUE NE "" THEN DO: 
          MESSAGE "First enter corresponding tax-code." VIEW-AS ALERT-BOX INFORMATION.
          SELF:SCREEN-VALUE = "".
          APPLY "ENTRY" TO stax.tax-code1[2].
          RETURN NO-APPLY.         
      END.    
    END. 
    RUN presetColor(INPUT stax.tax-acc1[2]:HANDLE).
   {&methods/lValidateError.i NO}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-acc1[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[3] V-table-Win
ON LEAVE OF stax.tax-acc1[3] IN FRAME F-Main /* Sales Tax Account[3] */
DO:
    DEFINE VARIABLE lError    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lInactive AS LOGICAL NO-UNDO.
    
    {&methods/lValidateError.i YES}
    IF LASTKEY EQ -1 THEN RETURN.
  
    IF stax.tax-code1[3]:SCREEN-VALUE NE "" THEN DO:
        RUN pvalidGLAccount(INPUT stax.tax-acc1[3]:SCREEN-VALUE, OUTPUT lError, OUTPUT lInactive) NO-ERROR.
        
        IF lError THEN DO:            
            RUN presetColor(INPUT stax.tax-acc1[3]:HANDLE) NO-ERROR.
            IF lInactive EQ YES THEN 
                RUN pchangeColor(INPUT stax.tax-acc1[3]:HANDLE) NO-ERROR.
            APPLY "ENTRY" TO stax.tax-acc1[3].
            RETURN NO-APPLY.
        END.    
    END.           
    ELSE DO:
      IF stax.tax-acc1[3]:SCREEN-VALUE NE "" THEN DO: 
          MESSAGE "First enter corresponding tax-code." VIEW-AS ALERT-BOX INFORMATION.
          SELF:SCREEN-VALUE = "".
          APPLY "ENTRY" TO stax.tax-code1[3].
          RETURN NO-APPLY.         
      END.    
    END. 
    RUN presetColor(INPUT stax.tax-acc1[3]:HANDLE) NO-ERROR.
   {&methods/lValidateError.i NO}
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-acc1[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[4] V-table-Win
ON LEAVE OF stax.tax-acc1[4] IN FRAME F-Main /* Sales Tax Account[4] */
DO:
    DEFINE VARIABLE lError    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lInactive AS LOGICAL NO-UNDO.
    
    {&methods/lValidateError.i YES}
    IF LASTKEY EQ -1 THEN RETURN.
  
    IF stax.tax-code1[4]:SCREEN-VALUE <> "" THEN DO:
        RUN pvalidGLAccount(INPUT stax.tax-acc1[4]:SCREEN-VALUE, OUTPUT lError, OUTPUT lInactive).
        
        IF lError THEN DO:            
            RUN presetColor(INPUT stax.tax-acc1[4]:HANDLE) NO-ERROR.
            IF lInactive EQ YES THEN 
                RUN pchangeColor(INPUT stax.tax-acc1[4]:HANDLE) NO-ERROR.
            APPLY "ENTRY" TO stax.tax-acc1[4].
            RETURN NO-APPLY.
        END.    
    END.            
    ELSE DO:
      IF stax.tax-acc1[4]:SCREEN-VALUE NE "" THEN DO: 
          MESSAGE "First enter corresponding tax-code." VIEW-AS ALERT-BOX INFORMATION.
          SELF:SCREEN-VALUE = "".
          APPLY "ENTRY" TO stax.tax-code1[4].
          RETURN NO-APPLY.         
      END.    
    END. 
    RUN presetColor(INPUT stax.tax-acc1[4]:HANDLE) NO-ERROR.
   {&methods/lValidateError.i NO}
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-acc1[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[5] V-table-Win
ON LEAVE OF stax.tax-acc1[5] IN FRAME F-Main /* Sales Tax Account[5] */
DO:
    DEFINE VARIABLE lError    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lInactive AS LOGICAL NO-UNDO.
    
    {&methods/lValidateError.i YES}
    IF LASTKEY EQ -1 THEN RETURN.
  
    IF stax.tax-code1[5]:SCREEN-VALUE NE "" THEN DO:
        RUN pvalidGLAccount(INPUT stax.tax-acc1[5]:SCREEN-VALUE, OUTPUT lError, OUTPUT lInactive).
       
        IF lError THEN DO:            
            RUN presetColor(INPUT stax.tax-acc1[5]:HANDLE) NO-ERROR.
            IF lInactive EQ YES THEN 
                RUN pchangeColor(INPUT stax.tax-acc1[5]:HANDLE) NO-ERROR.
            APPLY "ENTRY" TO stax.tax-acc1[5].
            RETURN NO-APPLY.
        END.    
    END.           
    ELSE DO:
      IF stax.tax-acc1[5]:SCREEN-VALUE NE "" THEN DO: 
          MESSAGE "First enter corresponding tax-code." VIEW-AS ALERT-BOX INFORMATION.
          SELF:SCREEN-VALUE = "".
          APPLY "ENTRY" TO stax.tax-code1[5].
          RETURN NO-APPLY.         
      END.    
    END. 
    RUN presetColor(INPUT stax.tax-acc1[5]:HANDLE) NO-ERROR. 
   {&methods/lValidateError.i NO}
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-acc1[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[6] V-table-Win
ON LEAVE OF stax.tax-acc1[6] IN FRAME F-Main /* Sales Tax Account[6] */
DO:
    DEFINE VARIABLE lError    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lInactive AS LOGICAL NO-UNDO.
    
    {&methods/lValidateError.i YES}
    IF LASTKEY EQ -1 THEN RETURN.
  
    IF stax.tax-code1[6]:SCREEN-VALUE NE "" THEN DO:
        RUN pvalidGLAccount(INPUT stax.tax-acc1[6]:SCREEN-VALUE, OUTPUT lError, OUTPUT lInactive).
        
        IF lError THEN DO:            
            RUN presetColor(INPUT stax.tax-acc1[6]:HANDLE) NO-ERROR.
            IF lInactive EQ YES THEN 
                RUN pchangeColor(INPUT stax.tax-acc1[6]:HANDLE) NO-ERROR.
            APPLY "ENTRY" TO stax.tax-acc1[6].
            RETURN NO-APPLY.
        END.    
    END.           
    ELSE DO:
      IF stax.tax-acc1[6]:SCREEN-VALUE NE "" THEN DO: 
          MESSAGE "First enter corresponding tax-code." VIEW-AS ALERT-BOX INFORMATION.
          SELF:SCREEN-VALUE = "".
          APPLY "ENTRY" TO stax.tax-code1[6].
          RETURN NO-APPLY.         
      END.    
    END. 
    RUN presetColor(INPUT stax.tax-acc1[6]:HANDLE) NO-ERROR. 
   {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-acc1[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[7] V-table-Win
ON LEAVE OF stax.tax-acc1[7] IN FRAME F-Main /* Sales Tax Account[7] */
DO:
    DEFINE VARIABLE lError    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lInactive AS LOGICAL NO-UNDO.
    
    {&methods/lValidateError.i YES}
    IF LASTKEY EQ -1 THEN RETURN.
  
    IF stax.tax-code1[7]:SCREEN-VALUE NE "" THEN DO:
        RUN pvalidGLAccount(INPUT stax.tax-acc1[7]:SCREEN-VALUE, OUTPUT lError, OUTPUT lInactive).
        
        IF lError THEN DO:            
            RUN presetColor(INPUT stax.tax-acc1[7]:HANDLE) NO-ERROR.
            IF lInactive EQ YES THEN 
                RUN pchangeColor(INPUT stax.tax-acc1[7]:HANDLE) NO-ERROR.
            APPLY "ENTRY" TO stax.tax-acc1[7].
            RETURN NO-APPLY.
        END.    
    END.           
    ELSE DO:
      IF stax.tax-acc1[7]:SCREEN-VALUE NE "" THEN DO: 
          MESSAGE "First enter corresponding tax-code." VIEW-AS ALERT-BOX INFORMATION.
          SELF:SCREEN-VALUE = "".
          APPLY "ENTRY" TO stax.tax-code1[7].
          RETURN NO-APPLY.         
      END.    
    END. 
    RUN presetColor(INPUT stax.tax-acc1[7]:HANDLE) NO-ERROR.
   {&methods/lValidateError.i NO}
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-acc1[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[8] V-table-Win
ON LEAVE OF stax.tax-acc1[8] IN FRAME F-Main /* Sales Tax Account[8] */
DO:
    DEFINE VARIABLE lError    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lInactive AS LOGICAL NO-UNDO.
    
    {&methods/lValidateError.i YES}
    IF LASTKEY EQ -1 THEN RETURN.
  
    IF stax.tax-code1[8]:SCREEN-VALUE NE "" THEN DO:
        RUN pvalidGLAccount(INPUT stax.tax-acc1[8]:SCREEN-VALUE, OUTPUT lError, OUTPUT lInactive).
        
        IF lError THEN DO:            
            RUN presetColor(INPUT stax.tax-acc1[8]:HANDLE) NO-ERROR.
            IF lInactive EQ YES THEN 
                RUN pchangeColor(INPUT stax.tax-acc1[8]:HANDLE) NO-ERROR.
            APPLY "ENTRY" TO stax.tax-acc1[8].
            RETURN NO-APPLY.
        END.    
    END.           
    ELSE DO:
      IF stax.tax-acc1[8]:SCREEN-VALUE NE "" THEN DO: 
          MESSAGE "First enter corresponding tax-code." VIEW-AS ALERT-BOX INFORMATION.
          SELF:SCREEN-VALUE = "".
          APPLY "ENTRY" TO stax.tax-code1[8].
          RETURN NO-APPLY.         
      END.    
    END. 
    RUN presetColor(INPUT stax.tax-acc1[8]:HANDLE) NO-ERROR.
   {&methods/lValidateError.i NO}
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-acc1[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-acc1[9] V-table-Win
ON LEAVE OF stax.tax-acc1[9] IN FRAME F-Main /* Sales Tax Account[9] */
DO:
    DEFINE VARIABLE lError    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lInactive AS LOGICAL NO-UNDO.
    
    {&methods/lValidateError.i YES}
    IF LASTKEY EQ -1 THEN RETURN.
  
    IF stax.tax-code1[9]:SCREEN-VALUE NE "" THEN DO:
        RUN pvalidGLAccount(INPUT stax.tax-acc1[9]:SCREEN-VALUE, OUTPUT lError, OUTPUT lInactive).
        
        IF lError THEN DO:            
            RUN presetColor(INPUT stax.tax-acc1[9]:HANDLE) NO-ERROR.
            IF lInactive EQ YES THEN 
                RUN pchangeColor(INPUT stax.tax-acc1[9]:HANDLE) NO-ERROR.
            APPLY "ENTRY" TO stax.tax-acc1[9].
            RETURN NO-APPLY.
        END.    
    END.           
    ELSE DO:
      IF stax.tax-acc1[9]:SCREEN-VALUE NE "" THEN DO: 
          MESSAGE "First enter corresponding tax-code." VIEW-AS ALERT-BOX INFORMATION.
          SELF:SCREEN-VALUE = "".
          APPLY "ENTRY" TO stax.tax-code1[9].
          RETURN NO-APPLY.         
      END.    
    END. 
   RUN presetColor(INPUT stax.tax-acc1[9]:HANDLE) NO-ERROR.
   {&methods/lValidateError.i NO}
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-code1[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[10] V-table-Win
ON HELP OF stax.tax-code1[10] IN FRAME F-Main /* Tax Code[10] */
DO:
  {viewers/stax2.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[10] V-table-Win
ON LEAVE OF stax.tax-code1[10] IN FRAME F-Main /* Tax Code[10] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-code IN THIS-PROCEDURE (SELF:SCREEN-VALUE, SELF) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[10] V-table-Win
ON VALUE-CHANGED OF stax.tax-code1[10] IN FRAME F-Main /* Tax Code[10] */
DO:
  {viewers/stax.i 10}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-code1[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[1] V-table-Win
ON HELP OF stax.tax-code1[1] IN FRAME F-Main /* Tax Code[1] */
DO:
  {viewers/stax2.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[1] V-table-Win
ON LEAVE OF stax.tax-code1[1] IN FRAME F-Main /* Tax Code[1] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-code IN THIS-PROCEDURE (SELF:SCREEN-VALUE, SELF) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[1] V-table-Win
ON VALUE-CHANGED OF stax.tax-code1[1] IN FRAME F-Main /* Tax Code[1] */
DO:
  {viewers/stax.i 1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-code1[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[2] V-table-Win
ON HELP OF stax.tax-code1[2] IN FRAME F-Main /* Tax Code[2] */
DO:
  {viewers/stax2.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[2] V-table-Win
ON LEAVE OF stax.tax-code1[2] IN FRAME F-Main /* Tax Code[2] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-code IN THIS-PROCEDURE (SELF:SCREEN-VALUE, SELF) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[2] V-table-Win
ON VALUE-CHANGED OF stax.tax-code1[2] IN FRAME F-Main /* Tax Code[2] */
DO:
  {viewers/stax.i 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-code1[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[3] V-table-Win
ON HELP OF stax.tax-code1[3] IN FRAME F-Main /* Tax Code[3] */
DO:
  {viewers/stax2.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[3] V-table-Win
ON LEAVE OF stax.tax-code1[3] IN FRAME F-Main /* Tax Code[3] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-code IN THIS-PROCEDURE (SELF:SCREEN-VALUE, SELF) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[3] V-table-Win
ON VALUE-CHANGED OF stax.tax-code1[3] IN FRAME F-Main /* Tax Code[3] */
DO:
  {viewers/stax.i 3}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-code1[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[4] V-table-Win
ON HELP OF stax.tax-code1[4] IN FRAME F-Main /* Tax Code[4] */
DO:
  {viewers/stax2.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[4] V-table-Win
ON LEAVE OF stax.tax-code1[4] IN FRAME F-Main /* Tax Code[4] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-code IN THIS-PROCEDURE (SELF:SCREEN-VALUE, SELF) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[4] V-table-Win
ON VALUE-CHANGED OF stax.tax-code1[4] IN FRAME F-Main /* Tax Code[4] */
DO:
  {viewers/stax.i 4}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-code1[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[5] V-table-Win
ON HELP OF stax.tax-code1[5] IN FRAME F-Main /* Tax Code[5] */
DO:
  {viewers/stax2.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[5] V-table-Win
ON LEAVE OF stax.tax-code1[5] IN FRAME F-Main /* Tax Code[5] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-code IN THIS-PROCEDURE (SELF:SCREEN-VALUE, SELF) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[5] V-table-Win
ON VALUE-CHANGED OF stax.tax-code1[5] IN FRAME F-Main /* Tax Code[5] */
DO:
  {viewers/stax.i 5}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-code1[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[6] V-table-Win
ON HELP OF stax.tax-code1[6] IN FRAME F-Main /* Tax Code[6] */
DO:
  {viewers/stax2.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[6] V-table-Win
ON LEAVE OF stax.tax-code1[6] IN FRAME F-Main /* Tax Code[6] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-code IN THIS-PROCEDURE (SELF:SCREEN-VALUE, SELF) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[6] V-table-Win
ON VALUE-CHANGED OF stax.tax-code1[6] IN FRAME F-Main /* Tax Code[6] */
DO:
    {viewers/stax.i 6}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-code1[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[7] V-table-Win
ON HELP OF stax.tax-code1[7] IN FRAME F-Main /* Tax Code[7] */
DO:
  {viewers/stax2.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[7] V-table-Win
ON LEAVE OF stax.tax-code1[7] IN FRAME F-Main /* Tax Code[7] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-code IN THIS-PROCEDURE (SELF:SCREEN-VALUE, SELF) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[7] V-table-Win
ON VALUE-CHANGED OF stax.tax-code1[7] IN FRAME F-Main /* Tax Code[7] */
DO:
  {viewers/stax.i 7}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-code1[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[8] V-table-Win
ON HELP OF stax.tax-code1[8] IN FRAME F-Main /* Tax Code[8] */
DO:
  {viewers/stax2.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[8] V-table-Win
ON LEAVE OF stax.tax-code1[8] IN FRAME F-Main /* Tax Code[8] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-code IN THIS-PROCEDURE (SELF:SCREEN-VALUE, SELF) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[8] V-table-Win
ON VALUE-CHANGED OF stax.tax-code1[8] IN FRAME F-Main /* Tax Code[8] */
DO:
  {viewers/stax.i 8}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stax.tax-code1[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[9] V-table-Win
ON HELP OF stax.tax-code1[9] IN FRAME F-Main /* Tax Code[9] */
DO:
  {viewers/stax2.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[9] V-table-Win
ON LEAVE OF stax.tax-code1[9] IN FRAME F-Main /* Tax Code[9] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-code IN THIS-PROCEDURE (SELF:SCREEN-VALUE, SELF) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stax.tax-code1[9] V-table-Win
ON VALUE-CHANGED OF stax.tax-code1[9] IN FRAME F-Main /* Tax Code[9] */
DO:
  {viewers/stax.i 9}
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win
PROCEDURE local-cancel-record:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    RUN presetColor(INPUT "") NO-ERROR.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .   

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-reset-record V-table-Win
PROCEDURE local-reset-record:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    RUN presetColor(INPUT "") NO-ERROR.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .   

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
  DEFINE VARIABLE lError    AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lInactive AS LOGICAL NO-UNDO.

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
  FOR EACH tt-accounts:
      IF tt-accounts.c-code NE "" THEN DO:
          RUN pvalidGLAccount(INPUT tt-accounts.c-acct, OUTPUT lError, OUTPUT lInactive).
          IF lError THEN 
          DO:
              RUN presetColor(INPUT h-acct) NO-ERROR.
              IF lInactive EQ YES THEN 
                  RUN pchangeColor(INPUT h-acct) NO-ERROR.
              APPLY "ENTRY" TO tt-accounts.h-acct.
              RETURN NO-APPLY.
          END. 
      END.    
      ELSE DO:
          IF tt-accounts.c-acct NE "" THEN DO:
              MESSAGE "First enter corresponding tax-code." VIEW-AS ALERT-BOX INFORMATION.
              tt-accounts.h-acct:SCREEN-VALUE = "".
              APPLY "ENTRY" TO tt-accounts.h-acct.
              RETURN NO-APPLY.                         
          END. 
      END.       
             
      RUN presetColor(INPUT h-acct) NO-ERROR.                     
  END.
  {&methods/lValidateError.i NO}
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pchangeColor V-table-Win
PROCEDURE pchangeColor:
/*------------------------------------------------------------------------------
 Purpose: If GL Account is inactive then change the background color of fill-in.
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphfieldHandle AS HANDLE NO-UNDO.
    
    IF VALID-HANDLE(iphfieldHandle) THEN DO:
        IF iphfieldHandle:TYPE      EQ "fill-in"   AND 
           iphfieldHandle:DATA-TYPE EQ "CHARACTER" AND 
           iphfieldHandle:NAME MATCHES "*tax-acc1*" THEN 
            ASSIGN 
                iphfieldHandle:BGCOLOR = 16
                iphfieldHandle:FGCOLOR = 15
                .                                            
    END.    
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE presetColor V-table-Win
PROCEDURE presetColor:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphfieldHandle AS HANDLE NO-UNDO.

    DEFINE VARIABLE hframeHandle AS HANDLE NO-UNDO.
    DEFINE VARIABLE hgroupHandle AS HANDLE NO-UNDO.
    DEFINE VARIABLE hfieldHandle AS HANDLE NO-UNDO.    

    ASSIGN 
        hframeHandle = FRAME {&FRAME-NAME}:HANDLE
        hgroupHandle = hframeHandle:FIRST-CHILD
        hfieldHandle = hgroupHandle:FIRST-CHILD
        .       
    IF VALID-HANDLE(iphfieldHandle) THEN DO:
        IF iphfieldHandle:TYPE      EQ "fill-in"   AND 
           iphfieldHandle:DATA-TYPE EQ "CHARACTER" AND 
           iphfieldHandle:NAME MATCHES "*tax-acc1*" THEN  
            IF iphfieldHandle:BGCOLOR EQ 16 THEN          
                ASSIGN 
                    iphfieldHandle:BGCOLOR = 15
                    iphfieldHandle:FGCOLOR = ?
                    .                                
    END. 
    ELSE DO:                
        DO WHILE VALID-HANDLE(hfieldHandle):                           
            IF hfieldHandle:TYPE      EQ "fill-in"   AND 
               hfieldHandle:DATA-TYPE EQ "CHARACTER" AND 
               hfieldHandle:NAME MATCHES "*tax-acc1*" THEN               
               IF hfieldHandle:BGCOLOR EQ 16 THEN             
                   ASSIGN 
                       hfieldHandle:BGCOLOR = 15
                       hfieldHandle:FGCOLOR = ?
                       .                                        
            hfieldHandle = hfieldHandle:NEXT-SIBLING.
        END. 
    END.                                     
   
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pvalidGLAccount V-table-Win
PROCEDURE pvalidGLAccount:
/*------------------------------------------------------------------------------
 Purpose: To check valid and active GL Account.
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipAccount LIKE account.actnum NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError    AS LOGICAL        NO-UNDO.
    DEFINE OUTPUT PARAMETER oplInactive AS LOGICAL        NO-UNDO.

    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lActive  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
  
    DO WITH FRAME {&FRAME-NAME}:                  
        RUN GL_CheckGLAccount IN hGLProcs(
            INPUT  gcompany,
            INPUT  ipAccount,            
            OUTPUT cMessage,
            OUTPUT lSuccess,
            OUTPUT lActive
            ). 
        IF lSuccess EQ NO THEN DO:
            MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.             
            oplError = YES.
        END.  
        IF lSuccess EQ YES AND lActive EQ NO THEN DO:                      
            MESSAGE cMessage VIEW-AS ALERT-BOX ERROR. 
            ASSIGN 
                oplError    = YES 
                oplInactive = YES
                .                                                        
        END.  
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

