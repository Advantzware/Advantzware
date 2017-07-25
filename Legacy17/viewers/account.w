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

  File: viewers/account.w

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

{custom/format.i}
{custom/globdefs.i}
{custom/gcompany.i}

gcompany = g_company.

DEF VAR li AS INT NO-UNDO.

&SCOPED-DEFINE enable-account enable-account
&SCOPED-DEFINE copy-proc proc-copy

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
&Scoped-define EXTERNAL-TABLES account
&Scoped-define FIRST-EXTERNAL-TABLE account


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR account.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS account.dscr account.cyr-open ~
account.lyr-open account.type account.bud[1] account.ly-bud[1] ~
account.bud[2] account.ly-bud[2] account.bud[3] account.ly-bud[3] ~
account.bud[4] account.ly-bud[4] account.bud[5] account.ly-bud[5] ~
account.bud[6] account.ly-bud[6] account.bud[7] account.ly-bud[7] ~
account.bud[8] account.ly-bud[8] account.bud[9] account.ly-bud[9] ~
account.bud[10] account.ly-bud[10] account.bud[11] account.ly-bud[11] ~
account.bud[12] account.ly-bud[12] account.bud[13] account.ly-bud[13] 
&Scoped-define ENABLED-TABLES account
&Scoped-define FIRST-ENABLED-TABLE account
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS account.actnum account.dscr ~
account.cyr-open account.lyr-open account.type account.cyr[1] ~
account.lyr[1] account.bud[1] account.ly-bud[1] account.cyr[2] ~
account.lyr[2] account.bud[2] account.ly-bud[2] account.cyr[3] ~
account.lyr[3] account.bud[3] account.ly-bud[3] account.cyr[4] ~
account.lyr[4] account.bud[4] account.ly-bud[4] account.cyr[5] ~
account.lyr[5] account.bud[5] account.ly-bud[5] account.cyr[6] ~
account.lyr[6] account.bud[6] account.ly-bud[6] account.cyr[7] ~
account.lyr[7] account.bud[7] account.ly-bud[7] account.cyr[8] ~
account.lyr[8] account.bud[8] account.ly-bud[8] account.cyr[9] ~
account.lyr[9] account.bud[9] account.ly-bud[9] account.cyr[10] ~
account.lyr[10] account.bud[10] account.ly-bud[10] account.cyr[11] ~
account.lyr[11] account.bud[11] account.ly-bud[11] account.cyr[12] ~
account.lyr[12] account.bud[12] account.ly-bud[12] account.cyr[13] ~
account.lyr[13] account.bud[13] account.ly-bud[13] 
&Scoped-define DISPLAYED-TABLES account
&Scoped-define FIRST-DISPLAYED-TABLE account
&Scoped-Define DISPLAYED-OBJECTS tb_not-disc 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS account.actnum 
&Scoped-define ADM-ASSIGN-FIELDS tb_not-disc 

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
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 143 BY 19.29
     BGCOLOR 9 FGCOLOR 9 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 1 BY 15.24
     BGCOLOR 0 .

DEFINE VARIABLE tb_not-disc AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .76 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     account.actnum AT ROW 1.24 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     account.dscr AT ROW 1.24 COL 67 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
          BGCOLOR 15 FONT 4
     account.cyr-open AT ROW 2.43 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.lyr-open AT ROW 2.43 COL 98 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.type AT ROW 2.43 COL 127 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Asset", "A":U,
"Capital", "C":U,
"Expense", "E":U,
"Liability", "L":U,
"Revenue", "R":U,
"Title", "T":U
          SIZE 15 BY 7.14
     account.cyr[1] AT ROW 4.81 COL 7 COLON-ALIGNED
          LABEL "01"
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.lyr[1] AT ROW 4.81 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.bud[1] AT ROW 4.81 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.ly-bud[1] AT ROW 4.81 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.cyr[2] AT ROW 6 COL 7 COLON-ALIGNED
          LABEL "02"
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.lyr[2] AT ROW 6 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.bud[2] AT ROW 6 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.ly-bud[2] AT ROW 6 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.cyr[3] AT ROW 7.19 COL 7 COLON-ALIGNED
          LABEL "03"
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.lyr[3] AT ROW 7.19 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.bud[3] AT ROW 7.19 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.ly-bud[3] AT ROW 7.19 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.cyr[4] AT ROW 8.38 COL 7 COLON-ALIGNED
          LABEL "04"
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.lyr[4] AT ROW 8.38 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.bud[4] AT ROW 8.38 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.ly-bud[4] AT ROW 8.38 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     account.cyr[5] AT ROW 9.57 COL 7 COLON-ALIGNED
          LABEL "05"
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.lyr[5] AT ROW 9.57 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.bud[5] AT ROW 9.57 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.ly-bud[5] AT ROW 9.57 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     tb_not-disc AT ROW 10.29 COL 127 HELP
          "Check box if this Acct# receives no terms discount"
     account.cyr[6] AT ROW 10.76 COL 7 COLON-ALIGNED
          LABEL "06"
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.lyr[6] AT ROW 10.76 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.bud[6] AT ROW 10.76 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.ly-bud[6] AT ROW 10.76 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.cyr[7] AT ROW 11.95 COL 7 COLON-ALIGNED
          LABEL "07"
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.lyr[7] AT ROW 11.95 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.bud[7] AT ROW 11.95 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.ly-bud[7] AT ROW 11.95 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.cyr[8] AT ROW 13.14 COL 7 COLON-ALIGNED
          LABEL "08"
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.lyr[8] AT ROW 13.14 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.bud[8] AT ROW 13.14 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.ly-bud[8] AT ROW 13.14 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.cyr[9] AT ROW 14.33 COL 7 COLON-ALIGNED
          LABEL "09"
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.lyr[9] AT ROW 14.33 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.bud[9] AT ROW 14.33 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     account.ly-bud[9] AT ROW 14.33 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.cyr[10] AT ROW 15.52 COL 7 COLON-ALIGNED
          LABEL "10"
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.lyr[10] AT ROW 15.52 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.bud[10] AT ROW 15.52 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.ly-bud[10] AT ROW 15.52 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.cyr[11] AT ROW 16.71 COL 7 COLON-ALIGNED
          LABEL "11"
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.lyr[11] AT ROW 16.71 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.bud[11] AT ROW 16.71 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.ly-bud[11] AT ROW 16.71 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.cyr[12] AT ROW 17.91 COL 7 COLON-ALIGNED
          LABEL "12"
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.lyr[12] AT ROW 17.91 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.bud[12] AT ROW 17.91 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.ly-bud[12] AT ROW 17.91 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.cyr[13] AT ROW 19.1 COL 7 COLON-ALIGNED
          LABEL "13"
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.lyr[13] AT ROW 19.1 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     account.bud[13] AT ROW 19.1 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     account.ly-bud[13] AT ROW 19.1 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24.8 BY 1
          BGCOLOR 15 FONT 4
     RECT-2 AT ROW 4.81 COL 65
     RECT-1 AT ROW 1 COL 1
     "Period" VIEW-AS TEXT
          SIZE 8 BY .52 AT ROW 4 COL 3
          FGCOLOR 9 
     "Last Year Budget" VIEW-AS TEXT
          SIZE 21 BY .52 AT ROW 4 COL 104
          FGCOLOR 9 
     "Current Year" VIEW-AS TEXT
          SIZE 18 BY .52 AT ROW 4 COL 14
          FGCOLOR 9 
     "Current Year Budget" VIEW-AS TEXT
          SIZE 25 BY .52 AT ROW 4 COL 70
          FGCOLOR 9 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "Type:" VIEW-AS TEXT
          SIZE 6.6 BY .81 AT ROW 1.48 COL 127
     "Discount" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 10.76 COL 131
     "No Terms" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 10.05 COL 131
     "Last Year" VIEW-AS TEXT
          SIZE 15 BY .52 AT ROW 4 COL 45
          FGCOLOR 9 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.account
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
         HEIGHT             = 19.52
         WIDTH              = 143.6.
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN account.actnum IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN account.bud[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN account.cyr[10] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN account.cyr[11] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN account.cyr[12] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN account.cyr[13] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN account.cyr[1] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN account.cyr[2] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN account.cyr[3] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN account.cyr[4] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN account.cyr[5] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN account.cyr[6] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN account.cyr[7] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN account.cyr[8] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN account.cyr[9] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN account.lyr[10] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN account.lyr[11] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN account.lyr[12] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN account.lyr[13] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN account.lyr[1] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN account.lyr[2] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN account.lyr[3] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN account.lyr[4] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN account.lyr[5] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN account.lyr[6] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN account.lyr[7] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN account.lyr[8] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN account.lyr[9] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_not-disc IN FRAME F-Main
   NO-ENABLE 2                                                          */
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
ON CTRL-O OF FRAME F-Main
DO:
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR lv-disable-list AS CHAR.


  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
  RUN set-buttons IN WIDGET-HANDLE(char-hdl) ("").

  FOR EACH period
      WHERE period.company EQ account.company
        AND period.pstat   EQ YES
      NO-LOCK:
    lv-disable-list = lv-disable-list + TRIM(STRING(period.pnum,">>>>>>>>>>")) + ",".
  END.
  IF SUBSTR(lv-disable-list,LENGTH(TRIM(lv-disable-list)),1) EQ "," THEN
    SUBSTR(lv-disable-list,LENGTH(TRIM(lv-disable-list)),1) = "".

  DO WITH FRAME {&FRAME-NAME}:
    ENABLE account.cyr[1]   WHEN LOOKUP("1",lv-disable-list) LE 0
           account.cyr[2]   WHEN LOOKUP("2",lv-disable-list) LE 0
           account.cyr[3]   WHEN LOOKUP("3",lv-disable-list) LE 0
           account.cyr[4]   WHEN LOOKUP("4",lv-disable-list) LE 0
           account.cyr[5]   WHEN LOOKUP("5",lv-disable-list) LE 0
           account.cyr[6]   WHEN LOOKUP("6",lv-disable-list) LE 0
           account.cyr[7]   WHEN LOOKUP("7",lv-disable-list) LE 0
           account.cyr[8]   WHEN LOOKUP("8",lv-disable-list) LE 0
           account.cyr[9]   WHEN LOOKUP("9",lv-disable-list) LE 0
           account.cyr[10]  WHEN LOOKUP("10",lv-disable-list) LE 0
           account.cyr[11]  WHEN LOOKUP("11",lv-disable-list) LE 0
           account.cyr[12]  WHEN LOOKUP("12",lv-disable-list) LE 0
           account.cyr[13]  WHEN LOOKUP("13",lv-disable-list) LE 0

           account.lyr[1]
           account.lyr[2]
           account.lyr[3]
           account.lyr[4]
           account.lyr[5]
           account.lyr[6]
           account.lyr[7]
           account.lyr[8]
           account.lyr[9]
           account.lyr[10]
           account.lyr[11]
           account.lyr[12]
           account.lyr[13].

    IF account.cyr[1]:SENSITIVE THEN
      ASSIGN
       account.cyr[1]:BGCOLOR = 15
       account.cyr[1]:FGCOLOR = 0.

    IF account.cyr[2]:SENSITIVE THEN
      ASSIGN
       account.cyr[2]:BGCOLOR = 15
       account.cyr[2]:FGCOLOR = 0.

    IF account.cyr[3]:SENSITIVE THEN
      ASSIGN
       account.cyr[3]:BGCOLOR = 15
       account.cyr[3]:FGCOLOR = 0.

    IF account.cyr[4]:SENSITIVE THEN
      ASSIGN
       account.cyr[4]:BGCOLOR = 15
       account.cyr[4]:FGCOLOR = 0.

    IF account.cyr[5]:SENSITIVE THEN
      ASSIGN
       account.cyr[5]:BGCOLOR = 15
       account.cyr[5]:FGCOLOR = 0.

    IF account.cyr[6]:SENSITIVE THEN
      ASSIGN
       account.cyr[6]:BGCOLOR = 15
       account.cyr[6]:FGCOLOR = 0.

    IF account.cyr[7]:SENSITIVE THEN
      ASSIGN
       account.cyr[7]:BGCOLOR = 15
       account.cyr[7]:FGCOLOR = 0.

    IF account.cyr[8]:SENSITIVE THEN
      ASSIGN
       account.cyr[8]:BGCOLOR = 15
       account.cyr[8]:FGCOLOR = 0.

    IF account.cyr[9]:SENSITIVE THEN
      ASSIGN
       account.cyr[9]:BGCOLOR = 15
       account.cyr[9]:FGCOLOR = 0.

    IF account.cyr[10]:SENSITIVE THEN
      ASSIGN
       account.cyr[10]:BGCOLOR = 15
       account.cyr[10]:FGCOLOR = 0.

    IF account.cyr[11]:SENSITIVE THEN
      ASSIGN
       account.cyr[11]:BGCOLOR = 15
       account.cyr[11]:FGCOLOR = 0.

    IF account.cyr[12]:SENSITIVE THEN
      ASSIGN
       account.cyr[12]:BGCOLOR = 15
       account.cyr[12]:FGCOLOR = 0.

    IF account.cyr[13]:SENSITIVE THEN
      ASSIGN
       account.cyr[13]:BGCOLOR = 15
       account.cyr[13]:FGCOLOR = 0.

    ASSIGN
     account.lyr[1]:BGCOLOR = 15
     account.lyr[1]:FGCOLOR = 0
     account.lyr[2]:BGCOLOR = 15
     account.lyr[2]:FGCOLOR = 0   
     account.lyr[3]:BGCOLOR = 15
     account.lyr[3]:FGCOLOR = 0
     account.lyr[4]:BGCOLOR = 15
     account.lyr[4]:FGCOLOR = 0
     account.lyr[5]:BGCOLOR = 15
     account.lyr[5]:FGCOLOR = 0
     account.lyr[6]:BGCOLOR = 15
     account.lyr[6]:FGCOLOR = 0   
     account.lyr[7]:BGCOLOR = 15
     account.lyr[7]:FGCOLOR = 0
     account.lyr[8]:BGCOLOR = 15
     account.lyr[8]:FGCOLOR = 0
     account.lyr[9]:BGCOLOR = 15
     account.lyr[9]:FGCOLOR = 0
     account.lyr[10]:BGCOLOR = 15
     account.lyr[10]:FGCOLOR = 0
     account.lyr[11]:BGCOLOR = 15
     account.lyr[11]:FGCOLOR = 0   
     account.lyr[12]:BGCOLOR = 15
     account.lyr[12]:FGCOLOR = 0
     account.lyr[13]:BGCOLOR = 15
     account.lyr[13]:FGCOLOR = 0.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME account.actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL account.actnum V-table-Win
ON ENTRY OF account.actnum IN FRAME F-Main /* Account No */
DO:
  {custom/actentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL account.actnum V-table-Win
ON LEAVE OF account.actnum IN FRAME F-Main /* Account No */
DO:
  {custom/actleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL account.actnum V-table-Win
ON RETURN OF account.actnum IN FRAME F-Main /* Account No */
DO:
  APPLY "leave" TO {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RECT-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RECT-1 V-table-Win
ON MOUSE-SELECT-CLICK OF RECT-1 IN FRAME F-Main
DO:
  IF LASTKEY = KEYCODE("CONTROL-O")  THEN ENABLE ACCOUNT.CYR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

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
  {src/adm/template/row-list.i "account"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "account"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-ctrl-o V-table-Win 
PROCEDURE assign-ctrl-o :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-cyr LIKE account.cyr NO-UNDO.
  DEF VAR lv-lyr LIKE account.lyr NO-UNDO.
  DEF VAR li AS INT.


  DO WITH FRAME {&FRAME-NAME}:
    IF account.cyr[01]:SENSITIVE THEN DO TRANSACTION:
      ASSIGN
       lv-cyr[01] = DEC(account.cyr[01]:SCREEN-VALUE)
       lv-cyr[02] = DEC(account.cyr[02]:SCREEN-VALUE)
       lv-cyr[03] = DEC(account.cyr[03]:SCREEN-VALUE)
       lv-cyr[04] = DEC(account.cyr[04]:SCREEN-VALUE)
       lv-cyr[05] = DEC(account.cyr[05]:SCREEN-VALUE)
       lv-cyr[06] = DEC(account.cyr[06]:SCREEN-VALUE)
       lv-cyr[07] = DEC(account.cyr[07]:SCREEN-VALUE)
       lv-cyr[08] = DEC(account.cyr[08]:SCREEN-VALUE)
       lv-cyr[09] = DEC(account.cyr[09]:SCREEN-VALUE)
       lv-cyr[10] = DEC(account.cyr[10]:SCREEN-VALUE)
       lv-cyr[11] = DEC(account.cyr[11]:SCREEN-VALUE)
       lv-cyr[12] = DEC(account.cyr[12]:SCREEN-VALUE)
       lv-cyr[13] = DEC(account.cyr[13]:SCREEN-VALUE)

       lv-lyr[01] = DEC(account.lyr[01]:SCREEN-VALUE)
       lv-lyr[02] = DEC(account.lyr[02]:SCREEN-VALUE)
       lv-lyr[03] = DEC(account.lyr[03]:SCREEN-VALUE)
       lv-lyr[04] = DEC(account.lyr[04]:SCREEN-VALUE)
       lv-lyr[05] = DEC(account.lyr[05]:SCREEN-VALUE)
       lv-lyr[06] = DEC(account.lyr[06]:SCREEN-VALUE)
       lv-lyr[07] = DEC(account.lyr[07]:SCREEN-VALUE)
       lv-lyr[08] = DEC(account.lyr[08]:SCREEN-VALUE)
       lv-lyr[09] = DEC(account.lyr[09]:SCREEN-VALUE)
       lv-lyr[10] = DEC(account.lyr[10]:SCREEN-VALUE)
       lv-lyr[11] = DEC(account.lyr[11]:SCREEN-VALUE)
       lv-lyr[12] = DEC(account.lyr[12]:SCREEN-VALUE)
       lv-lyr[13] = DEC(account.lyr[13]:SCREEN-VALUE).

      FIND CURRENT account EXCLUSIVE.

      DO li = 1 TO 13:
        FIND FIRST period
            WHERE period.company EQ account.company
              AND period.pnum    EQ li
              AND period.pstat   EQ YES
            NO-LOCK NO-ERROR.

        IF NOT AVAIL period THEN account.cyr[li] = lv-cyr[li].

        account.lyr[li] = lv-lyr[li].
      END.

      FIND CURRENT account NO-LOCK.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-bal-to-bud V-table-Win 
PROCEDURE copy-bal-to-bud :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-process AS LOG NO-UNDO.
  DEF VAR li AS INT NO-UNDO.

  DEF BUFFER b-account FOR account.


  FIND b-account EXCLUSIVE WHERE ROWID(b-account) EQ ROWID(account)
      NO-WAIT NO-ERROR.

  IF AVAIL b-account THEN DO:
    MESSAGE "Are you sure you wish to copy Current Year Balances to "
            "Current Year Budgets?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll-process.

    IF ll-process THEN DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
       account.bud[01]:SCREEN-VALUE = account.cyr[01]:SCREEN-VALUE
       account.bud[02]:SCREEN-VALUE = account.cyr[02]:SCREEN-VALUE
       account.bud[03]:SCREEN-VALUE = account.cyr[03]:SCREEN-VALUE
       account.bud[04]:SCREEN-VALUE = account.cyr[04]:SCREEN-VALUE
       account.bud[05]:SCREEN-VALUE = account.cyr[05]:SCREEN-VALUE
       account.bud[06]:SCREEN-VALUE = account.cyr[06]:SCREEN-VALUE
       account.bud[07]:SCREEN-VALUE = account.cyr[07]:SCREEN-VALUE
       account.bud[08]:SCREEN-VALUE = account.cyr[08]:SCREEN-VALUE
       account.bud[09]:SCREEN-VALUE = account.cyr[09]:SCREEN-VALUE
       account.bud[10]:SCREEN-VALUE = account.cyr[10]:SCREEN-VALUE
       account.bud[11]:SCREEN-VALUE = account.cyr[11]:SCREEN-VALUE
       account.bud[12]:SCREEN-VALUE = account.cyr[12]:SCREEN-VALUE
       account.bud[13]:SCREEN-VALUE = account.cyr[13]:SCREEN-VALUE

       b-account.bud[01] = DEC(account.cyr[01]:SCREEN-VALUE)
       b-account.bud[02] = DEC(account.cyr[02]:SCREEN-VALUE)
       b-account.bud[03] = DEC(account.cyr[03]:SCREEN-VALUE)
       b-account.bud[04] = DEC(account.cyr[04]:SCREEN-VALUE)
       b-account.bud[05] = DEC(account.cyr[05]:SCREEN-VALUE)
       b-account.bud[06] = DEC(account.cyr[06]:SCREEN-VALUE)
       b-account.bud[07] = DEC(account.cyr[07]:SCREEN-VALUE)
       b-account.bud[08] = DEC(account.cyr[08]:SCREEN-VALUE)
       b-account.bud[09] = DEC(account.cyr[09]:SCREEN-VALUE)
       b-account.bud[10] = DEC(account.cyr[10]:SCREEN-VALUE)
       b-account.bud[11] = DEC(account.cyr[11]:SCREEN-VALUE)
       b-account.bud[12] = DEC(account.cyr[12]:SCREEN-VALUE)
       b-account.bud[13] = DEC(account.cyr[13]:SCREEN-VALUE).
    END.

    FIND CURRENT b-account NO-LOCK NO-ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-ctrl-o V-table-Win 
PROCEDURE disable-ctrl-o :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE ACCOUNT.CYR[1] 
            ACCOUNT.CYR[2]
            ACCOUNT.CYR[3]
            ACCOUNT.CYR[4]
            ACCOUNT.CYR[5]
            ACCOUNT.CYR[6]
            ACCOUNT.CYR[7]
            ACCOUNT.CYR[8]
            ACCOUNT.CYR[9]
            ACCOUNT.CYR[10]
            ACCOUNT.CYR[11]
            ACCOUNT.CYR[12]
            ACCOUNT.CYR[13]
            ACCOUNT.LYR[1]
            ACCOUNT.LYR[2]
            ACCOUNT.LYR[3]
            ACCOUNT.LYR[4]
            ACCOUNT.LYR[5]
            ACCOUNT.LYR[6]
            ACCOUNT.LYR[7]
            ACCOUNT.LYR[8]
            ACCOUNT.LYR[9]
            ACCOUNT.LYR[10]
            ACCOUNT.LYR[11]
            ACCOUNT.LYR[12]
            ACCOUNT.LYR[13].

       ASSIGN
         account.cyr[1]:BGCOLOR = 7
         account.cyr[1]:FGCOLOR = 15
         account.cyr[2]:BGCOLOR = 7
         account.cyr[2]:FGCOLOR = 15   
         account.cyr[3]:BGCOLOR = 7
         account.cyr[3]:FGCOLOR = 15
         account.cyr[4]:BGCOLOR = 7
         account.cyr[4]:FGCOLOR = 15   
         account.cyr[5]:BGCOLOR = 7
         account.cyr[5]:FGCOLOR = 15
         account.cyr[6]:BGCOLOR = 7
         account.cyr[6]:FGCOLOR = 15   
         account.cyr[7]:BGCOLOR = 7
         account.cyr[7]:FGCOLOR = 15
         account.cyr[8]:BGCOLOR = 7
         account.cyr[8]:FGCOLOR = 15   
         account.cyr[9]:BGCOLOR = 7
         account.cyr[9]:FGCOLOR = 15
         account.cyr[10]:BGCOLOR = 7
         account.cyr[10]:FGCOLOR = 15   
         account.cyr[11]:BGCOLOR = 7
         account.cyr[11]:FGCOLOR = 15
         account.cyr[12]:BGCOLOR = 7
         account.cyr[12]:FGCOLOR = 15
         account.cyr[13]:BGCOLOR = 7
         account.cyr[13]:FGCOLOR = 15
         account.lyr[1]:BGCOLOR = 7
         account.lyr[1]:FGCOLOR = 15
         account.lyr[2]:BGCOLOR = 7
         account.lyr[2]:FGCOLOR = 15   
         account.lyr[3]:BGCOLOR = 7
         account.lyr[3]:FGCOLOR = 15
         account.lyr[4]:BGCOLOR = 7
         account.lyr[4]:FGCOLOR = 15
         account.lyr[5]:BGCOLOR = 7
         account.lyr[5]:FGCOLOR = 15
         account.lyr[6]:BGCOLOR = 7
         account.lyr[6]:FGCOLOR = 15   
         account.lyr[7]:BGCOLOR = 7
         account.lyr[7]:FGCOLOR = 15
         account.lyr[8]:BGCOLOR = 7
         account.lyr[8]:FGCOLOR = 15
         account.lyr[9]:BGCOLOR = 7
         account.lyr[9]:FGCOLOR = 15
         account.lyr[10]:BGCOLOR = 7
         account.lyr[10]:FGCOLOR = 15
         account.lyr[11]:BGCOLOR = 7
         account.lyr[11]:FGCOLOR = 15   
         account.lyr[12]:BGCOLOR = 7
         account.lyr[12]:FGCOLOR = 15
         account.lyr[13]:BGCOLOR = 7
         account.lyr[13]:FGCOLOR = 15
         .
   END.

   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-target",OUTPUT char-hdl).

   DO li = 1 TO NUM-ENTRIES(char-hdl):
     IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(li,char-hdl))) THEN
       RUN enable-disable-buttons IN WIDGET-HANDLE(ENTRY(li,char-hdl)) (1) NO-ERROR.
   END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-account V-table-Win 
PROCEDURE enable-account :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ENABLE tb_not-disc.

    IF adm-adding-record THEN tb_not-disc:SCREEN-VALUE = "No".

    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-target",OUTPUT char-hdl).

    DO li = 1 TO NUM-ENTRIES(char-hdl):
      IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(li,char-hdl))) THEN
        RUN enable-disable-buttons IN WIDGET-HANDLE(ENTRY(li,char-hdl)) (0) NO-ERROR.
    END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FIX-VALUES V-table-Win 
PROCEDURE FIX-VALUES :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-not-disc AS LOG NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    ll-not-disc = tb_not-disc:SCREEN-VALUE EQ "yes".
  END.

  RUN assign-ctrl-o.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST reftable
      WHERE reftable.reftable EQ "GLACCTDISC"
        AND reftable.company  EQ gcompany
        AND reftable.loc      EQ ""
        AND reftable.code     EQ account.actnum
      NO-ERROR.
  IF NOT AVAIL reftable THEN DO:
    CREATE reftable.
    ASSIGN
     reftable.reftable = "GLACCTDISC"
     reftable.company  = gcompany
     reftable.loc      = ""
     reftable.code     = account.actnum.
  END.
  ASSIGN
   reftable.val[1] = INT(ll-not-disc)
   tb_not-disc     = ll-not-disc. 

  /*for each period
      where period.company eq account.company
        and period.pstat   eq yes
      no-lock,

      each gltrans
      where gltrans.company eq account.company
        and gltrans.actnum  eq account.actnum
        and gltrans.period  eq period.pnum
        and gltrans.tr-date ge period.pst
        and gltrans.tr-date le period.pend
      no-lock:

    account.cyr[period.pnum] = account.cyr[period.pnum] - gltrans.tr-amt.
  end.*/

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
  DO WITH FRAME {&frame-name}:
    DISABLE tb_not-disc.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN disable-ctrl-o.

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
  {methods/viewers/create/account.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR ld-period$ AS DEC NO-UNDO EXTENT 20.
  DEF VAR li-fisc-yr AS INT NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL account AND account.actnum NE "" THEN
  FIND FIRST reftable
      WHERE reftable.reftable EQ "GLACCTDISC"
        AND reftable.company  EQ gcompany
        AND reftable.loc      EQ ""
        AND reftable.code     EQ account.actnum
      NO-LOCK NO-ERROR.
  tb_not-disc = AVAIL reftable AND reftable.val[1] EQ 1.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    find first company where company.company eq account.company no-lock no-error.

    do li = 1 to 13:
      ld-period$[li] = account.cyr[li].
    end.

    for each period
        where period.company eq account.company
          and period.pstat   eq yes
        no-lock,

        each gltrans
        where gltrans.company eq account.company
          and gltrans.actnum  eq account.actnum
          and gltrans.period  eq period.pnum
          and gltrans.tr-date ge period.pst
          and gltrans.tr-date le period.pend
            no-lock:

          ld-period$[period.pnum] = ld-period$[period.pnum] + gltrans.tr-amt.
    end.

    assign
     account.cyr[01]:screen-value = string(ld-period$[01])
     account.cyr[02]:screen-value = string(ld-period$[02])
     account.cyr[03]:screen-value = string(ld-period$[03])
     account.cyr[04]:screen-value = string(ld-period$[04])
     account.cyr[05]:screen-value = string(ld-period$[05])
     account.cyr[06]:screen-value = string(ld-period$[06])
     account.cyr[07]:screen-value = string(ld-period$[07])
     account.cyr[08]:screen-value = string(ld-period$[08])
     account.cyr[09]:screen-value = string(ld-period$[09])
     account.cyr[10]:screen-value = string(ld-period$[10])
     account.cyr[11]:screen-value = string(ld-period$[11])
     account.cyr[12]:screen-value = string(ld-period$[12])
     account.cyr[13]:screen-value = string(ld-period$[13]).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */ 
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&frame-name}:
    DISABLE tb_not-disc.
  END.

  RUN disable-ctrl-o. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-copy V-table-Win 
PROCEDURE proc-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     account.cyr-open:SCREEN-VALUE = ""
     account.cyr[01]:SCREEN-VALUE  = ""
     account.cyr[02]:SCREEN-VALUE  = ""
     account.cyr[03]:SCREEN-VALUE  = ""
     account.cyr[04]:SCREEN-VALUE  = ""
     account.cyr[05]:SCREEN-VALUE  = ""
     account.cyr[06]:SCREEN-VALUE  = ""
     account.cyr[07]:SCREEN-VALUE  = ""
     account.cyr[08]:SCREEN-VALUE  = ""
     account.cyr[09]:SCREEN-VALUE  = ""
     account.cyr[10]:SCREEN-VALUE  = ""
     account.cyr[11]:SCREEN-VALUE  = ""
     account.cyr[12]:SCREEN-VALUE  = ""
     account.cyr[13]:SCREEN-VALUE  = ""
     account.lyr-open:SCREEN-VALUE = ""
     account.lyr[01]:SCREEN-VALUE  = ""
     account.lyr[02]:SCREEN-VALUE  = ""
     account.lyr[03]:SCREEN-VALUE  = ""
     account.lyr[04]:SCREEN-VALUE  = ""
     account.lyr[05]:SCREEN-VALUE  = ""
     account.lyr[06]:SCREEN-VALUE  = ""
     account.lyr[07]:SCREEN-VALUE  = ""
     account.lyr[08]:SCREEN-VALUE  = ""
     account.lyr[09]:SCREEN-VALUE  = ""
     account.lyr[10]:SCREEN-VALUE  = ""
     account.lyr[11]:SCREEN-VALUE  = ""
     account.lyr[12]:SCREEN-VALUE  = ""
     account.lyr[13]:SCREEN-VALUE  = "".
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
  {src/adm/template/snd-list.i "account"}

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

