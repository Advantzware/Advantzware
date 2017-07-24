&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

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

DEF VAR v-industry-list AS cha NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES asi
&Scoped-define FIRST-EXTERNAL-TABLE asi


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR asi.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS asi.Type asi.Company asi.Address1 ~
asi.Address2 asi.City asi.ST asi.zipnu asi.Country asi.Territory asi.Emp ~
asi.Plts asi.sales-amt[1] asi.sales-year[1] asi.mkt-date[1] asi.mkt-date[2] ~
asi.mkt-date[3] asi.mkt-date[4] asi.mkt-date[5] asi.mkt-date[6] ~
asi.mkt-date[7] asi.mkt-date[8] asi.mkt-date[9] asi.mkt-date[10] asi.tel ~
asi.fax asi.ind-value[1] asi.ind-value[2] asi.ind-value[3] asi.ind-value[4] ~
asi.ind-value[5] asi.ind-value[6] asi.ind-value[7] asi.ind-value[8] ~
asi.ind-value[9] asi.ind-value[10] asi.mkt-text[1] asi.mkt-text[2] ~
asi.mkt-text[3] asi.mkt-text[4] asi.mkt-text[5] asi.mkt-text[6] ~
asi.mkt-text[11] asi.mkt-text[12] asi.mkt-text[13] asi.mkt-text[14] ~
asi.mkt-text[15] asi.mkt-text[16] asi.mkt-text[17] asi.mkt-text[18] ~
asi.cust-no 
&Scoped-define ENABLED-TABLES asi
&Scoped-define FIRST-ENABLED-TABLE asi
&Scoped-Define ENABLED-OBJECTS RECT-17 RECT-18 RECT-6 RECT-7 RECT-19 
&Scoped-Define DISPLAYED-FIELDS asi.Type asi.Company asi.Address1 ~
asi.Address2 asi.City asi.ST asi.zipnu asi.Country asi.Territory asi.Emp ~
asi.Plts asi.sales-amt[1] asi.sales-year[1] asi.mkt-date[1] asi.mkt-date[2] ~
asi.mkt-date[3] asi.mkt-date[4] asi.mkt-date[5] asi.mkt-date[6] ~
asi.mkt-date[7] asi.mkt-date[8] asi.mkt-date[9] asi.mkt-date[10] asi.tel ~
asi.fax asi.ind-value[1] asi.ind-value[2] asi.ind-value[3] asi.ind-value[4] ~
asi.ind-value[5] asi.ind-value[6] asi.ind-value[7] asi.ind-value[8] ~
asi.ind-value[9] asi.ind-value[10] asi.mkt-text[1] asi.mkt-text[2] ~
asi.mkt-text[3] asi.mkt-text[4] asi.mkt-text[5] asi.mkt-text[6] ~
asi.mkt-text[11] asi.mkt-text[12] asi.mkt-text[13] asi.mkt-text[14] ~
asi.mkt-text[15] asi.mkt-text[16] asi.mkt-text[17] asi.mkt-text[18] ~
asi.cust-no 
&Scoped-define DISPLAYED-TABLES asi
&Scoped-define FIRST-DISPLAYED-TABLE asi
&Scoped-Define DISPLAYED-OBJECTS v-type-dscr v-ind-label-1 v-ind-label-2 ~
v-ind-label-3 v-ind-label-4 v-ind-label-5 v-ind-label-6 v-ind-label-7 ~
v-ind-label-8 v-ind-label-9 v-ind-label-10 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

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
DEFINE VARIABLE v-ind-label-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY .71 NO-UNDO.

DEFINE VARIABLE v-ind-label-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY .71 NO-UNDO.

DEFINE VARIABLE v-ind-label-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY .71 NO-UNDO.

DEFINE VARIABLE v-ind-label-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY .71 NO-UNDO.

DEFINE VARIABLE v-ind-label-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY .71 NO-UNDO.

DEFINE VARIABLE v-ind-label-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY .71 NO-UNDO.

DEFINE VARIABLE v-ind-label-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY .71 NO-UNDO.

DEFINE VARIABLE v-ind-label-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY .71 NO-UNDO.

DEFINE VARIABLE v-ind-label-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY .71 NO-UNDO.

DEFINE VARIABLE v-ind-label-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY .71 NO-UNDO.

DEFINE VARIABLE v-type-dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 4.05.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 98 BY 9.05.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 158.4 BY 20.38.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46 BY 10.71.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 98 BY 6.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     asi.Type AT ROW 1.24 COL 38 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     v-type-dscr AT ROW 1.24 COL 42 COLON-ALIGNED NO-LABEL
     asi.Company AT ROW 2.43 COL 13 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     asi.Address1 AT ROW 3.43 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     asi.Address2 AT ROW 4.43 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     asi.City AT ROW 5.43 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     asi.ST AT ROW 5.43 COL 36 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     asi.zipnu AT ROW 5.52 COL 44 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     asi.Country AT ROW 6.48 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     asi.Territory AT ROW 6.48 COL 44 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     asi.Emp AT ROW 7.67 COL 19 COLON-ALIGNED
          LABEL "Total Employee"
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
     asi.Plts AT ROW 8.86 COL 20 COLON-ALIGNED
          LABEL "Total Plants"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     asi.sales-amt[1] AT ROW 7.67 COL 44 COLON-ALIGNED
          LABEL "Sales (Mill)" FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     asi.sales-year[1] AT ROW 8.86 COL 44 COLON-ALIGNED
          LABEL "Year"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     asi.mkt-date[1] AT ROW 11 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     asi.mkt-date[2] AT ROW 12 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     asi.mkt-date[3] AT ROW 13 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     asi.mkt-date[4] AT ROW 14 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     asi.mkt-date[5] AT ROW 15 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     asi.mkt-date[6] AT ROW 16 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     asi.mkt-date[7] AT ROW 17 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     asi.mkt-date[8] AT ROW 18 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     asi.mkt-date[9] AT ROW 19 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     asi.mkt-date[10] AT ROW 20 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     asi.tel AT ROW 2.43 COL 66 COLON-ALIGNED
          LABEL "Phone" FORMAT "xxx-xxx-xxxx"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     asi.fax AT ROW 3.38 COL 66 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     asi.ind-value[1] AT ROW 1.48 COL 100
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 4 BY .81
          FONT 6
     asi.ind-value[2] AT ROW 2.19 COL 100
          LABEL "ind-value[2]"
          VIEW-AS TOGGLE-BOX
          SIZE 4 BY .81
     asi.ind-value[3] AT ROW 2.91 COL 100
          LABEL "ind-value[3]"
          VIEW-AS TOGGLE-BOX
          SIZE 4 BY .81
     asi.ind-value[4] AT ROW 3.62 COL 100
          LABEL "ind-value"
          VIEW-AS TOGGLE-BOX
          SIZE 3 BY .81
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     asi.ind-value[5] AT ROW 4.33 COL 100
          LABEL "ind-value[5]"
          VIEW-AS TOGGLE-BOX
          SIZE 4 BY .81
     asi.ind-value[6] AT ROW 1.48 COL 128
          LABEL "ind-value[6]"
          VIEW-AS TOGGLE-BOX
          SIZE 3 BY .81
     asi.ind-value[7] AT ROW 2.19 COL 128
          LABEL "ind-value[7]"
          VIEW-AS TOGGLE-BOX
          SIZE 3 BY .81
     asi.ind-value[8] AT ROW 2.91 COL 128
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 3 BY .81
     asi.ind-value[9] AT ROW 3.62 COL 128
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 3 BY .81
     asi.ind-value[10] AT ROW 4.33 COL 128
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 3 BY .81
     asi.mkt-text[1] AT ROW 5.52 COL 85 COLON-ALIGNED FORMAT "x(50)"
          VIEW-AS FILL-IN 
          SIZE 71 BY 1
     asi.mkt-text[2] AT ROW 6.52 COL 85 COLON-ALIGNED FORMAT "x(50)"
          VIEW-AS FILL-IN 
          SIZE 71 BY 1
     asi.mkt-text[3] AT ROW 7.52 COL 85 COLON-ALIGNED FORMAT "x(50)"
          VIEW-AS FILL-IN 
          SIZE 71 BY 1
     asi.mkt-text[4] AT ROW 8.52 COL 85 COLON-ALIGNED FORMAT "x(50)"
          VIEW-AS FILL-IN 
          SIZE 71 BY 1
     asi.mkt-text[5] AT ROW 9.52 COL 85 COLON-ALIGNED FORMAT "x(50)"
          VIEW-AS FILL-IN 
          SIZE 71 BY 1
     asi.mkt-text[6] AT ROW 10.52 COL 85 COLON-ALIGNED FORMAT "x(50)"
          VIEW-AS FILL-IN 
          SIZE 71 BY 1
     v-ind-label-1 AT ROW 1.48 COL 101 COLON-ALIGNED NO-LABEL
     v-ind-label-2 AT ROW 2.19 COL 101 COLON-ALIGNED NO-LABEL
     v-ind-label-3 AT ROW 2.91 COL 101 COLON-ALIGNED NO-LABEL
     v-ind-label-4 AT ROW 3.62 COL 101 COLON-ALIGNED NO-LABEL
     v-ind-label-5 AT ROW 4.33 COL 101 COLON-ALIGNED NO-LABEL
     v-ind-label-6 AT ROW 1.48 COL 129 COLON-ALIGNED NO-LABEL
     v-ind-label-7 AT ROW 2.19 COL 129 COLON-ALIGNED NO-LABEL
     v-ind-label-8 AT ROW 2.91 COL 129 COLON-ALIGNED NO-LABEL
     v-ind-label-9 AT ROW 3.62 COL 129 COLON-ALIGNED NO-LABEL
     v-ind-label-10 AT ROW 4.33 COL 129 COLON-ALIGNED NO-LABEL
     asi.mkt-text[11] AT ROW 12.91 COL 60 COLON-ALIGNED NO-LABEL FORMAT "x(90)"
          VIEW-AS FILL-IN 
          SIZE 95 BY 1
     asi.mkt-text[12] AT ROW 13.91 COL 60 COLON-ALIGNED NO-LABEL FORMAT "x(90)"
          VIEW-AS FILL-IN 
          SIZE 95 BY 1
     asi.mkt-text[13] AT ROW 14.91 COL 60 COLON-ALIGNED NO-LABEL FORMAT "x(90)"
          VIEW-AS FILL-IN 
          SIZE 95 BY 1
     asi.mkt-text[14] AT ROW 15.91 COL 60 COLON-ALIGNED NO-LABEL FORMAT "x(90)"
          VIEW-AS FILL-IN 
          SIZE 95 BY 1
     asi.mkt-text[15] AT ROW 16.91 COL 60 COLON-ALIGNED NO-LABEL FORMAT "x(90)"
          VIEW-AS FILL-IN 
          SIZE 95 BY 1
     asi.mkt-text[16] AT ROW 17.91 COL 60 COLON-ALIGNED NO-LABEL FORMAT "x(90)"
          VIEW-AS FILL-IN 
          SIZE 95 BY 1
     asi.mkt-text[17] AT ROW 18.91 COL 60 COLON-ALIGNED NO-LABEL FORMAT "x(90)"
          VIEW-AS FILL-IN 
          SIZE 95 BY 1
     asi.mkt-text[18] AT ROW 19.91 COL 60 COLON-ALIGNED NO-LABEL FORMAT "x(90)"
          VIEW-AS FILL-IN 
          SIZE 95 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     asi.cust-no AT ROW 1.24 COL 13 COLON-ALIGNED
          LABEL "Company"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     "Marketing Date" VIEW-AS TEXT
          SIZE 20 BY .71 AT ROW 10.29 COL 13
     "Industry:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.48 COL 89
     "   Marketing Notes" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 12.19 COL 96
     RECT-17 AT ROW 1.24 COL 88
     RECT-18 AT ROW 12.19 COL 61
     RECT-6 AT ROW 10.52 COL 4
     RECT-7 AT ROW 5.29 COL 61
     RECT-19 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: name.asi
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
         HEIGHT             = 20.38
         WIDTH              = 158.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

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

/* SETTINGS FOR FILL-IN asi.Company IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN asi.cust-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN asi.Emp IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX asi.ind-value[10] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX asi.ind-value[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX asi.ind-value[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX asi.ind-value[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX asi.ind-value[4] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX asi.ind-value[5] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX asi.ind-value[6] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX asi.ind-value[7] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX asi.ind-value[8] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX asi.ind-value[9] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN asi.mkt-text[11] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN asi.mkt-text[12] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN asi.mkt-text[13] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN asi.mkt-text[14] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN asi.mkt-text[15] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN asi.mkt-text[16] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN asi.mkt-text[17] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN asi.mkt-text[18] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN asi.mkt-text[1] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN asi.mkt-text[2] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN asi.mkt-text[3] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN asi.mkt-text[4] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN asi.mkt-text[5] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN asi.mkt-text[6] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN asi.Plts IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN asi.sales-amt[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN asi.sales-year[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN asi.tel IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN v-ind-label-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-ind-label-10 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-ind-label-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-ind-label-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-ind-label-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-ind-label-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-ind-label-6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-ind-label-7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-ind-label-8 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-ind-label-9 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-type-dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN asi.zipnu IN FRAME F-Main
   EXP-LABEL                                                            */
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
    CASE FOCUS:NAME:
        WHEN "type" THEN DO:
            RUN names/l-stype.w ("",FOCUS:SCREEN-VALUE,OUTPUT char-val).
            IF char-val <> "" THEN 
                ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                       v-type-dscr:SCREEN-VALUE = ENTRY(2,char-val).
        END.
        WHEN "industry" THEN DO:
            RUN names/l-indus.w (FOCUS:SCREEN-VALUE,OUTPUT char-val).
            IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME asi.Type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL asi.Type V-table-Win
ON LEAVE OF asi.Type IN FRAME F-Main /* Type */
DO:
   FIND FIRST susptype WHERE susptype.suspTYPE = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL susptype THEN v-type-dscr:SCREEN-VALUE = susptype.dscr.
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
  {src/adm/template/row-list.i "asi"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "asi"}

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
  def var ls-key as cha form "x(20)" no-undo.
  /* Code placed here will execute PRIOR to standard behavior. */


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF adm-new-record THEN DO:
     CREATE contact.
     ASSIGN contact.cust-no = asi.cust-no /*STRING(asi.suspect-no)*/
            contact.first-name = asi.company
            contact.cust-name = asi.company
            contact.addr1 = asi.address1
            contact.addr2 = asi.address2
            contact.city  = asi.city
            contact.state = asi.st
            contact.zip   = asi.zipnu
            contact.country = asi.country
            contact.territory = asi.territory
            .


     ls-key = string(today,"99999999") +
              string(next-value(rec_key_seq,nosweat),"99999999").
     asi.rec_key = ls-key.               
     create rec_key.
     assign rec_key.rec_key = asi.rec_key
            rec_key.table_name = "asi".

  END.

  ASSIGN asi.ind-label[1] = v-ind-label-1 /*asi.ind-value[1]:LABEL IN FRAME {&frame-name}*/
         asi.ind-label[2] = v-ind-label-2 /*asi.ind-value[2]:LABEL IN FRAME {&frame-name}*/
         asi.ind-label[3] = v-ind-label-3 /*asi.ind-value[3]:LABEL IN FRAME {&frame-name}*/
         asi.ind-label[4] = v-ind-label-4 /*asi.ind-value[4]:LABEL IN FRAME {&frame-name}*/
         asi.ind-label[5] = v-ind-label-5 /*asi.ind-value[5]:LABEL IN FRAME {&frame-name}*/
         asi.ind-label[6] = v-ind-label-6 /*asi.ind-value[6]:LABEL IN FRAME {&frame-name}*/
         asi.ind-label[7] = v-ind-label-7 /*asi.ind-value[7]:LABEL IN FRAME {&frame-name}*/
         asi.ind-label[8] = v-ind-label-8
         asi.ind-label[9] = v-ind-label-9
         asi.ind-label[10] = v-ind-label-10
         .

  asi.industry = (IF asi.ind-value[1] THEN asi.ind-label[1] ELSE "") +
                 (IF asi.ind-value[2] THEN "," + asi.ind-label[2] ELSE "") +
                 (IF asi.ind-value[3] THEN "," + asi.ind-label[3] ELSE "" ) +
                 (IF asi.ind-value[4] THEN "," + asi.ind-label[4] ELSE "") +
                 (IF asi.ind-value[5] THEN "," + asi.ind-label[5] ELSE "" )+
                 (IF asi.ind-value[6] THEN "," + asi.ind-label[6] ELSE "") +
                 (IF asi.ind-value[7] THEN "," + asi.ind-label[7] ELSE "") +
                 (IF asi.ind-value[8] THEN "," + asi.ind-label[8] ELSE "") +
                 (IF asi.ind-value[9] THEN "," + asi.ind-label[9] ELSE "") +
                 (IF asi.ind-value[10] THEN "," + asi.ind-label[10] ELSE "") 
                 .
 IF SUBSTRING(asi.industry,1,1) = "," THEN asi.industry = SUBSTRING(asi.industry,2).

 IF adm-new-record THEN DO:
  /*  FIND FIRST cust WHERE cust.cust-no = string(asi.suspect-no) NO-LOCK NO-ERROR.
    IF NOT AVAIL cust THEN DO:
       CREATE cust.
       ASSIGN cust.cust-no = asi.cust-no  /*STRING(asi.suspect-no)*/
              cust.NAME = asi.company
              cust.addr[1] = asi.address1
              cust.addr[2] = asi.address2
              cust.city = asi.city
              cust.state = asi.st
              cust.zip = asi.zipnu.
    END.
   */ 
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-asi FOR asi.
  DEF VAR lv-next-susp AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  FOR EACH bf-asi NO-LOCK BY bf-asi.suspect-no DESC.
      lv-next-susp = bf-asi.suspect-no.
      LEAVE.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*IF lv-next-susp < 20000 THEN lv-next-susp = 19999.*/

  ASSIGN asi.suspect-no = lv-next-susp + 1
         .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-suspect LIKE asi.suspect-no NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/askdel.i}
  lv-suspect = asi.suspect-no.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FOR EACH contact WHERE contact.cust-no = STRING(lv-suspect):
      DELETE contact.
  END.
  FIND FIRST cust WHERE cust.cust-no = string(lv-suspect) NO-ERROR.
  IF AVAIL cust THEN DELETE cust.
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


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL asi THEN
     FIND FIRST susptype WHERE susptype.suspTYPE = asi.TYPE NO-LOCK NO-ERROR.
  IF AVAIL susptype THEN v-type-dscr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = susptype.dscr.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST mkt-ref NO-LOCK NO-ERROR.
  IF AVAIL mkt-ref THEN DO WITH FRAME {&FRAME-NAME}:
     DO  i = 1 TO 10:
        IF mkt-ref.industry[i] <> ""
           THEN v-industry-list = v-industry-list + mkt-ref.industry[i] + ",".
     END.
     ASSIGN /*asi.ind-value[1]:LABEL = mkt-ref.industry[1]
       asi.ind-value[2]:LABEL = mkt-ref.industry[2]
       asi.ind-value[3]:LABEL = mkt-ref.industry[3]
       asi.ind-value[4]:LABEL = mkt-ref.industry[4]
       asi.ind-value[5]:LABEL = mkt-ref.industry[5]
       asi.ind-value[6]:LABEL = mkt-ref.industry[6]
       asi.ind-value[7]:LABEL = mkt-ref.industry[7] */
       v-ind-label-1 = IF mkt-ref.ind-dis[1] THEN mkt-ref.industry[1] ELSE ""
       v-ind-label-2 = IF mkt-ref.ind-dis[2] THEN mkt-ref.industry[2] ELSE ""
       v-ind-label-3 = IF mkt-ref.ind-dis[3] THEN mkt-ref.industry[3] ELSE ""
       v-ind-label-4 = IF mkt-ref.ind-dis[4] THEN mkt-ref.industry[4] ELSE ""
       v-ind-label-5 = IF mkt-ref.ind-dis[5] THEN mkt-ref.industry[5] ELSE ""
       v-ind-label-6 = IF mkt-ref.ind-dis[6] THEN mkt-ref.industry[6] ELSE ""
       v-ind-label-7 = IF mkt-ref.ind-dis[7] THEN mkt-ref.industry[7] ELSE ""
       v-ind-label-8 = IF mkt-ref.ind-dis[8] THEN mkt-ref.industry[8] ELSE ""
       v-ind-label-9 = IF mkt-ref.ind-dis[9] THEN mkt-ref.industry[9] ELSE ""
       v-ind-label-10 = IF mkt-ref.ind-dis[10] THEN mkt-ref.industry[10] ELSE ""
       .

     ASSIGN asi.mkt-date[1]:LABEL = mkt-ref.mkt-date[1]
            asi.mkt-date[2]:LABEL = mkt-ref.mkt-date[2]
            asi.mkt-date[3]:LABEL = mkt-ref.mkt-date[3]
            asi.mkt-date[4]:LABEL = mkt-ref.mkt-date[4]
            asi.mkt-date[5]:LABEL = mkt-ref.mkt-date[5]
            asi.mkt-date[6]:LABEL = mkt-ref.mkt-date[6]
            asi.mkt-date[7]:LABEL = mkt-ref.mkt-date[7]
            asi.mkt-date[8]:LABEL = mkt-ref.mkt-date[8]
            asi.mkt-date[9]:LABEL = mkt-ref.mkt-date[9]
            asi.mkt-date[10]:LABEL = mkt-ref.mkt-date[10] 
            asi.mkt-text[1]:LABEL = mkt-ref.mkt-text[1]
            asi.mkt-text[2]:LABEL = mkt-ref.mkt-text[2]
            asi.mkt-text[3]:LABEL = mkt-ref.mkt-text[3]
            asi.mkt-text[4]:LABEL = mkt-ref.mkt-text[4]
            asi.mkt-text[5]:LABEL = mkt-ref.mkt-text[5]
            asi.mkt-text[6]:LABEL = mkt-ref.mkt-text[6]
            /*asi.mkt-text[7]:LABEL = mkt-ref.mkt-text[7]
            asi.mkt-text[8]:LABEL = mkt-ref.mkt-text[8]
            asi.mkt-text[9]:LABEL = mkt-ref.mkt-text[9]
            asi.mkt-text[10]:LABEL = mkt-ref.mkt-text[10] */
            .
        ASSIGN asi.mkt-date[1]:hidden = IF NOT mkt-ref.mdate-dis[1] THEN YES ELSE NO
               asi.mkt-date[2]:hidden = IF NOT mkt-ref.mdate-dis[2] THEN YES ELSE NO
               asi.mkt-date[3]:hidden = IF NOT mkt-ref.mdate-dis[3] THEN YES ELSE NO
               asi.mkt-date[4]:hidden = IF NOT mkt-ref.mdate-dis[4] THEN YES ELSE NO
               asi.mkt-date[5]:hidden = IF NOT mkt-ref.mdate-dis[5] THEN YES ELSE NO
               asi.mkt-date[6]:hidden = IF NOT mkt-ref.mdate-dis[6] THEN YES ELSE NO
               asi.mkt-date[7]:hidden = IF NOT mkt-ref.mdate-dis[7] THEN YES ELSE NO
               asi.mkt-date[8]:hidden = IF NOT mkt-ref.mdate-dis[8] THEN YES ELSE NO
               asi.mkt-date[9]:hidden = IF NOT mkt-ref.mdate-dis[9] THEN YES ELSE NO
               asi.mkt-date[10]:hidden = IF NOT mkt-ref.mdate-dis[10] THEN YES ELSE NO
               asi.mkt-text[1]:hidden = IF NOT mkt-ref.mtext-dis[1] THEN YES ELSE NO
               asi.mkt-text[2]:hidden = IF NOT mkt-ref.mtext-dis[2] THEN YES ELSE NO
               asi.mkt-text[3]:hidden = IF NOT mkt-ref.mtext-dis[3] THEN YES ELSE NO
               asi.mkt-text[4]:hidden = IF NOT mkt-ref.mtext-dis[4] THEN YES ELSE NO
               asi.mkt-text[5]:hidden = IF NOT mkt-ref.mtext-dis[5] THEN YES ELSE NO
               asi.mkt-text[6]:hidden = IF NOT mkt-ref.mtext-dis[6] THEN YES ELSE NO
               asi.mkt-text[11]:hidden = IF NOT mkt-ref.mtext-dis[11] THEN YES ELSE NO
               asi.mkt-text[12]:hidden = IF NOT mkt-ref.mtext-dis[12] THEN YES ELSE NO
               asi.mkt-text[13]:hidden = IF NOT mkt-ref.mtext-dis[13] THEN YES ELSE NO
               asi.mkt-text[14]:hidden = IF NOT mkt-ref.mtext-dis[14] THEN YES ELSE NO
               asi.mkt-text[15]:hidden = IF NOT mkt-ref.mtext-dis[15] THEN YES ELSE NO
               asi.mkt-text[16]:hidden = IF NOT mkt-ref.mtext-dis[16] THEN YES ELSE NO
               asi.mkt-text[17]:hidden = IF NOT mkt-ref.mtext-dis[17] THEN YES ELSE NO
               asi.mkt-text[18]:hidden = IF NOT mkt-ref.mtext-dis[18] THEN YES ELSE NO
               .
  END.
  v-industry-list = v-industry-list + "".
  DISPLAY v-ind-label-1 v-ind-label-2 v-ind-label-3 v-ind-label-4
          v-ind-label-5 v-ind-label-6 v-ind-label-7
          v-ind-label-8 v-ind-label-9 v-ind-label-10
          WITH FRAME {&FRAME-NAME}.
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
/*  
  IF INDEX(v-industry-list,asi.industry:SCREEN-VALUE IN FRAME {&FRAME-NAME}) <= 0
  THEN DO:
      MESSAGE "Invalid Industry. Try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO asi.industry.
      RETURN .
  END.
*/
  IF asi.TYPE:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" THEN DO:
     IF NOT CAN-FIND(first susptype where susptype eq asi.TYPE:SCREEN-VALUE IN FRAME {&FRAME-NAME} ) THEN DO:
        MESSAGE "Invalid Type. Try Help." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO asi.TYPE.
        RETURN.
     END.
  END.
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
  {src/adm/template/snd-list.i "asi"}

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

