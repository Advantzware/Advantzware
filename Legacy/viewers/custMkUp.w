&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/custMkUp.w

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters: <none>

  Output Parameters: <none>

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

&scoped-def proc-enable proc-enable

{sys/inc/var.i "NEW SHARED"}

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
&Scoped-define EXTERNAL-TABLES cust-markup
&Scoped-define FIRST-EXTERNAL-TABLE cust-markup


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR cust-markup.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS cust-markup.cust-no cust-markup.procat ~
cust-markup.style cust-markup.run-qty[1] cust-markup.markup[1] ~
cust-markup.lookup_reduction[1] cust-markup.markup_reduction[1] ~
cust-markup.run-qty[2] cust-markup.markup[2] ~
cust-markup.lookup_reduction[2] cust-markup.markup_reduction[2] ~
cust-markup.run-qty[3] cust-markup.markup[3] ~
cust-markup.lookup_reduction[3] cust-markup.markup_reduction[3] ~
cust-markup.run-qty[4] cust-markup.markup[4] ~
cust-markup.lookup_reduction[4] cust-markup.markup_reduction[4] ~
cust-markup.run-qty[5] cust-markup.markup[5] ~
cust-markup.lookup_reduction[5] cust-markup.markup_reduction[5] ~
cust-markup.run-qty[6] cust-markup.markup[6] ~
cust-markup.lookup_reduction[6] cust-markup.markup_reduction[6] ~
cust-markup.run-qty[7] cust-markup.markup[7] ~
cust-markup.lookup_reduction[7] cust-markup.markup_reduction[7] ~
cust-markup.run-qty[8] cust-markup.markup[8] ~
cust-markup.lookup_reduction[8] cust-markup.markup_reduction[8] ~
cust-markup.run-qty[9] cust-markup.markup[9] ~
cust-markup.lookup_reduction[9] cust-markup.markup_reduction[9] ~
cust-markup.run-qty[10] cust-markup.markup[10] ~
cust-markup.lookup_reduction[10] cust-markup.markup_reduction[10] 
&Scoped-define ENABLED-TABLES cust-markup
&Scoped-define FIRST-ENABLED-TABLE cust-markup
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-10 
&Scoped-Define DISPLAYED-FIELDS cust-markup.cust-no cust-markup.procat ~
cust-markup.style cust-markup.run-qty[1] cust-markup.markup[1] ~
cust-markup.lookup_reduction[1] cust-markup.markup_reduction[1] ~
cust-markup.run-qty[2] cust-markup.markup[2] ~
cust-markup.lookup_reduction[2] cust-markup.markup_reduction[2] ~
cust-markup.run-qty[3] cust-markup.markup[3] ~
cust-markup.lookup_reduction[3] cust-markup.markup_reduction[3] ~
cust-markup.run-qty[4] cust-markup.markup[4] ~
cust-markup.lookup_reduction[4] cust-markup.markup_reduction[4] ~
cust-markup.run-qty[5] cust-markup.markup[5] ~
cust-markup.lookup_reduction[5] cust-markup.markup_reduction[5] ~
cust-markup.run-qty[6] cust-markup.markup[6] ~
cust-markup.lookup_reduction[6] cust-markup.markup_reduction[6] ~
cust-markup.run-qty[7] cust-markup.markup[7] ~
cust-markup.lookup_reduction[7] cust-markup.markup_reduction[7] ~
cust-markup.run-qty[8] cust-markup.markup[8] ~
cust-markup.lookup_reduction[8] cust-markup.markup_reduction[8] ~
cust-markup.run-qty[9] cust-markup.markup[9] ~
cust-markup.lookup_reduction[9] cust-markup.markup_reduction[9] ~
cust-markup.run-qty[10] cust-markup.markup[10] ~
cust-markup.lookup_reduction[10] cust-markup.markup_reduction[10] 
&Scoped-define DISPLAYED-TABLES cust-markup
&Scoped-define FIRST-DISPLAYED-TABLE cust-markup
&Scoped-Define DISPLAYED-OBJECTS custName procatDscr styleDscr ~
fiLookupValue fiLookupValueRed cb_markup-on-01 cb_markup-on-02 ~
cb_markup-on-03 cb_markup-on-04 cb_markup-on-05 cb_markup-on-06 ~
cb_markup-on-07 cb_markup-on-08 cb_markup-on-09 cb_markup-on-10 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS cust-markup.markup[1] cb_markup-on-01 ~
cust-markup.markup[2] cb_markup-on-02 cust-markup.markup[3] cb_markup-on-03 ~
cust-markup.markup[4] cb_markup-on-04 cust-markup.markup[5] cb_markup-on-05 ~
cust-markup.markup[6] cb_markup-on-06 cust-markup.markup[7] cb_markup-on-07 ~
cust-markup.markup[8] cb_markup-on-08 cust-markup.markup[9] cb_markup-on-09 ~
cust-markup.markup[10] cb_markup-on-10 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
company||y|asi.cust-markup.company
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCustName V-table-Win 
FUNCTION getCustName RETURNS CHARACTER
    ( ipCustNo AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProCatDscr V-table-Win 
FUNCTION getProCatDscr RETURNS CHARACTER
    ( ipProCat AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStyleDscr V-table-Win 
FUNCTION getStyleDscr RETURNS CHARACTER
    ( ipStyle AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE cb_markup-on-01 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE cb_markup-on-02 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE cb_markup-on-03 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE cb_markup-on-04 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE cb_markup-on-05 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE cb_markup-on-06 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE cb_markup-on-07 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE cb_markup-on-08 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE cb_markup-on-09 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE cb_markup-on-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE custName AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE fiLookupValue AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fiLookupValueRed AS CHARACTER FORMAT "X(256)":U INITIAL "Board % of Cost" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE procatDscr AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE styleDscr AS CHARACTER FORMAT "x(25)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 17.14.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 13.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cust-markup.cust-no AT ROW 1.24 COL 14 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 
     custName AT ROW 1.24 COL 27 COLON-ALIGNED HELP
          "Enter the customer's name." NO-LABEL WIDGET-ID 8
     cust-markup.procat AT ROW 2.43 COL 14 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 
     procatDscr AT ROW 2.43 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     cust-markup.style AT ROW 3.62 COL 14 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          BGCOLOR 15 
     styleDscr AT ROW 3.62 COL 27 COLON-ALIGNED HELP
          "Enter Description of Style" NO-LABEL WIDGET-ID 12
     fiLookupValue AT ROW 4.81 COL 6 COLON-ALIGNED NO-LABEL WIDGET-ID 80
     fiLookupValueRed AT ROW 4.81 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     cust-markup.run-qty[1] AT ROW 6 COL 8 NO-LABEL WIDGET-ID 56 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[1] AT ROW 6 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 36 FORMAT ">9.9999"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-01 AT ROW 6 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     cust-markup.lookup_reduction[1] AT ROW 6 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 92
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.markup_reduction[1] AT ROW 6 COL 91 COLON-ALIGNED NO-LABEL WIDGET-ID 112 
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.run-qty[2] AT ROW 7.19 COL 8 NO-LABEL WIDGET-ID 58 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[2] AT ROW 7.19 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 38 FORMAT ">9.9999"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-02 AT ROW 7.19 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     cust-markup.lookup_reduction[2] AT ROW 7.19 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 94
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.markup_reduction[2] AT ROW 7.19 COL 91 COLON-ALIGNED NO-LABEL WIDGET-ID 114
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.run-qty[3] AT ROW 8.38 COL 8 NO-LABEL WIDGET-ID 60 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[3] AT ROW 8.38 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 40 FORMAT ">9.9999"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-03 AT ROW 8.38 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     cust-markup.lookup_reduction[3] AT ROW 8.38 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 96
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.markup_reduction[3] AT ROW 8.38 COL 91 COLON-ALIGNED NO-LABEL WIDGET-ID 116
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.run-qty[4] AT ROW 9.57 COL 8 NO-LABEL WIDGET-ID 62 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[4] AT ROW 9.57 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 42 FORMAT ">9.9999"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-04 AT ROW 9.57 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     cust-markup.lookup_reduction[4] AT ROW 9.57 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 98
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     cust-markup.markup_reduction[4] AT ROW 9.57 COL 91 COLON-ALIGNED NO-LABEL WIDGET-ID 118
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.run-qty[5] AT ROW 10.76 COL 8 NO-LABEL WIDGET-ID 64 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[5] AT ROW 10.76 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 44 FORMAT ">9.9999"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-05 AT ROW 10.76 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     cust-markup.lookup_reduction[5] AT ROW 10.76 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 100
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.markup_reduction[5] AT ROW 10.76 COL 91 COLON-ALIGNED NO-LABEL WIDGET-ID 120
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.run-qty[6] AT ROW 11.95 COL 8 NO-LABEL WIDGET-ID 66 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[6] AT ROW 11.95 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 46 FORMAT ">9.9999"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-06 AT ROW 11.95 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     cust-markup.lookup_reduction[6] AT ROW 11.95 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 102
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.markup_reduction[6] AT ROW 11.95 COL 91 COLON-ALIGNED NO-LABEL WIDGET-ID 122
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.run-qty[7] AT ROW 13.14 COL 8 NO-LABEL WIDGET-ID 68 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[7] AT ROW 13.14 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 48  FORMAT ">9.9999"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-07 AT ROW 13.14 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     cust-markup.lookup_reduction[7] AT ROW 13.14 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 104
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.markup_reduction[7] AT ROW 13.14 COL 91 COLON-ALIGNED NO-LABEL WIDGET-ID 124
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.run-qty[8] AT ROW 14.33 COL 8 NO-LABEL WIDGET-ID 70 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[8] AT ROW 14.33 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 50 FORMAT ">9.9999"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-08 AT ROW 14.33 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     cust-markup.lookup_reduction[8] AT ROW 14.33 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 106
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.markup_reduction[8] AT ROW 14.33 COL 91 COLON-ALIGNED NO-LABEL WIDGET-ID 126
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.run-qty[9] AT ROW 15.52 COL 8 NO-LABEL WIDGET-ID 72 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[9] AT ROW 15.52 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 52 FORMAT ">9.9999"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     cb_markup-on-09 AT ROW 15.52 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     cust-markup.lookup_reduction[9] AT ROW 15.52 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 108
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.markup_reduction[9] AT ROW 15.52 COL 91 COLON-ALIGNED NO-LABEL WIDGET-ID 128
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.run-qty[10] AT ROW 16.71 COL 8 NO-LABEL WIDGET-ID 54 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[10] AT ROW 16.71 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 34 FORMAT ">9.9999"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-10 AT ROW 16.71 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     cust-markup.lookup_reduction[10] AT ROW 16.71 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 90
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cust-markup.markup_reduction[10] AT ROW 16.71 COL 91 COLON-ALIGNED NO-LABEL WIDGET-ID 110
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     "Margin Reduction" VIEW-AS TEXT
          SIZE 23 BY 1 AT ROW 4.81 COL 93 WIDGET-ID 88
     "Margin On" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 4.81 COL 40 WIDGET-ID 74
     "Reduction Matrix" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 3.86 COL 73 WIDGET-ID 84
     "Margin" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 4.81 COL 27 WIDGET-ID 76
     RECT-1 AT ROW 1 COL 1
     RECT-10 AT ROW 4.1 COL 71 WIDGET-ID 82
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.cust-markup
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
         HEIGHT             = 17.19
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX cb_markup-on-01 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR COMBO-BOX cb_markup-on-02 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR COMBO-BOX cb_markup-on-03 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR COMBO-BOX cb_markup-on-04 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR COMBO-BOX cb_markup-on-05 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR COMBO-BOX cb_markup-on-06 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR COMBO-BOX cb_markup-on-07 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR COMBO-BOX cb_markup-on-08 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR COMBO-BOX cb_markup-on-09 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR COMBO-BOX cb_markup-on-10 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN custName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiLookupValue IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiLookupValueRed IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust-markup.markup[10] IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN cust-markup.markup[1] IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN cust-markup.markup[2] IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN cust-markup.markup[3] IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN cust-markup.markup[4] IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN cust-markup.markup[5] IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN cust-markup.markup[6] IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN cust-markup.markup[7] IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN cust-markup.markup[8] IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN cust-markup.markup[9] IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN procatDscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[10] IN FRAME F-Main
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[1] IN FRAME F-Main
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[2] IN FRAME F-Main
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[3] IN FRAME F-Main
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[4] IN FRAME F-Main
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[5] IN FRAME F-Main
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[6] IN FRAME F-Main
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[7] IN FRAME F-Main
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[8] IN FRAME F-Main
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[9] IN FRAME F-Main
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN styleDscr IN FRAME F-Main
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

&Scoped-define SELF-NAME cust-markup.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust-markup.cust-no V-table-Win
ON HELP OF cust-markup.cust-no IN FRAME F-Main /* Ack. Date */
DO:
    DEF VAR char-val AS CHARACTER NO-UNDO.

   RUN windows/l-cust.w (g_company, cust-markup.cust-no:SCREEN-VALUE, OUTPUT char-val).
   cust-markup.cust-no:SCREEN-VALUE = ENTRY(1,char-val) .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust-markup.cust-no V-table-Win
ON LEAVE OF cust-markup.cust-no IN FRAME F-Main /* cust */
DO:
  IF LASTKEY NE -1  THEN DO:
    IF TRIM(cust-markup.cust-no:SCREEN-VALUE) NE "" THEN DO:

      RUN valid-cust-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust-markup.style V-table-Win
ON LEAVE OF cust-markup.style IN FRAME F-Main /* cust */
DO:
  IF LASTKEY NE -1  THEN DO:
    IF TRIM(cust-markup.style:SCREEN-VALUE) NE "" THEN DO:

      RUN valid-style NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust-markup.procat V-table-Win
ON LEAVE OF cust-markup.procat IN FRAME F-Main /* cust */
DO:
  IF LASTKEY NE -1  THEN DO:
    IF TRIM(cust-markup.procat:SCREEN-VALUE) NE "" THEN DO:

      RUN valid-procat NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb_markup-on-01 V-table-Win
ON LEAVE OF cb_markup-on-01 IN FRAME F-Main /* cust */
DO:
    cb_markup-on-01 = cb_markup-on-01:SCREEN-VALUE .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

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
  {src/adm/template/row-list.i "cust-markup"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "cust-markup"}

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
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/


    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        cust-markup.markup-on[01] = SUBSTR(cb_markup-on-01:SCREEN-VALUE,2,1)
        cust-markup.markup-on[02] = SUBSTR(cb_markup-on-02:SCREEN-VALUE,2,1)
        cust-markup.markup-on[03] = SUBSTR(cb_markup-on-03:SCREEN-VALUE,2,1)
        cust-markup.markup-on[04] = SUBSTR(cb_markup-on-04:SCREEN-VALUE,2,1)
        cust-markup.markup-on[05] = SUBSTR(cb_markup-on-05:SCREEN-VALUE,2,1)
        cust-markup.markup-on[06] = SUBSTR(cb_markup-on-06:SCREEN-VALUE,2,1)
        cust-markup.markup-on[07] = SUBSTR(cb_markup-on-07:SCREEN-VALUE,2,1)
        cust-markup.markup-on[08] = SUBSTR(cb_markup-on-08:SCREEN-VALUE,2,1)
        cust-markup.markup-on[09] = SUBSTR(cb_markup-on-09:SCREEN-VALUE,2,1)
        cust-markup.markup-on[10] = SUBSTR(cb_markup-on-10:SCREEN-VALUE,2,1).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/


    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pDisableAll.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/


    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN
        cust-markup.company   = g_company
        cust-markup.cust-no   = ""
        cust-markup.markup-on = "N".

    RUN pDisplayMarkupOn.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLookupDisplay AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound         AS LOGICAL   NO-UNDO.


    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

    IF AVAIL cust-markup THEN 
    DO:
        RUN pDisplayMarkupOn.
        RUN sys/ref/nk1look.p (cust-markup.company,
            "CEMarkupMatrixLookup",
            "C",
            NO,
            NO,
            "",
            "",
            OUTPUT cLookupDisplay,
            OUTPUT lFound).

        IF NOT lFound OR cLookupDisplay EQ "" THEN 
            cLookupDisplay = "Square Feet".

        fiLookupValue:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cLookupDisplay.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/


    /* Code placed here will execute PRIOR to standard behavior. */

    RUN valid-cust-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-procat NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-style NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.



    RUN pDisableAll.
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

/* Code placed here will execute AFTER standard behavior.    */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisableAll V-table-Win 
PROCEDURE pDisableAll :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        DISABLE ALL.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayMarkupOn V-table-Win 
PROCEDURE pDisplayMarkupOn :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF NOT adm-new-record THEN DO:
            {viewers/custmark.i 01}
            {viewers/custmark.i 02}
            {viewers/custmark.i 03}
            {viewers/custmark.i 04}
            {viewers/custmark.i 05}
            {viewers/custmark.i 06}
            {viewers/custmark.i 07}
            {viewers/custmark.i 08}
            {viewers/custmark.i 09}
            {viewers/custmark.i 10}
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-enable V-table-Win 
PROCEDURE proc-enable :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
/*        IF NOT adm-new-record THEN   */
/*            DISABLE cust-markup.style*/
/*                cust-markup.procat.  */

        ENABLE cb_markup-on-01
            cb_markup-on-02
            cb_markup-on-03
            cb_markup-on-04
            cb_markup-on-05
            cb_markup-on-06
            cb_markup-on-07
            cb_markup-on-08
            cb_markup-on-09
            cb_markup-on-10.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "cust-markup" "company"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "cust-markup"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no V-table-Win 
PROCEDURE valid-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
   IF cust-markup.cust-no:SCREEN-VALUE <> "" THEN do:
    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ g_company 
          AND cust.cust-no EQ cust-markup.cust-no:SCREEN-VALUE NO-ERROR.

    IF NOT AVAIL cust THEN DO:
        MESSAGE "This customer does not exist, Try Help.."
            VIEW-AS ALERT-BOX INFO .
        APPLY "entry" TO cust-markup.cust-no.
        RETURN ERROR.
    END.
   END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-style V-table-Win 
PROCEDURE valid-style :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
   IF cust-markup.style:SCREEN-VALUE <> "" THEN do:
    FIND FIRST style NO-LOCK
        WHERE style.company EQ g_company 
          AND style.style EQ cust-markup.style:SCREEN-VALUE NO-ERROR.

    IF NOT AVAIL style THEN DO:
        MESSAGE "Invalid Style, Try Help.."
            VIEW-AS ALERT-BOX INFO .
        APPLY "entry" TO cust-markup.style.
        RETURN ERROR.
    END.
   END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-procat V-table-Win 
PROCEDURE valid-procat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF cust-markup.procat:SCREEN-VALUE <> "" THEN do:
    FIND FIRST fgcat NO-LOCK
        WHERE fgcat.company EQ g_company 
          AND fgcat.procat EQ cust-markup.procat:SCREEN-VALUE NO-ERROR.
        IF NOT AVAIL fgcat THEN DO:
            MESSAGE "Invalid Category, Try Help.."
                VIEW-AS ALERT-BOX INFO .
            APPLY "entry" TO cust-markup.procat.
            RETURN ERROR.
        END.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCustName V-table-Win 
FUNCTION getCustName RETURNS CHARACTER
    ( ipCustNo AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCust AS CHARACTER NO-UNDO. 

    IF ipCustNo EQ "" THEN 
        cCust = "All Customers".
    ELSE 
    DO:     
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ g_company
            AND cust.cust-no EQ ipCustNo
            NO-ERROR.
        IF AVAILABLE cust THEN 
            cCust = cust.name.
        ELSE 
            cCust = "Invalid Customer".
    END.    
    RETURN cCust.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProCatDscr V-table-Win 
FUNCTION getProCatDscr RETURNS CHARACTER
    ( ipProCat AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.

    IF ipProCat EQ "" THEN 
        cDescription = "All Categories".
    ELSE 
    DO: 
        FIND FIRST fgcat NO-LOCK
            WHERE fgcat.company EQ g_company
            AND fgcat.procat EQ ipProCat
            NO-ERROR.
        IF AVAILABLE fgcat THEN 
            cDescription = fgcat.dscr.
        ELSE
            cDescription = "Invalid Category".
    END.
    RETURN cDescription.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStyleDscr V-table-Win 
FUNCTION getStyleDscr RETURNS CHARACTER
    ( ipStyle AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStyle AS CHARACTER NO-UNDO.

    IF ipStyle EQ "" THEN
        cStyle = "All Styles".
    ELSE 
    DO:

        FIND FIRST style NO-LOCK
            WHERE style.company EQ g_company
            AND style.style EQ ipStyle
            NO-ERROR.
        IF AVAILABLE style THEN 
            cStyle = style.dscr.
        ELSE 
            cStyle = "Invalid Style".
    END.
    RETURN cStyle.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

