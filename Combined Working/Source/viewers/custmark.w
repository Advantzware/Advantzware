&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
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


&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

&scoped-def proc-enable proc-enable

{sys/inc/var.i new shared}

assign
    cocode = g_company
    locode = g_loc.

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
&Scoped-define EXTERNAL-TABLES cust-markup cust
&Scoped-define FIRST-EXTERNAL-TABLE cust-markup


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR cust-markup, cust.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS cust-markup.style cust-markup.procat ~
cust-markup.run-qty[1] cust-markup.markup[1] cust-markup.run-qty[2] ~
cust-markup.markup[2] cust-markup.run-qty[3] cust-markup.markup[3] ~
cust-markup.run-qty[4] cust-markup.markup[4] cust-markup.run-qty[5] ~
cust-markup.markup[5] cust-markup.run-qty[6] cust-markup.markup[6] ~
cust-markup.run-qty[7] cust-markup.markup[7] cust-markup.run-qty[8] ~
cust-markup.markup[8] cust-markup.run-qty[9] cust-markup.markup[9] ~
cust-markup.run-qty[10] cust-markup.markup[10] cust-markup.cust-no 
&Scoped-define ENABLED-TABLES cust-markup
&Scoped-define FIRST-ENABLED-TABLE cust-markup
&Scoped-Define ENABLED-OBJECTS RECT-1 custName procatDscr styleDscr 
&Scoped-Define DISPLAYED-FIELDS cust-markup.style cust-markup.procat ~
cust-markup.run-qty[1] cust-markup.markup[1] cust-markup.run-qty[2] ~
cust-markup.markup[2] cust-markup.run-qty[3] cust-markup.markup[3] ~
cust-markup.run-qty[4] cust-markup.markup[4] cust-markup.run-qty[5] ~
cust-markup.markup[5] cust-markup.run-qty[6] cust-markup.markup[6] ~
cust-markup.run-qty[7] cust-markup.markup[7] cust-markup.run-qty[8] ~
cust-markup.markup[8] cust-markup.run-qty[9] cust-markup.markup[9] ~
cust-markup.run-qty[10] cust-markup.markup[10] cust-markup.cust-no 
&Scoped-define DISPLAYED-TABLES cust-markup
&Scoped-define FIRST-DISPLAYED-TABLE cust-markup
&Scoped-Define DISPLAYED-OBJECTS fiLookupValue cb_markup-on-01 ~
cb_markup-on-02 cb_markup-on-03 cb_markup-on-04 cb_markup-on-05 ~
cb_markup-on-06 cb_markup-on-07 cb_markup-on-08 cb_markup-on-09 ~
cb_markup-on-10 custName procatDscr styleDscr F-4 F-2 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS cust-markup.procat 
&Scoped-define ADM-ASSIGN-FIELDS cust-markup.procat cust-markup.markup[1] ~
cb_markup-on-01 cust-markup.markup[2] cb_markup-on-02 cust-markup.markup[3] ~
cb_markup-on-03 cust-markup.markup[4] cb_markup-on-04 cust-markup.markup[5] ~
cb_markup-on-05 cust-markup.markup[6] cb_markup-on-06 cust-markup.markup[7] ~
cb_markup-on-07 cust-markup.markup[8] cb_markup-on-08 cust-markup.markup[9] ~
cb_markup-on-09 cust-markup.markup[10] cb_markup-on-10 
&Scoped-define F1 F-4 F-2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
company|y|y|ASI.cust-markup.company
uom||y|ASI.cust-markup.markup-on[1]
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "company",
     Keys-Supplied = "company,uom"':U).
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

DEFINE VARIABLE custName AS CHARACTER FORMAT "X(256)":U 
     LABEL "custName" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1 NO-UNDO.

DEFINE VARIABLE F-2 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-4 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE fiLookupValue AS CHARACTER FORMAT "X(256)":U INITIAL "Square Feet" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE procatDscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1 NO-UNDO.

DEFINE VARIABLE styleDscr AS CHARACTER FORMAT "X(256)":U 
     LABEL "styleDscr" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 57 BY 15.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cust-markup.style AT ROW 1.24 COL 16 COLON-ALIGNED
          LABEL "Style"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     cust-markup.procat AT ROW 2.19 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     fiLookupValue AT ROW 3.24 COL 2 NO-LABEL WIDGET-ID 80
     cust-markup.run-qty[1] AT ROW 4.33 COL 2 NO-LABEL FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[1] AT ROW 4.33 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-01 AT ROW 4.33 COL 32 COLON-ALIGNED NO-LABEL
     cust-markup.run-qty[2] AT ROW 5.52 COL 2 NO-LABEL FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[2] AT ROW 5.52 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-02 AT ROW 5.52 COL 32 COLON-ALIGNED NO-LABEL
     cust-markup.run-qty[3] AT ROW 6.71 COL 2 NO-LABEL FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[3] AT ROW 6.71 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-03 AT ROW 6.71 COL 32 COLON-ALIGNED NO-LABEL
     cust-markup.run-qty[4] AT ROW 7.91 COL 2 NO-LABEL FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[4] AT ROW 7.91 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-04 AT ROW 7.91 COL 32 COLON-ALIGNED NO-LABEL
     cust-markup.run-qty[5] AT ROW 9.1 COL 2 NO-LABEL FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[5] AT ROW 9.1 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-05 AT ROW 9.1 COL 32 COLON-ALIGNED NO-LABEL
     cust-markup.run-qty[6] AT ROW 10.29 COL 2 NO-LABEL FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[6] AT ROW 10.29 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-06 AT ROW 10.29 COL 32 COLON-ALIGNED NO-LABEL
     cust-markup.run-qty[7] AT ROW 11.48 COL 2 NO-LABEL FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[7] AT ROW 11.48 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-07 AT ROW 11.48 COL 32 COLON-ALIGNED NO-LABEL
     cust-markup.run-qty[8] AT ROW 12.67 COL 2 NO-LABEL FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[8] AT ROW 12.67 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-08 AT ROW 12.67 COL 32 COLON-ALIGNED NO-LABEL
     cust-markup.run-qty[9] AT ROW 13.86 COL 2 NO-LABEL FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     cust-markup.markup[9] AT ROW 13.86 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-09 AT ROW 13.86 COL 32 COLON-ALIGNED NO-LABEL
     cust-markup.run-qty[10] AT ROW 15.05 COL 2 NO-LABEL FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     cust-markup.markup[10] AT ROW 15.05 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     cb_markup-on-10 AT ROW 15.05 COL 32 COLON-ALIGNED NO-LABEL
     cust-markup.cust-no AT ROW 16.48 COL 11.2 COLON-ALIGNED WIDGET-ID 88
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     custName AT ROW 16.71 COL 27 COLON-ALIGNED WIDGET-ID 90
     procatDscr AT ROW 17.86 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     styleDscr AT ROW 17.86 COL 28 COLON-ALIGNED WIDGET-ID 94
     F-4 AT ROW 1.24 COL 40 NO-LABEL
     F-2 AT ROW 2.19 COL 40 NO-LABEL
     "Margin" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 3.24 COL 21
     "Margin On" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 3.24 COL 34
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.cust-markup,asi.cust
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
         HEIGHT             = 17.86
         WIDTH              = 57.
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
ASSIGN 
       cust-markup.cust-no:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       custName:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-4 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-4:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiLookupValue IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
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
/* SETTINGS FOR FILL-IN cust-markup.procat IN FRAME F-Main
   1 2                                                                  */
ASSIGN 
       procatDscr:HIDDEN IN FRAME F-Main           = TRUE.

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
/* SETTINGS FOR FILL-IN cust-markup.style IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       styleDscr:HIDDEN IN FRAME F-Main           = TRUE.

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

&Scoped-define SELF-NAME cust-markup.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust-markup.procat V-table-Win
ON LEAVE OF cust-markup.procat IN FRAME F-Main /* Category */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-procat NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust-markup.style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust-markup.style V-table-Win
ON LEAVE OF cust-markup.style IN FRAME F-Main /* Style */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-style NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  session:data-entry-return = yes.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'company':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = cust-markup
           &WHERE = "WHERE cust-markup.company eq key-value"
       }
  END CASE.

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
  {src/adm/template/row-list.i "cust"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "cust-markup"}
  {src/adm/template/row-find.i "cust"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-all V-table-Win 
PROCEDURE disable-all :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        DISABLE ALL.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-markup-on V-table-Win 
PROCEDURE display-markup-on :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN
        cust-markup.markup-on[01] = SUBSTR(cb_markup-on-01,2,1)
        cust-markup.markup-on[02] = SUBSTR(cb_markup-on-02,2,1)
        cust-markup.markup-on[03] = SUBSTR(cb_markup-on-03,2,1)
        cust-markup.markup-on[04] = SUBSTR(cb_markup-on-04,2,1)
        cust-markup.markup-on[05] = SUBSTR(cb_markup-on-05,2,1)
        cust-markup.markup-on[06] = SUBSTR(cb_markup-on-06,2,1)
        cust-markup.markup-on[07] = SUBSTR(cb_markup-on-07,2,1)
        cust-markup.markup-on[08] = SUBSTR(cb_markup-on-08,2,1)
        cust-markup.markup-on[09] = SUBSTR(cb_markup-on-09,2,1)
        cust-markup.markup-on[10] = SUBSTR(cb_markup-on-10,2,1).

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
    RUN disable-all.

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
    ASSIGN
        cust-markup.company   = cust.company
        cust-markup.cust-no   = cust.cust-no
        cust-markup.markup-on = "N".

    RUN display-markup-on.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
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
        RUN display-markup-on.
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
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    RUN valid-style NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-procat NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-markup NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN disable-all.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

/* Code placed here will execute AFTER standard behavior.    */

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

    DO WITH FRAME {&FRAME-NAME}:
        IF NOT adm-new-record THEN
            DISABLE cust-markup.style
                cust-markup.procat.

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
  {src/adm/template/sndkycas.i "uom" "cust-markup" "markup-on[1]"}

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
  {src/adm/template/snd-list.i "cust"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-key V-table-Win 
PROCEDURE valid-key :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF BUFFER b-cust-markup FOR cust-markup.


  {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:
        IF CAN-FIND(FIRST b-cust-markup
            WHERE b-cust-markup.company EQ cocode
            AND b-cust-markup.cust-no EQ cust-markup.cust-no
            AND b-cust-markup.style   EQ cust-markup.style:SCREEN-VALUE
            AND b-cust-markup.procat  EQ cust-markup.procat:SCREEN-VALUE
            AND ROWID(b-cust-markup)  NE ROWID(cust-markup))
            THEN 
        DO:
            MESSAGE "This record already exists for customer, please re-enter..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cust-markup.procat.
            RETURN ERROR.
        END.
    END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-markup V-table-Win 
PROCEDURE valid-markup :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:

        IF DEC(cust-markup.markup[1]:SCREEN-VALUE) GE 100 THEN
        DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[1].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[2]:SCREEN-VALUE) GE 100 THEN
        DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[2].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[3]:SCREEN-VALUE) GE 100 THEN
        DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[3].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[4]:SCREEN-VALUE) GE 100 THEN
        DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[4].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[5]:SCREEN-VALUE) GE 100 THEN
        DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[5].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[6]:SCREEN-VALUE) GE 100 THEN
        DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[6].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[7]:SCREEN-VALUE) GE 100 THEN
        DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[7].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[8]:SCREEN-VALUE) GE 100 THEN
        DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[8].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[9]:SCREEN-VALUE) GE 100 THEN
        DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[9].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[10]:SCREEN-VALUE) GE 100 THEN
        DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[10].
            RETURN ERROR.
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
        cust-markup.procat:SCREEN-VALUE =
            CAPS(cust-markup.procat:SCREEN-VALUE).

        IF cust-markup.procat:SCREEN-VALUE NE "" AND
            NOT CAN-FIND(FIRST fgcat
            WHERE fgcat.company EQ cocode
            AND fgcat.procat  EQ cust-markup.procat:SCREEN-VALUE)
            THEN 
        DO:
            MESSAGE TRIM(cust-markup.procat:LABEL) + " is invalid, try help..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cust-markup.procat.
            RETURN ERROR.
        END.

        RUN valid-key NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.
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
        cust-markup.style:SCREEN-VALUE =
            CAPS(cust-markup.style:SCREEN-VALUE).

        IF cust-markup.style:SCREEN-VALUE NE "" AND
            NOT CAN-FIND(FIRST style
            WHERE style.company EQ cocode
            AND style.style   EQ cust-markup.style:SCREEN-VALUE)
            THEN 
        DO:
            MESSAGE TRIM(cust-markup.style:LABEL) + " is invalid, try help..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cust-markup.style.
            RETURN ERROR.
        END.

        RUN valid-key NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.
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

