&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/company.w

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
DEF VAR lv-prev-co-acc AS CHAR NO-UNDO.
DEFINE VARIABLE copyRecord AS LOGICAL NO-UNDO.
DEFINE VARIABLE copyCompany AS CHARACTER NO-UNDO.

&SCOPED-DEFINE enable-company enable-company

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
&Scoped-define EXTERNAL-TABLES company
&Scoped-define FIRST-EXTERNAL-TABLE company


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR company.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS company.fid company.cBgColor company.name ~
company.sid company.addr[1] company.countryCode company.addr[2] ~
company.city company.state company.zip company.subLedgerAP company.co-acc ~
company.num-per company.subLedgerPO company.subLedgerOP company.acc-level ~
company.acc-dig[1] company.acc-dig[2] company.acc-dig[3] company.acc-dig[4] ~
company.acc-dig[5] company.subLedgerWIP company.yend-off ~
company.spare-char-1 company.subLedgerRM company.subLedgerFG ~
company.subLedgerBR company.curr-code 
&Scoped-define ENABLED-TABLES company
&Scoped-define FIRST-ENABLED-TABLE company
&Scoped-Define ENABLED-OBJECTS RECT-1 COMBO-BOX_subLedgerAR 
&Scoped-Define DISPLAYED-FIELDS company.company company.fid company.name ~
company.sid company.addr[1] company.countryCode company.addr[2] ~
company.city company.state company.zip company.subLedgerAP company.co-acc ~
company.num-per company.subLedgerPO company.subLedgerOP company.acc-level ~
company.acc-dig[1] company.acc-dig[2] company.acc-dig[3] company.acc-dig[4] ~
company.acc-dig[5] company.subLedgerWIP company.yend-off ~
company.spare-char-1 company.subLedgerRM company.subLedgerFG ~
company.yend-per company.subLedgerBR company.curr-code 
&Scoped-define DISPLAYED-TABLES company
&Scoped-define FIRST-DISPLAYED-TABLE company
&Scoped-Define DISPLAYED-OBJECTS fiBGColor fiSubLedgerClose lv-first-year ~
lv-prd-num lv-prd-dt1 lv-prd-dt2 c-desc COMBO-BOX_subLedgerAR 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS company.company 
&Scoped-define List-5 company.acc-dig[1] company.acc-dig[2] ~
company.acc-dig[3] company.acc-dig[4] company.acc-dig[5] 

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
DEFINE BUTTON bResetColor 
     LABEL "(Reset)" 
     SIZE 10 BY .71
     FONT 0.

DEFINE VARIABLE COMBO-BOX_subLedgerAR AS LOGICAL FORMAT "yes/no" INITIAL NO 
     LABEL "A/R - Receivables" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Yes",YES,
                     "No",NO
     DROP-DOWN-LIST
     SIZE 11 BY 1
     BGCOLOR 15 FONT 1.

DEFINE VARIABLE c-desc AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE fiBGColor AS CHARACTER FORMAT "X(256)":U INITIAL "BG Color" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 11 BY .95 NO-UNDO.

DEFINE VARIABLE fiSubLedgerClose AS CHARACTER FORMAT "X(256)":U INITIAL "SubLedger Close" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-first-year AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "First Open Year" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lv-prd-dt1 AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-prd-dt2 AS DATE FORMAT "99/99/9999":U 
     LABEL "TO" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE lv-prd-num AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "First Open Period" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE RECTANGLE rBgColor
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 9 BY 1.91
     BGCOLOR 21 FGCOLOR 21 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 130 BY 15.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     company.company AT ROW 1.24 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     company.fid AT ROW 1.48 COL 74 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 FONT 4
     fiBGColor AT ROW 1.48 COL 102 COLON-ALIGNED NO-LABEL
     company.cBgColor AT ROW 1.48 COL 114 COLON-ALIGNED HELP
          "" NO-LABEL FORMAT "x(11)"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     company.name AT ROW 2.43 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     company.sid AT ROW 2.67 COL 74 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 FONT 4
     company.addr[1] AT ROW 3.62 COL 19 COLON-ALIGNED
          LABEL "Address"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     bResetColor AT ROW 3.62 COL 116
     company.countryCode AT ROW 4 COL 83.2 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 FONT 4
     company.addr[2] AT ROW 4.81 COL 19 COLON-ALIGNED
          LABEL "Address"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     fiSubLedgerClose AT ROW 5.05 COL 100 COLON-ALIGNED NO-LABEL
     company.city AT ROW 6 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
          BGCOLOR 15 FONT 4
     company.state AT ROW 6 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 15 FONT 4
     company.zip AT ROW 6 COL 50 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 FONT 4
     company.subLedgerAP AT ROW 6.29 COL 116.6 COLON-ALIGNED
          LABEL "A/P - Payables"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Yes",YES,
                     "No",NO
          DROP-DOWN-LIST
          SIZE 11 BY 1
          BGCOLOR 15 FONT 1
     company.co-acc AT ROW 7.43 COL 39 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     company.num-per AT ROW 7.43 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
          BGCOLOR 15 FONT 4
     company.subLedgerPO AT ROW 7.57 COL 116.6 COLON-ALIGNED
          LABEL "P/O - Purchasing"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Yes",YES,
                     "No",NO
          DROP-DOWN-LIST
          SIZE 11 BY 1
          BGCOLOR 15 FONT 1
     company.subLedgerOP AT ROW 8.81 COL 116.6 COLON-ALIGNED
          LABEL "O/P - Order Processing"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Yes",YES,
                     "No",NO
          DROP-DOWN-LIST
          SIZE 11 BY 1
          BGCOLOR 15 FONT 1
     company.acc-level AT ROW 8.86 COL 39 COLON-ALIGNED
          LABEL "G/L Account - # of Levels"
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
          BGCOLOR 15 FONT 4
     company.acc-dig[1] AT ROW 8.86 COL 52 COLON-ALIGNED
          LABEL "Digits"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     company.acc-dig[2] AT ROW 8.86 COL 56 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     company.acc-dig[3] AT ROW 8.86 COL 60 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     company.acc-dig[4] AT ROW 8.86 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     company.acc-dig[5] AT ROW 8.86 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     company.subLedgerWIP AT ROW 10 COL 116.6 COLON-ALIGNED
          LABEL "WIP - Work in Process"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Yes",YES,
                     "No",NO
          DROP-DOWN-LIST
          SIZE 11 BY 1
          BGCOLOR 15 FONT 1
     company.yend-off AT ROW 10.29 COL 39 COLON-ALIGNED
          LABEL "Calendar Month of Year End"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
          BGCOLOR 15 FONT 4
     company.spare-char-1 AT ROW 10.29 COL 74 COLON-ALIGNED HELP
          "" WIDGET-ID 2
          LABEL "Seq. Suffix" FORMAT "x(2)"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     company.subLedgerRM AT ROW 11.24 COL 116.6 COLON-ALIGNED
          LABEL "R/M Inventory"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Yes",YES,
                     "No",NO
          DROP-DOWN-LIST
          SIZE 11 BY 1
          BGCOLOR 15 FONT 1
     lv-first-year AT ROW 11.48 COL 39 COLON-ALIGNED
     company.subLedgerFG AT ROW 12.48 COL 116.6 COLON-ALIGNED
          LABEL "F/G Inventory"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Yes",YES,
                     "No",NO
          DROP-DOWN-LIST
          SIZE 11 BY 1
          BGCOLOR 15 FONT 1
     company.yend-per AT ROW 12.67 COL 41
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 5 BY .81
     lv-prd-num AT ROW 13.62 COL 39 COLON-ALIGNED
     lv-prd-dt1 AT ROW 13.62 COL 48 COLON-ALIGNED NO-LABEL
     lv-prd-dt2 AT ROW 13.62 COL 70 COLON-ALIGNED
     company.subLedgerBR AT ROW 13.67 COL 116.6 COLON-ALIGNED
          LABEL "Bank Reconciliation"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Yes",YES,
                     "No",NO
          DROP-DOWN-LIST
          SIZE 11 BY 1
          BGCOLOR 15 FONT 1
     company.curr-code AT ROW 14.81 COL 39 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     c-desc AT ROW 14.81 COL 47 COLON-ALIGNED NO-LABEL
     COMBO-BOX_subLedgerAR AT ROW 14.86 COL 116.6 COLON-ALIGNED
     "Previous Year Closed?" VIEW-AS TEXT
          SIZE 28 BY .95 AT ROW 12.67 COL 12
     RECT-1 AT ROW 1 COL 1
     rBgColor AT ROW 2.43 COL 105
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.company
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
         HEIGHT             = 16.24
         WIDTH              = 132.2.
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

/* SETTINGS FOR FILL-IN company.acc-dig[1] IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN company.acc-dig[2] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN company.acc-dig[3] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN company.acc-dig[4] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN company.acc-dig[5] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN company.acc-level IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN company.addr[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN company.addr[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR BUTTON bResetColor IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN company.cBgColor IN FRAME F-Main
   NO-DISPLAY EXP-LABEL EXP-FORMAT EXP-HELP                             */
ASSIGN 
       company.cBgColor:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN company.company IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fiBGColor IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiBGColor:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fiSubLedgerClose IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiSubLedgerClose:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN lv-first-year IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-prd-dt1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-prd-dt2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-prd-num IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rBgColor IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN company.spare-char-1 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR COMBO-BOX company.subLedgerAP IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX company.subLedgerBR IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX company.subLedgerFG IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX company.subLedgerOP IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX company.subLedgerPO IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX company.subLedgerRM IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX company.subLedgerWIP IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN company.yend-off IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX company.yend-per IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
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

&Scoped-define SELF-NAME company.acc-level
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL company.acc-level V-table-Win
ON LEAVE OF company.acc-level IN FRAME F-Main /* G/L Account - # of Levels */
DO:
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  i = INTEGER({&SELF-NAME}:SCREEN-VALUE).
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&LIST-5}.
    ASSIGN
      company.acc-dig[1]:FGCOLOR = IF i GE 1 THEN ? ELSE 15
      company.acc-dig[1]:BGCOLOR = IF i GE 1 THEN 15 ELSE 7
      company.acc-dig[1]:SCREEN-VALUE = IF i GE 1 THEN company.acc-dig[1]:SCREEN-VALUE ELSE '0'
      company.acc-dig[2]:FGCOLOR = IF i GE 2 THEN ? ELSE 15
      company.acc-dig[2]:BGCOLOR = IF i GE 2 THEN 15 ELSE 7
      company.acc-dig[2]:SCREEN-VALUE = IF i GE 2 THEN company.acc-dig[2]:SCREEN-VALUE ELSE '0'
      company.acc-dig[3]:FGCOLOR = IF i GE 3 THEN ? ELSE 15
      company.acc-dig[3]:BGCOLOR = IF i GE 3 THEN 15 ELSE 7
      company.acc-dig[3]:SCREEN-VALUE = IF i GE 3 THEN company.acc-dig[3]:SCREEN-VALUE ELSE '0'
      company.acc-dig[4]:FGCOLOR = IF i GE 4 THEN ? ELSE 15
      company.acc-dig[4]:BGCOLOR = IF i GE 4 THEN 15 ELSE 7
      company.acc-dig[4]:SCREEN-VALUE = IF i GE 4 THEN company.acc-dig[4]:SCREEN-VALUE ELSE '0'
      company.acc-dig[5]:FGCOLOR = IF i GE 5 THEN ? ELSE 15
      company.acc-dig[5]:BGCOLOR = IF i GE 5 THEN 15 ELSE 7
      company.acc-dig[5]:SCREEN-VALUE = IF i GE 5 THEN company.acc-dig[5]:SCREEN-VALUE ELSE '0'.
  END.
  DO i = 1 TO INTEGER({&SELF-NAME}:SCREEN-VALUE) WITH FRAME {&FRAME-NAME}:
    ENABLE company.acc-dig[i].
  END.
  APPLY 'ENTRY' TO company.acc-dig[1].
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bResetColor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bResetColor V-table-Win
ON CHOOSE OF bResetColor IN FRAME F-Main /* (Reset) */
DO:
        DEFINE VARIABLE red   AS INTEGER NO-UNDO.
        DEFINE VARIABLE blue  AS INTEGER NO-UNDO INITIAL 127.
        DEFINE VARIABLE green AS INTEGER NO-UNDO INITIAL 127.
        DEFINE VARIABLE ix    AS INTEGER NO-UNDO.
        DEFINE VARIABLE lSave AS LOG NO-UNDO.

        IF users.securityLevel LT 900 THEN RETURN NO-APPLY.
    
        ix = COLOR-TABLE:NUM-ENTRIES.
        COLOR-TABLE:NUM-ENTRIES = ix + 1.
        COLOR-TABLE:SET-DYNAMIC(ix, TRUE).
        COLOR-TABLE:SET-RED-VALUE(ix,119).
        COLOR-TABLE:SET-GREEN-VALUE(ix,150).
        COLOR-TABLE:SET-BLUE-VALUE(ix,203).

        ASSIGN 
            rBgColor:FGCOLOR = ix
            rBgColor:BGCOLOR = ix
            company.cBgColor:SCREEN-VALUE = "".
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME company.co-acc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL company.co-acc V-table-Win
ON ENTRY OF company.co-acc IN FRAME F-Main /* Accounts Company */
DO:
  IF lv-prev-co-acc NE {&self-name}:SCREEN-VALUE THEN
    APPLY "value-changed" TO {&self-name}.

  lv-prev-co-acc = {&self-name}:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL company.co-acc V-table-Win
ON LEAVE OF company.co-acc IN FRAME F-Main /* Accounts Company */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-co-acc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL company.co-acc V-table-Win
ON VALUE-CHANGED OF company.co-acc IN FRAME F-Main /* Accounts Company */
DO:    
  DEF BUFFER bf-company FOR company.


  IF {&self-name}:SCREEN-VALUE NE "" THEN DO:
    {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */

    FIND FIRST bf-company
        WHERE bf-company.company EQ {&self-name}:SCREEN-VALUE
          AND ROWID(bf-company)  NE ROWID(company)
        NO-LOCK NO-ERROR.
    IF AVAIL bf-company THEN
      ASSIGN
       company.num-per:SCREEN-VALUE    = STRING(bf-company.num-per)
       company.acc-level:SCREEN-VALUE  = STRING(bf-company.acc-level)
       company.acc-dig[1]:SCREEN-VALUE = STRING(bf-company.acc-dig[1])
       company.acc-dig[2]:SCREEN-VALUE = STRING(bf-company.acc-dig[2])
       company.acc-dig[3]:SCREEN-VALUE = STRING(bf-company.acc-dig[3])
       company.acc-dig[4]:SCREEN-VALUE = STRING(bf-company.acc-dig[4])
       company.acc-dig[5]:SCREEN-VALUE = STRING(bf-company.acc-dig[5]).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME company.company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL company.company V-table-Win
ON LEAVE OF company.company IN FRAME F-Main /* Company */
DO:
  RUN enable-co-acc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME company.curr-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL company.curr-code V-table-Win
ON LEAVE OF company.curr-code IN FRAME F-Main /* Currency Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-curr-code (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL company.curr-code V-table-Win
ON VALUE-CHANGED OF company.curr-code IN FRAME F-Main /* Currency Code */
DO:
  FIND FIRST currency NO-LOCK
      WHERE currency.company EQ company.company:SCREEN-VALUE
        AND currency.c-code  EQ {&self-name}:SCREEN-VALUE
      NO-ERROR.
  IF AVAIL currency THEN c-desc:SCREEN-VALUE = currency.c-desc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rBgColor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rBgColor V-table-Win
ON MOUSE-SELECT-DBLCLICK OF rBgColor IN FRAME F-Main
DO:
    DEFINE VARIABLE red   AS INTEGER NO-UNDO.
    DEFINE VARIABLE blue  AS INTEGER NO-UNDO INITIAL 127.
    DEFINE VARIABLE green AS INTEGER NO-UNDO INITIAL 127.
    DEFINE VARIABLE ix    AS INTEGER NO-UNDO.
    DEFINE VARIABLE lSave AS LOG NO-UNDO.

    IF users.securityLevel LT 900 THEN RETURN NO-APPLY.
    
    ix = COLOR-TABLE:NUM-ENTRIES.
    COLOR-TABLE:NUM-ENTRIES = ix + 1.
    COLOR-TABLE:SET-DYNAMIC(ix, TRUE).

    SYSTEM-DIALOG COLOR ix UPDATE lSave. 
    
    IF lSave THEN DO:
        ASSIGN 
            rBgColor:FGCOLOR = ix
            rBgColor:BGCOLOR = ix
            company.cBgColor:SCREEN-VALUE = STRING(COLOR-TABLE:get-red-value(ix)) + "," +
                               STRING(COLOR-TABLE:get-green-value(ix)) + "," +
                               STRING(COLOR-TABLE:get-blue-value(ix)).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME company.spare-char-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL company.spare-char-1 V-table-Win
ON LEAVE OF company.spare-char-1 IN FRAME F-Main /* Seq. Suffix */
DO:

  IF LASTKEY NE -1 THEN DO:
    RUN valid-seq-suffix (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
    FIND FIRST users NO-LOCK WHERE 
        users.user_id EQ USERID(LDBNAME(1))
        NO-ERROR.

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
  {src/adm/template/row-list.i "company"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "company"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-controls V-table-Win 
PROCEDURE create-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF NOT AVAIL company  THEN RETURN.

 find first gl-ctrl where gl-ctrl.company = company.company no-lock no-error.
 if not available gl-ctrl then do:
                create gl-ctrl. gl-ctrl.company = company.company.
 end.
 find first rm-ctrl where rm-ctrl.company = company.company no-lock no-error.
 if not available rm-ctrl then do:
                        create rm-ctrl. rm-ctrl.company = company.company.
 end.
 find first fg-ctrl where fg-ctrl.company = company.company no-lock no-error.
 if not available fg-ctrl then do:
                        create fg-ctrl. fg-ctrl.company = company.company.
 end.
 find first ap-ctrl where ap-ctrl.company = company.company no-lock no-error.
 if not available ap-ctrl then do:
                        create ap-ctrl. ap-ctrl.company = company.company.
 end.
 find first ar-ctrl where ar-ctrl.company = company.company no-lock no-error.
 if not available ar-ctrl then do:
                create ar-ctrl. ar-ctrl.company = company.company.
 end.
 find first oe-ctrl where oe-ctrl.company = company.company no-lock no-error.
 if not available oe-ctrl then do:
               create oe-ctrl. oe-ctrl.company = company.company.
 end.

 create loc.
 assign loc.company = company.company
        loc.loc     = "Main"
        loc.dscr    = "Main".

 CREATE location.
 ASSIGN 
        location.company      = loc.company
        location.locationCode = loc.loc
        loc.addrRecKey        = location.rec_key.

 for each loc where loc.company = company.company:
         find first ce-ctrl where ce-ctrl.company = company.company and
                                  ce-ctrl.loc = loc.loc
                                                  no-lock no-error.
     if not available ce-ctrl then do:
                    create ce-ctrl.
                    assign
                    ce-ctrl.company = company.company
                    ce-ctrl.loc     = loc.loc.
         end.
 end.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-co-acc V-table-Win 
PROCEDURE enable-co-acc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST account
        WHERE account.company EQ company.company:SCREEN-VALUE
          AND company.company:SCREEN-VALUE NE ""
        NO-LOCK NO-ERROR.

    IF AVAIL account THEN DISABLE company.co-acc.
                     ELSE ENABLE company.co-acc.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-company V-table-Win 
PROCEDURE enable-company :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF adm-new-record AND NOT adm-adding-record THEN DO: /*copy*/
     MESSAGE "TO ENSURE DATA INTEGRITY" SKIP
             "DATABASE MUST BE BACKED UP TO TAPE AND/OR LOCAL DISK DRIVE." SKIP(1)
             "ALL USERS MUST BE LOGGED OUT OF ADVANTZWARE SOFTWARE PRIOR TO COPY OR DELETE!"
         VIEW-AS ALERT-BOX WARNING.
     END.



  RUN enable-co-acc.

    ASSIGN 
        rBgColor:SENSITIVE IN FRAME {&frame-name} = TRUE
        bResetColor:SENSITIVE = TRUE.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  DEF BUFFER bf-account FOR account.
  DEF BUFFER bf-company FOR company.
  DEFINE VARIABLE iNextSuffix AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iCompCnt    AS INTEGER     NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  SESSION:SET-WAIT-STATE ("general").

  DISABLE {&LIST-5} WITH FRAME {&FRAME-NAME}.

  IF adm-new-record THEN DO: 
    RUN create-controls.
    IF company.spare-char-1 EQ "" 
      AND company.spare-char-1:SCREEN-VALUE EQ "" THEN DO:
      iNextSuffix = 0.
      iCompCnt    = 0.
      FOR EACH bf-company NO-LOCK
         BY bf-company.spare-char-1.
        iCompCnt = iCompCnt + 1.
        IF INTEGER(bf-company.spare-char-1) GT iNextSuffix THEN
          iNextSuffix = INTEGER(bf-company.spare-char-1).
      END.

      iNextSuffix = iNextSuffix + 1.
      company.spare-char-1 = STRING((IF iCompCnt EQ 1 THEN 0 ELSE iNextSuffix), "99").
    END.

  END.
  
  IF adm-new-record AND NOT adm-adding-record THEN.
  ELSE do:
      FIND FIRST account WHERE account.company EQ company.company NO-LOCK NO-ERROR.

      IF NOT AVAIL account AND company.co-acc NE "" THEN
      FOR EACH account WHERE account.company EQ company.co-acc NO-LOCK:
        CREATE bf-account.
        BUFFER-COPY account TO bf-account
          ASSIGN
           bf-account.company = company.company.
      END.
  END.

  SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE red   AS INTEGER NO-UNDO.
    DEFINE VARIABLE blue  AS INTEGER NO-UNDO INITIAL 127.
    DEFINE VARIABLE green AS INTEGER NO-UNDO INITIAL 127.
    DEFINE VARIABLE ix    AS INTEGER NO-UNDO.

    ix = COLOR-TABLE:NUM-ENTRIES.
    COLOR-TABLE:NUM-ENTRIES = ix + 1.

    COLOR-TABLE:SET-DYNAMIC(ix, TRUE).
    COLOR-TABLE:SET-RED-VALUE(ix, red).
    COLOR-TABLE:SET-GREEN-VALUE(ix, green).
    COLOR-TABLE:SET-BLUE-VALUE(ix, blue).
  
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    IF AVAIL company THEN
    FIND FIRST currency WHERE currency.company = company.company
                          AND currency.c-code = company.curr-code
                        NO-LOCK NO-ERROR.
    IF AVAIL currency THEN c-desc:SCREEN-VALUE = currency.c-desc.

    lv-prev-co-acc = company.co-acc:SCREEN-VALUE.

    FIND FIRST period WHERE period.company = company.company AND
                            period.pstat NO-LOCK NO-ERROR.
    IF AVAIL period THEN ASSIGN lv-first-year = period.yr
                                lv-prd-num = period.pnum
                                lv-prd-dt1 = period.pst
                                lv-prd-dt2 = period.pend.
    DISPLAY lv-first-year lv-prd-num lv-prd-dt1 lv-prd-dt2 WITH FRAME {&FRAME-NAME}.
    IF company.cBgColor NE "" THEN DO:
        COLOR-TABLE:SET-DYNAMIC(ix, TRUE).
        COLOR-TABLE:SET-RED-VALUE(ix, INTEGER(ENTRY(1,company.cBgColor))).
        COLOR-TABLE:SET-GREEN-VALUE(ix, INTEGER(ENTRY(2,company.cBgColor))).
        COLOR-TABLE:SET-BLUE-VALUE(ix, INTEGER(ENTRY(3,company.cBgColor))).
    END.
    ELSE DO:
        COLOR-TABLE:SET-DYNAMIC(ix, TRUE).
        COLOR-TABLE:SET-RED-VALUE(ix, 119).
        COLOR-TABLE:SET-GREEN-VALUE(ix, 150).
        COLOR-TABLE:SET-BLUE-VALUE(ix, 203).
    END.
    ASSIGN 
        rBgColor:FGCOLOR = ix
        rBgColor:BGCOLOR = ix.
        
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
  RUN valid-co-acc.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-curr-code (company.curr-code:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF copyRecord THEN
  DO:
    /* gdm - 11070801 */
    RUN custom/companyCopy.w (copyCompany,company.company).
    ASSIGN
      copyCompany = ''
      copyRecord = NO.
  END.
  IF company.spare-char-1:SCREEN-VALUE EQ "" THEN
    company.spare-char-1:SCREEN-VALUE = company.spare-char-1.
    
    ASSIGN 
        rBgColor:SENSITIVE IN FRAME {&frame-name} = FALSE 
        bResetColor:SENSITIVE = FALSE.
    
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
  {src/adm/template/snd-list.i "company"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-buttons V-table-Win 
PROCEDURE valid-buttons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*add and delete not valid buttons*/
    DEF OUTPUT PARAMETER op-add-valid AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER op-delete-valid AS LOG INIT YES NO-UNDO.

    DEF VAR hPgmSecurity AS HANDLE NO-UNDO.
    DEF VAR lResult AS LOG NO-UNDO.
    RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
    RUN epCanAccess IN hPgmSecurity ("viewers/company.w", "", OUTPUT lResult).
    DELETE OBJECT hPgmSecurity.

    ASSIGN 
        op-add-valid = lResult.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-co-acc V-table-Win 
PROCEDURE valid-co-acc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF BUFFER bf-company FOR company.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF company.co-acc:SCREEN-VALUE NE ""                           AND
       company.co-acc:SCREEN-VALUE NE company.company:SCREEN-VALUE THEN DO:
      FIND FIRST bf-company
          WHERE bf-company.company EQ company.co-acc:SCREEN-VALUE
            AND ROWID(bf-company)  NE ROWID(company)
          NO-LOCK NO-ERROR.
      IF NOT AVAIL bf-company THEN DO:
        MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" to company.co-acc.
        RETURN ERROR.
      END.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-curr-code V-table-Win 
PROCEDURE valid-curr-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF (NOT adm-new-record AND
       NOT CAN-FIND(FIRST currency
                    WHERE currency.company EQ company.company:SCREEN-VALUE
                      AND currency.c-code  EQ ip-focus:SCREEN-VALUE)) OR
       (adm-new-record AND
       NOT CAN-FIND(FIRST currency WHERE
                    currency.c-code  EQ ip-focus:SCREEN-VALUE)) THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-seq-suffix V-table-Win 
PROCEDURE valid-seq-suffix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.
  DEF VAR iCurrOrd AS INT NO-UNDO.

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:

    ip-focus:SCREEN-VALUE = STRING(INTEGER(ip-focus:SCREEN-VALUE), "99") NO-ERROR.
    IF ip-focus:SCREEN-VALUE = "00" THEN
        ip-focus:SCREEN-VALUE = "".

    iCurrOrd = DYNAMIC-CURRENT-VALUE("order_seq" + ip-focus:SCREEN-VALUE, "ASI") NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

