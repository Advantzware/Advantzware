&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: fg\v-eitem.w

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
{custom/persist.i}

DEF BUFFER bf-itemfg FOR itemfg.
DEF BUFFER b-e-itemfg-vend FOR e-itemfg-vend.
DEF BUFFER b-reftable-1 FOR reftable.

DEF TEMP-TABLE tmpfile 
    FIELD siz AS DEC
    FIELD qty AS DEC
    FIELD setups AS DEC.

DEF VAR lv-roll-w LIKE e-itemfg.roll-w NO-UNDO.
DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR cRtnChar AS CHAR NO-UNDO.
DEF VAR lRecFound AS LOG NO-UNDO.
DEF VAR lVendCostMtx AS LOG NO-UNDO.
DEF VAR gTerm AS cha NO-UNDO.
DEF VAR gNewVendor AS LOG NO-UNDO.
DEF VAR lCopyRecord AS LOG NO-UNDO.
DEF VAR lAddRecord AS LOG NO-UNDO.

RUN sys/ref/nk1look.p ( g_company, 
                        "VendCostMatrix", 
                        "L" /* Logical */, 
                        NO /* check by cust */, 
                        YES /* use cust not vendor */, 
                        "" /* cust */, 
                        "" /* ship-to*/,
                        OUTPUT cRtnChar, 
                        OUTPUT lRecFound).
ASSIGN 
    lVendCostMtx = LOGICAL(cRtnChar) NO-ERROR .

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
&Scoped-define EXTERNAL-TABLES e-itemfg-vend e-itemfg
&Scoped-define FIRST-EXTERNAL-TABLE e-itemfg-vend


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR e-itemfg-vend, e-itemfg.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS e-itemfg-vend.vend-item e-itemfg-vend.cust-no ~
e-itemfg-vend.vend-no e-itemfg-vend.spare-dec-1 e-itemfg-vend.roll-w[27] ~
e-itemfg-vend.roll-w[28] e-itemfg-vend.roll-w[29] e-itemfg-vend.roll-w[30] 
&Scoped-define ENABLED-TABLES e-itemfg-vend
&Scoped-define FIRST-ENABLED-TABLE e-itemfg-vend
&Scoped-Define ENABLED-OBJECTS tb_sel tb_sel-01 tb_sel-02 tb_sel-03 ~
tb_sel-04 tb_sel-05 tb_sel-06 tb_sel-07 tb_sel-08 tb_sel-09 tb_sel-10 ~
RECT-24 RECT-26 
&Scoped-Define DISPLAYED-FIELDS e-itemfg-vend.i-no e-itemfg.std-uom ~
e-itemfg-vend.vend-item e-itemfg-vend.cust-no e-itemfg-vend.vend-no ~
e-itemfg-vend.spare-dec-1 e-itemfg-vend.roll-w[27] e-itemfg-vend.roll-w[28] ~
e-itemfg-vend.roll-w[29] e-itemfg-vend.roll-w[30] 
&Scoped-define DISPLAYED-TABLES e-itemfg-vend e-itemfg
&Scoped-define FIRST-DISPLAYED-TABLE e-itemfg-vend
&Scoped-define SECOND-DISPLAYED-TABLE e-itemfg
&Scoped-Define DISPLAYED-OBJECTS ls-item-name ls-item-dscr ls-vend-name ~
tb_sel tb_sel-01 tb_sel-02 tb_sel-03 tb_sel-04 tb_sel-05 tb_sel-06 ~
tb_sel-07 tb_sel-08 tb_sel-09 tb_sel-10 fi_oh-markup qty-label run-qty-01 ~
run-cost-01 setups-01 run-qty-02 run-cost-02 setups-02 run-qty-03 ~
run-cost-03 setups-03 run-qty-04 run-cost-04 setups-04 run-qty-05 ~
run-cost-05 setups-05 run-qty-06 run-cost-06 setups-06 run-qty-07 ~
run-cost-07 setups-07 run-qty-08 run-cost-08 setups-08 run-qty-09 ~
run-cost-09 setups-09 run-qty-10 run-cost-10 setups-10 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,farmFields,DISPLAY-FIELD,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS e-itemfg.std-uom 
&Scoped-define ADM-ASSIGN-FIELDS e-itemfg.std-uom fi_oh-markup 
&Scoped-define farmFields run-qty-01 run-cost-01 setups-01 run-qty-02 ~
run-cost-02 setups-02 run-qty-03 run-cost-03 setups-03 run-qty-04 ~
run-cost-04 setups-04 run-qty-05 run-cost-05 setups-05 run-qty-06 ~
run-cost-06 setups-06 run-qty-07 run-cost-07 setups-07 run-qty-08 ~
run-cost-08 setups-08 run-qty-09 run-cost-09 setups-09 run-qty-10 ~
run-cost-10 setups-10 
&Scoped-define DISPLAY-FIELD fi_oh-markup 

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
DEFINE BUTTON btnShowVendCostMtx 
     LABEL "*" 
     SIZE 3 BY .81 TOOLTIP "Show Vend Cost Mtx".

DEFINE VARIABLE fi_oh-markup AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "GS&&A O/H Markup %" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ls-item-dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1 NO-UNDO.

DEFINE VARIABLE ls-item-name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1 NO-UNDO.

DEFINE VARIABLE ls-vend-name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1 NO-UNDO.

DEFINE VARIABLE qty-label AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 12.8 BY .81 NO-UNDO.

DEFINE VARIABLE run-cost-01 AS DECIMAL FORMAT ">>,>>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-cost-02 AS DECIMAL FORMAT ">>,>>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-cost-03 AS DECIMAL FORMAT ">>,>>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-cost-04 AS DECIMAL FORMAT ">>,>>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-cost-05 AS DECIMAL FORMAT ">>,>>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-cost-06 AS DECIMAL FORMAT ">>,>>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-cost-07 AS DECIMAL FORMAT ">>,>>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-cost-08 AS DECIMAL FORMAT ">>,>>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-cost-09 AS DECIMAL FORMAT ">>,>>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-cost-10 AS DECIMAL FORMAT ">>,>>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-qty-01 AS DECIMAL FORMAT ">,>>>,>>9.9<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-qty-02 AS DECIMAL FORMAT ">,>>>,>>9.9<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-qty-03 AS DECIMAL FORMAT ">,>>>,>>9.9<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-qty-04 AS DECIMAL FORMAT ">,>>>,>>9.9<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-qty-05 AS DECIMAL FORMAT ">,>>>,>>9.9<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-qty-06 AS DECIMAL FORMAT ">,>>>,>>9.9<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-qty-07 AS DECIMAL FORMAT ">,>>>,>>9.9<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-qty-08 AS DECIMAL FORMAT ">,>>>,>>9.9<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-qty-09 AS DECIMAL FORMAT ">,>>>,>>9.9<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE run-qty-10 AS DECIMAL FORMAT ">,>>>,>>9.9<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE setups-01 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE setups-02 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE setups-03 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE setups-04 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE setups-05 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE setups-06 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE setups-07 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE setups-08 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE setups-09 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE setups-10 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 17.62.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 9.76.

DEFINE VARIABLE tb_sel AS LOGICAL INITIAL no 
     LABEL "Check to pre-select this quantity/" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-01 AS LOGICAL INITIAL no 
     LABEL "cost by vendor" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-02 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-03 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-04 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-05 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-06 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-07 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-08 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-09 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-10 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     e-itemfg-vend.i-no AT ROW 1.24 COL 14 COLON-ALIGNED FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ls-item-name AT ROW 1.24 COL 41 COLON-ALIGNED NO-LABEL
     ls-item-dscr AT ROW 2.19 COL 41 COLON-ALIGNED NO-LABEL
     e-itemfg.std-uom AT ROW 3.38 COL 14 COLON-ALIGNED
          LABEL "Cost UOM"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     e-itemfg-vend.vend-item AT ROW 3.38 COL 50 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
     e-itemfg-vend.cust-no AT ROW 3.38 COL 83.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.6 BY 1
     e-itemfg-vend.vend-no AT ROW 4.57 COL 14 COLON-ALIGNED
          LABEL "Vendor" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ls-vend-name AT ROW 4.57 COL 30 COLON-ALIGNED NO-LABEL
     tb_sel AT ROW 6 COL 53
     tb_sel-01 AT ROW 6.95 COL 53
     tb_sel-02 AT ROW 7.91 COL 53
     tb_sel-03 AT ROW 8.86 COL 53
     tb_sel-04 AT ROW 9.81 COL 53
     tb_sel-05 AT ROW 10.76 COL 53
     tb_sel-06 AT ROW 11.71 COL 53
     tb_sel-07 AT ROW 12.67 COL 53
     tb_sel-08 AT ROW 13.62 COL 53
     tb_sel-09 AT ROW 14.57 COL 53
     tb_sel-10 AT ROW 15.52 COL 53
     e-itemfg-vend.spare-dec-1 AT ROW 13.62 COL 81 COLON-ALIGNED HELP
          ""
          LABEL "Min. Charge"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     fi_oh-markup AT ROW 14.81 COL 81 COLON-ALIGNED
     e-itemfg-vend.roll-w[27] AT ROW 17.43 COL 17 COLON-ALIGNED HELP
          "Enter Sheet Width Minimum"
          LABEL "Sheet Width"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     e-itemfg-vend.roll-w[28] AT ROW 17.43 COL 31.2 COLON-ALIGNED HELP
          "Enter Sheet Width Maximum" NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     e-itemfg-vend.roll-w[29] AT ROW 17.43 COL 55.4 COLON-ALIGNED HELP
          "Enter Sheet Length Minimum"
          LABEL "Length"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     e-itemfg-vend.roll-w[30] AT ROW 17.43 COL 69.6 COLON-ALIGNED HELP
          "Enter Sheet Length Maximum" NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     qty-label AT ROW 6 COL 2.2 NO-LABEL
     run-qty-01 AT ROW 7.05 COL 2.4 NO-LABEL WIDGET-ID 28
     run-cost-01 AT ROW 7.05 COL 16.4 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     setups-01 AT ROW 7.05 COL 33.4 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     run-qty-02 AT ROW 8 COL 2.4 NO-LABEL WIDGET-ID 30
     run-cost-02 AT ROW 8 COL 16.4 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     setups-02 AT ROW 8 COL 33.4 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     run-qty-03 AT ROW 8.95 COL 2.4 NO-LABEL WIDGET-ID 32
     run-cost-03 AT ROW 8.95 COL 16.4 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     setups-03 AT ROW 8.95 COL 33.4 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     run-qty-04 AT ROW 9.91 COL 2.4 NO-LABEL WIDGET-ID 34
     run-cost-04 AT ROW 9.91 COL 16.4 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     setups-04 AT ROW 9.91 COL 33.4 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     run-qty-05 AT ROW 10.86 COL 2.4 NO-LABEL WIDGET-ID 36
     run-cost-05 AT ROW 10.86 COL 16.4 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     setups-05 AT ROW 10.86 COL 33.4 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     run-qty-06 AT ROW 11.81 COL 2.4 NO-LABEL WIDGET-ID 38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     run-cost-06 AT ROW 11.81 COL 16.4 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     setups-06 AT ROW 11.81 COL 33.4 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     run-qty-07 AT ROW 12.76 COL 2.4 NO-LABEL WIDGET-ID 40
     run-cost-07 AT ROW 12.76 COL 16.4 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     setups-07 AT ROW 12.76 COL 33.4 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     run-qty-08 AT ROW 13.71 COL 2.4 NO-LABEL WIDGET-ID 42
     run-cost-08 AT ROW 13.71 COL 16.4 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     setups-08 AT ROW 13.71 COL 33.4 COLON-ALIGNED NO-LABEL WIDGET-ID 62
     run-qty-09 AT ROW 14.67 COL 2.4 NO-LABEL WIDGET-ID 44
     run-cost-09 AT ROW 14.67 COL 16.4 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     setups-09 AT ROW 14.67 COL 33.4 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     run-qty-10 AT ROW 15.62 COL 2.4 NO-LABEL WIDGET-ID 46
     run-cost-10 AT ROW 15.62 COL 16.4 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     setups-10 AT ROW 15.62 COL 33.4 COLON-ALIGNED NO-LABEL WIDGET-ID 66
     btnShowVendCostMtx AT ROW 6.05 COL 15 WIDGET-ID 68
     "Cost Per" VIEW-AS TEXT
          SIZE 14 BY .71 AT ROW 6 COL 20
     "Min" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 16.71 COL 57
     "Max" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 16.71 COL 69
     "Setup $" VIEW-AS TEXT
          SIZE 14 BY .71 AT ROW 6 COL 37
     "Purchased" VIEW-AS TEXT
          SIZE 13 BY .95 AT ROW 2.67 COL 2
     "Min" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 16.71 COL 22
     "Max" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 16.71 COL 34
     RECT-24 AT ROW 1 COL 1
     RECT-26 AT ROW 6.95 COL 2 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.e-itemfg-vend,asi.e-itemfg
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
         HEIGHT             = 17.62
         WIDTH              = 99.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnShowVendCostMtx IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnShowVendCostMtx:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fi_oh-markup IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN e-itemfg-vend.i-no IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ls-item-dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-item-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-vend-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN qty-label IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN e-itemfg-vend.roll-w[27] IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN e-itemfg-vend.roll-w[28] IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN e-itemfg-vend.roll-w[29] IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN e-itemfg-vend.roll-w[30] IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN run-cost-01 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN run-cost-02 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN run-cost-03 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN run-cost-04 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN run-cost-05 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN run-cost-06 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN run-cost-07 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN run-cost-08 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN run-cost-09 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN run-cost-10 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN run-qty-01 IN FRAME F-Main
   NO-ENABLE ALIGN-L 3                                                  */
/* SETTINGS FOR FILL-IN run-qty-02 IN FRAME F-Main
   NO-ENABLE ALIGN-L 3                                                  */
/* SETTINGS FOR FILL-IN run-qty-03 IN FRAME F-Main
   NO-ENABLE ALIGN-L 3                                                  */
/* SETTINGS FOR FILL-IN run-qty-04 IN FRAME F-Main
   NO-ENABLE ALIGN-L 3                                                  */
/* SETTINGS FOR FILL-IN run-qty-05 IN FRAME F-Main
   NO-ENABLE ALIGN-L 3                                                  */
/* SETTINGS FOR FILL-IN run-qty-06 IN FRAME F-Main
   NO-ENABLE ALIGN-L 3                                                  */
/* SETTINGS FOR FILL-IN run-qty-07 IN FRAME F-Main
   NO-ENABLE ALIGN-L 3                                                  */
/* SETTINGS FOR FILL-IN run-qty-08 IN FRAME F-Main
   NO-ENABLE ALIGN-L 3                                                  */
/* SETTINGS FOR FILL-IN run-qty-09 IN FRAME F-Main
   NO-ENABLE ALIGN-L 3                                                  */
/* SETTINGS FOR FILL-IN run-qty-10 IN FRAME F-Main
   NO-ENABLE ALIGN-L 3                                                  */
/* SETTINGS FOR FILL-IN setups-01 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN setups-02 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN setups-03 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN setups-04 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN setups-05 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN setups-06 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN setups-07 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN setups-08 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN setups-09 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN setups-10 IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN e-itemfg-vend.spare-dec-1 IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN e-itemfg.std-uom IN FRAME F-Main
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN e-itemfg-vend.vend-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
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
    DEF VAR char-val AS CHAR NO-UNDO.
    DEF VAR uom-list AS CHAR INIT "C,CS,EA,L,M," NO-UNDO.

    CASE FOCUS:NAME:
        WHEN "std-uom" THEN DO:
            RUN sys/ref/uom-fg.p    (NO, 
                                    OUTPUT uom-list).
            RUN windows/l-stduom.w  (gcompany,
                                    uom-list, 
                                    focus:screen-value, 
                                    output char-val).
            IF char-val NE "" THEN ASSIGN
                e-itemfg.std-uom:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,char-val).
        END.
        WHEN "vend-no" THEN DO:
            RUN windows/l-vendno.w  (gcompany, 
                                    "", 
                                    focus:screen-value, 
                                    output char-val).
            IF char-val NE "" THEN ASSIGN
                e-itemfg-vend.vend-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,char-val).
        END.
        WHEN "cust-no" THEN DO:
            RUN windows/l-cust.w    (gcompany, 
                                    e-itemfg-vend.cust-no:SCREEN-VALUE, 
                                    OUTPUT char-val).
            IF char-val NE "" THEN ASSIGN
                e-itemfg-vend.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,char-val).
        END.
    END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnShowVendCostMtx
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnShowVendCostMtx V-table-Win
ON CHOOSE OF btnShowVendCostMtx IN FRAME F-Main /* * */
DO:
    RUN pVendCostMtx ("SHOW").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME e-itemfg-vend.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL e-itemfg-vend.cust-no V-table-Win
ON LEAVE OF e-itemfg-vend.cust-no IN FRAME F-Main /* Cust. # */
DO:
    IF LASTKEY NE -1 THEN DO:
        RUN valid-cust-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-cost-01
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-cost-01 V-table-Win
ON VALUE-CHANGED OF run-cost-01 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-cost-02
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-cost-02 V-table-Win
ON VALUE-CHANGED OF run-cost-02 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-cost-03
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-cost-03 V-table-Win
ON VALUE-CHANGED OF run-cost-03 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-cost-04
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-cost-04 V-table-Win
ON VALUE-CHANGED OF run-cost-04 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-cost-05
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-cost-05 V-table-Win
ON VALUE-CHANGED OF run-cost-05 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-cost-06
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-cost-06 V-table-Win
ON VALUE-CHANGED OF run-cost-06 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-cost-07
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-cost-07 V-table-Win
ON VALUE-CHANGED OF run-cost-07 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-cost-08
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-cost-08 V-table-Win
ON VALUE-CHANGED OF run-cost-08 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-cost-09
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-cost-09 V-table-Win
ON VALUE-CHANGED OF run-cost-09 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-cost-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-cost-10 V-table-Win
ON VALUE-CHANGED OF run-cost-10 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-qty-01
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-qty-01 V-table-Win
ON VALUE-CHANGED OF run-qty-01 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-qty-02
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-qty-02 V-table-Win
ON VALUE-CHANGED OF run-qty-02 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-qty-03
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-qty-03 V-table-Win
ON VALUE-CHANGED OF run-qty-03 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-qty-04
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-qty-04 V-table-Win
ON VALUE-CHANGED OF run-qty-04 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-qty-05
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-qty-05 V-table-Win
ON VALUE-CHANGED OF run-qty-05 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-qty-06
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-qty-06 V-table-Win
ON VALUE-CHANGED OF run-qty-06 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-qty-07
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-qty-07 V-table-Win
ON VALUE-CHANGED OF run-qty-07 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-qty-08
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-qty-08 V-table-Win
ON VALUE-CHANGED OF run-qty-08 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-qty-09
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-qty-09 V-table-Win
ON VALUE-CHANGED OF run-qty-09 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-qty-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-qty-10 V-table-Win
ON VALUE-CHANGED OF run-qty-10 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setups-01
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setups-01 V-table-Win
ON VALUE-CHANGED OF setups-01 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setups-02
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setups-02 V-table-Win
ON VALUE-CHANGED OF setups-02 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setups-03
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setups-03 V-table-Win
ON VALUE-CHANGED OF setups-03 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setups-04
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setups-04 V-table-Win
ON VALUE-CHANGED OF setups-04 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setups-05
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setups-05 V-table-Win
ON VALUE-CHANGED OF setups-05 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setups-06
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setups-06 V-table-Win
ON VALUE-CHANGED OF setups-06 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setups-07
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setups-07 V-table-Win
ON VALUE-CHANGED OF setups-07 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setups-08
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setups-08 V-table-Win
ON VALUE-CHANGED OF setups-08 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setups-09
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setups-09 V-table-Win
ON VALUE-CHANGED OF setups-09 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setups-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setups-10 V-table-Win
ON VALUE-CHANGED OF setups-10 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME e-itemfg.std-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL e-itemfg.std-uom V-table-Win
ON LEAVE OF e-itemfg.std-uom IN FRAME F-Main /* Cost UOM */
DO:
    IF LASTKEY NE -1 THEN DO:
        RUN valid-std-uom NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sel V-table-Win
ON VALUE-CHANGED OF tb_sel IN FRAME F-Main /* Check to pre-select this quantity/ */
OR VALUE-CHANGED OF tb_sel-01
OR VALUE-CHANGED OF tb_sel-02
OR VALUE-CHANGED OF tb_sel-03
OR VALUE-CHANGED OF tb_sel-04
OR VALUE-CHANGED OF tb_sel-05
OR VALUE-CHANGED OF tb_sel-06
OR VALUE-CHANGED OF tb_sel-07
OR VALUE-CHANGED OF tb_sel-08
OR VALUE-CHANGED OF tb_sel-09
OR VALUE-CHANGED OF tb_sel-10
DO:
  RUN new-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME e-itemfg-vend.vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL e-itemfg-vend.vend-no V-table-Win
ON LEAVE OF e-itemfg-vend.vend-no IN FRAME F-Main /* Vendor */
DO:
    IF LASTKEY NE -1 THEN DO:
        RUN valid-vend-no (e-itemfg-vend.vend-no:HANDLE) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.

    ASSIGN
        ls-vend-name = "".
    FIND FIRST vend NO-LOCK WHERE 
        vend.company = e-itemfg.company AND 
        vend.vend-no = e-itemfg-vend.vend-no:SCREEN-VALUE 
        NO-ERROR.
    IF AVAIL vend THEN ASSIGN
        ls-vend-name = vend.NAME.
    DISP 
        ls-vend-name 
        WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
    {sys/inc/f3help.i}  /* asi field contents help */
    RUN get-company (output gcompany).
    ASSIGN
        SESSION:DATA-ENTRY-RETURN = TRUE. 

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-vend-cost V-table-Win 
PROCEDURE add-vend-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*called from windows/d-vndcfg.w*/
    DEF INPUT PARAMETER ip-recid-eitem AS RECID NO-UNDO.
    DEF INPUT PARAMETER ip-recid-eitem-vend AS RECID NO-UNDO.
    DEF INPUT PARAMETER ip-recid-bf AS RECID NO-UNDO.
    DEF INPUT PARAMETER v-term LIKE report.term-id NO-UNDO.

    FIND bf-itemfg NO-LOCK WHERE 
        RECID(bf-itemfg) = ip-recid-bf
        NO-ERROR.
    FIND e-itemfg NO-LOCK WHERE 
        RECID(e-itemfg) = ip-recid-eitem 
        NO-ERROR.
    FIND e-itemfg-vend NO-LOCK WHERE 
        RECID(e-itemfg-vend) = ip-recid-eitem-vend 
        NO-ERROR.

  RUN dispatch ("display-fields").

    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "tableio-source", OUTPUT char-hdl).
    RUN set-buttons IN WIDGET-HANDLE(char-hdl) ("action-chosen").

    ASSIGN 
        gTerm = v-term
        gNewVendor = YES.

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
  {src/adm/template/row-list.i "e-itemfg-vend"}
  {src/adm/template/row-list.i "e-itemfg"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "e-itemfg-vend"}
  {src/adm/template/row-find.i "e-itemfg"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-est-matrices-proc V-table-Win 
PROCEDURE delete-est-matrices-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR ll-choice AS LOG NO-UNDO.

    IF CAN-FIND(FIRST eb WHERE
                eb.company EQ e-itemfg-vend.company AND
                eb.stock-no EQ e-itemfg-vend.i-no AND
                eb.pur-man EQ YES) THEN DO:
        MESSAGE 
            "Delete matching Estimate Farm Tab entry?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ll-choice.

        IF ll-choice THEN DO:
            SESSION:SET-WAIT-STATE("GENERAL").
            FOR EACH eb NO-LOCK WHERE
                eb.company EQ e-itemfg-vend.company AND
                eb.stock-no EQ e-itemfg-vend.i-no AND
                eb.pur-man EQ YES,
                FIRST b-e-itemfg-vend WHERE
                    b-e-itemfg-vend.company EQ e-itemfg-vend.company AND
                    b-e-itemfg-vend.est-no EQ eb.est-no AND
                    b-e-itemfg-vend.form-no EQ eb.form-no AND
                    b-e-itemfg-vend.eqty EQ eb.eqty AND
                    b-e-itemfg-vend.blank-no EQ eb.blank-no AND
                    b-e-itemfg-vend.vend-no EQ e-itemfg-vend.vend-no AND
                    b-e-itemfg-vend.cust-no EQ e-itemfg-vend.cust-no:

                    DELETE b-e-itemfg-vend.

                    FIND FIRST b-reftable-1 WHERE
                        b-reftable-1.reftable EQ "e-itemfg-vend.std-uom" AND
                        b-reftable-1.company  EQ b-e-itemfg-vend.company AND
                        b-reftable-1.loc      EQ "" AND
                        b-reftable-1.code     EQ b-e-itemfg-vend.est-no AND
                        b-reftable-1.val[1]   EQ b-e-itemfg-vend.form-no AND
                        b-reftable-1.val[2]   EQ b-e-itemfg-vend.blank-no
                        NO-ERROR.
                    IF AVAIL b-reftable-1 THEN
                        DELETE b-reftable-1.
            END.
            SESSION:SET-WAIT-STATE("").
        END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

    RUN dispatch ("display-fields").
    RUN pVendCostMtx ("INIT").
    lAddRecord = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR i AS INT NO-UNDO.
    DEF VAR lv-recid AS RECID NO-UNDO.
    DEF VAR lv-eb-recid AS RECID NO-UNDO.
    DEF VAR char-hdl AS CHAR NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN fi_oh-markup.
    END.

    IF AVAIL e-itemfg THEN ASSIGN 
        lv-recid = RECID(e-itemfg).
    ELSE DO:
        CREATE e-itemfg.
        ASSIGN 
            e-itemfg.company = g_company
            e-itemfg.i-no = ""
            lv-recid = RECID(e-itemfg).
    END.

    FIND FIRST e-itemfg WHERE 
        RECID(e-itemfg) = lv-recid 
        NO-ERROR.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            tb_sel-01
            tb_sel-02
            tb_sel-03
            tb_sel-04
            tb_sel-05
            tb_sel-06
            tb_sel-07
            tb_sel-08
            tb_sel-09
            tb_sel-10
            .
    END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

    RUN pVendCostMtx ("ASSIGN").
  
    FOR EACH tmpfile: 
        DELETE tmpfile.  
    END.

    DO i = 1 TO 30:
        IF e-itemfg.roll-w[i] NE 0 THEN DO:
            CREATE tmpfile.
            ASSIGN
                tmpfile.siz = e-itemfg.roll-w[i]
                e-itemfg.roll-w[i] = 0.
        END.
    END.
    
    ASSIGN
        i = 1.
    FOR EACH tmpfile BY tmpfile.siz :
        ASSIGN
            e-itemfg.roll-w[i] = tmpfile.siz
            i = i + 1.
    END.
  
    FOR EACH tmpfile: 
        DELETE tmpfile.  
    END.
  
    DO i = 1 TO 10:
        CREATE tmpfile.
        ASSIGN 
            tmpfile.qty = e-itemfg-vend.run-qty[i]
            tmpfile.siz = e-itemfg-vend.run-cost[i]
            tmpfile.setups = e-itemfg-vend.setups[i]
            e-itemfg-vend.run-qty[i] = 0
            e-itemfg-vend.run-cost[i] = 0
            e-itemfg-vend.setups[i] = 0.
    END.
  
    ASSIGN
        i = 1.
    FOR EACH tmpfile BY tmpfile.qty:
        IF tmpfile.qty EQ 0 THEN NEXT.
        ASSIGN
            e-itemfg-vend.run-qty[i] = tmpfile.qty
            e-itemfg-vend.run-cost[i] = tmpfile.siz
            e-itemfg-vend.setups[i] = tmpfile.setups
            i = i + 1.       
    END.

    DO i = 1 TO 10:
        ASSIGN
            e-itemfg.run-qty[i] = e-itemfg-vend.run-qty[i]
            e-itemfg.run-cost[i] = e-itemfg-vend.run-cost[i].
    END.

    IF e-itemfg-vend.i-no = "" THEN DO:
        run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
        run get-link-handle in adm-broker-hdl(widget-handle(char-hdl),"record-source", output char-hdl).
        run get-eb-record in widget-handle(char-hdl) (output lv-eb-recid).
        FIND FIRST eb NO-LOCK WHERE 
            RECID(eb) EQ lv-eb-recid 
            NO-ERROR.
        IF AVAIL eb THEN ASSIGN 
            e-itemfg-vend.est-no = eb.est-no
            e-itemfg-vend.eqty = eb.eqty
            e-itemfg-vend.form-no = eb.form-no
            e-itemfg-vend.blank-no = eb.blank-no.
    END.

    ASSIGN
        e-itemfg-vend.selected[01] = tb_sel-01
        e-itemfg-vend.selected[02] = tb_sel-02
        e-itemfg-vend.selected[03] = tb_sel-03
        e-itemfg-vend.selected[04] = tb_sel-04
        e-itemfg-vend.selected[05] = tb_sel-05
        e-itemfg-vend.selected[06] = tb_sel-06
        e-itemfg-vend.selected[07] = tb_sel-07
        e-itemfg-vend.selected[08] = tb_sel-08
        e-itemfg-vend.selected[09] = tb_sel-09
        e-itemfg-vend.selected[10] = tb_sel-10
        .

    FIND FIRST reftable EXCLUSIVE WHERE
        reftable.reftable EQ 'e-itemfg-vend.markup' AND
        reftable.company EQ e-itemfg-vend.company AND
        reftable.loc EQ e-itemfg-vend.i-no AND
        reftable.code EQ e-itemfg-vend.vend-no
        NO-ERROR.
    IF NOT AVAILABLE reftable THEN DO:
        CREATE reftable.
        ASSIGN
            reftable.reftable = 'e-itemfg-vend.markup'
            reftable.company = e-itemfg-vend.company
            reftable.loc = e-itemfg-vend.i-no
            reftable.code = e-itemfg-vend.vend-no.
    END.
    ASSIGN 
        reftable.val[1] = INT(fi_oh-markup:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  
    FIND CURRENT reftable NO-LOCK.
    FIND CURRENT e-itemfg NO-LOCK.

    IF gNewVendor THEN DO:
        /* costs zeroed out to indicate they need to be recalculated */
        CREATE report.
        ASSIGN
            report.term-id = gTerm
            report.key-01  = STRING(e-itemfg-vend.run-cost[1])
            report.key-02  = ""
            report.key-03  = e-itemfg-vend.vend-no
            report.key-04  = string(e-itemfg-vend.run-qty[1]) 
            report.key-05  = ""
            report.key-06  = string(e-itemfg-vend.setups[1])
            report.key-08  = "RECALC"
            report.rec-id  = recid(e-itemfg-vend).
        RELEASE report.
    END.

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

    DISABLE
        e-itemfg.std-uom 
        fi_oh-markup 
        {&farmFields} 
        WITH FRAME {&FRAME-NAME}.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record V-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lCopyRecord = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUFFER bf-eitemfg FOR e-itemfg.

    DEF VAR lv-recid AS RECID NO-UNDO.
    DEF VAR char-hdl AS CHAR NO-UNDO.
    DEF VAR i AS INT NO-UNDO.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

    RUN get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
    RUN get-item-record in widget-handle(char-hdl) (output lv-recid).
 
    FIND 
        bf-itemfg WHERE RECID(bf-itemfg) EQ lv-recid 
        NO-ERROR.

    ASSIGN 
        e-itemfg-vend.company = IF AVAIL e-itemfg THEN e-itemfg.company ELSE g_company
        e-itemfg-vend.i-no = IF AVAIL e-itemfg THEN e-itemfg.i-no ELSE ""
        e-itemfg-vend.item-type = NO.

    DO i = 1 TO 10:
        ASSIGN 
            e-itemfg-vend.run-qty[i] = IF AVAIL e-itemfg THEN e-itemfg.run-qty[i] ELSE 0
            e-itemfg-vend.run-cost[i] = IF AVAIL e-itemfg THEN e-itemfg.run-cost[i] ELSE 0.
    END.

    DO WITH FRAME {&FRAME-NAME}:
        IF adm-adding-record THEN ASSIGN
            e-itemfg-vend.vend-no:screen-value = ""
            fi_oh-markup:SCREEN-VALUE = "0".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR char-hdl AS CHAR NO-UNDO.

    IF NOT adm-new-record THEN DO:
        {custom/askdel.i}
    END.

    RUN delete-est-matrices-proc.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

    IF NOT AVAIL e-itemfg THEN DO:
        RUN get-link-handle in adm-broker-hdl(this-procedure, "record-source", output char-hdl).
        RUN dispatch in widget-handle(char-hdl) ("open-query").
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUFFER b-eiv FOR e-itemfg-vend.
    
    DEF VAR lv-markup AS CHAR NO-UNDO.

    IF AVAIL e-itemfg-vend 
    AND e-itemfg-vend.setup NE 0 THEN DO TRANSACTION:
        FIND b-eiv EXCLUSIVE WHERE 
            ROWID(b-eiv) EQ ROWID(e-itemfg-vend)
            NO-ERROR.
        IF AVAIL b-eiv THEN ASSIGN
            b-eiv.setups[1] = e-itemfg-vend.setup
            b-eiv.setup     = 0.
    END.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            lv-group-hdl = FRAME {&FRAME-NAME}:FIRST-CHILD
            lv-field-hdl = lv-group-hdl:FIRST-CHILD.

        IF NOT lVendCostMtx THEN ASSIGN
            e-itemfg-vend.spare-dec-1:HIDDEN = TRUE.
        ELSE ASSIGN
            e-itemfg-vend.spare-dec-1:HIDDEN = FALSE.

        DO WHILE VALID-HANDLE(lv-field-hdl):
            IF lv-field-hdl:NAME BEGINS "tb_sel" THEN ASSIGN
                lv-field-hdl:SENSITIVE = NO
                lv-field-hdl:HIDDEN = NOT AVAIL e-itemfg-vend OR
                                       e-itemfg-vend.est-no EQ "".
            ASSIGN
                lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
        END.

        IF adm-new-record 
        AND adm-adding-record THEN ASSIGN 
            lv-markup = fi_oh-markup:SCREEN-VALUE.

        IF AVAIL e-itemfg-vend 
        AND NOT adm-new-record THEN DO:
            FIND FIRST reftable NO-LOCK WHERE
                reftable.reftable EQ 'e-itemfg-vend.markup' AND
                reftable.company EQ e-itemfg-vend.company AND
                reftable.loc EQ e-itemfg-vend.i-no AND
                reftable.code EQ e-itemfg-vend.vend-no
                NO-ERROR.
            IF NOT AVAILABLE reftable THEN DO:
                CREATE reftable.
                ASSIGN
                    reftable.reftable = 'e-itemfg-vend.markup'
                    reftable.company = e-itemfg-vend.company
                    reftable.loc = e-itemfg-vend.i-no
                    reftable.code = e-itemfg-vend.vend-no.
            END.
            ASSIGN
                fi_oh-markup = reftable.val[1].
            RELEASE reftable.
        END.
    END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    IF adm-new-record AND adm-adding-record THEN ASSIGN
        fi_oh-markup:SCREEN-VALUE = lv-markup.

    ASSIGN 
        ls-vend-name = ""  
        ls-item-name = ""
        ls-item-dscr = ""
        qty-label    = "Qty " + STRING(lVendCostMtx,"From/To").
  
    IF lAddRecord EQ NO AND lCopyRecord EQ NO THEN
    RUN pVendCostMtx ("DISPLAY").
    ELSE
    ASSIGN
      lAddRecord  = NO
      lCopyRecord = NO
      .
  
    FIND FIRST vend NO-LOCK WHERE 
        vend.company EQ gcompany AND 
        vend.vend-no EQ e-itemfg-vend.vend-no:SCREEN-VALUE 
        NO-ERROR.
    IF AVAIL vend THEN ASSIGN
        ls-vend-name = vend.NAME.
    DISP 
        ls-vend-name 
        WITH FRAME {&FRAME-NAME}.

    IF AVAIL e-itemfg THEN FIND itemfg NO-LOCK WHERE 
        itemfg.company = gcompany AND 
        itemfg.i-no = e-itemfg.i-no 
        NO-ERROR.
    IF AVAIL itemfg THEN ASSIGN 
        ls-item-name = itemfg.i-name
        ls-item-dscr = itemfg.part-dscr1.
    DISP 
        ls-item-name 
        ls-item-dscr 
        qty-label 
        WITH FRAME {&FRAME-NAME}.

    RUN new-sel.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR char-hdl as cha NO-UNDO.
    DEF VAR lv-recid as recid NO-UNDO.
  
    RUN get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
    RUN get-item-record in widget-handle(char-hdl) (output lv-recid).
  
    FIND bf-itemfg NO-LOCK WHERE 
        RECID(bf-itemfg) EQ lv-recid 
        NO-ERROR.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            lv-group-hdl = FRAME {&FRAME-NAME}:FIRST-CHILD
            lv-field-hdl = lv-group-hdl:FIRST-CHILD.
        IF AVAIL e-itemfg-vend THEN DO WHILE VALID-HANDLE(lv-field-hdl):
            IF lv-field-hdl:NAME BEGINS "tb_sel" THEN ASSIGN
                lv-field-hdl:SENSITIVE = e-itemfg-vend.est-no NE "".
            ASSIGN
                lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
        END.

        ENABLE 
            e-itemfg.std-uom 
            fi_oh-markup 
            {&farmFields}.

        APPLY "entry" TO e-itemfg.std-uom.
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
    DEF BUFFER bf-evend FOR e-itemfg-vend.

    DEF VAR v-add-record AS LOG NO-UNDO.
    DEF VAR char-hdl AS cha NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        RUN valid-vend-no (e-itemfg-vend.vend-no:HANDLE) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-std-uom NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-cust-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    
        IF e-itemfg-vend.vend-no:SCREEN-VALUE EQ "" THEN DO:
            FIND FIRST bf-evend NO-LOCK WHERE 
                bf-evend.company   EQ e-itemfg-vend.company AND 
                bf-evend.i-no      EQ e-itemfg-vend.i-no AND 
                bf-evend.item-type EQ NO AND 
                bf-evend.vend-no   EQ "" AND 
                bf-evend.est-no    EQ e-itemfg-vend.est-no AND 
                bf-evend.eqty      EQ e-itemfg-vend.eqty AND 
                bf-evend.form-no   EQ e-itemfg-vend.form-no AND 
                bf-evend.blank-no  EQ e-itemfg-vend.blank-no AND 
                ROWID(bf-evend)    NE ROWID(e-itemfg-vend)
                NO-ERROR.
            IF AVAIL bf-evend THEN DO:
                MESSAGE 
                    "Blank vendor exists..." 
                    VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" to e-itemfg-vend.vend-no.
                RETURN. 
            END.
        END.
    END.

    ASSIGN 
        v-add-record = adm-adding-record.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

    IF v-add-record THEN DO:
        RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
        RUN repos-query IN WIDGET-HANDLE(char-hdl) (ROWID(e-itemfg)).
    END.
    ELSE RUN dispatch ("display-fields").

    DISABLE 
        e-itemfg.std-uom 
        fi_oh-markup 
        {&farmFields} 
        WITH FRAME {&FRAME-NAME}.

    RUN update-est-matrices-proc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-sel V-table-Win 
PROCEDURE new-sel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR lv-sel AS CHAR NO-UNDO.
    DEF VAR lv-sel-all AS CHAR NO-UNDO.
    DEF VAR lv-test-valid-widget AS CHAR NO-UNDO.
    DEF VAR lv-error-on-widget AS LOG NO-UNDO.
  
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            lv-test-valid-widget = FOCUS:NAME NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN.

        IF FOCUS:NAME EQ "tb_sel" THEN ASSIGN 
            lv-sel-all = FOCUS:SCREEN-VALUE.
        ELSE IF FOCUS:NAME BEGINS "tb_sel" THEN ASSIGN 
            lv-sel = FOCUS:SCREEN-VALUE.
        ELSE ASSIGN 
            lv-sel = tb_sel-01:SCREEN-VALUE.

        ASSIGN
            lv-group-hdl = FRAME {&FRAME-NAME}:FIRST-CHILD
            lv-field-hdl = lv-group-hdl:FIRST-CHILD.
    
        {&methods/lValidateError.i YES}
        DO WHILE VALID-HANDLE(lv-field-hdl):
            ASSIGN 
                lv-test-valid-widget = lv-field-hdl:NAME NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                RETURN.
            IF lv-field-hdl:NAME BEGINS "tb_sel" THEN
                IF lv-sel-all NE "" THEN ASSIGN
                    lv-field-hdl:SCREEN-VALUE = lv-sel-all.
                ELSE IF lv-field-hdl:NAME NE "tb_sel"
                AND lv-field-hdl:SCREEN-VALUE NE lv-sel THEN ASSIGN 
                    lv-sel = "".

            ASSIGN 
                lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
        END.
        {&methods/lValidateError.i NO}
    
        IF lv-sel NE "" THEN ASSIGN 
            lv-sel-all = lv-sel.
        IF lv-sel-all NE "" THEN ASSIGN 
            tb_sel:SCREEN-VALUE = lv-sel-all.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE price-change V-table-Win 
PROCEDURE price-change :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.

    DEF VAR v-pct AS DEC NO-UNDO.
    DEF VAR i AS INT NO-UNDO.
    DEF VAR char-hdl AS CHAR NO-UNDO.
    DEFINE VARIABLE ip-parms     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE op-values    AS CHARACTER NO-UNDO.

    ASSIGN 
        v-pct = 0.
    
    ip-parms = 
    /* price percentage */
    "|type=literal,name=label6,row=3.2,col=23,enable=false,width=38,scrval=" + "By what percentage:" + ",FORMAT=x(40)"
    + "|type=fill-in,name=perprice,row=3,col=45,enable=true,width=16,data-type=decimal,initial=" + STRING(v-pct)
    + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true,width=12,height=3 " 
    /* Box Title */
    + "|type=win,name=fi3,enable=true,label=  Update Price?,FORMAT=X(30),height=9".


    RUN custom/d-prompt.w (INPUT "", ip-parms, "", OUTPUT op-values).

    IF op-values NE "" THEN
        v-pct = DECIMAL(ENTRY(2, op-values)) .
    ELSE 
        RETURN .


    STATUS DEFAULT "Processing Raw Material: " + STRING(e-itemfg.i-no).

    FIND bf-e-itemfg-vend EXCLUSIVE WHERE 
        RECID(bf-e-itemfg-vend) = RECID(e-itemfg-vend).
    DO i = 1 TO 10:
        ASSIGN 
            bf-e-itemfg-vend.run-cost[i] = e-itemfg-vend.run-cost[i] + 
                                (e-itemfg-vend.run-cost[i] * v-pct / 100).
    END.

    RUN get-link-handle in adm-broker-hdl (this-procedure, "record-source", output char-hdl).
    RUN reopen-and-repo in widget-handle(char-hdl) (rowid(bf-e-itemfg-vend)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pVendCostMtx V-table-Win 
PROCEDURE pVendCostMtx :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {fg/pVendCostMtx.i}

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
  {src/adm/template/snd-list.i "e-itemfg-vend"}
  {src/adm/template/snd-list.i "e-itemfg"}

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
    DEF INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
    DEF INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.
      
    CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-est-matrices-proc V-table-Win 
PROCEDURE update-est-matrices-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR ll-choice AS LOG NO-UNDO.

    IF CAN-FIND(FIRST eb WHERE
                eb.company EQ e-itemfg-vend.company AND
                eb.stock-no EQ e-itemfg-vend.i-no AND
                eb.pur-man EQ YES) THEN DO:
        MESSAGE 
            "Update matching Estimate Farm Tab entry with these values?" SKIP(2) 
             "Item #: " e-itemfg-vend.i-no SKIP 
             "Vendor: " e-itemfg-vend.vend-no SKIP 
             "Customer: " e-itemfg-vend.cust-no
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ll-choice.

        IF ll-choice THEN DO:
            SESSION:SET-WAIT-STATE("GENERAL").
            FOR EACH eb NO-LOCK WHERE
                eb.company EQ e-itemfg-vend.company AND
                eb.stock-no EQ e-itemfg-vend.i-no AND
                eb.pur-man EQ YES:

                FIND FIRST b-e-itemfg-vend WHERE
                     b-e-itemfg-vend.company EQ e-itemfg-vend.company AND
                     b-e-itemfg-vend.est-no EQ eb.est-no AND
                     b-e-itemfg-vend.form-no EQ eb.form-no AND
                     b-e-itemfg-vend.eqty EQ eb.eqty AND
                     b-e-itemfg-vend.blank-no EQ eb.blank-no AND
                     b-e-itemfg-vend.vend-no EQ e-itemfg-vend.vend-no AND
                     b-e-itemfg-vend.cust-no EQ e-itemfg-vend.cust-no
                     NO-ERROR.

                IF AVAIL b-e-itemfg-vend THEN
                   BUFFER-COPY e-itemfg-vend EXCEPT rec_key est-no eqty form-no blank-no
                             TO b-e-itemfg-vend.
                ELSE DO:
                   CREATE b-e-itemfg-vend.
                   BUFFER-COPY e-itemfg-vend EXCEPT rec_key est-no eqty form-no blank-no
                             TO b-e-itemfg-vend
                    ASSIGN 
                        b-e-itemfg-vend.est-no = eb.est-no
                        b-e-itemfg-vend.eqty = eb.eqty
                        b-e-itemfg-vend.form-no = eb.form-no
                        b-e-itemfg-vend.blank-no = eb.blank-no.
                END.

                FIND FIRST b-reftable-1 WHERE
                     b-reftable-1.reftable EQ "e-itemfg-vend.std-uom" AND
                     b-reftable-1.company  EQ b-e-itemfg-vend.company AND
                     b-reftable-1.loc      EQ "" AND
                     b-reftable-1.code     EQ b-e-itemfg-vend.est-no AND
                     b-reftable-1.val[1]   EQ b-e-itemfg-vend.form-no AND
                     b-reftable-1.val[2]   EQ b-e-itemfg-vend.blank-no
                     NO-ERROR.

                IF AVAIL b-reftable-1 THEN DO:
                    FIND FIRST e-itemfg NO-LOCK WHERE
                        e-itemfg.company EQ b-e-itemfg-vend.company AND
                        e-itemfg.i-no EQ b-e-itemfg-vend.i-no
                        NO-ERROR.

                    IF AVAIL e-itemfg THEN DO:
                        ASSIGN 
                            b-reftable-1.code2 = e-itemfg.std-uom.
                        RELEASE e-itemfg.
                    END.

                    RELEASE b-reftable-1.
                END.
                ELSE DO:
                    CREATE b-reftable-1.
                    ASSIGN
                        b-reftable-1.reftable = "e-itemfg-vend.std-uom"
                        b-reftable-1.company  = b-e-itemfg-vend.company
                        b-reftable-1.loc      = ""
                        b-reftable-1.code     = b-e-itemfg-vend.est-no
                        b-reftable-1.val[1]   = b-e-itemfg-vend.form-no
                        b-reftable-1.val[2]   = b-e-itemfg-vend.blank-no.

                    FIND FIRST e-itemfg NO-LOCK WHERE
                        e-itemfg.company EQ b-e-itemfg-vend.company AND
                        e-itemfg.i-no EQ b-e-itemfg-vend.i-no
                        NO-ERROR.

                    IF AVAIL e-itemfg THEN DO:
                        ASSIGN 
                            b-reftable-1.code2 = e-itemfg.std-uom.
                        RELEASE e-itemfg.
                    END.
                    RELEASE b-reftable-1.
                END.
                RELEASE b-e-itemfg-vend.
            END.
            SESSION:SET-WAIT-STATE("").
        END.
    END.
    
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
        IF e-itemfg-vend.cust-no:SCREEN-VALUE NE "" 
        AND NOT CAN-FIND(FIRST cust WHERE
                        cust.company EQ gcompany AND
                        cust.cust-no EQ e-itemfg-vend.cust-no:SCREEN-VALUE) THEN DO:
            MESSAGE 
                "Invalid Cust. #, try help..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO e-itemfg-vend.cust-no.
            RETURN ERROR.
        END.
    END.
    {methods/lValidateError.i NO}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-std-uom V-table-Win 
PROCEDURE valid-std-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR uom-list AS CHAR INIT "" NO-UNDO.

    {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            e-itemfg.std-uom:SCREEN-VALUE = CAPS(e-itemfg.std-uom:SCREEN-VALUE).

        RUN sys/ref/uom-fg.p (NO, 
                              OUTPUT uom-list).

        IF LOOKUP(e-itemfg.std-uom:SCREEN-VALUE,uom-list) LE 0 
        OR (e-itemfg.std-uom:SCREEN-VALUE EQ "MSF" AND
            NOT CAN-FIND(FIRST bf-itemfg WHERE 
                        bf-itemfg.company EQ e-itemfg.company AND 
                        bf-itemfg.i-no    EQ e-itemfg.i-no AND 
                            ((bf-itemfg.t-len NE 0 AND bf-itemfg.t-wid NE 0) OR
                            bf-itemfg.t-sqin NE 0                           OR
                            bf-itemfg.t-sqft NE 0))) THEN DO:

            IF e-itemfg.std-uom:SCREEN-VALUE EQ "MSF" THEN MESSAGE 
                "When " + TRIM(e-itemfg.std-uom:LABEL) + " is MSF, " +
                "FG Item must have valid Blank Length & Width, SqIn, or SqFt..."
                VIEW-AS ALERT-BOX ERROR.
            ELSE MESSAGE 
                TRIM(e-itemfg.std-uom:LABEL) +
                " must be " + TRIM(uom-list)
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO e-itemfg.std-uom.
            RETURN ERROR.
        END.
    END.
    {methods/lValidateError.i NO}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-vend-no V-table-Win 
PROCEDURE valid-vend-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.

    DEF VAR lv-msg AS CHAR NO-UNDO.

    {methods/lValidateError.i YES}
    lv-msg = "".
    DO WITH FRAME {&FRAME-NAME}:
        IF ip-focus:SCREEN-VALUE NE "" THEN
            IF e-itemfg-vend.vend-no EQ "" AND NOT adm-new-record THEN ASSIGN
                lv-msg = "Sorry, you cannot change the 'Blank Vendor', please use copy button".
            ELSE IF NOT CAN-FIND(FIRST vend WHERE 
                            vend.company EQ e-itemfg.company AND 
                            vend.vend-no EQ ip-focus:SCREEN-VALUE) THEN ASSIGN
                lv-msg = TRIM(ip-focus:LABEL) + " " + ip-focus:SCREEN-VALUE +  " is invalid, try help".

        IF lv-msg NE "" THEN DO:
            MESSAGE 
                TRIM(lv-msg)
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "entry" TO ip-focus.
            RETURN ERROR.
        END.
    END.
    {methods/lValidateError.i NO}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

