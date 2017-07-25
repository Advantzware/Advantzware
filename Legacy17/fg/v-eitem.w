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
def buffer bf-itemfg for itemfg.
DEF BUFFER b-e-itemfg-vend FOR e-itemfg-vend.
DEF BUFFER b-reftable-1 FOR reftable.

def temp-table tmpfile field siz as dec
                       field qty as dec
                       field setups as dec.
def var lv-roll-w like e-itemfg.roll-w no-undo.
DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
DEF VAR char-hdl AS CHAR NO-UNDO.

{custom/gcompany.i}
{custom/persist.i}

DEF VAR gTerm AS cha NO-UNDO.
DEF VAR gNewVendor AS LOG NO-UNDO.

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
e-itemfg-vend.vend-no e-itemfg-vend.run-qty[1] e-itemfg-vend.run-cost[1] ~
e-itemfg-vend.setups[1] e-itemfg-vend.run-qty[2] e-itemfg-vend.run-cost[2] ~
e-itemfg-vend.setups[2] e-itemfg-vend.run-qty[3] e-itemfg-vend.run-cost[3] ~
e-itemfg-vend.setups[3] e-itemfg-vend.run-qty[4] e-itemfg-vend.run-cost[4] ~
e-itemfg-vend.setups[4] e-itemfg-vend.run-qty[5] e-itemfg-vend.run-cost[5] ~
e-itemfg-vend.setups[5] e-itemfg-vend.run-qty[6] e-itemfg-vend.run-cost[6] ~
e-itemfg-vend.setups[6] e-itemfg-vend.run-qty[7] e-itemfg-vend.run-cost[7] ~
e-itemfg-vend.setups[7] e-itemfg-vend.run-qty[8] e-itemfg-vend.run-cost[8] ~
e-itemfg-vend.setups[8] e-itemfg-vend.run-qty[9] e-itemfg-vend.run-cost[9] ~
e-itemfg-vend.setups[9] e-itemfg-vend.run-qty[10] ~
e-itemfg-vend.run-cost[10] e-itemfg-vend.setups[10] ~
e-itemfg-vend.roll-w[27] e-itemfg-vend.roll-w[28] e-itemfg-vend.roll-w[29] ~
e-itemfg-vend.roll-w[30] 
&Scoped-define ENABLED-TABLES e-itemfg-vend
&Scoped-define FIRST-ENABLED-TABLE e-itemfg-vend
&Scoped-Define ENABLED-OBJECTS tb_sel tb_sel-01 tb_sel-02 tb_sel-03 ~
tb_sel-04 tb_sel-05 tb_sel-06 tb_sel-07 tb_sel-08 tb_sel-09 tb_sel-10 ~
RECT-24 
&Scoped-Define DISPLAYED-FIELDS e-itemfg-vend.i-no e-itemfg.std-uom ~
e-itemfg-vend.vend-item e-itemfg-vend.cust-no e-itemfg-vend.vend-no ~
e-itemfg-vend.run-qty[1] e-itemfg-vend.run-cost[1] e-itemfg-vend.setups[1] ~
e-itemfg-vend.run-qty[2] e-itemfg-vend.run-cost[2] e-itemfg-vend.setups[2] ~
e-itemfg-vend.run-qty[3] e-itemfg-vend.run-cost[3] e-itemfg-vend.setups[3] ~
e-itemfg-vend.run-qty[4] e-itemfg-vend.run-cost[4] e-itemfg-vend.setups[4] ~
e-itemfg-vend.run-qty[5] e-itemfg-vend.run-cost[5] e-itemfg-vend.setups[5] ~
e-itemfg-vend.run-qty[6] e-itemfg-vend.run-cost[6] e-itemfg-vend.setups[6] ~
e-itemfg-vend.run-qty[7] e-itemfg-vend.run-cost[7] e-itemfg-vend.setups[7] ~
e-itemfg-vend.run-qty[8] e-itemfg-vend.run-cost[8] e-itemfg-vend.setups[8] ~
e-itemfg-vend.run-qty[9] e-itemfg-vend.run-cost[9] e-itemfg-vend.setups[9] ~
e-itemfg-vend.run-qty[10] e-itemfg-vend.run-cost[10] ~
e-itemfg-vend.setups[10] e-itemfg-vend.roll-w[27] e-itemfg-vend.roll-w[28] ~
e-itemfg-vend.roll-w[29] e-itemfg-vend.roll-w[30] 
&Scoped-define DISPLAYED-TABLES e-itemfg-vend e-itemfg
&Scoped-define FIRST-DISPLAYED-TABLE e-itemfg-vend
&Scoped-define SECOND-DISPLAYED-TABLE e-itemfg
&Scoped-Define DISPLAYED-OBJECTS ls-item-name ls-item-dscr ls-vend-name ~
tb_sel tb_sel-01 tb_sel-02 tb_sel-03 tb_sel-04 tb_sel-05 tb_sel-06 ~
tb_sel-07 tb_sel-08 tb_sel-09 tb_sel-10 fi_oh-markup 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,DISPLAY-FIELD,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS e-itemfg.std-uom 
&Scoped-define ADM-ASSIGN-FIELDS e-itemfg.std-uom fi_oh-markup 
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
DEFINE VARIABLE fi_oh-markup AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "GS&&A O/H Markup %" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE ls-item-dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1 NO-UNDO.

DEFINE VARIABLE ls-item-name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1 NO-UNDO.

DEFINE VARIABLE ls-vend-name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 17.62.

DEFINE VARIABLE tb_sel AS LOGICAL INITIAL no 
     LABEL "Check to pre-select this quantity/" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-01 AS LOGICAL INITIAL no 
     LABEL "cost by vendor" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-02 AS LOGICAL INITIAL no 
     LABEL "tb_sel 2" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-03 AS LOGICAL INITIAL no 
     LABEL "tb_sel 3" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-04 AS LOGICAL INITIAL no 
     LABEL "tb_sel 4" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-05 AS LOGICAL INITIAL no 
     LABEL "tb_sel 5" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-06 AS LOGICAL INITIAL no 
     LABEL "tb_sel 6" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-07 AS LOGICAL INITIAL no 
     LABEL "tb_sel 7" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-08 AS LOGICAL INITIAL no 
     LABEL "tb_sel 8" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-09 AS LOGICAL INITIAL no 
     LABEL "tb_sel 9" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sel-10 AS LOGICAL INITIAL no 
     LABEL "tb_sel 10" 
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
     e-itemfg-vend.vend-no AT ROW 4.57 COL 14 COLON-ALIGNED FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ls-vend-name AT ROW 4.57 COL 30 COLON-ALIGNED NO-LABEL
     tb_sel AT ROW 6 COL 53
     e-itemfg-vend.run-qty[1] AT ROW 6.95 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     e-itemfg-vend.run-cost[1] AT ROW 6.95 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-itemfg-vend.setups[1] AT ROW 6.95 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     tb_sel-01 AT ROW 6.95 COL 53
     e-itemfg-vend.run-qty[2] AT ROW 7.91 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     e-itemfg-vend.run-cost[2] AT ROW 7.91 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-itemfg-vend.setups[2] AT ROW 7.91 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     tb_sel-02 AT ROW 7.91 COL 53
     e-itemfg-vend.run-qty[3] AT ROW 8.86 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-itemfg-vend.run-cost[3] AT ROW 8.86 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-itemfg-vend.setups[3] AT ROW 8.86 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     tb_sel-03 AT ROW 8.86 COL 53
     e-itemfg-vend.run-qty[4] AT ROW 9.81 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY .95
     e-itemfg-vend.run-cost[4] AT ROW 9.81 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-itemfg-vend.setups[4] AT ROW 9.81 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     tb_sel-04 AT ROW 9.81 COL 53
     e-itemfg-vend.run-qty[5] AT ROW 10.76 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-itemfg-vend.run-cost[5] AT ROW 10.76 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-itemfg-vend.setups[5] AT ROW 10.76 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     tb_sel-05 AT ROW 10.76 COL 53
     e-itemfg-vend.run-qty[6] AT ROW 11.71 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-itemfg-vend.run-cost[6] AT ROW 11.71 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-itemfg-vend.setups[6] AT ROW 11.71 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     tb_sel-06 AT ROW 11.71 COL 53
     e-itemfg-vend.run-qty[7] AT ROW 12.67 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     e-itemfg-vend.run-cost[7] AT ROW 12.67 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-itemfg-vend.setups[7] AT ROW 12.67 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     tb_sel-07 AT ROW 12.67 COL 53
     e-itemfg-vend.run-qty[8] AT ROW 13.62 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-itemfg-vend.run-cost[8] AT ROW 13.62 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-itemfg-vend.setups[8] AT ROW 13.62 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     tb_sel-08 AT ROW 13.62 COL 53
     e-itemfg-vend.run-qty[9] AT ROW 14.57 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-itemfg-vend.run-cost[9] AT ROW 14.57 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-itemfg-vend.setups[9] AT ROW 14.57 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     tb_sel-09 AT ROW 14.57 COL 53
     e-itemfg-vend.run-qty[10] AT ROW 15.52 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-itemfg-vend.run-cost[10] AT ROW 15.52 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-itemfg-vend.setups[10] AT ROW 15.52 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     tb_sel-10 AT ROW 15.52 COL 53
     fi_oh-markup AT ROW 15.76 COL 88.2 COLON-ALIGNED
     e-itemfg-vend.roll-w[27] AT ROW 17.43 COL 17 COLON-ALIGNED HELP
          "Enter Sheet Width Minimum"
          LABEL "Sheet Width"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-itemfg-vend.roll-w[28] AT ROW 17.43 COL 29 COLON-ALIGNED HELP
          "Enter Sheet Width Maximum" NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-itemfg-vend.roll-w[29] AT ROW 17.43 COL 52 COLON-ALIGNED HELP
          "Enter Sheet Length Minimum"
          LABEL "Length"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-itemfg-vend.roll-w[30] AT ROW 17.43 COL 64 COLON-ALIGNED HELP
          "Enter Sheet Length Maximum" NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     "Cost Per" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 6 COL 20
     "Min" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 16.71 COL 57
     "Max" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 16.71 COL 69
     "QTY to" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 6 COL 3
     "Setup $" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 6 COL 37
     "Purchased" VIEW-AS TEXT
          SIZE 13 BY .95 AT ROW 2.67 COL 2
     "Min" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 16.71 COL 22
     "Max" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 16.71 COL 34
     RECT-24 AT ROW 1 COL 1
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
         HEIGHT             = 19.43
         WIDTH              = 118.
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
/* SETTINGS FOR FILL-IN e-itemfg-vend.roll-w[27] IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN e-itemfg-vend.roll-w[28] IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN e-itemfg-vend.roll-w[29] IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN e-itemfg-vend.roll-w[30] IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN e-itemfg-vend.run-qty[10] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-itemfg-vend.run-qty[1] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-itemfg-vend.run-qty[2] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-itemfg-vend.run-qty[3] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-itemfg-vend.run-qty[4] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-itemfg-vend.run-qty[5] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-itemfg-vend.run-qty[6] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-itemfg-vend.run-qty[7] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-itemfg-vend.run-qty[8] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-itemfg-vend.run-qty[9] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-itemfg.std-uom IN FRAME F-Main
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN e-itemfg-vend.vend-no IN FRAME F-Main
   EXP-FORMAT                                                           */
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
    def var char-val as cha no-undo.
    def var uom-list as cha init "C,CS,EA,L,M," no-undo.


    case focus:name :
        when "std-uom" then do:
             /*find bf-itemfg of e-item no-lock no-error.*/
             RUN sys/ref/uom-fg.p  (NO, OUTPUT uom-list).
             run windows/l-stduom.w (gcompany,uom-list, focus:screen-value, output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       .
        end.
        when "vend-no" then do:
             run windows/l-vendno.w (gcompany, "", focus:screen-value, output char-val).
             if char-val <> "" then 
                assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       .
        end.

        WHEN "cust-no" THEN DO:
             RUN windows/l-cust.w (gcompany, e-itemfg-vend.cust-no:SCREEN-VALUE, OUTPUT char-val).
             IF char-val NE "" THEN
                e-itemfg-vend.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,char-val).
        END.
    end.    

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
DO:
  RUN new-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sel-01
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sel-01 V-table-Win
ON VALUE-CHANGED OF tb_sel-01 IN FRAME F-Main /* cost by vendor */
DO:
  RUN new-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sel-02
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sel-02 V-table-Win
ON VALUE-CHANGED OF tb_sel-02 IN FRAME F-Main /* tb_sel 2 */
DO:
  RUN new-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sel-03
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sel-03 V-table-Win
ON VALUE-CHANGED OF tb_sel-03 IN FRAME F-Main /* tb_sel 3 */
DO:
  RUN new-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sel-04
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sel-04 V-table-Win
ON VALUE-CHANGED OF tb_sel-04 IN FRAME F-Main /* tb_sel 4 */
DO:
  RUN new-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sel-05
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sel-05 V-table-Win
ON VALUE-CHANGED OF tb_sel-05 IN FRAME F-Main /* tb_sel 5 */
DO:
  RUN new-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sel-06
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sel-06 V-table-Win
ON VALUE-CHANGED OF tb_sel-06 IN FRAME F-Main /* tb_sel 6 */
DO:
  RUN new-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sel-07
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sel-07 V-table-Win
ON VALUE-CHANGED OF tb_sel-07 IN FRAME F-Main /* tb_sel 7 */
DO:
  RUN new-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sel-08
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sel-08 V-table-Win
ON VALUE-CHANGED OF tb_sel-08 IN FRAME F-Main /* tb_sel 8 */
DO:
  RUN new-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sel-09
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sel-09 V-table-Win
ON VALUE-CHANGED OF tb_sel-09 IN FRAME F-Main /* tb_sel 9 */
DO:
  RUN new-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sel-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sel-10 V-table-Win
ON VALUE-CHANGED OF tb_sel-10 IN FRAME F-Main /* tb_sel 10 */
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
    RUN valid-vend-no (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  ls-vend-name = "".
  FIND FIRST vend WHERE vend.company = e-itemfg.company
                    AND vend.vend-no = e-itemfg-vend.vend-no:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAIL vend THEN ls-vend-name = vend.NAME.
  DISP ls-vend-name WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}  /* asi field contents help */
run get-company (output gcompany).
session:data-entry-return = yes. 

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

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

  DEFINE INPUT PARAMETER ip-recid-eitem AS RECID NO-UNDO.
  DEFINE INPUT PARAMETER ip-recid-eitem-vend AS RECID NO-UNDO.
  DEFINE INPUT PARAMETER ip-recid-bf AS RECID NO-UNDO.
  def input param v-term like report.term-id no-undo.

  find bf-itemfg where recid(bf-itemfg) = ip-recid-bf.

  FIND e-itemfg WHERE recid(e-itemfg) = ip-recid-eitem NO-LOCK.
  FIND e-itemfg-vend WHERE RECID(e-itemfg-vend) = ip-recid-eitem-vend NO-LOCK.

  RUN dispatch ("display-fields").

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "tableio-source", OUTPUT char-hdl).

  RUN set-buttons IN WIDGET-HANDLE(char-hdl) ("action-chosen").

  ASSIGN gTerm = v-term
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
      eb.pur-man EQ YES) THEN
      DO:
         MESSAGE "Delete Estimate Price Matrices With These Values?" 
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ll-choice.

         IF ll-choice THEN
         DO:
            session:set-wait-state("GENERAL").
            FOR EACH eb WHERE
                eb.company EQ e-itemfg-vend.company AND
                eb.stock-no EQ e-itemfg-vend.i-no AND
                eb.pur-man EQ YES
                NO-LOCK,
                FIRST b-e-itemfg-vend WHERE
                     b-e-itemfg-vend.company EQ e-itemfg-vend.company AND
                     b-e-itemfg-vend.i-no EQ e-itemfg-vend.i-no AND
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

            session:set-wait-state("").
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
  /*def buffer bf-e-vend for e-itemfg-vend.
  def var char-hdl as cha no-undo.*/

  /* Code placed here will execute PRIOR to standard behavior. */
 /*find first bf-e-vend where bf-e-vend.company = e-itemfg.company
                          and bf-e-vend.i-no = e-itemfg.i-no   
                          and bf-e-vend.vend-no = ""
                          no-lock no-error.
  if avail bf-e-vend then do:
     message "Vendor# Blank already exists. Check existing vendor first." view-as alert-box error.
     return error.
  end.                        
*/
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN dispatch ("display-fields").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var i as int no-undo.
  def var lv-recid as recid no-undo.
  def var lv-eb-recid as recid no-undo.
  DEF VAR char-hdl AS cha NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN fi_oh-markup.
  END.

  IF AVAIL e-itemfg THEN lv-recid = recid(e-itemfg).
  ELSE DO:
     CREATE e-itemfg.
     ASSIGN e-itemfg.company = g_company
            e-itemfg.i-no = "".    
            lv-recid = RECID(e-itemfg).
  END.
  /*IF NOT AVAIL e-itemfg then*/
  find first e-itemfg where recid(e-itemfg) = lv-recid NO-ERROR.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  for each tmpfile: delete tmpfile .  end.

  do i = 1 to 30:
     if e-itemfg.roll-w[i] <> 0 then do:
        create tmpfile.
        assign tmpfile.siz = e-itemfg.roll-w[i]
               e-itemfg.roll-w[i] = 0.
     end.
  end.
  i = 1.
  for each tmpfile by tmpfile.siz :
      assign e-itemfg.roll-w[i] = tmpfile.siz
             i = i + 1.
  end.
  for each tmpfile: delete tmpfile .  end.
  do i = 1 to 10:
     create tmpfile.
     assign tmpfile.qty = e-itemfg-vend.run-qty[i]
            tmpfile.siz = e-itemfg-vend.run-cost[i]
            tmpfile.setups = e-itemfg-vend.setups[i]
            e-itemfg-vend.run-qty[i] = 0
            e-itemfg-vend.run-cost[i] = 0
            e-itemfg-vend.setups[i] = 0.
  end.
  i = 1.
  for each tmpfile by tmpfile.qty:
      if tmpfile.qty = 0 then next.
      assign e-itemfg-vend.run-qty[i] = tmpfile.qty
             e-itemfg-vend.run-cost[i] = tmpfile.siz
             e-itemfg-vend.setups[i] = tmpfile.setups.
      i = i + 1.       
  end.

  do i = 1 to 10:
     assign e-itemfg.run-qty[i] = e-itemfg-vend.run-qty[i]
            e-itemfg.run-cost[i] = e-itemfg-vend.run-cost[i]
            .
  end.

  IF e-itemfg-vend.i-no = "" THEN DO:
     run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
     run get-link-handle in adm-broker-hdl(widget-handle(char-hdl),"record-source", output char-hdl).
     run get-eb-record in widget-handle(char-hdl) (output lv-eb-recid).
     FIND FIRST eb WHERE RECID(eb) = lv-eb-recid NO-LOCK NO-ERROR.
     IF AVAIL eb THEN
        ASSIGN e-itemfg-vend.est-no = eb.est-no
               e-itemfg-vend.eqty = eb.eqty
               e-itemfg-vend.form-no = eb.form-no
               e-itemfg-vend.blank-no = eb.blank-no.

  END.

  FIND FIRST reftable WHERE
       reftable.reftable EQ 'e-itemfg-vend.markup' AND
       reftable.company EQ e-itemfg-vend.company AND
       reftable.loc EQ e-itemfg-vend.i-no AND
       reftable.code EQ e-itemfg-vend.vend-no
       EXCLUSIVE-LOCK NO-ERROR.

  IF NOT AVAILABLE reftable THEN DO:
    CREATE reftable.
    ASSIGN
      reftable.reftable = 'e-itemfg-vend.markup'
      reftable.company = e-itemfg-vend.company
      reftable.loc = e-itemfg-vend.i-no
      reftable.code = e-itemfg-vend.vend-no.
  END.

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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  disable e-itemfg.std-uom fi_oh-markup WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-recid as recid no-undo.
  def var char-hdl as cha no-undo.
  def var i as int no-undo.
  DEF BUFFER bf-eitemfg FOR e-itemfg.

  /* Code placed here will execute PRIOR to standard behavior. */

   /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
 run get-item-record in widget-handle(char-hdl) (output lv-recid).
 find bf-itemfg where recid(bf-itemfg) = lv-recid no-error.

 ASSIGN e-itemfg-vend.company = IF AVAIL e-itemfg THEN e-itemfg.company ELSE g_company
        e-itemfg-vend.i-no = IF AVAIL e-itemfg THEN e-itemfg.i-no ELSE ""
        e-itemfg-vend.item-type = NO 
        /*e-itemfg-vend.roll-w[28] = 999
        e-itemfg-vend.roll-w[30] = 999*/  .  /* for finished good */

 /*
 bf-itemfg.min-sqft = 0.       
 */
 do i = 1 to 10:
     assign e-itemfg-vend.run-qty[i] = IF AVAIL e-itemfg THEN e-itemfg.run-qty[i] ELSE 0
            e-itemfg-vend.run-cost[i] = IF AVAIL e-itemfg THEN e-itemfg.run-cost[i] ELSE 0
            .
  end.

  DO WITH FRAME {&FRAME-NAME}:

     IF adm-adding-record THEN
        ASSIGN
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

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  RUN delete-est-matrices-proc.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
/*  find first e-itemfg-vend of e-itemfg no-lock no-error.
  if not avail e-itemfg-vend then do:  /* no e-itemfg-vend then delete e-itemfg */
     def buffer bf-e-itemfg for e-itemfg.

     find bf-e-itemfg  where recid(bf-e-itemfg) = recid(e-itemfg) no-error.
     if avail bf-e-itemfg then delete bf-e-itemfg.

  end.
*/  
/* gdm - 08040903 - THIS IS FOR e-itemfg - FINISHGOODS  
                                e-item  - is for RAW MATERIALS
 */

if not avail e-itemfg then do:
   def var char-hdl as cha no-undo.
   run get-link-handle in adm-broker-hdl(this-procedure, "record-source", output char-hdl).
   run dispatch in widget-handle(char-hdl) ("open-query").
end.



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

  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL e-itemfg-vend AND e-itemfg-vend.setup NE 0 THEN DO TRANSACTION:
    FIND b-eiv WHERE ROWID(b-eiv) EQ ROWID(e-itemfg-vend).
    ASSIGN
     b-eiv.setups[1] = e-itemfg-vend.setup
     b-eiv.setup     = 0.
  END.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-group-hdl = FRAME {&FRAME-NAME}:FIRST-CHILD
     lv-field-hdl = lv-group-hdl:FIRST-CHILD.

    DO WHILE VALID-HANDLE(lv-field-hdl):
      IF lv-field-hdl:NAME BEGINS "tb_sel" THEN
        ASSIGN
         lv-field-hdl:SENSITIVE = NO
         lv-field-hdl:HIDDEN = NOT AVAIL e-itemfg-vend OR
                               e-itemfg-vend.est-no EQ "".

      lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
    END.

    IF adm-new-record AND adm-adding-record THEN lv-markup = fi_oh-markup:SCREEN-VALUE.

    IF AVAIL e-itemfg-vend AND NOT adm-new-record THEN
    DO:
       FIND FIRST reftable WHERE
            reftable.reftable EQ 'e-itemfg-vend.markup' AND
            reftable.company EQ e-itemfg-vend.company AND
            reftable.loc EQ e-itemfg-vend.i-no AND
            reftable.code EQ e-itemfg-vend.vend-no
            NO-LOCK NO-ERROR.

       IF NOT AVAILABLE reftable THEN
       DO:
          CREATE reftable.
          ASSIGN
             reftable.reftable = 'e-itemfg-vend.markup'
             reftable.company = e-itemfg-vend.company
             reftable.loc = e-itemfg-vend.i-no
             reftable.code = e-itemfg-vend.vend-no.
       END.

       fi_oh-markup = reftable.val[1].

    END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  IF adm-new-record AND adm-adding-record THEN
     fi_oh-markup:SCREEN-VALUE = lv-markup.
  /* Code placed here will execute AFTER standard behavior.    */
  /*task# 07190509*/
  ASSIGN ls-vend-name = ""  
         ls-item-name = ""
         ls-item-dscr = "".

  FIND FIRST vend WHERE vend.company = gcompany
                    AND vend.vend-no = e-itemfg-vend.vend-no:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAIL vend THEN ls-vend-name = vend.NAME.
  DISP ls-vend-name WITH FRAME {&FRAME-NAME}.

  /*task# 07190509*/
  IF AVAIL e-itemfg THEN
     FIND itemfg WHERE itemfg.company = gcompany AND itemfg.i-no = e-itemfg.i-no NO-LOCK NO-ERROR.
  IF AVAIL itemfg THEN ASSIGN ls-item-name = itemfg.i-name
                              ls-item-dscr = itemfg.part-dscr1.
  DISP ls-item-name ls-item-dscr WITH FRAME {&FRAME-NAME}.

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
  def var char-hdl as cha no-undo.
  def var lv-recid as recid no-undo.


  /* Code placed here will execute PRIOR to standard behavior. */
  run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
  run get-item-record in widget-handle(char-hdl) (output lv-recid).
  find bf-itemfg where recid(bf-itemfg) eq lv-recid no-lock no-error.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-group-hdl = FRAME {&FRAME-NAME}:FIRST-CHILD
     lv-field-hdl = lv-group-hdl:FIRST-CHILD.

    IF AVAIL e-itemfg-vend THEN
    DO WHILE VALID-HANDLE(lv-field-hdl):
      IF lv-field-hdl:NAME BEGINS "tb_sel" THEN
        lv-field-hdl:SENSITIVE = e-itemfg-vend.est-no NE "".

      lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
    END.

    ENABLE e-itemfg.std-uom fi_oh-markup.

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
  def buffer bf-evend for e-itemfg-vend.
  DEF VAR v-add-record AS LOG NO-UNDO.
  DEF VAR char-hdl AS cha NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  /* ============= validateion ================= */
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-vend-no (e-itemfg-vend.vend-no:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  RUN valid-std-uom NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-cust-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  {&methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF e-itemfg-vend.vend-no:SCREEN-VALUE EQ "" THEN DO:
       FIND FIRST bf-evend
           WHERE bf-evend.company   EQ e-itemfg-vend.company
             AND bf-evend.i-no      EQ e-itemfg-vend.i-no
             AND bf-evend.item-type EQ NO
             AND bf-evend.vend-no   EQ ""
             AND bf-evend.est-no    EQ e-itemfg-vend.est-no
             AND bf-evend.eqty      EQ e-itemfg-vend.eqty
             AND bf-evend.form-no   EQ e-itemfg-vend.form-no
             AND bf-evend.blank-no  EQ e-itemfg-vend.blank-no
             AND ROWID(bf-evend)    NE ROWID(e-itemfg-vend)
           NO-LOCK NO-ERROR.
       IF AVAIL bf-evend THEN DO:
         MESSAGE "Blank vendor exists..." 
             VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" to e-itemfg-vend.vend-no.
         RETURN NO-APPLY. 
       END.
     END.
  END.
  {&methods/lValidateError.i NO}

  /* ============= end of validation ================*/
  v-add-record = adm-adding-record.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF v-add-record THEN DO:
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
      RUN repos-query IN WIDGET-HANDLE(char-hdl) (ROWID(e-itemfg)).
  END.

  ELSE RUN dispatch ("display-fields").

  disable e-itemfg.std-uom fi_oh-markup WITH FRAME {&FRAME-NAME}.

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
      lv-test-valid-widget = FOCUS:NAME NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
          RETURN.

    IF FOCUS:NAME EQ "tb_sel" THEN lv-sel-all = FOCUS:SCREEN-VALUE.
    ELSE
    IF FOCUS:NAME BEGINS "tb_sel" THEN lv-sel = FOCUS:SCREEN-VALUE.
    ELSE lv-sel = tb_sel-01:SCREEN-VALUE.

    ASSIGN
     lv-group-hdl = FRAME {&FRAME-NAME}:FIRST-CHILD
     lv-field-hdl = lv-group-hdl:FIRST-CHILD.
    {&methods/lValidateError.i YES}
    DO WHILE VALID-HANDLE(lv-field-hdl):
      lv-test-valid-widget = lv-field-hdl:NAME NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
          RETURN.
      IF lv-field-hdl:NAME BEGINS "tb_sel" THEN
        IF lv-sel-all NE "" THEN
          lv-field-hdl:SCREEN-VALUE = lv-sel-all.
        ELSE
        IF lv-field-hdl:NAME NE "tb_sel"       AND
           lv-field-hdl:SCREEN-VALUE NE lv-sel THEN lv-sel = "".

      lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
    END.
    {&methods/lValidateError.i NO}
    IF lv-sel NE "" THEN lv-sel-all = lv-sel.

    IF lv-sel-all NE "" THEN tb_sel:SCREEN-VALUE = lv-sel-all.
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
   def var v-pct as dec no-undo.
   def var i as int no-undo.
   def var char-hdl as cha no-undo.
   def buffer bf-e-itemfg-vend for e-itemfg-vend.


   v-pct = 0.
   message "By what percentage:" update v-pct .

   status default "Processing Raw Material: " + string(e-itemfg.i-no).

   find bf-e-itemfg-vend where recid(bf-e-itemfg-vend) = recid(e-itemfg-vend).

   do i = 1 to 10:
      bf-e-itemfg-vend.run-cost[i] = e-itemfg-vend.run-cost[i] + 
                                (e-itemfg-vend.run-cost[i] * v-pct / 100).
   end.

   run get-link-handle in adm-broker-hdl (this-procedure, "record-source", output char-hdl).
   run reopen-and-repo in widget-handle(char-hdl) (rowid(bf-e-itemfg-vend)).

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
      eb.pur-man EQ YES) THEN
      DO:
         MESSAGE "Update Estimate Price Matrices With These Values?" 
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ll-choice.

         IF ll-choice THEN
         DO:
            session:set-wait-state("GENERAL").
            FOR EACH eb WHERE
                eb.company EQ e-itemfg-vend.company AND
                eb.stock-no EQ e-itemfg-vend.i-no AND
                eb.pur-man EQ YES
                NO-LOCK:

                FIND FIRST b-e-itemfg-vend WHERE
                     b-e-itemfg-vend.company EQ e-itemfg-vend.company AND
                     b-e-itemfg-vend.i-no EQ e-itemfg-vend.i-no AND
                     b-e-itemfg-vend.est-no EQ eb.est-no AND
                     b-e-itemfg-vend.form-no EQ eb.form-no AND
                     b-e-itemfg-vend.eqty EQ eb.eqty AND
                     b-e-itemfg-vend.blank-no EQ eb.blank-no AND
                     b-e-itemfg-vend.vend-no EQ e-itemfg-vend.vend-no AND
                     b-e-itemfg-vend.cust-no EQ e-itemfg-vend.cust-no
                     NO-ERROR.

                IF AVAIL b-e-itemfg-vend THEN
                   BUFFER-COPY e-itemfg-vend EXCEPT i-no rec_key est-no eqty form-no blank-no
                             TO b-e-itemfg-vend.
                ELSE
                DO:
                   CREATE b-e-itemfg-vend.
                   BUFFER-COPY e-itemfg-vend EXCEPT rec_key est-no eqty form-no blank-no
                             TO b-e-itemfg-vend
                     ASSIGN b-e-itemfg-vend.est-no = eb.est-no
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

                IF AVAIL b-reftable-1 THEN
                DO:
                   FIND FIRST e-itemfg WHERE
                        e-itemfg.company EQ b-e-itemfg-vend.company AND
                        e-itemfg.i-no EQ b-e-itemfg-vend.i-no
                        NO-LOCK NO-ERROR.

                   IF AVAIL e-itemfg THEN
                   DO:
                      b-reftable-1.code2 = e-itemfg.std-uom.
                      RELEASE e-itemfg.
                   END.

                   RELEASE b-reftable-1.
                END.
                ELSE
                DO:
                   CREATE b-reftable-1.
                   ASSIGN
                      b-reftable-1.reftable = "e-itemfg-vend.std-uom"
                      b-reftable-1.company  = b-e-itemfg-vend.company
                      b-reftable-1.loc      = ""
                      b-reftable-1.code     = b-e-itemfg-vend.est-no
                      b-reftable-1.val[1]   = b-e-itemfg-vend.form-no
                      b-reftable-1.val[2]   = b-e-itemfg-vend.blank-no.

                   FIND FIRST e-itemfg WHERE
                        e-itemfg.company EQ b-e-itemfg-vend.company AND
                        e-itemfg.i-no EQ b-e-itemfg-vend.i-no
                        NO-LOCK NO-ERROR.

                   IF AVAIL e-itemfg THEN
                   DO:
                      b-reftable-1.code2 = e-itemfg.std-uom.
                      RELEASE e-itemfg.
                   END.

                   RELEASE b-reftable-1.
                END.

                RELEASE b-e-itemfg-vend.
            END.

            session:set-wait-state("").
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
      IF e-itemfg-vend.cust-no:SCREEN-VALUE NE "" AND
         NOT CAN-FIND(FIRST cust WHERE
             cust.company EQ gcompany AND
             cust.cust-no EQ e-itemfg-vend.cust-no:SCREEN-VALUE) THEN
      DO:
         MESSAGE "Invalid Cust. #, try help..."
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
    e-itemfg.std-uom:SCREEN-VALUE = CAPS(e-itemfg.std-uom:SCREEN-VALUE).

    RUN sys/ref/uom-fg.p (NO, OUTPUT uom-list).

    IF LOOKUP(e-itemfg.std-uom:SCREEN-VALUE,uom-list) LE 0 OR 
       (e-itemfg.std-uom:SCREEN-VALUE EQ "MSF" AND
        NOT CAN-FIND(FIRST bf-itemfg
                     WHERE bf-itemfg.company EQ e-itemfg.company
                       AND bf-itemfg.i-no    EQ e-itemfg.i-no
                       AND ((bf-itemfg.t-len NE 0 AND bf-itemfg.t-wid NE 0) OR
                            bf-itemfg.t-sqin NE 0                           OR
                            bf-itemfg.t-sqft NE 0)))     THEN DO:

      IF e-itemfg.std-uom:SCREEN-VALUE EQ "MSF" THEN
        MESSAGE "When " + TRIM(e-itemfg.std-uom:LABEL) + " is MSF, " +
                "FG Item must have valid Blank Length & Width, SqIn, or SqFt..."
            VIEW-AS ALERT-BOX ERROR.
      ELSE
        MESSAGE TRIM(e-itemfg.std-uom:LABEL) +
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
  DO WITH FRAME {&FRAME-NAME}:
    IF ip-focus:SCREEN-VALUE NE "" THEN
      IF e-itemfg-vend.vend-no EQ "" AND NOT adm-new-record THEN
        lv-msg = "Sorry, you cannot change the 'Blank Vendor', please use copy button".

      ELSE
      IF NOT CAN-FIND(FIRST vend
                      WHERE vend.company EQ e-itemfg.company
                        AND vend.vend-no EQ ip-focus:SCREEN-VALUE) THEN
        lv-msg = TRIM(ip-focus:LABEL) + " is invalid, try help".

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

