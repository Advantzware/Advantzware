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

  File: viewers\e-item.w

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
def buffer bf-item for item.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.
DEF BUFFER b-blank-vend-qty FOR reftable.
DEF BUFFER b-blank-vend-cost FOR reftable.

DEF BUFFER b2-qty FOR reftable.
DEF BUFFER b2-cost FOR reftable.
DEF BUFFER b2-setup FOR reftable.
DEF BUFFER b2-blank-vend-qty FOR reftable.
DEF BUFFER b2-blank-vend-cost FOR reftable.

DEF VAR v-copy-record AS LOG NO-UNDO.
DEF VAR v-old-vend-no AS CHAR NO-UNDO.

def temp-table tmpfile NO-UNDO
    field siz as dec
    field qty as dec
    field setups as dec.

def var lv-roll-w like e-item-vend.roll-w no-undo.
{custom/gcompany.i}
{custom/persist.i}
def var uom-list as cha init ["M,EA,L,CS,C"] no-undo.
DEF VAR char-hdl AS CHAR NO-UNDO.

&SCOPED-DEFINE where-adders                         ~
    WHERE reftable.rec_key  EQ e-item-vend.rec_key  ~
      AND reftable.reftable EQ "e-item-vend.adders" ~
    USE-INDEX rec_key

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
&Scoped-define EXTERNAL-TABLES e-item-vend e-item
&Scoped-define FIRST-EXTERNAL-TABLE e-item-vend


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR e-item-vend, e-item.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS e-item-vend.vend-item e-item-vend.vend-no ~
e-item-vend.updated-date[1] e-item-vend.run-qty[1] e-item-vend.run-cost[1] ~
e-item-vend.setups[1] e-item-vend.run-qty[2] e-item-vend.run-cost[2] ~
e-item-vend.setups[2] e-item-vend.run-qty[3] e-item-vend.run-cost[3] ~
e-item-vend.setups[3] e-item-vend.run-qty[4] e-item-vend.run-cost[4] ~
e-item-vend.setups[4] e-item-vend.run-qty[5] e-item-vend.run-cost[5] ~
e-item-vend.setups[5] e-item-vend.run-qty[6] e-item-vend.run-cost[6] ~
e-item-vend.setups[6] e-item-vend.run-qty[7] e-item-vend.run-cost[7] ~
e-item-vend.setups[7] e-item-vend.run-qty[8] e-item-vend.run-cost[8] ~
e-item-vend.setups[8] e-item-vend.run-qty[9] e-item-vend.run-cost[9] ~
e-item-vend.setups[9] e-item-vend.run-qty[10] e-item-vend.run-cost[10] ~
e-item-vend.setups[10] e-item-vend.roll-w[1] e-item-vend.roll-w[2] ~
e-item-vend.roll-w[3] e-item-vend.roll-w[4] e-item-vend.roll-w[5] ~
e-item-vend.roll-w[6] e-item-vend.roll-w[7] e-item-vend.roll-w[8] ~
e-item-vend.roll-w[9] e-item-vend.roll-w[10] e-item-vend.roll-w[11] ~
e-item-vend.roll-w[12] e-item-vend.roll-w[13] e-item-vend.roll-w[14] ~
e-item-vend.roll-w[15] e-item-vend.roll-w[16] e-item-vend.roll-w[17] ~
e-item-vend.roll-w[18] e-item-vend.roll-w[19] e-item-vend.roll-w[20] ~
e-item-vend.roll-w[21] e-item-vend.roll-w[22] e-item-vend.roll-w[23] ~
e-item-vend.roll-w[24] e-item-vend.roll-w[25] e-item-vend.roll-w[26] ~
e-item-vend.roll-w[27] e-item-vend.roll-w[28] e-item-vend.roll-w[29] ~
e-item-vend.roll-w[30] 
&Scoped-define ENABLED-TABLES e-item-vend
&Scoped-define FIRST-ENABLED-TABLE e-item-vend
&Scoped-Define ENABLED-OBJECTS btn_more-breaks RECT-24 RECT-25 
&Scoped-Define DISPLAYED-FIELDS e-item-vend.i-no e-item.std-uom ~
e-item-vend.vend-item e-item-vend.vend-no e-item-vend.updated-date[1] ~
e-item-vend.run-qty[1] e-item-vend.run-cost[1] e-item-vend.setups[1] ~
e-item-vend.run-qty[2] e-item-vend.run-cost[2] e-item-vend.setups[2] ~
e-item-vend.run-qty[3] e-item-vend.run-cost[3] e-item-vend.setups[3] ~
e-item-vend.run-qty[4] e-item-vend.run-cost[4] e-item-vend.setups[4] ~
e-item-vend.run-qty[5] e-item-vend.run-cost[5] e-item-vend.setups[5] ~
e-item-vend.run-qty[6] e-item-vend.run-cost[6] e-item-vend.setups[6] ~
e-item-vend.run-qty[7] e-item-vend.run-cost[7] e-item-vend.setups[7] ~
e-item-vend.run-qty[8] e-item-vend.run-cost[8] e-item-vend.setups[8] ~
e-item-vend.run-qty[9] e-item-vend.run-cost[9] e-item-vend.setups[9] ~
e-item-vend.run-qty[10] e-item-vend.run-cost[10] e-item-vend.setups[10] ~
e-item-vend.roll-w[1] e-item-vend.roll-w[2] e-item-vend.roll-w[3] ~
e-item-vend.roll-w[4] e-item-vend.roll-w[5] e-item-vend.roll-w[6] ~
e-item-vend.roll-w[7] e-item-vend.roll-w[8] e-item-vend.roll-w[9] ~
e-item-vend.roll-w[10] e-item-vend.roll-w[11] e-item-vend.roll-w[12] ~
e-item-vend.roll-w[13] e-item-vend.roll-w[14] e-item-vend.roll-w[15] ~
e-item-vend.roll-w[16] e-item-vend.roll-w[17] e-item-vend.roll-w[18] ~
e-item-vend.roll-w[19] e-item-vend.roll-w[20] e-item-vend.roll-w[21] ~
e-item-vend.roll-w[22] e-item-vend.roll-w[23] e-item-vend.roll-w[24] ~
e-item-vend.roll-w[25] e-item-vend.roll-w[26] e-item-vend.roll-w[27] ~
e-item-vend.roll-w[28] e-item-vend.roll-w[29] e-item-vend.roll-w[30] 
&Scoped-define DISPLAYED-TABLES e-item-vend e-item
&Scoped-define FIRST-DISPLAYED-TABLE e-item-vend
&Scoped-define SECOND-DISPLAYED-TABLE e-item
&Scoped-Define DISPLAYED-OBJECTS lbl_setup lbl_roll-w ls-item-name ~
ls-vend-name fi_width-min fi_width-cst fi_length-min fi_length-cst 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS e-item.std-uom 
&Scoped-define ADM-ASSIGN-FIELDS e-item.std-uom fi_width-min fi_width-cst ~
fi_length-min fi_length-cst 
&Scoped-define List-3 lbl_roll-w e-item-vend.roll-w[1] ~
e-item-vend.roll-w[2] e-item-vend.roll-w[3] e-item-vend.roll-w[4] ~
e-item-vend.roll-w[5] e-item-vend.roll-w[6] e-item-vend.roll-w[7] ~
e-item-vend.roll-w[8] e-item-vend.roll-w[9] e-item-vend.roll-w[10] ~
e-item-vend.roll-w[11] e-item-vend.roll-w[12] e-item-vend.roll-w[13] ~
e-item-vend.roll-w[14] e-item-vend.roll-w[15] e-item-vend.roll-w[16] ~
e-item-vend.roll-w[17] e-item-vend.roll-w[18] e-item-vend.roll-w[19] ~
e-item-vend.roll-w[20] e-item-vend.roll-w[21] e-item-vend.roll-w[22] ~
e-item-vend.roll-w[23] e-item-vend.roll-w[24] e-item-vend.roll-w[25] ~
e-item-vend.roll-w[26] RECT-25 
&Scoped-define List-4 e-item-vend.roll-w[27] e-item-vend.roll-w[28] ~
e-item-vend.roll-w[29] e-item-vend.roll-w[30] 
&Scoped-define List-5 lbl_setup e-item-vend.setups[1] e-item-vend.setups[2] ~
e-item-vend.setups[3] e-item-vend.setups[4] e-item-vend.setups[5] ~
e-item-vend.setups[6] e-item-vend.setups[7] e-item-vend.setups[8] ~
e-item-vend.setups[9] e-item-vend.setups[10] 
&Scoped-define List-6 btn_more-breaks fi_width-min fi_width-cst ~
fi_length-min fi_length-cst 

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
DEFINE BUTTON btn_more-breaks 
     LABEL "More Breaks" 
     SIZE 15 BY 1.

DEFINE VARIABLE fi_length-cst AS DECIMAL FORMAT ">>,>>9.9999":U INITIAL 0 
     LABEL "$" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 NO-UNDO.

DEFINE VARIABLE fi_length-min AS DECIMAL FORMAT ">>9.99999":U INITIAL 0 
     LABEL "Length" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi_width-cst AS DECIMAL FORMAT ">>,>>9.9999":U INITIAL 0 
     LABEL "$" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1 NO-UNDO.

DEFINE VARIABLE fi_width-min AS DECIMAL FORMAT ">>9.99999":U INITIAL 0 
     LABEL "Under Width" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_roll-w AS CHARACTER FORMAT "X(256)":U INITIAL "Valid  Estimated  Roll  Width" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_setup AS CHARACTER FORMAT "X(256)":U INITIAL "Setup $" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE ls-item-name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE ls-vend-name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 58.8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 17.62.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 10.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     lbl_setup AT ROW 5.05 COL 34 COLON-ALIGNED NO-LABEL
     lbl_roll-w AT ROW 5.05 COL 53 COLON-ALIGNED NO-LABEL
     e-item-vend.i-no AT ROW 1 COL 15 COLON-ALIGNED FORMAT "x(10)"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ls-item-name AT ROW 1 COL 31 COLON-ALIGNED NO-LABEL
     e-item.std-uom AT ROW 2.1 COL 31 COLON-ALIGNED
          LABEL "Purchased Cost UOM" FORMAT "x(4)"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     e-item-vend.vend-item AT ROW 2.1 COL 61 COLON-ALIGNED FORMAT "x(15)" NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     e-item-vend.vend-no AT ROW 3.19 COL 15 COLON-ALIGNED FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ls-vend-name AT ROW 3.19 COL 31.2 COLON-ALIGNED NO-LABEL
     e-item-vend.updated-date[1] AT ROW 4.19 COL 16.8 COLON-ALIGNED HELP
          "Enter the cost update date." WIDGET-ID 2
          LABEL "Cost Updated"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     e-item-vend.run-qty[1] AT ROW 6 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     e-item-vend.run-cost[1] AT ROW 6 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.setups[1] AT ROW 6 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     e-item-vend.run-qty[2] AT ROW 6.95 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     e-item-vend.run-cost[2] AT ROW 6.95 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.setups[2] AT ROW 6.95 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     e-item-vend.run-qty[3] AT ROW 7.91 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.run-cost[3] AT ROW 7.91 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.setups[3] AT ROW 7.91 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     e-item-vend.run-qty[4] AT ROW 8.86 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY .95
     e-item-vend.run-cost[4] AT ROW 8.86 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.setups[4] AT ROW 8.86 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     e-item-vend.run-qty[5] AT ROW 9.81 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.run-cost[5] AT ROW 9.81 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.setups[5] AT ROW 9.81 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     e-item-vend.run-qty[6] AT ROW 10.76 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.run-cost[6] AT ROW 10.76 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.setups[6] AT ROW 10.76 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     e-item-vend.run-qty[7] AT ROW 11.71 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.run-cost[7] AT ROW 11.71 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     e-item-vend.setups[7] AT ROW 11.71 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     e-item-vend.run-qty[8] AT ROW 12.67 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.run-cost[8] AT ROW 12.67 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.setups[8] AT ROW 12.67 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     e-item-vend.run-qty[9] AT ROW 13.62 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.run-cost[9] AT ROW 13.62 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.setups[9] AT ROW 13.62 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     e-item-vend.run-qty[10] AT ROW 14.57 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.run-cost[10] AT ROW 14.57 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.setups[10] AT ROW 14.57 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     e-item-vend.roll-w[1] AT ROW 6.24 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[2] AT ROW 7.19 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[3] AT ROW 8.14 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[4] AT ROW 9.1 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[5] AT ROW 10.05 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[6] AT ROW 11 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[7] AT ROW 11.95 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[8] AT ROW 12.91 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[9] AT ROW 13.86 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[10] AT ROW 6.71 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[11] AT ROW 7.67 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[12] AT ROW 8.62 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[13] AT ROW 9.57 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[14] AT ROW 10.52 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[15] AT ROW 11.48 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     e-item-vend.roll-w[16] AT ROW 12.43 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[17] AT ROW 13.38 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[18] AT ROW 6.24 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[19] AT ROW 7.19 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[20] AT ROW 8.14 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[21] AT ROW 9.1 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[22] AT ROW 10.05 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[23] AT ROW 11 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY .95
     e-item-vend.roll-w[24] AT ROW 11.95 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[25] AT ROW 12.91 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     e-item-vend.roll-w[26] AT ROW 13.86 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     btn_more-breaks AT ROW 15.48 COL 2.4 WIDGET-ID 4
     fi_width-min AT ROW 16.48 COL 18 COLON-ALIGNED HELP
          "Minimum width to avoid added cost at right"
     fi_width-cst AT ROW 16.48 COL 34 COLON-ALIGNED HELP
          "Amount to Charge per UOM when under width"
     fi_length-min AT ROW 17.48 COL 18 COLON-ALIGNED HELP
          "Minimum length to avoid added cost at right"
     fi_length-cst AT ROW 17.48 COL 34 COLON-ALIGNED HELP
          "Amount to Charge per UOM when under width"
     e-item-vend.roll-w[27] AT ROW 16.48 COL 66 COLON-ALIGNED
          LABEL "Sheet Width"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     e-item-vend.roll-w[28] AT ROW 16.48 COL 78 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     e-item-vend.roll-w[29] AT ROW 17.48 COL 66 COLON-ALIGNED
          LABEL "Length"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     e-item-vend.roll-w[30] AT ROW 17.48 COL 78 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     "Cost Per" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 5.05 COL 19
     "QTY to" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 5.05 COL 2
     "Min" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 15.52 COL 70
     "Max" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 15.52 COL 82
     "Vendor Item No:" VIEW-AS TEXT
          SIZE 28 BY .62 AT ROW 1.24 COL 63.2 WIDGET-ID 10
     RECT-24 AT ROW 1 COL 1
     RECT-25 AT ROW 4.81 COL 52
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.e-item-vend,ASI.e-item
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
         HEIGHT             = 17.71
         WIDTH              = 92.8.
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

/* SETTINGS FOR BUTTON btn_more-breaks IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR FILL-IN fi_length-cst IN FRAME F-Main
   NO-ENABLE 2 6                                                        */
/* SETTINGS FOR FILL-IN fi_length-min IN FRAME F-Main
   NO-ENABLE 2 6                                                        */
/* SETTINGS FOR FILL-IN fi_width-cst IN FRAME F-Main
   NO-ENABLE 2 6                                                        */
/* SETTINGS FOR FILL-IN fi_width-min IN FRAME F-Main
   NO-ENABLE 2 6                                                        */
/* SETTINGS FOR FILL-IN e-item-vend.i-no IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN lbl_roll-w IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN lbl_setup IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN ls-item-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-vend-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-25 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[10] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[11] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[12] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[13] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[14] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[15] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[16] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[17] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[18] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[19] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[1] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[20] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[21] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[22] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[23] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[24] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[25] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[26] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[27] IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[28] IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[29] IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[2] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[30] IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[3] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[4] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[5] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[6] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[7] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[8] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.roll-w[9] IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.run-qty[10] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-item-vend.run-qty[1] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-item-vend.run-qty[2] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-item-vend.run-qty[3] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-item-vend.run-qty[4] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-item-vend.run-qty[5] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-item-vend.run-qty[6] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-item-vend.run-qty[7] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-item-vend.run-qty[8] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-item-vend.run-qty[9] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN e-item-vend.setups[10] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.setups[1] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.setups[2] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.setups[3] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.setups[4] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.setups[5] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.setups[6] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.setups[7] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.setups[8] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN e-item-vend.setups[9] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN e-item.std-uom IN FRAME F-Main
   NO-ENABLE 1 2 EXP-LABEL EXP-FORMAT                                   */
/* SETTINGS FOR FILL-IN e-item-vend.updated-date[1] IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN e-item-vend.vend-no IN FRAME F-Main
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
  DEF VAR char-val AS CHAR NO-UNDO.
  DEF VAR lw-focus AS HANDLE NO-UNDO.


  lw-focus = FOCUS.

  CASE lw-focus:NAME:
    WHEN "std-uom" THEN DO:
      FIND bf-item OF e-item NO-LOCK NO-ERROR.
      RUN sys/ref/uom-rm.p  (bf-item.mat-type, output uom-list).
      RUN windows/l-stduom.w (gcompany, uom-list, lw-focus:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND ENTRY(1,char-val) NE lw-focus:SCREEN-VALUE THEN
        lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
    END.
    WHEN "vend-no" THEN DO:
      run windows/l-vendno.w (gcompany, "", lw-focus:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND ENTRY(1,char-val) NE lw-focus:SCREEN-VALUE THEN DO:
        lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
        RUN new-vend.
      END.
    END.
    WHEN "fi_adder-w" OR WHEN "fi_adder-l" THEN DO:
      RUN windows/l-itemi.w (gcompany, "", "A", "", lw-focus:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND ENTRY(1,char-val) NE lw-focus:SCREEN-VALUE THEN
        lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
    END.
  END.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_more-breaks
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_more-breaks V-table-Win
ON CHOOSE OF btn_more-breaks IN FRAME F-Main /* More Breaks */
DO:
    DO:
        RUN rm/d-rmbreaks.w PERSISTENT SET hProgram (INPUT ROWID(e-item-vend), INPUT ROWID(e-item)).
        RUN dispatch IN hProgram ("initialize").
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME e-item.std-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL e-item.std-uom V-table-Win
ON LEAVE OF e-item.std-uom IN FRAME F-Main /* Purchased Cost UOM */
DO:
    {&methods/lValidateError.i YES}
    find bf-item of e-item no-lock no-error.
    run sys/ref/uom-rm.p (bf-item.mat-type, output uom-list). 

    if lastkey <> -1 AND
       E-ITEM.std-uom:screen-value <> "" and
       /* not can-find(uom where uom.uom = self:screen-value)  */
        LOOKUP(SELF:SCREEN-VALUE,uom-list) <= 0
    then do:
       message "Invalid UOM. Try Help." view-as alert-box error.       
       return no-apply.
    end.   
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME e-item-vend.vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL e-item-vend.vend-no V-table-Win
ON LEAVE OF e-item-vend.vend-no IN FRAME F-Main /* Vendor */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-vend-no (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL e-item-vend.vend-no V-table-Win
ON VALUE-CHANGED OF e-item-vend.vend-no IN FRAME F-Main /* Vendor */
DO:
  RUN new-vend.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
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
  /*called from windows/d-vndcst.w*/
  DEFINE INPUT PARAMETER ip-item-recid AS RECID NO-UNDO.
  DEFINE INPUT PARAMETER ip-eitem-recid AS RECID NO-UNDO.
  DEFINE INPUT PARAMETER ip-eitem-vend-recid AS RECID NO-UNDO.
  def input param v-term like report.term-id no-undo.

  find bf-item where recid(bf-item) = ip-item-recid.

  RUN dispatch ("add-records").

  FIND e-item WHERE recid(e-item) = ip-eitem-recid NO-LOCK.
  FIND e-item-vend WHERE recid(e-item-vend) = ip-eitem-vend-recid NO-LOCK.

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
  {src/adm/template/row-list.i "e-item-vend"}
  {src/adm/template/row-list.i "e-item"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "e-item-vend"}
  {src/adm/template/row-find.i "e-item"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-manual-fields V-table-Win 
PROCEDURE disable-manual-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&list-4} {&list-6}.
    ENABLE btn_more-breaks.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE label-display V-table-Win 
PROCEDURE label-display :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-recid AS RECID NO-UNDO.


  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
  RUN get-item-record IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-recid).
  FIND bf-item WHERE RECID(bf-item) EQ lv-recid NO-LOCK NO-ERROR.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fi_width-min:HIDDEN  = YES
     fi_length-min:HIDDEN = YES
     fi_width-cst:HIDDEN  = YES
     fi_length-cst:HIDDEN = YES.

    IF AVAIL bf-item AND bf-item.industry EQ "2" AND bf-item.mat-type EQ "B" THEN
      ASSIGN
       fi_width-min:HIDDEN  = NO
       fi_length-min:HIDDEN = NO
       fi_width-cst:HIDDEN  = NO
       fi_length-cst:HIDDEN = NO.
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
  DEF BUFFER bf-evend FOR e-item-vend.

  def var i as int no-undo.
  def var lv-recid as recid no-undo.
  DEF VAR v-count AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  lv-recid = recid(e-item).
  find first e-item where recid(e-item) = lv-recid .

  DISABLE TRIGGERS FOR LOAD OF bf-evend.

  IF TRIM(e-item-vend.vend-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}) EQ "" THEN
  FOR EACH bf-evend
      WHERE bf-evend.company   EQ e-item.company
        AND bf-evend.i-no      EQ e-item.i-no
        AND bf-evend.item-type EQ YES
        AND bf-evend.vend-no   EQ ""
        AND ROWID(bf-evend)    NE ROWID(e-item-vend):
    DELETE bf-evend.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN reftable-values (NO).

  EMPTY TEMP-TABLE tmpfile.

  do i = 1 to 26:
     if e-item-vend.roll-w[i] <> 0 then do:
        create tmpfile.
        assign tmpfile.siz = e-item-vend.roll-w[i]
               e-item-vend.roll-w[i] = 0.
     end.
  end.
  i = 1.
  for each tmpfile by tmpfile.siz :
      assign e-item-vend.roll-w[i] = tmpfile.siz
             i = i + 1.
  end.
  EMPTY TEMP-TABLE tmpfile.

  IF v-copy-record THEN
  DO:
     FIND FIRST b2-qty WHERE
          b2-qty.reftable = "vend-qty" AND
          b2-qty.company = e-item-vend.company AND
          b2-qty.CODE    = e-item-vend.i-no AND
          b2-qty.code2   = v-old-vend-no
          NO-ERROR.

     FIND FIRST b2-cost WHERE
          b2-cost.reftable = "vend-cost" AND
          b2-cost.company = e-item-vend.company AND
          b2-cost.CODE    = e-item-vend.i-no AND
          b2-cost.code2   = v-old-vend-no
          NO-ERROR.

     FIND FIRST b2-setup WHERE
          b2-setup.reftable = "vend-setup" AND
          b2-setup.company = e-item-vend.company AND
          b2-setup.CODE    = e-item-vend.i-no AND
          b2-setup.code2   = v-old-vend-no
          NO-ERROR.

     IF AVAIL b2-qty AND AVAIL b2-cost AND AVAIL b2-setup THEN
     DO:
        FIND FIRST b-qty WHERE
             b-qty.reftable = "vend-qty" AND
             b-qty.company = e-item-vend.company AND
             b-qty.CODE    = e-item-vend.i-no AND
             b-qty.code2   = e-item-vend.vend-no
             NO-ERROR.

        FIND FIRST b-cost WHERE
             b-cost.reftable = "vend-cost" AND
             b-cost.company = e-item-vend.company AND
             b-cost.CODE    = e-item-vend.i-no AND
             b-cost.code2   = e-item-vend.vend-no
             NO-ERROR.

        FIND FIRST b-setup WHERE
             b-setup.reftable = "vend-setup" AND
             b-setup.company = e-item-vend.company AND
             b-setup.CODE    = e-item-vend.i-no AND
             b-setup.code2   = e-item-vend.vend-no
             NO-ERROR.

        IF NOT AVAIL b-qty THEN
        DO:
           CREATE b-qty.
           BUFFER-COPY b2-qty EXCEPT code2 TO b-qty
              ASSIGN b-qty.code2 = e-item-vend.vend-no.
           RELEASE b-qty.
        END.
        ELSE
           DO v-count = 1 TO 10:
              b-qty.val[v-count] = b2-qty.val[v-count].
           END.

        IF NOT AVAIL b-cost THEN
        DO:
           CREATE b-cost.
           BUFFER-COPY b2-cost EXCEPT code2 TO b-cost
              ASSIGN b-cost.code2 = e-item-vend.vend-no.
           RELEASE b-cost.
        END.
        ELSE
           DO v-count = 1 TO 10:
              b-cost.val[v-count] = b2-cost.val[v-count].
           END.

        IF NOT AVAIL b-setup THEN
        DO:
           CREATE b-setup.
           BUFFER-COPY b2-setup EXCEPT code2 TO b-setup
              ASSIGN b-setup.code2 = e-item-vend.vend-no.
           RELEASE b-setup.
        END.
        ELSE
        DO v-count = 1 TO 10:
           b-setup.val[v-count] = b2-setup.val[v-count].
        END.

        IF e-item-vend.vend-no EQ "" THEN
        DO:
           FIND FIRST b-blank-vend-qty WHERE
                b-blank-vend-qty.reftable = "blank-vend-qty" AND
                b-blank-vend-qty.company = e-item.company and
                    b-blank-vend-qty.CODE    = e-item.i-no
                NO-ERROR.

           IF NOT AVAIL b-blank-vend-qty THEN
           DO:
              CREATE b-blank-vend-qty.
              ASSIGN
                 b-blank-vend-qty.reftable = "blank-vend-qty"
                 b-blank-vend-qty.company = e-item.company
                     b-blank-vend-qty.CODE    = e-item.i-no.
           END.

           DO v-count = 1 TO 10:
              b-blank-vend-qty.val[v-count] = b-qty.val[v-count].
           END.

           FIND FIRST b-blank-vend-cost WHERE
                b-blank-vend-cost.reftable = "blank-vend-cost" AND
                b-blank-vend-cost.company = e-item.company and
                    b-blank-vend-cost.CODE    = e-item.i-no
                NO-ERROR.

           IF NOT AVAIL b-blank-vend-cost THEN
           DO:
              CREATE b-blank-vend-cost.
              ASSIGN
                 b-blank-vend-cost.reftable = "blank-vend-cost"
                 b-blank-vend-cost.company = e-item.company
                     b-blank-vend-cost.CODE    = e-item.i-no.
           END.

           DO v-count = 1 TO 10:
              b-blank-vend-cost.val[v-count] = b-cost.val[v-count].
           END.
        END.
     END.
  END.

  FIND FIRST b-qty WHERE
       b-qty.reftable = "vend-qty" AND
       b-qty.company = e-item-vend.company AND
       b-qty.CODE    = e-item-vend.i-no AND
       b-qty.code2   = e-item-vend.vend-no
       NO-ERROR.

  FIND FIRST b-cost WHERE
       b-cost.reftable = "vend-cost" AND
       b-cost.company = e-item-vend.company AND
       b-cost.CODE    = e-item-vend.i-no AND
       b-cost.code2   = e-item-vend.vend-no
       NO-ERROR.

  FIND FIRST b-setup WHERE
       b-setup.reftable = "vend-setup" AND
       b-setup.company = e-item-vend.company AND
       b-setup.CODE    = e-item-vend.i-no AND
       b-setup.code2   = e-item-vend.vend-no
       NO-ERROR.

  IF AVAIL b-qty AND AVAIL b-cost AND AVAIL b-setup THEN
     v-count = 20.
  ELSE
     v-count = 10.

  do i = 1 to v-count:
     create tmpfile.

     IF i LE 10 THEN
        assign tmpfile.qty = e-item-vend.run-qty[i]
               tmpfile.siz = e-item-vend.run-cost[i]
               tmpfile.setups = e-item-vend.setups[i]
               e-item-vend.run-qty[i] = 0
               e-item-vend.run-cost[i] = 0
               e-item-vend.setups[i] = 0.
     ELSE
        assign tmpfile.qty = b-qty.val[i - 10]
               tmpfile.siz = b-cost.val[i - 10]
               tmpfile.setups = b-setup.val[i - 10]
               b-qty.val[i - 10] = 0
               b-cost.val[i - 10] = 0
               b-setup.val[i - 10] = 0.
  end.
  i = 1.

  IF e-item-vend.vend-no EQ "" THEN
  DO:
     FIND FIRST b-blank-vend-qty WHERE
          b-blank-vend-qty.reftable = "blank-vend-qty" AND
          b-blank-vend-qty.company = e-item.company AND
          b-blank-vend-qty.CODE    = e-item.i-no
          NO-ERROR.

     FIND FIRST b-blank-vend-cost WHERE
          b-blank-vend-cost.reftable = "blank-vend-cost" AND
          b-blank-vend-cost.company = e-item.company AND
          b-blank-vend-cost.CODE    = e-item.i-no
          NO-ERROR.
  END.

  for each tmpfile by tmpfile.qty:
      if tmpfile.qty = 0 then next.

      IF i LE 10 THEN
         assign e-item-vend.run-qty[i] = tmpfile.qty
                e-item-vend.run-cost[i] = tmpfile.siz
                e-item-vend.setups[i] = tmpfile.setups.
      ELSE
         ASSIGN
            b-qty.val[i - 10] = tmpfile.qty
            b-cost.val[i - 10] = tmpfile.siz
            b-setup.val[i - 10] = tmpfile.setups.

      IF i GT 10 AND AVAIL b-blank-vend-qty AND AVAIL b-blank-vend-cost THEN
         ASSIGN
            b-blank-vend-qty.val[i - 10] = tmpfile.qty
            b-blank-vend-cost.val[i - 10] = tmpfile.siz.

      i = i + 1.       
  end.

  RELEASE b-blank-vend-qty.
  RELEASE b-blank-vend-cost.

  IF e-item-vend.vend-no EQ "" THEN
     do i = 1 to 10:
        assign e-item.run-qty[i] = e-item-vend.run-qty[i]
               e-item.run-cost[i] = e-item-vend.run-cost[i].
     end.

  IF e-item-vend.vend-no EQ "" THEN
  DO i = 1 TO 30:
     e-item.roll-w[i] = e-item-vend.roll-w[i].
  END.

  FIND CURRENT e-item NO-LOCK.

  ASSIGN
     v-copy-record = NO
     v-old-vend-no = "".

  IF gNewVendor THEN DO:
     CREATE report.
     ASSIGN
       report.term-id = gTerm
       report.key-01  = ""
       report.key-02  = ""
       report.key-03  = e-item-vend.vend-no
       report.key-04  = string(e-item-vend.run-qty[1])
       report.key-05  = ""
       report.key-06  = string(e-item-vend.setups[1])
       report.rec-id  = recid(e-item-vend).
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
  RUN label-display.

  ASSIGN
     v-copy-record = NO
     v-old-vend-no = "".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN disable-manual-fields.

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
  ASSIGN
     v-copy-record = YES
     v-old-vend-no = e-item-vend.vend-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-eiv FOR e-item-vend.
  DEF BUFFER b-ref FOR reftable.

  def var lv-recid as recid no-undo.
  def var char-hdl as cha no-undo.
  def var i as int no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */


   /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
 run get-item-record in widget-handle(char-hdl) (output lv-recid).
 find bf-item where recid(bf-item) = lv-recid
                    no-error.

 assign e-item-vend.company = e-item.company
        e-item-vend.i-no = e-item.i-no    
        e-item-vend.setup = bf-item.min-sqft
        e-item-vend.item-type = YES
        e-item-vend.vend-no = FILL(" ",100) + STRING(TIME,">>>>>>>>>>")
        bf-item.min-sqft = 0.

 do i = 1 to 10:
     assign e-item-vend.run-qty[i] = e-item.run-qty[i]
            e-item-vend.run-cost[i] = e-item.run-cost[i].
 end.
 IF adm-adding-record THEN e-item-vend.vend-no:screen-value in frame {&frame-name} = "".
 display e-item-vend.i-no with frame {&frame-name}.

 IF e-item-vend.rec_key EQ "" THEN
 DO:
    {custom/rec_key.i e-item-vend}
 END.

 IF v-copy-record = NO THEN
 DO WITH FRAME {&FRAME-NAME}:
   ASSIGN
    fi_width-min:SCREEN-VALUE  = ""
    fi_length-min:SCREEN-VALUE = ""
    fi_width-cst:SCREEN-VALUE  = ""
    fi_length-cst:SCREEN-VALUE = "".
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
  def var char-hdl as cha no-undo.
  DEF VAR ll-blank AS LOG NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  v-copy-record = NO.

  /* Code placed here will execute AFTER standard behavior.    */ 
  if not avail e-item then do:
    run get-link-handle in adm-broker-hdl(this-procedure, "record-source", output char-hdl).
    run dispatch in widget-handle(char-hdl) ("open-query").
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  disable e-item.std-uom with frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-eiv FOR e-item-vend.

  DEF VAR li AS INT NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL e-item-vend THEN DO TRANSACTION:
    IF NOT adm-new-record THEN RUN reftable-values (YES).

    FIND b-eiv WHERE ROWID(b-eiv) EQ ROWID(e-item-vend).

    IF e-item-vend.setup NE 0 THEN
      ASSIGN
       b-eiv.setups[1] = e-item-vend.setup
       b-eiv.setup     = 0.

    IF e-item-vend.vend-no EQ "" AND AVAIL e-item THEN
    DO li = 1 TO 30:
      b-eiv.roll-w[li] = e-item.roll-w[li].
    END.

    FIND CURRENT e-item-vend NO-LOCK.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN label-display.

  RUN new-vend.

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
 find bf-item where recid(bf-item) EQ lv-recid no-lock no-error.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */  
  if avail e-item-vend and (e-item-vend.vend-no:screen-value in frame {&frame-name} = '' or 
                            e-item.std-uom:screen-value = '')
  then do:
    enable e-item.std-uom with frame {&frame-name}.
    apply "entry" to e-item.std-uom in frame {&frame-name}.
  end.

  DO WITH FRAME {&FRAME-NAME} :
    IF bf-item.industry EQ "2" AND bf-item.mat-type EQ "B" THEN
       ENABLE fi_width-min fi_length-min fi_width-cst fi_length-cst.

    DISABLE btn_more-breaks.
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
  DEF VAR lv-new-record AS LOG NO-UNDO.
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR v-cost-diff AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  /* ============= validateion ================= */
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-vend-no (e-item-vend.vend-no:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  RUN valid-roll-w-27 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-roll-w-28 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-roll-w-29 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-roll-w-30 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  {&methods/lValidateError.i YES}
  find bf-item of e-item no-lock no-error.
  run sys/ref/uom-rm.p  (bf-item.mat-type, output uom-list).       
  IF E-ITEM.std-uom:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" and
       /* not can-find(uom where uom.uom = self:screen-value)  */
      LOOKUP(e-item.std-uom:SCREEN-VALUE,uom-list) <= 0
  then do:
       message "Invalid UOM. Try Help." view-as alert-box error.
       APPLY "entry" TO e-item.std-uom.
       return no-apply.
  end.   

  if   e-item-vend.vend-no:screen-value <> "" and
       not can-find(first vend where vend.company = e-item.company and
                                     vend.vend-no = e-item-vend.vend-no:screen-value) 
  then do:
        message "Invalid Vendor. Try help." view-as alert-box error.
        apply "entry" to e-item-vend.vend-no.
        return no-apply.
  end.
  {&methods/lValidateError.i NO}
  lv-new-record = adm-new-record.
  /* ============= end of validation ================*/

  RUN upd-cost-date-proc(OUTPUT v-cost-diff).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN disable-manual-fields.

  IF v-cost-diff THEN
     RUN upd-cost-date2-proc.

  IF lv-new-record THEN DO:
     run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
     RUN repos-query IN WIDGET-HANDLE(char-hdl) (ROWID(e-item)).
  END.

  ELSE RUN dispatch ("display-fields").

  ASSIGN gTerm = ""
         gNewVendor = NO.


END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-vend V-table-Win 
PROCEDURE new-vend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST vend NO-LOCK
        WHERE vend.company EQ gcompany
          AND vend.vend-no EQ e-item-vend.vend-no:SCREEN-VALUE
        NO-ERROR.
    ls-vend-name:SCREEN-VALUE = IF AVAIL vend THEN vend.name ELSE "".
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
   def buffer bf-e-item-vend for e-item-vend.


   v-pct = 0.
   message "By what percentage:" update v-pct .

   status default "Processing Raw Material: " + string(e-item.i-no).

   find bf-e-item-vend where recid(bf-e-item-vend) = recid(e-item-vend).

   do i = 1 to 10:
      bf-e-item-vend.run-cost[i] = e-item-vend.run-cost[i] + 
                                (e-item-vend.run-cost[i] * v-pct / 100).
   end.

   run get-link-handle in adm-broker-hdl (this-procedure, "record-source", output char-hdl).
   run dispatch in widget-handle(char-hdl) ('open-query').

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


  IF AVAIL e-item-vend THEN DO:
    FIND FIRST reftable {&where-adders} NO-ERROR.
    IF NOT AVAIL reftable THEN DO:
      CREATE reftable.
      ASSIGN
       reftable.rec_key  = e-item-vend.rec_key
       reftable.reftable = "e-item-vend.adders"
       reftable.company  = e-item-vend.company.
    END.
    IF ip-display THEN
      ASSIGN
       fi_width-min  = reftable.val[1] / 10000
       fi_length-min = reftable.val[2] / 10000
       fi_width-cst  = reftable.val[3] / 10000
       fi_length-cst = reftable.val[4] / 10000.
    ELSE
      ASSIGN
       reftable.val[1] = fi_width-min  * 10000
       reftable.val[2] = fi_length-min * 10000
       reftable.val[3] = fi_width-cst  * 10000
       reftable.val[4] = fi_length-cst * 10000.

    FIND CURRENT reftable NO-LOCK.
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
  {src/adm/template/snd-list.i "e-item-vend"}
  {src/adm/template/snd-list.i "e-item"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE upd-cost-date-proc V-table-Win 
PROCEDURE upd-cost-date-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-cost-diff AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

  IF adm-new-record OR

     dec(e-item-vend.run-qty[1]:SCREEN-VALUE) NE e-item-vend.run-qty[1] OR
     dec(e-item-vend.run-cost[1]:SCREEN-VALUE) NE e-item-vend.run-cost[1] OR
     dec(e-item-vend.setups[1]:SCREEN-VALUE) NE e-item-vend.setups[1] OR

     dec(e-item-vend.run-qty[2]:SCREEN-VALUE) NE e-item-vend.run-qty[2] OR
     dec(e-item-vend.run-cost[2]:SCREEN-VALUE) NE e-item-vend.run-cost[2] OR
     dec(e-item-vend.setups[2]:SCREEN-VALUE) NE e-item-vend.setups[2] OR

     dec(e-item-vend.run-qty[3]:SCREEN-VALUE) NE e-item-vend.run-qty[3] OR
     dec(e-item-vend.run-cost[3]:SCREEN-VALUE) NE e-item-vend.run-cost[3] OR
     dec(e-item-vend.setups[3]:SCREEN-VALUE) NE e-item-vend.setups[3] OR

     dec(e-item-vend.run-qty[4]:SCREEN-VALUE) NE e-item-vend.run-qty[4] OR
     dec(e-item-vend.run-cost[4]:SCREEN-VALUE) NE e-item-vend.run-cost[4] OR
     dec(e-item-vend.setups[4]:SCREEN-VALUE) NE e-item-vend.setups[4] OR

     dec(e-item-vend.run-qty[5]:SCREEN-VALUE) NE e-item-vend.run-qty[5] OR
     dec(e-item-vend.run-cost[5]:SCREEN-VALUE) NE e-item-vend.run-cost[5] OR
     dec(e-item-vend.setups[5]:SCREEN-VALUE) NE e-item-vend.setups[5] OR

     dec(e-item-vend.run-qty[6]:SCREEN-VALUE) NE e-item-vend.run-qty[6] OR
     dec(e-item-vend.run-cost[6]:SCREEN-VALUE) NE e-item-vend.run-cost[6] OR
     dec(e-item-vend.setups[6]:SCREEN-VALUE) NE e-item-vend.setups[6] OR

     dec(e-item-vend.run-qty[7]:SCREEN-VALUE) NE e-item-vend.run-qty[7] OR
     dec(e-item-vend.run-cost[7]:SCREEN-VALUE) NE e-item-vend.run-cost[7] OR
     dec(e-item-vend.setups[7]:SCREEN-VALUE) NE e-item-vend.setups[7] OR

     dec(e-item-vend.run-qty[8]:SCREEN-VALUE) NE e-item-vend.run-qty[8] OR
     dec(e-item-vend.run-cost[8]:SCREEN-VALUE) NE e-item-vend.run-cost[8] OR
     dec(e-item-vend.setups[8]:SCREEN-VALUE) NE e-item-vend.setups[8] OR

     dec(e-item-vend.run-qty[9]:SCREEN-VALUE) NE e-item-vend.run-qty[9] OR
     dec(e-item-vend.run-cost[9]:SCREEN-VALUE) NE e-item-vend.run-cost[9] OR
     dec(e-item-vend.setups[9]:SCREEN-VALUE) NE e-item-vend.setups[9] OR

     dec(e-item-vend.run-qty[10]:SCREEN-VALUE) NE e-item-vend.run-qty[10] OR
     dec(e-item-vend.run-cost[10]:SCREEN-VALUE) NE e-item-vend.run-cost[10] OR
     dec(e-item-vend.setups[10]:SCREEN-VALUE) NE e-item-vend.setups[10] THEN
     DO:
        op-cost-diff = YES.
        LEAVE.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE upd-cost-date2-proc V-table-Win 
PROCEDURE upd-cost-date2-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER b-e-item-vend FOR e-item-vend.

   FIND b-e-item-vend WHERE
        ROWID(b-e-item-vend) EQ ROWID(e-item-vend)
        EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

   IF AVAIL b-e-item-vend THEN
   DO:
      ASSIGN
         b-e-item-vend.updated-id[1] = USERID("NOSWEAT")
         b-e-item-vend.updated-date[1] = TODAY.

      FIND CURRENT b-e-item-vend NO-LOCK.
      RELEASE b-e-item-vend.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-roll-w-27 V-table-Win 
PROCEDURE valid-roll-w-27 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(e-item-vend.roll-w[27]:SCREEN-VALUE) GT DEC(e-item-vend.roll-w[28]:SCREEN-VALUE) THEN DO:
      MESSAGE TRIM(e-item-vend.roll-w[27]:LABEL) " may not be greater than " TRIM(e-item-vend.roll-w[28]:LABEL).
      APPLY "entry" TO e-item-vend.roll-w[27].
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-roll-w-28 V-table-Win 
PROCEDURE valid-roll-w-28 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(e-item-vend.roll-w[28]:SCREEN-VALUE) LT DEC(e-item-vend.roll-w[27]:SCREEN-VALUE) THEN DO:
      MESSAGE TRIM(e-item-vend.roll-w[28]:LABEL) " may not be less than " TRIM(e-item-vend.roll-w[27]:LABEL).
      APPLY "entry" TO e-item-vend.roll-w[28].
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-roll-w-29 V-table-Win 
PROCEDURE valid-roll-w-29 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(e-item-vend.roll-w[29]:SCREEN-VALUE) GT DEC(e-item-vend.roll-w[30]:SCREEN-VALUE) THEN DO:
      MESSAGE TRIM(e-item-vend.roll-w[29]:LABEL) " may not be greater than " TRIM(e-item-vend.roll-w[30]:LABEL).
      APPLY "entry" TO e-item-vend.roll-w[29].
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-roll-w-30 V-table-Win 
PROCEDURE valid-roll-w-30 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(e-item-vend.roll-w[30]:SCREEN-VALUE) LT DEC(e-item-vend.roll-w[29]:SCREEN-VALUE) THEN DO:
      MESSAGE TRIM(e-item-vend.roll-w[30]:LABEL) " may not be less than " TRIM(e-item-vend.roll-w[29]:LABEL).
      APPLY "entry" TO e-item-vend.roll-w[30].
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

  DEF BUFFER bf-evend FOR e-item-vend.

  DEF VAR lv-msg AS CHAR NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF ip-focus:SCREEN-VALUE EQ "" THEN
      IF e-item-vend.vend-no NE "" AND NOT adm-new-record THEN
        lv-msg = "Sorry, you cannot change to 'Blank Vendor', please use copy button".
      ELSE.

    ELSE
      IF e-item-vend.vend-no EQ "" AND NOT adm-new-record THEN
        lv-msg = "Sorry, you cannot change the 'Blank Vendor', please use copy button".

      ELSE
      IF NOT CAN-FIND(FIRST vend
                      WHERE vend.company EQ e-item.company
                        AND vend.vend-no EQ ip-focus:SCREEN-VALUE) THEN
        lv-msg = TRIM(ip-focus:LABEL) + " is invalid, try help".

      ELSE
      IF CAN-FIND(FIRST bf-evend NO-LOCK
                  WHERE bf-evend.company   EQ e-item.company
                    AND bf-evend.i-no      EQ e-item.i-no
                    AND bf-evend.item-type EQ YES
                    AND bf-evend.vend-no   EQ ip-focus:SCREEN-VALUE
                    AND (ROWID(bf-evend)   NE ROWID(e-item-vend) OR
                         (adm-new-record AND NOT adm-adding-record))) THEN
        lv-msg = TRIM(ip-focus:LABEL) + " already exists...".

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

