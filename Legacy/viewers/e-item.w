&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
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
{system/fSuperRunning.i}

def var uom-list as cha init ["M,EA,L,CS,C"] no-undo.
DEF VAR char-hdl AS CHAR NO-UNDO.

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
e-item-vend.updated-date[1] e-item-vend.updated-id[1] ~
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
e-item-vend.underWidth e-item-vend.underWidthCost e-item-vend.underLength ~
e-item-vend.underLengthCost e-item-vend.roll-w[1] e-item-vend.roll-w[2] ~
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
e-item-vend.updated-id[1] e-item-vend.run-qty[1] e-item-vend.run-cost[1] ~
e-item-vend.setups[1] e-item-vend.run-qty[2] e-item-vend.run-cost[2] ~
e-item-vend.setups[2] e-item-vend.run-qty[3] e-item-vend.run-cost[3] ~
e-item-vend.setups[3] e-item-vend.run-qty[4] e-item-vend.run-cost[4] ~
e-item-vend.setups[4] e-item-vend.run-qty[5] e-item-vend.run-cost[5] ~
e-item-vend.setups[5] e-item-vend.run-qty[6] e-item-vend.run-cost[6] ~
e-item-vend.setups[6] e-item-vend.run-qty[7] e-item-vend.run-cost[7] ~
e-item-vend.setups[7] e-item-vend.run-qty[8] e-item-vend.run-cost[8] ~
e-item-vend.setups[8] e-item-vend.run-qty[9] e-item-vend.run-cost[9] ~
e-item-vend.setups[9] e-item-vend.run-qty[10] e-item-vend.run-cost[10] ~
e-item-vend.setups[10] e-item-vend.underWidth e-item-vend.underWidthCost ~
e-item-vend.underLength e-item-vend.underLengthCost e-item-vend.roll-w[1] ~
e-item-vend.roll-w[2] e-item-vend.roll-w[3] e-item-vend.roll-w[4] ~
e-item-vend.roll-w[5] e-item-vend.roll-w[6] e-item-vend.roll-w[7] ~
e-item-vend.roll-w[8] e-item-vend.roll-w[9] e-item-vend.roll-w[10] ~
e-item-vend.roll-w[11] e-item-vend.roll-w[12] e-item-vend.roll-w[13] ~
e-item-vend.roll-w[14] e-item-vend.roll-w[15] e-item-vend.roll-w[16] ~
e-item-vend.roll-w[17] e-item-vend.roll-w[18] e-item-vend.roll-w[19] ~
e-item-vend.roll-w[20] e-item-vend.roll-w[21] e-item-vend.roll-w[22] ~
e-item-vend.roll-w[23] e-item-vend.roll-w[24] e-item-vend.roll-w[25] ~
e-item-vend.roll-w[26] e-item-vend.roll-w[27] e-item-vend.roll-w[28] ~
e-item-vend.roll-w[29] e-item-vend.roll-w[30] 
&Scoped-define DISPLAYED-TABLES e-item-vend e-item
&Scoped-define FIRST-DISPLAYED-TABLE e-item-vend
&Scoped-define SECOND-DISPLAYED-TABLE e-item
&Scoped-Define DISPLAYED-OBJECTS ls-item-name ls-vend-name lbl_setup ~
lbl_roll-w 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS e-item.std-uom 
&Scoped-define ADM-ASSIGN-FIELDS e-item.std-uom 
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
&Scoped-define List-6 btn_more-breaks 

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

DEFINE VARIABLE lbl_roll-w AS CHARACTER FORMAT "X(256)":U INITIAL "Valid  Estimated  Roll  Width" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_setup AS CHARACTER FORMAT "X(256)":U INITIAL "Setup $" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .76 NO-UNDO.

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
     e-item-vend.i-no AT ROW 1 COL 15 COLON-ALIGNED FORMAT "x(10)"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ls-item-name AT ROW 1 COL 31 COLON-ALIGNED NO-LABEL
     e-item.std-uom AT ROW 2.1 COL 31 COLON-ALIGNED
          LABEL "Purchased Cost UOM" FORMAT "x(4)"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     e-item-vend.vend-item AT ROW 2.1 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     e-item-vend.vend-no AT ROW 3.19 COL 15 COLON-ALIGNED FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ls-vend-name AT ROW 3.19 COL 31.2 COLON-ALIGNED NO-LABEL
     e-item-vend.updated-date[1] AT ROW 4.19 COL 15.2 COLON-ALIGNED HELP
          "Enter the cost update date." WIDGET-ID 2
          LABEL "Updated"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     e-item-vend.updated-id[1] AT ROW 4.19 COL 33.2 COLON-ALIGNED WIDGET-ID 12
          LABEL "by"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     lbl_setup AT ROW 5.29 COL 34 COLON-ALIGNED NO-LABEL
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
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     e-item-vend.run-cost[7] AT ROW 11.71 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
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
     btn_more-breaks AT ROW 15.48 COL 2.4 WIDGET-ID 4
     e-item-vend.underWidth AT ROW 16.48 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.underWidthCost AT ROW 16.48 COL 36 COLON-ALIGNED
          LABEL "$"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     e-item-vend.underLength AT ROW 17.43 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     e-item-vend.underLengthCost AT ROW 17.43 COL 36 COLON-ALIGNED
          LABEL "$"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     lbl_roll-w AT ROW 5.05 COL 53 COLON-ALIGNED NO-LABEL
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
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
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
          SIZE 15 BY .67 AT ROW 5.29 COL 19
     "Min" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 15.81 COL 68.8
     "Max" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 15.81 COL 80.8
     "Vendor Item No:" VIEW-AS TEXT
          SIZE 28 BY .62 AT ROW 1.24 COL 63.2 WIDGET-ID 10
     "QTY to" VIEW-AS TEXT
          SIZE 15 BY .81 AT ROW 5.14 COL 2
     RECT-24 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
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
/* SETTINGS FOR FILL-IN e-item-vend.underLengthCost IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN e-item-vend.underWidthCost IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN e-item-vend.updated-date[1] IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN e-item-vend.updated-id[1] IN FRAME F-Main
   EXP-LABEL                                                            */
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
   RUN rm/d-rmbreaks.w(INPUT ROWID(e-item-vend), INPUT ROWID(e-item)).
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
     e-item-vend.underWidth:HIDDEN  = YES
     e-item-vend.underLength:HIDDEN  = YES
     e-item-vend.underWidthCost:HIDDEN  = YES
     e-item-vend.underLengthCost:HIDDEN  = YES.

    IF AVAIL bf-item AND bf-item.industry EQ "2" AND bf-item.mat-type EQ "B" THEN
      ASSIGN
     e-item-vend.underWidth:HIDDEN  = NO 
     e-item-vend.underLength:HIDDEN  = NO 
     e-item-vend.underWidthCost:HIDDEN  = NO 
     e-item-vend.underLengthCost:HIDDEN  = NO.
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
    DEF BUFFER b-e-item FOR e-item. /* Just to make sure this is the ONLY procedure that accesses this record */

/*    DISABLE TRIGGERS FOR LOAD OF bf-evend.  In order to fire write trigger for ticket 38640 */
    DISABLE TRIGGERS FOR LOAD OF b-e-item.  /* and we don't need triggers; we're controlling fields */

    DEF VAR i AS INT NO-UNDO.
    DEF VAR lv-recid AS RECID NO-UNDO.
    DEF VAR v-count AS INT NO-UNDO.

    lv-recid = RECID(e-item).
    FIND FIRST e-item WHERE RECID(e-item) = lv-recid .

    /* This ensures there's ONLY ONE blank vendor code (used in master e-item record) */
    IF TRIM(e-item-vend.vend-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}) EQ "" THEN
    FOR EACH bf-evend
        WHERE bf-evend.company EQ e-item.company
        AND bf-evend.i-no      EQ e-item.i-no
        AND bf-evend.item-type EQ YES
        AND bf-evend.vend-no   EQ ""
        AND ROWID(bf-evend)    NE ROWID(e-item-vend):
        DELETE bf-evend.
    END.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .


    /* This sorts the roll widths in descending order */
    EMPTY TEMP-TABLE tmpfile.
    DO i = 1 TO 26:
        IF e-item-vend.roll-w[i] <> 0 THEN DO:
            CREATE tmpfile.
            ASSIGN 
                tmpfile.siz = e-item-vend.roll-w[i]
                e-item-vend.roll-w[i] = 0.
        END.
    END.
    ASSIGN 
        i = 1.
    FOR EACH tmpfile BY tmpfile.siz :
        ASSIGN 
            e-item-vend.roll-w[i] = tmpfile.siz
            i = i + 1.
    END.
  
    /* If this is a copy, get the non-displayed fields from the source record */
    IF v-copy-record THEN DO:
        FIND FIRST bf-evend
        WHERE bf-evend.company   EQ e-item.company
        AND bf-evend.i-no      EQ e-item.i-no
        AND bf-evend.vend-no   EQ v-old-vend-no
        AND ROWID(bf-evend)    NE ROWID(e-item-vend)
        NO-ERROR.
        IF AVAIL bf-evend THEN DO: 
            DO v-count = 1 TO 10:
                e-item-vend.runQtyXtra[v-count] = bf-evend.runQtyXtra[v-count].
                e-item-vend.runCostXtra[v-count] = bf-evend.runCostXtra[v-count].
                e-item-vend.setupsXtra[v-count] = bf-evend.setupsXtra[v-count].
            END.
        END.
    END.

    /* If this is the "blank" vendor code, copy the qty/cost values to the "master" (e-item) record */
    IF e-item-vend.vend-no EQ "" THEN  DO:    
        FIND b-e-item EXCLUSIVE WHERE 
            ROWID(b-e-item) EQ ROWID(e-item)
            NO-ERROR.
        IF NOT AVAIL b-e-item THEN DO:
            MESSAGE 
                "b-e-item not avail"
                VIEW-AS ALERT-BOX.
            RETURN.
        END.
/*           Just a word of explanation for this:                                              */
/*           e-item and e-item-vend SHOULD have single fields for these elements with extent 20*/
/*           They don't                                                                        */
/*           run-qty and run-cost (the original fields) are still extent 10                    */
/*           e-item has runqty and runcost (new fields with no hyphen) with extent 20          */
/*           e-item-vend has runQtyXtra and runCostXtra with extent 10                         */
/*           This code is designed to populate all of these properly                           */
/*           I don't want to make a db change to correct this since we're doing a release      */
/*           this afternoon and I'm sure there would be unintended consequences - MYT          */
        DO v-count = 1 TO 10:
            b-e-item.run-qty[v-count] = e-item-vend.run-qty[v-count].
            b-e-item.runqty[v-count] = e-item-vend.run-qty[v-count].
            b-e-item.runqty[v-count + 10] = e-item-vend.runQtyXtra[v-count].
            b-e-item.run-cost[v-count] = e-item-vend.run-cost[v-count].
            b-e-item.runcost[v-count] = e-item-vend.run-cost[v-count].
            b-e-item.runcost[v-count + 10] = e-item-vend.runCostXtra[v-count].
        END.
        FIND b-e-item NO-LOCK WHERE 
            ROWID(b-e-item) EQ ROWID(e-item)
            NO-ERROR.
    END.
  
    EMPTY TEMP-TABLE tmpfile.
    IF AVAIL e-item-vend THEN ASSIGN 
        v-count = 20.
    ELSE ASSIGN 
        v-count = 10.

    /* Build a temp file for sorting */
    DO i = 1 TO v-count:
        CREATE tmpfile.

        IF i LE 10 THEN ASSIGN 
            tmpfile.qty = e-item-vend.run-qty[i]
            tmpfile.siz = e-item-vend.run-cost[i]
            tmpfile.setups = e-item-vend.setups[i]
            e-item-vend.run-qty[i] = 0
            e-item-vend.run-cost[i] = 0
            e-item-vend.setups[i] = 0.
        ELSE ASSIGN 
            tmpfile.qty = e-item-vend.runQtyXtra[i - 10]
            tmpfile.siz = e-item-vend.runCostXtra[i - 10]
            tmpfile.setups = e-item-vend.setupsXtra[i - 10]
            e-item-vend.runQtyXtra[i - 10] = 0
            e-item-vend.runCostXtra[i - 10] = 0
            e-item-vend.setupsXtra[i - 10] = 0.
    END.
  
    ASSIGN 
        i = 1.

    /* If the "master", get the e-item (using the buffer) for updating */
    IF e-item-vend.vend-no EQ "" THEN
    DO:
        FIND b-e-item EXCLUSIVE WHERE 
            ROWID(b-e-item) EQ ROWID(e-item)
            NO-ERROR.
        IF NOT AVAIL b-e-item THEN DO:
            MESSAGE 
                "b-e-item not avail"
                VIEW-AS ALERT-BOX.
            RETURN.
        END.
    END.

    FOR EACH tmpfile BY tmpfile.qty:
        /* If all done, just skip */
        IF tmpfile.qty = 0 THEN NEXT.
        /* See the comment above about field names/extents */
        IF i LE 10 THEN DO:
            ASSIGN 
                e-item-vend.run-qty[i] = tmpfile.qty
                e-item-vend.run-cost[i] = tmpfile.siz
                e-item-vend.setups[i] = tmpfile.setups.
            IF e-item-vend.vend-no EQ "" THEN ASSIGN 
                b-e-item.run-qty[i] = e-item-vend.run-qty[i]
                b-e-item.runqty[i] = e-item-vend.run-qty[i]
                b-e-item.run-cost[i] = e-item-vend.run-cost[i]
                b-e-item.runcost[i] = e-item-vend.run-cost[i].
        END.
        ELSE DO:
            ASSIGN
                e-item-vend.runQtyXtra[i - 10] = tmpfile.qty
                e-item-vend.runCostXtra[i - 10] = tmpfile.siz
                e-item-vend.setupsXtra[i - 10] = tmpfile.setups.
            IF e-item-vend.vend-no EQ "" THEN ASSIGN 
                b-e-item.runqty[i + 10] = e-item-vend.runQtyXtra[i]
                b-e-item.runcost[i + 10] = e-item-vend.runCostXtra[i].
        END.
        i = i + 1.       
    END.

    /* If we found the master buffer exclusive, let's release it */
    IF e-item-vend.vend-no EQ "" THEN DO:
        DO i = 1 TO 30:
            e-item.roll-w[i] = e-item-vend.roll-w[i].
        END.
        FIND b-e-item NO-LOCK WHERE 
            ROWID(b-e-item) EQ ROWID(e-item)
            NO-ERROR.
    END.

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
            report.key-04  = STRING(e-item-vend.run-qty[1])
            report.key-05  = ""
            report.key-06  = STRING(e-item-vend.setups[1])
            report.rec-id  = RECID(e-item-vend).
        RELEASE report.
    END.

    /* Now redisplay what we changed */
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
        e-item-vend.roll-w[28] = 999.000
        e-item-vend.roll-w[30] = 999.000
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

  /* Code placed here will execute PRIOR to standard behavior. */

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
        IF bf-item.industry EQ "2" AND bf-item.mat-type EQ "B" THEN ASSIGN 
            e-item-vend.underWidth:SENSITIVE  = YES
            e-item-vend.underLength:SENSITIVE  = YES
            e-item-vend.underWidthCost:SENSITIVE  = YES
            e-item-vend.underLengthCost:SENSITIVE  = YES.

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
  
  DO WITH FRAME {&FRAME-NAME}:

  IF e-item-vend.vend-item:SCREEN-VALUE EQ "?" THEN
      ASSIGN e-item-vend.vend-item:SCREEN-VALUE = "" .

  IF e-item-vend.roll-w[28]:SCREEN-VALUE EQ "0.0000" THEN
      ASSIGN 
      e-item-vend.roll-w[28]:SCREEN-VALUE = "999.000" .

  IF e-item-vend.roll-w[30]:screen-value EQ "0.0000" THEN
      ASSIGN 
      e-item-vend.roll-w[30]:SCREEN-VALUE = "999.000" .

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

