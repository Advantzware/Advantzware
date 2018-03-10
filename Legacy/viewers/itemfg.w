&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/itemfg.w

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

{sys/inc/var.i NEW SHARED}

DEF VAR old-est-no      LIKE itemfg.est-no NO-UNDO.
DEF VAR old-part-no#    LIKE itemfg.part-no NO-UNDO.
DEF VAR old-i-name      LIKE itemfg.i-name NO-UNDO.
DEF VAR old-part-dscr1  LIKE itemfg.part-dscr1 NO-UNDO.
DEF VAR old-part-dscr2  LIKE itemfg.part-dscr2 NO-UNDO.
DEF VAR old-part-dscr3  LIKE itemfg.part-dscr3 NO-UNDO.
DEF VAR old-die-no      LIKE itemfg.die-no NO-UNDO.
DEF VAR old-plate-no    LIKE itemfg.plate-no NO-UNDO.
DEF VAR old-cad-no      LIKE itemfg.cad-no NO-UNDO.
DEF VAR old-spc-no      LIKE itemfg.spc-no NO-UNDO.
DEF VAR old-upc-no      LIKE itemfg.upc-no NO-UNDO.
DEF VAR old-procat      LIKE itemfg.procat NO-UNDO.
DEF VAR uom-list AS cha INIT "C,CS,EA,L,M,LB,DRM,ROL,PKG,SET,DOZ,BDL" NO-UNDO.
DEF VAR lv-type-codes AS CHAR NO-UNDO.
DEF VAR lv-type-dscrs AS CHAR NO-UNDO.
DEF VAR v-mat AS LOG INIT YES NO-UNDO.
DEF VAR cDefaultProdUom AS CHAR NO-UNDO.

DEF VAR v-cpyspc AS LOG NO-UNDO.
DEF VAR v-begspc AS CHAR NO-UNDO.
DEF VAR v-endspc AS CHAR NO-UNDO .

DEF VAR lv-puruom       LIKE itemfg.pur-uom NO-UNDO.

/* gdm - 11190901 */
DEF VAR v-shpmet        LIKE itemfg.ship-meth NO-UNDO.
DEFINE VARIABLE lCheckPurMan AS LOGICAL NO-UNDO .

&scoped-define copy-proc proc-copy
&SCOPED-DEFINE itemfg-maint itemfg-maint





DEF TEMP-TABLE w-est-no
    FIELD w-est-no LIKE itemfg.est-no
    FIELD w-run AS LOG.

RUN sys/ref/ordtypes.p (OUTPUT lv-type-codes, OUTPUT lv-type-dscrs).

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
&Scoped-define EXTERNAL-TABLES itemfg
&Scoped-define FIRST-EXTERNAL-TABLE itemfg


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR itemfg.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS itemfg.spare-int-2 itemfg.setupDate itemfg.i-no itemfg.isaset ~
itemfg.part-no itemfg.i-name itemfg.part-dscr1 itemfg.part-dscr2 ~
itemfg.part-dscr3 itemfg.spare-char-1 itemfg.est-no itemfg.style ~
itemfg.style-desc itemfg.die-no itemfg.plate-no itemfg.cad-no itemfg.spc-no ~
itemfg.upc-no itemfg.cust-no itemfg.cust-name itemfg.stat itemfg.pur-man ~
itemfg.ship-meth itemfg.i-code itemfg.sell-price itemfg.sell-uom ~
itemfg.curr-code[1] itemfg.procat itemfg.procat-desc itemfg.type-code ~
itemfg.def-loc itemfg.def-loc-bin itemfg.case-count itemfg.case-pall ~
itemfg.weight-100 itemfg.frt-class itemfg.frt-class-dscr itemfg.class ~
itemfg.cc-code itemfg.prod-code itemfg.prod-notes itemfg.std-mat-cost ~
itemfg.std-lab-cost itemfg.std-var-cost itemfg.std-fix-cost ~
itemfg.spare-dec-1 itemfg.total-std-cost itemfg.avg-cost itemfg.last-cost ~
itemfg.prod-uom 
&Scoped-define ENABLED-TABLES itemfg
&Scoped-define FIRST-ENABLED-TABLE itemfg
&Scoped-Define ENABLED-OBJECTS tg-Freeze-weight RECT-10 RECT-8 RECT-9 ~
RECT-11 RECT-12 
&Scoped-Define DISPLAYED-FIELDS itemfg.spare-int-2 itemfg.setupDate itemfg.i-no ~
itemfg.isaset itemfg.part-no itemfg.i-name itemfg.part-dscr1 ~
itemfg.part-dscr2 itemfg.part-dscr3 itemfg.spare-char-1 itemfg.exempt-disc ~
itemfg.est-no itemfg.style itemfg.style-desc itemfg.die-no itemfg.plate-no ~
itemfg.cad-no itemfg.spc-no itemfg.upc-no itemfg.cust-no itemfg.cust-name ~
itemfg.stat itemfg.pur-man itemfg.ship-meth itemfg.i-code itemfg.sell-price ~
itemfg.sell-uom itemfg.curr-code[1] itemfg.procat itemfg.procat-desc ~
itemfg.type-code itemfg.def-loc itemfg.def-loc-bin itemfg.case-count ~
itemfg.case-pall itemfg.weight-100 itemfg.frt-class itemfg.frt-class-dscr ~
itemfg.class itemfg.cc-code itemfg.prod-code itemfg.prod-notes ~
itemfg.std-mat-cost itemfg.std-lab-cost itemfg.std-var-cost ~
itemfg.std-fix-cost itemfg.spare-dec-1 itemfg.total-std-cost ~
itemfg.avg-cost itemfg.last-cost itemfg.prod-uom 
&Scoped-define DISPLAYED-TABLES itemfg
&Scoped-define FIRST-DISPLAYED-TABLE itemfg
&Scoped-Define DISPLAYED-OBJECTS tb_taxable tgVaried tg-Freeze-weight ~
fi_type-dscr 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS itemfg.exempt-disc tb_taxable itemfg.stat 
&Scoped-define DISPLAY-FIELD itemfg.exempt-disc tb_taxable itemfg.stat 

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
DEFINE VARIABLE fi_type-dscr AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76.6 BY 5.24.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 8.33.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76.6 BY 4.1.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 8.62.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76.6 BY 7.62.

DEFINE VARIABLE tb_taxable AS LOGICAL INITIAL no 
     LABEL "Taxable?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tg-Freeze-weight AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE tgVaried AS LOGICAL INITIAL no 
     LABEL "Varied" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     itemfg.spare-int-2 AT ROW 16.91 COL 11 COLON-ALIGNED HELP
          "" WIDGET-ID 16
          LABEL "Rel Seq" FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
    itemfg.setupDate AT ROW 16.91 COL 47 COLON-ALIGNED
          LABEL "Setup Date" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     itemfg.i-no AT ROW 1.48 COL 15.4 COLON-ALIGNED
          LABEL "FG Item #"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     itemfg.isaset AT ROW 1.38 COL 47
          LABEL "Set Header?"
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY 1.19
     itemfg.part-no AT ROW 2.43 COL 15.4 COLON-ALIGNED
          LABEL "Cust Part #" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     itemfg.i-name AT ROW 3.38 COL 15.4 COLON-ALIGNED
          LABEL " Name"
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
     itemfg.part-dscr1 AT ROW 4.33 COL 15.4 COLON-ALIGNED
          LABEL "Desc 1"
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
     itemfg.part-dscr2 AT ROW 5.29 COL 15.4 COLON-ALIGNED
          LABEL "Desc 2"
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
     itemfg.part-dscr3 AT ROW 6.24 COL 15.4 COLON-ALIGNED
          LABEL "Desc 3"
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
     itemfg.spare-char-1 AT ROW 7.33 COL 15.4 COLON-ALIGNED WIDGET-ID 10
          LABEL "Group" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     itemfg.exempt-disc AT ROW 8.38 COL 17.4
          LABEL "Exempt From Discount?"
          VIEW-AS TOGGLE-BOX
          SIZE 43 BY 1
     itemfg.est-no AT ROW 10 COL 11 COLON-ALIGNED
          LABEL "Est#" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     itemfg.style AT ROW 11 COL 11 COLON-ALIGNED
          LABEL "Style"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     itemfg.style-desc AT ROW 11 COL 27.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     itemfg.die-no AT ROW 12.1 COL 11 COLON-ALIGNED FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
     itemfg.plate-no AT ROW 13.05 COL 11 COLON-ALIGNED FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
     itemfg.cad-no AT ROW 14 COL 11 COLON-ALIGNED
          LABEL "CAD#" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 30 BY 1.05
     itemfg.spc-no AT ROW 14.95 COL 11 COLON-ALIGNED
          LABEL "QC #" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
     itemfg.upc-no AT ROW 15.91 COL 11 COLON-ALIGNED
          LABEL "UPC #" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
     itemfg.cust-no AT ROW 1.48 COL 76 COLON-ALIGNED
          LABEL "Cust#" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     itemfg.cust-name AT ROW 1.48 COL 91.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 35.8 BY 1
     tb_taxable AT ROW 1.29 COL 129.6
     tgVaried AT ROW 2.14 COL 129.6 WIDGET-ID 12
     itemfg.stat AT ROW 3.05 COL 78.8 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Active", "A":U,
"InActive", "I":U
          SIZE 27 BY .95
     itemfg.pur-man AT ROW 3.05 COL 106.2 HELP
          "" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Purchased", yes,
"Manufactured", no
          SIZE 38.2 BY .95
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     itemfg.ship-meth AT ROW 4 COL 85.4 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Case", yes,
"Pallet", no
          SIZE 22 BY .95
     itemfg.i-code AT ROW 4 COL 108.2 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Stock Item", "S":U,
"Custom Box", "C":U
          SIZE 35 BY .95
     itemfg.sell-price AT ROW 5.57 COL 81.6 COLON-ALIGNED
          LABEL "Sell Price" FORMAT ">,>>>,>>9.99<<"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     itemfg.sell-uom AT ROW 5.57 COL 109.2 COLON-ALIGNED
          LABEL "UOM" FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     itemfg.curr-code[1] AT ROW 5.57 COL 132 COLON-ALIGNED
          LABEL "Currency"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     itemfg.procat AT ROW 6.57 COL 81.6 COLON-ALIGNED
          LABEL "Category" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 12.4 BY 1
     itemfg.procat-desc AT ROW 6.57 COL 94 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 23 BY .95
     itemfg.type-code AT ROW 6.57 COL 138 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     itemfg.def-loc AT ROW 7.57 COL 81.6 COLON-ALIGNED
          LABEL "Warehse"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     itemfg.def-loc-bin AT ROW 8.57 COL 81.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     itemfg.case-count AT ROW 9.52 COL 81.6 COLON-ALIGNED HELP
          "Enter Qty per Case, Bundle or Pallet"
          LABEL "Count" FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     itemfg.case-pall AT ROW 9.52 COL 104.2 COLON-ALIGNED HELP
          ""
          LABEL "Unit/Pall" FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     itemfg.weight-100 AT ROW 10.52 COL 81.6 COLON-ALIGNED
          LABEL "Lbs/ 100" FORMAT ">>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     tg-Freeze-weight AT ROW 10.52 COL 100 WIDGET-ID 14
     itemfg.frt-class AT ROW 11.57 COL 85 COLON-ALIGNED HELP
          "Enter A,B,C"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     itemfg.frt-class-dscr AT ROW 11.57 COL 97 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     itemfg.class AT ROW 7.57 COL 128.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.8 BY .95
     itemfg.cc-code AT ROW 8.52 COL 128.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.8 BY .95
     itemfg.prod-code AT ROW 9.48 COL 128.4 COLON-ALIGNED HELP
          ""
          LABEL "Prod Code" FORMAT "X(6)"
          VIEW-AS FILL-IN 
          SIZE 13.8 BY 1
     itemfg.prod-notes AT ROW 10.52 COL 114 COLON-ALIGNED
          LABEL "Pk Note"
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     itemfg.std-mat-cost AT ROW 13.1 COL 88.6 COLON-ALIGNED
          LABEL "Std Mat'l Cost" FORMAT "->>>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     itemfg.std-lab-cost AT ROW 14.05 COL 88.6 COLON-ALIGNED
          LABEL "Std Labor Cost" FORMAT "->>>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     itemfg.std-var-cost AT ROW 15 COL 88.6 COLON-ALIGNED
          LABEL "Std Var OH Cost" FORMAT "->>>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     itemfg.std-fix-cost AT ROW 15.95 COL 88.6 COLON-ALIGNED
          LABEL "Std Fix OH Cost" FORMAT "->>>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     itemfg.spare-dec-1 AT ROW 16.91 COL 88.6 COLON-ALIGNED WIDGET-ID 4
          LABEL "Full Cost" FORMAT "->>>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     itemfg.total-std-cost AT ROW 13.1 COL 124.6 COLON-ALIGNED
          LABEL "Total Std Cost" FORMAT "->>>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     itemfg.avg-cost AT ROW 14.05 COL 124.6 COLON-ALIGNED
          LABEL "Average Cost" FORMAT ">>>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     itemfg.last-cost AT ROW 15 COL 124.6 COLON-ALIGNED
          LABEL "Last Cost" FORMAT ">>>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     fi_type-dscr AT ROW 6.57 COL 117 COLON-ALIGNED NO-LABEL
     itemfg.prod-uom AT ROW 15.95 COL 124.6 COLON-ALIGNED
          LABEL "Cost UOM"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     "Status:" VIEW-AS TEXT
          SIZE 8 BY .95 AT ROW 3.05 COL 70
     "Ship Method:" VIEW-AS TEXT
          SIZE 15 BY .95 AT ROW 4 COL 70
     RECT-10 AT ROW 12.91 COL 69
     RECT-8 AT ROW 9.52 COL 2
     RECT-9 AT ROW 5.29 COL 69
     RECT-11 AT ROW 1.19 COL 2 WIDGET-ID 6
     RECT-12 AT ROW 1.19 COL 69 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.itemfg
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
         HEIGHT             = 17.33
         WIDTH              = 145.8.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN itemfg.avg-cost IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.cad-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.case-count IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN itemfg.case-pall IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN itemfg.curr-code[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.cust-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.def-loc IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.die-no IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN itemfg.est-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR TOGGLE-BOX itemfg.exempt-disc IN FRAME F-Main
   NO-ENABLE 2 4 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN fi_type-dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN itemfg.frt-class IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN itemfg.i-name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.i-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX itemfg.isaset IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.last-cost IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.part-dscr1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.part-dscr2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.part-dscr3 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.part-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.plate-no IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN itemfg.procat IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.prod-code IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN itemfg.prod-notes IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.prod-uom IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR RADIO-SET itemfg.pur-man IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN itemfg.sell-price IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.sell-uom IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.spare-char-1 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.spare-dec-1 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.spare-int-2 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN itemfg.setupDate IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.spc-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR RADIO-SET itemfg.stat IN FRAME F-Main
   2 4                                                                  */
/* SETTINGS FOR FILL-IN itemfg.std-fix-cost IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.std-lab-cost IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.std-mat-cost IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.std-var-cost IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.style IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_taxable IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR TOGGLE-BOX tgVaried IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN itemfg.total-std-cost IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.type-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg.upc-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.weight-100 IN FRAME F-Main
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
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR lv-prep-type AS cha NO-UNDO.
   DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.


   lw-focus = FOCUS.

   CASE lw-focus:NAME :
        WHEN "sell-uom" THEN DO:
             /*run sys/ref/uom-rm.p  (item.mat-type, output uom-list). */
             RUN windows/l-stduom.w (gcompany,uom-list, lw-focus:SCREEN-VALUE, OUTPUT char-val).
         /*    run windows/l-uom.w (lw-focus:SCREEN-VALUE, output char-val).     display all Uom */
             IF char-val <> "" THEN 
                ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val)
                       .
        END.
        WHEN "def-loc" THEN DO:
             RUN windows/l-loc.w (gcompany,lw-focus:SCREEN-VALUE, OUTPUT char-val).
             IF char-val <> "" THEN 
                ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val)
                       .
        END.
        WHEN "cust-no" THEN DO:
             RUN windows/l-cust.w (gcompany, lw-focus:SCREEN-VALUE, OUTPUT char-val).
             IF char-val NE "" AND lw-focus:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:
               lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
               APPLY "value-changed" TO lw-focus.
             END.
        END.
        WHEN "def-loc-bin" THEN DO:
             RUN windows/l-fgbin.w (gcompany,itemfg.def-loc:SCREEN-VALUE,lw-focus:SCREEN-VALUE, OUTPUT char-val).
      /*       run windows/l-locbin.w (gcompany,itemfg.def-loc:SCREEN-VALUE,lw-focus:SCREEN-VALUE, output char-val).  
      */  
             IF char-val <> "" THEN 
                ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val)
                       .
        END.
        WHEN "est-no" THEN DO:
             RUN windows/l-fgest.w (gcompany, itemfg.cust-no:SCREEN-VALUE, lw-focus:SCREEN-VALUE, OUTPUT char-val).
             FIND FIRST eb WHERE RECID(eb) EQ INT(char-val) NO-LOCK NO-ERROR.
             IF AVAIL eb AND TRIM(lw-focus:SCREEN-VALUE) NE TRIM(eb.est-no) THEN DO:
               lw-focus:SCREEN-VALUE = eb.est-no.
               APPLY "value-changed" TO lw-focus.
             END. 
        END.
        WHEN "procat" THEN DO:
             RUN windows/l-fgcat.w (gcompany,lw-focus:SCREEN-VALUE, OUTPUT char-val).
             IF char-val <> "" THEN 
                ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val)
                       itemfg.procat-desc:SCREEN-VALUE = ENTRY(2,char-val)
                       .
        END.
        WHEN "type-code" THEN DO:
             RUN windows/l-ordtyp.w (itemfg.type-code:SCREEN-VALUE, OUTPUT char-val).
             IF char-val NE "" AND itemfg.type-code:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:
               itemfg.type-code:SCREEN-VALUE = ENTRY(1,char-val).
               RUN new-type.
             END.
        END.
        WHEN "style" THEN DO:
             RUN windows/l-style.w (gcompany,lw-focus:SCREEN-VALUE, OUTPUT char-val).
             IF char-val <> "" THEN 
                ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val)
                       itemfg.style-desc:SCREEN-VALUE = ENTRY(2,char-val)
                       .
        END.
        WHEN "plate-no" OR WHEN "die-no" THEN DO:
           lv-prep-type = IF lw-focus:NAME = "Plate-no" THEN "P" ELSE "".
           RUN windows/l-diepl.w (gcompany,lv-prep-type,lw-focus:SCREEN-VALUE, OUTPUT char-val). 
           IF char-val <> "" THEN 
              lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
       END.
       WHEN "frt-class" THEN DO:
             RUN windows/l-frtcls.w (itemfg.frt-class:SCREEN-VALUE, OUTPUT char-val).
             IF char-val <> "" THEN 
                ASSIGN itemfg.frt-class:SCREEN-VALUE = ENTRY(1,char-val)
                       itemfg.frt-class-dscr:SCREEN-VALUE = ENTRY(2,char-val)
                       .
        END.

       WHEN "spare-char-1" THEN DO:
           RUN windows/l-usrgrp.w (INPUT "SALES GROUPS", OUTPUT char-val).
           IF char-val <> "" THEN
               ASSIGN lw-focus:SCREEN-VALUE = char-val.
       END.

       WHEN "cc-code" THEN DO:
           RUN windows/l-usrgrp.w (INPUT "FG Cycle Code", OUTPUT char-val).
           IF char-val <> "" THEN
               ASSIGN lw-focus:SCREEN-VALUE = char-val.
       END.

       WHEN "prod-code" THEN DO:
           RUN windows/l-usrgrp.w (INPUT "FG Production Code", OUTPUT char-val).
           IF char-val <> "" THEN
               ASSIGN lw-focus:SCREEN-VALUE = char-val.
       END.

       WHEN "class" THEN DO:
           RUN windows/l-usrgrp.w (INPUT "FG CLASS", OUTPUT char-val).
           IF char-val <> "" THEN
               ASSIGN lw-focus:SCREEN-VALUE = char-val.
       END.


    END.    


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.case-count
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.case-count V-table-Win
ON LEAVE OF itemfg.case-count IN FRAME F-Main /* Count */
DO:
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND 
       (/*itemfg.i-code:SCREEN-VALUE = "S" and*/
         int(itemfg.case-count:SCREEN-VALUE) < 1 ) 
    THEN DO:
         MESSAGE "Case count can not less than ONE !!! " VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
    END.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.cust-no V-table-Win
ON LEAVE OF itemfg.cust-no IN FRAME F-Main /* Cust# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cust-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-cust-user NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.cust-no V-table-Win
ON VALUE-CHANGED OF itemfg.cust-no IN FRAME F-Main /* Cust# */
DO:
  FIND cust
      WHERE cust.company EQ gcompany
        AND cust.cust-no EQ {&self-name}:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  IF AVAIL cust THEN
    ASSIGN
     {&self-name}:SCREEN-VALUE        = cust.cust-no
     itemfg.cust-name:SCREEN-VALUE    = cust.name
     itemfg.curr-code[1]:SCREEN-VALUE = cust.curr-code
     tb_taxable:SCREEN-VALUE          = STRING(cust.sort EQ "Y" AND cust.tax-gr NE "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.def-loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.def-loc V-table-Win
ON LEAVE OF itemfg.def-loc IN FRAME F-Main /* Warehse */
DO:
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND itemfg.def-loc:SCREEN-VALUE <> "" AND
    NOT CAN-FIND(FIRST loc WHERE loc.company = gcompany AND loc.loc = itemfg.def-loc:SCREEN-VALUE)
    THEN DO:
         MESSAGE "Invalid Warehouse. Try Help." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
    END.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.def-loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.def-loc-bin V-table-Win
ON LEAVE OF itemfg.def-loc-bin IN FRAME F-Main /* Bin */
DO:
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND itemfg.def-loc-bin:SCREEN-VALUE <> "" AND
       NOT CAN-FIND(FIRST fg-bin WHERE fg-bin.company = gcompany AND fg-bin.loc = itemfg.def-loc:SCREEN-VALUE AND
                          fg-bin.loc-bin = itemfg.def-loc-bin:SCREEN-VALUE)
    THEN DO:
         MESSAGE "Invalid Warehouse Bin. Try Help." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
    END.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.est-no V-table-Win
ON LEAVE OF itemfg.est-no IN FRAME F-Main /* Est# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-est-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.frt-class
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.frt-class V-table-Win
ON LEAVE OF itemfg.frt-class IN FRAME F-Main /* Freight Class */
DO:
   DO WITH FRAME {&FRAME-NAME} :

      FIND FIRST freight-class WHERE
           freight-class.freight-class = itemfg.frt-class:SCREEN-VALUE
           NO-LOCK NO-ERROR.

      IF AVAIL freight-class THEN
      DO:
         itemfg.frt-class-dscr:SCREEN-VALUE = freight-class.DESCRIPTION.
         RELEASE freight-class.
      END.
      ELSE
         itemfg.frt-class-dscr:SCREEN-VALUE = "".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.i-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.i-code V-table-Win
ON RETURN OF itemfg.i-code IN FRAME F-Main /* Item Code */
DO:
  APPLY "tab" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.i-code V-table-Win
ON VALUE-CHANGED OF itemfg.i-code IN FRAME F-Main /* Item Code */
DO:
  IF {&self-name}:SCREEN-VALUE EQ "C" THEN DO:

      itemfg.prod-uom:SCREEN-VALUE = "M".

      APPLY "LEAVE" TO itemfg.prod-uom IN FRAME F-MAIN.
   END.

  RUN prod-uom-able.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.i-no V-table-Win
ON ENTRY OF itemfg.i-no IN FRAME F-Main /* FG Item # */
DO:
  RUN enable-itemfg-field.
  IF NOT adm-new-record THEN DO:
    APPLY "tab" TO {&self-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.i-no V-table-Win
ON LEAVE OF itemfg.i-no IN FRAME F-Main /* FG Item # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.isaset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.isaset V-table-Win
ON RETURN OF itemfg.isaset IN FRAME F-Main /* Set Header? */
DO:
   APPLY "tab" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.isaset V-table-Win
ON VALUE-CHANGED OF itemfg.isaset IN FRAME F-Main /* Set Header? */
DO:
  RUN SetPurMan(itemfg.isaset:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Y").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.part-no V-table-Win
ON LEAVE OF itemfg.part-no IN FRAME F-Main /* Cust Part # */
DO:
  IF LASTKEY NE -1 AND itemfg.part-no:SCREEN-VALUE EQ "" THEN DO:
    itemfg.part-no:SCREEN-VALUE = itemfg.i-no:SCREEN-VALUE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.procat V-table-Win
ON LEAVE OF itemfg.procat IN FRAME F-Main /* Category */
DO:
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND SELF:SCREEN-VALUE <> "" AND
       NOT CAN-FIND(FIRST fgcat WHERE fgcat.company = gcompany AND
                                      fgcat.procat = SELF:SCREEN-VALUE)
    THEN DO:
         MESSAGE "Invalid Product Category. Try Help." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
    END.

    FIND FIRST fgcat WHERE fgcat.company = gcompany AND
                           fgcat.procat = SELF:SCREEN-VALUE
                           NO-LOCK NO-ERROR.
   itemfg.procat-desc:SCREEN-VALUE = IF AVAIL fgcat THEN fgcat.dscr ELSE "".
   {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.prod-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.prod-uom V-table-Win
ON LEAVE OF itemfg.prod-uom IN FRAME F-Main /* Cost UOM */
DO:
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND 
       ( (itemfg.i-code:SCREEN-VALUE = "S" AND
          can-do("EA,M",itemfg.prod-uom:SCREEN-VALUE )) OR
         (itemfg.i-code:SCREEN-VALUE = "C" AND can-do("M",itemfg.prod-uom:SCREEN-VALUE) )
        )
    THEN DO:  END.
    ELSE IF LASTKEY <>  -1 THEN DO:
         MESSAGE "Enter M for Box Products, Enter EA or M for Non Box Products."
                   VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
    END.     
    DEF VAR cocode AS cha NO-UNDO.
    cocode = gcompany.

    FIND CURRENT itemfg NO-WAIT NO-ERROR.

    ASSIGN itemfg.prod-uom:SCREEN-VALUE  = INPUT itemfg.prod-uom.

    RUN sys/ref/convcuom.p (itemfg.prod-uom, INPUT itemfg.prod-uom:SCREEN-VALUE,
                            0, 0, 0, 0, itemfg.avg-cost,
                                 OUTPUT itemfg.avg-cost).

    RUN sys/ref/convcuom.p (itemfg.prod-uom, INPUT itemfg.prod-uom:SCREEN-VALUE,
                            0, 0, 0, 0, itemfg.last-cost,
                                 OUTPUT itemfg.last-cost).

    RUN sys/ref/convcuom.p (itemfg.prod-uom, INPUT itemfg.prod-uom:SCREEN-VALUE,
                            0, 0, 0, 0, itemfg.std-mat-cost,
                                 OUTPUT itemfg.std-mat-cost).

    RUN sys/ref/convcuom.p (itemfg.prod-uom, INPUT itemfg.prod-uom:SCREEN-VALUE,
                            0, 0, 0, 0, itemfg.std-lab-cost,
                                 OUTPUT itemfg.std-lab-cost).

    RUN sys/ref/convcuom.p (itemfg.prod-uom, INPUT itemfg.prod-uom:SCREEN-VALUE,
                            0, 0, 0, 0, itemfg.std-var-cost,
                                 OUTPUT itemfg.std-var-cost).

    RUN sys/ref/convcuom.p (itemfg.prod-uom, INPUT itemfg.prod-uom:SCREEN-VALUE,
                            0, 0, 0, 0, itemfg.std-fix-cost,
                                 OUTPUT itemfg.std-fix-cost).

    ASSIGN itemfg.avg-cost:SCREEN-VALUE = STRING(itemfg.avg-cost)
           itemfg.last-cost:SCREEN-VALUE = STRING(itemfg.last-cost)
           itemfg.std-mat-cost:SCREEN-VALUE = STRING(itemfg.std-mat-cost)
           itemfg.std-lab-cost:SCREEN-VALUE = STRING(itemfg.std-lab-cost)
           itemfg.std-var-cost:SCREEN-VALUE = STRING(itemfg.std-var-cost)
           itemfg.std-fix-cost:SCREEN-VALUE = STRING(itemfg.std-fix-cost).

    RUN calc-std-cost.    

    FOR EACH fg-bin  WHERE fg-bin.company EQ gcompany
                       AND fg-bin.i-no    EQ itemfg.i-no:SCREEN-VALUE
                       AND fg-bin.pur-uom NE INPUT itemfg.prod-uom:


        RUN sys/ref/convcuom.p (fg-bin.pur-uom, INPUT itemfg.prod-uom:SCREEN-VALUE,
                                0, 0, 0, 0, fg-bin.avg-cost,
                                     OUTPUT fg-bin.avg-cost).

        RUN sys/ref/convcuom.p (fg-bin.pur-uom, INPUT itemfg.prod-uom:SCREEN-VALUE,
                                0, 0, 0, 0, fg-bin.last-cost,
                                     OUTPUT fg-bin.last-cost).

        RUN sys/ref/convcuom.p (fg-bin.pur-uom, INPUT itemfg.prod-uom:SCREEN-VALUE,
                                0, 0, 0, 0, fg-bin.std-mat-cost,
                                     OUTPUT fg-bin.std-mat-cost).

        RUN sys/ref/convcuom.p (fg-bin.pur-uom, INPUT itemfg.prod-uom:SCREEN-VALUE,
                                0, 0, 0, 0, fg-bin.std-lab-cost,
                                     OUTPUT fg-bin.std-lab-cost).

        RUN sys/ref/convcuom.p (fg-bin.pur-uom, INPUT itemfg.prod-uom:SCREEN-VALUE,
                                0, 0, 0, 0, fg-bin.std-var-cost,
                                     OUTPUT fg-bin.std-var-cost).

        RUN sys/ref/convcuom.p (fg-bin.pur-uom, INPUT itemfg.prod-uom:SCREEN-VALUE,
                                0, 0, 0, 0, fg-bin.std-fix-cost,
                                     OUTPUT fg-bin.std-fix-cost).

        ASSIGN
         fg-bin.std-tot-cost = fg-bin.std-mat-cost +
                               fg-bin.std-lab-cost +
                               fg-bin.std-var-cost +
                               fg-bin.std-fix-cost
         fg-bin.pur-uom      = INPUT itemfg.prod-uom.

    END.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.pur-man
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.pur-man V-table-Win
ON RETURN OF itemfg.pur-man IN FRAME F-Main /* Purchased or Manf */
DO:
   APPLY "tab" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.pur-man V-table-Win
ON VALUE-CHANGED OF itemfg.pur-man IN FRAME F-Main /* Purchased or Manf */
DO:
    DEFINE BUFFER bf-eb FOR eb.
    
    IF itemfg.pur-man:SCREEN-VALUE EQ "no" THEN ASSIGN
        itemfg.prod-uom:SCREEN-VALUE = "M".
    
    FIND FIRST bf-eb NO-LOCK WHERE 
        bf-eb.company EQ cocode AND 
        bf-eb.stock-no EQ itemfg.i-no:SCREEN-VALUE AND 
        bf-eb.pur-man NE logical(itemfg.pur-man:SCREEN-VALUE) 
        NO-ERROR.

    IF AVAIL bf-eb THEN DO:
        MESSAGE "Purchased / Manufactured Field" SKIP 
                "Estimate Does Not Match Finished Goods." SKIP
                "Reset Both? "
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lCheckPurMan . 
     END.
     ELSE lCheckPurMan = NO .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.sell-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.sell-uom V-table-Win
ON LEAVE OF itemfg.sell-uom IN FRAME F-Main /* UOM */
DO:
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 THEN DO:
      IF SELF:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE 
              "Unit of Measure can't be blank. Please enter a valid UOM." 
             VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.

      IF SELF:SCREEN-VALUE <> "" AND
         NOT CAN-FIND(FIRST uom WHERE uom.uom = SELF:SCREEN-VALUE AND
                         lookup(uom.uom, uom-list) > 0 )
        THEN DO:
          MESSAGE 
              "Invalid Unit of Measure. Try help." 
             VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
    END.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.std-fix-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.std-fix-cost V-table-Win
ON LEAVE OF itemfg.std-fix-cost IN FRAME F-Main /* Std Fix OH Cost */
DO:
  RUN calc-std-cost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.std-lab-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.std-lab-cost V-table-Win
ON LEAVE OF itemfg.std-lab-cost IN FRAME F-Main /* Std Labor Cost */
DO:
  RUN calc-std-cost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.std-mat-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.std-mat-cost V-table-Win
ON LEAVE OF itemfg.std-mat-cost IN FRAME F-Main /* Std Mat'l Cost */
DO:
  RUN calc-std-cost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.std-var-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.std-var-cost V-table-Win
ON LEAVE OF itemfg.std-var-cost IN FRAME F-Main /* Std Var OH Cost */
DO:
  RUN calc-std-cost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.style V-table-Win
ON LEAVE OF itemfg.style IN FRAME F-Main /* Style */
DO:  
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND SELF:SCREEN-VALUE <> "" AND
       NOT CAN-FIND(FIRST style WHERE style.company = gcompany AND
                                      style.style = SELF:SCREEN-VALUE)
    THEN DO:
         MESSAGE "Invalid Style. Try Help." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
    END.

    FIND FIRST style WHERE style.company = gcompany AND
                                      style.style = SELF:SCREEN-VALUE
                                      NO-LOCK NO-ERROR.
    itemfg.style-desc:SCREEN-VALUE = IF AVAIL style THEN style.dscr ELSE "".
    {&methods/lValidateError.i NO}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.type-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.type-code V-table-Win
ON LEAVE OF itemfg.type-code IN FRAME F-Main /* Type Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-type NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.type-code V-table-Win
ON VALUE-CHANGED OF itemfg.type-code IN FRAME F-Main /* Type Code */
DO:
  RUN new-type.
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
{sys/ref/CustList.i NEW}
DO TRANSACTION:
   {sys/inc/graphic.i}
   {sys/inc/fgsecur.i}
   {sys/inc/custlistform.i ""IF1"" }
END.

SESSION:DATA-ENTRY-RETURN = YES.

/* IF fgsecurity-log THEN                                  */
/* DO:                                                     */
/*    FIND FIRST usergrps WHERE                            */
/*         usergrps.usergrps = fgsecurity-char             */
/*         NO-LOCK NO-ERROR.                               */
/*                                                         */
/*    IF AVAIL usergrps AND                                */
/*       (NOT CAN-DO(usergrps.users,USERID("NOSWEAT")) AND */
/*        TRIM(usergrps.users) NE "*") THEN                */
/*        ASSIGN itemfg.std-mat-cost:VISIBLE = NO          */
/*               itemfg.std-lab-cost:VISIBLE = NO          */
/*               itemfg.std-var-cost:VISIBLE = NO          */
/*               itemfg.std-fix-cost:VISIBLE = NO          */
/*               itemfg.total-std-cost:VISIBLE = NO        */
/*               itemfg.avg-cost:VISIBLE = NO              */
/*               itemfg.last-cost:VISIBLE = NO             */
/*               itemfg.spare-dec-1:VISIBLE = NO           */
/*               itemfg.prod-uom:VISIBLE = NO.             */
/* END.                                                    */
RUN hide-fgsecure-fields.
tg-Freeze-weight:SENSITIVE = FALSE. /* Was enabled initially without doing update */
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
  {src/adm/template/row-list.i "itemfg"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "itemfg"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-std-cost V-table-Win 
PROCEDURE calc-std-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&frame-name}:
    itemfg.total-std-cost:SCREEN-VALUE =
       STRING(dec(itemfg.std-mat-cost:SCREEN-VALUE) +
              dec(itemfg.std-lab-cost:SCREEN-VALUE) +
              dec(itemfg.std-var-cost:SCREEN-VALUE) +
              dec(itemfg.std-fix-cost:SCREEN-VALUE)).

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-Navigation V-table-Win 
PROCEDURE Disable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-itemfg-field V-table-Win 
PROCEDURE enable-itemfg-field :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-itemfg FOR itemfg.

  DEF VAR ll AS LOG NO-UNDO.

  DO WITH FRAME {&frame-name}:
    ENABLE ALL.

    ASSIGN
       old-est-no = IF AVAIL itemfg THEN itemfg.est-no ELSE ""
       lv-puruom = ""
       cDefaultProdUom = "".

    DISABLE itemfg.cust-name
            itemfg.procat-desc
            itemfg.style-desc
            fi_type-dscr.

    IF NOT adm-new-record THEN DO:
      DISABLE itemfg.i-no .
      /*IF itemfg.est-no:SCREEN-VALUE NE "" THEN DO:
        MESSAGE "IMPORT Estimate Info (Part#, Unit Count, Style, Die#, Plate#, etc.) for FG?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll.
        IF ll THEN RUN new-est-no.
      END.*/
    END.
    ELSE
    IF adm-adding-record THEN DO: 
        ASSIGN tb_taxable:SCREEN-VALUE = "No".

        FIND FIRST sys-ctrl NO-LOCK
            WHERE sys-ctrl.company EQ cocode
              AND sys-ctrl.NAME EQ "FGMASTER" NO-ERROR.
        IF AVAIL sys-ctrl THEN
            FIND FIRST bf-itemfg NO-LOCK
                WHERE bf-itemfg.company EQ sys-ctrl.company
                  AND bf-itemfg.i-no EQ trim(sys-ctrl.char-fld) NO-ERROR.
            IF AVAIL bf-itemfg 
              THEN ASSIGN tb_taxable:SCREEN-VALUE = STRING(bf-itemfg.taxable)
                          lv-puruom               = bf-itemfg.pur-uom
                          /* gdm - 11190901 */  
                          v-shpmet                = bf-itemfg.ship-meth
                          cDefaultProdUom         = bf-itemfg.prod-uom
                           .  

    END.

    IF AVAIL itemfg THEN DO:
      IF itemfg.q-onh NE 0 THEN
        DISABLE itemfg.std-mat-cost
                itemfg.std-lab-cost
                itemfg.std-var-cost
                itemfg.std-fix-cost
                itemfg.total-std-cost
                itemfg.avg-cost
                itemfg.last-cost.

      RUN prod-uom-able.
    END.
  END.
    RUN hide-fgsecure-fields.

  /* Per validation code, M should be valid in all cases */ 
  IF cDefaultProdUom EQ "" THEN
     cDefaultProdUom = "M".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Navigation V-table-Win 
PROCEDURE Enable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hide-fgsecure-fields V-table-Win 
PROCEDURE hide-fgsecure-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF fgsecurity-log THEN
DO WITH FRAME {&FRAME-NAME}:
   FIND FIRST usergrps WHERE
        usergrps.usergrps = fgsecurity-char
        NO-LOCK NO-ERROR.

   IF AVAIL usergrps AND
      (NOT CAN-DO(usergrps.users,USERID("NOSWEAT")) AND
       TRIM(usergrps.users) NE "*") THEN
       ASSIGN itemfg.std-mat-cost:SENSITIVE = NO
              itemfg.std-lab-cost:SENSITIVE = NO
              itemfg.std-var-cost:SENSITIVE = NO
              itemfg.std-fix-cost:SENSITIVE = NO
              itemfg.total-std-cost:SENSITIVE = NO
              itemfg.avg-cost:SENSITIVE = NO
              itemfg.last-cost:SENSITIVE = NO
              itemfg.spare-dec-1:SENSITIVE = NO
              itemfg.prod-uom:SENSITIVE = NO
              itemfg.std-mat-cost:VISIBLE = NO
              itemfg.std-lab-cost:VISIBLE = NO
              itemfg.std-var-cost:VISIBLE = NO
              itemfg.std-fix-cost:VISIBLE = NO
              itemfg.total-std-cost:VISIBLE = NO
              itemfg.avg-cost:VISIBLE = NO
              itemfg.last-cost:VISIBLE = NO
              itemfg.spare-dec-1:VISIBLE = NO
              itemfg.prod-uom:VISIBLE = NO.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IsASet V-table-Win 
PROCEDURE IsASet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opIsASet AS LOGICAL NO-UNDO.

  opIsASet = AVAILABLE itemfg AND itemfg.isaset.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-custype      LIKE oe-prmtx.custype NO-UNDO.
  DEF VAR lv-cust-no      LIKE oe-prmtx.cust-no NO-UNDO.


  DEF BUFFER b-i   FOR itemfg.
  DEF BUFFER b-ei  FOR e-itemfg.
  DEF BUFFER b-eiv FOR e-itemfg-vend.
  DEF BUFFER bf-eb FOR eb.
  DEF BUFFER bf-notes FOR notes.

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND FIRST b-i 
      WHERE b-i.company EQ itemfg.company
        AND b-i.i-no    EQ itemfg.i-no
      NO-LOCK NO-ERROR.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN tg-Freeze-weight.
  END.
   IF adm-new-record AND NOT adm-adding-record THEN
       lv-puruom = itemfg.pur-uom.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  

  IF tg-freeze-weight THEN
      itemfg.spare-int-1 = 1.
  ELSE
      itemfg.spare-int-1 = 0.

  IF adm-new-record AND NOT adm-adding-record AND AVAIL b-i THEN DO: /* copy */

    FOR EACH b-ei OF b-i NO-LOCK:
      FIND FIRST e-itemfg OF itemfg NO-LOCK NO-ERROR.

      IF NOT AVAIL e-itemfg THEN DO:
        CREATE e-itemfg.
        BUFFER-COPY b-ei EXCEPT rec_key TO e-itemfg
        ASSIGN
         e-itemfg.i-no = itemfg.i-no.
        RELEASE e-itemfg.
      END.

      FOR EACH b-eiv OF b-ei NO-LOCK:
        CREATE e-itemfg-vend.
        BUFFER-COPY b-eiv EXCEPT rec_key TO e-itemfg-vend
        ASSIGN
         e-itemfg-vend.i-no = itemfg.i-no.
        RELEASE e-itemfg-vend.
      END.

      LEAVE.
    END.
    IF v-cpyspc THEN DO:
         FOR EACH notes WHERE notes.rec_key = b-i.rec_key
             AND notes.note_type EQ "S" 
             AND notes.note_code GE v-begspc 
             AND notes.note_code LE v-endspc NO-LOCK:

             CREATE bf-notes .
             BUFFER-COPY notes EXCEPT rec_key TO bf-notes .
             ASSIGN
                 bf-notes.rec_key = itemfg.rec_key .

         END. /*FOR EACH notes */
         ASSIGN v-cpyspc = NO .

     END. /*IF v-cpyspc THEN */

  END.

  /*Task# 04121312*/
 FIND FIRST fg-set WHERE fg-set.company = itemfg.company 
     AND fg-set.set-no = itemfg.i-no NO-LOCK NO-ERROR.

 IF AVAIL itemfg AND (AVAIL fg-set OR lCheckPurMan) THEN
   FOR EACH eb NO-LOCK      
       WHERE eb.company EQ itemfg.company
         AND eb.cust-no EQ itemfg.cust-no
         AND eb.stock-no EQ itemfg.i-no:

     FIND bf-eb WHERE ROWID(bf-eb) EQ ROWID(eb) EXCLUSIVE NO-WAIT NO-ERROR.
     IF AVAIL bf-eb THEN DO:
       ASSIGN bf-eb.pur-man = itemfg.pur-man.
     END.
     RELEASE bf-eb.
   END. /* each eb */
   ASSIGN lCheckPurMan = NO .

  IF NOT  v-mat AND adm-new-record AND NOT adm-adding-record THEN DO: /* task 06161508 */

         FOR EACH e-itemfg OF itemfg  NO-LOCK:

             FOR EACH e-itemfg-vend OF e-itemfg 
                 WHERE e-itemfg-vend.est-no EQ "" NO-LOCK:
               FIND b-eiv WHERE ROWID(b-eiv) EQ ROWID(e-itemfg-vend) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
               IF AVAIL b-eiv THEN
                 DELETE b-eiv .
             END.

             FIND b-ei WHERE ROWID(b-ei) EQ ROWID(e-itemfg) EXCLUSIVE-LOCK NO-ERROR.

             IF AVAIL b-ei THEN
               DELETE b-ei.

         END.
         ASSIGN v-mat = YES .
  END. /* if copy and not v-mat */
  ELSE IF adm-new-record AND NOT adm-adding-record THEN DO:  /* task 06161508 Blank vander problem on test */

     ASSIGN i = 0 .
     FOR EACH e-itemfg OF itemfg  EXCLUSIVE-LOCK:

          FOR EACH e-itemfg-vend OF e-itemfg 
                 WHERE e-itemfg-vend.est-no EQ "" 
                 AND e-itemfg-vend.vend-no EQ "" NO-LOCK :

                    ASSIGN i =  i + 1.

                    IF i = 2  THEN DO:
                      FIND b-eiv WHERE ROWID(b-eiv) EQ ROWID(e-itemfg-vend) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                      IF AVAIL b-eiv THEN
                        DELETE b-eiv .
                    END. /* if i = 2 */

          END. /* each e-itemfg-vend ... */

     END. /* each e-itemfg ... */
  END. /* if copy */

  ASSIGN itemfg.taxable = tb_taxable
         itemfg.spare-char-2 = (IF tgVaried:CHECKED IN FRAME {&FRAME-NAME} THEN 'YES' ELSE 'NO').

  /* btr - refresh the screen */
  RUN local-display-fields.

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
  DO WITH FRAME {&frame-name}:
    DISABLE ALL.
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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST oe-ctrl WHERE oe-ctrl.company = gcompany NO-LOCK NO-ERROR.

  ASSIGN itemfg.company = gcompany
         itemfg.loc = gloc
/*          itemfg.sell-uom = "M" */
/*       itemfg.alloc = 0 ??? logical */
         itemfg.i-code = IF AVAIL oe-ctrl AND oe-ctrl.i-code THEN "S" ELSE "C"
         itemfg.pur-uom = lv-puruom
         /* gdm - 11190901 */
         itemfg.ship-meth =  v-shpmet
         itemfg.exempt-disc = NO
         itemfg.stat = "A"
         itemfg.setupDate = TODAY.

  DO WITH FRAME {&FRAME-NAME}:

    IF itemfg.prod-uom:VISIBLE EQ FALSE THEN 
       itemfg.prod-uom = cDefaultProdUom.

  END.

  /* Create an itemfg-loc for the default warehouse */
  DO WITH FRAME {&FRAME-NAME}:                                                                           
    RUN fg/chkfgloc.p (INPUT itemfg.i-no:SCREEN-VALUE, INPUT "").
  END.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     tb_taxable:SCREEN-VALUE     = STRING(itemfg.taxable).
/*      rd_status:SCREEN-VALUE      = "A"   */
/*      tb_exempt-disc:SCREEN-VALUE = "no". */
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

  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL itemfg AND NOT adm-new-record THEN DO:
    tb_taxable = itemfg.taxable.


  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
/*   DISABLE rd_status tb_exempt-disc WITH FRAME {&FRAME-NAME}. */

  IF AVAIL itemfg THEN DO:

    FIND FIRST cust WHERE cust.company = itemfg.company
                      AND cust.cust-no = itemfg.cust-no NO-LOCK NO-ERROR.
    IF AVAIL cust AND cust.name <> itemfg.cust-name THEN
       itemfg.cust-name:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cust.name.

    ASSIGN tgVaried:CHECKED = (IF itemfg.spare-char-2 = 'YES' THEN TRUE ELSE FALSE)
           tg-freeze-weight:CHECKED = (IF itemfg.spare-int-1 = 1 THEN TRUE ELSE FALSE).
    RUN SetPurMan(itemfg.isaset).
  END. /* avail itemfg */

  RUN new-type.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-hide V-table-Win 
PROCEDURE local-hide :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN GET-ATTRIBUTE("FIELDS-ENABLED":U).
  IF RETURN-VALUE = "YES":U THEN
  DO:
    MESSAGE "Would you like to save changes before changing pages?":U
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vlChangePages AS LOG.
    RUN dispatch IN THIS-PROCEDURE (IF vlChangePages THEN
                                      'update-record':U
                                    ELSE
                                      'cancel-record':U).
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'hide':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
tg-Freeze-weight:SENSITIVE IN FRAME f-main = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-tax LIKE itemfg.taxable NO-UNDO.
  DEF VAR ll-add-record AS LOG NO-UNDO.
  DEF VAR ll-new-record AS LOG NO-UNDO.
  DEF VAR op-upd-die AS LOG NO-UNDO.
  DEFINE VARIABLE ll-copy AS LOGICAL  INIT  NO  NO-UNDO.

  DEF VAR ll-new-part-no AS LOG NO-UNDO.
  DEF VAR ll-new-part-dscr1 AS LOG NO-UNDO.
  DEF VAR ll-new-part-dscr2 AS LOG NO-UNDO.
  DEF VAR ll-new-part-dscr3 AS LOG NO-UNDO.
  DEF VAR ll-new-i-name AS LOG NO-UNDO.
  DEF VAR ll-new-die-no AS LOG NO-UNDO.
  DEF VAR ll-new-plate-no AS LOG NO-UNDO.
  DEF VAR ll-new-cad-no AS LOG NO-UNDO.
  DEF VAR ll-new-spc-no AS LOG NO-UNDO.
  DEF VAR ll-new-upc-no AS LOG NO-UNDO.
  DEF VAR ll-new-procat AS LOG NO-UNDO.

  tg-Freeze-weight:SENSITIVE  IN FRAME f-main = TRUE.
  /* Code placed here will execute PRIOR to standard behavior. */
  ll-add-record = adm-adding-record.

  /* copy records */
  IF adm-new-record AND NOT adm-adding-record THEN 
      ll-copy = TRUE.

  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-est-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-cust-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-cust-user NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-cust-part NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  {&methods/lValidateError.i YES}
  DO WITH FRAME {&frame-name}:
    IF itemfg.style:SCREEN-VALUE <> "" AND
       NOT CAN-FIND(FIRST style WHERE style.company = gcompany AND
                                      style.style = itemfg.style:SCREEN-VALUE)
    THEN DO:
         MESSAGE "Invalid Style. Try Help." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO itemfg.style.
         RETURN NO-APPLY.
    END.
    IF itemfg.procat:SCREEN-VALUE <> "" AND
       NOT CAN-FIND(FIRST fgcat WHERE fgcat.company = gcompany AND
                                      fgcat.procat = itemfg.procat:SCREEN-VALUE)
    THEN DO:
         MESSAGE "Invalid Product Category. Try Help." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO itemfg.procat.
         RETURN NO-APPLY.
    END.

    RUN valid-type NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF itemfg.def-loc:SCREEN-VALUE <> "" AND
       NOT CAN-FIND(FIRST loc WHERE loc.company = gcompany AND loc.loc = itemfg.def-loc:SCREEN-VALUE)
    THEN DO:
         MESSAGE "Invalid Warehouse. Try Help." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO itemfg.def-loc.
         RETURN NO-APPLY.
    END.

    IF itemfg.def-loc-bin:SCREEN-VALUE <> "" AND
       NOT CAN-FIND(FIRST fg-bin WHERE fg-bin.company = gcompany AND fg-bin.loc = itemfg.def-loc:SCREEN-VALUE AND
                          fg-bin.loc-bin = itemfg.def-loc-bin:SCREEN-VALUE)
    THEN DO:
         MESSAGE "Invalid Warehouse Bin. Try Help." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO itemfg.def-loc-bin.
         RETURN NO-APPLY.
    END.
      IF itemfg.prod-uom:VISIBLE = YES THEN 
      DO:
        IF (itemfg.i-code:SCREEN-VALUE = "S" AND can-do("EA,M",itemfg.prod-uom:SCREEN-VALUE )) OR
           (itemfg.i-code:SCREEN-VALUE = "C" AND can-do("M",itemfg.prod-uom:SCREEN-VALUE) )
        THEN DO:  END.
        ELSE DO:

            IF fgsecurity-log AND ((itemfg.i-code:SCREEN-VALUE = "S" AND can-do("EA,M",itemfg.prod-uom )) OR
           (itemfg.i-code:SCREEN-VALUE = "C" AND can-do("M",itemfg.prod-uom) )) THEN DO: END.
           ELSE DO:
                MESSAGE "Enter M for Box Products, Enter EA or M for Non Box Products."
                       VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO itemfg.prod-uom.
                RETURN NO-APPLY.
            END.
        END.     
    END.
    IF int(itemfg.case-count:SCREEN-VALUE) < 1  
    THEN DO:
         MESSAGE "Case count can not less than ONE !!! " VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO itemfg.case-count.
         RETURN NO-APPLY.
    END.
    ll-tax = tb_taxable:SCREEN-VALUE EQ "YES".
  END.

  /* gdm - 10080910 */
  IF itemfg.sell-uom:SCREEN-VALUE EQ "" THEN DO:
    MESSAGE 
      "Unit of Measure can't be blank. Please enter a valid UOM." 
     VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.
  /* gdm - 10080910 end */
  IF itemfg.prod-uom:SCREEN-VALUE EQ "" AND  itemfg.prod-uom:VISIBLE = YES THEN DO:
    MESSAGE 
      "Cost Unit of Measure can't be blank. Please enter a valid UOM." 
     VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
  RUN calc-std-cost.

  ASSIGN
   old-part-no#   = itemfg.part-no
   old-part-dscr1 = itemfg.part-dscr1
   old-part-dscr2 = itemfg.part-dscr2
   old-part-dscr3 = itemfg.part-dscr3
   old-i-name     = itemfg.i-name
   old-die-no     = itemfg.die-no
   old-plate-no   = itemfg.plate-no
   old-cad-no     = itemfg.cad-no
   old-spc-no     = itemfg.spc-no
   old-upc-no     = itemfg.upc-no
   old-procat     = itemfg.procat
   ll-new-record  = adm-new-record.

  /* Create an itemfg-loc for the default warehouse */
  DO WITH FRAME {&FRAME-NAME}:
    RUN fg/chkfgloc.p (INPUT itemfg.i-no:SCREEN-VALUE, INPUT "").
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE ALL.

    /* Overcome Write trigger */
    DO TRANSACTION:
      FIND CURRENT itemfg EXCLUSIVE NO-ERROR.
      IF AVAIL itemfg THEN
      DO:
         itemfg.taxable = ll-tax.

         IF ll-new-record AND v-graphic-char NE "" AND itemfg.box-image EQ "" THEN 
         DO:
            IF LOOKUP(SUBSTR(v-graphic-char,LENGTH(v-graphic-char)),"\,/") EQ 0 THEN
               v-graphic-char = v-graphic-char + "\".

            IF SEARCH(v-graphic-char + itemfg.i-no:SCREEN-VALUE + ".jpg") NE ? THEN
            DO:
               /*FIND CURRENT itemfg EXCLUSIVE-LOCK NO-ERROR.
               IF AVAIL itemfg THEN 
               DO: */
                  itemfg.box-image = v-graphic-char + itemfg.i-no:SCREEN-VALUE + ".jpg".
                  /*FIND CURRENT itemfg NO-LOCK NO-ERROR.
               END. */
            END.
         END.
      END.
    END.

    tb_taxable:SCREEN-VALUE = STRING(itemfg.taxable).
  END.
    ASSIGN
        ll-new-part-no = old-part-no# NE itemfg.part-no
        ll-new-i-name =  old-i-name  NE itemfg.i-name 
        ll-new-part-dscr1 = old-part-dscr1 NE itemfg.part-dscr1 
        ll-new-part-dscr2 = old-part-dscr2 NE itemfg.part-dscr2
        ll-new-part-dscr3 = old-part-dscr3 NE itemfg.part-dscr3 
        ll-new-plate-no = old-plate-no   NE itemfg.plate-no 
        ll-new-spc-no = old-spc-no     NE itemfg.spc-no 
        ll-new-upc-no = old-upc-no     NE itemfg.upc-no 
        ll-new-die-no = old-die-no    NE itemfg.die-no 
        ll-new-cad-no = old-cad-no NE itemfg.cad-no
        ll-new-procat =  old-procat     NE itemfg.procat.

/*   if old-part-no#   ne itemfg.part-no or     */
/*      old-i-name     ne itemfg.i-name or      */
/*      old-part-dscr1 ne itemfg.part-dscr1 or  */
/*      old-plate-no   ne itemfg.plate-no or    */
/*      old-spc-no     ne itemfg.spc-no or      */
/*      old-upc-no     ne itemfg.upc-no OR      */
/*      old-die-no     NE itemfg.die-no or      */
/*      old-procat     NE itemfg.procat then    */
/*         run update-order(OUTPUT op-upd-die). */
    IF ll-new-part-no OR ll-new-i-name 
        OR ll-new-part-dscr1 OR ll-new-part-dscr2 OR ll-new-part-dscr3 THEN
        RUN update-order(OUTPUT op-upd-die).

    IF ll-new-part-no OR ll-new-i-name OR ll-new-part-dscr1
        OR ll-new-die-no OR ll-new-plate-no OR ll-new-spc-no OR ll-new-upc-no 
        OR ll-new-procat 
        AND TRIM(itemfg.est-no) GT "" THEN
        RUN fg/estupdt.w (ROWID(itemfg),old-part-no#,OUTPUT op-upd-die).

/*     IF old-die-no NE itemfg.die-no OR old-cad-no NE itemfg.cad-no THEN                  */
/*     RUN est/updiecad.p (ROWID(itemfg), old-die-no, old-cad-no, "","itemfg",op-upd-die). */
    IF ll-new-die-no OR ll-new-cad-no THEN
        RUN est/updiecad.p (ROWID(itemfg), old-die-no, old-cad-no, "","itemfg",op-upd-die).

  FIND CURRENT itemfg NO-LOCK NO-ERROR.

  IF ll-add-record THEN DO:

    FIND CURRENT itemfg NO-LOCK NO-ERROR.

    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
    RUN repo-query2 IN WIDGET-HANDLE(char-hdl) (ROWID(itemfg)).
  END.
  IF ll-copy = TRUE THEN 
  DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
    RUN repo-query2 IN WIDGET-HANDLE(char-hdl) (ROWID(itemfg)).
  END.

  /* disable/enable set parts tab */

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "container-source", OUTPUT char-hdl).  
  RUN get-link-handle IN adm-broker-hdl(WIDGET-HANDLE(char-hdl), "page-source", OUTPUT char-hdl). 

  IF itemfg.isaset AND 
    char-hdl NE ""
      THEN RUN enable-folder-page IN WIDGET-HANDLE(char-hdl) (INPUT 6).
      ELSE 
        IF char-hdl NE ""
          THEN RUN disable-folder-page IN WIDGET-HANDLE(char-hdl) (INPUT 6).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-est-no V-table-Win 
PROCEDURE new-est-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-cust-no LIKE eb.cust-no NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    lv-cust-no = itemfg.cust-no:SCREEN-VALUE.
    ASSIGN
       old-est-no  = itemfg.est-no:SCREEN-VALUE .

    FIND FIRST eb
        WHERE eb.company   EQ gcompany
          AND eb.est-no    EQ STRING(INT(itemfg.est-no:SCREEN-VALUE),">>>>>>>>")
          AND (eb.part-no  EQ itemfg.part-no:SCREEN-VALUE OR
               eb.stock-no EQ itemfg.i-no:SCREEN-VALUE)
        NO-LOCK NO-ERROR.

    IF AVAIL eb THEN DO:
      ASSIGN
       itemfg.cad-no:SCREEN-VALUE   = CAPS(eb.cad-no)
       itemfg.spc-no:SCREEN-VALUE   = CAPS(eb.spc-no)
       itemfg.upc-no:SCREEN-VALUE   = CAPS(eb.upc-no)
       itemfg.plate-no:SCREEN-VALUE = CAPS(eb.plate-no)
       itemfg.die-no:SCREEN-VALUE   = CAPS(eb.die-no)
       itemfg.cust-no:SCREEN-VALUE  = CAPS(eb.cust-no)
       itemfg.part-no:SCREEN-VALUE  = CAPS(eb.part-no)
       itemfg.style:SCREEN-VALUE    = eb.style  .   /* Task 01311402 */

      itemfg.case-count:SCREEN-VALUE = STRING(eb.cas-cnt).

      FIND FIRST sys-ctrl
          WHERE sys-ctrl.company EQ cocode
            AND sys-ctrl.name    EQ "OECOUNT"
          NO-LOCK NO-ERROR.
      IF AVAIL sys-ctrl AND NOT sys-ctrl.log-fld THEN
        itemfg.case-count:SCREEN-VALUE =
                            STRING(IF eb.tr-cnt NE 0 THEN eb.tr-cnt
                                   ELSE (eb.cas-cnt * eb.cas-pal)).
    END.

    IF itemfg.cust-no:SCREEN-VALUE NE lv-cust-no THEN
      APPLY "value-changed" TO itemfg.cust-no.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-type V-table-Win 
PROCEDURE new-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    itemfg.type-code:SCREEN-VALUE = CAPS(itemfg.type-code:SCREEN-VALUE).

    li = LOOKUP(itemfg.type-code:SCREEN-VALUE,lv-type-codes) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN li = 0.

    IF li GT 0 AND li LE NUM-ENTRIES(lv-type-dscrs) THEN DO:
      fi_type-dscr:SCREEN-VALUE = ENTRY(li,lv-type-dscrs).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-speccard V-table-Win 
PROCEDURE print-speccard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN fgrep/d-spcard.w (ROWID(itemfg)). 
  /* RUN fgrep/r-spcard.p (ROWID(itemfg)).*/


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
  DEF VAR ll-copied AS LOG NO-UNDO.

  DEF VAR ls-focus AS CHAR NO-UNDO.
  DEF VAR v-cost AS LOG INIT YES NO-UNDO.

  IF AVAIL itemfg THEN DO WITH FRAME {&FRAME-NAME}:

      RUN oeinq/d-cpyfg.w (OUTPUT v-cost, OUTPUT v-mat, OUTPUT v-cpyspc, OUTPUT v-begspc, OUTPUT v-endspc).
     IF NOT v-cost THEN
         ASSIGN
         itemfg.std-mat-cost:SCREEN-VALUE    = "0"
         itemfg.std-lab-cost:SCREEN-VALUE    = "0"
         itemfg.std-var-cost:SCREEN-VALUE    = "0"
         itemfg.std-fix-cost:SCREEN-VALUE    = "0"
         itemfg.total-std-cost:SCREEN-VALUE  = "0"
         itemfg.avg-cost:SCREEN-VALUE        = "0"
         itemfg.last-cost:SCREEN-VALUE       = "0"
         itemfg.spare-dec-1:SCREEN-VALUE     = "0" .


  END. /*IF AVAIL itemfg THEN DO WITH  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prod-uom-able V-table-Win 
PROCEDURE prod-uom-able :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF (itemfg.i-code:SCREEN-VALUE EQ "S" AND itemfg.q-onh EQ 0) OR
       itemfg.prod-uom:SCREEN-VALUE EQ ""                        THEN
      ENABLE itemfg.prod-uom.
    ELSE 
      DISABLE itemfg.prod-uom.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalc-cost V-table-Win 
PROCEDURE recalc-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.


  RUN fg/d-recost.w (ROWID(itemfg)).

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, "record-source", OUTPUT char-hdl).

  RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(itemfg)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query V-table-Win 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  FIND CURRENT itemfg NO-LOCK NO-ERROR.
  RUN dispatch ('display-fields').


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
  {src/adm/template/snd-list.i "itemfg"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetPurMan V-table-Win 
PROCEDURE SetPurMan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iplSetHeader AS LOGICAL NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    IF iplSetHeader THEN DO:
        itemfg.pur-man:REPLACE("Unitized",YES,"Purchased") NO-ERROR.
        itemfg.pur-man:REPLACE("Not Unitized",NO,"Manufactured") NO-ERROR.
        itemfg.pur-man:HELP = "Is the Set Unitized?".
    END.
     ELSE DO:
        itemfg.pur-man:REPLACE("Purchased",YES,"Unitized") NO-ERROR.
        itemfg.pur-man:REPLACE("Manufactured",NO,"Not Unitized") NO-ERROR.
        itemfg.pur-man:HELP = "Is the Item (P)urchased or (M)anufactured?".
     END.

END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-order V-table-Win 
PROCEDURE update-order :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-upd-die AS LOG NO-UNDO.

  RUN fg/ordrupdt.w (ROWID(itemfg),old-part-no#).
/*   IF TRIM(itemfg.est-no) GT "" THEN                                */
/*   RUN fg/estupdt.w (ROWID(itemfg),old-part-no#,OUTPUT op-upd-die). */

   /* -- this code replaced with above dialog run -- per joe 9.13.2005
   def var yn# AS log format "Yes/No" no-undo.
   DEF VAR lv-est-no LIKE itemfg.est-no NO-UNDO.

   def buffer b-eb for eb.

   &SCOPED-DEFINE where-phrase WHERE eb.company    EQ itemfg.company   ~
                                 AND eb.cust-no    EQ itemfg.cust-no   ~
                                 AND ((eb.part-no  EQ old-part-no# AND ~
                                       eb.stock-no EQ "") OR           ~
                                      eb.stock-no  EQ itemfg.i-no)

   MESSAGE "Update Information for Order?"
       VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE yn#.
   if yn# then do:
      if old-part-no# ne itemfg.part-no then
      for each oe-ordl where oe-ordl.company eq itemfg.company
                         and oe-ordl.i-no    eq itemfg.i-no
                         and oe-ordl.part-no eq old-part-no# exclusive-lock:
           oe-ordl.part-no = itemfg.part-no.
      end.

      for each oe-ordl where oe-ordl.company eq itemfg.company
                         and oe-ordl.i-no    eq itemfg.i-no exclusive-lock:
            assign
                oe-ordl.i-name     = itemfg.i-name
                oe-ordl.part-no    = itemfg.part-no
                oe-ordl.part-dscr1 = itemfg.part-dscr1
                oe-ordl.part-dscr2 = itemfg.part-dscr2.
      end.    
   end.

   yn# = TRIM(itemfg.est-no) GT "".

   IF yn# THEN DO:
     yn# = NO.
     MESSAGE "Update Information for Estimate?"
         VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE yn#.
   END. /* if yn# */

   if yn# then do:
     FOR EACH w-est-no:
       DELETE w-est-no.
     END. /* each w-est-no */

     CREATE w-est-no.
     w-est-no = itemfg.est-no.

     DO WHILE AVAIL w-est-no:
       ASSIGN
        w-run     = YES
        lv-est-no = w-est-no.

       FOR EACH eb
           {&where-phrase}
             AND eb.est-no              EQ lv-est-no
             AND TRIM(eb.master-est-no) NE ""
             AND NOT CAN-FIND(FIRST w-est-no WHERE w-est-no EQ eb.master-est-no)
           NO-LOCK:

         CREATE w-est-no.
         w-est-no = eb.master-est-no.
       END. /* each eb */

       FIND FIRST w-est-no WHERE w-run EQ NO NO-ERROR.
     END. /* do while */

     FOR EACH w-est-no BREAK BY w-est-no:
       IF NOT FIRST-OF(w-est-no) THEN DELETE w-est-no.
     END. /* each w-est-no */

     for each w-est-no,
         each eb 
         {&where-phrase}
           and eb.est-no eq w-est-no
         exclusive-lock:

          find first b-eb where b-eb.company eq eb.company
                            and b-eb.est-no  eq w-est-no
                            and w-est-no     eq itemfg.est-no
                            and b-eb.form-no eq eb.form-no
                            and b-eb.part-no eq itemfg.part-no
                            and recid(b-eb)  ne recid(eb)
                            no-lock no-error.
          if not avail b-eb then eb.part-no = itemfg.part-no.  
          assign eb.part-dscr1 = itemfg.i-name
                 eb.part-dscr2 = itemfg.part-dscr1
                 eb.plate-no   = itemfg.plate-no
                 eb.spc-no     = itemfg.spc-no
                 eb.upc-no     = itemfg.upc-no.
     end. /* each w-est-no */

     if avail b-eb then do:
        itemfg.part-no = old-part-no#.
        message "ERROR: Customer Part# already exists on form,"
                "please update estimate."
                view-as alert-box error.
     end. /* if avail b-eb */
   end. /* if yn# */
   -- this code replaced with above dialog run -- per joe 9.12.2005 */

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
    IF (itemfg.cust-no:SCREEN-VALUE EQ "" AND
        itemfg.i-code:SCREEN-VALUE  EQ "C")                              OR
       (itemfg.cust-no:SCREEN-VALUE NE "" AND
        NOT CAN-FIND(FIRST cust
                     WHERE cust.company EQ gcompany
                       AND cust.cust-no EQ itemfg.cust-no:SCREEN-VALUE)) THEN DO:
      MESSAGE "Invalid " + TRIM(itemfg.cust-no:LABEL) + ", try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO itemfg.cust-no.
      RETURN ERROR.
    END.
    FIND cust WHERE cust.company EQ gcompany
                AND cust.cust-no EQ itemfg.cust-no:SCREEN-VALUE
                NO-LOCK NO-ERROR.
    IF AVAIL cust THEN ASSIGN itemfg.cust-name:SCREEN-VALUE    = cust.name.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-part V-table-Win 
PROCEDURE valid-cust-part :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
       IF itemfg.part-no:SCREEN-VALUE EQ "" THEN
           ASSIGN itemfg.part-no:SCREEN-VALUE = itemfg.i-no:SCREEN-VALUE .

       IF itemfg.part-no:SCREEN-VALUE EQ "" THEN DO:
         MESSAGE "Cust part# can't be blank." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO itemfg.part-no.
         RETURN ERROR.
    END.

  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-user V-table-Win 
PROCEDURE valid-cust-user :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
custcount = "".
DEF VAR lActive AS LOG NO-UNDO.
RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'IF1',
                            INPUT YES,
                            OUTPUT lActive).

{sys/inc/chblankcust.i ""IF1""}

  IF ou-log THEN
    DO WITH FRAME {&FRAME-NAME}:
      IF LOOKUP(itemfg.cust-no:SCREEN-VALUE,custcount) = 0 THEN DO:
          MESSAGE "Customer is not on Users Customer List.  "  SKIP
              "Please add customer to Network Admin - Users Customer List."  VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO itemfg.cust-no .
          RETURN ERROR.
      END.
    END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-est-no V-table-Win 
PROCEDURE valid-est-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&frame-name}:
    IF INT(old-est-no) NE INT(itemfg.est-no:SCREEN-VALUE) THEN RUN new-est-no.

    IF INT(itemfg.est-no:SCREEN-VALUE) GT 0 AND
       NOT CAN-FIND(FIRST eb WHERE eb.company   EQ gcompany
                               AND eb.est-no    EQ STRING(INT(itemfg.est-no:SCREEN-VALUE),">>>>>>>>")
                               AND (eb.part-no  EQ itemfg.part-no:SCREEN-VALUE OR
                                    eb.stock-no EQ itemfg.i-no:SCREEN-VALUE))
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO itemfg.est-no.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no V-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-itemfg FOR itemfg.

  DEF VAR v-msg AS CHAR NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    v-msg = "".

    IF itemfg.i-no:SCREEN-VALUE EQ "" THEN v-msg = "may not be spaces".

    ELSE
    IF CAN-FIND(FIRST b-itemfg WHERE b-itemfg.company EQ gcompany
                                 AND b-itemfg.i-no    EQ itemfg.i-no:SCREEN-VALUE
                                 AND ROWID(b-itemfg)  NE ROWID(itemfg))
    THEN v-msg = "already exists".

    IF v-msg NE "" THEN DO:
      MESSAGE "Sorry, " + TRIM(itemfg.i-no:LABEL) + " " + TRIM(v-msg)
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO itemfg.i-no.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-type V-table-Win 
PROCEDURE valid-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF TRIM(itemfg.type-code:SCREEN-VALUE) NE ""                AND
       LOOKUP(itemfg.type-code:SCREEN-VALUE,lv-type-codes) LE 0 THEN DO:
      MESSAGE "Invalid Type, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO itemfg.type-code.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

