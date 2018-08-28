&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/item.w

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

{custom/gcompany.i}
{custom/gloc.i}

&Scoped-define first-time yes

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.
&Scoped-define enable-item enable-item

DEF VAR ect-label AS CHAR NO-UNDO.
DEF VAR ect-help AS CHAR NO-UNDO.
DEF VAR ect-format AS CHAR NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES item
&Scoped-define FIRST-EXTERNAL-TABLE item


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR item.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS item.i-code item.tax-rcpt item.i-name ~
item.i-dscr item.cost-type item.est-dscr item.procat item.cal item.shrink ~
item.case-l item.basis-w item.s-wid item.case-w item.reg-no item.s-len ~
item.case-d item.r-wid item.avg-w item.weight-100 item.dept-name[1] ~
item.dept-name[2] item.dept-name[3] item.dept-name[4] item.dept-name[5] ~
item.dept-name[6] item.dept-name[7] item.dept-name[8] item.dept-name[9] ~
item.dept-name[10] item.box-case item.speed%[1] item.speed%[2] ~
item.speed%[3] item.speed%[4] item.speed%[5] item.speed%[6] item.speed%[7] ~
item.speed%[8] item.speed%[9] item.speed%[10] item.case-pall item.sqin-lb ~
item.ink-type item.flute item.press-type item.linin-lb item.yield ~
item.min-lbs 
&Scoped-define ENABLED-TABLES item
&Scoped-define FIRST-ENABLED-TABLE item
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 RECT-6 ~
RECT-7 fi_ect fi_cas-pal-w fi_reg-no 
&Scoped-Define DISPLAYED-FIELDS item.i-no item.i-code item.tax-rcpt ~
item.i-name item.i-dscr item.cost-type item.est-dscr item.procat item.cal ~
item.shrink item.case-l item.q-ptd item.basis-w item.s-wid item.case-w ~
item.q-ytd item.reg-no item.s-len item.case-d item.q-lyr item.r-wid ~
item.avg-w item.weight-100 item.dept-name[1] item.dept-name[2] ~
item.dept-name[3] item.dept-name[4] item.dept-name[5] item.dept-name[6] ~
item.dept-name[7] item.dept-name[8] item.dept-name[9] item.dept-name[10] ~
item.box-case item.speed%[1] item.speed%[2] item.speed%[3] item.speed%[4] ~
item.speed%[5] item.speed%[6] item.speed%[7] item.speed%[8] item.speed%[9] ~
item.speed%[10] item.case-pall item.sqin-lb item.ink-type item.flute ~
item.press-type item.linin-lb item.yield item.min-lbs 
&Scoped-define DISPLAYED-TABLES item
&Scoped-define FIRST-DISPLAYED-TABLE item
&Scoped-Define DISPLAYED-OBJECTS fi_mat-type mat_dscr u-ptd costtype_descr ~
u-ytd procat_dscr u-lyr fi_ect fi_cas-pal-w fi_reg-no group1-text ~
group4-text group3-text group2-text ink-type-label press-type-label 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS item.i-no 
&Scoped-define ADM-ASSIGN-FIELDS fi_cas-pal-w fi_reg-no 
&Scoped-define DISPLAY-FIELD fi_mat-type item.cost-type item.procat fi_ect ~
fi_cas-pal-w 

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
DEFINE VARIABLE costtype_descr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE fi_cas-pal-w AS DECIMAL FORMAT ">,>>9.99":U INITIAL 0 
     LABEL "Weight" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE fi_ect AS DECIMAL FORMAT ">>9" INITIAL 0 
     LABEL "Finish" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE fi_mat-type AS CHARACTER FORMAT "X" 
     LABEL "Mat'l Type" 
     VIEW-AS FILL-IN 
     SIZE 5.2 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE fi_reg-no AS CHARACTER FORMAT "X(6)" 
     LABEL "Test" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE group1-text AS CHARACTER FORMAT "X(256)":U INITIAL "Board/Paper/PVC" 
      VIEW-AS TEXT 
     SIZE 21 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE group2-text AS CHARACTER FORMAT "X(256)":U INITIAL "Ink/PVC" 
      VIEW-AS TEXT 
     SIZE 12 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE group3-text AS CHARACTER FORMAT "X(256)":U INITIAL "Film/Leaf/Glue" 
      VIEW-AS TEXT 
     SIZE 18 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE group4-text AS CHARACTER FORMAT "X(256)":U INITIAL "Case/Pallet" 
      VIEW-AS TEXT 
     SIZE 15 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE ink-type-label AS CHARACTER FORMAT "X(256)":U INITIAL "Ink Type:" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE mat_dscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE press-type-label AS CHARACTER FORMAT "X(256)":U INITIAL "Press Type:" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE procat_dscr AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 27.8 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE u-lyr AS DECIMAL FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "LYR" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE u-ptd AS DECIMAL FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "PTD" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE u-ytd AS DECIMAL FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "YTD" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 7.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 3.33.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 3.33.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31 BY 11.19.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 113 BY 5.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 9.29.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 1.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     item.i-no AT ROW 1.48 COL 12 COLON-ALIGNED
          LABEL "Item#"
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
          BGCOLOR 15 FONT 4
     item.i-code AT ROW 1.71 COL 41 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "RM Stocked", "R":U,
"Estimated Mat'l", "E":U
          SIZE 41 BY .81
     item.tax-rcpt AT ROW 1.71 COL 85.4
          LABEL "Taxable?"
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY .81
     item.i-name AT ROW 2.67 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     fi_mat-type AT ROW 2.67 COL 66.8 COLON-ALIGNED
     mat_dscr AT ROW 2.67 COL 73.2 COLON-ALIGNED NO-LABEL
     u-ptd AT ROW 2.67 COL 121 COLON-ALIGNED
     item.i-dscr AT ROW 3.86 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     item.cost-type AT ROW 3.86 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     costtype_descr AT ROW 3.86 COL 73.2 COLON-ALIGNED NO-LABEL
     u-ytd AT ROW 3.86 COL 121 COLON-ALIGNED
     item.est-dscr AT ROW 5.05 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 FONT 4
     item.procat AT ROW 5.05 COL 60 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     procat_dscr AT ROW 5.05 COL 73.2 COLON-ALIGNED NO-LABEL
     u-lyr AT ROW 5.05 COL 121 COLON-ALIGNED
     item.cal AT ROW 6.95 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 FONT 4
     item.shrink AT ROW 6.95 COL 50 COLON-ALIGNED
          LABEL "Shrink %"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     item.case-l AT ROW 6.95 COL 95 COLON-ALIGNED FORMAT ">>9.9999"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     item.q-ptd AT ROW 6.95 COL 121 COLON-ALIGNED
          LABEL "PTD"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
          BGCOLOR 15 FONT 4
     item.basis-w AT ROW 8.14 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 FONT 4
     item.s-wid AT ROW 8.14 COL 50 COLON-ALIGNED
          LABEL "Sheet Width"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     item.case-w AT ROW 8.14 COL 95 COLON-ALIGNED FORMAT ">>9.9999"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     item.q-ytd AT ROW 8.14 COL 121 COLON-ALIGNED
          LABEL "YTD"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
          BGCOLOR 15 FONT 4
     item.reg-no AT ROW 9.33 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 FONT 4
     item.s-len AT ROW 9.33 COL 50 COLON-ALIGNED
          LABEL "Sheet Length"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     item.case-d AT ROW 9.33 COL 95 COLON-ALIGNED FORMAT ">>9.9999"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     item.q-lyr AT ROW 9.33 COL 121 COLON-ALIGNED
          LABEL "LYR"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     item.r-wid AT ROW 10.52 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 FONT 4
     fi_ect AT ROW 10.52 COL 50 COLON-ALIGNED
     item.avg-w AT ROW 10.52 COL 95 COLON-ALIGNED FORMAT ">,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     item.weight-100 AT ROW 11.24 COL 124 COLON-ALIGNED
          LABEL "Wgt/100"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 
     item.dept-name[1] AT ROW 11.71 COL 17 COLON-ALIGNED
          LABEL "Department"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[2] AT ROW 11.71 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[3] AT ROW 11.71 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[4] AT ROW 11.71 COL 35 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[5] AT ROW 11.71 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[6] AT ROW 11.71 COL 47 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[7] AT ROW 11.71 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[8] AT ROW 11.71 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[9] AT ROW 11.71 COL 65 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[10] AT ROW 11.71 COL 71 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.box-case AT ROW 11.71 COL 95 COLON-ALIGNED
          LABEL "Qty/Case" FORMAT ">>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[1] AT ROW 12.91 COL 17 COLON-ALIGNED
          LABEL "Reduction %"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[2] AT ROW 12.91 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[3] AT ROW 12.91 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[4] AT ROW 12.91 COL 35 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[5] AT ROW 12.91 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[6] AT ROW 12.91 COL 47 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     item.speed%[7] AT ROW 12.91 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[8] AT ROW 12.91 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[9] AT ROW 12.91 COL 65 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[10] AT ROW 12.91 COL 71 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.case-pall AT ROW 12.91 COL 95 COLON-ALIGNED FORMAT ">>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     fi_cas-pal-w AT ROW 14.1 COL 95 COLON-ALIGNED HELP
          "Enter weight of this RM in Lbs."
     item.sqin-lb AT ROW 15.05 COL 15 COLON-ALIGNED FORMAT ">>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     item.ink-type AT ROW 15.05 COL 45 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Ink", "I":U,
"Lacquer", "L":U,
"Ultra Violet", "U":U,
"Varnish", "V":U,
"Aqueous", "A":U
          SIZE 75 BY .62
     item.flute AT ROW 15.29 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 
     item.press-type AT ROW 15.76 COL 45 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Flexo", "F":U,
"Gravure", "G":U,
"Letterpress", "L":U,
"Offset", "O":U,
"Silkscreen", "S":U
          SIZE 76 BY .62
     item.linin-lb AT ROW 16.24 COL 15 COLON-ALIGNED
          LABEL "Lin In/UOM"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     item.yield AT ROW 16.48 COL 43 COLON-ALIGNED
          LABEL "SI/Lb" FORMAT ">,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     item.min-lbs AT ROW 16.48 COL 73 COLON-ALIGNED
          LABEL "Min Lbs/Job"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     fi_reg-no AT ROW 16.48 COL 95 COLON-ALIGNED
     group1-text AT ROW 6.24 COL 2 COLON-ALIGNED NO-LABEL
     group4-text AT ROW 6.24 COL 81 COLON-ALIGNED NO-LABEL
     group3-text AT ROW 14.1 COL 1 COLON-ALIGNED NO-LABEL
     group2-text AT ROW 14.1 COL 29 COLON-ALIGNED NO-LABEL
     ink-type-label AT ROW 15.05 COL 31 COLON-ALIGNED NO-LABEL
     press-type-label AT ROW 15.76 COL 28 COLON-ALIGNED NO-LABEL
     "Consumption Qty" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 6.24 COL 116
     "Consumption Cost" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.95 COL 116
     "Totals" VIEW-AS TEXT
          SIZE 9 BY .71 AT ROW 1 COL 117
          FONT 6
     RECT-1 AT ROW 6.48 COL 1
     RECT-2 AT ROW 14.33 COL 29
     RECT-3 AT ROW 14.33 COL 1
     RECT-4 AT ROW 6.48 COL 81
     RECT-5 AT ROW 1.24 COL 1
     RECT-6 AT ROW 1.24 COL 115
     RECT-7 AT ROW 10.76 COL 113
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.item
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
         HEIGHT             = 16.67
         WIDTH              = 139.
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

/* SETTINGS FOR FILL-IN item.avg-w IN FRAME F-Main
   EXP-FORMAT                                                           */
ASSIGN 
       item.avg-w:HIDDEN IN FRAME F-Main           = TRUE
       item.avg-w:PRIVATE-DATA IN FRAME F-Main     = 
                "group4".

ASSIGN 
       item.basis-w:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN item.box-case IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       item.box-case:HIDDEN IN FRAME F-Main           = TRUE
       item.box-case:PRIVATE-DATA IN FRAME F-Main     = 
                "group4".

ASSIGN 
       item.cal:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN item.case-d IN FRAME F-Main
   EXP-FORMAT                                                           */
ASSIGN 
       item.case-d:HIDDEN IN FRAME F-Main           = TRUE
       item.case-d:PRIVATE-DATA IN FRAME F-Main     = 
                "group4".

/* SETTINGS FOR FILL-IN item.case-l IN FRAME F-Main
   EXP-FORMAT                                                           */
ASSIGN 
       item.case-l:HIDDEN IN FRAME F-Main           = TRUE
       item.case-l:PRIVATE-DATA IN FRAME F-Main     = 
                "group4".

/* SETTINGS FOR FILL-IN item.case-pall IN FRAME F-Main
   EXP-FORMAT                                                           */
ASSIGN 
       item.case-pall:HIDDEN IN FRAME F-Main           = TRUE
       item.case-pall:PRIVATE-DATA IN FRAME F-Main     = 
                "group4".

/* SETTINGS FOR FILL-IN item.case-w IN FRAME F-Main
   EXP-FORMAT                                                           */
ASSIGN 
       item.case-w:HIDDEN IN FRAME F-Main           = TRUE
       item.case-w:PRIVATE-DATA IN FRAME F-Main     = 
                "group4".

/* SETTINGS FOR FILL-IN item.cost-type IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN costtype_descr IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       item.dept-name[10]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN item.dept-name[1] IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       item.dept-name[1]:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       item.dept-name[2]:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       item.dept-name[3]:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       item.dept-name[4]:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       item.dept-name[5]:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       item.dept-name[6]:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       item.dept-name[7]:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       item.dept-name[8]:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       item.dept-name[9]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fi_cas-pal-w IN FRAME F-Main
   2 4                                                                  */
ASSIGN 
       fi_cas-pal-w:HIDDEN IN FRAME F-Main           = TRUE
       fi_cas-pal-w:PRIVATE-DATA IN FRAME F-Main     = 
                "group4".

/* SETTINGS FOR FILL-IN fi_ect IN FRAME F-Main
   4                                                                    */
ASSIGN 
       fi_ect:HIDDEN IN FRAME F-Main           = TRUE
       fi_ect:PRIVATE-DATA IN FRAME F-Main     = 
                "group1".

/* SETTINGS FOR FILL-IN fi_mat-type IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi_reg-no IN FRAME F-Main
   2                                                                    */
ASSIGN 
       fi_reg-no:HIDDEN IN FRAME F-Main           = TRUE
       fi_reg-no:PRIVATE-DATA IN FRAME F-Main     = 
                "group4".

ASSIGN 
       item.flute:HIDDEN IN FRAME F-Main           = TRUE
       item.flute:PRIVATE-DATA IN FRAME F-Main     = 
                "group4".

/* SETTINGS FOR FILL-IN group1-text IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       group1-text:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN group2-text IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       group2-text:HIDDEN IN FRAME F-Main           = TRUE
       group2-text:PRIVATE-DATA IN FRAME F-Main     = 
                "group2".

/* SETTINGS FOR FILL-IN group3-text IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       group3-text:HIDDEN IN FRAME F-Main           = TRUE
       group3-text:PRIVATE-DATA IN FRAME F-Main     = 
                "group3".

/* SETTINGS FOR FILL-IN group4-text IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       group4-text:HIDDEN IN FRAME F-Main           = TRUE
       group4-text:PRIVATE-DATA IN FRAME F-Main     = 
                "group4".

/* SETTINGS FOR FILL-IN item.i-no IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
ASSIGN 
       item.ink-type:HIDDEN IN FRAME F-Main           = TRUE
       item.ink-type:PRIVATE-DATA IN FRAME F-Main     = 
                "group2".

/* SETTINGS FOR FILL-IN ink-type-label IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       ink-type-label:HIDDEN IN FRAME F-Main           = TRUE
       ink-type-label:PRIVATE-DATA IN FRAME F-Main     = 
                "group2".

/* SETTINGS FOR FILL-IN item.linin-lb IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       item.linin-lb:HIDDEN IN FRAME F-Main           = TRUE
       item.linin-lb:PRIVATE-DATA IN FRAME F-Main     = 
                "group3".

/* SETTINGS FOR FILL-IN mat_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN item.min-lbs IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       item.min-lbs:HIDDEN IN FRAME F-Main           = TRUE
       item.min-lbs:PRIVATE-DATA IN FRAME F-Main     = 
                "group2".

ASSIGN 
       item.press-type:HIDDEN IN FRAME F-Main           = TRUE
       item.press-type:PRIVATE-DATA IN FRAME F-Main     = 
                "group2".

/* SETTINGS FOR FILL-IN press-type-label IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       press-type-label:HIDDEN IN FRAME F-Main           = TRUE
       press-type-label:PRIVATE-DATA IN FRAME F-Main     = 
                "group2".

/* SETTINGS FOR FILL-IN item.procat IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN procat_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN item.q-lyr IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN item.q-ptd IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN item.q-ytd IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       item.r-wid:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       RECT-1:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       RECT-2:HIDDEN IN FRAME F-Main           = TRUE
       RECT-2:PRIVATE-DATA IN FRAME F-Main     = 
                "group2".

ASSIGN 
       RECT-3:HIDDEN IN FRAME F-Main           = TRUE
       RECT-3:PRIVATE-DATA IN FRAME F-Main     = 
                "group3".

ASSIGN 
       RECT-4:HIDDEN IN FRAME F-Main           = TRUE
       RECT-4:PRIVATE-DATA IN FRAME F-Main     = 
                "group4".

ASSIGN 
       item.reg-no:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN item.s-len IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       item.s-len:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN item.s-wid IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       item.s-wid:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN item.shrink IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       item.shrink:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       item.speed%[10]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN item.speed%[1] IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       item.speed%[1]:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       item.speed%[2]:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       item.speed%[3]:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       item.speed%[4]:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       item.speed%[5]:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       item.speed%[6]:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       item.speed%[7]:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       item.speed%[8]:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       item.speed%[9]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN item.sqin-lb IN FRAME F-Main
   EXP-FORMAT                                                           */
ASSIGN 
       item.sqin-lb:HIDDEN IN FRAME F-Main           = TRUE
       item.sqin-lb:PRIVATE-DATA IN FRAME F-Main     = 
                "group9".

/* SETTINGS FOR TOGGLE-BOX item.tax-rcpt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN u-lyr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN u-ptd IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN u-ytd IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN item.weight-100 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN item.yield IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       item.yield:HIDDEN IN FRAME F-Main           = TRUE
       item.yield:PRIVATE-DATA IN FRAME F-Main     = 
                "group2".

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
   def var lv-handle as handle no-undo.
   def var char-val as cha no-undo.

   CASE Focus:name :
       when "flute" then do:
           /*run est/l-flute.w (output char-val).  using reftable*/
           run windows/l-flute.w (gcompany,output char-val).  /* using flute */
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     item.cal:screen-value = entry(3,char-val).
           return no-apply.   
       end.
       when "fi_reg-no" then do: /* test*/
           run est/l-test.w 
              (gcompany, gloc, item.flute:screen-value /*, focus:screen-value in frame {&frame-name}*/ , output char-val).
           if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
       end.
       when "dept-name" then do: /* dept-name*/
           run windows/l-dept.w 
              ("",focus:screen-value in frame {&frame-name}, output char-val).
           if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
     end.
     when "reg-no" then do: /* dept-name*/
         IF fi_mat-type:SCREEN-VALUE = "R" THEN do:
           run windows/l-reamc.w 
              ( output char-val).
           if char-val <> "" THEN 
               ASSIGN
              focus:screen-value in frame {&frame-name} = entry(1,char-val)
              item.s-wid:SCREEN-VALUE in frame {&frame-name} = entry(2,char-val)
              item.s-len:SCREEN-VALUE in frame {&frame-name}  = entry(3,char-val) .
         END.
           return no-apply.  
     end.
     otherwise do:
           lv-handle = focus:handle.
           run applhelp.p.

           if g_lookup-var <> "" then do:
              lv-handle:screen-value = g_lookup-var.

           end.   /* g_lookup-var <> "" */
           apply "entry" to lv-handle.
           return no-apply.

     end.  /* otherwise */
  end case.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.cost-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.cost-type V-table-Win
ON LEAVE OF item.cost-type IN FRAME F-Main /* Cost Type */
DO:
  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_mat-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_mat-type V-table-Win
ON LEAVE OF fi_mat-type IN FRAME F-Main /* Mat'l Type */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-mat-type NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   &Scoped-define mat-types-enable YES
    DO WITH FRAME {&FRAME-NAME}:
      {custom/mattypes.i}
    END.
    &UNDEFINE mat-types-enable

       IF fi_mat-type:SCREEN-VALUE = "R" THEN do:
        ASSIGN
            item.reg-no:LABEL = "Paper Type" 
            .
    END.
    ELSE IF INDEX("BAP1234",fi_mat-type:SCREEN-VALUE) GT 0 THEN do:
        ASSIGN
            item.reg-no:LABEL = "Reg.#" 
            .
    END.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_mat-type V-table-Win
ON VALUE-CHANGED OF fi_mat-type IN FRAME F-Main /* Mat'l Type */
DO:
  &Scoped-define mat-types-enable NO

  FIND mat WHERE mat.mat EQ fi_mat-type:SCREEN-VALUE NO-LOCK NO-ERROR.

  IF AVAIL mat THEN DO:
    mat_dscr:SCREEN-VALUE = mat.dscr.

    &Scoped-define mat-types-enable YES
    DO WITH FRAME {&FRAME-NAME}:
      {custom/mattypes.i}
    END.
    &UNDEFINE mat-types-enable

    IF fi_mat-type:SCREEN-VALUE = "R" THEN do:
        ASSIGN
            item.reg-no:LABEL = "Paper Type" .
          IF item.reg-no:SCREEN-VALUE = "" THEN
              ASSIGN
              item.reg-no:SCREEN-VALUE = "Writing"
              item.s-wid:SCREEN-VALUE = "17.00"
              item.s-len:SCREEN-VALUE = "22.00" .
    END.
    ELSE IF INDEX("BAP1234",fi_mat-type:SCREEN-VALUE) GT 0 THEN do:
        ASSIGN
            item.reg-no:LABEL = "Reg.#" 
            .
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_reg-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_reg-no V-table-Win
ON LEAVE OF fi_reg-no IN FRAME F-Main /* Test */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-test (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.flute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.flute V-table-Win
ON LEAVE OF item.flute IN FRAME F-Main /* Flute */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-flute NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.i-no V-table-Win
ON LEAVE OF item.i-no IN FRAME F-Main /* Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.ink-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.ink-type V-table-Win
ON VALUE-CHANGED OF item.ink-type IN FRAME F-Main /* Ink Type */
DO:
  {&methods/lValidateError.i YES}
  DEF VAR lv-msg AS CHAR NO-UNDO.

  lv-msg = "".

  IF ITEM.mat-type EQ "V" AND {&self-name}:SCREEN-VALUE EQ "I" THEN
    lv-msg = "a Varnish cannot be Ink".
  ELSE
  IF ITEM.mat-type EQ "I" AND {&self-name}:SCREEN-VALUE EQ "V" THEN
    lv-msg = "an Ink cannot be Varnish".

  IF lv-msg NE "" THEN DO:
    MESSAGE "Invalid entry, " + TRIM(lv-msg) VIEW-AS ALERT-BOX.
    APPLY "entry" TO {&self-name}.
    RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.procat V-table-Win
ON LEAVE OF item.procat IN FRAME F-Main /* Category */
DO:
  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
session:data-entry-return = yes.

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
  {src/adm/template/row-list.i "item"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "item"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-for-record V-table-Win 
PROCEDURE check-for-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER opAvail AS LOGICAL NO-UNDO.

  opAvail =  AVAILABLE {&FIRST-ENABLED-TABLE}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-item V-table-Win 
PROCEDURE disable-item :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-item V-table-Win 
PROCEDURE enable-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAILABLE item THEN
  DO WITH FRAME {&FRAME-NAME}:
    ENABLE fi_mat-type.
    IF INDEX("DC",item.mat-type) GT 0 THEN ENABLE fi_cas-pal-w.
    IF item.mat-type EQ "C" THEN ENABLE item.flute fi_reg-no.
    IF INDEX("BAPR1234",item.mat-type) GT 0 THEN do:
         ENABLE fi_ect.
         IF fi_mat-type = "R" THEN do:
            ASSIGN
                item.reg-no:LABEL = "Paper Type" 
                item.shrink:HIDDEN = TRUE 
                item.dept-name[1]:HIDDEN = TRUE 
                item.dept-name[2]:HIDDEN = TRUE
                item.dept-name[3]:HIDDEN = TRUE
                item.dept-name[4]:HIDDEN = TRUE
                item.dept-name[5]:HIDDEN = TRUE
                item.dept-name[6]:HIDDEN = TRUE
                item.dept-name[7]:HIDDEN = TRUE
                item.dept-name[8]:HIDDEN = TRUE
                item.dept-name[9]:HIDDEN = TRUE
                item.dept-name[10]:HIDDEN = TRUE
                item.speed%[1]:HIDDEN = TRUE
                item.speed%[2]:HIDDEN = TRUE
                item.speed%[3]:HIDDEN = TRUE
                item.speed%[4]:HIDDEN = TRUE
                item.speed%[5]:HIDDEN = TRUE
                item.speed%[6]:HIDDEN = TRUE
                item.speed%[7]:HIDDEN = TRUE
                item.speed%[8]:HIDDEN = TRUE
                item.speed%[9]:HIDDEN = TRUE
                item.speed%[10]:HIDDEN = TRUE .
    END.
    ELSE do:
        ASSIGN
            item.reg-no:LABEL = "Reg.#" 
            item.shrink:HIDDEN = FALSE 
            item.dept-name[1]:HIDDEN = FALSE 
            item.dept-name[2]:HIDDEN = FALSE
            item.dept-name[3]:HIDDEN = FALSE
            item.dept-name[4]:HIDDEN = FALSE
            item.dept-name[5]:HIDDEN = FALSE
            item.dept-name[6]:HIDDEN = FALSE
            item.dept-name[7]:HIDDEN = FALSE
            item.dept-name[8]:HIDDEN = FALSE
            item.dept-name[9]:HIDDEN = FALSE
            item.dept-name[10]:HIDDEN = FALSE
            item.speed%[1]:HIDDEN = FALSE
            item.speed%[2]:HIDDEN = FALSE
            item.speed%[3]:HIDDEN = FALSE
            item.speed%[4]:HIDDEN = FALSE
            item.speed%[5]:HIDDEN = FALSE
            item.speed%[6]:HIDDEN = FALSE
            item.speed%[7]:HIDDEN = FALSE
            item.speed%[8]:HIDDEN = FALSE
            item.speed%[9]:HIDDEN = FALSE
            item.speed%[10]:HIDDEN = FALSE .
    END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE histblnkmsg V-table-Win 
PROCEDURE histblnkmsg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR char-hdl AS CHAR NO-UNDO.  
define buffer b-poline for po-ordl.
{&methods/lValidateError.i YES}
find first b-poline where b-poline.company = item.company
                                      and b-poline.i-no    = item.i-no
                                      and b-poline.opened  = yes no-lock no-error.      

IF not avail b-poline and ITEM.i-no <> "" THEN DO:
   If can-find(first rm-rdtlh where rm-rdtlh.company = item.company
                                      and rm-rdtlh.i-no    = item.i-no
                                      and rm-rdtlh.i-no  NE "") then DO:
      message 'Sorry, History exists, item and history must be purged via system administrator' view-as alert-box error.
      return error .                                  
   END.
end. 
{&methods/lValidateError.i NO}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-item-bom FOR item-bom.
  def buffer bf-item for item.
  def buffer xstack-f for stack-flute.
  def buffer xstack-s for stack-size.
  def var v-i-no like item.i-no no-undo.
  def var cocode as cha no-undo.
  def var locode as cha no-undo.
  def buffer bf-e-item for e-item .
  def buffer bf-e-vend for e-item-vend.
  def var ls-prev-i-no like item.i-no no-undo.
  DEF VAR lv-mat-type LIKE item.mat-type NO-UNDO.
  DEF VAR lv-cas-pal-w LIKE ITEM.basis-w NO-UNDO.
  DEF VAR lv-ect AS DEC NO-UNDO.
  DEF VAR lv-code LIKE stack-flute.code NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  ls-prev-i-no = item.i-no.
  if /*adm-new-record and */ fi_mat-type:screen-value in frame {&frame-name} <> "I" 
  then do:
     if item.ink-type:screen-value = "?" or item.ink-type:screen-value = ?
        then item.ink-type:screen-value = "I".
     if item.press-type:screen-value = "?" or  item.press-type:screen-value = ?
        then item.press-type:screen-value = "F".
  end.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-mat-type  = fi_mat-type:SCREEN-VALUE
     lv-cas-pal-w = DEC(fi_cas-pal-w:SCREEN-VALUE)
     lv-ect       = DEC(fi_ect:SCREEN-VALUE).
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/assign/item.i}

  session:set-wait-state("general").  

  assign cocode = gcompany
         locode = gloc
         fi_mat-type = lv-mat-type
         item.mat-type = lv-mat-type.   

  if adm-new-record and item.mat-type = "D" then do:  /* from rm/cpall.i */
     find first bf-item where bf-item.company = gcompany and
                              bf-item.mat-type = "D" and
                              bf-item.i-no <> item.i-no
                              use-index i-no /*mat-type*/ no-lock no-error.    
     v-i-no = if avail bf-item then caps(bf-item.i-no) else "".
     repeat:
       message "Please enter pallet you wish to copy to the stacking info from:" 
               update v-i-no .
       find first bf-item where bf-item.company = gcompany and
                                bf-item.i-no = v-i-no 
                                no-lock no-error.    
       if not avail bf-item then  next.
       else leave.
     end.  /* repeat v-i-no */
     session:set-wait-state("general").    

     DO WHILE TRUE:
       FIND FIRST stack-flute
           WHERE stack-flute.company EQ cocode
             AND stack-flute.loc     EQ locode
             AND stack-flute.code    GT lv-code
           NO-LOCK NO-ERROR.
       IF NOT AVAIL stack-flute THEN LEAVE.

       lv-code = stack-flute.code.

       FOR EACH stack-flute
           WHERE stack-flute.company EQ cocode
             AND stack-flute.loc     EQ locode
             AND stack-flute.code    EQ lv-code
             AND stack-flute.pallet  EQ item.i-no:
         DELETE stack-flute.
       END.
     END.

     DISABLE TRIGGERS FOR LOAD OF stack-flute.

     FOR EACH flute NO-LOCK WHERE stack-flute.company EQ cocode,
         EACH stack-flute NO-LOCK
         WHERE stack-flute.company EQ flute.company
           AND stack-flute.loc     EQ flute.loc
           AND stack-flute.code    EQ flute.code
           AND stack-flute.pallet  EQ v-i-no:

       CREATE xstack-f.
       BUFFER-COPY stack-flute EXCEPT rec_key to xstack-f
       ASSIGN 
        xstack-f.code   = flute.code
        xstack-f.pallet = CAPS(item.i-no).
     END.

      for each stack-size where stack-size.company eq cocode
                            and stack-size.loc     eq locode
                            and stack-size.pallet  eq item.i-no:
          delete stack-size.
      end.
      for each stack-size where stack-size.company eq cocode
                            and stack-size.loc     eq locode
                            and stack-size.pallet  eq v-i-no
                            no-lock:
          create xstack-s.
          buffer-copy stack-size to xstack-s
          assign xstack-s.pallet = caps(item.i-no).
      end.                     
  end.  /* adm-new-record , item-type = "D" */

  if adm-new-record and not adm-adding-record then do: /* copy */
         find bf-item where bf-item.company = gcompany and
                            bf-item.i-no = ls-prev-i-no
                            no-lock no-error.

         DO WITH FRAME {&FRAME-NAME}:
           BUFFER-COPY bf-item EXCEPT rec_key TO ITEM
           ASSIGN
            {&displayed-fields}
            item.q-onh    = 0
            item.q-ono    = 0
            item.q-comm   = 0
            item.q-back   = 0
            item.q-avail  = 0
            item.mat-type = lv-mat-type
            item.pur-uom  = if bf-item.pur-uom  EQ "" then "MSF"
                                                      else bf-item.pur-uom
            item.cons-uom = if bf-item.cons-uom EQ "" then "EA"
                                                      else bf-item.cons-uom.
         END.
         for each e-item where e-item.company = cocode and
                                e-item.i-no = item.i-no:
               delete e-item.  /* delete rec if exists before create */                
         end.                             
         for each e-item where e-item.company = cocode and
                               e-item.i-no = bf-item.i-no:
             create bf-e-item.
             buffer-copy e-item except e-item.i-no to bf-e-item.
             assign bf-e-item.i-no = item.i-no.                  
         end.                             
         for each e-item-vend where e-item-vend.company = cocode and
                                    e-item-vend.i-no = item.i-no:
             delete e-item-vend.  /* delete rec if exists before create */                
         end.                             
         for each e-item-vend where e-item-vend.company = cocode and
                                    e-item-vend.i-no = bf-item.i-no:
             create bf-e-vend.
             buffer-copy e-item-vend except e-item-vend.i-no to bf-e-vend.
             assign bf-e-vend.i-no      = item.i-no
                    bf-e-vend.item-type = yes.
         end.

         for each item-bom where item-bom.company = cocode and
                                item-bom.parent-i = item.i-no:
               delete item-bom.  /* delete rec if exists before create */                
         end.                             
         for each item-bom where item-bom.company = cocode and
                               item-bom.parent-i = bf-item.i-no:
             create bf-item-bom.
             buffer-copy item-bom except rec_key to bf-item-bom
             assign bf-item-bom.parent-i = item.i-no.                  
         end. 
  end.  /* not adding-record */

  if index("BAP",item.mat-type) > 0 then
    assign
     fi_ect   = lv-ect
     item.ect = lv-ect * (IF item.mat-type NE "A" THEN 10000 ELSE 1).      
  else
  if index("DC",item.mat-type) > 0 then
    assign
     fi_cas-pal-w = lv-cas-pal-w
     item.basis-w = lv-cas-pal-w
     item.reg-no  = fi_reg-no. 

  IF adm-adding-record THEN DO:
     IF INDEX("MOXY789@",ITEM.mat-type) GT 0 THEN
        ASSIGN ITEM.cons-uom = "EA"
               ITEM.pur-uom = "EA".
  END.

  session:set-wait-state("").

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
  RUN disable-item.

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
  assign item.company = gcompany
         item.loc = gloc
         item.industry = "1"
         item.mat-type = ""
         fi_mat-type:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = item.mat-type
         fi_cas-pal-w:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(item.basis-w)
         fi_reg-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = item.reg-no.

  {custom/newkey.i item.i-no}

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
  IF AVAIL ITEM THEN DO WITH FRAME {&FRAME-NAME}:
    fi_mat-type = item.mat-type.

    if index("BAPR1234",fi_mat-type) gt 0 then do:
      IF ect-label EQ "" THEN
        ASSIGN
         ect-label  = fi_ect:LABEL
         ect-help   = fi_ect:HELP
         ect-format = fi_ect:FORMAT.

      IF fi_mat-type NE "A" THEN
        ASSIGN
         fi_ect:LABEL  = "Core Dia."
         fi_ect:HELP   = "Please enter the Core Diameter of this roll"
         fi_ect:FORMAT = ">,>>9.9<<<"
         fi_ect        = item.ect / 10000.

      ELSE
        ASSIGN
         fi_ect:LABEL  = ect-label
         fi_ect:HELP   = ect-help
         fi_ect:FORMAT = ect-format
         fi_ect        = item.ect.

        IF fi_mat-type = "R" THEN 
            ASSIGN
            item.reg-no:LABEL = "Paper Type" .
        ELSE 
            ASSIGN
                item.reg-no:LABEL = "Reg.#" .

    end.

    else
    if index("DC",fi_mat-type) gt 0 then do:
      assign
       fi_cas-pal-w /*:screen-value*/ = (item.basis-w)
       fi_reg-no /*:screen-value*/    = item.reg-no.
    end.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var is-new-record as log no-undo.
  DEF VAR char-hdl AS CHAR NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  is-new-record = adm-new-record.
  /* ========== validate all inputs =============*/

    /* 33482 - Ensure blank record is not saved - MYT - 08/28/18 */
    IF adm-new-record 
    AND item.i-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" THEN DO:
        RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .
        RETURN NO-APPLY.
    END.

  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
     {&methods/lValidateError.i YES}
     if item.dept-name[1]:screen-value in frame {&frame-name} <> "" and
        not can-find(first dept where dept.company = "" and dept.code = item.dept-name[1]:screen-value
                            and (dept.setup <> 99 or dept.fc <> 99 or dept.corr <> 99
                                 or dept.therm <> 99 )
                     )
     then do:
          message "Invalid Department. Try Help." view-as alert-box error.
          apply "entry" to item.dept-name[1] in frame {&frame-name}.
          return no-apply.
     end.                            
     if item.dept-name[2]:screen-value in frame {&frame-name} <> "" and
        not can-find(first dept where dept.company = "" and dept.code = item.dept-name[2]:screen-value
                            and (dept.setup <> 99 or dept.fc <> 99 or dept.corr <> 99
                                 or dept.therm <> 99 )
                     )
     then do:
          message "Invalid Department. Try Help." view-as alert-box error.
          apply "entry" to item.dept-name[2] in frame {&frame-name}.
          return no-apply.
     end.                            
     if item.dept-name[3]:screen-value in frame {&frame-name} <> "" and
        not can-find(first dept where dept.company = "" and dept.code = item.dept-name[3]:screen-value
                            and (dept.setup <> 99 or dept.fc <> 99 or dept.corr <> 99
                                 or dept.therm <> 99 )
                     )
     then do:
          message "Invalid Department. Try Help." view-as alert-box error.
          apply "entry" to item.dept-name[3] in frame {&frame-name}.
          return no-apply.
     end.                            
     if item.dept-name[3]:screen-value in frame {&frame-name} <> "" and
        not can-find(first dept where dept.company = "" and dept.code = item.dept-name[3]:screen-value
                            and (dept.setup <> 99 or dept.fc <> 99 or dept.corr <> 99
                                 or dept.therm <> 99 )
                     )
     then do:
          message "Invalid Department. Try Help." view-as alert-box error.
          apply "entry" to item.dept-name[3] in frame {&frame-name}.
          return no-apply.
     end.                            
     if item.dept-name[4]:screen-value in frame {&frame-name} <> "" and
        not can-find(first dept where dept.company = "" and dept.code = item.dept-name[4]:screen-value
                            and (dept.setup <> 99 or dept.fc <> 99 or dept.corr <> 99
                                 or dept.therm <> 99 )
                     )
     then do:
          message "Invalid Department. Try Help." view-as alert-box error.
          apply "entry" to item.dept-name[4] in frame {&frame-name}.
          return no-apply.
     end.                            
     if item.dept-name[5]:screen-value in frame {&frame-name} <> "" and
        not can-find(first dept where dept.company = "" and dept.code = item.dept-name[5]:screen-value
                            and (dept.setup <> 99 or dept.fc <> 99 or dept.corr <> 99
                                 or dept.therm <> 99 )
                     )
     then do:
          message "Invalid Department. Try Help." view-as alert-box error.
          apply "entry" to item.dept-name[5] in frame {&frame-name}.
          return no-apply.
     end.                            
     if item.dept-name[6]:screen-value in frame {&frame-name} <> "" and
        not can-find(first dept where dept.company = "" and dept.code = item.dept-name[6]:screen-value
                            and (dept.setup <> 99 or dept.fc <> 99 or dept.corr <> 99
                                 or dept.therm <> 99 )
                     )
     then do:
          message "Invalid Department. Try Help." view-as alert-box error.
          apply "entry" to item.dept-name[6] in frame {&frame-name}.
          return no-apply.
     end.                            
     if item.dept-name[7]:screen-value in frame {&frame-name} <> "" and
        not can-find(first dept where dept.company = "" and dept.code = item.dept-name[7]:screen-value
                            and (dept.setup <> 99 or dept.fc <> 99 or dept.corr <> 99
                                 or dept.therm <> 99 )
                     )
     then do:
          message "Invalid Department. Try Help." view-as alert-box error.
          apply "entry" to item.dept-name[7] in frame {&frame-name}.
          return no-apply.
     end.                            
     if item.dept-name[8]:screen-value in frame {&frame-name} <> "" and
        not can-find(first dept where dept.company = "" and dept.code = item.dept-name[8]:screen-value
                            and (dept.setup <> 99 or dept.fc <> 99 or dept.corr <> 99
                                 or dept.therm <> 99 )
                     )
     then do:
          message "Invalid Department. Try Help." view-as alert-box error.
          apply "entry" to item.dept-name[8] in frame {&frame-name}.
          return no-apply.
     end.                            
     if item.dept-name[9]:screen-value in frame {&frame-name} <> "" and
        not can-find(first dept where dept.company = "" and dept.code = item.dept-name[9]:screen-value
                            and (dept.setup <> 99 or dept.fc <> 99 or dept.corr <> 99
                                 or dept.therm <> 99 )
                     )
     then do:
          message "Invalid Department. Try Help." view-as alert-box error.
          apply "entry" to item.dept-name[9] in frame {&frame-name}.
          return no-apply.
     end.                            
     if item.dept-name[10]:screen-value in frame {&frame-name} <> "" and
        not can-find(first dept where dept.company = "" and dept.code = item.dept-name[10]:screen-value
                            and (dept.setup <> 99 or dept.fc <> 99 or dept.corr <> 99
                                 or dept.therm <> 99 )
                     )
     then do:
          message "Invalid Department. Try Help." view-as alert-box error.
          apply "entry" to item.dept-name[10] in frame {&frame-name}.
          return no-apply.
     end.                            

     if /*fi_mat-type:screen-value <> "" and*/
        not can-find(first procat where procat.company = gcompany and
                                        procat.procat = item.procat:screen-value)
     then do:
        message "Invalid Product Category. Try Help."  view-as alert-box error.
        apply "entry" to item.procat in frame {&frame-name}.
        return no-apply.
     end.
     if item.i-code:screen-value = "R" and
        not can-find(costtype where costtype.company = gcompany
          AND costtype.loc = gloc
          AND costtype.cost-type = {&FIRST-EXTERNAL-TABLE}.cost-type:SCREEN-VALUE
          )
     then do:
        message "Invalid Cost Type for real item. Try Help."  view-as alert-box error.
        apply "entry" to item.cost-type in frame {&frame-name}.
        return no-apply.
     end.
    {&methods/lValidateError.i NO}
   RUN valid-mat-type NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN valid-dimensions NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN valid-flute NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   DO WITH FRAME {&FRAME-NAME}:
     RUN valid-test (fi_reg-no:HANDLE) NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.
   {&methods/lValidateError.i YES}
   if index("BAPR1234",fi_mat-type:screen-value) > 0 then do:
     if dec(item.cal:screen-value) = 0 then do:
        message "Caliper is mandatory" view-as alert-box error.
        apply "entry" to item.cal.
        return no-apply.
     end. 
     if dec(item.basis-w:screen-value) = 0 then do:
        message "Basis Weight is mandatory" view-as alert-box error.
        apply "entry" to item.basis-w.
        return no-apply.
     end. 
     if dec(item.r-wid:screen-value) = 0 and item.i-code:screen-value = "R" and
        (dec(item.s-len:screen-value) = 0 or dec(item.s-wid:screen-value) = 0 )
     then do:
        message "Dimensions are mandatory for Real Items!" view-as alert-box error.
        apply "entry" to item.r-wid.
        return no-apply.
     end. 
   end.  /* "BAP" */
   else if index("W",fi_mat-type:screen-value) > 0 then do:
        if dec(item.sqin-lb:screen-value) = 0 then do:
           message trim(item.sqin-lb:label) + " is mandatory!" view-as alert-box error.
           apply "entry" to item.sqin-lb.
           return no-apply.
        end. 
   end.  /* "W"  */ 
   else if index("GTS",fi_mat-type:screen-value) > 0 then do:
        if dec(item.linin-lb:screen-value) eq 0 and
           dec(item.sqin-lb:screen-value) eq 0 then do:
           message "For " + (if fi_mat-type:screen-value eq "S" then "Stitch" 
                             else if fi_mat-type:screen-value eq "T" then "Tape" 
                             else "Glue") +
                    ", " +
                    trim(item.sqin-lb:label) " OR " +
                    trim(item.linin-lb:label) " must be entered..."
               view-as alert-box error.
           apply "entry" to item.linin-lb.
           return no-apply.
        end.
   end.  /* "GTS"  */ 
   else if index("DC",fi_mat-type:screen-value) > 0 then do:
        if dec(item.box-case:screen-value) <> 0 and
           dec(item.avg-w:screen-value) <> 0 then do:
               message "You can enter either Case Weight or Number of blanks per Case!" skip
                       "Only one field can be entered."
                       view-as alert-box error.
               if dec(item.box-case:screen-value) = 0 
                  then apply "entry" to item.box-case.
                  else apply "entry" to item.avg-w.
               return no-apply.
        end. 
   end.
  /* ======== end validation =================== */
  {&methods/lValidateError.i NO}
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN disable-item.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "record-source", OUTPUT char-hdl).

  IF is-new-record THEN do:
      RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(item)). 
      RUN reset-init-values IN WIDGET-HANDLE(char-hdl).
   END.

  RUN dispatch IN WIDGET-HANDLE(char-hdl) ("row-changed").


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
  {src/adm/template/snd-list.i "item"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-dimensions V-table-Win 
PROCEDURE valid-dimensions :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  {custom/validDim.i}

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-flute V-table-Win 
PROCEDURE valid-flute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    item.flute:SCREEN-VALUE = CAPS(item.flute:SCREEN-VALUE).

    IF fi_mat-type:SCREEN-VALUE EQ "C"                              AND
       item.flute:SCREEN-VALUE NE ""                                AND
       NOT CAN-FIND(FIRST flute
                    WHERE flute.company EQ gcompany
                      AND flute.code    EQ item.flute:SCREEN-VALUE) THEN DO:
      MESSAGE item.flute:LABEL + " invalid, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO item.flute.
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

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF item.i-no:SCREEN-VALUE EQ "" THEN DO:
      MESSAGE "Item# may not be spaces..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO item.i-no.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-mat-type V-table-Win 
PROCEDURE valid-mat-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    fi_mat-type:SCREEN-VALUE = CAPS(fi_mat-type:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST mat WHERE mat.mat EQ fi_mat-type:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid Material Type. Try Help."  VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fi_mat-type IN FRAME {&FRAME-NAME}.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-test V-table-Win 
PROCEDURE valid-test :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF fi_mat-type:SCREEN-VALUE EQ "C" AND
       item.flute:SCREEN-VALUE NE ""   AND
       ip-focus:SCREEN-VALUE EQ ""     THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " may not be blank..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

