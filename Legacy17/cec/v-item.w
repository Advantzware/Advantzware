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
DEF VAR k_frac     AS   DEC   INIT 6.25 NO-UNDO.
{custom/gcompany.i}
{custom/gloc.i}
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
ASSIGN cocode = g_company
       locode = g_loc.

&Scoped-define first-time yes
   /* {&item-corr} - for different include file {cec/mattypes.i} 
                         not custom/mattypes.i */
&scoped-define item-corr Corrugated
&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.
&Scoped-define enable-item enable-item

DEF VAR ect-label AS CHAR NO-UNDO.
DEF VAR ect-help AS CHAR NO-UNDO.
DEF VAR ect-format AS CHAR NO-UNDO.

{sys/inc/f16to32.i}

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
item.i-dscr item.cost-type item.est-dscr item.procat item.flute item.shrink ~
item.case-l item.reg-no item.case-w item.s-wid item.s-dep item.cal ~
item.s-len item.density item.case-d item.basis-w item.r-wid item.color-1 ~
item.avg-w item.weight-100 item.dept-name[1] item.dept-name[2] ~
item.dept-name[3] item.dept-name[4] item.dept-name[5] item.dept-name[6] ~
item.dept-name[7] item.dept-name[8] item.dept-name[9] item.dept-name[10] ~
item.box-case item.speed%[1] item.speed%[2] item.speed%[3] item.speed%[4] ~
item.speed%[5] item.speed%[6] item.speed%[7] item.speed%[8] item.speed%[9] ~
item.speed%[10] item.case-pall item.sqin-lb item.linin-lb item.ink-type ~
item.press-type item.yield item.min-lbs item.spare-char-1 
&Scoped-define ENABLED-TABLES item
&Scoped-define FIRST-ENABLED-TABLE item
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-20 RECT-3 RECT-4 RECT-5 ~
RECT-6 RECT-7 RECT-21 fi_ect fi_cas-pal-w fi_flute fi_reg-no group6-text ~
group5-text 
&Scoped-Define DISPLAYED-FIELDS item.i-no item.i-code item.tax-rcpt ~
item.i-name item.i-dscr item.cost-type item.est-dscr item.procat item.q-ptd ~
item.flute item.shrink item.case-l item.reg-no item.q-ytd item.case-w ~
item.s-wid item.s-dep item.cal item.q-lyr item.s-len item.density ~
item.case-d item.basis-w item.r-wid item.color-1 item.avg-w item.weight-100 ~
item.dept-name[1] item.dept-name[2] item.dept-name[3] item.dept-name[4] ~
item.dept-name[5] item.dept-name[6] item.dept-name[7] item.dept-name[8] ~
item.dept-name[9] item.dept-name[10] item.box-case item.speed%[1] ~
item.speed%[2] item.speed%[3] item.speed%[4] item.speed%[5] item.speed%[6] ~
item.speed%[7] item.speed%[8] item.speed%[9] item.speed%[10] item.case-pall ~
item.sqin-lb item.linin-lb item.ink-type item.press-type item.yield ~
item.min-lbs item.spare-char-1 
&Scoped-define DISPLAYED-TABLES item
&Scoped-define FIRST-DISPLAYED-TABLE item
&Scoped-Define DISPLAYED-OBJECTS fi_mat-type mat_dscr u-ptd costtype_descr ~
u-ytd procat_dscr u-lyr fi_ect fi_cas-pal-w fi_flute fi_reg-no F1 F-2 F-3 ~
group1-text group4-text group6-text group5-text group3-text group2-text ~
ink-type-label press-type-label 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS item.i-no 
&Scoped-define ADM-ASSIGN-FIELDS fi_flute fi_reg-no 
&Scoped-define DISPLAY-FIELD fi_mat-type item.cost-type item.procat fi_ect ~
fi_cas-pal-w 
&Scoped-define F1 F1 F-2 F-3 

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

DEFINE VARIABLE F-2 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-3 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE fi_cas-pal-w AS DECIMAL FORMAT ">,>>9.99":U INITIAL 0 
     LABEL "Weight" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE fi_ect AS DECIMAL FORMAT ">>9" INITIAL 0 
     LABEL "ECT" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE fi_flute AS CHARACTER FORMAT "XXX" 
     LABEL "Flute" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE fi_mat-type AS CHARACTER FORMAT "X" 
     LABEL "Mat'l Type" 
     VIEW-AS FILL-IN 
     SIZE 4.2 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE fi_reg-no AS CHARACTER FORMAT "X(6)" 
     LABEL "Test" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE group1-text AS CHARACTER FORMAT "X(256)":U INITIAL "Board/Adder/Wax" 
      VIEW-AS TEXT 
     SIZE 22 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE group2-text AS CHARACTER FORMAT "X(256)":U INITIAL "Ink/Foam" 
      VIEW-AS TEXT 
     SIZE 12 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE group3-text AS CHARACTER FORMAT "X(256)":U INITIAL "Label/Joint" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE group4-text AS CHARACTER FORMAT "X(256)":U INITIAL "Bundle/Pallet" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE group5-text AS CHARACTER FORMAT "X(256)":U INITIAL "" 
      VIEW-AS TEXT 
     SIZE 2.4 BY .62
     FONT 4 NO-UNDO.

DEFINE VARIABLE group6-text AS CHARACTER FORMAT "X(256)":U INITIAL "" 
      VIEW-AS TEXT 
     SIZE 2.4 BY .62
     FONT 4 NO-UNDO.

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
     SIZE 85 BY 7.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 3.1.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 3.43.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29 BY 1.91.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 2.86.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 11.19.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 113 BY 5.24.

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
          SIZE 23 BY 1
          BGCOLOR 15 FONT 4
     item.i-code AT ROW 1.48 COL 43 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "RM Stocked", "R":U,
"Estimated Mat'l", "E":U
          SIZE 37 BY .81
          FONT 4
     item.tax-rcpt AT ROW 1.48 COL 89
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
          SIZE 7 BY 1
          BGCOLOR 15 FONT 4
     costtype_descr AT ROW 3.86 COL 73.2 COLON-ALIGNED NO-LABEL
     u-ytd AT ROW 3.86 COL 121 COLON-ALIGNED
     item.est-dscr AT ROW 5.05 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 FONT 4
     item.procat AT ROW 5.05 COL 59 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     procat_dscr AT ROW 5.05 COL 73.2 COLON-ALIGNED NO-LABEL
     u-lyr AT ROW 5.05 COL 121 COLON-ALIGNED
     item.q-ptd AT ROW 6.95 COL 121 COLON-ALIGNED
          LABEL "PTD"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
          BGCOLOR 15 FONT 4
     item.flute AT ROW 7.19 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     item.shrink AT ROW 7.43 COL 62 COLON-ALIGNED
          LABEL "Shrink %"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     item.case-l AT ROW 7.43 COL 97 COLON-ALIGNED FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.4 BY 1
          BGCOLOR 15 FONT 4
     item.reg-no AT ROW 8.14 COL 17 COLON-ALIGNED
          LABEL "Test" FORMAT "X(6)"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     item.q-ytd AT ROW 8.14 COL 121 COLON-ALIGNED
          LABEL "YTD"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
          BGCOLOR 15 FONT 4
     item.case-w AT ROW 8.62 COL 97 COLON-ALIGNED FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.4 BY 1
          BGCOLOR 15 FONT 4
     item.s-wid AT ROW 8.86 COL 41 COLON-ALIGNED
          LABEL "Width" FORMAT ">>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 12.8 BY 1
          BGCOLOR 15 FONT 4
     item.s-dep AT ROW 8.86 COL 69 COLON-ALIGNED
          LABEL "Depth" FORMAT ">>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 12.8 BY 1
          BGCOLOR 15 FONT 4
     item.cal AT ROW 9.1 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     item.q-lyr AT ROW 9.33 COL 121 COLON-ALIGNED
          LABEL "LYR"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
          BGCOLOR 15 FONT 4
     item.s-len AT ROW 9.81 COL 41 COLON-ALIGNED
          LABEL "Length" FORMAT ">>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 12.8 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     item.density AT ROW 9.81 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     item.case-d AT ROW 9.81 COL 97 COLON-ALIGNED FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.4 BY 1
          BGCOLOR 15 FONT 4
     item.basis-w AT ROW 10.05 COL 17 COLON-ALIGNED
          LABEL "Weight/MSF" FORMAT ">>>9.99"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     item.r-wid AT ROW 10.76 COL 41 COLON-ALIGNED
          LABEL "Roll W" FORMAT ">>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 12.8 BY 1
          BGCOLOR 15 FONT 4
     item.color-1 AT ROW 10.76 COL 62 COLON-ALIGNED
          LABEL "Color"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 FONT 4
     fi_ect AT ROW 11 COL 17 COLON-ALIGNED
     item.avg-w AT ROW 11 COL 101 COLON-ALIGNED FORMAT ">,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     item.weight-100 AT ROW 11.24 COL 126 COLON-ALIGNED
          LABEL "Wgt/100"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 
     item.dept-name[1] AT ROW 12.19 COL 17 COLON-ALIGNED
          LABEL "Department"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[2] AT ROW 12.19 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[3] AT ROW 12.19 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[4] AT ROW 12.19 COL 35 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[5] AT ROW 12.19 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[6] AT ROW 12.19 COL 47 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[7] AT ROW 12.19 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[8] AT ROW 12.19 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[9] AT ROW 12.19 COL 65 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.dept-name[10] AT ROW 12.19 COL 71 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.box-case AT ROW 12.19 COL 103.4 COLON-ALIGNED
          LABEL "Boxes/Bundle" FORMAT ">>,>>9"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[1] AT ROW 13.38 COL 17 COLON-ALIGNED
          LABEL "Reduction %"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[2] AT ROW 13.38 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     item.speed%[3] AT ROW 13.38 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[4] AT ROW 13.38 COL 35 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[5] AT ROW 13.38 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[6] AT ROW 13.38 COL 47 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[7] AT ROW 13.38 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[8] AT ROW 13.38 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[9] AT ROW 13.38 COL 65 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.speed%[10] AT ROW 13.38 COL 71 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     item.case-pall AT ROW 13.38 COL 104.6 COLON-ALIGNED
          LABEL "Bundle/Pallet" FORMAT ">>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
          BGCOLOR 15 FONT 4
     item.sqin-lb AT ROW 13.62 COL 130 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     fi_cas-pal-w AT ROW 14.57 COL 99 COLON-ALIGNED HELP
          "Enter weight of this RM in Lbs."
     item.linin-lb AT ROW 14.81 COL 130 COLON-ALIGNED
          LABEL "Lin In/UOM"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     item.ink-type AT ROW 15.29 COL 18 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Ink", "I":U,
"Lacquer", "L":U,
"Ultra Violet", "U":U,
"Varnish", "V":U
          SIZE 67 BY .62
     fi_flute AT ROW 15.76 COL 99 COLON-ALIGNED HELP
          "Enter Flute Code for this Case/Bundle."
     item.press-type AT ROW 16 COL 18 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Flexo", "F":U,
"Gravure", "G":U,
"Letterpress", "L":U,
"Offset", "O":U,
"Silkscreen", "S":U
          SIZE 68 BY .62
     item.yield AT ROW 16.71 COL 16 COLON-ALIGNED
          LABEL "SI/Lb" FORMAT ">,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     item.min-lbs AT ROW 16.71 COL 46 COLON-ALIGNED
          LABEL "Min Lbs/Job"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     item.spare-char-1 AT ROW 16.71 COL 129 COLON-ALIGNED HELP
          "" WIDGET-ID 2
          LABEL "Freight Class"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     fi_reg-no AT ROW 16.95 COL 99 COLON-ALIGNED
     F1 AT ROW 2.67 COL 73 NO-LABEL
     F-2 AT ROW 3.86 COL 73 NO-LABEL
     F-3 AT ROW 5.05 COL 73 NO-LABEL
     group1-text AT ROW 6.48 COL 1 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     group4-text AT ROW 6.71 COL 87 COLON-ALIGNED NO-LABEL
     group6-text AT ROW 7.43 COL 43 NO-LABEL
     group5-text AT ROW 8 COL 34.4 NO-LABEL
     group3-text AT ROW 12.91 COL 116 COLON-ALIGNED NO-LABEL
     group2-text AT ROW 14.57 COL 2 COLON-ALIGNED NO-LABEL
     ink-type-label AT ROW 15.29 COL 4 COLON-ALIGNED NO-LABEL
     press-type-label AT ROW 16 COL 3 NO-LABEL
     "Consumption Cost" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.95 COL 116
     "Totals" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1 COL 117
          FONT 4
     "Consumption Qty" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 6.24 COL 116
     RECT-1 AT ROW 6.95 COL 1
     RECT-2 AT ROW 14.81 COL 2
     RECT-20 AT ROW 8.62 COL 32.4
     RECT-3 AT ROW 13.14 COL 116
     RECT-4 AT ROW 6.95 COL 87
     RECT-5 AT ROW 1.24 COL 1
     RECT-6 AT ROW 1.24 COL 115
     RECT-7 AT ROW 10.76 COL 115
     RECT-21 AT ROW 16.24 COL 114 WIDGET-ID 4
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
   Other Settings: PERSISTENT-ONLY
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
         HEIGHT             = 20.67
         WIDTH              = 143.8.
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

/* SETTINGS FOR FILL-IN item.basis-w IN FRAME F-Main
   EXP-LABEL                                                            */
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
   EXP-LABEL EXP-FORMAT                                                 */
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

/* SETTINGS FOR FILL-IN item.color-1 IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       item.color-1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN item.cost-type IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN costtype_descr IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       item.density:HIDDEN IN FRAME F-Main           = TRUE.

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

/* SETTINGS FOR FILL-IN F-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-3 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-3:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fi_cas-pal-w IN FRAME F-Main
   4                                                                    */
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

/* SETTINGS FOR FILL-IN fi_flute IN FRAME F-Main
   2                                                                    */
ASSIGN 
       fi_flute:HIDDEN IN FRAME F-Main           = TRUE
       fi_flute:PRIVATE-DATA IN FRAME F-Main     = 
                "group4".

/* SETTINGS FOR FILL-IN fi_mat-type IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi_reg-no IN FRAME F-Main
   2                                                                    */
ASSIGN 
       fi_reg-no:HIDDEN IN FRAME F-Main           = TRUE
       fi_reg-no:PRIVATE-DATA IN FRAME F-Main     = 
                "group4".

ASSIGN 
       item.flute:HIDDEN IN FRAME F-Main           = TRUE.

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

/* SETTINGS FOR FILL-IN group5-text IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       group5-text:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN group6-text IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       group6-text:HIDDEN IN FRAME F-Main           = TRUE.

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
   NO-ENABLE ALIGN-L                                                    */
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
/* SETTINGS FOR FILL-IN item.r-wid IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       item.r-wid:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       RECT-1:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       RECT-2:HIDDEN IN FRAME F-Main           = TRUE
       RECT-2:PRIVATE-DATA IN FRAME F-Main     = 
                "group2".

ASSIGN 
       RECT-20:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       RECT-3:HIDDEN IN FRAME F-Main           = TRUE
       RECT-3:PRIVATE-DATA IN FRAME F-Main     = 
                "group3".

ASSIGN 
       RECT-4:HIDDEN IN FRAME F-Main           = TRUE
       RECT-4:PRIVATE-DATA IN FRAME F-Main     = 
                "group4".

/* SETTINGS FOR FILL-IN item.reg-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       item.reg-no:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN item.s-dep IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       item.s-dep:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN item.s-len IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       item.s-len:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN item.s-wid IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       item.s-wid:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN item.shrink IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       item.shrink:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN item.spare-char-1 IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
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

ASSIGN 
       item.sqin-lb:HIDDEN IN FRAME F-Main           = TRUE
       item.sqin-lb:PRIVATE-DATA IN FRAME F-Main     = 
                "group3".

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
   DEF VAR ls-cur-val AS cha NO-UNDO.
   DEF VAR lv-eb-tmpid AS RECID NO-UNDO.  
   DEF VAR lv-handle AS HANDLE NO-UNDO.          
   DEF VAR char-val AS cha NO-UNDO.

   CASE FOCUS:NAME :
     WHEN "flute" OR WHEN "fi_flute" THEN DO:
           /*run est/l-flute.w (output char-val).  using reftable*/
           RUN windows/l-flute.w (gcompany,OUTPUT char-val).  /* using flute */
           IF char-val <> "" THEN 
              ASSIGN FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = ENTRY(1,char-val)
                     item.cal:screen-value = ENTRY(3,char-val).
           RETURN NO-APPLY.   
     END. 
     WHEN "reg-no" THEN DO: /* test*/
           RUN est/l-test.w 
              (gcompany, gloc, item.flute:screen-value /*, focus:screen-value in frame {&frame-name}*/ , OUTPUT char-val).
           IF char-val <> "" THEN 
              FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = entry(1,char-val).
           RETURN NO-APPLY.  
     END. 
     WHEN "dept-name" THEN DO: /* dept-name*/
           RUN windows/l-dept.w 
              ("",FOCUS:SCREEN-VALUE IN FRAME {&frame-name}, OUTPUT char-val).
           IF char-val <> "" THEN 
              FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = entry(1,char-val).
           RETURN NO-APPLY.  
     END.
     OTHERWISE DO:
           lv-handle = FOCUS:HANDLE.
           RUN applhelp.p.

           IF g_lookup-var <> "" THEN DO:
              lv-handle:SCREEN-VALUE = g_lookup-var.

           END.   /* g_lookup-var <> "" */
           APPLY "entry" TO lv-handle.
           RETURN NO-APPLY.

     END.  /* otherwise */
  END CASE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.cost-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.cost-type V-table-Win
ON LEAVE OF item.cost-type IN FRAME F-Main /* Cost Type */
DO:
  /*{methods/dispflds.i}*/

  IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
     NOT CAN-FIND(costtype WHERE costtype.company = gcompany
          AND costtype.loc = gloc
          AND costtype.cost-type = {&FIRST-EXTERNAL-TABLE}.cost-type:SCREEN-VALUE
          )
  THEN DO:
  {&methods/lValidateError.i YES}
     MESSAGE "Invalid Cost Type. Try Help."  VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
{&methods/lValidateError.i NO}
  END.


   FIND costtype
        WHERE costtype.company = gcompany
          AND costtype.loc = gloc
          AND costtype.cost-type = {&FIRST-EXTERNAL-TABLE}.cost-type:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    costtype_descr = IF NOT AVAILABLE costtype THEN ""
                     ELSE costtype.descr.
    DISPLAY costtype_descr WITH FRAME {&frame-name}.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.dept-name[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.dept-name[10] V-table-Win
ON LEAVE OF item.dept-name[10] IN FRAME F-Main /* Dept.[10] */
DO:
     IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = SELF:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99
                                 )
                     )
     THEN DO:
    {&methods/lValidateError.i YES}
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
     {&methods/lValidateError.i NO}
     END.                            

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.dept-name[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.dept-name[1] V-table-Win
ON LEAVE OF item.dept-name[1] IN FRAME F-Main /* Department */
DO:
     IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = SELF:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99
                                 )
                     )
     THEN DO:
     {&methods/lValidateError.i YES}
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
     {&methods/lValidateError.i NO}
     END.                            
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.dept-name[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.dept-name[2] V-table-Win
ON LEAVE OF item.dept-name[2] IN FRAME F-Main /* Dept.[2] */
DO:
       IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = SELF:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99
                                 )
                     )
     THEN DO:
     {&methods/lValidateError.i YES}
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
     {&methods/lValidateError.i NO}
     END.                            

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.dept-name[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.dept-name[3] V-table-Win
ON LEAVE OF item.dept-name[3] IN FRAME F-Main /* Dept.[3] */
DO:
       IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = SELF:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99      )
                     )
     THEN DO:
     {&methods/lValidateError.i YES}
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
     {&methods/lValidateError.i NO}
     END.                            

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.dept-name[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.dept-name[4] V-table-Win
ON LEAVE OF item.dept-name[4] IN FRAME F-Main /* Dept.[4] */
DO:
       IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = SELF:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99
                                 )
                     )
     THEN DO:
     {&methods/lValidateError.i YES}
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
     {&methods/lValidateError.i NO}
     END.                            

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.dept-name[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.dept-name[5] V-table-Win
ON LEAVE OF item.dept-name[5] IN FRAME F-Main /* Dept.[5] */
DO:
       IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = SELF:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99
                                 )
                     )
     THEN DO:
     {&methods/lValidateError.i YES}
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
     {&methods/lValidateError.i NO}
     END.                            

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.dept-name[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.dept-name[6] V-table-Win
ON LEAVE OF item.dept-name[6] IN FRAME F-Main /* Dept.[6] */
DO:
       IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = SELF:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99
                                 )
                     )
     THEN DO:
     {&methods/lValidateError.i YES}
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
     {&methods/lValidateError.i NO}
     END.                            

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.dept-name[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.dept-name[7] V-table-Win
ON LEAVE OF item.dept-name[7] IN FRAME F-Main /* Dept.[7] */
DO:
       IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = SELF:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99
                                 )
                     )
     THEN DO:
     {&methods/lValidateError.i YES}
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
     {&methods/lValidateError.i NO}
     END.                            

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.dept-name[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.dept-name[8] V-table-Win
ON LEAVE OF item.dept-name[8] IN FRAME F-Main /* Dept.[8] */
DO:
       IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = SELF:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99
                                 )
                     )
     THEN DO:
     {&methods/lValidateError.i YES}
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
     {&methods/lValidateError.i NO}
     END.                            

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.dept-name[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.dept-name[9] V-table-Win
ON LEAVE OF item.dept-name[9] IN FRAME F-Main /* Dept.[9] */
DO:
       IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = SELF:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99
                                 )
                     )
     THEN DO:
     {&methods/lValidateError.i YES}
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
     {&methods/lValidateError.i NO}
     END.                            

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_flute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_flute V-table-Win
ON LEAVE OF fi_flute IN FRAME F-Main /* Flute */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-flute (FOCUS, "C") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_flute V-table-Win
ON VALUE-CHANGED OF fi_flute IN FRAME F-Main /* Flute */
DO:
  item.flute:SCREEN-VALUE = fi_flute:SCREEN-VALUE.
  RUN new-flute.
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
    IF LOOKUP(fi_mat-type:SCREEN-VALUE IN FRAME {&frame-name},"B,P") = 0 THEN
      asi.item.spare-char-1:HIDDEN = TRUE.
    ELSE
      asi.item.spare-char-1:HIDDEN = FALSE.
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
      {cec/mattypes.i}  /* folding - custom/mattypes.i */
    END.
    &UNDEFINE mat-types-enable
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_reg-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_reg-no V-table-Win
ON LEAVE OF fi_reg-no IN FRAME F-Main /* Test */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-test (FOCUS, "C") NO-ERROR.
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
    RUN valid-flute (FOCUS, "B") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.flute V-table-Win
ON VALUE-CHANGED OF item.flute IN FRAME F-Main /* Flute */
DO:
  RUN new-flute.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.i-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.i-code V-table-Win
ON RETURN OF item.i-code IN FRAME F-Main /* Item Code */
DO:
   APPLY "tab" TO SELF.
   RETURN NO-APPLY.
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


&Scoped-define SELF-NAME item.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.procat V-table-Win
ON LEAVE OF item.procat IN FRAME F-Main /* Category */
DO:
  /*{methods/dispflds.i}*/
  FIND procat
        WHERE procat.company = gcompany
          AND procat.procat = {&FIRST-EXTERNAL-TABLE}.procat:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    procat_dscr = IF NOT AVAILABLE procat THEN ""
                  ELSE procat.dscr.
    DISPLAY procat_dscr WITH FRAME {&frame-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.reg-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.reg-no V-table-Win
ON LEAVE OF item.reg-no IN FRAME F-Main /* Test */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-test (FOCUS, "B") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME item.s-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.s-wid V-table-Win
ON LEAVE OF item.s-wid IN FRAME F-Main /* Test */
DO:

IF LASTKEY EQ -1 THEN Return .
{&methods/lValidateError.i YES}
    IF sys-ctrl.char-fld = "16th's" THEN DO:
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN DO:
            MESSAGE "Please enter 16ths decimal from .01 to .15."
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
    END.
    IF sys-ctrl.char-fld = "32nd's" THEN DO:
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) > v-16-or-32 
            THEN DO:
            MESSAGE "Please enter decimal from .01 to .32."
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

    END.
{&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.s-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.s-len V-table-Win
ON LEAVE OF item.s-len IN FRAME F-Main /* Test */
DO:

IF LASTKEY EQ -1 THEN Return .
{&methods/lValidateError.i YES}
    IF sys-ctrl.char-fld = "16th's" THEN DO:
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN DO:
            MESSAGE "Please enter 16ths decimal from .01 to .15."
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
    END.
    IF sys-ctrl.char-fld = "32nd's" THEN DO:
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) > v-16-or-32 
            THEN DO:
            MESSAGE "Please enter decimal from .01 to .32."
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

    END.
{&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.r-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.r-wid V-table-Win
ON LEAVE OF item.r-wid IN FRAME F-Main /* Test */
DO:
IF LASTKEY EQ -1 THEN Return .
{&methods/lValidateError.i YES}
    IF sys-ctrl.char-fld = "16th's" THEN DO:
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN DO:
            MESSAGE "Please enter 16ths decimal from .01 to .15."
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
    END.
    IF sys-ctrl.char-fld = "32nd's" THEN DO:
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) > v-16-or-32 
            THEN DO:
            MESSAGE "Please enter decimal from .01 to .32."
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

    END.
{&methods/lValidateError.i NO}
END.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item.s-dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item.s-dep V-table-Win
ON LEAVE OF item.s-dep IN FRAME F-Main /* Test */
DO:
IF LASTKEY EQ -1 THEN Return .
{&methods/lValidateError.i YES}
    IF sys-ctrl.char-fld = "16th's" THEN DO:
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN DO:
            MESSAGE "Please enter 16ths decimal from .01 to .15."
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
    END.
    IF sys-ctrl.char-fld = "32nd's" THEN DO:
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) > v-16-or-32 
            THEN DO:
            MESSAGE "Please enter decimal from .01 to .32."
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

    END.
{&methods/lValidateError.i NO}
END.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
SESSION:DATA-ENTRY-RETURN = YES.
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

  DO WITH FRAME {&FRAME-NAME}:
    ENABLE fi_mat-type.
    IF INDEX("DC",item.mat-type) GT 0 THEN ENABLE fi_cas-pal-w. 
    IF item.mat-type EQ "C" THEN ENABLE fi_flute fi_reg-no.
    IF INDEX("BAP",item.mat-type) GT 0 THEN ENABLE fi_ect.
    IF INDEX("1234",item.mat-type) GT 0 THEN ENABLE fi_ect.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE histblnkmsg V-table-Win 
PROCEDURE histblnkmsg :
/*------------------------------------------------------------------------------
  Purpose:     Just here for compatability
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-item FOR item.
  DEF BUFFER xstack-f FOR stack-flute.
  DEF BUFFER xstack-s FOR stack-size.
  DEF VAR v-i-no LIKE item.i-no NO-UNDO.
  DEF VAR cocode AS cha NO-UNDO.
  DEF VAR locode AS cha NO-UNDO.
  DEF BUFFER bf-e-item FOR e-item .
  DEF BUFFER bf-e-vend FOR e-item-vend.
  DEF BUFFER bf-item-bom FOR item-bom .
  DEF VAR ls-prev-i-no LIKE item.i-no NO-UNDO.
  DEF VAR lv-mat-type LIKE ITEM.mat-type NO-UNDO.
  DEF VAR lv-cas-pal-w LIKE ITEM.basis-w NO-UNDO.
  DEF VAR lv-ect AS DEC NO-UNDO.
  DEF VAR lv-code LIKE stack-flute.code NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  ls-prev-i-no = item.i-no.
  IF /*adm-new-record and YSK error in assign-statement for ? */ 
     fi_mat-type:screen-value IN FRAME {&frame-name} <> "I" 
  THEN DO:
     IF item.ink-type:screen-value = "?" OR item.ink-type:screen-value = ?
        THEN item.ink-type:screen-value = "I".
     IF item.press-type:screen-value = "?" OR item.press-type:screen-value = ?
        THEN item.press-type:screen-value = "F".
  END.

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

  SESSION:SET-WAIT-STATE("general").  

  ASSIGN cocode = gcompany
         locode = gloc
         fi_mat-type = lv-mat-type
         item.mat-type = lv-mat-type.

  IF adm-new-record AND item.mat-type = "D" THEN DO:  /* from rm/cpall.i */
     FIND FIRST bf-item WHERE bf-item.company = gcompany AND
                              bf-item.mat-type = "D" AND
                              bf-item.i-no <> item.i-no
                              USE-INDEX i-no /*mat-type*/ NO-LOCK NO-ERROR.    
     v-i-no = IF AVAIL bf-item THEN CAPS(bf-item.i-no) ELSE "".
     REPEAT:
       MESSAGE "Please enter pallet you wish to copy to the stacking info from:" 
               UPDATE v-i-no .
       FIND FIRST bf-item WHERE bf-item.company = gcompany AND
                                bf-item.i-no = v-i-no 
                                NO-LOCK NO-ERROR.    
       IF NOT AVAIL bf-item THEN  NEXT.
       ELSE LEAVE.
     END.  /* repeat v-i-no */
     SESSION:SET-WAIT-STATE("general").    

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
       BUFFER-COPY stack-flute EXCEPT rec_key TO xstack-f
       ASSIGN 
        xstack-f.code   = flute.code
        xstack-f.pallet = CAPS(item.i-no).
     END.

      FOR EACH stack-size WHERE stack-size.company EQ cocode
                            AND stack-size.loc     EQ locode
                            AND stack-size.pallet  EQ item.i-no:
          DELETE stack-size.
      END.
      FOR EACH stack-size WHERE stack-size.company EQ cocode
                            AND stack-size.loc     EQ locode
                            AND stack-size.pallet  EQ v-i-no
                            NO-LOCK:
          CREATE xstack-s.
          BUFFER-COPY stack-size EXCEPT rec_key TO xstack-s
          ASSIGN xstack-s.pallet = CAPS(item.i-no).
      END.                     
  END.  /* adm-new-record , item-type = "D" */

  IF adm-new-record AND NOT adm-adding-record THEN DO: /* copy */
         FIND bf-item WHERE bf-item.company = gcompany AND
                            bf-item.i-no = ls-prev-i-no
                            NO-LOCK NO-ERROR.
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
            item.pur-uom  = IF bf-item.pur-uom  EQ "" THEN "MSF"
                                                      ELSE bf-item.pur-uom
            item.cons-uom = IF bf-item.cons-uom EQ "" THEN "EA"
                                                      ELSE bf-item.cons-uom.
         END.
         FOR EACH e-item WHERE e-item.company = cocode AND
                                e-item.i-no = item.i-no:
               DELETE e-item.  /* delete rec if exists before create */                
         END.                             
         FOR EACH e-item WHERE e-item.company = cocode AND
                               e-item.i-no = bf-item.i-no:
             CREATE bf-e-item.
             BUFFER-COPY e-item EXCEPT e-item.i-no TO bf-e-item.
             ASSIGN bf-e-item.i-no = item.i-no.                  
         END.                             
         FOR EACH e-item-vend WHERE e-item-vend.company = cocode AND
                                    e-item-vend.i-no = item.i-no:
             DELETE e-item-vend.  /* delete rec if exists before create */                
         END.                             
         FOR EACH e-item-vend WHERE e-item-vend.company = cocode AND
                                    e-item-vend.i-no = bf-item.i-no:
             CREATE bf-e-vend.
             BUFFER-COPY e-item-vend EXCEPT e-item-vend.i-no TO bf-e-vend.
             ASSIGN bf-e-vend.i-no      = item.i-no
                    bf-e-vend.item-type = YES.                  
         END.
         FOR EACH item-bom WHERE item-bom.company = cocode AND
                                item-bom.parent-i = item.i-no:
               DELETE item-bom.  /* delete rec if exists before create */                
         END.                             
         FOR EACH item-bom WHERE item-bom.company = cocode AND
                               item-bom.parent-i = bf-item.i-no:
             CREATE bf-item-bom.
             BUFFER-COPY item-bom EXCEPT rec_key TO bf-item-bom
             ASSIGN bf-item-bom.parent-i = item.i-no.                  
         END. 
   END.  /* not adding-record */

   IF INDEX("BAP",item.mat-type) > 0 THEN DO:  
        {sys/inc/k16bb.i item.s-wid}
        {sys/inc/k16bb.i item.s-len}
        {sys/inc/k16bb.i item.r-wid}
        item.ect = lv-ect * (IF item.mat-type EQ "P" THEN 10000 ELSE 1).
  END.      
  ELSE IF INDEX("1234",item.mat-type) > 0 THEN DO:  
        {sys/inc/k16bb.i item.s-wid}
        {sys/inc/k16bb.i item.s-len}
        {sys/inc/k16bb.i item.s-dep}
        {sys/inc/k16bb.i item.r-wid}
        item.ect = lv-ect * (IF item.mat-type EQ "P" THEN 10000 ELSE 1).
  END.      
  ELSE IF INDEX("DC",item.mat-type) > 0 THEN DO:  
        {sys/inc/k16bb.i item.case-w}
        {sys/inc/k16bb.i item.case-l}
        {sys/inc/k16bb.i item.case-d}

        ASSIGN
         item.basis-w = lv-cas-pal-w
         item.flute   = fi_flute
         item.reg-no  = fi_reg-no.
  END. 

  IF adm-adding-record THEN DO:
     IF INDEX("MOXY789@",ITEM.mat-type) GT 0 THEN
        ASSIGN ITEM.cons-uom = "EA"
               ITEM.pur-uom = "EA".

  END.

  SESSION:SET-WAIT-STATE("").

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
 /* {methods/viewers/create/item.i} */
  ASSIGN item.company = gcompany
         item.loc = gloc
         item.industry = "2"
         item.mat-type = ""
         fi_mat-type:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = item.mat-type
         fi_cas-pal-w:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(item.basis-w)
         fi_flute:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = item.flute
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

    IF v-cecscrn-char EQ "Decimal" THEN
       ASSIGN
          item.s-wid:FORMAT = ">>,>>9.999999"
          item.s-len:FORMAT = ">>,>>9.999999"
          item.s-dep:FORMAT = ">>,>>9.999999"
          item.r-wid:FORMAT = ">>,>>9.999999"
          ITEM.case-l:FORMAT = ">>9.999999"
          ITEM.case-w:FORMAT = ">>9.999999"
          ITEM.case-d:FORMAT = ">>9.999999" .

    fi_mat-type = item.mat-type.

    IF LOOKUP(fi_mat-type,"B,P") = 0  THEN
      asi.item.spare-char-1:HIDDEN = TRUE.
    ELSE
      asi.item.spare-char-1:HIDDEN = FALSE.

    IF INDEX("BAP",fi_mat-type) GT 0 THEN DO:
      ASSIGN
       item.s-wid:screen-value = STRING({sys/inc/k16v.i item.s-wid})
       item.s-len:screen-value = STRING({sys/inc/k16v.i item.s-len})
       item.r-wid:screen-value = STRING({sys/inc/k16v.i item.r-wid}).

      IF ect-label EQ "" THEN
        ASSIGN
         ect-label  = fi_ect:LABEL
         ect-help   = fi_ect:HELP
         ect-format = fi_ect:FORMAT.

      IF fi_mat-type EQ "P" THEN
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
    END.

    ELSE
    IF INDEX("1234",fi_mat-type) GT 0 THEN DO:
      ASSIGN
       item.s-wid:screen-value = STRING({sys/inc/k16v.i item.s-wid})
       item.s-len:screen-value = STRING({sys/inc/k16v.i item.s-len})
       item.s-dep:screen-value = STRING({sys/inc/k16v.i item.s-dep})
       item.r-wid:screen-value = STRING({sys/inc/k16v.i item.r-wid}).

       IF ect-label EQ "" THEN
        ASSIGN
         ect-label  = fi_ect:LABEL
         ect-help   = fi_ect:HELP
         ect-format = fi_ect:FORMAT.

      IF fi_mat-type EQ "P" THEN
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

    END.

    ELSE
    IF INDEX("DC",fi_mat-type) GT 0 THEN DO:
      ASSIGN
       item.case-l:screen-value  = STRING({sys/inc/k16v.i item.case-l})
       item.case-w:screen-value  = STRING({sys/inc/k16v.i item.case-w})
       item.case-d:screen-value  = string({sys/inc/k16v.i item.case-d})
       fi_cas-pal-w:screen-value = string(item.basis-w)
       fi_flute:screen-value     = item.flute
       fi_reg-no:screen-value    = item.reg-no.
    END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* &Scoped-define mat-types-enable yes
  DO WITH FRAME {&FRAME-NAME}:
    {cec/mattypes.i}  /* folding - custom/mattypes.i enable fields group*/
  END.

  &UNDEFINE mat-types-enable
 */
/*  
  if avail item then do:
     if index("BAPW",fi_mat-type) > 0 then do:
        /* change color head1 head5 */
        if item.s-len <> 0 then display {sys/inc/k16.i item.s-len} with frame {&frame-name}.
        if item.s-wid <> 0 then display {sys/inc/k16.i item.s-wid} with frame {&frame-name}.
        if item.r-wid <> 0 then display {sys/inc/k16.i item.r-wid} with frame {&frame-name}.

     end.   
     else if index("IV1234",fi_mat-type) > 0 then do:
        /* change color head1 head5 */
        if item.s-len <> 0 then display {sys/inc/k16.i item.s-len} with frame {&frame-name}.
        if item.s-wid <> 0 then display {sys/inc/k16.i item.s-wid} with frame {&frame-name}.
        if item.r-wid <> 0 then display {sys/inc/k16.i item.r-wid} with frame {&frame-name}.

     end.
  end.
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR is-new-record AS LOG NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  is-new-record = adm-new-record.
  /*
  IF fi_mat-type:SCREEN-VALUE in frame {&frame-name} <> "B" THEN
      asi.item.spare-char-1:ENABLED = FALSE.
  ELSE
      asi.item.spare-char-1:ENABLED = TRUE.
      */
  /* ========== validate all inputs =============*/
  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-16th&32th NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
{&methods/lValidateError.i YES}
     IF item.dept-name[1]:screen-value IN FRAME {&frame-name} <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = item.dept-name[1]:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99 )
                     )
     THEN DO:
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO item.dept-name[1] IN FRAME {&frame-name}.
          RETURN NO-APPLY.
     END.                            
     IF item.dept-name[2]:screen-value IN FRAME {&frame-name} <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = item.dept-name[2]:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99 )
                     )
     THEN DO:
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO item.dept-name[2] IN FRAME {&frame-name}.
          RETURN NO-APPLY.
     END.                            
     IF item.dept-name[3]:screen-value IN FRAME {&frame-name} <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = item.dept-name[3]:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99 )
                     )
     THEN DO:
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO item.dept-name[3] IN FRAME {&frame-name}.
          RETURN NO-APPLY.
     END.                            
     IF item.dept-name[3]:screen-value IN FRAME {&frame-name} <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = item.dept-name[3]:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99 )
                     )
     THEN DO:
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO item.dept-name[3] IN FRAME {&frame-name}.
          RETURN NO-APPLY.
     END.                            
     IF item.dept-name[4]:screen-value IN FRAME {&frame-name} <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = item.dept-name[4]:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99 )
                     )
     THEN DO:
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO item.dept-name[4] IN FRAME {&frame-name}.
          RETURN NO-APPLY.
     END.                            
     IF item.dept-name[5]:screen-value IN FRAME {&frame-name} <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = item.dept-name[5]:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99 )
                     )
     THEN DO:
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO item.dept-name[5] IN FRAME {&frame-name}.
          RETURN NO-APPLY.
     END.                            
     IF item.dept-name[6]:screen-value IN FRAME {&frame-name} <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = item.dept-name[6]:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99 )
                     )
     THEN DO:
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO item.dept-name[6] IN FRAME {&frame-name}.
          RETURN NO-APPLY.
     END.                            
     IF item.dept-name[7]:screen-value IN FRAME {&frame-name} <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = item.dept-name[7]:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99 )
                     )
     THEN DO:
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO item.dept-name[7] IN FRAME {&frame-name}.
          RETURN NO-APPLY.
     END.                            
     IF item.dept-name[8]:screen-value IN FRAME {&frame-name} <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = item.dept-name[8]:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99 )
                     )
     THEN DO:
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO item.dept-name[8] IN FRAME {&frame-name}.
          RETURN NO-APPLY.
     END.                            
     IF item.dept-name[9]:screen-value IN FRAME {&frame-name} <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = item.dept-name[9]:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99 )
                     )
     THEN DO:
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO item.dept-name[9] IN FRAME {&frame-name}.
          RETURN NO-APPLY.
     END.                            
     IF item.dept-name[10]:screen-value IN FRAME {&frame-name} <> "" AND
        NOT CAN-FIND(FIRST dept WHERE dept.company = "" AND dept.code = item.dept-name[10]:screen-value
                            AND (dept.setup <> 99 OR dept.fc <> 99 OR dept.corr <> 99
                                 OR dept.therm <> 99 )
                     )
     THEN DO:
          MESSAGE "Invalid Department. Try Help." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO item.dept-name[10] IN FRAME {&frame-name}.
          RETURN NO-APPLY.
     END.
    {&methods/lValidateError.i NO}
     RUN valid-mat-type NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   {&methods/lValidateError.i YES}
     IF /*fi_mat-type:screen-value <> "" and*/
        NOT CAN-FIND(FIRST procat WHERE procat.company = gcompany AND
                                        procat.procat = item.procat:screen-value)
     THEN DO:
        MESSAGE "Invalid Product Category. Try Help."  VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO item.procat IN FRAME {&frame-name}.
        RETURN NO-APPLY.
     END.
     IF item.i-code:screen-value = "R" AND
        NOT CAN-FIND(costtype WHERE costtype.company = gcompany
          AND costtype.loc = gloc
          AND costtype.cost-type = {&FIRST-EXTERNAL-TABLE}.cost-type:SCREEN-VALUE
          )
     THEN DO:
        MESSAGE "Invalid Cost Type for real item. Try Help."  VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO item.cost-type IN FRAME {&frame-name}.
        RETURN NO-APPLY.
     END.
   {&methods/lValidateError.i NO}
   RUN valid-dimensions NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   DO WITH FRAME {&FRAME-NAME}:
     RUN valid-flute (item.flute:HANDLE, "B") NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-test (item.reg-no:HANDLE, "B") NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-flute (fi_flute:HANDLE, "C") NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-test (fi_reg-no:HANDLE, "C") NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.
   {&methods/lValidateError.i YES}
   IF INDEX("BAP",fi_mat-type:screen-value) > 0 THEN DO:
     IF dec(item.cal:screen-value) = 0 THEN DO:
        MESSAGE "Caliper is mandatory" VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO item.cal.
        RETURN NO-APPLY.
     END. 
     IF dec(item.basis-w:screen-value) = 0 THEN DO:
        MESSAGE "Basis Weight is mandatory" VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO item.basis-w.
        RETURN NO-APPLY.
     END. 
     IF dec(item.r-wid:screen-value) = 0 AND item.i-code:screen-value = "R" AND
        (dec(item.s-len:screen-value) = 0 OR dec(item.s-wid:screen-value) = 0 )
     THEN DO:
        MESSAGE "Dimensions are mandatory for Real Items!" VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO item.r-wid.
        RETURN NO-APPLY.
     END. 
   END.  /* "BAP" */
   ELSE IF INDEX("W",fi_mat-type:screen-value) > 0 THEN DO:
        IF dec(item.shrink:screen-value) = 0 THEN DO:
           MESSAGE "Pickup% is mandatory!" VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO item.shrink.
           RETURN NO-APPLY.
        END. 
   END.  /* "W"  */ 
   ELSE IF INDEX("GTS",fi_mat-type:screen-value) > 0 THEN DO:
        IF dec(item.linin-lb:screen-value) EQ 0 AND
           dec(item.sqin-lb:screen-value) EQ 0 THEN DO:
           MESSAGE "For " + (IF fi_mat-type:screen-value EQ "S" THEN "Stitch" 
                             ELSE IF fi_mat-type:screen-value EQ "T" THEN "Tape" 
                             ELSE "Glue") +
                    ", " +
                    trim(item.sqin-lb:label) " OR " +
                    trim(item.linin-lb:label) " must be entered..."
               VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO item.linin-lb.
           RETURN NO-APPLY.
        END. 
   END.  /* "GTS"  */ 
   ELSE IF INDEX("DC",fi_mat-type:screen-value) > 0 THEN DO:
        IF dec(item.box-case:screen-value) <> 0 AND
           dec(item.avg-w:screen-value) <> 0 THEN DO:
               MESSAGE "You can enter either Case Weight or Number of blanks per Case!" SKIP
                       "Only one field can be entered."
                       VIEW-AS ALERT-BOX ERROR.
               IF dec(item.box-case:screen-value) = 0 
                  THEN APPLY "entry" TO item.box-case.
                  ELSE APPLY "entry" TO item.avg-w.
               RETURN NO-APPLY.
        END. 
   END.
{&methods/lValidateError.i NO}
  /* ======== end validation =================== */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  RUN disable-item.



  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "record-source", OUTPUT char-hdl).
  IF is-new-record THEN do:
      RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(item)). 
  END.

  RUN dispatch IN WIDGET-HANDLE(char-hdl) ("row-changed").

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-flute V-table-Win 
PROCEDURE new-flute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DO WITH FRAME {&FRAME-NAME}:
     FIND FIRST flute
         WHERE flute.company EQ gcompany
           AND flute.code    EQ item.flute:SCREEN-VALUE
         NO-LOCK NO-ERROR.
     IF AVAIL flute THEN item.cal:SCREEN-VALUE = STRING(flute.thickness).
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query V-table-Win 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     NONE
  Parameters:  <none>
  Notes:       does nothing, here to support calls other objects use in different
               modules that actually have a repo-query routine
------------------------------------------------------------------------------*/

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
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.
  DEF INPUT PARAM ip-mat-type LIKE item.mat-type NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF fi_mat-type:SCREEN-VALUE EQ ip-mat-type                    AND
       ip-focus:SCREEN-VALUE NE ""                                AND
       NOT CAN-FIND(FIRST flute
                    WHERE flute.company EQ cocode
                      AND flute.code    EQ ip-focus:SCREEN-VALUE) THEN DO:
      MESSAGE ip-focus:LABEL + " invalid, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
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
  DEF INPUT PARAM ip-mat-type LIKE item.mat-type NO-UNDO.

  DEF VAR lv-flute LIKE item.flute NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE)
     lv-flute              = IF ip-mat-type EQ "C" THEN
                               fi_flute:SCREEN-VALUE
                             ELSE
                               item.flute:SCREEN-VALUE.

    IF fi_mat-type:SCREEN-VALUE EQ ip-mat-type AND
       lv-flute NE ""                          AND
       ip-focus:SCREEN-VALUE EQ ""             THEN DO:
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-16th&32th V-table-Win 
PROCEDURE valid-16th&32th :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
DO WITH FRAME {&FRAME-NAME}:
    IF sys-ctrl.char-fld = "16th's" THEN DO:
        IF DECIMAL(item.s-wid:screen-value) - trunc(DECIMAL(item.s-wid:screen-value),0) >= v-16-or-32 
            THEN DO:
            MESSAGE "Please enter 16ths decimal from .01 to .15."
                VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.
    END.
    IF sys-ctrl.char-fld = "32nd's" THEN DO:
        IF DECIMAL(item.s-wid:screen-value) - trunc(DECIMAL(item.s-wid:screen-value),0) > v-16-or-32 
            THEN DO:
            MESSAGE "Please enter decimal from .01 to .32."
                VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.

    END.

    IF sys-ctrl.char-fld = "16th's" THEN DO:
        IF DECIMAL(item.s-len:screen-value) - trunc(DECIMAL(item.s-len:screen-value),0) >= v-16-or-32 
            THEN DO:
            MESSAGE "Please enter 16ths decimal from .01 to .15."
                VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.
    END.
    IF sys-ctrl.char-fld = "32nd's" THEN DO:
        IF DECIMAL(item.s-len:screen-value) - trunc(DECIMAL(item.s-len:screen-value),0) > v-16-or-32 
            THEN DO:
            MESSAGE "Please enter decimal from .01 to .32."
                VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.
    END.


    IF sys-ctrl.char-fld = "16th's" THEN DO:
        IF DECIMAL(item.r-wid:screen-value) - trunc(DECIMAL(item.r-wid:screen-value),0) >= v-16-or-32 
            THEN DO:
            MESSAGE "Please enter 16ths decimal from .01 to .15."
                VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.
    END.
    IF sys-ctrl.char-fld = "32nd's" THEN DO:
        IF DECIMAL(item.r-wid:screen-value) - trunc(DECIMAL(item.r-wid:screen-value),0) > v-16-or-32 
            THEN DO:
            MESSAGE "Please enter decimal from .01 to .32."
                VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.            
    END.


    IF sys-ctrl.char-fld = "16th's" THEN DO:
        IF DECIMAL(item.s-dep:screen-value) - trunc(DECIMAL(item.s-dep:screen-value),0) >= v-16-or-32 
            THEN DO:
            MESSAGE "Please enter 16ths decimal from .01 to .15."
                VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.
    END.
    IF sys-ctrl.char-fld = "32nd's" THEN DO:
        IF DECIMAL(item.s-dep:screen-value) - trunc(DECIMAL(item.s-dep:screen-value),0) > v-16-or-32 
            THEN DO:
            MESSAGE "Please enter decimal from .01 to .32."
                VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.
    END.
END.

  {methods/lValidateError.i NO}
END PROCEDURE.
