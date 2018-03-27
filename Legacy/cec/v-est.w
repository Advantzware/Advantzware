&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: cec\v-est.w

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
/*on f3 help. */

/* Local Variable Definitions ---                                       */
    /* preprocedure for local-view */
&scoped-define LOC-VIEW EST-SPEC
&Scoped-define proc-enable proc-enable

def new shared temp-table formule field formule as dec extent 12.
DEF VAR char-val as cha no-undo.
DEF VAR lv-part-no-prev like eb.part-no no-undo.
&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.
DEF VAR k_frac as dec init 6.25 no-undo.
DEF VAR ll-auto-calc-selected as log no-undo.
DEF VAR lv-sqin as dec no-undo.
DEF VAR ll-warn as log no-undo.
DEF VAR ll-wid-len-warned AS LOG NO-UNDO.
DEF VAR ld-k-wid-array LIKE eb.k-wid-array2 NO-UNDO.
DEF VAR ld-k-len-array LIKE eb.k-len-array2 NO-UNDO.
DEF TEMP-TABLE tt-array NO-UNDO
    FIELD tt-dec AS DEC DECIMALS 6
    FIELD tt-type AS CHAR.

DEF VAR v-l-array AS DEC DECIMALS 6 EXTENT 30 NO-UNDO.

def new shared var cocode as cha no-undo.
def new shared var locode as cha no-undo.

{custom/globdefs.i}

assign cocode = g_company
       locode = g_loc.

def new shared buffer xest    for est.
def new shared buffer xef     for ef.
def new shared buffer xeb     for eb.

{cec/descalc.i "new"}
DEF VAR lv-foam as log no-undo.
DEF VAR ll-blank-size-changed AS LOG NO-UNDO.
DEF VAR lv-format AS cha NO-UNDO.
DEF VAR ll-new-shipto AS LOG NO-UNDO.

DEF VAR lv-industry like item.industry init "2".
DEF VAR lv-i-code   like item.i-code   init "B".
DEF VAR ll-style-is-valid AS LOG NO-UNDO.
DEF VAR ll-one-eb-on-ef AS LOG NO-UNDO.
DEF VAR lc-previous-values AS CHAR NO-UNDO.
DEF VAR lc-new-values AS CHAR NO-UNDO.

DEF VAR ls-qty AS CHAR NO-UNDO.
DEF VAR ls-per AS CHAR NO-UNDO.
DEF VAR ls-msf AS CHAR NO-UNDO.
DEF VAR lv-cad-path AS cha NO-UNDO.  /* cad file - boximage path for Fibre */
DEF VAR lv-cad-ext AS cha NO-UNDO.
DEF VAR lv-hld-wid like eb.t-wid no-undo.
DEF VAR lv-hld-len like eb.t-len no-undo.
DEF VAR lv-hold-flute LIKE eb.flute NO-UNDO.
DEF VAR lv-hold-test LIKE eb.test NO-UNDO.
DEF VAR dieFile AS CHARACTER NO-UNDO.
DEF VAR cadFile AS CHARACTER NO-UNDO.
DEF VAR lv-master-est-no LIKE eb.master-est-no NO-UNDO.
DEF BUFFER b-style FOR style.
DEF VAR v-count AS INT NO-UNDO.

DO TRANSACTION:
  {sys/inc/addprep.i}
  {sys/inc/ceroute.i C}
  {sys/inc/cestyle.i C}
  {sys/inc/cedicad.i C}
  {sys/inc/cefgitem.i}
  {sys/inc/graphic.i}
  {sys/inc/ecbrowse.i}
  {sys/inc/cepdies.i}
  {sys/inc/shiptorep.i}
END.

{sys/inc/f16to32.i}

IF v-cecscrn-dec THEN
DO:
   DEF TEMP-TABLE tt-64-dec NO-UNDO
       FIELD DEC AS DEC DECIMALS 6.

   DO v-count = 0 TO 63:
       CREATE tt-64-dec.
       tt-64-dec.DEC = v-count / 64.0.
       RELEASE tt-64-dec.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Corr

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES est eb est-qty ef
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est, eb, est-qty, ef.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS est.highlight eb.cust-no eb.ship-id est.csrUser eb.part-no ~
eb.stock-no eb.part-dscr1 eb.part-dscr2 eb.die-no ef.cad-image eb.sman ~
eb.comm eb.cad-no eb.plate-no eb.procat eb.spc-no eb.upc-no eb.style ~
eb.flute eb.test est.metric ef.board ef.brd-dscr eb.len eb.wid eb.dep ~
eb.adhesive eb.dust eb.fpanel eb.lock eb.gluelap eb.k-wid eb.k-len eb.tuck ~
eb.lin-in eb.t-wid eb.t-len eb.t-sqin eb.loc 
&Scoped-define ENABLED-TABLES est eb ef
&Scoped-define FIRST-ENABLED-TABLE est
&Scoped-define SECOND-ENABLED-TABLE eb
&Scoped-define THIRD-ENABLED-TABLE ef
&Scoped-Define ENABLED-OBJECTS tb-set bt-new-die btn_qty-msf bt-new-plate ~
btn_fgitem btn_style btn_board btn_cust RECT-18 RECT-19 RECT-23 RECT-24 
&Scoped-Define DISPLAYED-FIELDS est.est-no eb.form-no est.form-qty ~
eb.blank-no est.mod-date eb.ord-no est.ord-date est.highlight eb.cust-no ~
eb.ship-id eb.ship-name eb.ship-addr[1] eb.ship-addr[2] eb.ship-city ~
eb.ship-state eb.ship-zip est-qty.eqty eb.part-no eb.stock-no eb.part-dscr1 ~
eb.part-dscr2 eb.die-no ef.cad-image est.csrUser eb.sman eb.comm eb.cad-no eb.plate-no ~
eb.procat eb.spc-no eb.upc-no eb.style eb.flute eb.test est.metric ef.board ~
ef.brd-dscr eb.len eb.wid eb.dep eb.adhesive eb.dust eb.fpanel eb.lock ~
eb.gluelap eb.k-wid eb.k-len eb.tuck eb.lin-in eb.t-wid eb.t-len eb.t-sqin ~
eb.loc 
&Scoped-define DISPLAYED-TABLES est eb est-qty ef
&Scoped-define FIRST-DISPLAYED-TABLE est
&Scoped-define SECOND-DISPLAYED-TABLE eb
&Scoped-define THIRD-DISPLAYED-TABLE est-qty
&Scoped-define FOURTH-DISPLAYED-TABLE ef
&Scoped-Define DISPLAYED-OBJECTS tb-set fi_msf fi_per-set fi_from-est-no ~
fi_blank-qty sman_sname procat_desc style_dscr tab-inout 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS fi_from-est-no eb.ship-name ~
eb.ship-addr[1] eb.ship-addr[2] eb.ship-city eb.ship-state eb.ship-zip ~
style_dscr tab-inout est.metric 
&Scoped-define List-5 eb.len eb.wid eb.dep eb.adhesive eb.dust eb.fpanel ~
eb.lock eb.gluelap eb.k-wid eb.k-len eb.tuck eb.lin-in 

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ImageName V-table-Win 
FUNCTION ImageName RETURNS CHARACTER
  (ipImageFileName AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-new-die 
     LABEL "+" 
     SIZE 3.6 BY .95
     FONT 6.

DEFINE BUTTON bt-new-plate 
     LABEL "+" 
     SIZE 3 BY .95
     FONT 6.

DEFINE BUTTON btnCadLookup 
     IMAGE-UP FILE "Graphics/16x16/find.bmp":U
     LABEL "" 
     SIZE 4.4 BY 1.05.

DEFINE BUTTON btnDieLookup 
     IMAGE-UP FILE "Graphics/16x16/find.bmp":U
     LABEL "" 
     SIZE 4.4 BY 1.05.

DEFINE BUTTON btn_fgitem 
     LABEL "" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_style
     LABEL "" 
     SIZE 15 BY 1.

DEFINE BUTTON btn_board
     LABEL "" 
     SIZE 11 BY 1.

DEFINE BUTTON btn_cust
     LABEL "" 
     SIZE 9 BY 1.

DEFINE BUTTON btn_qty-msf 
     LABEL "" 
     SIZE 74 BY 1.

DEFINE VARIABLE fi_blank-qty AS INTEGER FORMAT ">9" INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE fi_from-est-no AS CHARACTER FORMAT "X(8)" 
     LABEL "From" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE fi_msf AS DECIMAL FORMAT "->>,>>9.999":U INITIAL 0 
     LABEL "MSF" 
     VIEW-AS FILL-IN 
     SIZE 15.4 BY 1 NO-UNDO.

DEFINE VARIABLE fi_per-set AS DECIMAL FORMAT "->>>9.9<<<":U INITIAL 0 
     LABEL "Qty/Set" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE procat_desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE sman_sname AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE style_dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE tab-inout AS CHARACTER FORMAT "X(3)":U 
     LABEL "Tab" 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 147 BY 7.14.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 153 BY 16.67.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 7.86.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 7.86.

DEFINE VARIABLE tb-set AS LOGICAL INITIAL no 
     LABEL "Set?" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 TOOLTIP "Indicates 2 boxes glued together shipping as one" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Corr
     tb-set AT ROW 16 COL 90 WIDGET-ID 12
     bt-new-die AT ROW 6.95 COL 80 WIDGET-ID 8
     btn_qty-msf AT ROW 2.67 COL 78
     fi_msf AT ROW 2.67 COL 134.6 COLON-ALIGNED
     fi_per-set AT ROW 2.67 COL 114 COLON-ALIGNED
     est.est-no AT ROW 1.24 COL 9.2 COLON-ALIGNED
          LABEL "Est #" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     fi_from-est-no AT ROW 1.24 COL 29 COLON-ALIGNED
     eb.form-no AT ROW 1.24 COL 46.8 COLON-ALIGNED
          LABEL "Frm"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     est.form-qty AT ROW 1.24 COL 54 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     eb.blank-no AT ROW 1.24 COL 64.2 COLON-ALIGNED
          LABEL "Blk"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     fi_blank-qty AT ROW 1.24 COL 71.6 COLON-ALIGNED NO-LABEL
     est.mod-date AT ROW 1.24 COL 84.4 COLON-ALIGNED
          LABEL "Mod"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.ord-no AT ROW 1.24 COL 115.4 COLON-ALIGNED
          LABEL "Last Ord#" FORMAT ">>>>>>>>"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     est.ord-date AT ROW 1.24 COL 135.6 COLON-ALIGNED
          LABEL "Date"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     est.highlight AT ROW 2.81 COL 2.4 HELP
          "Enter whether to drop slitter if MSF > minimum" WIDGET-ID 6
          LABEL "Yellow?"
          VIEW-AS TOGGLE-BOX
          SIZE 12.6 BY .81
     eb.cust-no AT ROW 2.67 COL 22.2 COLON-ALIGNED
          LABEL "Cust#"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          FONT 6
     eb.ship-id AT ROW 2.67 COL 49.2 COLON-ALIGNED HELP
          ""
          LABEL "Ship To"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          FONT 6
     eb.ship-name AT ROW 3.80 COL 22.2 COLON-ALIGNED
          LABEL "Company"
          VIEW-AS FILL-IN 
          SIZE 45.8 BY 1
          FONT 6
     eb.ship-addr[1] AT ROW 4.70 COL 22.2 COLON-ALIGNED
          LABEL "Address"
          VIEW-AS FILL-IN 
          SIZE 45.8 BY 1
     eb.ship-addr[2] AT ROW 5.55 COL 22.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 45.8 BY 1
     eb.ship-city AT ROW 6.45 COL 22.2 COLON-ALIGNED
          LABEL "City/State/Zip"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     eb.ship-state AT ROW 6.45 COL 45.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     eb.ship-zip AT ROW 6.45 COL 52.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.8 BY 1
     est.csrUser_id AT ROW 7.30 COL 22 COLON-ALIGNED
          LABEL "CSR"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     est-qty.eqty AT ROW 2.67 COL 90 COLON-ALIGNED
          LABEL "Quantity" FORMAT ">>>>>>>"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.part-no AT ROW 3.86 COL 90 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          FONT 6
     eb.stock-no AT ROW 3.86 COL 124 COLON-ALIGNED
          LABEL "FG Item#"
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
          FONT 6
     eb.part-dscr1 AT ROW 5.05 COL 90 COLON-ALIGNED
          LABEL "Item Name"
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
          FONT 6
     eb.part-dscr2 AT ROW 6 COL 90 COLON-ALIGNED
          LABEL "Description"
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Corr
     btnDieLookup AT ROW 6.95 COL 75
     eb.die-no AT ROW 6.95 COL 90 COLON-ALIGNED HELP
          ""
          LABEL "Die #" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          FONT 6
     ef.cad-image AT ROW 6.95 COL 127 COLON-ALIGNED HELP
          "Filename of the Die image"
          LABEL "Image" FORMAT "x(80)"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     eb.sman AT ROW 8.14 COL 21 COLON-ALIGNED
          LABEL "Sales Rep"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     sman_sname AT ROW 8.14 COL 28 COLON-ALIGNED NO-LABEL
     eb.comm AT ROW 8.14 COL 61 COLON-ALIGNED
          LABEL "%"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     btnCadLookup AT ROW 7.91 COL 75
     eb.cad-no AT ROW 7.91 COL 114 RIGHT-ALIGNED
          LABEL "CAD#"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          FONT 6
     eb.plate-no AT ROW 7.95 COL 127 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          FONT 6
     eb.procat AT ROW 9.14 COL 17.8 COLON-ALIGNED
          LABEL "FG Category" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          FONT 6
     procat_desc AT ROW 9.14 COL 27.8 COLON-ALIGNED NO-LABEL
     eb.spc-no AT ROW 8.86 COL 90 COLON-ALIGNED
          LABEL "SPC/QC #"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     eb.upc-no AT ROW 8.91 COL 121 COLON-ALIGNED
          LABEL "UPC#" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 29 BY 1
          FONT 6
     eb.style AT ROW 10.52 COL 21 COLON-ALIGNED
          LABEL "Style Code"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     style_dscr AT ROW 10.52 COL 35 COLON-ALIGNED NO-LABEL
     eb.flute AT ROW 10.52 COL 84 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     eb.test AT ROW 10.52 COL 98 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.2 BY 1
     tab-inout AT ROW 10.52 COL 117 COLON-ALIGNED
     est.metric AT ROW 10.52 COL 137 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     ef.board AT ROW 11.71 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     ef.brd-dscr AT ROW 11.71 COL 45 COLON-ALIGNED NO-LABEL FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 61 BY 1
     eb.len AT ROW 12.91 COL 26 COLON-ALIGNED
          LABEL "Length" FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.wid AT ROW 12.91 COL 61 COLON-ALIGNED
          LABEL "Width" FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.dep AT ROW 12.91 COL 88 COLON-ALIGNED
          LABEL "Depth" FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.adhesive AT ROW 12.91 COL 127 COLON-ALIGNED
          LABEL "Joint Material"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     eb.dust AT ROW 13.86 COL 26 COLON-ALIGNED
          LABEL "Top/Dust Flap" FORMAT "->>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.fpanel AT ROW 13.86 COL 61 COLON-ALIGNED
          LABEL "Bottom Flap" FORMAT "->>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Corr
     eb.lock AT ROW 13.86 COL 88 COLON-ALIGNED
          LABEL "Lock Tab" FORMAT "->>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.gluelap AT ROW 13.86 COL 127 COLON-ALIGNED
          LABEL "Joint Tab Width" FORMAT "->>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.k-wid AT ROW 14.81 COL 26 COLON-ALIGNED
          LABEL "Scores on Width" FORMAT "->>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.k-len AT ROW 14.81 COL 61 COLON-ALIGNED
          LABEL "Scores on Length" FORMAT "->>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.tuck AT ROW 14.81 COL 88 COLON-ALIGNED
          LABEL "Tuck" FORMAT "->>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.lin-in AT ROW 14.81 COL 127 COLON-ALIGNED
          LABEL "Joint Length" FORMAT "->>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.t-wid AT ROW 16 COL 26 COLON-ALIGNED
          LABEL "Blank Width" FORMAT ">>>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.t-len AT ROW 16 COL 61 COLON-ALIGNED
          LABEL "Blank Length" FORMAT ">>>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.t-sqin AT ROW 16 COL 127 COLON-ALIGNED
          LABEL "Blank Square Feet" FORMAT ">>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     bt-new-plate AT ROW 7.91 COL 115.6 WIDGET-ID 10
     eb.loc AT ROW 9.19 COL 62.2 COLON-ALIGNED WIDGET-ID 14
          LABEL "W"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1     
     btn_fgitem AT ROW 3.81 COL 114 WIDGET-ID 16
     btn_style AT ROW 10.52 COL 8 WIDGET-ID 16
     btn_board AT ROW 11.71 COL 12 WIDGET-ID 16
     btn_cust AT ROW 2.67 COL 15 WIDGET-ID 16
     "of" VIEW-AS TEXT
          SIZE 3 BY .95 AT ROW 1.24 COL 70.4
     "of" VIEW-AS TEXT
          SIZE 3 BY .95 AT ROW 1.24 COL 53
     RECT-18 AT ROW 10.29 COL 2
     RECT-19 AT ROW 1 COL 1
     RECT-23 AT ROW 2.43 COL 73
     RECT-24 AT ROW 2.43 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.est,ASI.eb,ASI.est-qty,ASI.ef
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
         HEIGHT             = 19
         WIDTH              = 153.2.
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
/* SETTINGS FOR FRAME Corr
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME Corr:SCROLLABLE       = FALSE
       FRAME Corr:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN eb.adhesive IN FRAME Corr
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN eb.blank-no IN FRAME Corr
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ef.brd-dscr IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR BUTTON btnCadLookup IN FRAME Corr
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnDieLookup IN FRAME Corr
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ef.cad-image IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN eb.cad-no IN FRAME Corr
   ALIGN-R EXP-LABEL                                                    */
/* SETTINGS FOR FILL-IN eb.comm IN FRAME Corr
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.cust-no IN FRAME Corr
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.dep IN FRAME Corr
   5 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN eb.die-no IN FRAME Corr
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR TOGGLE-BOX est.highlight IN FRAME Corr
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN eb.dust IN FRAME Corr
   5 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN est.csrUser_id IN FRAME Corr
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN est-qty.eqty IN FRAME Corr
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
ASSIGN 
       est-qty.eqty:HIDDEN IN FRAME Corr           = TRUE.

/* SETTINGS FOR FILL-IN est.est-no IN FRAME Corr
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN fi_blank-qty IN FRAME Corr
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_from-est-no IN FRAME Corr
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_msf IN FRAME Corr
   NO-ENABLE                                                            */
ASSIGN 
       fi_msf:HIDDEN IN FRAME Corr           = TRUE.

/* SETTINGS FOR FILL-IN fi_per-set IN FRAME Corr
   NO-ENABLE                                                            */
ASSIGN 
       fi_per-set:HIDDEN IN FRAME Corr           = TRUE.

/* SETTINGS FOR FILL-IN eb.form-no IN FRAME Corr
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN est.form-qty IN FRAME Corr
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN eb.fpanel IN FRAME Corr
   5 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN eb.gluelap IN FRAME Corr
   5 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN eb.k-len IN FRAME Corr
   5 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN eb.k-wid IN FRAME Corr
   5 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN eb.len IN FRAME Corr
   5 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN eb.lin-in IN FRAME Corr
   5 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN eb.loc IN FRAME Corr
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.lock IN FRAME Corr
   5 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN est.metric IN FRAME Corr
   2                                                                    */
/* SETTINGS FOR FILL-IN est.mod-date IN FRAME Corr
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN est.ord-date IN FRAME Corr
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN eb.ord-no IN FRAME Corr
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN eb.part-dscr1 IN FRAME Corr
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.part-dscr2 IN FRAME Corr
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.procat IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN procat_desc IN FRAME Corr
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.ship-addr[1] IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.ship-addr[2] IN FRAME Corr
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.ship-city IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.ship-id IN FRAME Corr
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN eb.ship-name IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.ship-state IN FRAME Corr
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.ship-zip IN FRAME Corr
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.sman IN FRAME Corr
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN sman_sname IN FRAME Corr
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.spc-no IN FRAME Corr
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.stock-no IN FRAME Corr
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style_dscr IN FRAME Corr
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.t-len IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN eb.t-sqin IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN eb.t-wid IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN tab-inout IN FRAME Corr
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.tuck IN FRAME Corr
   5 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN eb.upc-no IN FRAME Corr
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.wid IN FRAME Corr
   5 EXP-LABEL EXP-FORMAT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Corr
/* Query rebuild information for FRAME Corr
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME Corr */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Corr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Corr V-table-Win
ON HELP OF FRAME Corr
DO:
   DEF VAR lv-handle as widget-handle no-undo.
   DEF VAR ls-cur-val as cha no-undo.
   DEF VAR lv-eb-tmpid as recid no-undo.
   DEF VAR lv-prep-type AS cha NO-UNDO.
   DEF VAR lv-rowid as rowid no-undo.
   DEF VAR li AS INT NO-UNDO.
   DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.


   lw-focus = FOCUS.

   case lw-focus:name :
        when "sman" then do:
             run windows/l-sman.w (cocode, output char-val).
             if char-val <> "" and entry(1,char-val) ne lw-focus:screen-value then do:
                lw-focus:screen-value = entry(1,char-val).
                run new-sman.
             end.          
        end.
        when "req-date" or when "due-date" then do:
             /*{methods/calendar.i}  run on self's help trigger*/

        end.
        when "part-no" then do: 
           run est/l-ebrfqP.w (cocode, locode, lw-focus:screen-value, output lv-eb-tmpid) .
           if lv-eb-tmpid = ?  then return no-apply.
           find xeb where recid(eb) = lv-eb-tmpid no-lock no-error.
           find xef where xef.company = xeb.company and
                          xef.est-no = xeb.est-no  
               no-lock no-error.
           run copy-from-est.
           /*run copy-from-est2.*/
           lv-part-no-prev = eb.part-no.
      end.
      when "stock-no" then do:
        /* run windows/l-itemfg.w  (cocode, output char-val). */
           run est/l-ebrfq.w (cocode, locode,lw-focus:screen-value, output lv-eb-tmpid) .
           if lv-eb-tmpid = ?  then return.
           find xeb where recid(xeb) = lv-eb-tmpid no-lock no-error.
           find xef of xeb where xef.company = xeb.company and
                                 xef.est-no = xeb.est-no
                          no-lock no-error.

           run copy-from-est.
           /*run copy-from-est2. */
      end.
      when "style" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-stylec.w (cocode,ls-cur-val, output char-val).
           if char-val <> "" then do:
              assign lw-focus:screen-value =  entry(1,char-val)
                     style_dscr:screen-value = entry(2,char-val) 
                  /*   ef.board:screen-value in frame {&frame-name} = if ef.board:screen-value = "" then entry(3,char-val)  else ef.board:screen-value /* style.material[1] */
                  */  
                     .
              /*find style where style.company = cocode and
                               style.style = ef.board:screen-value in frame {&frame-name}
                         no-lock no-error.            
              if avail style then 
                 assign ef.brd-dscr:screen-value in frame {&frame-name} = style.dscr
                        .
              */     
              RUN new-style.
           end.  
      end.
      when "procat" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-fgcat.w (cocode,ls-cur-val,output char-val).
           if char-val ne "" and ls-cur-val ne entry(1,char-val) then do:
              eb.procat:screen-value in frame {&frame-name} = entry(1,char-val).
              RUN new-procat.
           END.
       end.
       when "flute" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-flute.w (cocode,output char-val).
           if char-val <> "" then
              lw-focus:screen-value =  entry(1,char-val).
       end.
       when "test" then do:
           ls-cur-val = eb.flute:screen-value.
           run windows/l-test.w (cocode,locode,ls-cur-val,output char-val).
           if char-val <> "" then
              lw-focus:screen-value =  entry(1,char-val).       
       end.
       when "Board" then do:
           DEF VAR lv-ind like style.industry no-undo.
           ls-cur-val = lw-focus:screen-value.
           find style where style.company = cocode and
                            style.style = eb.style:screen-value in frame {&frame-name}
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           if avail style and style.type = "f" then DO: /* foam */
             run windows/l-boardf.w (cocode,lv-ind,ls-cur-val,output char-val).
             if char-val <> "" then
               ASSIGN lw-focus:screen-value =  entry(1,char-val)
                      ef.brd-dscr:screen-value in frame {&frame-name} =  entry(3,char-val).       
           END.
           else do:
             run windows/l-board1.w (eb.company,lv-ind,lw-focus:screen-value, output lv-rowid).
             FIND FIRST ITEM WHERE ROWID(item) EQ lv-rowid NO-LOCK NO-ERROR.
             IF AVAIL ITEM THEN
                assign ef.board:screen-value in frame {&frame-name} = item.i-no
                       ef.brd-dscr:screen-value in frame {&frame-name} = item.i-name.  
           END.
       end.
       when "cust-no" then do:
           ls-cur-val = eb.cust-no:screen-value.
           run windows/l-cust.w (cocode,ls-cur-val, output char-val).
           if char-val <> "" then do:
              eb.cust-no:screen-value in frame {&frame-name} =  ENTRY(1,char-val).
              RUN csr-display .
              find first shipto where shipto.company = cocode
                                  and shipto.cust-no = eb.cust-no:screen-value
                                  no-lock no-error.
               eb.ship-id:screen-value = if avail shipto then shipto.ship-id else "".
                if v-shiptorep-log AND AVAIL shipto AND shipto.spare-char-1 <> "" THEN do:   /* task 05301401 */
                   eb.sman:SCREEN-VALUE = shipto.spare-char-1 .
                   run new-sman.
               END.
           end.
       end.  /* cust-no*/
       when "plate-no" or when "die-no" then do:
           /*run windows/l-matpr.w  (cocode,lw-focus:screen-value, output char-val).  */
           lv-prep-type = IF lw-focus:NAME = "Plate-no" THEN "P" ELSE "D,F,R".
           RUN windows/l-diepl.w (cocode,lv-prep-type,lw-focus:screen-value, output char-val). 
           if char-val <> "" then 
              lw-focus:screen-value = entry(1,char-val).
       end.
       when "cad-no" then do:
           /*IF lv-cad-path <> "" THEN DO:
               DEF VAR ls-filename as cha no-undo.
               DEF VAR ls-cadimage AS cha NO-UNDO.
               DEF VAR ll-ok as log no-undo.

               system-dialog get-file ls-filename 
                 title "Select Image File to insert"
                 filters "JPG Files    (*.jpg)" "*.jpg",
                         "Bitmap files (*.bmp)" "*.bmp",
                         "JPEG Files   (*.jpeg)" "*.jpeg",
                         "TIF Files    (*.tif)" "*.tif",
                         "All Files    (*.*) " "*.*"
                 initial-dir lv-cad-path /*"cadimage\"*/
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.

                 IF ll-ok THEN do:
                    DO li = 1 TO LENGTH(ls-filename):
                       ls-cadimage = ls-cadimage + SUBSTRING(ls-filename,li,1).
                       IF SUBSTRING(ls-filename,li,1) = "/" OR SUBSTRING(ls-filename,li,1) = "\" 
                          THEN ls-cadimage = "".
                    END.
                    IF INDEX(ls-cadimage,".") > 0 THEN
                         ASSIGN lv-cad-ext = SUBSTRING(ls-cadimage,INDEX(ls-cadimage,"."))
                                ls-cadimage = substring(ls-cadimage,1,INDEX(ls-cadimage,".") - 1).
                    lw-focus:screen-value = ls-cadimage.
                 END.
           END.
           ELSE*/ DO:
                RUN windows/l-itemfc.w  (cocode,eb.cad-no:screen-value, output char-val). 
                if char-val <> "" then 
                   eb.cad-no:screen-value in frame {&frame-name} = entry(1,char-val).
           END.
       end.
       when "upc-no" then do:     
           run windows/l-itemfu.w  (cocode,lw-focus:screen-value, output char-val). 
           if char-val <> "" then 
              lw-focus:screen-value = entry(1,char-val).
       end.
       when "spc-no" then do:
           run windows/l-itemfs.w  (cocode,lw-focus:screen-value, output char-val). 
           if char-val <> "" then 
              lw-focus:screen-value = entry(1,char-val).
       end.  
       when "ship-id" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-shipto.w (cocode,locode,eb.cust-no:screen-value,ls-cur-val, output char-val).
           if char-val <> "" then DO:
              lw-focus:screen-value =  ENTRY(1,char-val).
              RUN display-shipto.
              find first shipto where shipto.company = cocode
                                  and shipto.cust-no = eb.cust-no:screen-value
                                  AND shipto.ship-id = lw-focus:SCREEN-VALUE
                                  no-lock no-error.
               if v-shiptorep-log AND AVAIL shipto AND shipto.spare-char-1 <> "" THEN do:   /* task 05301401 */
                   eb.sman:SCREEN-VALUE = shipto.spare-char-1 .
                   run new-sman.
               END.
              ll-new-shipto = NO.
           END.
       end.  /* ship-id*/
       WHEN "adhesive" THEN DO:
           RUN windows/l-item.w (cocode,"","G,S,T",lw-focus:SCREEN-VALUE,OUTPUT char-val).
           IF char-val <> "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
       END.
       WHEN "loc" THEN DO:
         run windows/l-loc.w (cocode, lw-focus:SCREEN-VALUE, output char-val).
         if char-val <> "" then 
            assign lw-focus:SCREEN-VALUE = entry(1,char-val).
       END.
       when "csrUser_id" then do:
         run windows/l-users.w (est.csrUser_id:SCREEN-VALUE in frame {&frame-name}, output char-val).
           if char-val <> "" then 
              assign est.csrUser_id:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.
       END.
  end case.
  return no-apply.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.adhesive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.adhesive V-table-Win
ON LEAVE OF eb.adhesive IN FRAME Corr /* Joint Material */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-adhesive (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
    IF ll-auto-calc-selected AND {&self-name} <> self:SCREEN-VALUE 
    THEN ll-style-is-valid = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.board V-table-Win
ON ENTRY OF ef.board IN FRAME Corr /* Board */
DO:
  IF NOT lv-foam THEN
      RUN check-flute-test-change.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME est.csrUser_id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est.csrUser_id V-table-Win
ON LEAVE OF est.csrUser_id IN FRAME Corr /* Type */
DO:
  
  IF LASTKEY <> -1 THEN DO:
     RUN valid-custcsr NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.board V-table-Win
ON LEAVE OF ef.board IN FRAME Corr /* Board */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-board NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN new-board.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-new-die
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-new-die V-table-Win
ON CHOOSE OF bt-new-die IN FRAME Corr /* + */
DO:

    DEF VAR v-code AS CHAR NO-UNDO.
    DEF VAR ip-type AS INT NO-UNDO.
    DEF VAR lv-code AS CHAR NO-UNDO.

    repeat:
     MESSAGE TRIM(IF addprep-chr EQ "" THEN
                    "Enter 'R'otary Die, 'F'lat Die, 'M'=Foam " /*, " +
                    "or leave blank" */
                  ELSE addprep-chr) + ":" UPDATE lv-code.
     if index("RFM",lv-code) eq 0 and lv-code ne "" then undo, retry.
     IF lv-code GT "" THEN
       lv-code = "!" + caps(lv-code).
     leave.
    end.
    IF lv-code EQ "" THEN
        RETURN NO-APPLY.
    ip-type = 1.

    RUN windows/prepfly.w (INPUT-OUTPUT lv-code, ROWID(eb), ip-type) NO-ERROR.
    IF lv-code GT "" THEN
        ASSIGN eb.die-no:SCREEN-VALUE   = lv-code
               v-code = lv-code /* just in cse this is needed somewhere */.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-new-plate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-new-plate V-table-Win
ON CHOOSE OF bt-new-plate IN FRAME Corr /* + */
DO:
    DEF VAR v-code AS CHAR NO-UNDO.
    DEF VAR ip-type AS INT NO-UNDO.
    DEF VAR lv-code AS CHAR NO-UNDO.

    repeat:
/*      MESSAGE TRIM(IF addprep-chr EQ "" THEN                                              */
/*                     "Enter 'R'otary Die, 'F'lat Die, 'P'rinting Die/Plate, 'A'=Foam, " + */
/*                     "or leave blank"                                                     */
/*                   ELSE addprep-chr) + ":" UPDATE v-code.                                 */
/*      if index("RFPA",v-code) eq 0 and v-code ne "" then undo, retry.                     */
     lv-code = "!" + caps("P").
     leave.
    end.
    ip-type = 2.
    RUN windows/prepfly.w (INPUT-OUTPUT lv-code, ROWID(eb), ip-type) NO-ERROR.
    IF lv-code GT "" THEN
        ASSIGN eb.plate-no:SCREEN-VALUE   = lv-code
               v-code = lv-code /* just in case this is needed somewhere */.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCadLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCadLookup V-table-Win
ON CHOOSE OF btnCadLookup IN FRAME Corr
DO:
  DEF VAR initDir AS CHARACTER NO-UNDO.
  DEF VAR okClicked AS LOGICAL NO-UNDO.

  FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ cocode
                                AND sys-ctrl.name EQ 'CADFILE' NO-ERROR.
  IF NOT AVAILABLE sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN sys-ctrl.company = cocode
           sys-ctrl.name = 'CADFILE'
           sys-ctrl.descrip = 'Dictate the location of the cad image to search.'
           sys-ctrl.char-fld = '.\'.
    FIND CURRENT sys-ctrl NO-LOCK.
  END.
  ASSIGN
    initDir = sys-ctrl.char-fld
    cadFile = ''.

  SYSTEM-DIALOG GET-FILE cadfile 
                TITLE 'Select Image File to insert'
                FILTERS 'JPG Files    (*.jpg)' '*.jpg',
                        'Bitmap files (*.bmp)' '*.bmp',
                        'JPEG Files   (*.jpeg)' '*.jpeg',
                        'TIF Files    (*.tif)' '*.tif',
                        'All Files    (*.*) ' '*.*'
                INITIAL-DIR initDir
                MUST-EXIST USE-FILENAME UPDATE okClicked.
  IF okClicked THEN
  eb.cad-no:SCREEN-VALUE = imageName(cadfile).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDieLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDieLookup V-table-Win
ON CHOOSE OF btnDieLookup IN FRAME Corr
DO:
  DEF VAR initDir AS CHARACTER NO-UNDO.
  DEF VAR okClicked AS LOGICAL NO-UNDO.
  DEF VAR v-intval AS INT NO-UNDO.

  FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ cocode
                                AND sys-ctrl.name EQ 'DIEFILE' NO-ERROR.
  IF NOT AVAILABLE sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN sys-ctrl.company = cocode
           sys-ctrl.name    = 'DIEFILE'
           sys-ctrl.descrip = 'Dictate the location of the die image to search.'
           sys-ctrl.char-fld = '.\'.
    FIND CURRENT sys-ctrl NO-LOCK.
  END.
  ASSIGN
    initDir = sys-ctrl.char-fld
    dieFile = ''
    v-intval = sys-ctrl.int-fld  .

  IF v-intval = 0 THEN
     SYSTEM-DIALOG GET-FILE dieFile 
                TITLE 'Select Image File to insert'
                FILTERS 'JPG Files    (*.jpg)' '*.jpg',
                        'PDF Files    (*.pdf)' '*.pdf',
                        'Bitmap files (*.bmp)' '*.bmp',
                        'JPEG Files   (*.jpeg)' '*.jpeg',
                        'TIF Files    (*.tif)' '*.tif',
                        'All Files    (*.*) ' '*.*'
                INITIAL-DIR initDir
                MUST-EXIST USE-FILENAME UPDATE okClicked.
  ELSE IF v-intval = 1 THEN
      SYSTEM-DIALOG GET-FILE dieFile 
                TITLE 'Select Image File to insert'
                FILTERS 'Bitmap files (*.bmp)' '*.bmp',
                        'PDF Files    (*.pdf)' '*.pdf',
                        'JPG Files    (*.jpg)' '*.jpg',
                        'JPEG Files   (*.jpeg)' '*.jpeg',
                        'TIF Files    (*.tif)' '*.tif',
                        'All Files    (*.*) ' '*.*'
                INITIAL-DIR initDir
                MUST-EXIST USE-FILENAME UPDATE okClicked.

  ELSE SYSTEM-DIALOG GET-FILE dieFile 
                TITLE 'Select Image File to insert'
                FILTERS 'PDF Files    (*.pdf)' '*.pdf',
                        'JPG Files    (*.jpg)' '*.jpg',
                        'Bitmap files (*.bmp)' '*.bmp',
                        'JPEG Files   (*.jpeg)' '*.jpeg',
                        'TIF Files    (*.tif)' '*.tif',
                        'All Files    (*.*) ' '*.*'
                INITIAL-DIR initDir
                MUST-EXIST USE-FILENAME UPDATE okClicked.

  IF okClicked THEN
     ASSIGN eb.die-no:SCREEN-VALUE = IF eb.die-no:SCREEN-VALUE = "" THEN imageName(dieFile) ELSE eb.die-no:SCREEN-VALUE
            ef.cad-image:SCREEN-VALUE = imageName(dieFile).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_fgitem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_fgitem V-table-Win
ON CHOOSE OF btn_fgitem IN FRAME Corr
DO:
  IF AVAIL eb THEN
   FIND FIRST itemfg WHERE itemfg.company  = cocode
       AND itemfg.i-no = eb.stock-no NO-LOCK NO-ERROR.

   IF AVAIL itemfg THEN
   RUN oe/w-estfg.w(RECID(eb)) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_style V-table-Win
ON CHOOSE OF btn_style IN FRAME Corr
DO:
  IF AVAIL eb THEN
   FIND FIRST style WHERE style.company  = cocode
       AND style.style = eb.style NO-LOCK NO-ERROR.

   IF AVAIL style THEN
   RUN windows/stylec-e.w(RECID(style)) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_board V-table-Win
ON CHOOSE OF btn_board IN FRAME Corr
DO:
  IF AVAIL eb THEN
   FIND FIRST ITEM WHERE ITEM.company  = cocode
       AND ITEM.i-no = ef.board NO-LOCK NO-ERROR.

   IF AVAIL ITEM THEN
   RUN cec/w-itemc.w(RECID(ITEM)) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_cust V-table-Win
ON CHOOSE OF btn_cust IN FRAME Corr
DO:
  IF AVAIL eb THEN
   FIND FIRST cust WHERE cust.company  = cocode
       AND cust.cust-no = eb.cust-no NO-LOCK NO-ERROR.

   IF AVAIL cust THEN
   RUN windows/v-cust.w(RECID(cust)) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_qty-msf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_qty-msf V-table-Win
ON CHOOSE OF btn_qty-msf IN FRAME Corr
DO:
  IF AVAIL eb THEN RUN est/d-estmsf.w (ROWID(eb)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cad-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cad-no V-table-Win
ON LEAVE OF eb.cad-no IN FRAME Corr /* CAD# */
DO:    
   IF lv-cad-path <> "" THEN DO:
      IF lv-cad-ext = "" THEN lv-cad-ext = ".jpg".
      IF SEARCH(lv-cad-path + eb.cad-no:SCREEN-VALUE + lv-cad-ext) = ? THEN lv-cad-ext = "".

   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cust-no V-table-Win
ON ENTRY OF eb.cust-no IN FRAME Corr /* Cust# */
DO:
  assign
   eb.ship-name:sensitive     = no
   eb.ship-addr[1]:sensitive  = no
   eb.ship-addr[2]:sensitive  = no
   eb.ship-city:sensitive     = no
   eb.ship-state:sensitive    = no
   eb.ship-zip:sensitive      = no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cust-no V-table-Win
ON VALUE-CHANGED OF eb.cust-no IN FRAME Corr /* Cust# */
DO:
  RUN shipto-enable.
  RUN csr-display .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.dep V-table-Win
ON LEAVE OF eb.dep IN FRAME Corr /* Depth */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR dep-num AS INT NO-UNDO.

   if lastkey = -1 then return.
{&methods/lValidateError.i YES}
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).
   if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.

   IF v-cecscrn-dec THEN
   DO:
      dep-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
         /* eb.dep:screen-value  = string( dep-num +  op-dec) . */
      END.
   END.
   IF ll-auto-calc-selected THEN DO:
       ASSIGN
       eb.lin-in:SCREEN-VALUE = eb.dep:SCREEN-VALUE .
   END.
{&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.die-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.die-no V-table-Win
ON LEAVE OF eb.die-no IN FRAME Corr /* Die # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-prep (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.dust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.dust V-table-Win
ON LEAVE OF eb.dust IN FRAME Corr /* Top/Dust Flap */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR var-num AS INT NO-UNDO.

  if lastkey = -1 then return.
{&methods/lValidateError.i YES} 
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).
  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:

      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.

   end.
   IF v-cecscrn-dec THEN
   DO:
      var-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:

         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.

      END.
      ELSE do: 
         /* self:screen-value = string( var-num +  op-dec) */.
      END.
   END.
{&methods/lValidateError.i NO}
END.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_from-est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_from-est-no V-table-Win
ON LEAVE OF fi_from-est-no IN FRAME Corr /* From */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-fi_from-est-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.flute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.flute V-table-Win
ON LEAVE OF eb.flute IN FRAME Corr /* Flute */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-flute NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.fpanel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.fpanel V-table-Win
ON LEAVE OF eb.fpanel IN FRAME Corr /* Bottom Flap */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR var-num AS INT NO-UNDO.

   if lastkey = -1 then return.
{&methods/lValidateError.i YES}
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:

      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.

   end.
   IF v-cecscrn-dec THEN
   DO:
      var-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:

         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.

      END.
      ELSE do: 
        /*  self:screen-value = string( var-num +  op-dec) . */
      END.
   END.

    IF ll-auto-calc-selected AND {&self-name} <> dec(self:SCREEN-VALUE )
    THEN ll-style-is-valid = YES.
{&methods/lValidateError.i NO}
END.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.gluelap
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.gluelap V-table-Win
ON LEAVE OF eb.gluelap IN FRAME Corr /* Joint Tab Width */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR var-num AS INT NO-UNDO.

   if lastkey = -1 then return.
{&methods/lValidateError.i YES}
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).
  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:

      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.

   end.
   IF v-cecscrn-dec THEN
   DO:
      var-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:     
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.      
      END.
      ELSE do: 
          self:screen-value = string( var-num +  op-dec) .
      END.
   END.

   IF ll-auto-calc-selected AND {&self-name} <> dec(self:SCREEN-VALUE )
    THEN ll-style-is-valid = YES.
 {&methods/lValidateError.i NO}
END.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.k-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.k-len V-table-Win
ON LEAVE OF eb.k-len IN FRAME Corr /* Scores on Length */
DO:
    DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR var-num AS INT NO-UNDO.

   if lastkey = -1 then return.
{&methods/lValidateError.i YES}
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).
  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:

      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.

   end.
   IF v-cecscrn-dec THEN
   DO:
      var-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:      
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.     
      END.
      ELSE do: 
          /*self:screen-value = string( var-num +  op-dec) . */
      END.
   END.

   IF ll-auto-calc-selected AND {&self-name} <> dec(self:SCREEN-VALUE )
    THEN ll-style-is-valid = YES.
{&methods/lValidateError.i NO}
END.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.k-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.k-wid V-table-Win
ON LEAVE OF eb.k-wid IN FRAME Corr /* Scores on Width */
DO:
    DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR var-num AS INT NO-UNDO.

   if lastkey = -1 then return.
{&methods/lValidateError.i YES}
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).
  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:

      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.

   end.
   IF v-cecscrn-dec THEN
   DO:
      var-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:      
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.     
      END.
      ELSE do: 
         /* self:screen-value = string( var-num +  op-dec) . */
      END.
   END.

   IF ll-auto-calc-selected AND {&self-name} <> dec(self:SCREEN-VALUE )
    THEN ll-style-is-valid = YES.
{&methods/lValidateError.i NO}
END.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.len V-table-Win
ON LEAVE OF eb.len IN FRAME Corr /* Length */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR len-num AS INT NO-UNDO.

    if lastkey = -1 then return.
{&methods/lValidateError.i YES}
    v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).
   if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:

      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.

   end.

   IF v-cecscrn-dec THEN
   DO:
      len-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:

         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.

      END.
      ELSE do: 

          /* eb.len:screen-value = string( len-num +  op-dec) . */
      END.
   END.


  IF LASTKEY NE -1 THEN DO:
    RUN valid-wid-len NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
{&methods/lValidateError.i NO}
END.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.len V-table-Win
ON VALUE-CHANGED OF eb.len IN FRAME Corr /* Length */
DO:
  ll-wid-len-warned = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.lin-in
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.lin-in V-table-Win
ON LEAVE OF eb.lin-in IN FRAME Corr /* Joint Length */
DO:
    DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR var-num AS INT NO-UNDO.

   if lastkey = -1 then return.
{&methods/lValidateError.i YES}  
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).
  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:

      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.

   end.
   IF v-cecscrn-dec THEN
   DO:
      var-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:      
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.     
      END.
      ELSE do: 
         /* self:screen-value = string( var-num +  op-dec) . */
      END.
   END.
{&methods/lValidateError.i NO}

END.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.lock
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.lock V-table-Win
ON LEAVE OF eb.lock IN FRAME Corr /* Lock Tab */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR var-num AS INT NO-UNDO.

   if lastkey = -1 then return.
{&methods/lValidateError.i YES}
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).
  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:   
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.   
   end.
   IF v-cecscrn-dec THEN
   DO:
      var-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:      
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.     
      END.
      ELSE do: 
         /* self:screen-value = string( var-num +  op-dec) .*/
      END.
   END.

   IF ll-auto-calc-selected AND {&self-name} <> dec(self:SCREEN-VALUE )
    THEN ll-style-is-valid = YES.
{&methods/lValidateError.i NO}
END.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.plate-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.plate-no V-table-Win
ON LEAVE OF eb.plate-no IN FRAME Corr /* Plate # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-prep (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.procat V-table-Win
ON LEAVE OF eb.procat IN FRAME Corr /* FG Category */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-procat NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.procat V-table-Win
ON VALUE-CHANGED OF eb.procat IN FRAME Corr /* FG Category */
DO:
  RUN new-procat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.ship-id V-table-Win
ON ENTRY OF eb.ship-id IN FRAME Corr /* Ship To */
DO:
  if eb.cust-no:screen-value eq "TEMP" then do:
    find first shipto
        where shipto.company eq cocode
          and shipto.cust-no eq eb.cust-no:screen-value
          and shipto.ship-id eq {&self-name}:screen-value
        no-lock no-error.

    if not avail shipto then do:
      {&self-name}:screen-value = "TEMP".

      if eb.ship-name:screen-value    eq "" and
         eb.ship-addr[1]:screen-value eq "" and
         eb.ship-addr[2]:screen-value eq "" and
         eb.ship-city:screen-value    eq "" and
         eb.ship-state:screen-value   eq "" and
         eb.ship-zip:screen-value     eq "" THEN RUN display-shipto.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.ship-id V-table-Win
ON LEAVE OF eb.ship-id IN FRAME Corr /* Ship To */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-ship-id NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN shipto-enable.
     find first shipto where shipto.company = cocode
                                  and shipto.cust-no = eb.cust-no:screen-value
                                  AND shipto.ship-id = eb.ship-id:SCREEN-VALUE
                                  no-lock no-error.
               if v-shiptorep-log AND AVAIL shipto AND shipto.spare-char-1 <> "" THEN do:   /* task 05301401 */
                   eb.sman:SCREEN-VALUE = shipto.spare-char-1 .
                   run new-sman.
               END.
{&methods/lValidateError.i YES}
    IF eb.ship-name:SENSITIVE THEN DO:
      APPLY "entry" TO eb.ship-name.
      RETURN NO-APPLY.
    END.
{&methods/lValidateError.i NO}

  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.ship-id V-table-Win
ON VALUE-CHANGED OF eb.ship-id IN FRAME Corr /* Ship To */
DO:
  RUN display-shipto.
  ll-new-shipto = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.sman
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.sman V-table-Win
ON LEAVE OF eb.sman IN FRAME Corr /* Sales Rep */
DO:
  IF LASTKEY NE -1  THEN DO:
    RUN valid-sman NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.sman V-table-Win
ON VALUE-CHANGED OF eb.sman IN FRAME Corr /* Sales Rep */
DO:
  RUN new-sman.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.stock-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.stock-no V-table-Win
ON LEAVE OF eb.stock-no IN FRAME Corr /* FG Item# */
DO:
    find first itemfg where itemfg.company = cocode and
                            itemfg.i-no = eb.stock-no:screen-value in frame {&frame-name}
                      no-lock no-error.

    if not avail itemfg and eb.stock-no:screen-value <> "" then do:
    {&methods/lValidateError.i YES}
    /*   message "Invalid FG Item#. Try Help.".
       return no-apply.
     */
       message "This item does not exist, would you like to add it?" view-as alert-box question
               button yes-no update ll-ans as log.  
       if ll-ans then do:
          find xest where recid(xest) = recid(est) no-lock no-error.
          find xeb where recid(xeb) = recid(eb) no-lock no-error.
          find xef where recid(xef) = recid(ef) no-lock no-error.
          run crt-itemfg (input self:screen-value).
       end.   
       return no-apply. 
   {&methods/lValidateError.i NO}       
    end.  

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.style V-table-Win
ON LEAVE OF eb.style IN FRAME Corr /* Style Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-style NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-wid-len NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.style V-table-Win
ON VALUE-CHANGED OF eb.style IN FRAME Corr /* Style Code */
DO:
  RUN new-style.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.t-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.t-len V-table-Win
ON ENTRY OF eb.t-len IN FRAME Corr /* Blank Length */
DO:
  DEF VAR ld-total AS DEC DECIMALS 6 NO-UNDO.
  DEF VAR i AS INT NO-UNDO.


    EMPTY TEMP-TABLE tt-array.

    DO i = 1 TO EXTENT(ld-k-len-array):

      CREATE tt-array.
      ASSIGN
       tt-dec  = ld-k-len-array[i]
       tt-type = lv-k-len-scr-type[i].
      RELEASE tt-array.
    END.
                          /*is 16th*/ 
    RUN cec/d-panels.w (YES, "Length Panels", INPUT-OUTPUT TABLE tt-array).

    i = 0.
    FOR EACH tt-array:
       i = i + 1.
       IF i GT EXTENT(ld-k-len-array) THEN LEAVE.
       ASSIGN
        ld-k-len-array[i]    = tt-dec
        ld-total             = ld-total + ld-k-len-array[i]
        lv-k-len-scr-type[i] = tt-type.
    END.

    SELF:SCREEN-VALUE = STRING({sys/inc/k16.i ld-total}).

    APPLY "leave" TO {&self-name}.

    RETURN NO-APPLY.
  END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.t-len V-table-Win
ON LEAVE OF eb.t-len IN FRAME Corr /* Blank Length */
DO:
  {cec/msfcalc.i}

IF LASTKEY EQ -1 THEN Return .
{&methods/lValidateError.i YES}

  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:    
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.    
   end.

   DEF VAR lv-len as dec no-undo.
   DEF VAR lv-wid as dec no-undo.
   ASSIGN
   lv-len = decimal(eb.t-len:screen-value)
   lv-wid = decimal(eb.t-wid:screen-value).
   {sys/inc/k16bb.i lv-len}
   {sys/inc/k16bb.i lv-wid}

   IF v-cecscrn-char NE "Decimal" THEN
      assign lv-len = trunc(lv-len,3)
             lv-wid = trunc(lv-wid,3)
             lv-sqin = ( lv-len * lv-wid )
             eb.t-sqin:screen-value = string( if v-corr then round(lv-sqin * 0.007,4) else round(lv-sqin / 144,4)).
   ELSE
      assign lv-len = trunc(lv-len,6)
             lv-wid = trunc(lv-wid,6)
             lv-sqin = ( lv-len * lv-wid )
             eb.t-sqin:screen-value = string( if v-corr then round(lv-sqin * 0.007,6) else round(lv-sqin / 144,6)).
{&methods/lValidateError.i NO}
END.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.t-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.t-wid V-table-Win
ON ENTRY OF eb.t-wid IN FRAME Corr /* Blank Width */
DO:
  DEF VAR ld-total AS DEC DECIMALS 6 NO-UNDO.
  DEF VAR i AS INT NO-UNDO.



    EMPTY TEMP-TABLE tt-array.

    DO i = 1 TO EXTENT(ld-k-wid-array):

      CREATE tt-array.
      ASSIGN
       tt-dec  = ld-k-wid-array[i]
       tt-type = lv-k-wid-scr-type[i].
      RELEASE tt-array.
    END.

    RUN cec/d-panels.w (YES, "Width Panels", INPUT-OUTPUT TABLE tt-array).

    i = 0.
    FOR EACH tt-array:
      i = i + 1.
      IF i GT EXTENT(ld-k-wid-array) THEN LEAVE.
      ASSIGN
       ld-k-wid-array[i]    = tt-dec
       ld-total             = ld-total + ld-k-wid-array[i]
       lv-k-wid-scr-type[i] = tt-type.
    END.

    SELF:SCREEN-VALUE = STRING({sys/inc/k16.i ld-total}).

    APPLY "leave" TO {&self-name}.

    RETURN NO-APPLY.
  END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.t-wid V-table-Win
ON LEAVE OF eb.t-wid IN FRAME Corr /* Blank Width */
DO:
  {cec/msfcalc.i}
  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
   {&methods/lValidateError.i YES}
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
    {&methods/lValidateError.i NO}
   end.

   DEF VAR lv-len as dec no-undo.
   DEF VAR lv-wid as dec no-undo.
   ASSIGN
   lv-len = decimal(eb.t-len:screen-value)
   lv-wid = decimal(eb.t-wid:screen-value).
   {sys/inc/k16bb.i lv-len}
   {sys/inc/k16bb.i lv-wid}

   IF v-cecscrn-char NE "Decimal" THEN
 /*     assign
         lv-len = trunc(lv-len,3)
         lv-wid = trunc(lv-wid,3)
         lv-sqin = ( lv-len * lv-wid )
         eb.t-sqin:screen-value = string(if v-corr then round(lv-sqin * 0.007,4)
                                         else round(lv-sqin / 144,4)). */
       .
   ELSE
      assign
         lv-len = trunc(lv-len,6)
         lv-wid = trunc(lv-wid,6)
         lv-sqin = ( lv-len * lv-wid )
         eb.t-sqin:screen-value = string(if v-corr then round(lv-sqin * 0.007,6)
                                         else round(lv-sqin / 144,6)).
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tab-inout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tab-inout V-table-Win
ON VALUE-CHANGED OF tab-inout IN FRAME Corr /* Tab */
DO:
  tab-inout:SCREEN-VALUE =
      IF TRIM(tab-inout:SCREEN-VALUE) BEGINS "I" THEN "In"
      ELSE
      IF TRIM(tab-inout:SCREEN-VALUE) EQ "" THEN "" ELSE "Out".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.test
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.test V-table-Win
ON LEAVE OF eb.test IN FRAME Corr /* Test */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-test NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.tuck
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.tuck V-table-Win
ON LEAVE OF eb.tuck IN FRAME Corr /* Tuck */
DO:
  DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR var-num AS INT NO-UNDO.

   if lastkey = -1 then return.
{&methods/lValidateError.i YES}
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).
  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:    
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.    
   end.
   IF v-cecscrn-dec THEN
   DO:
      var-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:      
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.      
      END.
      ELSE do: 
         /* self:screen-value = string( var-num +  op-dec) . */
      END.
   END.
   IF ll-auto-calc-selected AND {&self-name} <> dec(self:SCREEN-VALUE )
    THEN ll-style-is-valid = YES.
{&methods/lValidateError.i NO}
END.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.wid V-table-Win
ON LEAVE OF eb.wid IN FRAME Corr /* Width */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR wid-num AS INT NO-UNDO.

   if lastkey = -1 then return.
{&methods/lValidateError.i YES}
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).
    if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:

      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.

   end.

   IF v-cecscrn-dec THEN
   DO:
      wid-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:      
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.      
      END.
      ELSE do: 

          /*eb.wid:screen-value = string( wid-num +  op-dec) */.
      END.
   END.

  IF LASTKEY NE -1 THEN DO:    
    RUN valid-wid-len NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.   
  END.
  IF ll-auto-calc-selected THEN DO:
     {cec/slotheitD.i "in frame {&frame-name}"}
  END.
{&methods/lValidateError.i NO}
END.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.wid V-table-Win
ON VALUE-CHANGED OF eb.wid IN FRAME Corr /* Width */
DO:
  ll-wid-len-warned = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "CE W>L"
       no-lock no-error.
  if not avail sys-ctrl then do transaction:
     create sys-ctrl.
     assign sys-ctrl.company = cocode
            sys-ctrl.name    = "CE W>L"
            sys-ctrl.descrip = "Default to display Warning when Carton Width > Length."
            sys-ctrl.log-fld = yes.
     MESSAGE sys-ctrl.descrip
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE sys-ctrl.log-fld.
  end.
  ll-warn = sys-ctrl.log-fld.

  find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "CADFILE"
                        no-lock no-error.
  IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
      create sys-ctrl.
      assign sys-ctrl.company = cocode
             sys-ctrl.name    = "CADFILE"
             sys-ctrl.descrip = "Dictate the location of the cad image to search."
             sys-ctrl.char-fld = "R:\rcode\cadimage\".      

  END.
  lv-cad-path = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".


  session:data-entry-return = yes.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  {src/adm/template/row-list.i "est"}
  {src/adm/template/row-list.i "eb"}
  {src/adm/template/row-list.i "est-qty"}
  {src/adm/template/row-list.i "ef"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "est"}
  {src/adm/template/row-find.i "eb"}
  {src/adm/template/row-find.i "est-qty"}
  {src/adm/template/row-find.i "ef"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-calc V-table-Win 
PROCEDURE auto-calc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ll-auto-calc-selected = yes.
   run dispatch ('enable-fields').
   disable eb.t-wid eb.t-len eb.t-sqin
           with frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-blank-size V-table-Win 
PROCEDURE calc-blank-size :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  from rfq not right for corrugate
   /* calc blank W,L SqIn */

         find first reftable
          where reftable.reftable eq "STYFLU"
            and reftable.company  eq eb.style
            and reftable.loc      eq eb.flute
            and reftable.code     eq "DIM-FIT"
          no-lock no-error.
      find style where style.company = eb.company and
                       style.style = eb.style
                       no-lock no-error.
      if avail style and style.type <> "F" then run calc-blank-size2. 

      run est/u2kinc1c.p (recid(eb)).
      run est/u2kinc2c.p (recid(eb)).

   find first formule no-lock.
/* will be displayed automatically for folding or corrware
   eb.t-wid:screen-value in frame {&frame-name} = string(formule.formule[1]).
   eb.t-len:screen-value in frame {&frame-name} = string(formule.formule[2]).
   lv-sqin:screen-value in frame {&frame-name} = 

   if lv-is-corr then string((formule.formule[7] * formule.formule[8]) * 0.007) 
                 else string(formule.formule[7] * formule.formule[8]).
*/   

======== old */

 /* calc blank W,L SqIn */

   def buffer bf-eb for eb .
   DEF VAR i as int no-undo.
   DEF VAR j as int no-undo.
   DEF VAR v-score-char like v-lscore-c extent 100.
   def buffer xest for est.
   DEF VAR v-index AS INT NO-UNDO.
   DEF VAR v-str AS CHAR NO-UNDO.

   find xest where /*recid(xest) = recid(est) no-lock. */
                   xest.company = eb.company and
                   xest.est-no = eb.est-no
                   no-lock no-error.
   find xef where recid(xef) = recid(ef) no-lock.
   find xeb where recid(xeb) = recid(eb) no-lock.

   DO i = 1 TO EXTENT(eb.k-wid-array2):

     ASSIGN
      ld-k-wid-array[i]    = eb.k-wid-array2[i]
      lv-k-wid-scr-type[i] = eb.k-wid-scr-type2[i].
   END.
   DO i = 1 TO EXTENT(eb.k-len-array2):
     ASSIGN
      ld-k-len-array[i]    = eb.k-len-array2[i]
      lv-k-len-scr-type[i] = eb.k-len-scr-type2[i].
   END.

   find style where style.company = eb.company and
                    style.style = eb.style
                    no-lock no-error.
   if avail style then do:
       if style.type <> "F"
          AND NOT ll-style-is-valid then run calc-blank-size2. 

      {cec/msfcalc.i}
      run est/u2kinc1c.p (recid(eb)).
      run est/u2kinc2c.p (recid(eb)).
      find first formule no-lock no-error.
      find bf-eb of eb exclusive-lock.

      assign bf-eb.t-wid = (formule.formule[1])
          bf-eb.t-len = (formule.formule[2])
          bf-eb.t-sqin = (formule.formule[7] * formule.formule[8])
          .

   /*   bf-eb.t-sqin = if v-corr then bf-eb.t-sqin * .007 else bf-eb.t-sqin / 144. */
       ASSIGN bf-eb.k-wid-array2 = 0
              bf-eb.k-len-array2 = 0.

      if style.type = "F" then 
         assign bf-eb.k-wid-array2[1] = bf-eb.t-wid
                bf-eb.k-len-array2[1] = bf-eb.t-len
                .
      else do:
         run cec/descalc.p (recid(xest),recid(xeb)).

         DO i = 1 TO EXTENT(xeb.k-wid-scr-type2):
           ASSIGN
            xeb.k-wid-scr-type2[i] = lv-k-wid-scr-type[i]
            xeb.k-len-scr-type2[i] = lv-k-len-scr-type[i].
         END.

         if v-lscore-c begins "No" then
            assign  xeb.k-wid-array2[1] = xeb.t-wid
                    xeb.k-len-array2[1] = xeb.t-len.
         else do:
           i = 0.
           for each w-box-design-line:
              i = i + 1.
              xeb.k-wid-array2[i] = w-box-design-line.wscore-d.
                 {sys/inc/k16bb.i xeb.k-wid-array2[i]} 
           end.
           assign  v-score-char    = ""
                   j               = 1.

           do i = 1 to 80:
             if substr(v-lscore-c,i,1) ne "" then do:
                v-score-char[j] = v-score-char[j] + substr(v-lscore-c,i,1).
                if substr(v-lscore-c,i + 1,1) eq "" then
                   assign  v-score-char[j] = trim(v-score-char[j])
                           j = j + 1.
             end.
             if j gt EXTENT(xeb.k-len-array2) then leave.
           end.
           DO i = 1 TO EXTENT(xeb.k-len-array2):

              IF v-cecscrn-dec AND v-score-char[i] NE "" THEN
                 ASSIGN
                    v-index = INDEX(v-score-char[i],".")
                    v-str = SUBSTRING(v-score-char[i],v-index + 1)
                    v-str = LEFT-TRIM(STRING(INT(v-str) / 64.0,">.999999"))
                    SUBSTRING(v-score-char[i],v-index) = v-str.

              xeb.k-len-array2[i] = dec(v-score-char[i]).
              {sys/inc/k16bb.i xeb.k-len-array2[i]}. 
           end.
         end.  /* else v-lscore */
       end. /* panels or not foam */
   end.

   IF NOT ll-blank-size-changed THEN
   DO i = 1 TO EXTENT(ld-k-wid-array):
     IF xeb.k-wid-array2[i] NE ld-k-wid-array[i] THEN DO:
       ll-blank-size-changed = YES.
       LEAVE.
     END.
   END.

   IF NOT ll-blank-size-changed THEN
   DO i = 1 TO EXTENT(ld-k-len-array):
     IF xeb.k-len-array2[i] NE ld-k-len-array[i] THEN DO:
       ll-blank-size-changed = YES.
       LEAVE.
     END.
   END.

   RUN display-matrix.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-blank-size2 V-table-Win 
PROCEDURE calc-blank-size2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND CURRENT eb .
   ASSIGN FRAME {&frame-name} {&list-5}.

   {sys/inc/k16bb.i eb.wid  } 
   {sys/inc/k16bb.i eb.len  } 
   {sys/inc/k16bb.i eb.dep  } 
   {sys/inc/k16bb.i eb.dust  } 
   {sys/inc/k16bb.i eb.fpanel  } 
   {sys/inc/k16bb.i eb.lock  }
   {sys/inc/k16bb.i eb.gluelap  } 
   {sys/inc/k16bb.i eb.k-wid  } 
   {sys/inc/k16bb.i eb.k-len  } 
   {sys/inc/k16bb.i eb.tuck  }  
   {sys/inc/k16bb.i eb.lin-in}

   find xeb where recid(xeb) = recid(eb) no-lock.

   find style where style.company = eb.company and
                    style.style = eb.style
                    no-lock no-error.
   if AVAIL style AND style.material[7] ne "" then do:
     eb.adhesive = style.material[7].
     /*if eb.gluelap ne 0 then*/ eb.lin-in = eb.dep.  /* Ticket 13021 */
   end.

   {est/u2estc.i eb.gluelap 1}
   {est/u2estc.i eb.k-wid 2}

   find first item where item.company = est.company
                    and item.i-no eq eb.adhesive
                  no-lock no-error.
   if avail item and eb.adhesive ne "NO JOINT" then do:
            if item.mat-type eq "G" then do:
                    if eb.tab-in then do:
                       {est/u2estc.i eb.k-len 3}
                    end.
                    else do:
                       eb.tab-in = no.
                       {est/u2estc.i eb.k-len 4}
                    end.
            end.
            else if item.mat-type eq "S" then do:
                    if eb.tab-in then do:
                       {est/u2estc.i eb.k-len 5}
                    end.
                    else do:
                       eb.tab-in = no.
                       {est/u2estc.i eb.k-len 6}
                    end.
            end.
            else if item.mat-type eq "T" then do:
                    eb.tab-in = ?.
                    {est/u2estc.i eb.k-len 7}
            end.
    end.
    else do:
                 eb.tab-in = ?.
                 {est/u2estc.i eb.k-len 7}
    end.

    if eb.len eq eb.wid
    then do:
                 {est/u2estc.i eb.k-wid 2 dim-fit}
    end.
    else do:
                 {est/u2estc.i eb.k-wid 2}
    end.

    /* run dispatch ('display-fields'). */
    RUN display-matrix.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancel-updates V-table-Win 
PROCEDURE cancel-updates :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN check-modified IN THIS-PROCEDURE ('clear':U) NO-ERROR.
RUN dispatch('cancel-record':U).
RUN dispatch('initialize':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-flute-test-change V-table-Win 
PROCEDURE check-flute-test-change :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ll = NO.

    IF (lv-hold-flute NE "" AND eb.flute:SCREEN-VALUE NE lv-hold-flute) OR
       (lv-hold-test  NE "" AND eb.test:SCREEN-VALUE  NE lv-hold-test)  THEN
      MESSAGE "Find new Board Material?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.

    RUN set-hold-values.

    IF ll THEN DO:
      RUN new-flute-test.
      APPLY "entry" TO ef.board.
      APPLY "help" TO FRAME {&FRAME-NAME}.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-style V-table-Win 
PROCEDURE check-style :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST style
        WHERE style.company EQ cocode
          AND style.style   EQ eb.style:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL style THEN
      ASSIGN
       lv-foam                 = style.type EQ "F"
       style_dscr:SCREEN-VALUE = style.dscr.
    ELSE
      ASSIGN
       lv-foam                 = NO
       style_dscr:SCREEN-VALUE = "".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-itemfg V-table-Win 
PROCEDURE crt-itemfg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* -------------------------------------------------- fg/ce-addfg.p 08/98 JLF */
/* Add FG thru estimating                                                     */
/* -------------------------------------------------------------------------- */

def input parameter v-item like itemfg.i-no.
/*
{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest    for est.
def shared buffer xef     for ef.
def shared buffer xeb     for eb.
*/

DEF VAR tmpstore as cha no-undo.
DEF VAR i as int no-undo.


{ce/msfcalc.i}
{oe/fgfreight.i}

find first cust  where cust.company eq cocode
                   and cust.cust-no eq xeb.cust-no
    no-lock no-error.

create itemfg.
assign
 itemfg.company    = cocode
 itemfg.loc        = locode
 itemfg.i-no       = v-item
 itemfg.i-code     = "C"
 itemfg.i-name     = xeb.part-dscr1
 itemfg.part-dscr1 = xeb.part-dscr2
 itemfg.sell-uom   = "M"
 itemfg.part-no    = xeb.part-no
 itemfg.cust-no    = xeb.cust-no
 itemfg.cust-name  = if avail cust then cust.name else ""
 itemfg.pur-uom    = IF xeb.pur-man THEN "EA" ELSE "M"
 itemfg.prod-uom   = IF xeb.pur-man THEN "EA" ELSE "M"
 itemfg.stocked    = yes
 itemfg.die-no     = xeb.die-no
 itemfg.plate-no   = xeb.plate-no
 itemfg.style      = xeb.style
 itemfg.procat     = xeb.procat
 itemfg.cad-no     = xeb.cad-no
 itemfg.upc-no     = xeb.upc-no
 itemfg.spc-no     = xeb.spc-no
 itemfg.isaset     = (xest.est-type eq 2 or xest.est-type eq 6) and
                     xeb.form-no eq 0
 itemfg.pur-man    = xeb.pur-man  
 itemfg.alloc      = xeb.set-is-assembled
 itemfg.setupDate  = TODAY.

 IF itemfg.alloc NE ? THEN itemfg.alloc = NOT itemfg.alloc.

 {oe/fgfreighta.i xeb}


IF v-graphic-char NE "" THEN 
DO:
   IF LOOKUP(SUBSTR(v-graphic-char,LENGTH(v-graphic-char)),"\,/") EQ 0 THEN
      v-graphic-char = v-graphic-char + "\".

   IF SEARCH(v-graphic-char + itemfg.i-no + ".jpg") NE ? THEN
      itemfg.box-image = v-graphic-char + itemfg.i-no + ".jpg".
END.
RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT "").
{fg/set-inks1.i itemfg xeb}

{sys/inc/fgcascnt.i itemfg xeb}

{sys/inc/updfgdim.i "xeb"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cust-spec V-table-Win 
PROCEDURE cust-spec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR rec_key_value as cha no-undo.
  DEF VAR header_value as cha no-undo.
{&methods/lValidateError.i YES}
  RUN Get_Procedure IN Persistent-Handle ('specnot2.',OUTPUT run-proc,no).
  find itemfg where itemfg.company = eb.company and
                    itemfg.i-no = eb.stock-no:screen-value in frame {&frame-name}
                    no-lock no-error.
  rec_key_value = if avail itemfg then itemfg.rec_key else "".

  IF rec_key_value <> "" and run-proc NE '' THEN {methods/smartrun.i (rec_key_value,header_value)}        
  else do:
    message "No FG Item Spec note." rec_key_value view-as alert-box.
    return.
  end.
{&methods/lValidateError.i NO}
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
  HIDE FRAME Corr.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-matrix V-table-Win 
PROCEDURE display-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR li AS INT NO-UNDO.


 {cec/msfcalc.i}

  DO li = 1 TO EXTENT(eb.k-wid-array2):

    ASSIGN
     ld-k-wid-array[li]    = eb.k-wid-array2[li]
     lv-k-wid-scr-type[li] = eb.k-wid-scr-type2[li].
  END.
  DO li = 1 TO EXTENT(eb.k-len-array2):
    ASSIGN
     ld-k-len-array[li]    = eb.k-len-array2[li]
     lv-k-len-scr-type[li] = eb.k-len-scr-type2[li].
  END.

  assign eb.len:screen-value in frame {&frame-name} = string( {sys/inc/k16.i eb.len } ) 
         eb.wid:screen-value = string( {sys/inc/k16.i eb.wid } )
         eb.dep:screen-value = string({sys/inc/k16.i eb.dep    } )
         eb.dust:screen-value = string({sys/inc/k16.i eb.dust   } )
         eb.fpanel:screen-value = string({sys/inc/k16.i eb.fpanel } )
         eb.tuck:screen-value = string({sys/inc/k16.i eb.tuck   } )
         eb.k-wid:screen-value = string({sys/inc/k16.i eb.k-wid  } )
         eb.k-len:screen-value = string({sys/inc/k16.i eb.k-len  } )
         eb.gluelap:screen-value = string({sys/inc/k16.i eb.gluelap   } )
         eb.lock:screen-value = string({sys/inc/k16.i eb.lock   } )
         eb.lin-in:screen-value = string({sys/inc/k16.i eb.lin-in } )
         eb.t-len:screen-value = string({sys/inc/k16.i eb.t-len  } )
         eb.t-wid:screen-value = string({sys/inc/k16.i eb.t-wid  } )
         eb.t-sqin:screen-value = if v-corr then string(eb.t-sqin * .007)
                                  else string(eb.t-sqin / 144).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-shipto V-table-Win 
PROCEDURE display-shipto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&frame-name}:
    RELEASE shipto.
    IF eb.ship-id:SCREEN-VALUE NE "TEMP" THEN
    FIND FIRST shipto
        WHERE shipto.company EQ cocode
          AND shipto.cust-no EQ eb.cust-no:SCREEN-VALUE
          AND shipto.ship-id EQ eb.ship-id:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL shipto THEN
      ASSIGN
       eb.ship-id:SCREEN-VALUE      = shipto.ship-id
       eb.ship-name:SCREEN-VALUE    = shipto.ship-name
       eb.ship-addr[1]:SCREEN-VALUE = shipto.ship-addr[1]
       eb.ship-addr[2]:SCREEN-VALUE = shipto.ship-addr[2]
       eb.ship-city:SCREEN-VALUE    = shipto.ship-city
       eb.ship-state:SCREEN-VALUE   = shipto.ship-state
       eb.ship-zip:SCREEN-VALUE     = shipto.ship-zip.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-current-values V-table-Win 
PROCEDURE get-current-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER op-values AS CHAR NO-UNDO.
  DEFINE VARIABLE hProc AS HANDLE NO-UNDO.
  DEFINE VARIABLE vc-values AS CHAR NO-UNDO.
  DEFINE VARIABLE v-temp AS CHAR NO-UNDO.
  DEF VAR l-was-modified AS LOG NO-UNDO.
  l-was-modified = NO.
    IF FRAME corr:SENSITIVE = NO THEN
        RETURN.
    vc-values = "".
    hProc = FRAME Corr:HANDLE.
    hProc = hProc:FIRST-CHILD NO-ERROR.
    IF NOT VALID-HANDLE(hProc) THEN
     RETURN.
    hProc = hProc:FIRST-CHILD NO-ERROR.

    DO WHILE VALID-HANDLE(hProc):

        hProc = hProc:NEXT-SIBLING NO-ERROR.
        IF NOT VALID-HANDLE(hProc) THEN
            LEAVE.
        IF LOOKUP(hProc:TYPE, "BUTTON,LITERAL,RECTANGLE") GT 0 THEN     
            NEXT.

        v-temp = hProc:SCREEN-VALUE NO-ERROR.
        IF hProc:MODIFIED THEN
            l-was-modified = TRUE.
        IF ERROR-STATUS:ERROR THEN
            NEXT.

        IF hProc:SCREEN-VALUE NE ? THEN
          vc-values = vc-values + hProc:SCREEN-VALUE.
    END.
    op-values = vc-values.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def buffer b-est for est.
  def buffer b-ef for ef.
  def buffer b-eb for eb.
  def buffer b-set for eb.
  DEF BUFFER xbox-design-hdr FOR box-design-hdr.

  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR lv-hld-cust like eb.cust-no no-undo.
  DEF VAR lv-hld-ship like eb.ship-id no-undo.
  DEF VAR lv-hld-stock-no like eb.stock-no no-undo.
  DEF VAR lv-hld-part-no like eb.part-no no-undo.
  DEF VAR lv-hld-board LIKE ef.board NO-UNDO.
  DEF VAR lv-prev-cad# AS cha NO-UNDO.
  DEF VAR lv-prev-style AS cha NO-UNDO. 
  DEF VAR ll-set AS LOG NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR v-cad-no LIKE eb.cad-no NO-UNDO.
  DEF VAR lv-box-des AS CHAR INIT "S" NO-UNDO.
  DEF VAR v-dec AS DEC NO-UNDO.
  DEF VAR v-dec2 AS DEC NO-UNDO.
  DEF VAR v-w-array AS DEC EXTENT 30 NO-UNDO.
  DEF VAR v-count AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  assign
   lv-hld-cust = eb.cust-no
   lv-hld-ship = eb.ship-id
   lv-prev-style = eb.style
   lv-prev-cad# = eb.cad-no
   lv-hld-part-no = eb.part-no
   lv-hld-stock-no = eb.stock-no
   v-cad-no = eb.cad-no.
  DO WITH FRAME {&FRAME-NAME}:
         ASSIGN tb-set.
  END.

  IF AVAIL ef THEN
    lv-hld-board = ef.board.

  IF lv-cad-ext = "" THEN lv-cad-ext = ".jpg".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-new-shipto THEN DO WITH FRAME {&FRAME-NAME}:
    RUN windows/d-shpfly.w (ROWID(eb)).
    RUN display-shipto.
    ASSIGN
     eb.ship-id
     eb.ship-name
     eb.ship-addr[1]
     eb.ship-addr[2]
     eb.ship-city
     eb.ship-state
     eb.ship-zip.
  END.

  {ce/uship-id.i no}

  FIND CURRENT eb.

  REPEAT:
     find b-ef where recid(b-ef) = recid(ef) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
     IF AVAIL b-ef THEN
     DO:
        assign b-ef.flute = eb.flute
               b-ef.test  = eb.test.
        find b-ef where recid(b-ef) = recid(ef) no-lock.
        LEAVE.
     END.
  END.

  find b-est where recid(b-est) = recid(est) exclusive-lock.
  assign b-est.mod-date = today
         est.mod-date:screen-value in frame {&frame-name} = string(today)
         .
  find b-est where recid(b-est) = recid(est) no-lock.

  {sys/inc/k16bb.i eb.t-wid  } 
  {sys/inc/k16bb.i eb.t-len  } 
  {sys/inc/k16bb.i eb.wid  } 
  {sys/inc/k16bb.i eb.len  } 
  {sys/inc/k16bb.i eb.dep  } 
  {sys/inc/k16bb.i eb.dust  } 
  {sys/inc/k16bb.i eb.fpanel  } 
  {sys/inc/k16bb.i eb.tuck  } 
  {sys/inc/k16bb.i eb.k-wid  } 
  {sys/inc/k16bb.i eb.k-len  } 
  {sys/inc/k16bb.i eb.gluelap  } 
  {sys/inc/k16bb.i eb.lock  } 
  {sys/inc/k16bb.i eb.lin-in}

  {cec/slotheit.i}  
  {cec/slotwidth.i}
  ASSIGN
   eb.sty-lock = NOT ll-auto-calc-selected
   eb.tab-in   = IF tab-inout EQ "In"  THEN YES ELSE
                 IF tab-inout EQ "Out" THEN NO  ELSE ?.

  IF ll-auto-calc-selected THEN DO:

    RUN calc-blank-size.
    FIND CURRENT eb.
  END.

  ELSE DO:
    DO li = 1 TO EXTENT(ld-k-wid-array):

      eb.k-wid-scr-type2[li] = lv-k-wid-scr-type[li].
      IF eb.k-wid-array2[li] NE ld-k-wid-array[li] THEN
        ASSIGN
         ll-blank-size-changed = YES
         eb.k-wid-array2[li]   = ld-k-wid-array[li].
    END.

    DO li = 1 TO EXTENT(ld-k-len-array):
      eb.k-len-scr-type2[li] = lv-k-len-scr-type[li].
      IF eb.k-len-array2[li] NE ld-k-len-array[li] THEN
        ASSIGN
         ll-blank-size-changed = YES
         eb.k-len-array2[li]   = ld-k-len-array[li].
    END.
  END.

  IF lv-foam THEN
    ASSIGN
     eb.t-wid = eb.wid
     eb.t-len = eb.len
     eb.t-dep = eb.dep.

  IF v-cecscrn-char NE "Decimal" THEN
     ASSIGN
        eb.t-wid = TRUNC(eb.t-wid * li-16-32,0) / li-16-32
        eb.t-len = TRUNC(eb.t-len * li-16-32,0) / li-16-32.

  IF lv-foam OR NOT ll-auto-calc-selected THEN eb.t-sqin = eb.t-len * eb.t-wid.

  IF NOT ll-blank-size-changed THEN
    ll-blank-size-changed = eb.t-wid NE lv-hld-wid OR eb.t-len NE lv-hld-len.

  RUN one-eb-on-ef (ROWID(ef), OUTPUT ll-one-eb-on-ef).

  IF ll-one-eb-on-ef AND ll-blank-size-changed THEN DO:
    MESSAGE "Do you wish to reset layout screen?"
        VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans2 AS LOG.

    ef.lsh-lock = NO.    
    IF ll-ans2 THEN RUN update-sheet.    
  END.
  IF tb-set = YES THEN
    eb.spare-char-2 = "Y".
  ELSE
    eb.spare-char-2 = "N".
  IF TRIM(fi_from-est-no) NE TRIM(lv-master-est-no) THEN DO:
    FOR EACH b-eb
        WHERE b-eb.company EQ eb.company
          AND b-eb.est-no  EQ eb.est-no:
      b-eb.master-est-no = FILL(" ",8 - LENGTH(TRIM(fi_from-est-no))) +
                           TRIM(fi_from-est-no).
    END.
    FIND b-est WHERE ROWID(b-est) EQ ROWID(est).
    IF TRIM(fi_from-est-no) EQ "" AND b-est.est-type EQ 8 THEN b-est.e-num = 0.
    FIND CURRENT b-est NO-LOCK.
  END.

  IF ll-blank-size-changed                       OR
     (cestyle-log AND eb.style NE lv-prev-style) THEN DO:
    ll-ans2 = NO.
    MESSAGE "Do you wish to reset box design?"
        VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans2.
    IF ll-ans2 THEN
       lv-box-des = "B".
    ELSE
       lv-box-des = "N".
  END.
  ELSE
  DO:
     for FIRST box-design-hdr where
         box-design-hdr.design-no = 0 and
         box-design-hdr.company = eb.company AND
         box-design-hdr.est-no = eb.est-no AND
         box-design-hdr.form-no = eb.form-no AND
         box-design-hdr.blank-no = eb.blank-no
         NO-LOCK:

         FOR EACH box-design-line FIELDS(wscore) of box-design-hdr
             NO-LOCK:

             v-dec = DECIMAL(TRIM(box-design-line.wscore)) NO-ERROR.

             IF NOT ERROR-STATUS:ERROR AND
                TRIM(box-design-line.wscore) NE "" THEN
                ASSIGN
                   v-count = v-count + 1
                   v-w-array[v-count] = v-dec.
         END.

         RUN tokenize-proc(box-design-hdr.lscore).

         DO v-count = 1 TO 30:

            ASSIGN
               v-dec = {sys/inc/k16v.i eb.k-len-array2[v-count]}
               v-dec2 = {sys/inc/k16v.i eb.k-wid-array2[v-count]}.

            IF v-l-array[v-count] NE v-dec OR
               v-w-array[v-count] NE v-dec2 THEN
               DO:
                  MESSAGE "Do you wish to reset box design?"
                     VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans2.

                  IF ll-ans2 THEN
                     lv-box-des = "B".
                  ELSE
                     lv-box-des = "N".

                  LEAVE.
               END.
         END.
     END.
  END.

  DO li = 1 TO 2:
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"box-calc-target",OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(1,char-hdl))) THEN DO:

      RUN build-box IN WIDGET-HANDLE(ENTRY(1,char-hdl)) (lv-box-des).
      li = 2.
    END.
    ELSE
    IF li EQ 1 THEN DO:
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
      IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
        RUN init-box-design IN WIDGET-HANDLE(char-hdl) (THIS-PROCEDURE).
      ELSE li = 2.
    END.
  END.

  ASSIGN
   ll-auto-calc-selected = NO
   ll-blank-size-changed = NO
   ll-new-shipto         = NO.

  /* update box's image for Fibre/Artios */
  IF eb.cad-no <> "" AND lv-cad-path <> "" THEN DO:
     IF SEARCH(lv-cad-path + eb.cad-no + lv-cad-ext) <> ? OR
        (cadfile NE '' AND SEARCH(cadfile) <> ?) THEN DO:
        FIND first box-design-hdr where box-design-hdr.design-no = 0 and
                                     box-design-hdr.company = eb.company 
                                 and box-design-hdr.est-no = eb.est-no     
                                 and box-design-hdr.form-no   eq eb.form-no
                                 and box-design-hdr.blank-no  eq eb.blank-no NO-ERROR.
        IF AVAIL box-design-hdr AND
            ( ((cadfile NE '') AND SEARCH(cadfile) <> ?) OR
              SEARCH(lv-cad-path + eb.cad-no + lv-cad-ext) <> ? ) THEN 
        DO:

           ASSIGN box-design-hdr.box-image = IF cadfile NE '' THEN cadfile
                  ELSE lv-cad-path + eb.cad-no + lv-cad-ext. /*".jpg"*/.
        END.
     END.
     ELSE DO: /* reset from style */
        find first style where style.company EQ eb.company and style.style  eq eb.style
                 no-lock no-error.
        if avail style then
           find first xbox-design-hdr where xbox-design-hdr.design-no eq style.design-no
                                        and xbox-design-hdr.est-no    eq ""
                                        no-lock no-error.
           FIND first box-design-hdr where box-design-hdr.design-no = 0 and
                                     box-design-hdr.company = eb.company 
                                 and box-design-hdr.est-no = eb.est-no     
                                 and box-design-hdr.form-no   eq eb.form-no
                                 and box-design-hdr.blank-no  eq eb.blank-no NO-ERROR.
        IF AVAIL box-design-hdr AND (SEARCH(box-design-hdr.box-image) EQ ? OR cadfile NE '') THEN 
           ASSIGN box-design-hdr.box-image = IF cadfile NE '' THEN cadfile
                                             ELSE xbox-design-hdr.box-image.

     END.
     ASSIGN
       cadfile = ''
       lv-cad-ext = "".
  END.
  IF eb.cad-no EQ "" AND v-cad-no <> "" THEN DO:
     find first style where style.company EQ eb.company and style.style  eq eb.style
                 no-lock no-error.
     if avail style then
        find first xbox-design-hdr where xbox-design-hdr.design-no eq style.design-no
                                        and xbox-design-hdr.est-no    eq ""
                                        no-lock no-error.
     FIND first box-design-hdr where box-design-hdr.design-no = 0 and
                                     box-design-hdr.company = eb.company 
                                 and box-design-hdr.est-no = eb.est-no     
                                 and box-design-hdr.form-no   eq eb.form-no
                                 and box-design-hdr.blank-no  eq eb.blank-no NO-ERROR.

     IF AVAIL box-design-hdr THEN ASSIGN box-design-hdr.box-image = xbox-design-hdr.box-image.
  END.

  IF est.est-type NE 8 THEN
  FOR EACH b-eb
      WHERE b-eb.company EQ eb.company
        AND b-eb.est-no  EQ eb.est-no
        AND ROWID(b-eb)  NE ROWID(eb):
    assign
     b-eb.cust-no      = eb.cust-no
     b-eb.ship-id      = eb.ship-id
     b-eb.ship-no      = eb.ship-no
     b-eb.ship-name    = eb.ship-name
     b-eb.ship-addr[1] = eb.ship-addr[1]
     b-eb.ship-addr[2] = eb.ship-addr[2]
     b-eb.ship-city    = eb.ship-city
     b-eb.ship-state   = eb.ship-state
     b-eb.ship-zip     = eb.ship-zip
     b-eb.sman         = eb.sman
     b-eb.comm         = eb.comm.
  END.

  IF est.est-type = 6 THEN DO:
       /* update set info from eb for two peice box*/
     FIND FIRST b-set WHERE b-set.company = est.company
                         AND b-set.est-no = est.est-no
                         AND b-set.form-no = 0 NO-ERROR.

     li = 0.
     FOR EACH b-eb WHERE b-eb.company EQ est.company
                    AND b-eb.est-no  EQ est.est-no
                    AND b-eb.form-no NE 0 NO-LOCK:
       li = li + 1.
     END.
     ll-set = li GT 1.
     IF NOT ll-set THEN DO: /* it's two piece box */ 
       IF NOT AVAIL b-set THEN DO:
         CREATE b-set.
         ASSIGN
          b-set.est-type = 6
          b-set.company  = eb.company
          b-set.loc      = eb.loc
          b-set.e-num    = eb.e-num
          b-set.est-no   = eb.est-no
          b-set.form-no  = 0
          b-set.blank-no = 0
          b-set.est-int  = INT(eb.est-no).
       END.
       IF NEW b-set                     OR
          lv-hld-stock-no NE eb.stock-no OR
          lv-hld-part-no  NE eb.part-no  THEN
       ASSIGN
        b-set.stock-no = eb.stock-no
        b-set.part-no = eb.part-no
        b-set.part-dscr1 = eb.part-dscr1
        b-set.part-dscr2 = eb.part-dscr2
        b-set.procat = eb.procat
        b-set.len = eb.len
        b-set.wid = eb.wid
        b-set.dep = eb.dep.
   END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-orig-style LIKE eb.style NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  FIND CURRENT ef EXCLUSIVE-LOCK.
  FIND CURRENT est EXCLUSIVE-LOCK.

  v-orig-style = eb.style.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .
  IF tb-set = YES THEN
    eb.spare-char-2 = "Y".
  ELSE
    eb.spare-char-2 = "N".
  /* Code placed here will execute AFTER standard behavior.    */
  FIND CURRENT ef NO-LOCK.
  FIND CURRENT est NO-LOCK.

  IF v-orig-style NE eb.style THEN DO:
    FIND FIRST style
               WHERE style.company  EQ cocode
                 AND style.style    EQ eb.style
                 AND style.industry EQ lv-industry
               NO-LOCK NO-ERROR.

    /* task 12011101 - to update this even on an update */
    IF avail(style) AND style.qty-per-set NE 0 THEN
      eb.yld-qty = style.qty-per-set.
  END.

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
  DO WITH FRAME {&FRAME-NAME}: 
    DISABLE est.metric
            tab-inout
            fi_from-est-no.
  END.

  RUN release-shared-buffers.
  DISABLE btncadlookup btndielookup WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-eqty AS CHAR NO-UNDO.
  DEF VAR lv-image AS CHAR NO-UNDO.
  DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.
  DEF BUFFER b-ef FOR ef.
  DEF BUFFER b-eb FOR eb.

  /* Code placed here will execute PRIOR to standard behavior. */
  {cec/msfcalc.i}


  IF NOT AVAIL est OR NOT AVAIL eb THEN RETURN.

  DO WITH FRAME {&FRAME-NAME}:

  ASSIGN tb-set:SENSITIVE = FALSE
         bt-new-die:SENSITIVE = FALSE
         bt-new-plate:SENSITIVE = FALSE.

  IF v-cecscrn-char EQ "Decimal" THEN
     ASSIGN
        eb.len:FORMAT = ">>9.999999"
        eb.len:WIDTH = 15.2
        eb.wid:FORMAT = ">>9.999999"
        eb.wid:WIDTH = 15.2
        eb.dep:FORMAT = ">>9.999999"
        eb.dep:WIDTH = 15.2
        eb.dust:FORMAT = "->>9.999999"
        eb.dust:WIDTH = 15.2
        eb.fpanel:FORMAT = "->>9.999999"
        eb.fpanel:WIDTH = 15.2
        eb.lock:FORMAT = "->>9.999999"
        eb.lock:WIDTH = 15.2
        eb.gluelap:FORMAT = "->>9.999999"
        eb.gluelap:WIDTH = 15.2
        eb.k-wid:FORMAT = "->>9.999999"
        eb.k-wid:WIDTH = 15.2
        eb.k-len:FORMAT = "->>9.999999"
        eb.k-len:WIDTH = 15.2
        eb.tuck:FORMAT = "->>9.999999"
        eb.tuck:WIDTH = 15.2
        eb.lin-in:FORMAT = "->>9.999999"
        eb.lin-in:WIDTH = 15.2
        eb.t-wid:FORMAT = ">>>9.999999"
        eb.t-wid:WIDTH = 15.2
        eb.t-len:FORMAT = ">>>9.999999"
        eb.t-len:WIDTH = 15.2
        eb.t-sqin:FORMAT = ">>>9.999999"
        eb.t-sqin:WIDTH = 15.2.
  END.

  IF AVAIL est THEN DO TRANSACTION:
    DISABLE TRIGGERS FOR LOAD OF est.
    FOR EACH b-ef NO-LOCK
        WHERE b-ef.company EQ est.company
          AND b-ef.est-no  EQ est.est-no
        BREAK BY b-ef.eqty:
      IF FIRST-OF(b-ef.eqty) THEN li = 0.
      li = li + 1.
    END.
    IF li NE est.form-qty THEN DO:
      FIND CURRENT est EXCLUSIVE NO-WAIT NO-ERROR.
      IF AVAIL est THEN est.form-qty = li.
      FIND CURRENT est NO-LOCK.
    END.

    li = 0.
    DISABLE TRIGGERS FOR LOAD OF ef.
    FIND FIRST ef
        WHERE ef.company EQ eb.company
          AND ef.est-no  EQ eb.est-no
          AND ef.form-no EQ eb.form-no
        NO-LOCK NO-ERROR.
    IF AVAIL ef THEN DO:
      FOR EACH b-eb
          WHERE b-eb.company EQ ef.company
            AND b-eb.est-no  EQ ef.est-no
            AND b-eb.form-no EQ ef.form-no
          NO-LOCK:
        li = li + 1.
      END.
      IF li NE ef.blank-qty THEN DO:
        FIND CURRENT ef EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL ef THEN ef.blank-qty = li.
        FIND CURRENT ef NO-LOCK.
      END.
    END.
    fi_blank-qty = li.
  END.

  IF AVAIL est-qty THEN 
    ASSIGN
       tab-inout = IF eb.tab-in EQ YES THEN "In"  ELSE
                   IF eb.tab-in EQ NO  THEN "Out" ELSE ""
       fi_per-set = IF eb.est-type GE 7 THEN 1
                    ELSE
                    IF eb.yld-qty LT 0 THEN -1 / eb.yld-qty ELSE eb.yld-qty
       fi_msf     = (IF eb.est-type GE 7 THEN eb.bl-qty
                     ELSE (est-qty.eqty * fi_per-set)) *
                    (IF v-corr THEN (eb.t-sqin * .007)
                               ELSE (eb.t-sqin / 144)) / 1000.


  fi_from-est-no = IF eb.master-est-no NE "" AND
                      eb.est-type EQ 8       THEN eb.master-est-no
                   ELSE STRING(est.e-num,">>>>>>>>").

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL ef AND AVAIL eb THEN DO WITH FRAME {&FRAME-NAME}:

    RUN check-style.

    DO li = 1 TO EXTENT(eb.k-wid-array2):

      ASSIGN
       ld-k-wid-array[li]    = eb.k-wid-array2[li]
       lv-k-wid-scr-type[li] = eb.k-wid-scr-type2[li].
    END.
    DO li = 1 TO EXTENT(eb.k-len-array2):
      ASSIGN
       ld-k-len-array[li]    = eb.k-len-array2[li]
       lv-k-len-scr-type[li] = eb.k-len-scr-type2[li].
    END.

    FIND FIRST sman
        WHERE sman.company EQ eb.company
          AND sman.sman    EQ eb.sman
        NO-LOCK NO-ERROR.
    sman_sname:SCREEN-VALUE = IF AVAIL sman THEN sman.sname ELSE "".

    RUN new-procat.

    ASSIGN
     eb.len:screen-value = string( {sys/inc/k16.i eb.len } ) 
     eb.wid:screen-value = string( {sys/inc/k16.i eb.wid } )
     eb.dep:screen-value = string({sys/inc/k16.i eb.dep    } )
     eb.dust:screen-value = string({sys/inc/k16.i eb.dust   } )
     eb.fpanel:screen-value = string({sys/inc/k16.i eb.fpanel } )
     eb.tuck:screen-value = string({sys/inc/k16.i eb.tuck   } )
     eb.k-wid:screen-value = string({sys/inc/k16.i eb.k-wid  } )
     eb.k-len:screen-value = string({sys/inc/k16.i eb.k-len  } )
     eb.gluelap:screen-value = string({sys/inc/k16.i eb.gluelap   } )
     eb.lock:screen-value = string({sys/inc/k16.i eb.lock   } )
     eb.lin-in:screen-value = string({sys/inc/k16.i eb.lin-in } )
     eb.t-len:screen-value = string({sys/inc/k16.i eb.t-len  } )
     eb.t-wid:screen-value = string({sys/inc/k16.i eb.t-wid  } )
     eb.t-sqin:screen-value = if v-corr then string(eb.t-sqin * .007)
                             else string(eb.t-sqin / 144)
     eb.ship-name:sensitive     = no
     eb.ship-addr[1]:sensitive  = no
     eb.ship-addr[2]:sensitive  = no
     eb.ship-city:sensitive     = no
     eb.ship-state:sensitive    = no
     eb.ship-zip:sensitive      = no.

    IF eb.est-type GE 7 THEN est-qty.eqty:SCREEN-VALUE = STRING(eb.bl-qty).

    lv-eqty = est-qty.eqty:SCREEN-VALUE.
    IF eb.spare-char-2 = "Y" THEN
        ASSIGN tb-set = YES tb-set:SCREEN-VALUE = "YES".
    ELSE
        ASSIGN tb-set = NO tb-set:SCREEN-VALUE = "NO".
    IF TRIM(lv-eqty) EQ "" AND AVAIL est-qty THEN
      lv-eqty = STRING(est-qty.eqty,est-qty.eqty:FORMAT).

    btn_qty-msf:LABEL = TRIM(est-qty.eqty:LABEL) + ": " +
                        TRIM(lv-eqty) +
                        FILL(" ",10) +
                        TRIM(fi_per-set:LABEL) + ": " +
                        TRIM(STRING(fi_per-set,fi_per-set:FORMAT)) +
                        FILL(" ",10) +
                        TRIM(fi_msf:LABEL) + ": " +
                        TRIM(STRING(fi_msf,fi_msf:FORMAT)).

    btn_fgitem:LABEL = " " + TRIM(eb.stock:LABEL) + ": " /*+ TRIM(eb.stock) */.
    IF eb.stock = "" THEN
            btn_fgitem:HIDDEN  = TRUE .
    ELSE 
         btn_fgitem:HIDDEN  = FALSE .

    btn_style:LABEL = " " + TRIM(eb.style:LABEL) + ": " /*+ TRIM(eb.style)*/ .

    IF eb.style = "" THEN
            btn_style:HIDDEN  = TRUE .
    ELSE 
         btn_style:HIDDEN  = FALSE .

    btn_cust:LABEL = " " + TRIM(eb.cust-no:LABEL) + ": " /*+ TRIM(ef.board) */ .

    IF eb.cust-no = "" THEN
            btn_cust:HIDDEN  = TRUE .
    ELSE 
         btn_cust:HIDDEN  = FALSE .

    DO li = LENGTH(TRIM(ef.cad-image)) TO 1 BY -1:
      IF SUBSTR(ef.cad-image,li,1) EQ "/" OR
         SUBSTR(ef.cad-image,li,1) EQ "\" OR
         SUBSTR(ef.cad-image,li,1) EQ ":" THEN LEAVE.
      lv-image = SUBSTR(ef.cad-image,li,1) + TRIM(lv-image).
    END.
    ef.cad-image:SCREEN-VALUE = lv-image.

    IF v-cefgitem-log THEN
    DO:
/*        FIND FIRST reftable WHERE                        */
/*             reftable.reftable EQ "FGSTATUS" AND         */
/*             reftable.company  EQ cocode AND             */
/*             reftable.loc      EQ "" AND                 */
/*             reftable.code     EQ eb.stock-no            */
/*             NO-LOCK NO-ERROR.                           */
/*                                                         */
/*        IF AVAIL reftable AND reftable.code2 EQ "I" THEN */
       RUN fg/GetItemfgActInact.p (INPUT cocode,
                                   INPUT eb.stock-no,
                                   OUTPUT lActive).
       IF NOT lActive THEN
          eb.stock-no:BGCOLOR = 11.
       ELSE
          eb.stock-no:BGCOLOR = ?.

       RELEASE reftable.
    END.

    FIND FIRST b-style WHERE
         b-style.company EQ cocode AND
         b-style.style EQ eb.style:SCREEN-VALUE
         NO-LOCK NO-ERROR.

    IF AVAIL b-style AND lookup(b-style.TYPE,'P,R') > 0 THEN
    DO:
       ASSIGN
          eb.dep:LABEL = "Slot Hei"
          eb.gluelap:LABEL = "Slot Width"
          eb.wid:LABEL = "Height"
          eb.t-wid:LABEL = "Height"
          eb.dust:HIDDEN = YES
          eb.fpanel:HIDDEN = YES
          eb.lock:HIDDEN = YES
          eb.k-wid:HIDDEN = YES
          eb.k-len:HIDDEN = YES
          eb.tuck:HIDDEN = YES
          eb.adhesive:HIDDEN = YES
          eb.lin-in:HIDDEN = YES.
    END.
    ELSE
       ASSIGN
          eb.dep:LABEL = "Depth"
          eb.gluelap:LABEL = "Joint Tab Width"
          eb.t-wid:LABEL = "Blank Width"
          eb.wid:LABEL = "Width"
          eb.dust:HIDDEN = NO
          eb.fpanel:HIDDEN = NO
          eb.lock:HIDDEN = NO
          eb.k-wid:HIDDEN = NO
          eb.k-len:HIDDEN = NO
          eb.tuck:HIDDEN = NO
          eb.adhesive:HIDDEN = NO
          eb.lin-in:HIDDEN = NO.
  END.
  IF AVAIL b-style AND b-style.TYPE = "F" THEN
      ef.board:LABEL      =  "Foam" .
  ELSE IF AVAIL b-style AND b-style.TYPE = "W" THEN
      ef.board:LABEL      =  "Wood" .
  ELSE IF AVAIL b-style AND b-style.TYPE = "C" THEN
      ef.board:LABEL      =  "PolyBag" .
  ELSE
      ef.board:LABEL      =  "Board" .

  btn_board:LABEL = " " + TRIM(ef.board:LABEL) + ": " /*+ TRIM(ef.board) */ .

    IF ef.board = "" THEN
        btn_board:HIDDEN  = TRUE .
    ELSE 
        btn_board:HIDDEN  = FALSE .

  RUN get-current-values (OUTPUT lc-previous-values).              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR old-die-no LIKE eb.die-no NO-UNDO.
  DEF VAR old-cad-no LIKE eb.cad-no NO-UNDO.
  DEF VAR old-cimage LIKE ef.cad-image NO-UNDO.
  DEF VAR old-cat-no LIKE eb.procat NO-UNDO.
  DEF VAR hd1 as handle no-undo.
  DEF VAR hd2 as handle no-undo.
  DEF VAR li AS INT NO-UNDO.


{&methods/lValidateError.i YES}
  def buffer bf-eb for eb.
  DO WITH frame {&frame-name}:
      ASSIGN tb-set:SENSITIVE = FALSE
             bt-new-die:SENSITIVE = FALSE
             bt-new-plate:SENSITIVE = FALSE.
  END.

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-fi_from-est-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  IF NOT lv-foam THEN
      RUN check-flute-test-change.

    /* ==== Corrugated item validation ======== */
     ASSIGN
     hd1 = frame {&frame-name}:HANDLE
     hd1 = hd1:FIRST-CHILD
     hd2 = hd1:first-child.

     do while valid-handle(hd2):
        if hd2:type = "fill-in" and 
           hd2:data-type = "decimal" and
           decimal(hd2:screen-value) - trunc(decimal(hd2:screen-value),0) >= v-16-or-32 
           AND hd2:name NE "t-sqin" AND hd2:NAME NE "comm"
           AND hd2:NAME NE "fi_msf" AND hd2:NAME NE "fi_per-set"
        then do:
             message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                     view-as alert-box error.
             apply "entry" to hd2.
             return no-apply.
        end. 
        hd2 = hd2:next-sibling.
     end.       

  if eb.stock-no:screen-value <> "" then do:
       find first itemfg where itemfg.company = cocode and
                            itemfg.i-no = eb.stock-no:screen-value in frame {&frame-name}
                      no-lock no-error.
       if not avail itemfg then do:
         /*   message "Invalid FG Item#. Try Help.".
              return no-apply.
         */
         message "This item does not exist, would you like to add it?" view-as alert-box question
                  button yes-no update ll-ans as log.  
         if ll-ans then do:
            find xest where recid(xest) = recid(est) no-lock no-error.
            find xeb where recid(xeb) = recid(eb) no-lock no-error.
            find xef where recid(xef) = recid(ef) no-lock no-error.
            run crt-itemfg (input self:screen-value).
         end.   
         return no-apply.        
      end.  
  end.

  ASSIGN
   lv-hld-wid = eb.t-wid
   lv-hld-len = eb.t-len.

  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-ship-id NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN display-shipto.

    RUN valid-style NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-flute NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-test NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-board NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-sman NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-procat NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    DO li = 1 TO 2:
      RUN valid-prep (li) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.

    RUN valid-adhesive (eb.adhesive:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-wid-len NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.   

    RUN valid-dec(eb.len:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-dec(eb.wid:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-dec(eb.dep:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-dec(eb.lin-in:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-dec(eb.lock:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-dec(eb.tuck:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-dec(eb.dust:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-dec(eb.fpanel:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-dec(eb.gluelap:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-dec(eb.k-len:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     RUN valid-dec(eb.k-wid:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-custcsr NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF eb.ord-no NE 0 AND eb.cust-no:SCREEN-VALUE NE eb.cust-no AND
       eb.cust-no NE "" THEN
    DO:
      MESSAGE "Cannot Change Customer."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY "entry" TO eb.cust-no.
      RETURN NO-APPLY.
    END.

  /*  IF eb.t-wid:MODIFIED OR eb.t-len:MODIFIED OR 
       eb.wid:MODIFIED OR eb.len:MODIFIED OR eb.dep:MODIFIED */
    IF {sys/inc/k16.i eb.t-wid} NE DEC(eb.t-wid:SCREEN-VALUE) OR  
       {sys/inc/k16.i eb.t-len} NE DEC(eb.t-len:SCREEN-VALUE) OR
       {sys/inc/k16.i eb.wid}   NE DEC(eb.wid:SCREEN-VALUE) OR
       {sys/inc/k16.i eb.len}   NE DEC(eb.len:SCREEN-VALUE) OR
       {sys/inc/k16.i eb.dep}   NE DEC(eb.dep:SCREEN-VALUE) 
    THEN ll-blank-size-changed = YES.
  END.

  ASSIGN
   old-die-no = eb.die-no
   old-cad-no = eb.cad-no
   old-cimage = ef.cad-image  
   OLD-cat-no = eb.procat .

  RUN new-board.

   /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}: 
    DISABLE est.metric
            tab-inout
            fi_from-est-no.
  END.

  RUN dispatch ('display-fields').  /* refresh 2nd & all children pages */

  RUN release-shared-buffers.

  ll-style-is-valid = NO.

  DISABLE btncadlookup btndielookup WITH FRAME {&FRAME-NAME}.

  IF cedicad-log                  AND
     (old-die-no NE eb.die-no OR
      old-cad-no NE eb.cad-no OR
      old-cimage NE ef.cad-image) THEN
    RUN est/updiecad.p (ROWID(eb), old-die-no, old-cad-no, old-cimage, "", YES).

  IF old-cat-no NE eb.procat THEN do:
       find first itemfg where itemfg.company = cocode and
                            itemfg.i-no = eb.stock-no NO-LOCK no-error.
       IF AVAIL itemfg AND itemfg.est-no EQ eb.est-no THEN do:
           FOR EACH oe-prmtx
               WHERE oe-prmtx.company            EQ itemfg.company
               AND oe-prmtx.i-no               BEGINS itemfg.i-no
               AND SUBSTR(oe-prmtx.i-no,1,100) EQ itemfg.i-no
               EXCLUSIVE-LOCK:

               ASSIGN
                   SUBSTR(oe-prmtx.i-no,1,100) = STRING(itemfg.i-no,"X(100)")
                   oe-prmtx.procat             = itemfg.procat.

           END. /* Each oe-prmtx */
       END.
  END.
{&methods/lValidateError.i NO}

END PROCEDURE.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-board V-table-Win 
PROCEDURE new-board :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:

    IF ef.board NE ef.board:SCREEN-VALUE THEN DO:
      FIND FIRST item NO-LOCK {sys/look/itemb1W.i}
             AND item.i-no EQ ef.board:SCREEN-VALUE NO-ERROR.
      IF AVAIL item THEN do:
         ASSIGN
           ef.brd-dscr:SCREEN-VALUE = item.i-name .
           IF NOT lv-foam THEN
               ASSIGN
               eb.flute:SCREEN-VALUE = ITEM.flute
               eb.test:SCREEN-VALUE = ITEM.reg-no.
           ELSE
               ASSIGN 
               eb.flute:SCREEN-VALUE = ITEM.flute
               eb.test:SCREEN-VALUE = ITEM.reg-no.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-flute-test V-table-Win 
PROCEDURE new-flute-test :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FOR EACH item
        WHERE item.company  EQ cocode
          AND item.mat-type EQ "B"
          AND item.i-code   EQ "E"
          AND item.flute    EQ eb.flute:SCREEN-VALUE
          AND item.reg-no   EQ eb.test:SCREEN-VALUE
        USE-INDEX mat-type NO-LOCK
        BY item.i-no:
      ef.board:SCREEN-VALUE = item.i-no.
      LEAVE.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-procat V-table-Win 
PROCEDURE new-procat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND fgcat
        WHERE fgcat.company EQ cocode
          AND fgcat.procat  EQ eb.procat:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL fgcat THEN procat_desc:SCREEN-VALUE = fgcat.dscr.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-sman V-table-Win 
PROCEDURE new-sman :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-comm LIKE eb.comm NO-UNDO.
  DEF VAR lv-sman LIKE sman.sman NO-UNDO.
  DEF VAR ld-markup AS DEC NO-UNDO.

  DEF BUFFER bf-eb FOR eb.


  DO WITH FRAME {&FRAME-NAME}.
    sman_sname:SCREEN-VALUE = "".

    FIND FIRST sman
        WHERE sman.company EQ cocode
          AND sman.sman    EQ eb.sman:SCREEN-VALUE
        NO-LOCK NO-ERROR.

    IF AVAIL sman THEN DO:
      ASSIGN
       sman_sname:SCREEN-VALUE = sman.sname
       lv-sman = sman.sman.

      RELEASE bf-eb.
      IF eb.est-type EQ 6 THEN
      FIND FIRST bf-eb
          WHERE bf-eb.company EQ eb.company
            AND bf-eb.est-no  EQ eb.est-no
            AND bf-eb.form-no EQ 0
            AND bf-eb.procat  NE ""
          NO-LOCK NO-ERROR.

      RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).

      RUN sys/inc/getsmncm.p (eb.cust-no:SCREEN-VALUE,
                              INPUT-OUTPUT lv-sman,
                              (IF AVAIL bf-eb THEN bf-eb.procat ELSE eb.procat:SCREEN-VALUE),
                              ld-markup,
                              OUTPUT lv-comm).

      IF lv-comm NE 0 THEN eb.comm:SCREEN-VALUE = STRING(lv-comm).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-style V-table-Win 
PROCEDURE new-style :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST style
        WHERE style.company  EQ cocode
          AND style.style    EQ eb.style:SCREEN-VALUE
          AND style.industry EQ lv-industry
        NO-LOCK NO-ERROR.
    IF AVAIL style THEN DO:
      ASSIGN
       lv-foam           = style.type EQ "F"
       ll-wid-len-warned = NO.
      IF style.qty-per-set NE 0 THEN DO:
        fi_per-set:SCREEN-VALUE = string(style.qty-per-set).
      END.
      IF lv-foam THEN DISABLE eb.flute eb.test. ELSE ENABLE eb.flute eb.test.

      IF ll-auto-calc-selected THEN DO:
        assign eb.adhesive:SCREEN-VALUE = style.material[7]
               eb.gluelap:SCREEN-VALUE = string({sys/inc/k16.i style.dim-gl})
               /*eb.k-len:SCREEN-VALUE = string({sys/inc/k16.i style.dim-dkl})
               eb.k-wid:SCREEN-VALUE = string({sys/inc/k16.i style.dim-dkw})*/
               eb.fpanel:SCREEN-VALUE = string({sys/inc/k16.i style.dim-pan5}) 
               eb.lock:SCREEN-VALUE = string({sys/inc/k16.i style.dim-fit})
               eb.tuck:SCREEN-VALUE = string({sys/inc/k16.i style.dim-tk}).                 
               .

        RUN calc-blank-size.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE one-eb-on-ef V-table-Win 
PROCEDURE one-eb-on-ef :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAM op-one-eb AS LOG NO-UNDO.

  DEF BUFFER b-ac-eb FOR eb.
  DEF BUFFER b-ac-ef FOR ef.


  FIND b-ac-ef WHERE ROWID(b-ac-ef) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-ac-ef THEN
  FIND b-ac-eb
      WHERE b-ac-eb.company EQ b-ac-ef.company
        AND b-ac-eb.est-no  EQ b-ac-ef.est-no
        AND b-ac-eb.eqty    EQ b-ac-ef.eqty
        AND b-ac-eb.form-no EQ b-ac-ef.form-no
      NO-LOCK NO-ERROR.

  op-one-eb = AVAIL b-ac-eb.

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
  DEF VAR ll AS LOG NO-UNDO.


  {sys/inc/jobcard.i "C" }
  lv-format = sys-ctrl.char-fld.

  DO WITH frame {&frame-name}:
      ASSIGN tb-set:SENSITIVE = TRUE
             bt-new-die:SENSITIVE = TRUE
             bt-new-plate:SENSITIVE = TRUE.
  END.

  ASSIGN
   ll-blank-size-changed = NO
   ll-new-shipto         = NO
   ll-wid-len-warned     = NO.

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
         btn_style:HIDDEN = TRUE 
         btn_board:HIDDEN = TRUE 
         btn_cust:HIDDEN = TRUE .

    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ eb.company
          AND job-hdr.est-no  EQ eb.est-no
        USE-INDEX est-no:

      ll = job-hdr.i-no EQ eb.stock-no OR
           CAN-FIND(FIRST reftable
                    WHERE reftable.reftable EQ "jc/jc-calc.p"
                      AND reftable.company  EQ job-hdr.company
                      AND reftable.loc      EQ ""
                      AND reftable.code     EQ STRING(job-hdr.job,"999999999")
                      AND reftable.code2    EQ eb.stock-no).

      IF ll THEN LEAVE.
    END.

    IF NOT ll THEN
      ll = CAN-FIND(FIRST oe-ordl
                    WHERE oe-ordl.company EQ eb.company
                      AND oe-ordl.est-no  EQ eb.est-no
                      AND oe-ordl.i-no    EQ eb.stock-no
                    USE-INDEX est).

    IF ll THEN DISABLE eb.stock-no.

    IF est.est-type EQ 5 OR est.est-type EQ 8 THEN DO:
      ENABLE fi_from-est-no.
      APPLY "entry" TO fi_from-est-no.
    END.
    ELSE DISABLE fi_from-est-no.

    lv-master-est-no = fi_from-est-no.

    IF LOOKUP(lv-format,"ASI,Pacific") GT 0 THEN ENABLE est.metric.

    ENABLE tab-inout.
    RUN check-style.
    IF lv-foam THEN do:
        eb.flute:SCREEN-VALUE = "" .
        eb.test:SCREEN-VALUE = "" .
         DISABLE eb.flute eb.test.
    END.
    ELSE ENABLE  eb.flute eb.test.

    RUN shipto-enable.
    ENABLE btnDieLookup btnCadLookup.

    ef.cad-image:SCREEN-VALUE = ef.cad-image.
  END.

  RUN set-hold-values.

  RUN release-shared-buffers.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE release-shared-buffers V-table-Win 
PROCEDURE release-shared-buffers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RELEASE xest.
  RELEASE xef.
  RELEASE xeb.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-eb V-table-Win 
PROCEDURE reopen-eb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FIND CURRENT eb NO-LOCK NO-ERROR.
  IF AVAIL eb THEN RUN dispatch ('display-fields').

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
  {src/adm/template/snd-list.i "est"}
  {src/adm/template/snd-list.i "eb"}
  {src/adm/template/snd-list.i "est-qty"}
  {src/adm/template/snd-list.i "ef"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-hold-values V-table-Win 
PROCEDURE set-hold-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-hold-flute = eb.flute:SCREEN-VALUE
     lv-hold-test  = eb.test:SCREEN-VALUE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE csr-display V-table-Win 
PROCEDURE csr-display :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&frame-name}:

      FIND FIRST cust NO-LOCK
            WHERE cust.company = cocode
              AND cust.cust-no = eb.cust-no:SCREEN-VALUE NO-ERROR.
     
       IF AVAIL cust THEN
           est.csrUser_id:SCREEN-VALUE = cust.csrUser_id .

  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE shipto-enable V-table-Win 
PROCEDURE shipto-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&frame-name}:
    IF ll-new-shipto                            OR
       (eb.cust-no:SCREEN-VALUE EQ "TEMP" AND
        eb.ship-id:SCREEN-VALUE EQ "TEMP")      THEN
      ENABLE
       eb.ship-name
       eb.ship-addr[1]
       eb.ship-addr[2]
       eb.ship-city
       eb.ship-state
       eb.ship-zip.
    ELSE
      DISABLE
       eb.ship-name
       eb.ship-addr[1]
       eb.ship-addr[2]
       eb.ship-city
       eb.ship-state
       eb.ship-zip.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tokenize-proc V-table-Win 
PROCEDURE tokenize-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ip-string AS CHAR NO-UNDO.

DEF VAR lv-tmp AS CHAR NO-UNDO.
DEF VAR lv-tmp-val AS CHAR NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.

v-l-array = 0.

DO i = 1 TO LENGTH(ip-string):
   lv-tmp-val = SUBSTRING(ip-string,i,1).
   IF lv-tmp-val <> " " THEN
      lv-tmp = lv-tmp + lv-tmp-val.
   ELSE IF lv-tmp <> "" THEN
      ASSIGN
         v-count = v-count + 1
         v-l-array[v-count] = dec(lv-tmp)
         lv-tmp = "".
END.

IF lv-tmp <> "" THEN
   ASSIGN
      v-count = v-count + 1
      v-l-array[v-count] = dec(lv-tmp).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-sheet V-table-Win 
PROCEDURE update-sheet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  find xef where recid(xef) = recid(ef).
  find xeb where recid(xeb) = recid(eb).

   assign xef.n-out  = 0
          xef.n-out-l = 0
          xef.n-out-d = 0
          xef.gsh-len = 0
          xef.gsh-wid = 0
          xef.gsh-dep = 0
          xef.nsh-len = 0
          xef.nsh-wid = 0
          xef.nsh-dep = 0
          xef.trim-w = 0
          xef.trim-l = 0
          xef.trim-d = 0
          xeb.num-len = 0
          xeb.num-wid = 0
          xeb.num-dep =0.

  /*     find xest where recid(xest) = recid(est).  */

  IF NOT lv-foam THEN DO:
    {sys/inc/ceroute1.i w id l en}
  END.

  RUN cec/calc-dim.p .

  IF ceroute-chr NE "" THEN DO:
    FIND FIRST mach
        WHERE mach.company EQ est.company
          AND mach.loc     EQ est.loc
          AND mach.m-code  EQ ceroute-chr
          AND mach.dept[1] EQ "CR"
        NO-LOCK NO-ERROR.
    IF AVAIL mach THEN DO:
      ASSIGN
       xef.m-code   = ceroute-chr
       xef.lsh-lock = NO
       xeb.num-wid  = 1
       xeb.num-len  = 1.

      RUN cec/calc-dim1.p NO-ERROR.

      ASSIGN
       xef.gsh-len = xef.gsh-len - (xef.nsh-len * xef.n-out-l)
       xef.n-out-l = 1
       xef.gsh-len = xef.gsh-len + (xef.nsh-len * xef.n-out-l).

      IF ceroute-int NE 0 AND ceroute-int LT xef.gsh-wid THEN
        ASSIGN
         xef.n-out   = TRUNC(ceroute-int / xef.nsh-wid,0)
         xef.gsh-wid = xef.n-out * xef.nsh-wid + (mach.min-trimw * 2).
    END.
  END.

  find xef where recid(xef) = recid(ef) no-lock.
  find xeb where recid(xeb) = recid(eb) no-lock.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-64-dec V-table-Win 
PROCEDURE valid-64-dec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-dec AS DEC DECIMALS 6 NO-UNDO.
   DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.
   DEFINE OUTPUT PARAMETER op-dec AS DEC DECIMALS 6 NO-UNDO.

  {methods/lValidateError.i YES}
    FIND FIRST tt-64-dec WHERE
      substring(string(tt-64-dec.DEC),1,3) EQ substring(string(ip-dec),1,3) NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-64-dec  THEN
      op-error = YES.
    ELSE  op-dec = tt-64-dec.DEC .

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-adhesive V-table-Win 
PROCEDURE valid-adhesive :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-field AS WIDGET-HANDLE NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    ip-field:SCREEN-VALUE = CAPS(ip-field:SCREEN-VALUE).

    IF TRIM(ip-field:SCREEN-VALUE) NE ""                         AND
       NOT CAN-FIND(FIRST item
                    WHERE item.company  EQ cocode
                      AND item.i-no     EQ ip-field:SCREEN-VALUE
                      AND CAN-DO("G,S,T",item.mat-type)
                      /*AND item.industry EQ lv-industry*/)          THEN DO:
      MESSAGE "Invalid " + TRIM(ip-field:LABEL) + ", try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-field.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-board V-table-Win 
PROCEDURE valid-board :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    ef.board:SCREEN-VALUE = CAPS(ef.board:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST item
                    {sys/look/itemb1W.i}
                      AND item.i-no EQ ef.board:SCREEN-VALUE) OR
       ef.board:SCREEN-VALUE EQ ""                            THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ef.board.
      RETURN ERROR.
    END.

    IF ef.brd-dscr:SCREEN-VALUE EQ "" THEN RUN new-board.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-custcsr V-table-Win 
PROCEDURE valid-custcsr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}

   IF est.csrUser_id:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN DO:
       IF NOT CAN-FIND(FIRST users WHERE users.USER_ID EQ est.csrUser_id:SCREEN-VALUE IN FRAME {&FRAME-NAME})
       THEN DO:
           MESSAGE "Invalid customer CSR. Try help." VIEW-AS ALERT-BOX ERROR.
           RETURN ERROR.
       END.
   END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-dec V-table-Win 
PROCEDURE valid-dec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
   /*DEFINE INPUT PARAMETER p-dec AS DEC DECIMALS 6 NO-UNDO.*/
    DEF INPUT PARAM ip-field AS WIDGET-HANDLE NO-UNDO.

   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR len-num AS INT NO-UNDO.

   v-dec = decimal(ip-field:SCREEN-VALUE) - trunc(decimal(ip-field:SCREEN-VALUE),0).

   IF v-cecscrn-dec THEN DO:
       len-num = INT(ip-field:screen-value) .
       RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
       IF op-error THEN DO:
           MESSAGE "Invalid Dimension."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           APPLY "entry" TO ip-field .
           RETURN ERROR.
       END.
       ELSE do: 
         /* ip-field:screen-value = string( len-num +  op-dec) . */
      END.

   END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-fi_from-est-no V-table-Win 
PROCEDURE valid-fi_from-est-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-master-est AS LOG NO-UNDO.

  DEF BUFFER b-est FOR est.
  DEF BUFFER b-eb FOR eb.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    fi_from-est-no:SCREEN-VALUE =
        STRING(INT(fi_from-est-no:SCREEN-VALUE),">>>>>>>>") NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
      APPLY "entry" TO fi_from-est-no.
      RETURN ERROR.
    END.

    IF TRIM(fi_from-est-no:SCREEN-VALUE) NE TRIM(lv-master-est-no) AND
       INT(fi_from-est-no:SCREEN-VALUE) NE 0                       THEN DO:
      FIND FIRST b-est
          WHERE b-est.company  EQ cocode
            AND b-est.est-no   EQ fi_from-est-no:SCREEN-VALUE
          NO-LOCK NO-ERROR.
      IF AVAIL b-est THEN DO:
        RUN ce/com/istandem.p (ROWID(b-est), OUTPUT ll-master-est).

        IF ll-master-est THEN
        FOR EACH b-eb OF b-est
            WHERE b-eb.master-est-no NE ""
            NO-LOCK:
          ll-master-est = NO.
          LEAVE.
        END.
      END.

      IF NOT ll-master-est                                                OR
         INT(fi_from-est-no:SCREEN-VALUE) GE INT(est.est-no:SCREEN-VALUE) THEN DO:
        MESSAGE TRIM(fi_from-est-no:LABEL) + " Est# is invalid..."
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO fi_from-est-no.
        RETURN ERROR.
      END.
    END.
  END.

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
  IF NOT lv-foam THEN DO:
    {est/valflute.i "eb.flute" ":SCREEN-VALUE"}
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-prep V-table-Win 
PROCEDURE valid-prep :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-type AS INT NO-UNDO.

  DEF VAR lv-code LIKE prep.code NO-UNDO.
  DEF VAR lv-labl AS CHAR NO-UNDO.
  DEF VAR ll AS LOG INIT YES NO-UNDO.


  {methods/lValidateError.i YES}
  IF addprep-log THEN
  DO WITH FRAME {&FRAME-NAME}:
    IF ip-type EQ 1 THEN
      ASSIGN
       lv-code = eb.die-no:SCREEN-VALUE
       lv-labl = eb.die-no:LABEL.
    ELSE
      ASSIGN
       lv-code = eb.plate-no:SCREEN-VALUE
       lv-labl = eb.plate-no:LABEL.

    IF lv-code NE "" THEN
    DO WHILE NOT CAN-FIND(FIRST prep
                          WHERE prep.company EQ cocode
                            AND prep.code    EQ lv-code):

      IF NOT ll THEN DO:
        IF ip-type EQ 1 THEN APPLY "entry" TO eb.die-no.
                        ELSE APPLY "entry" TO eb.plate-no.
        RETURN ERROR.
      END.

      MESSAGE TRIM(lv-labl) + ": " + TRIM(lv-code) +
              " doesn't exist in Prep File, do you want to add it?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.
      IF ll THEN DO:
        RUN windows/prepfly.w (INPUT-OUTPUT lv-code, ROWID(eb), ip-type) NO-ERROR.
        IF lv-code GT "" THEN
          IF ip-type EQ 1 THEN eb.die-no:SCREEN-VALUE   = lv-code.
                          ELSE eb.plate-no:SCREEN-VALUE = lv-code.
        ELSE ll = NO.
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
    eb.procat:SCREEN-VALUE = CAPS(eb.procat:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST fgcat
                    WHERE fgcat.company EQ cocode
                      AND fgcat.procat  EQ eb.procat:SCREEN-VALUE) OR
       eb.procat:SCREEN-VALUE EQ ""                                THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO eb.procat.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ship-id V-table-Win 
PROCEDURE valid-ship-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST shipto
                    WHERE shipto.company EQ cocode
                      AND shipto.cust-no EQ eb.cust-no:SCREEN-VALUE
                      AND shipto.ship-id EQ eb.ship-id:SCREEN-VALUE) AND
       NOT ll-new-shipto                                             THEN DO:
      MESSAGE "            Invalid entry, try help...             " SKIP(1)
              "                        OR                         " SKIP(1)
              "Do you wish to add this Shipto ID to this Customer?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll-new-shipto.
      IF NOT ll-new-shipto THEN DO:
        APPLY "entry" TO eb.ship-id.
        RETURN ERROR.
      END.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sman V-table-Win 
PROCEDURE valid-sman :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
   FIND FIRST sman
        WHERE sman.company EQ cocode
          AND sman.sman    EQ eb.sman:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        NO-LOCK NO-ERROR.

    IF NOT AVAIL sman THEN DO:
       MESSAGE "Invalid Sales Rep. Try help." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO eb.sman.
       RETURN ERROR.
    END.
    sman_sname:SCREEN-VALUE = sman.sNAME.

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
    IF NOT CAN-FIND(FIRST style
                    WHERE style.company  EQ cocode
                      AND style.style    EQ eb.style:SCREEN-VALUE
                      AND style.industry EQ lv-industry) OR
       eb.style:SCREEN-VALUE EQ ""               THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO eb.style.
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

  {methods/lValidateError.i YES}
  IF NOT lv-foam THEN DO:
    {est/valtest.i "eb.flute" "eb.test" ":SCREEN-VALUE"}
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-wid-len V-table-Win 
PROCEDURE valid-wid-len :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-handle AS HANDLE NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    lv-handle = IF LOOKUP(FOCUS:NAME,"style,len") GT 0 THEN FOCUS ELSE ?.
    IF NOT VALID-HANDLE(lv-handle) THEN lv-handle = eb.wid:HANDLE.

    IF ll-warn AND ll-wid-len-warned EQ NO                         AND
       CAN-FIND(FIRST style
                WHERE style.company  EQ cocode
                  AND style.style    EQ eb.style:SCREEN-VALUE
                  AND style.industry EQ lv-industry
                  AND INDEX("DF",style.type) LE 0)                 AND
       (eb.style:SCREEN-VALUE    NE eb.style OR
        {sys/inc/k16bv.i "DEC(eb.wid:SCREEN-VALUE)"} NE eb.wid OR
        {sys/inc/k16bv.i "DEC(eb.len:SCREEN-VALUE)"} NE eb.len)    AND
        {sys/inc/k16bv.i "DEC(eb.wid:SCREEN-VALUE)"} GT
        {sys/inc/k16bv.i "DEC(eb.len:SCREEN-VALUE)"}               THEN DO:
      MESSAGE "This is an abnormal box, carton width should not be"
              "greater than length." SKIP
              "Would you like to continue with abnormal box?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll-wid-len-warned.
      IF NOT ll-wid-len-warned THEN DO:
        APPLY "entry" TO lv-handle.
        RETURN ERROR.
      END.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE was-modified V-table-Win 
PROCEDURE was-modified :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER opl-was-modified AS LOG.
RUN get-current-values (OUTPUT lc-new-values).
IF lc-new-values = lc-previous-values THEN DO:
    opl-was-modified = NO.
    /*  RUN local-cancel-record. */
END.
ELSE
  opl-was-modified = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ImageName V-table-Win 
FUNCTION ImageName RETURNS CHARACTER
  (ipImageFileName AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  ASSIGN
    ipImageFileName = SUBSTR(ipImageFileName,1,R-INDEX(ipImageFileName,'.') - 1)
    ipImageFileName = SUBSTR(ipImageFileName,R-INDEX(ipImageFileName,'\') + 1).
  RETURN ipImageFileName.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

