&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: ce\v-est.w

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
&scoped-define PROC-ENABLE PROC-ENABLE

/*def new shared var formule as dec extent 12 NO-UNDO.*/
DEF NEW SHARED TEMP-TABLE formule FIELD formule AS DEC EXTENT 12.

def var char-val as cha no-undo.
def var lv-part-no-prev like eb.part-no no-undo.
&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.
def var k_frac as dec init "6.25" no-undo.
def var ll-auto-calc-selected as log no-undo.
def var lv-sqin as dec no-undo.
def var lv-panels as log no-undo.
def var ll-warn as log no-undo.
DEF VAR ll-wid-len-warned AS LOG NO-UNDO.
DEF VAR ld-k-wid-array LIKE eb.k-wid-array NO-UNDO.
DEF VAR ld-k-len-array LIKE eb.k-len-array NO-UNDO.
DEF TEMP-TABLE tt-array FIELD tt-dec AS DEC FIELD tt-type AS CHAR.

def new shared var cocode as cha no-undo.
def new shared var locode as cha no-undo.

{custom/globdefs.i}

assign cocode = g_company
       locode = g_loc.

def new shared buffer xest    for est.
def new shared buffer xef     for ef.
def new shared buffer xeb     for eb.

{cec/descalc.i "new"}
def var lv-foam as log no-undo.
def var lv-industry like item.industry init "1".
def var lv-i-code   like item.i-code   init "B".
DEF VAR ll-new-shipto AS LOG NO-UNDO.
DEF VAR ll-blank-size-changed AS LOG NO-UNDO.
DEF VAR lv-format AS cha NO-UNDO.
DEF VAR ll-style-is-valid AS LOG NO-UNDO.
DEF VAR ll-one-eb-on-ef AS LOG NO-UNDO.
def var lv-hld-wid like eb.t-wid no-undo.
def var lv-hld-len like eb.t-len no-undo.
DEF VAR lv-master-est-no LIKE eb.master-est-no NO-UNDO.
DEFINE VARIABLE dieFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cadFile AS CHARACTER NO-UNDO.

{custom/framechk.i NEW}

DO TRANSACTION:
  {sys/inc/addprep.i}
  {sys/inc/ceroute.i F}
  {sys/inc/cestyle.i F}
  {sys/inc/cedicad.i F}
  {sys/inc/cefgitem.i}
  {sys/inc/graphic.i}
  {sys/inc/shiptorep.i}
END.

{sys/inc/f16to32.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fold

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES est eb ef
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est, eb, ef.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS eb.cust-no eb.ship-id est.csrUser_id eb.sman eb.comm ~
eb.procat eb.part-no eb.stock-no eb.part-dscr1 eb.part-dscr2 eb.die-no ~
ef.cad-image eb.cad-no eb.plate-no eb.spc-no eb.upc-no eb.style est.metric ~
ef.board eb.len eb.wid eb.dep eb.adhesive eb.dust eb.fpanel eb.lock ~
eb.gluelap eb.k-len eb.k-wid eb.tuck eb.lin-in eb.t-wid eb.t-len eb.t-sqin ~
eb.bl-qty 
&Scoped-define ENABLED-TABLES eb ef est
&Scoped-define FIRST-ENABLED-TABLE eb
&Scoped-define SECOND-ENABLED-TABLE ef
&Scoped-define THIRD-ENABLED-TABLE est
&Scoped-Define ENABLED-OBJECTS btn_fgitem btn_from btn_style btn_board ~
btn_cust RECT-18 RECT-19 RECT-23 RECT-24 
&Scoped-Define DISPLAYED-FIELDS est.est-no eb.form-no est.form-qty ~
eb.blank-no est.mod-date est.ord-date eb.cust-no eb.ship-id eb.ship-name ~
eb.ship-addr[1] eb.ship-addr[2] eb.ship-city eb.ship-state eb.ship-zip est.csrUser_id ~
eb.sman eb.comm eb.procat eb.part-no eb.stock-no eb.part-dscr1 ~
eb.part-dscr2 eb.die-no ef.cad-image eb.cad-no eb.plate-no eb.spc-no ~
eb.upc-no eb.style est.metric ef.board ef.brd-dscr eb.len eb.wid eb.dep ~
eb.adhesive eb.dust eb.fpanel eb.lock eb.gluelap eb.k-len eb.k-wid eb.tuck ~
eb.lin-in eb.t-wid eb.t-len eb.t-sqin eb.bl-qty eb.ord-no 
&Scoped-define DISPLAYED-TABLES est eb ef
&Scoped-define FIRST-DISPLAYED-TABLE est
&Scoped-define SECOND-DISPLAYED-TABLE eb
&Scoped-define THIRD-DISPLAYED-TABLE ef
&Scoped-Define DISPLAYED-OBJECTS fi_from-est-no fi_blank-qty sman_sname ~
procat_desc style_dscr 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS fi_from-est-no eb.ship-name ~
eb.ship-addr[1] eb.ship-addr[2] eb.ship-city eb.ship-state eb.ship-zip ~
style_dscr est.metric ef.brd-dscr 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD imageName V-table-Win 
FUNCTION imageName RETURNS CHARACTER
  (ipImageFileName AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCadLookup 
     IMAGE-UP FILE "Graphics/16x16/find.bmp":U
     LABEL "" 
     SIZE 4.4 BY 1.05.

DEFINE BUTTON btnDieLookup 
     IMAGE-UP FILE "Graphics/16x16/find.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1.05.

DEFINE BUTTON btn_board 
     LABEL "" 
     SIZE 10 BY 1.

DEFINE BUTTON btn_cust 
     LABEL "" 
     SIZE 10 BY 1.

DEFINE BUTTON btn_fgitem 
     LABEL "" 
     SIZE 14 BY 1.

DEFINE BUTTON btn_from 
     LABEL "From:" 
     SIZE 7.6 BY 1.

DEFINE BUTTON btn_style 
     LABEL "" 
     SIZE 15 BY 1.

DEFINE VARIABLE fi_blank-qty AS INTEGER FORMAT ">9" INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE fi_from-est-no AS CHARACTER FORMAT "X(8)" 
     LABEL "From" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

DEFINE VARIABLE procat_desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE sman_sname AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE style_dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 152 BY 7.14.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 156 BY 16.91.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 7.86.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 7.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fold
     btnDieLookup AT ROW 6.95 COL 74
     est.est-no AT ROW 1.24 COL 10.2 COLON-ALIGNED
          LABEL "Est #" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     fi_from-est-no AT ROW 1.24 COL 30 COLON-ALIGNED
     eb.form-no AT ROW 1.24 COL 48.2 COLON-ALIGNED
          LABEL "Frm" FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     est.form-qty AT ROW 1.24 COL 56 COLON-ALIGNED NO-LABEL FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     eb.blank-no AT ROW 1.24 COL 66.2 COLON-ALIGNED
          LABEL "Blk"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     fi_blank-qty AT ROW 1.24 COL 73.8 COLON-ALIGNED NO-LABEL
     est.mod-date AT ROW 1.24 COL 86.2 COLON-ALIGNED
          LABEL "Mod"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     est.ord-date AT ROW 1.24 COL 138.4 COLON-ALIGNED
          LABEL "Date"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     eb.cust-no AT ROW 2.67 COL 21 COLON-ALIGNED
          LABEL "Cust#"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          FONT 6
     eb.ship-id AT ROW 2.67 COL 48 COLON-ALIGNED
          LABEL "Ship To"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          FONT 6
     eb.ship-name AT ROW 3.80 COL 21 COLON-ALIGNED
          LABEL "Company"
          VIEW-AS FILL-IN 
          SIZE 46 BY 1
          FONT 6
     eb.ship-addr[1] AT ROW 4.70 COL 21 COLON-ALIGNED
          LABEL "Address"
          VIEW-AS FILL-IN 
          SIZE 46 BY 1
     eb.ship-addr[2] AT ROW 5.55 COL 21 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 46 BY 1
     eb.ship-city AT ROW 6.45 COL 21 COLON-ALIGNED
          LABEL "City/State/Zip"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     eb.ship-state AT ROW 6.45 COL 44.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     eb.ship-zip AT ROW 6.45 COL 50.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16.2 BY 1
     est.csrUser_id AT ROW 7.30 COL 22 COLON-ALIGNED
          LABEL "CSR"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     eb.sman AT ROW 8.14 COL 21 COLON-ALIGNED
          LABEL "Sales Rep"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     sman_sname AT ROW 8.14 COL 28 COLON-ALIGNED NO-LABEL
     eb.comm AT ROW 8.14 COL 61 COLON-ALIGNED
          LABEL "%"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     eb.procat AT ROW 9.1 COL 21 COLON-ALIGNED
          LABEL "FG Category" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          FONT 6
     procat_desc AT ROW 9.1 COL 31 COLON-ALIGNED NO-LABEL
     eb.part-no AT ROW 3.86 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          FONT 6
     eb.stock-no AT ROW 3.86 COL 128.2 COLON-ALIGNED
          LABEL "FG Item#"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          FONT 6
     eb.part-dscr1 AT ROW 5.05 COL 86 COLON-ALIGNED
          LABEL "Item Name"
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
          FONT 6
     eb.part-dscr2 AT ROW 6 COL 86 COLON-ALIGNED
          LABEL "Description"
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
          FONT 6
     eb.die-no AT ROW 6.95 COL 86 COLON-ALIGNED HELP
          ""
          LABEL "Die #" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fold
     ef.cad-image AT ROW 6.95 COL 128.2 COLON-ALIGNED HELP
          "Filename of the Die image"
          LABEL "Image" FORMAT "x(80)"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     eb.cad-no AT ROW 7.91 COL 110 RIGHT-ALIGNED
          LABEL "CAD#"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          FONT 6
     eb.plate-no AT ROW 7.91 COL 128.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          FONT 6
     eb.spc-no AT ROW 8.86 COL 86 COLON-ALIGNED
          LABEL "SPC/QC #"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     eb.upc-no AT ROW 8.86 COL 121.2 COLON-ALIGNED
          LABEL "UPC#" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          FONT 6
     eb.style AT ROW 10.52 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     style_dscr AT ROW 10.52 COL 37 COLON-ALIGNED NO-LABEL
     est.metric AT ROW 10.52 COL 136 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     ef.board AT ROW 11.71 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     ef.brd-dscr AT ROW 11.71 COL 49 COLON-ALIGNED NO-LABEL FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 57 BY 1
     eb.len AT ROW 12.91 COL 25 COLON-ALIGNED
          LABEL "Length"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.wid AT ROW 12.91 COL 57 COLON-ALIGNED
          LABEL "Width"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.dep AT ROW 12.91 COL 88 COLON-ALIGNED
          LABEL "Depth"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.adhesive AT ROW 12.91 COL 126 COLON-ALIGNED
          LABEL "Adhesive"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     eb.dust AT ROW 13.86 COL 25 COLON-ALIGNED
          LABEL "Top/Dust Flap"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.fpanel AT ROW 13.86 COL 57 COLON-ALIGNED
          LABEL "Fifth Panel"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.lock AT ROW 13.86 COL 88 COLON-ALIGNED
          LABEL "Lock Tab"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.gluelap AT ROW 13.86 COL 126 COLON-ALIGNED
          LABEL "Glue Lap"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     eb.k-len AT ROW 14.76 COL 24.8 COLON-ALIGNED
          LABEL "DK Length"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.k-wid AT ROW 14.76 COL 57 COLON-ALIGNED
          LABEL "DK Width"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.tuck AT ROW 14.81 COL 88 COLON-ALIGNED
          LABEL "Tuck"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.lin-in AT ROW 14.81 COL 126 COLON-ALIGNED
          LABEL "Lin Inches"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     eb.t-wid AT ROW 16 COL 25 COLON-ALIGNED
          LABEL "Blank Width"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.t-len AT ROW 16 COL 57 COLON-ALIGNED
          LABEL "Blank Length"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.t-sqin AT ROW 16 COL 126 COLON-ALIGNED
          LABEL "Blank Sq. In." FORMAT ">>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fold
     eb.bl-qty AT ROW 2.67 COL 86 COLON-ALIGNED
          LABEL "Qty" FORMAT ">>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     btnCadLookup AT ROW 7.91 COL 74
     eb.ord-no AT ROW 1.24 COL 117 COLON-ALIGNED
          LABEL "Last Order#" FORMAT ">>>>>>>>"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     btn_fgitem AT ROW 3.81 COL 115 WIDGET-ID 16
     btn_from AT ROW 1.19 COL 24.8 WIDGET-ID 16
     btn_style AT ROW 10.52 COL 11 WIDGET-ID 16
     btn_board AT ROW 11.71 COL 16 WIDGET-ID 16
     btn_cust AT ROW 2.67 COL 12 WIDGET-ID 16
     "of" VIEW-AS TEXT
          SIZE 3 BY .95 AT ROW 1.24 COL 72.6
     "of" VIEW-AS TEXT
          SIZE 3 BY .95 AT ROW 1.24 COL 54.6
     RECT-18 AT ROW 10.29 COL 4
     RECT-19 AT ROW 1 COL 1
     RECT-23 AT ROW 2.43 COL 73
     RECT-24 AT ROW 2.43 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.est,ASI.eb,ASI.ef
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
         HEIGHT             = 17.24
         WIDTH              = 157.2.
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
/* SETTINGS FOR FRAME fold
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME fold:SCROLLABLE       = FALSE
       FRAME fold:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN eb.adhesive IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.bl-qty IN FRAME fold
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN eb.blank-no IN FRAME fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ef.brd-dscr IN FRAME fold
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR BUTTON btnCadLookup IN FRAME fold
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnDieLookup IN FRAME fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ef.cad-image IN FRAME fold
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN eb.cad-no IN FRAME fold
   ALIGN-R EXP-LABEL                                                    */
/* SETTINGS FOR FILL-IN eb.comm IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.cust-no IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.dep IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.die-no IN FRAME fold
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
ASSIGN 
       eb.die-no:PRIVATE-DATA IN FRAME fold     = 
                "framechk".

/* SETTINGS FOR FILL-IN eb.dust IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN est.est-no IN FRAME fold
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN fi_blank-qty IN FRAME fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_from-est-no IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.form-no IN FRAME fold
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN est.form-qty IN FRAME fold
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN eb.fpanel IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.gluelap IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.k-len IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.k-wid IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.len IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.lin-in IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.lock IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN est.metric IN FRAME fold
   2                                                                    */
/* SETTINGS FOR FILL-IN est.mod-date IN FRAME fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN est.ord-date IN FRAME fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN eb.ord-no IN FRAME fold
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN eb.part-dscr1 IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.part-dscr2 IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.procat IN FRAME fold
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN procat_desc IN FRAME fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.ship-addr[1] IN FRAME fold
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.ship-addr[2] IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.ship-city IN FRAME fold
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.ship-id IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.ship-name IN FRAME fold
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.ship-state IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.ship-zip IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN est.csrUser_id IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.sman IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN sman_sname IN FRAME fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.spc-no IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.stock-no IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style_dscr IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.t-len IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.t-sqin IN FRAME fold
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN eb.t-wid IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.tuck IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.upc-no IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.wid IN FRAME fold
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fold
/* Query rebuild information for FRAME fold
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME fold */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME fold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fold V-table-Win
ON HELP OF FRAME fold
DO:
   def var lv-handle as widget-handle no-undo.
   def var ls-cur-val as cha no-undo.
   def var lv-eb-tmpid as recid no-undo.
   DEF VAR lv-prep-type AS cha NO-UNDO.
   def var lv-rowid as rowid no-undo.
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
           run windows/l-stylef.w (cocode,ls-cur-val, output char-val).
           if char-val <> "" AND lw-focus:SCREEN-VALUE NE entry(1,char-val) then do:
              lw-focus:screen-value =  entry(1,char-val).
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
       when "Board" then do:
           def var lv-ind like style.industry no-undo.
           ls-cur-val = lw-focus:screen-value.
           find style where style.company = cocode and
                            style.style = eb.style:screen-value in frame {&frame-name}
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           if avail style and style.type = "f" then  /* foam */
                 run windows/l-boardf.w (cocode,lv-ind,ls-cur-val,output char-val).
           else run windows/l-board1.w (eb.company,lv-ind,lw-focus:screen-value, output lv-rowid).
           FIND FIRST ITEM WHERE ROWID(item) EQ lv-rowid NO-LOCK NO-ERROR.
           IF AVAIL ITEM THEN DO:
             ASSIGN 
              ef.board:screen-value in frame {&frame-name} = item.i-no
              ef.brd-dscr:screen-value in frame {&frame-name} = item.i-name.
             IF ITEM.i-no NE lw-focus:SCREEN-VALUE THEN RUN new-board.
           END.  
       end.
       when "cust-no" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-cust.w (cocode,ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
              RUN csr-display .
              find first shipto where shipto.company = cocode
                                  and shipto.cust-no = lw-focus:screen-value
                                  no-lock no-error.
               eb.ship-id:screen-value = if avail shipto then shipto.ship-id else "".
               if v-shiptorep-log AND AVAIL shipto AND shipto.spare-char-1 <> "" THEN do:   /* task 05301401 */
                   eb.sman:SCREEN-VALUE = shipto.spare-char-1 .
                   run new-sman.
               END.

           end.
       end.  /* cust-no*/
       when "plate-no" or when "die-no" then do:
           /*run windows/l-matpr.w  (cocode,lw-focus:screen-value, output char-val). */
           lv-prep-type = IF lw-focus:NAME = "plate-no" THEN "P" ELSE "D,F,R".
           RUN windows/l-diepl.w (cocode,lv-prep-type,lw-focus:SCREEN-VALUE, OUTPUT char-val).
           if char-val <> "" then 
              lw-focus:screen-value = entry(1,char-val).
       end.
       when "cad-no" then do:
           run windows/l-itemfc.w  (cocode,lw-focus:screen-value, output char-val). 
           if char-val <> "" then 
              lw-focus:screen-value = entry(1,char-val).
       end.
       when "upc-no" then do:     
           run windows/l-itemfu.w  (cocode,lw-focus:screen-value, output char-val). 
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

       when "spc-no" then do:
           run windows/l-itemfs.w  (cocode,lw-focus:screen-value, output char-val). 
           if char-val <> "" then 
              lw-focus:screen-value = entry(1,char-val).
       end.  
       WHEN "adhesive" THEN DO:
           RUN windows/l-item.w (cocode,"","G,T",lw-focus:SCREEN-VALUE,OUTPUT char-val).
           IF char-val <> "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
       END.
       WHEN "cad-image"THEN DO:
           APPLY "choose" TO btnDieLookup.
       END.
       when "csrUser_id" then do:
         run windows/l-users.w (est.csrUser_id:SCREEN-VALUE in frame {&frame-name}, output char-val).
           if char-val <> "" then 
              assign est.csrUser_id:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.
       END.
  end case. 

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.adhesive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.adhesive V-table-Win
ON LEAVE OF eb.adhesive IN FRAME fold /* Adhesive */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-adhesive (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME est.csrUser_id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est.csrUser_id V-table-Win
ON LEAVE OF est.csrUser_id IN FRAME fold /* Type */
DO:
  
  IF LASTKEY <> -1 THEN DO:
     RUN valid-custcsr NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.board V-table-Win
ON LEAVE OF ef.board IN FRAME fold /* Board */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-board NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.board V-table-Win
ON VALUE-CHANGED OF ef.board IN FRAME fold /* Board */
DO:
  RUN new-board.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCadLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCadLookup V-table-Win
ON CHOOSE OF btnCadLookup IN FRAME fold
DO:
  DEFINE VARIABLE initDir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE okClicked AS LOGICAL NO-UNDO.

  FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ cocode
                                AND sys-ctrl.name EQ 'CADFILE' NO-ERROR.
  IF NOT AVAILABLE sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN sys-ctrl.company = cocode
           sys-ctrl.name    = 'CADFILE'
           sys-ctrl.descrip = 'Dictate the location of the cad image to search.'
           sys-ctrl.char-fld = '.\'.
    FIND CURRENT sys-ctrl NO-LOCK.
  END.
  ASSIGN
    initDir = sys-ctrl.char-fld
    cadfile = ''.

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
      ASSIGN eb.cad-no:SCREEN-VALUE = imageName(cadfile).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDieLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDieLookup V-table-Win
ON CHOOSE OF btnDieLookup IN FRAME fold
DO:
  DEFINE VARIABLE initDir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE okClicked AS LOGICAL NO-UNDO.
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
            ef.cad-image:SCREEN-VALUE = diefile /*imageName(dieFile)*/.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_board V-table-Win
ON CHOOSE OF btn_board IN FRAME fold
DO:
  IF AVAIL eb THEN
   FIND FIRST ITEM WHERE ITEM.company  = cocode
       AND ITEM.i-no = ef.board NO-LOCK NO-ERROR.

   IF AVAIL ITEM THEN
   RUN windows/item-fe.w(RECID(ITEM)) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_cust V-table-Win
ON CHOOSE OF btn_cust IN FRAME fold
DO:
  IF AVAIL eb THEN
   FIND FIRST cust WHERE cust.company  = cocode
       AND cust.cust-no = eb.cust-no NO-LOCK NO-ERROR.

   IF AVAIL cust THEN
   RUN windows/v-cust.w(RECID(cust)) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_fgitem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_fgitem V-table-Win
ON CHOOSE OF btn_fgitem IN FRAME fold
DO:
  IF AVAIL eb THEN
   FIND FIRST itemfg WHERE itemfg.company  = cocode
       AND itemfg.i-no = eb.stock-no NO-LOCK NO-ERROR.

   IF AVAIL itemfg THEN
   RUN oe/w-estfg.w(RECID(eb)) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_from
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_from V-table-Win
ON CHOOSE OF btn_from IN FRAME fold /* From: */
DO:
    DEF BUFFER bf-est FOR est.
  IF AVAIL eb THEN
   FIND FIRST bf-est WHERE bf-est.company  = cocode
       AND bf-est.est-no = FILL(" ",8 - LENGTH(TRIM(INPUT fi_from-est-no))) + TRIM(INPUT fi_from-est-no) NO-LOCK NO-ERROR.

   IF AVAIL bf-est THEN
   RUN est/w-estesf.w(RECID(bf-est)) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_style V-table-Win
ON CHOOSE OF btn_style IN FRAME fold
DO:
  IF AVAIL eb THEN
   FIND FIRST style WHERE style.company  = cocode
       AND style.style = eb.style NO-LOCK NO-ERROR.

   IF AVAIL style THEN
   RUN windows/stylef-e.w(RECID(style)) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cust-no V-table-Win
ON ENTRY OF eb.cust-no IN FRAME fold /* Cust# */
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
ON VALUE-CHANGED OF eb.cust-no IN FRAME fold /* Cust# */
DO:
  RUN shipto-enable. 
  RUN csr-display .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.die-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.die-no V-table-Win
ON LEAVE OF eb.die-no IN FRAME fold /* Die # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-prep (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_from-est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_from-est-no V-table-Win
ON LEAVE OF fi_from-est-no IN FRAME fold /* From */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-fi_from-est-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.fpanel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.fpanel V-table-Win
ON LEAVE OF eb.fpanel IN FRAME fold /* Fifth Panel */
DO:
  IF ll-auto-calc-selected and
     {&self-name} <> DEC(SELF:SCREEN-VALUE)
   THEN ll-style-is-valid = yes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.gluelap
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.gluelap V-table-Win
ON LEAVE OF eb.gluelap IN FRAME fold /* Glue Lap */
DO:
  IF ll-auto-calc-selected and
     {&self-name} <> DEC(SELF:SCREEN-VALUE)
   THEN ll-style-is-valid = yes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.k-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.k-len V-table-Win
ON LEAVE OF eb.k-len IN FRAME fold /* DK Length */
DO:
  IF ll-auto-calc-selected and
     {&self-name} <> DEC(SELF:SCREEN-VALUE)
   THEN ll-style-is-valid = yes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.k-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.k-wid V-table-Win
ON LEAVE OF eb.k-wid IN FRAME fold /* DK Width */
DO:
  IF ll-auto-calc-selected and
     {&self-name} <> DEC(SELF:SCREEN-VALUE)
   THEN ll-style-is-valid = yes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.len V-table-Win
ON LEAVE OF eb.len IN FRAME fold /* Length */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-wid-len NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.len V-table-Win
ON VALUE-CHANGED OF eb.len IN FRAME fold /* Length */
DO:
  ll-wid-len-warned = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.lock
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.lock V-table-Win
ON LEAVE OF eb.lock IN FRAME fold /* Lock Tab */
DO:
  IF ll-auto-calc-selected and
     {&self-name} <> DEC(SELF:SCREEN-VALUE)
   THEN ll-style-is-valid = yes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.plate-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.plate-no V-table-Win
ON LEAVE OF eb.plate-no IN FRAME fold /* Plate # */
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
ON LEAVE OF eb.procat IN FRAME fold /* FG Category */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-procat NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.procat V-table-Win
ON VALUE-CHANGED OF eb.procat IN FRAME fold /* FG Category */
DO:
  RUN new-procat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.ship-id V-table-Win
ON ENTRY OF eb.ship-id IN FRAME fold /* Ship To */
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
ON LEAVE OF eb.ship-id IN FRAME fold /* Ship To */
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
    IF eb.ship-name:SENSITIVE THEN DO:
      APPLY "entry" TO eb.ship-name.
      RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.ship-id V-table-Win
ON VALUE-CHANGED OF eb.ship-id IN FRAME fold /* Ship To */
DO:
  RUN display-shipto.
  ll-new-shipto = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.sman
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.sman V-table-Win
ON LEAVE OF eb.sman IN FRAME fold /* Sales Rep */
DO:
  IF LASTKEY NE -1  THEN DO:
    RUN valid-sman NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.sman V-table-Win
ON VALUE-CHANGED OF eb.sman IN FRAME fold /* Sales Rep */
DO:
  RUN new-sman.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.stock-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.stock-no V-table-Win
ON LEAVE OF eb.stock-no IN FRAME fold /* FG Item# */
DO:
    find first itemfg where itemfg.company = cocode and
                            itemfg.i-no = eb.stock-no:screen-value in frame {&frame-name}
                      no-lock no-error.

    if not avail itemfg and eb.stock-no:screen-value <> "" then do:
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

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.style V-table-Win
ON LEAVE OF eb.style IN FRAME fold /* Style Code */
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
ON VALUE-CHANGED OF eb.style IN FRAME fold /* Style Code */
DO:
  RUN new-style.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.t-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.t-len V-table-Win
ON LEAVE OF eb.t-len IN FRAME fold /* Blank Length */
DO:
  {ce/msfcalc.i}

   def var lv-len as dec no-undo.
   def var lv-wid as dec no-undo.

   ASSIGN
     lv-len = decimal(eb.t-len:screen-value)
     lv-wid = decimal(eb.t-wid:screen-value)
     lv-sqin = ( lv-len * lv-wid )
     eb.t-sqin:screen-value = string(lv-sqin).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.t-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.t-wid V-table-Win
ON LEAVE OF eb.t-wid IN FRAME fold /* Blank Width */
DO:
  {ce/msfcalc.i}

   def var lv-len as dec no-undo.
   def var lv-wid as dec no-undo.

   ASSIGN
     lv-len = decimal(eb.t-len:screen-value)
     lv-wid = decimal(eb.t-wid:screen-value)
     lv-sqin = ( lv-len * lv-wid )
     eb.t-sqin:SCREEN-VALUE = STRING(lv-sqin).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.tuck
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.tuck V-table-Win
ON LEAVE OF eb.tuck IN FRAME fold /* Tuck */
DO:
   IF ll-auto-calc-selected and
      eb.tuck <> DEC(eb.tuck:SCREEN-VALUE IN FRAME {&FRAME-NAME})
   THEN ll-style-is-valid = yes.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.wid V-table-Win
ON LEAVE OF eb.wid IN FRAME fold /* Width */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-wid-len NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.wid V-table-Win
ON VALUE-CHANGED OF eb.wid IN FRAME fold /* Width */
DO:
  ll-wid-len-warned = NO.
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
  {src/adm/template/row-list.i "ef"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "est"}
  {src/adm/template/row-find.i "eb"}
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
   def var lv-panels as log no-undo.
   def var i as int no-undo.
   def var j as int no-undo.
   def var K_FRAC as dec init 6.25 no-undo.
   def var v-score-char like v-lscore-c extent 12.
   def buffer xest for est.

   find first sys-ctrl  where sys-ctrl.company eq cocode
                           and sys-ctrl.name    eq "PANELS"
        no-lock no-error.
   if not avail sys-ctrl then do transaction:
      create sys-ctrl.
      assign  sys-ctrl.company = cocode
              sys-ctrl.name    = "PANELS"
              sys-ctrl.descrip = "CE Lock=Yes Panel Size Popup when Overriding W&L?"
              sys-ctrl.log-fld = yes.
      MESSAGE sys-ctrl.descrip
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE sys-ctrl.log-fld.
   end.
   lv-panels = sys-ctrl.log-fld.

   find xest where /*recid(xest) = recid(est) no-lock. */
                   xest.company = eb.company and
                   xest.est-no = eb.est-no
                   no-lock no-error.
   find xef where recid(xef) = recid(ef) no-lock.
   find xeb where recid(xeb) = recid(eb) no-lock.

   find FIRST style where style.company = eb.company and
                    style.style = eb.style
                    no-lock no-error.
   if avail style then do:
      run est/u2kinc1.p (RECID(xeb)).
      run est/u2kinc2.p (RECID(xeb)).
      find bf-eb of eb exclusive-lock.    
      FIND FIRST formule NO-ERROR.

      assign bf-eb.t-wid = (formule[1])
          bf-eb.t-len = (formule[2])
          bf-eb.t-sqin = (formule[7] * formule[8])
          bf-eb.k-wid-array = 0
          bf-eb.k-len-array = 0.
   end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-blank V-table-Win 
PROCEDURE copy-blank :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN est/d-copyblank.w (INPUT ROWID(eb),
                          INPUT ROWID(ef)).

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
def var tmpstore as cha no-undo.
def var i as int no-undo.


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
 itemfg.alloc      = xeb.set-is-assembled.

 RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT "").

 IF itemfg.alloc NE ? THEN itemfg.alloc = NOT itemfg.alloc.

 {oe/fgfreighta.i xeb}


IF v-graphic-char NE "" THEN 
DO:
   IF LOOKUP(SUBSTR(v-graphic-char,LENGTH(v-graphic-char)),"\,/") EQ 0 THEN
      v-graphic-char = v-graphic-char + "\".

   IF SEARCH(v-graphic-char + itemfg.i-no + ".jpg") NE ? THEN
      itemfg.box-image = v-graphic-char + itemfg.i-no + ".jpg".
END.

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
  def var rec_key_value as cha no-undo.
  def var header_value as cha no-undo.

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
  HIDE FRAME fold.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-est for est.

  DEF VAR char-hdl AS cha NO-UNDO.
  def var lv-hld-cust like eb.cust-no no-undo.
  def var lv-hld-ship like eb.ship-id no-undo.
  DEF VAR lv-hld-board LIKE ef.board NO-UNDO.
  DEF VAR lv-die-in LIKE ef.die-in NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-return-cha AS cha NO-UNDO.
  DEF VAR lv-prev-style AS cha NO-UNDO.
  DEF VAR lv-box-des AS CHAR INIT "S" NO-UNDO.
  DEF VAR lv-t-sqin LIKE eb.t-sqin NO-UNDO.

  DEF BUFFER b-est FOR est.
  DEF BUFFER b-eb FOR eb.
  DEF BUFFER bf-eb FOR eb.


  /* Code placed here will execute PRIOR to standard behavior. */
  assign
   lv-hld-cust   = eb.cust-no
   lv-hld-ship   = eb.ship-id
   lv-prev-style = eb.style
   lv-t-sqin     = eb.t-sqin.
  IF AVAIL(ef) THEN
    lv-hld-board = ef.board.

  DISABLE btnDieLookup btncadLookup WITH FRAME {&FRAME-NAME}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND CURRENT ef.

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

  find bf-est where recid(bf-est) = recid(est) exclusive-lock.
  assign bf-est.mod-date = today
         est.mod-date:screen-value in frame {&frame-name} = string(today).

  IF ll-auto-calc-selected THEN DO:
    RUN calc-blank-size.
    FIND CURRENT eb.
  END.

  ELSE DO:
    DO li = 1 TO EXTENT(ld-k-wid-array):
      eb.k-wid-scr-type[li] = lv-k-wid-scr-type[li].
      IF eb.k-wid-array[li] NE ld-k-wid-array[li] THEN
        ASSIGN
         ll-blank-size-changed = YES
         eb.k-wid-array[li]     = ld-k-wid-array[li].
    END.

    DO li = 1 TO EXTENT(ld-k-len-array):
      eb.k-len-scr-type[li] = lv-k-len-scr-type[li].
      IF eb.k-len-array[li] NE ld-k-len-array[li] THEN
        ASSIGN
         ll-blank-size-changed = YES
         eb.k-len-array[li]     = ld-k-len-array[li].
    END.
  END.

  IF NOT ll-blank-size-changed THEN
    ll-blank-size-changed = eb.t-wid NE lv-hld-wid OR eb.t-len NE lv-hld-len.

  IF ll-blank-size-changed     AND
     NOT ll-auto-calc-selected AND
     lv-t-sqin EQ eb.t-sqin    THEN eb.t-sqin = eb.t-wid * eb.t-len.

  RUN one-eb-on-ef (ROWID(ef), OUTPUT ll-one-eb-on-ef).

  IF ll-one-eb-on-ef AND ll-blank-size-changed THEN DO:
    MESSAGE "Do you wish to reset layout screen?"
            VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans2 AS LOG.

    ef.lsh-lock = NO.    
    IF ll-ans2 THEN RUN update-sheet.    
  END.

  RUN est/u2kinc1.p (RECID(eb)).
  RUN est/u2kinc2.p (RECID(eb)).
  find first formule.

  IF TRIM(fi_from-est-no) NE TRIM(lv-master-est-no) THEN DO:
    FOR EACH b-eb
        WHERE b-eb.company EQ eb.company
          AND b-eb.est-no  EQ eb.est-no:
      b-eb.master-est-no = FILL(" ",8 - LENGTH(TRIM(fi_from-est-no))) +
                           TRIM(fi_from-est-no).
    END.
    FIND b-est WHERE ROWID(b-est) EQ ROWID(est).
    IF TRIM(fi_from-est-no) EQ "" AND b-est.est-type EQ 4 THEN b-est.e-num = 0.
    FIND CURRENT b-est NO-LOCK.
  END.

  IF cestyle-log AND eb.style NE lv-prev-style THEN DO:
    ll-ans2 = NO.
    MESSAGE "Do you wish to reset box design?"
        VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans2.
    IF ll-ans2 THEN
       lv-box-des = "B".
    ELSE
       lv-box-des = "N".
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

  /*eb.die-in = 0.
  do i = 1 to 4:
    if ef.leaf[i] ne "" and ef.leaf-bnum[i] ne 0 and
       ((ef.leaf-w[i] ne 0) and (ef.leaf-l[i] ne 0)) THEN do:
      find first item {sys/look/itemW.i} and item.i-no eq ef.leaf[i]
          no-lock no-error.
      if item.mat-type ne "W" then next.
      eb.die-in = eb.die-in +
                  ((ef.leaf-w[i] + ef.leaf-l[i]) * 2 * eb.num-up).
    end.
  end.

  eb.die-in = eb.die-in + (formule[12] * eb.num-up).

  find first est-prep
      where est-prep.company  eq eb.company
        and est-prep.est-no   eq eb.est-no
        and est-prep.s-num    eq eb.form-no
        and est-prep.mat-type eq "R"
      no-error.

  lv-die-in = 0.
  for each b-eb
      where b-eb.company  eq eb.company
        and b-eb.est-no   eq eb.est-no
        and b-eb.form-no  eq eb.form-no
      no-lock:
    lv-die-in = lv-die-in + b-eb.die-in.
  end.

  IF lv-die-in NE 0 THEN ef.die-in = lv-die-in.
  IF AVAIL est-prep THEN est-prep.qty = ef.die-in.
  IF eb.die-in EQ 0 THEN eb.die-in = ef.die-in.

  RUN sys/inc/die-prep.p (ROWID(ef)).*/
  FIND CURRENT ef.

  ASSIGN
   ll-auto-calc-selected = NO
   ll-blank-size-changed = NO
   ll-new-shipto         = NO.

  IF est.est-type NE 4 THEN
  FOR EACH bf-eb
      WHERE bf-eb.company EQ eb.company
        AND bf-eb.est-no  EQ eb.est-no
        AND ROWID(bf-eb)  NE ROWID(eb):
    assign
     bf-eb.cust-no      = eb.cust-no
     bf-eb.ship-id      = eb.ship-id
     bf-eb.ship-no      = eb.ship-no
     bf-eb.ship-name    = eb.ship-name
     bf-eb.ship-addr[1] = eb.ship-addr[1]
     bf-eb.ship-addr[2] = eb.ship-addr[2]
     bf-eb.ship-city    = eb.ship-city
     bf-eb.ship-state   = eb.ship-state
     bf-eb.ship-zip     = eb.ship-zip
     bf-eb.sman         = eb.sman
     bf-eb.comm         = eb.comm.
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

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND CURRENT ef EXCLUSIVE-LOCK.
  FIND CURRENT est EXCLUSIVE-LOCK.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND CURRENT ef NO-LOCK.
  FIND CURRENT est NO-LOCK.

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

  DISABLE btnDieLookup btncadLookup WITH FRAME {&FRAME-NAME}.
  dieFile = ''.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}: 
    DISABLE est.metric
            fi_from-est-no.
  END.

  RUN release-shared-buffers.

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
  DEF VAR lv-image AS CHAR NO-UNDO.
  DEF BUFFER b2-eb FOR eb.
  DEF VAR lv-one-eb AS LOG NO-UNDO.
  DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.
  DEF BUFFER b-ef FOR ef.
  DEF BUFFER b-eb FOR eb.

  /* Code placed here will execute PRIOR to standard behavior. */
  {ce/msfcalc.i}

  IF NOT AVAIL est OR NOT AVAIL eb THEN RETURN.

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

  find style where style.company = eb.company and
                        style.style = eb.style
                        no-lock no-error.
  if avail style and style.type = "F" then lv-foam = yes.
  else lv-foam = no.

       IF avail style AND style.TYPE EQ "P" THEN DO:
        ASSIGN
            ef.board:LABEL IN FRAME {&FRAME-NAME} = "Paper" 
            eb.lock:LABEL IN FRAME {&FRAME-NAME} = "Pocket Size" 
            eb.tuck:LABEL IN FRAME {&FRAME-NAME} = "Tab" 
            eb.adhesive:LABEL IN FRAME {&FRAME-NAME} = "Pocket Adhesive"
            eb.gluelap:LABEL IN FRAME {&FRAME-NAME} = "Pocket  Glue Width"
            eb.lin-in:LABEL IN FRAME {&FRAME-NAME} = "Pocket  Glue Length"
            eb.t-wid:LABEL IN FRAME {&FRAME-NAME} = "Flat Width"
            eb.t-len:LABEL IN FRAME {&FRAME-NAME} = "Flat Length"
            eb.t-sqin:LABEL IN FRAME {&FRAME-NAME} = "Flat Sq Inches" 
            eb.wid:LABEL IN FRAME {&FRAME-NAME} = "Finished Width"
            eb.len:LABEL IN FRAME {&FRAME-NAME} = "Finished Length"
            eb.dep:LABEL IN FRAME {&FRAME-NAME} = "Finished Depth"
            eb.k-len:LABEL IN FRAME {&FRAME-NAME} = "Pocket Capacity"
            eb.k-wid:LABEL IN FRAME {&FRAME-NAME} = "Spine Capacity" .
    END.
    ELSE DO:
        ASSIGN
            ef.board:LABEL IN FRAME {&FRAME-NAME} = "Board" 
            eb.lock:LABEL IN FRAME {&FRAME-NAME} = "Lock Tab" 
            eb.tuck:LABEL IN FRAME {&FRAME-NAME} = "Tuck" 
            eb.adhesive:LABEL IN FRAME {&FRAME-NAME} = "Adhesive"
            eb.gluelap:LABEL IN FRAME {&FRAME-NAME} = "Glue lap" 
            eb.lin-in:LABEL IN FRAME {&FRAME-NAME} = "Lin Inches"
            eb.t-wid:LABEL IN FRAME {&FRAME-NAME} = "Blank Width"
            eb.t-len:LABEL IN FRAME {&FRAME-NAME} = "Blank Length"
            eb.t-sqin:LABEL IN FRAME {&FRAME-NAME} = "Blank Sq. In." 
            eb.wid:LABEL IN FRAME {&FRAME-NAME} = "Width"
            eb.len:LABEL IN FRAME {&FRAME-NAME} = "Length"
            eb.dep:LABEL IN FRAME {&FRAME-NAME} = "Depth"
            eb.k-len:LABEL IN FRAME {&FRAME-NAME} = "DK Length"
            eb.k-wid:LABEL IN FRAME {&FRAME-NAME} = "DK Width" .
    END.


  fi_from-est-no = IF eb.master-est-no NE "" AND
                      eb.est-type EQ 4       THEN eb.master-est-no
                   ELSE STRING(est.e-num,">>>>>>>>").

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /*see if there is more than one blank here*/
  lv-one-eb = CAN-FIND(b2-eb WHERE
              b2-eb.company EQ eb.company AND
              b2-eb.est-no EQ eb.est-no AND
              b2-eb.blank-no NE 0).

  /* Code placed here will execute AFTER standard behavior.    */
  IF eb.k-wid-array[1] EQ 0 THEN DO:
    FIND CURRENT eb.
    eb.k-wid-array[1] = eb.t-wid.
    FIND CURRENT eb NO-LOCK.
  END.

  IF eb.k-len-array[1] EQ 0 THEN DO:
    FIND CURRENT eb.
    eb.k-len-array[1] = eb.t-len.
    FIND CURRENT eb NO-LOCK.
  END.

  DO li = 1 TO EXTENT(eb.k-wid-array):
    ASSIGN
     ld-k-wid-array[li]    = eb.k-wid-array[li]
     lv-k-wid-scr-type[li] = eb.k-wid-scr-type[li].
  END.
  DO li = 1 TO EXTENT(eb.k-len-array):
    ASSIGN
     ld-k-len-array[li]    = eb.k-len-array[li]
     lv-k-len-scr-type[li] = eb.k-len-scr-type[li].
  END.

  DO WITH FRAME {&FRAME-NAME}:
    style_dscr:SCREEN-VALUE = IF AVAIL style THEN style.dscr ELSE "".

    FIND FIRST item
        {sys/look/itemb1W.i}
          AND item.i-no EQ ef.board:SCREEN-VALUE
        NO-LOCK NO-ERROR.

    ef.brd-dscr:SCREEN-VALUE = IF AVAIL item THEN item.i-name ELSE "". 

    FIND FIRST sman
        WHERE sman.company EQ eb.company
          AND sman.sman    EQ eb.sman
        NO-LOCK NO-ERROR.
    sman_sname:SCREEN-VALUE = IF AVAIL sman THEN sman.sname ELSE "".

    RUN new-procat.

    ASSIGN
     eb.ship-name:SENSITIVE     = NO
     eb.ship-addr[1]:SENSITIVE  = NO
     eb.ship-addr[2]:SENSITIVE  = NO
     eb.ship-city:SENSITIVE     = NO
     eb.ship-state:SENSITIVE    = NO
     eb.ship-zip:SENSITIVE      = NO.

    DO li = LENGTH(TRIM(ef.cad-image)) TO 1 BY -1:
      IF SUBSTR(ef.cad-image,li,1) EQ "/" OR
         SUBSTR(ef.cad-image,li,1) EQ "\" OR
         SUBSTR(ef.cad-image,li,1) EQ ":" THEN LEAVE.
      lv-image = SUBSTR(ef.cad-image,li,1) + TRIM(lv-image).
    END.
    ef.cad-image:SCREEN-VALUE = lv-image.

     btn_fgitem:LABEL = TRIM(eb.stock:LABEL) + ": " /*+ TRIM(eb.stock)*/ .
    IF eb.stock = "" THEN
            btn_fgitem:HIDDEN  = TRUE .
    ELSE 
         btn_fgitem:HIDDEN  = FALSE .

    btn_from:LABEL = TRIM(fi_from-est-no:LABEL) + ": " /*+ TRIM(eb.stock)*/ .

    btn_style:LABEL = TRIM(eb.style:LABEL) + ": " /*+ TRIM(eb.style) */ .
    IF eb.style = "" THEN
            btn_style:HIDDEN  = TRUE .
    ELSE 
         btn_style:HIDDEN  = FALSE .

    btn_board:LABEL = TRIM(ef.board:LABEL) + ": " /*+ TRIM(ef.board)*/ .
    IF ef.board = "" THEN
            btn_board:HIDDEN  = TRUE .
    ELSE 
         btn_board:HIDDEN  = FALSE .

   btn_cust:LABEL = TRIM(eb.cust-no:LABEL) + ": " /*+ TRIM(ef.board)*/ .
    IF eb.cust-no = "" THEN
            btn_cust:HIDDEN  = TRUE .
    ELSE 
         btn_cust:HIDDEN  = FALSE .

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

    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"copy-source",OUTPUT char-hdl).

    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
       RUN enable-copy IN WIDGET-HANDLE(char-hdl) (lv-one-eb).

  END.

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
                        and sys-ctrl.name    eq "PANELS"
             no-lock no-error.
  if not avail sys-ctrl then do transaction:
     create sys-ctrl.
     assign sys-ctrl.company = cocode
            sys-ctrl.name    = "PANELS"
            sys-ctrl.descrip = "CE Lock=Yes Panel Size Popup when Overriding W&L?"
            sys-ctrl.log-fld = yes.
     MESSAGE sys-ctrl.descrip
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE sys-ctrl.log-fld.
  end.
  lv-panels = sys-ctrl.log-fld.

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
  DEF VAR ll-tandem AS LOG NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR li AS INT NO-UNDO.

  def var hd1 as handle no-undo.
  def var hd2 as handle no-undo.

  def buffer bf-eb for eb.

  /* Code placed here will execute PRIOR to standard behavior. */
    /* ==== Folding item validation ======== */
  RUN valid-fi_from-est-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    if eb.stock-no:screen-value in frame {&frame-name} <> "" then do:
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

  IF eb.ord-no NE 0 AND eb.cust-no:SCREEN-VALUE NE eb.cust-no AND
     eb.cust-no NE "" THEN
  DO:
     MESSAGE "Cannot Change Customer."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY "entry" TO eb.cust-no.
     RETURN NO-APPLY.
  END.

  RUN display-shipto.

  RUN valid-style NO-ERROR.
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

  RUN valid-custcsr NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
  /*  IF eb.t-wid:MODIFIED OR eb.t-len:MODIFIED OR 
       eb.wid:MODIFIED OR eb.len:MODIFIED OR eb.dep:MODIFIED */
    IF eb.t-wid <> dec(eb.t-wid:SCREEN-VALUE) OR  
       eb.t-len <> dec(eb.t-len:SCREEN-VALUE) OR
       eb.wid <> dec(eb.wid:SCREEN-VALUE) OR
       eb.len <> dec(eb.len:SCREEN-VALUE) OR
       eb.dep <> dec(eb.dep:SCREEN-VALUE) 
    THEN ll-blank-size-changed = YES.
  END.

  ASSIGN
   old-die-no = eb.die-no
   old-cad-no = eb.cad-no
   old-cimage = ef.cad-image
   old-cat-no = eb.procat .

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN ce/com/istandem.p (ROWID(est), OUTPUT ll-tandem).

  DO WITH FRAME {&FRAME-NAME}: 
    DISABLE est.metric
            fi_from-est-no.
  END.

  RUN dispatch ('display-fields').  /* refresh 2nd & all children pages */

  RUN release-shared-buffers.

  ll-style-is-valid = NO.

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
    FIND FIRST item
        {sys/look/itemb1W.i}
          AND item.i-no EQ ef.board:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL item THEN ef.brd-dscr:SCREEN-VALUE = item.i-name.
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

      RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).

      RUN sys/inc/getsmncm.p (eb.cust-no:SCREEN-VALUE,
                              INPUT-OUTPUT lv-sman,
                              eb.procat:SCREEN-VALUE,
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
  DEF VAR hv-foam LIKE lv-foam NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    hv-foam = lv-foam.

    FIND FIRST style
        WHERE style.company  EQ cocode
          AND style.style    EQ eb.style:SCREEN-VALUE
          AND style.industry EQ lv-industry
        NO-LOCK NO-ERROR.   
    IF AVAIL style THEN DO:

    IF style.TYPE EQ "P" THEN DO:
        ASSIGN
            ef.board:LABEL = "Paper" 
            eb.lock:LABEL = "Pocket Size" 
            eb.tuck:LABEL = "Tab" 
            eb.adhesive:LABEL = "Pocket Adhesive"
            eb.gluelap:LABEL = "Pocket  Glue Width"
            eb.lin-in:LABEL = "Pocket  Glue Length" 
            eb.t-wid:LABEL = "Flat Width"
            eb.t-len:LABEL = "Flat Length"
            eb.t-sqin:LABEL = "Flat Sq Inches" 
            eb.wid:LABEL = "Finished Width"
            eb.len:LABEL = "Finished Length"
            eb.dep:LABEL = "Finished Depth"
            eb.k-len:LABEL = "Pocket Capacity"
            eb.k-wid:LABEL = "Spine Capacity" .
    END.
    ELSE DO:
        ASSIGN
            ef.board:LABEL = "Board" 
            eb.lock:LABEL = "Lock Tab" 
            eb.tuck:LABEL = "Tuck" 
            eb.adhesive:LABEL = "Adhesive"
            eb.gluelap:LABEL = "Glue lap" 
            eb.lin-in:LABEL = "Lin Inches"
            eb.t-wid:LABEL = "Blank Width"
            eb.t-len:LABEL = "Blank Length"
            eb.t-sqin:LABEL = "Blank Sq. In." 
            eb.wid:LABEL = "Width"
            eb.len:LABEL = "Length"
            eb.dep:LABEL = "Depth"
            eb.k-len:LABEL = "DK Length"
            eb.k-wid:LABEL = "DK Width"  .
    END.

      ASSIGN
       style_dscr:SCREEN-VALUE = style.dscr
       lv-foam                 = style.type EQ "F"
       ll-wid-len-warned       = NO.

      IF ll-auto-calc-selected THEN DO:
        ASSIGN
         eb.adhesive:SCREEN-VALUE = style.material[7]
         eb.gluelap:SCREEN-VALUE  = STRING(style.dim-gl)
         eb.k-len:SCREEN-VALUE    = STRING(style.dim-dkl)
         eb.k-wid:SCREEN-VALUE    = STRING(style.dim-dkw)
         eb.fpanel:SCREEN-VALUE   = STRING(style.dim-pan5)
         eb.lock:SCREEN-VALUE     = STRING(style.dim-fit)
         eb.tuck:SCREEN-VALUE     = STRING(style.dim-tk).

        FIND FIRST item
            WHERE item.company EQ cocode 
              AND item.i-no    EQ eb.adhesive:SCREEN-VALUE
            NO-LOCK NO-ERROR.
        IF AVAIL ITEM AND INDEX("G,S,T",item.mat-type) GT 0 AND
           item.i-no NE "No Joint" THEN eb.lin-in:SCREEN-VALUE = STRING(eb.dep).
      END.

      IF ef.board:SCREEN-VALUE EQ "" OR lv-foam NE hv-foam THEN DO:
        ASSIGN
         ef.board:SCREEN-VALUE    = style.material[1]
         ef.brd-dscr:SCREEN-VALUE = "".
        RUN new-board.
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


  {sys/inc/jobcard.i "F"}
  lv-format = sys-ctrl.char-fld.

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

    IF est.est-type EQ 1 OR est.est-type EQ 4 THEN DO:
      ENABLE fi_from-est-no.
      APPLY "entry" TO fi_from-est-no.
    END.
    ELSE DISABLE fi_from-est-no.

    lv-master-est-no = fi_from-est-no.

    IF INDEX(lv-format,"Keystone") GT 0 THEN ENABLE est.metric.
                                        ELSE DISABLE est.metric.

    RUN shipto-enable.

    ENABLE btnDieLookup btnCadLookup.

    ef.cad-image:SCREEN-VALUE = ef.cad-image.
  END.

  RUN release-shared-buffers.

  ASSIGN
   lv-hld-wid = eb.t-wid
   lv-hld-len = eb.t-len.

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
  {src/adm/template/snd-list.i "ef"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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
          xeb.num-dep = 0.

  IF NOT lv-foam THEN DO:
    {ce/ceroute1.i w id l en}
  END.

  RUN ce/calc-dim.p.

  find xef where recid(xef) = recid(ef) no-lock.
  find xeb where recid(xeb) = recid(eb) no-lock.

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

    IF TRIM(ip-field:SCREEN-VALUE) NE ""                          AND
       NOT CAN-FIND(FIRST item
                    WHERE item.company  EQ cocode
                      AND item.i-no     EQ ip-field:SCREEN-VALUE
                      AND CAN-DO("G,T",item.mat-type)
                      /*AND item.industry EQ lv-industry*/)       THEN DO:
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
    eb.style:SCREEN-VALUE = CAPS(eb.style:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST style
                    WHERE style.company  EQ cocode
                      AND style.style    EQ eb.style:SCREEN-VALUE
                      AND style.industry EQ lv-industry) OR
                      eb.style:SCREEN-VALUE EQ "" 
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO eb.style.
      RETURN ERROR.
    END.

    IF style_dscr:SCREEN-VALUE EQ "" THEN RUN new-style.
    IF ll-auto-calc-selected AND NOT ll-style-is-valid THEN DO:
       FIND FIRST style WHERE style.company  EQ cocode
                           AND style.style    EQ eb.style:SCREEN-VALUE
                           AND style.industry EQ lv-industry NO-LOCK.
       ASSIGN eb.fpanel:SCREEN-VALUE  = string(style.dim-pan5)
              eb.k-len:SCREEN-VALUE   = string(style.dim-dkl)
              eb.k-wid:SCREEN-VALUE   = string(style.dim-dkw)
              eb.tuck:SCREEN-VALUE    = string(style.dim-tk)
              eb.gluelap:SCREEN-VALUE = string(style.dim-gl)
              eb.lock:SCREEN-VALUE    = string(style.dim-fit).
       ll-style-is-valid = YES.
    END.
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

    IF ll-warn AND ll-wid-len-warned EQ NO                  AND
       CAN-FIND(FIRST style
                WHERE style.company  EQ cocode
                  AND style.style    EQ eb.style:SCREEN-VALUE
                  AND style.industry EQ lv-industry
                  AND INDEX("DF",style.type) LE 0)          AND
       (eb.style:SCREEN-VALUE    NE eb.style OR
        DEC(eb.wid:SCREEN-VALUE) NE eb.wid   OR
        DEC(eb.len:SCREEN-VALUE) NE eb.len)                 AND
       DEC(eb.wid:SCREEN-VALUE) GT DEC(eb.len:SCREEN-VALUE) THEN DO:
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION imageName V-table-Win 
FUNCTION imageName RETURNS CHARACTER
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

