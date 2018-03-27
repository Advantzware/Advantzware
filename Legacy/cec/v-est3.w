&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: cec/v-est3.w

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
def var ll-update-pack as log no-undo.
def var ll-unit-calc as log no-undo.
&scoped-define est-pack PACK  /* for disable pack */
{sys/inc/var.i new shared}
{custom/globdefs.i}
assign cocode = g_company
       locode = g_loc.
def new shared buffer xest for est.
def new shared buffer xef  for ef.
def new shared buffer xeb  for eb.
def var k_frac as dec init 6.25 no-undo.

DEF VAR ll-ink-def AS LOG NO-UNDO.
DEF VAR lv-numstack AS INT NO-UNDO.
DEF VAR lv-stack-code AS cha NO-UNDO.
def var lv-layers AS DEC no-undo.
DEF VAR ll-foam AS LOG NO-UNDO.
DEF VAR lv-label AS CHAR EXTENT 10 NO-UNDO.
DEF VAR ll-assem-part AS LOG NO-UNDO.

PROCEDURE ShellExecuteA EXTERNAL "shell32":u :
      define input parameter hwnd as long.
      define input parameter lpOperation as char.
      define input parameter lpFile as char.
      define input parameter lpParameters as char.
      define input parameter lpDirectory as char.
      define input parameter nShowCmd as long.
      define return parameter hInstance as long.
END PROCEDURE.

DO TRANSACTION:
  {sys/inc/cecunit.i}
  {sys/inc/setprint.i}
END.
{sys/inc/f16to32.i}

{est/inksvarn.i NEW}

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
&Scoped-define EXTERNAL-TABLES est eb
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est, eb.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS eb.i-col eb.i-pass eb.i-coat eb.i-coat-p ~
eb.i-coldscr eb.i-ps[1] eb.i-code[1] eb.i-dscr[1] eb.i-%[1] eb.i-ps[2] ~
eb.i-code[2] eb.i-dscr[2] eb.i-%[2] eb.i-ps[3] eb.i-code[3] eb.i-dscr[3] ~
eb.i-%[3] eb.i-ps[4] eb.i-code[4] eb.i-dscr[4] eb.i-%[4] eb.i-ps[5] ~
eb.i-code[5] eb.i-dscr[5] eb.i-%[5] eb.i-ps[6] eb.i-code[6] eb.i-dscr[6] ~
eb.i-%[6] eb.i-ps[7] eb.i-code[7] eb.i-dscr[7] eb.i-%[7] eb.i-ps[8] ~
eb.i-code[8] eb.i-dscr[8] eb.i-%[8] eb.i-ps[9] eb.i-code[9] eb.i-dscr[9] ~
eb.i-%[9] eb.i-ps[10] eb.i-code[10] eb.i-dscr[10] eb.i-%[10] 
&Scoped-define ENABLED-TABLES eb
&Scoped-define FIRST-ENABLED-TABLE eb
&Scoped-Define ENABLED-OBJECTS RECT-26 RECT-27 RECT-28 RECT-29 
&Scoped-Define DISPLAYED-FIELDS eb.cas-no eb.cas-len eb.i-col eb.i-pass ~
eb.i-coat eb.i-coat-p eb.cas-cost eb.cas-wid eb.i-coldscr eb.cas-cnt ~
eb.cas-dep eb.cas-pal eb.cas-wt eb.i-ps[1] eb.i-code[1] eb.i-dscr[1] ~
eb.i-%[1] eb.tr-no eb.tr-len eb.i-ps[2] eb.i-code[2] eb.i-dscr[2] eb.i-%[2] ~
eb.tr-cost eb.tr-wid eb.i-ps[3] eb.i-code[3] eb.i-dscr[3] eb.i-%[3] ~
eb.tr-cnt eb.tr-dep eb.i-ps[4] eb.i-code[4] eb.i-dscr[4] eb.i-%[4] ~
eb.tr-cas eb.i-ps[5] eb.i-code[5] eb.i-dscr[5] eb.i-%[5] eb.stacks ~
eb.i-ps[6] eb.i-code[6] eb.i-dscr[6] eb.i-%[6] eb.stack-code eb.i-ps[7] ~
eb.i-code[7] eb.i-dscr[7] eb.i-%[7] eb.i-ps[8] eb.i-code[8] eb.i-dscr[8] ~
eb.i-%[8] eb.chg-method eb.i-ps[9] eb.i-code[9] eb.i-dscr[9] eb.i-%[9] ~
eb.weight-m eb.carrier eb.carr-dscr eb.i-ps[10] eb.i-code[10] eb.i-dscr[10] ~
eb.i-%[10] eb.dest-code eb.fr-out-c eb.fr-out-m 
&Scoped-define DISPLAYED-TABLES eb
&Scoped-define FIRST-DISPLAYED-TABLE eb


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS eb.cas-no eb.cas-len eb.cas-cost ~
eb.cas-wid eb.cas-cnt eb.cas-dep eb.cas-pal eb.cas-wt eb.tr-no eb.tr-len ~
eb.tr-cost eb.tr-wid eb.tr-cnt eb.tr-dep eb.tr-cas eb.stacks eb.stack-code ~
eb.chg-method eb.weight-m eb.carrier eb.carr-dscr eb.dest-code eb.fr-out-c ~
eb.fr-out-m 

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
DEFINE IMAGE stackImage
     FILENAME "adeicon/blank":U
     STRETCH-TO-FIT
     SIZE 18 BY 2.86.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 15.71.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 5.24.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 6.19.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 4.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Corr
     eb.cas-no AT ROW 1.24 COL 89 COLON-ALIGNED
          LABEL "Packing Code"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.cas-len AT ROW 1.24 COL 125 COLON-ALIGNED
          LABEL "Unit Length" FORMAT ">9.99"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     eb.i-col AT ROW 1.48 COL 9 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-pass AT ROW 1.48 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-coat AT ROW 1.48 COL 42 COLON-ALIGNED
          LABEL "Coatings"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-coat-p AT ROW 1.48 COL 57 COLON-ALIGNED
          LABEL "Passes"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.cas-cost AT ROW 2.19 COL 89 COLON-ALIGNED
          LABEL "Cost/Ea"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.cas-wid AT ROW 2.19 COL 125 COLON-ALIGNED
          LABEL "Unit Width" FORMAT ">9.99"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     eb.i-coldscr AT ROW 2.43 COL 9 COLON-ALIGNED NO-LABEL FORMAT "x(40)"
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     eb.cas-cnt AT ROW 3.14 COL 89 COLON-ALIGNED
          LABEL "Boxes/Code"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.cas-dep AT ROW 3.14 COL 125 COLON-ALIGNED
          LABEL "Unit Depth" FORMAT ">9.99"
          VIEW-AS FILL-IN 
          SIZE 14 BY .95
     eb.cas-pal AT ROW 4.1 COL 89 COLON-ALIGNED
          LABEL "Bundles/Pallet" FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.cas-wt AT ROW 4.1 COL 125 COLON-ALIGNED
          LABEL "Wt/PackCode"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     eb.i-ps[1] AT ROW 4.57 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[1] AT ROW 4.57 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[1] AT ROW 4.57 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[1] AT ROW 4.57 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.tr-no AT ROW 5.52 COL 89 COLON-ALIGNED
          LABEL "Pallet #"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.tr-len AT ROW 5.52 COL 125 COLON-ALIGNED
          LABEL "Length" FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     eb.i-ps[2] AT ROW 5.57 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[2] AT ROW 5.57 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[2] AT ROW 5.57 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[2] AT ROW 5.57 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.tr-cost AT ROW 6.48 COL 89 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.tr-wid AT ROW 6.48 COL 125 COLON-ALIGNED
          LABEL "Width" FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     eb.i-ps[3] AT ROW 6.57 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[3] AT ROW 6.57 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Corr
     eb.i-dscr[3] AT ROW 6.57 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[3] AT ROW 6.57 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.tr-cnt AT ROW 7.43 COL 89 COLON-ALIGNED FORMAT ">,>>>,>>>"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.tr-dep AT ROW 7.43 COL 125 COLON-ALIGNED FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     eb.i-ps[4] AT ROW 7.67 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[4] AT ROW 7.67 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[4] AT ROW 7.67 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[4] AT ROW 7.67 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.tr-cas AT ROW 8.38 COL 89 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.i-ps[5] AT ROW 8.67 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[5] AT ROW 8.67 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[5] AT ROW 8.67 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[5] AT ROW 8.67 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.stacks AT ROW 9.33 COL 89 COLON-ALIGNED
          LABEL "# of Stacks" FORMAT ">>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.i-ps[6] AT ROW 9.67 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[6] AT ROW 9.67 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[6] AT ROW 9.67 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[6] AT ROW 9.67 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.stack-code AT ROW 10.29 COL 89 COLON-ALIGNED
          LABEL "Stack Code" FORMAT "X(8)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.i-ps[7] AT ROW 10.67 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[7] AT ROW 10.67 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[7] AT ROW 10.67 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[7] AT ROW 10.67 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[8] AT ROW 11.67 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[8] AT ROW 11.67 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[8] AT ROW 11.67 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[8] AT ROW 11.67 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Corr
     eb.chg-method AT ROW 11.71 COL 91 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Prepaid", "P":U,
"Collect", "C":U,
"Bill", "B":U,
"Third Party", "T":U
          SIZE 52 BY .95
     eb.i-ps[9] AT ROW 12.67 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[9] AT ROW 12.67 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[9] AT ROW 12.67 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[9] AT ROW 12.67 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.weight-m AT ROW 12.67 COL 91 COLON-ALIGNED FORMAT ">>>9.99"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.carrier AT ROW 13.62 COL 91 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.carr-dscr AT ROW 13.62 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 29 BY 1
     eb.i-ps[10] AT ROW 13.67 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[10] AT ROW 13.67 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[10] AT ROW 13.67 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[10] AT ROW 13.67 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.dest-code AT ROW 14.57 COL 91 COLON-ALIGNED HELP
          "Enter User Defined Delivery Code for this Ship-To Location"
          LABEL "Delivery Zone" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     eb.fr-out-c AT ROW 15.52 COL 91 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     eb.fr-out-m AT ROW 15.52 COL 124 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     "Description" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 3.86 COL 36
     "%" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 3.86 COL 63
     "Code" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 3.86 COL 15
     "PS" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 3.86 COL 4
     "Freight Charge" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.71 COL 73
          FGCOLOR 9 
     stackImage AT ROW 8.38 COL 122
     RECT-26 AT ROW 1 COL 1
     RECT-27 AT ROW 11.48 COL 71
     RECT-28 AT ROW 5.29 COL 71
     RECT-29 AT ROW 1 COL 71
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.est,ASI.eb
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
         HEIGHT             = 17.95
         WIDTH              = 160.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME Corr:SCROLLABLE       = FALSE
       FRAME Corr:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN eb.carr-dscr IN FRAME Corr
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.carrier IN FRAME Corr
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.cas-cnt IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.cas-cost IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.cas-dep IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN eb.cas-len IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN eb.cas-no IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.cas-pal IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN eb.cas-wid IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN eb.cas-wt IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR RADIO-SET eb.chg-method IN FRAME Corr
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.dest-code IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT EXP-HELP                            */
/* SETTINGS FOR FILL-IN eb.fr-out-c IN FRAME Corr
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.fr-out-m IN FRAME Corr
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.i-coat IN FRAME Corr
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.i-coat-p IN FRAME Corr
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.i-coldscr IN FRAME Corr
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN eb.stack-code IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR IMAGE stackImage IN FRAME Corr
   NO-ENABLE                                                            */
ASSIGN 
       stackImage:HIDDEN IN FRAME Corr           = TRUE.

/* SETTINGS FOR FILL-IN eb.stacks IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN eb.tr-cas IN FRAME Corr
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.tr-cnt IN FRAME Corr
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN eb.tr-cost IN FRAME Corr
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.tr-dep IN FRAME Corr
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN eb.tr-len IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN eb.tr-no IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.tr-wid IN FRAME Corr
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN eb.weight-m IN FRAME Corr
   NO-ENABLE 2 EXP-FORMAT                                               */
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
  DEF VAR lv-ind LIKE style.industry NO-UNDO.
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.

  /* gdm - 03250913 end */
  DEF VAR v-cnt    AS INT NO-UNDO.
  DEF VAR v-cnt2   AS INT NO-UNDO.
  DEF VAR v-cnt3   AS INT NO-UNDO.
  DEF VAR v-loopct AS INT NO-UNDO.
  DEF VAR v-valhld AS CHAR NO-UNDO.

  ASSIGN
    v-loopct = INT(eb.i-col:SCREEN-VALUE) + 
               INT(eb.i-coat:SCREEN-VALUE). 
 /* gdm - 03250913 end */

  lw-focus = FOCUS.

  FIND FIRST style NO-LOCK
      WHERE style.company EQ eb.company
        AND style.style   EQ eb.style
      NO-ERROR.   
  lv-ind = IF AVAIL style THEN style.industry ELSE "".

  CASE lw-focus:NAME:
    WHEN "i-code" THEN DO:
        /*
         RUN windows/l-item2.w 
         (eb.company, lv-ind, "I", lw-focus:SCREEN-VALUE, OUTPUT char-val).
        */

        /* gdm - 03250913 - NEW LOOK UP - MULTIPLE SELECTION SUPPORTED */
        RUN windows/l-item4.w 
            (eb.company, lv-ind, "I",focus:SCREEN-VALUE, OUTPUT char-val).

        IF char-val NE "" AND 
           lw-focus:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:

            ASSIGN 
              lw-focus:SCREEN-VALUE = ENTRY(1,char-val)
              v-valhld = char-val.

            DO v-cnt = FOCUS:INDEX  TO v-loopct:
                CASE v-cnt:
                 WHEN 1 THEN 
                   ASSIGN 
                    eb.i-code[1]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                    eb.i-dscr[1]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                 WHEN 2 THEN                                                              
                   ASSIGN                                                                 
                    eb.i-code[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)  
                    eb.i-dscr[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld). 
                 WHEN 3 THEN                                                              
                   ASSIGN                                                                 
                    eb.i-code[3]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                    eb.i-dscr[3]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                 WHEN 4 THEN                                                              
                   ASSIGN                                                                 
                    eb.i-code[4]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)  
                    eb.i-dscr[4]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld). 
                 WHEN 5 THEN                                                              
                   ASSIGN                                                                 
                    eb.i-code[5]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)  
                    eb.i-dscr[5]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld). 
                 WHEN 6 THEN                                                              
                   ASSIGN                                                                 
                    eb.i-code[6]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)  
                    eb.i-dscr[6]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld). 
                 WHEN 7 THEN                                                              
                   ASSIGN                                                                 
                    eb.i-code[7]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)  
                    eb.i-dscr[7]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld). 
                 WHEN 8 THEN                                                              
                   ASSIGN                                                                 
                    eb.i-code[8]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)  
                    eb.i-dscr[8]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld). 
                 WHEN 9 THEN                                                              
                   ASSIGN                                                                 
                    eb.i-code[9]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)  
                    eb.i-dscr[9]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld). 
                 WHEN 10 THEN                                                             
                   ASSIGN                                                                 
                    eb.i-code[10]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld) 
                    eb.i-dscr[10]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
            END CASE .

            ASSIGN 
             v-valhld = SUBSTR(v-valhld,(LENGTH(ENTRY(1,v-valhld)) + 
                                         LENGTH(ENTRY(2,v-valhld)) + 3)).
            IF TRIM(v-valhld) EQ ""  THEN LEAVE.

        END. /* loop */
      END. /* IF chr-val*/
    END.  /* WHEN */

    WHEN "i-dscr" THEN DO: 
      RUN windows/l-itmdsc.w (eb.company, lv-ind, "I", lw-focus:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND lw-focus:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:
        lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
        CASE lw-focus:index:        
          WHEN 1  THEN eb.i-code[1]:SCREEN-VALUE  = ENTRY(2,char-val) .
          WHEN 2  THEN eb.i-code[2]:SCREEN-VALUE  = ENTRY(2,char-val) .
          WHEN 3  THEN eb.i-code[3]:SCREEN-VALUE  = ENTRY(2,char-val) .
          WHEN 4  THEN eb.i-code[4]:SCREEN-VALUE  = ENTRY(2,char-val) .
          WHEN 5  THEN eb.i-code[5]:SCREEN-VALUE  = ENTRY(2,char-val) .
          WHEN 6  THEN eb.i-code[6]:SCREEN-VALUE  = ENTRY(2,char-val) .
          WHEN 7  THEN eb.i-code[7]:SCREEN-VALUE  = ENTRY(2,char-val) .
          WHEN 8  THEN eb.i-code[8]:SCREEN-VALUE  = ENTRY(2,char-val) .
          WHEN 9  THEN eb.i-code[9]:SCREEN-VALUE  = ENTRY(2,char-val) .
          WHEN 10 THEN eb.i-code[10]:SCREEN-VALUE = ENTRY(2,char-val) .
        END CASE.
      END.                              
    END.

    WHEN "cas-no" THEN DO:
      RUN windows/l-item.w (eb.company, "", "C", lw-focus:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND lw-focus:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:
        lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
        APPLY "value-changed" TO eb.cas-no.
      END.
    END.

    WHEN "tr-no" THEN DO: 
      RUN windows/l-item.w (eb.company, "", "D", lw-focus:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND lw-focus:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:
        FIND FIRST item NO-LOCK
            WHERE item.company EQ eb.company
              AND item.i-no    EQ ENTRY(1,char-val)
            NO-ERROR.
        lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
        IF AVAIL item THEN
          ASSIGN
           eb.tr-len:SCREEN-VALUE = STRING(item.case-l)
           eb.tr-wid:SCREEN-VALUE = STRING(item.case-w)
           eb.tr-dep:SCREEN-VALUE = STRING(item.case-d).
      END.
    END.

    WHEN "carrier" THEN DO:
      RUN windows/l-carrie.w  (eb.company, eb.loc, lw-focus:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND lw-focus:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:
        eb.carrier:SCREEN-VALUE = ENTRY(1,char-val).
        RUN new-carrier.
      END.
    END.

    WHEN "dest-code" THEN DO:
      RUN windows/l-delzon.w (eb.company, eb.loc, eb.carrier:SCREEN-VALUE, lw-focus:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND lw-focus:SCREEN-VALUE NE ENTRY(1,char-val) THEN 
        lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
    END.

    WHEN "stack-code" THEN DO:
      RUN windows/l-stcode.w (eb.company, eb.stack-code:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND eb.stack-code:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:
        eb.stack-code:SCREEN-VALUE = ENTRY(1,char-val).
        RUN new-stack-code.
      END.
    END.
  END CASE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.carrier V-table-Win
ON LEAVE OF eb.carrier IN FRAME Corr /* Carrier */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-carrier NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.carrier V-table-Win
ON VALUE-CHANGED OF eb.carrier IN FRAME Corr /* Carrier */
DO:
  RUN new-carrier.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cas-cnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cas-cnt V-table-Win
ON VALUE-CHANGED OF eb.cas-cnt IN FRAME Corr /* Boxes/Code */
DO:
  RUN calc-tr-cnt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cas-dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cas-dep V-table-Win
ON LEAVE OF eb.cas-dep IN FRAME Corr /* Unit Depth */
DO:
   /*if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= 0.16 
   then do:
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      return no-apply.
   end.*/


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cas-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cas-len V-table-Win
ON LEAVE OF eb.cas-len IN FRAME Corr /* Unit Length */
DO:
   /*if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= 0.16 
   then do:
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      return no-apply.
   end.*/


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cas-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cas-no V-table-Win
ON LEAVE OF eb.cas-no IN FRAME Corr /* Packing Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cas-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cas-no V-table-Win
ON VALUE-CHANGED OF eb.cas-no IN FRAME Corr /* Packing Code */
DO:
  find item where item.company = eb.company and
                     item.i-no = eb.cas-no:screen-value and
                     item.mat-type = "C"
                     no-lock no-error.
  if avail item then do:
       assign /*eb.cas-cost:Screen-value = */
              eb.cas-len:Screen-value = string(item.case-l)
              eb.cas-wid:Screen-value = string(item.case-w)
              eb.cas-dep:Screen-value = string(item.case-d)
              eb.cas-pal:Screen-value = string(item.case-pall)
              eb.cas-cnt:Screen-value = string(item.box-case)
              eb.cas-wt:Screen-value = string(item.avg-w).
       RUN new-cas-pal.
  end.

  RUN enable-case-dims.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cas-pal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cas-pal V-table-Win
ON LEAVE OF eb.cas-pal IN FRAME Corr /* Bundles/Pallet */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cas-pal NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cas-pal V-table-Win
ON VALUE-CHANGED OF eb.cas-pal IN FRAME Corr /* Bundles/Pallet */
DO:
  RUN new-cas-pal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cas-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cas-wid V-table-Win
ON LEAVE OF eb.cas-wid IN FRAME Corr /* Unit Width */
DO:
   /*if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= 0.16 
   then do:
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      return no-apply.
   end.*/


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cas-wt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cas-wt V-table-Win
ON LEAVE OF eb.cas-wt IN FRAME Corr /* Wt/PackCode */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cas-wt NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.chg-method
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.chg-method V-table-Win
ON return OF eb.chg-method IN FRAME Corr /* chg-method */
DO:
  apply "tab" to self.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.dest-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.dest-code V-table-Win
ON LEAVE OF eb.dest-code IN FRAME Corr /* Delivery Zone */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dest-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-coat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-coat V-table-Win
ON VALUE-CHANGED OF eb.i-coat IN FRAME Corr /* Coatings */
DO:
  IF DEC({&self-name}:SCREEN-VALUE) EQ 0 THEN
    eb.i-coat-p:SCREEN-VALUE = "".
  ELSE
  IF DEC(eb.i-pass:SCREEN-VALUE) EQ 0 THEN
    eb.i-coat-p:SCREEN-VALUE = "1".

  {est/def-inks.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-coat-p
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-coat-p V-table-Win
ON VALUE-CHANGED OF eb.i-coat-p IN FRAME Corr /* Passes */
DO:
  {est/def-inks.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[10] V-table-Win
ON LEAVE OF eb.i-code[10] IN FRAME Corr /* Code[10] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {est/val-inks.i 10}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[10] V-table-Win
ON VALUE-CHANGED OF eb.i-code[10] IN FRAME Corr /* Code[10] */
DO:
  {est/new-inks.i 10}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[1] V-table-Win
ON LEAVE OF eb.i-code[1] IN FRAME Corr /* Code[1] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {est/val-inks.i 1}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[1] V-table-Win
ON VALUE-CHANGED OF eb.i-code[1] IN FRAME Corr /* Code[1] */
DO:
  {est/new-inks.i 1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[2] V-table-Win
ON LEAVE OF eb.i-code[2] IN FRAME Corr /* Code[2] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {est/val-inks.i 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[2] V-table-Win
ON VALUE-CHANGED OF eb.i-code[2] IN FRAME Corr /* Code[2] */
DO:
  {est/new-inks.i 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[3] V-table-Win
ON LEAVE OF eb.i-code[3] IN FRAME Corr /* Code[3] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {est/val-inks.i 3}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[3] V-table-Win
ON VALUE-CHANGED OF eb.i-code[3] IN FRAME Corr /* Code[3] */
DO:
  {est/new-inks.i 3}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[4] V-table-Win
ON LEAVE OF eb.i-code[4] IN FRAME Corr /* Code[4] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {est/val-inks.i 4}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[4] V-table-Win
ON VALUE-CHANGED OF eb.i-code[4] IN FRAME Corr /* Code[4] */
DO:
  {est/new-inks.i 4}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[5] V-table-Win
ON LEAVE OF eb.i-code[5] IN FRAME Corr /* Code[5] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {est/val-inks.i 5}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[5] V-table-Win
ON VALUE-CHANGED OF eb.i-code[5] IN FRAME Corr /* Code[5] */
DO:
  {est/new-inks.i 5}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[6] V-table-Win
ON LEAVE OF eb.i-code[6] IN FRAME Corr /* Code[6] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {est/val-inks.i 6}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[6] V-table-Win
ON VALUE-CHANGED OF eb.i-code[6] IN FRAME Corr /* Code[6] */
DO:
  {est/new-inks.i 6}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[7] V-table-Win
ON LEAVE OF eb.i-code[7] IN FRAME Corr /* Code[7] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {est/val-inks.i 7}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[7] V-table-Win
ON VALUE-CHANGED OF eb.i-code[7] IN FRAME Corr /* Code[7] */
DO:
  {est/new-inks.i 7}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[8] V-table-Win
ON LEAVE OF eb.i-code[8] IN FRAME Corr /* Code[8] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {est/val-inks.i 8}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[8] V-table-Win
ON VALUE-CHANGED OF eb.i-code[8] IN FRAME Corr /* Code[8] */
DO:
  {est/new-inks.i 8}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[9] V-table-Win
ON LEAVE OF eb.i-code[9] IN FRAME Corr /* Code[9] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {est/val-inks.i 9}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[9] V-table-Win
ON VALUE-CHANGED OF eb.i-code[9] IN FRAME Corr /* Code[9] */
DO:
  {est/new-inks.i 9}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-col
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-col V-table-Win
ON VALUE-CHANGED OF eb.i-col IN FRAME Corr /* Colors */
DO:
  IF DEC({&self-name}:SCREEN-VALUE) EQ 0 THEN
    eb.i-pass:SCREEN-VALUE = "".
  ELSE
  IF DEC(eb.i-pass:SCREEN-VALUE) EQ 0 THEN
    eb.i-pass:SCREEN-VALUE = "1".

  {est/def-inks.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-pass
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-pass V-table-Win
ON VALUE-CHANGED OF eb.i-pass IN FRAME Corr /* Passes */
DO:
  {est/def-inks.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.stack-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.stack-code V-table-Win
ON LEAVE OF eb.stack-code IN FRAME Corr /* Stack Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-stack-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.stack-code V-table-Win
ON VALUE-CHANGED OF eb.stack-code IN FRAME Corr /* Stack Code */
DO:
  RUN new-stack-code.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.stacks
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.stacks V-table-Win
ON VALUE-CHANGED OF eb.stacks IN FRAME Corr /* # of Stacks */
DO:
  RUN new-tr-cas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.tr-cas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.tr-cas V-table-Win
ON VALUE-CHANGED OF eb.tr-cas IN FRAME Corr /* # of Layers */
DO:
  RUN new-tr-cas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.tr-cnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.tr-cnt V-table-Win
ON VALUE-CHANGED OF eb.tr-cnt IN FRAME Corr /* Count */
DO:
  lv-layers = INT(eb.cas-pal:SCREEN-VALUE) / INT(eb.stacks:SCREEN-VALUE).
  {sys/inc/roundup.i lv-layers}
  ASSIGN
   eb.tr-cas:SCREEN-VALUE  = STRING(lv-layers)
   eb.cas-pal:SCREEN-VALUE = STRING(INT(eb.tr-cas:SCREEN-VALUE) *
                                    INT(eb.stacks:SCREEN-VALUE)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.tr-dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.tr-dep V-table-Win
ON LEAVE OF eb.tr-dep IN FRAME Corr /* Height */
DO:
   /*if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= 0.16 
   then do:
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      return no-apply.
   end.*/


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.tr-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.tr-len V-table-Win
ON LEAVE OF eb.tr-len IN FRAME Corr /* Length */
DO:
   /*if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= 0.16 
   then do:
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      return no-apply.
   end.*/


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.tr-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.tr-no V-table-Win
ON LEAVE OF eb.tr-no IN FRAME Corr /* Pallet # */
DO:
    FIND ITEM NO-LOCK 
      WHERE item.company = eb.company 
        AND item.i-no = eb.tr-no:SCREEN-VALUE 
        AND item.mat-type EQ "D" NO-ERROR.
    IF AVAIL item 
      THEN  ASSIGN eb.tr-len:SCREEN-VALUE = STRING(item.case-l)
                   eb.tr-wid:SCREEN-VALUE = STRING(item.case-w)
                   eb.tr-dep:SCREEN-VALUE = STRING(item.case-d).
      ELSE 
       IF LASTKEY <> -1 AND 
          eb.tr-no:SCREEN-VALUE <> "" 
         THEN DO:
        {&methods/lValidateError.i YES}

           MESSAGE 
             "Invalid Pallet #. Try Help." 
             VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
      {&methods/lValidateError.i NO}
       END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.tr-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.tr-wid V-table-Win
ON LEAVE OF eb.tr-wid IN FRAME Corr /* Width */
DO:
   /*if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= 0.16 
   then do:
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      return no-apply.
   end.*/


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

PROCEDURE WinExec EXTERNAL "KERNEL32.DLL":
       DEFINE INPUT PARAMETER programname AS cha.
       DEFINE INPUT PARAMETER visualstyle AS long.
       DEFINE RETURN PARAM statuscode AS LONG.
END.

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

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "est"}
  {src/adm/template/row-find.i "eb"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-tr-cnt V-table-Win 
PROCEDURE calc-tr-cnt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF ll-update-pack THEN
  DO WITH FRAME {&FRAME-NAME}:
    eb.tr-cnt:SCREEN-VALUE = "0".

    IF DEC(eb.cas-cnt:SCREEN-VALUE) NE 0 THEN
      ASSIGN
       eb.tr-cnt:SCREEN-VALUE = STRING(DEC(eb.cas-cnt:SCREEN-VALUE) *
                                       DEC(eb.cas-pal:SCREEN-VALUE))
       eb.cas-wt:SCREEN-VALUE = "0".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-frat V-table-Win 
PROCEDURE copy-frat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-eb FOR eb.


  IF eb.form-no NE 0                                      AND
     CAN-FIND(FIRST b-eb WHERE b-eb.company EQ eb.company
                           AND b-eb.est-no  EQ eb.est-no
                           AND b-eb.eqty    EQ eb.eqty
                           AND b-eb.form-no NE 0
                           AND ROWID(b-eb)  NE ROWID(eb)) THEN
    RUN est/copyfrat.p (ROWID(eb)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-inks V-table-Win 
PROCEDURE copy-inks :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-eb FOR eb.


  IF eb.form-no NE 0                                      AND
     CAN-FIND(FIRST b-eb WHERE b-eb.company EQ eb.company
                           AND b-eb.est-no  EQ eb.est-no
                           AND b-eb.eqty    EQ eb.eqty
                           AND b-eb.form-no NE 0
                           AND ROWID(b-eb)  NE ROWID(eb)) THEN
    RUN est/copyinks.p (ROWID(eb)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-pack V-table-Win 
PROCEDURE copy-pack :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-eb FOR eb.


  IF eb.form-no NE 0                                      AND
     CAN-FIND(FIRST b-eb WHERE b-eb.company EQ eb.company
                           AND b-eb.est-no  EQ eb.est-no
                           AND b-eb.eqty    EQ eb.eqty
                           AND b-eb.form-no NE 0
                           AND ROWID(b-eb)  NE ROWID(eb)) THEN
    RUN est/copypack.p (ROWID(eb)).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dynamic-labels V-table-Win 
PROCEDURE dynamic-labels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF lv-label[1] EQ "" THEN lv-label[1] = eb.cas-cnt:LABEL.

    IF eb.form-no EQ 0 THEN
      eb.cas-cnt:LABEL = "Sets/Code".
    ELSE
      eb.cas-cnt:LABEL= lv-label[1].
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-case-dims V-table-Win 
PROCEDURE enable-case-dims :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DO WITH FRAME {&FRAME-NAME}:
    IF CAN-FIND(FIRST item
                WHERE item.company   EQ eb.company
                   AND item.i-no     EQ eb.cas-no:SCREEN-VALUE
                   AND item.mat-type EQ "C"
                   AND item.i-code   EQ "R") THEN
      ASSIGN
       eb.cas-len:SENSITIVE = NO
       eb.cas-wid:SENSITIVE = NO
       eb.cas-dep:SENSITIVE = NO
       eb.cas-wt:SENSITIVE  = NO.
    ELSE
      ASSIGN
       eb.cas-len:SENSITIVE = YES
       eb.cas-wid:SENSITIVE = YES
       eb.cas-dep:SENSITIVE = YES
       eb.cas-wt:SENSITIVE  = YES.
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
  DEF BUFFER b-eb FOR eb.
  DEF BUFFER set-eb FOR eb.
  DEF BUFFER bf-eb FOR eb. /* for 2 pc box copy */
  DEF VAR lv-cas-pal LIKE eb.cas-pal NO-UNDO.
  DEF VAR lv-tr-cnt LIKE eb.tr-cnt NO-UNDO.
  DEF VAR lv-error AS log NO-UNDO.
  DEF VAR ll-ans AS LOG NO-UNDO.
  DEF VAR ll-assem AS LOG INIT ? NO-UNDO.
{&methods/lValidateError.i YES}

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN new-carrier.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST set-eb
      WHERE set-eb.company EQ eb.company
        AND set-eb.est-no  EQ eb.est-no
        AND set-eb.form-no EQ 0
      NO-LOCK NO-ERROR.
  IF AVAIL set-eb THEN
    ll-assem = (set-eb.stock-no EQ "" AND set-eb.set-is-assembled) OR
               (set-eb.stock-no NE "" AND
                CAN-FIND(FIRST itemfg
                         WHERE itemfg.company EQ set-eb.company
                           AND itemfg.i-no    EQ set-eb.stock-no
                           AND itemfg.alloc   NE YES)).

  FOR EACH b-eb
      WHERE b-eb.company EQ eb.company
        AND b-eb.est-no  EQ eb.est-no
        AND ROWID(b-eb)  NE ROWID(eb):

    b-eb.chg-method = eb.chg-method.

    IF ll-assem THEN
      ASSIGN
       b-eb.carrier   = eb.carrier
       b-eb.carr-dscr = eb.carr-dscr
       b-eb.dest-code = eb.dest-code.
  END.

  IF ll-unit-calc THEN DO WITH FRAME {&FRAME-NAME}:
    FIND xest NO-LOCK
        WHERE xest.company EQ eb.company
          AND xest.est-no  EQ eb.est-no
        NO-ERROR.
    FIND xeb WHERE RECID(xeb) EQ RECID(eb) NO-LOCK.
    RUN cec/kpallet.p (RECID(xeb), OUTPUT lv-cas-pal, OUTPUT lv-tr-cnt, OUTPUT lv-numstack, OUTPUT lv-stack-code, OUTPUT lv-error).
    IF lv-error THEN DO:
      MESSAGE "An error occured while attempting to calculate the number of pallets. "
              SKIP
              "Please review any previous error messages for more information." 
          VIEW-AS ALERT-BOX ERROR.
      ASSIGN
       eb.cas-pal:SCREEN-VALUE    = ?
       eb.tr-cnt:SCREEN-VALUE     = ?
       eb.stacks:SCREEN-VALUE     = ?
       eb.stack-code:SCREEN-VALUE = ?
       eb.cas-pal                 = ?
       eb.tr-cnt                  = ?.
      RETURN.
    END.

    lv-layers = lv-cas-pal / lv-numstack.
    {sys/inc/roundup.i lv-layers}

    IF cecunit-chr NE "AUTOCALC" THEN DO:
      ll-ans = NO.
      MESSAGE "Stack under pallet height?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll-ans.

      ll-ans = NOT ll-ans.

      RUN cec/d-layers.w (ll-ans, ROWID(eb), eb.tr-no, eb.tr-dep, eb.cas-cnt,
                          lv-numstack, INPUT-OUTPUT lv-layers).
    END.

    ASSIGN
     lv-cas-pal    = lv-numstack * lv-layers
     lv-tr-cnt     = eb.cas-cnt * lv-cas-pal
     eb.cas-pal    = lv-cas-pal
     eb.tr-cnt     = lv-tr-cnt
     eb.tr-cas     = lv-layers
     eb.stacks     = lv-numstack
     eb.stack-code = lv-stack-code.
  END.

  IF ll-update-pack OR ll-unit-calc THEN DO:
    lv-layers = eb.tr-cas.
    RUN cec/d-layers.w (NO, ROWID(eb), eb.tr-no, eb.tr-dep, eb.cas-cnt,
                        eb.stacks, INPUT-OUTPUT lv-layers).

    IF (cecunit-log EQ NO AND eb.tr-cas GT lv-layers) OR
        cecunit-log THEN DO:
        ll-ans = NO.

        IF cecunit-log EQ NO THEN
           MESSAGE "Stacks are over pallet height, " +
                   "display pallet stacking options?"
               VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
               UPDATE ll-ans.
        ELSE
           MESSAGE "Display pallet stacking options?"
               VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
               UPDATE ll-ans.

        IF ll-ans THEN
           RUN cec/d-layers.w (YES, ROWID(eb), eb.tr-no, eb.tr-dep, eb.cas-cnt,
                               eb.stacks, INPUT-OUTPUT eb.tr-cas).
    END.

    ASSIGN
     eb.cas-pal = eb.stacks * eb.tr-cas
     eb.tr-cnt  = eb.cas-cnt * eb.cas-pal.

    DISPLAY eb.cas-pal eb.tr-cnt WITH FRAME {&FRAME-NAME}.
  END.

  IF (ll-unit-calc OR ll-update-pack) AND cecunit-int NE 1 THEN DO:
    IF est.est-type EQ 6 AND eb.form-no NE 0               AND
       eb.cas-no NE "" AND eb.tr-no NE "" AND NOT ll-assem AND
       CAN-FIND(FIRST b-eb
                WHERE b-eb.company EQ eb.company
                  AND b-eb.est-no  EQ eb.est-no
                  AND b-eb.form-no NE 0
                  AND ROWID(b-eb)  NE ROWID(eb))           THEN DO:

      MESSAGE "Copy this form's Packing Info to all other forms?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans.

      IF ll-ans THEN
      FOR EACH b-eb
          WHERE b-eb.company EQ eb.company
            AND b-eb.est-no  EQ eb.est-no
            AND b-eb.eqty    EQ eb.eqty
            AND b-eb.form-no NE 0
            AND ROWID(b-eb)  NE ROWID(eb):

        ASSIGN
         b-eb.cas-no   = eb.cas-no
         b-eb.cas-cost = eb.cas-cost
         b-eb.cas-cnt  = eb.cas-cnt
         b-eb.cas-len  = eb.cas-len
         b-eb.cas-wid  = eb.cas-wid
         b-eb.cas-dep  = eb.cas-dep
         b-eb.cas-pal  = eb.cas-pal
         b-eb.cas-wt   = eb.cas-wt
         b-eb.tr-no    = eb.tr-no
         b-eb.tr-cost  = eb.tr-cost
         b-eb.tr-cnt   = eb.tr-cnt
         b-eb.tr-len   = eb.tr-len
         b-eb.tr-wid   = eb.tr-wid
         b-eb.tr-dep   = eb.tr-dep
         b-eb.tr-cas   = eb.tr-cas.
      END.
    END.
  END.
  ELSE DO:
      /* If this is a 2 piece box, make sure changes are copied to the other form */
      FIND xest NO-LOCK
          WHERE xest.company EQ eb.company
            AND xest.est-no  EQ eb.est-no
          NO-ERROR.

      find first bf-eb where bf-eb.company = xest.company and
                         bf-eb.est-no    = xest.est-no and
                         bf-eb.form-no = 0 NO-LOCK NO-ERROR. 

      find first b-eb where b-eb.company = xest.company and
                         b-eb.est-no    = xest.est-no and
                         b-eb.form-no = 1 NO-LOCK NO-ERROR. 
      /* 11151303 - taking set-is-assembled and pur-man and stock-no being */
      /* the same on form 0 & 1 to mean 2 pc box */
      IF (xest.est-type EQ 6 
              AND avail(bf-eb) 
              AND AVAIL(b-eb)
              AND bf-eb.pur-man = YES 
              AND bf-eb.set-is-assembled 
              AND bf-eb.stock-no EQ b-eb.stock-no) THEN DO:

          /* find eb for other form */
          FIND FIRST bf-eb
              WHERE bf-eb.company EQ eb.company
                AND bf-eb.est-no  EQ eb.est-no                
                AND bf-eb.form-no NE eb.form-no
              EXCLUSIVE-LOCK.

          IF AVAIL bf-eb THEN
            ASSIGN
             bf-eb.cas-no   = eb.cas-no
             bf-eb.cas-cost = eb.cas-cost
             bf-eb.cas-cnt  = eb.cas-cnt
             bf-eb.cas-len  = eb.cas-len
             bf-eb.cas-wid  = eb.cas-wid
             bf-eb.cas-dep  = eb.cas-dep
             bf-eb.cas-pal  = eb.cas-pal
             bf-eb.cas-wt   = eb.cas-wt
             bf-eb.tr-no    = eb.tr-no
             bf-eb.tr-cost  = eb.tr-cost
             bf-eb.tr-cnt   = eb.tr-cnt
             bf-eb.tr-len   = eb.tr-len
             bf-eb.tr-wid   = eb.tr-wid
             bf-eb.tr-dep   = eb.tr-dep
             bf-eb.tr-cas   = eb.tr-cas.
          RELEASE bf-eb.
      END.


  END.
  /*{sys/inc/k16bb.i eb.cas-wid  } 
  {sys/inc/k16bb.i eb.cas-len  } 
  {sys/inc/k16bb.i eb.cas-dep  } 
  {sys/inc/k16bb.i eb.tr-wid  } 
  {sys/inc/k16bb.i eb.tr-len  } 
  {sys/inc/k16bb.i eb.tr-dep  }*/

  RUN dispatch ("display-fields").
{&methods/lValidateError.i NO}

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var char-hdl as cha no-undo.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ).

  /* Code placed here will execute AFTER standard behavior.    */
  /*RUN dispatch ('display-fields').

  RUN dispatch ('assign-record').

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'tableio-source',OUTPUT char-hdl).
  RUN disable-cancel-button IN WIDGET-HANDLE(char-hdl).

  RUN dispatch ('disable-fields').*/

  RUN update-ink.  /* to disable unit fields */

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

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN dynamic-labels.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ll-foam = CAN-FIND(FIRST style
                     WHERE style.company EQ eb.company
                       AND style.style   EQ eb.style
                       AND style.type    EQ "F").

  if eb.tr-cas <> 0 then  lv-numstack = eb.cas-pal / eb.tr-cas.
  else lv-numstack = 0.

  def var lv-cas-pal like eb.cas-pal no-undo.
  def var lv-tr-cnt like eb.tr-cnt no-undo.
  def var lv-error as log no-undo.

  find xest where xest.company = eb.company and
                     xest.est-no = eb.est-no NO-LOCK.
  find xeb where recid(xeb) = recid(eb) NO-LOCK.

  /*  used variables first. But now using database field instead of variables */
  IF xeb.part-no NE "" AND xeb.form-no NE 0 THEN
     run cec/kpallet.p (recid(xeb), output lv-cas-pal, output lv-tr-cnt, output lv-numstack, output lv-stack-code, output lv-error).

  if lv-error THEN
     assign lv-numstack = ?
            lv-stack-code = ?.

  {sys/inc/roundup.i lv-numstack}
  IF eb.stack-code = "" THEN eb.stack-code:SCREEN-VALUE IN FRAME {&FRAME-NAME} = lv-stack-code.
  RUN stackImage (eb.stack-code:SCREEN-VALUE).

  IF v-cecscrn-char EQ "Decimal" THEN
     ASSIGN
        eb.cas-len:FORMAT = ">9.999999"
        eb.cas-wid:FORMAT = ">9.999999"
        eb.cas-dep:FORMAT = ">9.999999"
        eb.tr-len:FORMAT = ">9.999999"
        eb.tr-wid:FORMAT = ">9.999999"
        eb.tr-dep:FORMAT = ">9.999999".

  /* don't display any value if not unitized */
  IF eb.form-no = 0 AND NOT eb.pur-man THEN
   ASSIGN eb.cas-no:SCREEN-VALUE = ""
            eb.cas-cost:SCREEN-VALUE = "0"
            eb.cas-cnt:SCREEN-VALUE  = "0"
            eb.cas-pal:SCREEN-VALUE  = "0"
            eb.cas-len:SCREEN-VALUE = "0"
            eb.cas-wid:SCREEN-VALUE = "0"
            eb.cas-dep:SCREEN-VALUE = "0"
            eb.cas-wt:SCREEN-VALUE  = "0"
            eb.tr-no:SCREEN-VALUE   = ""
            eb.tr-cost:SCREEN-VALUE = "0"
            eb.tr-cnt:SCREEN-VALUE  = "0"
            eb.tr-cas:SCREEN-VALUE  = "0"
            eb.tr-len:SCREEN-VALUE  = "0"
            eb.tr-wid:SCREEN-VALUE  = "0"
            eb.tr-dep:SCREEN-VALUE  = "0"
            eb.stacks:SCREEN-VALUE  = "0"
            eb.stack-code:SCREEN-VALUE = ""
            eb.weight-m:SCREEN-VALUE   = "0"
            eb.carrier:SCREEN-VALUE    = ""
            eb.carr-dscr:SCREEN-VALUE  = ""
            eb.dest-code:SCREEN-VALUE  = ""
            eb.fr-out-c:SCREEN-VALUE  = "0"
            eb.fr-out-m:SCREEN-VALUE  = "0".

  RELEASE xest.
  RELEASE xeb. 
  RUN check-modified IN THIS-PROCEDURE ('clear':U) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var li-num-of-code as int no-undo.
{&methods/lValidateError.i YES}
  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    IF eb.form-no NE 0 THEN DO:
      {est/valinks2.i 1}
      {est/valinks2.i 2}
      {est/valinks2.i 3}
      {est/valinks2.i 4}
      {est/valinks2.i 5}
      {est/valinks2.i 6}
      {est/valinks2.i 7}
      {est/valinks2.i 8}
      {est/valinks2.i 9}
      {est/valinks2.i 10}
    END.

    IF ll-unit-calc EQ NO AND ll-update-pack EQ NO THEN DO:
      RUN est/val-inks.p (FRAME {&FRAME-NAME}:HANDLE, 0, ?) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

      li-num-of-code = 0.
      if eb.i-code[1]:screen-value <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code[2]:screen-value <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code[3]:screen-value <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code[4]:screen-value <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code[5]:screen-value <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code[6]:screen-value <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code[7]:screen-value <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code[8]:screen-value <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code[9]:screen-value <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code[10]:screen-value <> "" then li-num-of-code = li-num-of-code + 1.

      if li-num-of-code <> (integer(eb.i-col:screen-value) + 
                           integer(eb.i-coat:screen-value) )
      then do:
          message "Invalid Number of Color and Coating." view-as alert-box.
          apply "entry" to eb.i-col.
          return no-apply.
      end.                     
    END.

    ELSE DO:
      if eb.carrier:screen-value <> "" and
        not can-find(first carrier where carrier.carrier = eb.carrier:screen-value)
      then do:
         message "Invalid Carrier. Try Help." view-as alert-box error.
         apply "entry" to eb.carrier.
         return no-apply.
      end.
      if eb.cas-no:screen-value <> "" and
         not can-find(item where item.company = eb.company and item.i-no = eb.cas-no:screen-value)
      then do:
            message "Invalid Packing Code. Try Help." view-as alert-box.
            apply "entry" to eb.cas-no.
            return no-apply.
      end.
      if eb.tr-no:screen-value <> "" and
         not can-find(item where item.company = eb.company and item.i-no = eb.tr-no:screen-value)
      then do:
            message "Invalid Unit#. Try Help." view-as alert-box error.
            apply "entry" to eb.tr-no.
            return no-apply.
      end.
      /*if decimal(eb.cas-len:screen-value) - trunc(decimal(eb.cas-len:screen-value),0) >= 0.16 
      then do:
        message "Can not have more than .15 as decimal, field is (inches.16ths) "
            view-as alert-box error.
        apply "entry" to eb.cas-len.
        return no-apply.
      end.
      if decimal(eb.cas-wid:screen-value) - trunc(decimal(eb.cas-wid:screen-value),0) >= 0.16 
      then do:
        message "Can not have more than .15 as decimal, field is (inches.16ths) "
            view-as alert-box error.
        apply "entry" to eb.cas-wid.
        return no-apply.
      end.
      if decimal(eb.cas-dep:screen-value) - trunc(decimal(eb.cas-dep:screen-value),0) >= 0.16 
      then do:
        message "Can not have more than .15 as decimal, field is (inches.16ths) "
            view-as alert-box error.
        apply "entry" to eb.cas-dep.
        return no-apply.
      end.
      if decimal(eb.tr-len:screen-value) - trunc(decimal(eb.tr-len:screen-value),0) >= 0.16 
      then do:
        message "Can not have more than .15 as decimal, field is (inches.16ths) "
            view-as alert-box error.
        apply "entry" to eb.tr-len.
        return no-apply.
      end.
      if decimal(eb.tr-wid:screen-value) - trunc(decimal(eb.tr-wid:screen-value),0) >= 0.16 
      then do:
        message "Can not have more than .15 as decimal, field is (inches.16ths) "
            view-as alert-box error.
        apply "entry" to eb.tr-wid.
        return no-apply.
      end.
      if decimal(eb.tr-dep:screen-value) - trunc(decimal(eb.tr-dep:screen-value),0) >= 0.16 
      then do:
        message "Can not have more than .15 as decimal, field is (inches.16ths) "
            view-as alert-box error.
        apply "entry" to eb.tr-dep.
        return no-apply.
      end.*/

      RUN valid-cas-pal NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

      RUN valid-cas-wt NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

      RUN valid-stack-code NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

      RUN valid-carrier NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

      RUN valid-dest-code NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   ll-unit-calc   = NO.
   ll-update-pack = NO.

  DISABLE eb.cas-no
          eb.cas-cost
          eb.cas-cnt
          eb.cas-len
          eb.cas-wid
          eb.cas-dep
          eb.cas-pal
          eb.cas-wt
          eb.tr-no
          eb.tr-cost
          eb.tr-cnt
          eb.tr-len
          eb.tr-wid
          eb.tr-dep
          eb.tr-cas
          eb.carrier
          eb.carr-dscr
          eb.weight-m
          eb.dest-code
          eb.fr-out-c
          eb.fr-out-m
          eb.chg-method
          eb.stacks
          eb.stack-code
      WITH FRAME {&FRAME-NAME}.

  RUN release-shared-buffers.
{&methods/lValidateError.i NO}

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-carrier V-table-Win 
PROCEDURE new-carrier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST carrier
        {sys\look/carrierW.i}
          AND carrier.carrier EQ eb.carrier:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL carrier THEN eb.carr-dscr:SCREEN-VALUE = carrier.dscr.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-cas-pal V-table-Win 
PROCEDURE new-cas-pal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    lv-layers = INT(eb.cas-pal:SCREEN-VALUE) / INT(eb.stacks:SCREEN-VALUE).
    {sys/inc/roundup.i lv-layers}
    eb.tr-cas:SCREEN-VALUE = STRING(lv-layers).

    RUN calc-tr-cnt.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-stack-code V-table-Win 
PROCEDURE new-stack-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    eb.stack-code:SCREEN-VALUE = CAPS(eb.stack-code:SCREEN-VALUE).
    FIND FIRST stackPattern
         WHERE stackPattern.stackCode EQ eb.stack-code:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL stackPattern AND INT(eb.stacks:SCREEN-VALUE) NE stackPattern.stackCount THEN DO:
      eb.stacks:SCREEN-VALUE = STRING(stackCount).
      RUN new-tr-cas.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-tr-cas V-table-Win 
PROCEDURE new-tr-cas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF INT(eb.stacks:SCREEN-VALUE) EQ 0 THEN eb.stacks:SCREEN-VALUE = "1".
    eb.cas-pal:SCREEN-VALUE = STRING(INT(eb.tr-cas:SCREEN-VALUE) *
                                     INT(eb.stacks:SCREEN-VALUE)).
    RUN calc-tr-cnt.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-ink V-table-Win 
PROCEDURE reset-ink :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{&methods/lValidateError.i YES}
IF eb.form-no NE 0 THEN DO:
  {custom/checkuse.i}

  message "Are you sure you want to reset all ink codes?" view-as alert-box question
         button yes-no update ll-ans as log.
  if not ll-ans then return no-apply.       

  def buffer bf-eb for eb .
/*********** copied from uest3.p ***********/

      def var k as int no-undo.
      def var counter as int no-undo.
      def var i as int no-undo.
      def var j as int no-undo.
      def var save_id as recid no-undo.
      def var save_id2 as recid no-undo.
      def buffer alt-item for item .
      def var choice as log no-undo.

      find first style where style.company = eb.company and
                 style.style = eb.style no-lock no-error.
      if avail style then do:
         if k = 0 then k = integer(style.material[3]).

         RELEASE ITEM.

         IF style.material[2] NE "" THEN
            find first item where
                 item.company = eb.company and
                 item.i-no = style.material[2]
                 no-lock no-error.

         if avail item then k = integer(style.material[3]).

         RELEASE alt-item.

         IF style.material[6] NE "" THEN
            find first alt-item where
                 alt-item.company  = eb.company  and
                 alt-item.mat-type = "V"     and
                 alt-item.i-no     = style.material[6]
                 no-lock no-error.
      end.
      if not avail item or not avail alt-item or (k = 0) then do:
         find first ce-ctrl where ce-ctrl.company = eb.company and
                                  ce-ctrl.loc = eb.loc
                                   no-lock no-error.
         if k = 0 then k = ce-ctrl.def-inkcov.
         if not avail item then do:
            find first item where item.company = eb.company and
                       item.i-no = ce-ctrl.def-ink no-lock no-error.
         end.
         if not avail alt-item then
            find first alt-item where alt-item.company  = eb.company  and
                                      alt-item.mat-type = "V"     and
                                      alt-item.i-no     = ce-ctrl.def-coat
                                      no-lock no-error.
      end.

      ASSIGN
      save_id = recid(item)
      save_id2 = recid(alt-item)
      j = (integer(eb.i-col:screen-value in frame {&frame-name})
          + integer(eb.i-coat:screen-value in frame {&frame-name})  ) 
          / integer(eb.i-pass:screen-value in frame {&frame-name})
      counter = 1
      choice = true.
      {sys/inc/roundup.i j}

/*    do i = 1 to 10:
       if eb.i-code[i] ne "" then do:
          choice = no.
          leave.
       end.
      end.     
 commented to recalc every time */

      if choice then do i = 1 to 10:
         if i le integer(eb.i-col:screen-value) then do with frame {&frame-name}:
              find item where recid(item) = save_id no-lock no-error.
             /* assign bf-eb.i-ps[i]   = counter
                     bf-eb.i-code[i] = item.i-no
                     bf-eb.i-dscr[i] = item.est-dscr
                     bf-eb.i-%[i]    = k.
             */
             case string(i) :
                when "1" then assign eb.i-ps[1]:screen-value   = string(counter)
                                     eb.i-code[1]:screen-value = item.i-no
                                     eb.i-dscr[1]:screen-value = item.est-dscr
                                     eb.i-%[1]:screen-value    = string(k).
                when "2" then assign eb.i-ps[2]:screen-value   = string(counter)
                                     eb.i-code[2]:screen-value = item.i-no
                                     eb.i-dscr[2]:screen-value = item.est-dscr
                                     eb.i-%[2]:screen-value    = string(k).
                when "3" then assign eb.i-ps[3]:screen-value   = string(counter)
                                     eb.i-code[3]:screen-value = item.i-no
                                     eb.i-dscr[3]:screen-value = item.est-dscr
                                     eb.i-%[3]:screen-value    = string(k).
                when "4" then assign eb.i-ps[4]:screen-value   = string(counter)
                                     eb.i-code[4]:screen-value = item.i-no
                                     eb.i-dscr[4]:screen-value = item.est-dscr
                                     eb.i-%[4]:screen-value    = string(k).
                when "5" then assign eb.i-ps[5]:screen-value   = string(counter)
                                     eb.i-code[5]:screen-value = item.i-no
                                     eb.i-dscr[5]:screen-value = item.est-dscr
                                     eb.i-%[5]:screen-value    = string(k).
                when "6" then assign eb.i-ps[6]:screen-value   = string(counter)
                                     eb.i-code[6]:screen-value = item.i-no
                                     eb.i-dscr[6]:screen-value = item.est-dscr
                                     eb.i-%[6]:screen-value    = string(k).
                when "7" then assign eb.i-ps[7]:screen-value   = string(counter)
                                     eb.i-code[7]:screen-value = item.i-no
                                     eb.i-dscr[7]:screen-value = item.est-dscr
                                     eb.i-%[7]:screen-value    = string(k).
                when "8" then assign eb.i-ps[8]:screen-value   = string(counter)
                                     eb.i-code[8]:screen-value = item.i-no
                                     eb.i-dscr[8]:screen-value = item.est-dscr
                                     eb.i-%[8]:screen-value    = string(k).
                when "9" then assign eb.i-ps[9]:screen-value   = string(counter)
                                     eb.i-code[9]:screen-value = item.i-no
                                     eb.i-dscr[9]:screen-value = item.est-dscr
                                     eb.i-%[9]:screen-value    = string(k).
                when "10" then assign eb.i-ps[10]:screen-value   = string(counter)
                                     eb.i-code[10]:screen-value = item.i-no
                                     eb.i-dscr[10]:screen-value = item.est-dscr
                                     eb.i-%[10]:screen-value    = string(k).             
             end case.
         end.
         else if (i > integer(eb.i-col:screen-value)) and
                 (i <= (integer(eb.i-col:screen-value) + 
                       integer(eb.i-coat:screen-value)))
         then do:
              find alt-item where recid(alt-item) = save_id2 no-lock no-error.
         /*     assign bf-eb.i-ps[i]   = counter
                     bf-eb.i-code[i] = alt-item.i-no
                     bf-eb.i-dscr[i] = alt-item.est-dscr
                     bf-eb.i-%[i]    = 100.
           */
              case string(i) :
                when "1" then assign eb.i-ps[1]:screen-value   = string(counter)
                                     eb.i-code[1]:screen-value = alt-item.i-no
                                     eb.i-dscr[1]:screen-value = alt-item.est-dscr
                                     eb.i-%[1]:screen-value    = "100".
                when "2" then assign eb.i-ps[2]:screen-value   = string(counter)
                                     eb.i-code[2]:screen-value = alt-item.i-no
                                     eb.i-dscr[2]:screen-value = alt-item.est-dscr
                                     eb.i-%[2]:screen-value    = "100".
                when "3" then assign eb.i-ps[3]:screen-value   = string(counter)
                                     eb.i-code[3]:screen-value = alt-item.i-no
                                     eb.i-dscr[3]:screen-value = alt-item.est-dscr
                                     eb.i-%[3]:screen-value    = "100".
                when "4" then assign eb.i-ps[4]:screen-value   = string(counter)
                                     eb.i-code[4]:screen-value = alt-item.i-no
                                     eb.i-dscr[4]:screen-value = alt-item.est-dscr
                                     eb.i-%[4]:screen-value    = "100".
                when "5" then assign eb.i-ps[5]:screen-value   = string(counter)
                                     eb.i-code[5]:screen-value = alt-item.i-no
                                     eb.i-dscr[5]:screen-value = alt-item.est-dscr
                                     eb.i-%[5]:screen-value    = "100".
                when "6" then assign eb.i-ps[6]:screen-value   = string(counter)
                                     eb.i-code[6]:screen-value = alt-item.i-no
                                     eb.i-dscr[6]:screen-value = alt-item.est-dscr
                                     eb.i-%[6]:screen-value    = "100".
                when "7" then assign eb.i-ps[7]:screen-value   = string(counter)
                                     eb.i-code[7]:screen-value = alt-item.i-no
                                     eb.i-dscr[7]:screen-value = alt-item.est-dscr
                                     eb.i-%[7]:screen-value    = "100".
                when "8" then assign eb.i-ps[8]:screen-value   = string(counter)
                                     eb.i-code[8]:screen-value = alt-item.i-no
                                     eb.i-dscr[8]:screen-value = alt-item.est-dscr
                                     eb.i-%[8]:screen-value    = "100".
                when "9" then assign eb.i-ps[9]:screen-value   = string(counter)
                                     eb.i-code[9]:screen-value = alt-item.i-no
                                     eb.i-dscr[9]:screen-value = alt-item.est-dscr
                                     eb.i-%[9]:screen-value    = "100".
                when "10" then assign eb.i-ps[10]:screen-value   = string(counter)
                                     eb.i-code[10]:screen-value = alt-item.i-no
                                     eb.i-dscr[10]:screen-value = alt-item.est-dscr
                                     eb.i-%[10]:screen-value    = "100".             
             end.                   
         end.
         else if (i >  integer(eb.i-col:screen-value) + 
                       integer(eb.i-coat:screen-value) )
         then do:
        /*    assign bf-eb.i-ps[i]   = 0  
                     bf-eb.i-code[i] = ""
                     bf-eb.i-dscr[i] = "" 
                     bf-eb.i-%[i]    = 0.  */
              case string(i) :
                   when "1" then assign eb.i-ps[1]:screen-value   = "0"
                                        eb.i-code[1]:screen-value = ""
                                        eb.i-dscr[1]:screen-value = ""
                                        eb.i-%[1]:screen-value    = "0".
                   when "2" then assign eb.i-ps[2]:screen-value   = "0"
                                        eb.i-code[2]:screen-value = ""
                                        eb.i-dscr[2]:screen-value = ""
                                        eb.i-%[2]:screen-value    = "0".
                   when "3" then assign eb.i-ps[3]:screen-value   = "0"
                                        eb.i-code[3]:screen-value = ""
                                        eb.i-dscr[3]:screen-value = ""
                                        eb.i-%[3]:screen-value    = "0".
                   when "4" then assign eb.i-ps[4]:screen-value   = "0"
                                        eb.i-code[4]:screen-value = ""
                                        eb.i-dscr[4]:screen-value = ""
                                        eb.i-%[4]:screen-value    = "0".
                   when "5" then assign eb.i-ps[5]:screen-value   = "0"
                                        eb.i-code[5]:screen-value = ""
                                        eb.i-dscr[5]:screen-value = ""
                                        eb.i-%[5]:screen-value    = "0".
                   when "6" then assign eb.i-ps[6]:screen-value   = "0"
                                        eb.i-code[6]:screen-value = ""
                                        eb.i-dscr[6]:screen-value = ""
                                        eb.i-%[6]:screen-value    = "0".
                   when "7" then assign eb.i-ps[7]:screen-value   = "0"
                                        eb.i-code[7]:screen-value = ""
                                        eb.i-dscr[7]:screen-value = ""
                                        eb.i-%[7]:screen-value    = "0".
                   when "8" then assign eb.i-ps[8]:screen-value   = "0"
                                        eb.i-code[8]:screen-value = ""
                                        eb.i-dscr[8]:screen-value = ""
                                        eb.i-%[8]:screen-value    = "0".
                   when "9" then assign eb.i-ps[9]:screen-value   = "0"
                                        eb.i-code[9]:screen-value = ""
                                        eb.i-dscr[9]:screen-value = ""
                                        eb.i-%[9]:screen-value    = "0".
                   when "10" then assign eb.i-ps[10]:screen-value   = "0"
                                        eb.i-code[10]:screen-value = ""
                                        eb.i-dscr[10]:screen-value = ""
                                        eb.i-%[10]:screen-value    = "0".

              end case.       

         end.
         if i modulo j = 0 then counter = counter + 1.
         if counter > integer(eb.i-pass:screen-value) then counter = integer(eb.i-pass:screen-value).

      end.

   find bf-eb where recid(bf-eb) = recid(eb).
   assign bf-eb.i-ps[1] = int(eb.i-ps[1]:screen-value in frame {&frame-name})
          bf-eb.i-ps[2] = int(eb.i-ps[2]:screen-value in frame {&frame-name})
          bf-eb.i-ps[3] = int(eb.i-ps[3]:screen-value in frame {&frame-name})
          bf-eb.i-ps[4] = int(eb.i-ps[4]:screen-value in frame {&frame-name})
          bf-eb.i-ps[5] = int(eb.i-ps[5]:screen-value in frame {&frame-name})
          bf-eb.i-ps[6] = int(eb.i-ps[6]:screen-value in frame {&frame-name})
          bf-eb.i-ps[7] = int(eb.i-ps[7]:screen-value in frame {&frame-name})
          bf-eb.i-ps[8] = int(eb.i-ps[8]:screen-value in frame {&frame-name})
          bf-eb.i-ps[9] = int(eb.i-ps[9]:screen-value in frame {&frame-name})
          bf-eb.i-ps[10] = int(eb.i-ps[10]:screen-value in frame {&frame-name})
          bf-eb.i-code[1] = eb.i-code[1]:screen-value 
          bf-eb.i-code[2] = eb.i-code[2]:screen-value 
          bf-eb.i-code[3] = eb.i-code[3]:screen-value 
          bf-eb.i-code[4] = eb.i-code[4]:screen-value 
          bf-eb.i-code[5] = eb.i-code[5]:screen-value 
          bf-eb.i-code[6] = eb.i-code[6]:screen-value 
          bf-eb.i-code[7] = eb.i-code[7]:screen-value 
          bf-eb.i-code[8] = eb.i-code[8]:screen-value 
          bf-eb.i-code[9] = eb.i-code[9]:screen-value 
          bf-eb.i-code[10] = eb.i-code[10]:screen-value 
          bf-eb.i-dscr[1] = eb.i-dscr[1]:screen-value 
          bf-eb.i-dscr[2] = eb.i-dscr[2]:screen-value 
          bf-eb.i-dscr[3] = eb.i-dscr[3]:screen-value 
          bf-eb.i-dscr[4] = eb.i-dscr[4]:screen-value 
          bf-eb.i-dscr[5] = eb.i-dscr[5]:screen-value 
          bf-eb.i-dscr[6] = eb.i-dscr[6]:screen-value 
          bf-eb.i-dscr[7] = eb.i-dscr[7]:screen-value 
          bf-eb.i-dscr[8] = eb.i-dscr[8]:screen-value 
          bf-eb.i-dscr[9] = eb.i-dscr[9]:screen-value 
          bf-eb.i-dscr[10] = eb.i-dscr[10]:screen-value 
          bf-eb.i-%[1] = int(eb.i-%[1]:screen-value )
          bf-eb.i-%[2] = int(eb.i-%[2]:screen-value )
          bf-eb.i-%[3] = int(eb.i-%[3]:screen-value )
          bf-eb.i-%[4] = int(eb.i-%[4]:screen-value )
          bf-eb.i-%[5] = int(eb.i-%[5]:screen-value )
          bf-eb.i-%[6] = int(eb.i-%[6]:screen-value )
          bf-eb.i-%[7] = int(eb.i-%[7]:screen-value )
          bf-eb.i-%[8] = int(eb.i-%[8]:screen-value )
          bf-eb.i-%[9] = int(eb.i-%[9]:screen-value )
          bf-eb.i-%[10] = int(eb.i-%[10]:screen-value )
          .
END.
{&methods/lValidateError.i NO}

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-job-stds V-table-Win 
PROCEDURE run-job-stds :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF AVAIL est THEN
  FOR EACH job-hdr
      WHERE job-hdr.company EQ est.company
        AND job-hdr.est-no  EQ est.est-no
      NO-LOCK,
      FIRST job
      where job.company EQ job-hdr.company
        and job.job     EQ job-hdr.job
        and job.job-no  EQ job-hdr.job-no
        and job.job-no2 EQ job-hdr.job-no2
        and job.est-no  EQ job-hdr.est-no
        AND job.opened  EQ YES
      NO-LOCK
      BREAK BY job.job:

    IF LAST(job.job) OR job-hdr.ord-no EQ est.ord-no THEN DO:
      RUN jc/jobstds.p (ROWID(job)).
      LEAVE.
    END.
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
  {src/adm/template/snd-list.i "est"}
  {src/adm/template/snd-list.i "eb"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-pack V-table-Win 
PROCEDURE set-pack :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-update AS LOG NO-UNDO.


  DEF BUFFER b-eb FOR eb.
  DEF BUFFER b-eb1 FOR eb.

  DEF VAR ll-ans AS LOG NO-UNDO.


  IF est.est-type EQ 6 AND eb.form-no NE 0 AND
     CAN-FIND(FIRST b-eb
              WHERE b-eb.company EQ eb.company
                AND b-eb.est-no  EQ eb.est-no
                AND b-eb.form-no NE 0
                AND ROWID(b-eb)  NE ROWID(eb)) THEN
  FIND FIRST b-eb
      WHERE b-eb.company EQ eb.company
        AND b-eb.est-no  EQ eb.est-no
        AND b-eb.eqty    EQ eb.eqty
        AND b-eb.yld-qty LT 0
        AND CAN-FIND(FIRST b-eb1
                     WHERE b-eb1.company EQ b-eb.company
                       AND b-eb1.est-no  EQ b-eb.est-no
                       AND b-eb1.cas-no  NE ""
                       AND ROWID(b-eb1)  NE ROWID(b-eb))
       NO-LOCK NO-ERROR.

  ll-ans = AVAIL b-eb.

  IF ll-ans THEN DO:
    ll-ans = NO.

    MESSAGE "Ship set assembled?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans.

    IF ll-ans THEN DO:
      FOR EACH b-eb
          WHERE b-eb.company EQ eb.company
            AND b-eb.est-no  EQ eb.est-no
            AND b-eb.form-no NE 0
            AND b-eb.yld-qty GE 0:

        ASSIGN
         b-eb.cas-no   = ""
         b-eb.cas-cost = 0
         b-eb.cas-cnt  = 0
         b-eb.cas-len  = 0
         b-eb.cas-wid  = 0
         b-eb.cas-dep  = 0
         b-eb.cas-pal  = 0
         b-eb.cas-wt   = 0
         b-eb.tr-no    = ""
         b-eb.tr-cost  = 0
         b-eb.tr-cnt   = 0
         b-eb.tr-len   = 0
         b-eb.tr-wid   = 0
         b-eb.tr-dep   = 0.
      END.

      FIND CURRENT eb NO-LOCK.
    END.
  END.

  op-update = NOT ll-ans OR eb.yld-qty LT 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stack-pattern V-table-Win 
PROCEDURE stack-pattern :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER pattern FOR reftable.
   DEF VAR lv-cmd AS cha NO-UNDO.
   DEF VAR lv-return AS INT NO-UNDO.
   DEF VAR tInt As Int No-undo.

/*   FIND FIRST pattern OUTER-JOIN where pattern.reftable = "STACKPAT"        */
/*                   and pattern.company = ""                                 */
/*                   and pattern.loc = ""                                     */
/*                   and pattern.code = eb.stack-code NO-LOCK NO-ERROR.       */
/*   IF AVAIL pattern THEN DO:                                                */
/*      IF pattern.dscr MATCHES "*.pdf*"  THEN DO:                            */
/*                                                                            */
/*         RUN ShellExecuteA(0, "open", pattern.dscr, "", "", 0, OUTPUT tInt).*/
/*         IF tInt LE 32 THEN                                                 */
/*         DO:                                                                */
/*            RUN custom/runapdf.p (OUTPUT lv-cmd).                           */
/*            lv-cmd = lv-cmd + chr(32) + pattern.Dscr.                       */
/*            RUN WinExec (INPUT lv-cmd, INPUT 1,OUTPUT lv-return).           */
/*         END.                                                               */
/*      END.                                                                  */

     FIND FIRST stackPattern OUTER-JOIN where stackPattern.stackCode = eb.stack-code NO-LOCK NO-ERROR.
     IF AVAIL stackPattern THEN DO:
         IF stackPattern.stackImage MATCHES "*.pdf*"  THEN DO:
             RUN ShellExecuteA(0, "open", stackPattern.stackImage, "", "", 0, OUTPUT tInt).
             IF tInt LE 32 THEN
             DO:
                RUN custom/runapdf.p (OUTPUT lv-cmd).
                lv-cmd = lv-cmd + chr(32) + stackPattern.stackImage.      
                RUN WinExec (INPUT lv-cmd, INPUT 1,OUTPUT lv-return).
             END.
         END.
         ELSE DO:
              lv-cmd = ".\custom\mspaint.exe".
              IF SEARCH("c:\winnt\system32\mspaint.exe") <> ? THEN lv-cmd = "c:\winnt\system32\mspaint.exe".
              ELSE IF    SEARCH("c:\windows\system32\mspaint.exe") <> ? THEN lv-cmd = "c:\windows\system32\mspaint.exe".
    
              lv-cmd = lv-cmd + " " + chr(34) + stackPattern.stackImage + CHR(34) .
              OS-COMMAND SILENT   VALUE(lv-cmd).
         END.
   END.
   ELSE DO:
       MESSAGE "No Stack Pattern found..." VIEW-AS ALERT-BOX ERROR.

   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stackImage V-table-Win 
PROCEDURE stackImage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipStackCode AS CHARACTER NO-UNDO.

  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

  stackImage:HIDDEN IN FRAME {&FRAME-NAME} = YES.
  IF ipStackCode NE '' THEN
  DO:
    FIND FIRST stackPattern NO-LOCK 
        WHERE stackPattern.stackCode EQ ipStackCode
        NO-ERROR.
    IF AVAILABLE stackPattern AND SEARCH(stackPattern.stackImage) NE ? THEN
        ASSIGN
            stackImage:HIDDEN = NO
            ldummy = stackImage:LOAD-IMAGE(stackPattern.stackImage).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE unit-calc V-table-Win 
PROCEDURE unit-calc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN est/assmpart.p (BUFFER eb, OUTPUT ll-assem-part).

  IF eb.form-no EQ 0 AND NOT ll-assem-part THEN DO:
    RUN dispatch ("cancel-record").
    RETURN "ADM-ERROR":U.
  END.

  {custom/checkuse.i}

  /*RUN set-pack (OUTPUT ll-update-pack).*/ ll-unit-calc = YES.

  if eb.tr-cas = 0 then eb.tr-cas:screen-value in frame {&frame-name} = "1".

  IF ll-unit-calc THEN DO:
    ENABLE eb.cas-no eb.cas-cost eb.cas-cnt eb.cas-len eb.cas-wid
           eb.cas-dep eb.cas-wt
           eb.tr-no
           /*eb.tr-cost eb.tr-cnt eb.tr-len eb.tr-wid eb.tr-dep
           eb.tr-cas */
           eb.carrier eb.weight-m eb.dest-code
           eb.fr-out-c eb.fr-out-m eb.chg-method
           /*lv-numstack lv-stackcode */
           WITH FRAME {&FRAME-NAME}.

    RUN enable-case-dims.
  END.

  disable eb.i-col eb.i-pass eb.i-coat eb.i-coat-p eb.i-coldscr
          eb.i-ps[1 for 10]
          eb.i-code[1 for 10]
          eb.i-dscr[1 for 10]
          eb.i-%[1 for 10]
          with frame {&frame-name}.

  apply "entry" to eb.cas-no in frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-ink V-table-Win 
PROCEDURE update-ink :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  disable eb.cas-no eb.cas-cost eb.cas-cnt eb.cas-len eb.cas-wid
          eb.cas-dep eb.cas-pal eb.cas-wt
          eb.tr-no eb.tr-cost eb.tr-cnt eb.tr-len eb.tr-wid eb.tr-dep
          eb.tr-cas
          eb.carrier eb.carr-dscr eb.weight-m eb.dest-code
          eb.fr-out-c eb.fr-out-m eb.chg-method
          eb.stacks eb.stack-code
          with frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-pack V-table-Win 
PROCEDURE update-pack :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {custom/checkuse.i}

  /*RUN set-pack (OUTPUT ll-update-pack).*/ ll-update-pack = YES.

  if eb.tr-cas = 0 then eb.tr-cas:screen-value in frame {&frame-name} = "1".

  IF ll-update-pack THEN DO:
    ENABLE eb.cas-no eb.cas-cost eb.cas-cnt eb.cas-len eb.cas-wid
           eb.cas-dep eb.cas-pal eb.cas-wt
           eb.tr-no eb.tr-cost /*eb.tr-cnt*/ eb.tr-len eb.tr-wid eb.tr-dep
           eb.tr-cas
           eb.carrier eb.weight-m eb.dest-code
           eb.fr-out-c eb.fr-out-m eb.chg-method
           eb.stacks eb.stack-code
           WITH FRAME {&FRAME-NAME}.
    IF INT(eb.stacks:SCREEN-VALUE) EQ 0 THEN RUN new-stack-code.
    RUN enable-case-dims.
  END.

  disable eb.i-col eb.i-pass eb.i-coat eb.i-coat-p eb.i-coldscr
          eb.i-ps[1 for 10]
          eb.i-code[1 for 10]
          eb.i-dscr[1 for 10]
          eb.i-%[1 for 10]
          with frame {&frame-name}.

  apply "entry" to eb.cas-no in frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-carrier V-table-Win 
PROCEDURE valid-carrier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    eb.carrier:SCREEN-VALUE = CAPS(eb.carrier:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST carrier
                    {sys\look/carrierW.i}
                      AND carrier.carrier EQ eb.carrier:SCREEN-VALUE)
    THEN DO:
      MESSAGE "Invalid " + TRIM(eb.carrier:LABEL) + ", try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO eb.carrier.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cas-no V-table-Win 
PROCEDURE valid-cas-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    if eb.cas-no:screen-value <> "" and
       not can-find(item where item.company = eb.company
                           and item.i-no = eb.cas-no:screen-value
                           and item.mat-type = "C")
    then do:
      message "Invalid " + TRIM(eb.cas-no:LABEL) + ", try help..."
          view-as alert-box.
      apply "entry" to eb.cas-no.
      return ERROR.
    end.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cas-pal V-table-Win 
PROCEDURE valid-cas-pal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-ans AS LOG NO-UNDO.


  {methods/lValidateError.i YES}
  /*IF ll-update-pack THEN
  DO WITH FRAME {&FRAME-NAME}:
    lv-layers = INT(eb.cas-pal:SCREEN-VALUE) / INT(eb.stacks:SCREEN-VALUE).
    {sys/inc/roundup.i lv-layers}

    IF INT(eb.stacks:SCREEN-VALUE) * lv-layers NE INT(eb.cas-pal:SCREEN-VALUE) THEN DO:
      ll-ans = NO.
      MESSAGE "Make Layer quantities equal?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll-ans.
      IF ll-ans THEN DO:
        eb.cas-pal:SCREEN-VALUE = STRING(INT(eb.stacks:SCREEN-VALUE) * lv-layers).
        RUN calc-tr-cnt.
      END.
    END.
  END.*/

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cas-wt V-table-Win 
PROCEDURE valid-cas-wt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(eb.cas-cnt:SCREEN-VALUE) NE 0 AND
       DEC(eb.cas-wt:SCREEN-VALUE)  NE 0 THEN DO:
      MESSAGE "You may enter EITHER " +
              TRIM(eb.cas-cnt:LABEL) + " OR " + TRIM(eb.cas-wt:LABEL) +
              ", set " + TRIM(eb.cas-cnt:LABEL) + " to zero?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll-ans AS LOG.
      IF ll-ans THEN eb.cas-cnt:SCREEN-VALUE = "0".
                ELSE eb.cas-wt:SCREEN-VALUE  = "0".
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-dest-code V-table-Win 
PROCEDURE valid-dest-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST carr-mtx
                    WHERE carr-mtx.company  EQ cocode 
                      AND carr-mtx.loc      EQ locode
                      AND carr-mtx.carrier  EQ eb.carrier:SCREEN-VALUE
                      AND carr-mtx.del-zone EQ eb.dest-code:SCREEN-VALUE)
    THEN DO:
      MESSAGE "Invalid " + TRIM(eb.dest-code:LABEL) + ", try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO eb.dest-code.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-stack-code V-table-Win 
PROCEDURE valid-stack-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT ll-foam OR eb.stack-code:SCREEN-VALUE NE "" THEN DO:
      IF NOT CAN-FIND(FIRST stackPattern
                        WHERE stackPattern.stackCode EQ eb.stack-code:SCREEN-VALUE)
      THEN DO:
        MESSAGE "Invalid Stacking Code..."  VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO eb.stack-code.
        RETURN ERROR.
      END.
      RUN stackImage (eb.stack-code:SCREEN-VALUE).
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

