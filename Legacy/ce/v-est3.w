&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: ce/v-est3.w

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

{custom/globdefs.i}
{sys/inc/var.i new shared}
assign cocode = g_company
       locode = g_loc.

def new shared buffer xest for est.
def new shared buffer xef  for ef.
def new shared buffer xeb  for eb.
def var k_frac as dec init 6.25 no-undo.
def var lv-layers AS DEC no-undo.
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR lv-pr-types AS CHAR INIT "FGLO" NO-UNDO.
DEF VAR lv-pr-list AS CHAR INIT "Flexo,Gravure,Letterpress,Offset" NO-UNDO.
DEF VAR lv-label AS CHAR EXTENT 10 NO-UNDO.
DEF VAR ll-assem-part AS LOG NO-UNDO.

{custom/framechk.i NEW}

{est/inksvarn.i NEW}

DO TRANSACTION:
  {sys/inc/fgcolors.i}
END.

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
&Scoped-define EXTERNAL-TABLES est eb
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est, eb.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS eb.spare-int-3 eb.i-col eb.i-pass eb.i-coat ~
eb.i-coat-p eb.i-coldscr eb.i-ps2[1] eb.i-code2[1] eb.i-dscr2[1] eb.i-%2[1] ~
eb.i-ps2[2] eb.i-code2[2] eb.i-dscr2[2] eb.i-%2[2] eb.i-ps2[3] ~
eb.i-code2[3] eb.i-dscr2[3] eb.i-%2[3] eb.i-ps2[4] eb.i-code2[4] ~
eb.i-dscr2[4] eb.i-%2[4] eb.i-ps2[5] eb.i-code2[5] eb.i-dscr2[5] eb.i-%2[5] ~
eb.i-ps2[6] eb.i-code2[6] eb.i-dscr2[6] eb.i-%2[6] eb.i-ps2[7] ~
eb.i-code2[7] eb.i-dscr2[7] eb.i-%2[7] eb.i-ps2[8] eb.i-code2[8] ~
eb.i-dscr2[8] eb.i-%2[8] eb.i-ps2[9] eb.i-code2[9] eb.i-dscr2[9] eb.i-%2[9] ~
eb.i-ps2[10] eb.i-code2[10] eb.i-dscr2[10] eb.i-%2[10] eb.i-ps2[11] ~
eb.i-code2[11] eb.i-dscr2[11] eb.i-%2[11] eb.i-ps2[12] eb.i-code2[12] ~
eb.i-dscr2[12] eb.i-%2[12] eb.i-ps2[13] eb.i-code2[13] eb.i-dscr2[13] ~
eb.i-%2[13] eb.i-ps2[14] eb.i-code2[14] eb.i-dscr2[14] eb.i-%2[14] ~
eb.i-ps2[15] eb.i-code2[15] eb.i-dscr2[15] eb.i-%2[15] eb.i-ps2[16] ~
eb.i-code2[16] eb.i-dscr2[16] eb.i-%2[16] eb.i-ps2[17] eb.i-code2[17] ~
eb.i-dscr2[17] eb.i-%2[17] eb.layer-pad eb.lp-len eb.lp-wid eb.lp-up ~
eb.spare-char-3 eb.divider eb.div-len eb.div-wid eb.div-up eb.spare-char-4 
&Scoped-define ENABLED-TABLES eb
&Scoped-define FIRST-ENABLED-TABLE eb
&Scoped-Define ENABLED-OBJECTS RECT-26 RECT-27 RECT-28 RECT-29 RECT-30 
&Scoped-Define DISPLAYED-FIELDS eb.spare-int-3 eb.i-col eb.i-pass eb.i-coat ~
eb.i-coat-p eb.i-coldscr eb.i-ps2[1] eb.i-code2[1] eb.i-dscr2[1] eb.i-%2[1] ~
eb.i-ps2[2] eb.i-code2[2] eb.i-dscr2[2] eb.i-%2[2] eb.i-ps2[3] ~
eb.i-code2[3] eb.i-dscr2[3] eb.i-%2[3] eb.i-ps2[4] eb.i-code2[4] ~
eb.i-dscr2[4] eb.i-%2[4] eb.i-ps2[5] eb.i-code2[5] eb.i-dscr2[5] eb.i-%2[5] ~
eb.i-ps2[6] eb.i-code2[6] eb.i-dscr2[6] eb.i-%2[6] eb.i-ps2[7] ~
eb.i-code2[7] eb.i-dscr2[7] eb.i-%2[7] eb.i-ps2[8] eb.i-code2[8] ~
eb.i-dscr2[8] eb.i-%2[8] eb.i-ps2[9] eb.i-code2[9] eb.i-dscr2[9] eb.i-%2[9] ~
eb.i-ps2[10] eb.i-code2[10] eb.i-dscr2[10] eb.i-%2[10] eb.i-ps2[11] ~
eb.i-code2[11] eb.i-dscr2[11] eb.i-%2[11] eb.i-ps2[12] eb.i-code2[12] ~
eb.i-dscr2[12] eb.i-%2[12] eb.i-ps2[13] eb.i-code2[13] eb.i-dscr2[13] ~
eb.i-%2[13] eb.i-ps2[14] eb.i-code2[14] eb.i-dscr2[14] eb.i-%2[14] ~
eb.i-ps2[15] eb.i-code2[15] eb.i-dscr2[15] eb.i-%2[15] eb.i-ps2[16] ~
eb.i-code2[16] eb.i-dscr2[16] eb.i-%2[16] eb.i-ps2[17] eb.i-code2[17] ~
eb.i-dscr2[17] eb.i-%2[17] eb.cas-no eb.cas-len eb.cas-wid eb.cas-dep ~
eb.layer-pad eb.lp-len eb.lp-wid eb.lp-up eb.spare-char-3 eb.divider ~
eb.div-len eb.div-wid eb.div-up eb.spare-char-4 eb.cas-wt eb.cas-cost ~
eb.cas-cnt eb.cas-pal eb.tr-no eb.tr-len eb.tr-wid eb.tr-dep eb.tr-cost ~
eb.tr-cnt eb.tr-cas eb.chg-method eb.weight-m eb.carrier eb.carr-dscr ~
eb.dest-code eb.fr-out-c eb.fr-out-m 
&Scoped-define DISPLAYED-TABLES eb
&Scoped-define FIRST-DISPLAYED-TABLE eb
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 fi_unit-1 fi_side-1 fi_unit-2 ~
fi_side-2 fi_unit-3 fi_side-3 fi_unit-4 fi_side-4 fi_unit-5 fi_side-5 ~
fi_unit-6 fi_side-6 fi_unit-7 fi_side-7 fi_unit-8 fi_side-8 fi_unit-9 ~
fi_side-9 fi_unit-10 fi_side-10 fi_unit-11 fi_side-11 fi_unit-12 fi_side-12 ~
fi_unit-13 fi_side-13 fi_unit-14 fi_side-14 fi_unit-15 fi_side-15 ~
fi_unit-16 fi_side-16 fi_unit-17 fi_side-17 f-lp-dep f-div-dep ~
fi_prod-notes fi_pr-type 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS fi_unit-1 fi_side-1 fi_unit-2 fi_side-2 ~
fi_unit-3 fi_side-3 fi_unit-4 fi_side-4 fi_unit-5 fi_side-5 fi_unit-6 ~
fi_side-6 fi_unit-7 fi_side-7 fi_unit-8 fi_side-8 fi_unit-9 fi_side-9 ~
fi_unit-10 fi_side-10 fi_unit-11 fi_side-11 fi_unit-12 fi_side-12 ~
fi_unit-13 fi_side-13 fi_unit-14 fi_side-14 fi_unit-15 fi_side-15 ~
fi_unit-16 fi_side-16 fi_unit-17 fi_side-17 eb.carr-dscr 

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
DEFINE VARIABLE f-div-dep AS DECIMAL FORMAT ">9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE f-lp-dep AS DECIMAL FORMAT ">9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 45.6 BY .62 NO-UNDO.

DEFINE VARIABLE fi_pr-type AS CHARACTER FORMAT "X(10)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .95 NO-UNDO.

DEFINE VARIABLE fi_prod-notes AS CHARACTER FORMAT "X(20)":U 
     LABEL "Pack Note" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fi_side-1 AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .76 NO-UNDO.

DEFINE VARIABLE fi_side-10 AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .76 NO-UNDO.

DEFINE VARIABLE fi_side-11 AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .76 NO-UNDO.

DEFINE VARIABLE fi_side-12 AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .76 NO-UNDO.

DEFINE VARIABLE fi_side-13 AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .76 NO-UNDO.

DEFINE VARIABLE fi_side-14 AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .76 NO-UNDO.

DEFINE VARIABLE fi_side-15 AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .76 NO-UNDO.

DEFINE VARIABLE fi_side-16 AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .76 NO-UNDO.

DEFINE VARIABLE fi_side-17 AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .76 NO-UNDO.

DEFINE VARIABLE fi_side-2 AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .76 NO-UNDO.

DEFINE VARIABLE fi_side-3 AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .76 NO-UNDO.

DEFINE VARIABLE fi_side-4 AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .76 NO-UNDO.

DEFINE VARIABLE fi_side-5 AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .76 NO-UNDO.

DEFINE VARIABLE fi_side-6 AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .76 NO-UNDO.

DEFINE VARIABLE fi_side-7 AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .76 NO-UNDO.

DEFINE VARIABLE fi_side-8 AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .76 NO-UNDO.

DEFINE VARIABLE fi_side-9 AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .76 NO-UNDO.

DEFINE VARIABLE fi_unit-1 AS DECIMAL FORMAT ">>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY .76 NO-UNDO.

DEFINE VARIABLE fi_unit-10 AS DECIMAL FORMAT ">>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY .76 NO-UNDO.

DEFINE VARIABLE fi_unit-11 AS DECIMAL FORMAT ">>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY .76 NO-UNDO.

DEFINE VARIABLE fi_unit-12 AS DECIMAL FORMAT ">>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY .76 NO-UNDO.

DEFINE VARIABLE fi_unit-13 AS DECIMAL FORMAT ">>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY .76 NO-UNDO.

DEFINE VARIABLE fi_unit-14 AS DECIMAL FORMAT ">>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY .76 NO-UNDO.

DEFINE VARIABLE fi_unit-15 AS DECIMAL FORMAT ">>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY .76 NO-UNDO.

DEFINE VARIABLE fi_unit-16 AS DECIMAL FORMAT ">>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY .76 NO-UNDO.

DEFINE VARIABLE fi_unit-17 AS DECIMAL FORMAT ">>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY .76 NO-UNDO.

DEFINE VARIABLE fi_unit-2 AS DECIMAL FORMAT ">>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY .76 NO-UNDO.

DEFINE VARIABLE fi_unit-3 AS DECIMAL FORMAT ">>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY .76 NO-UNDO.

DEFINE VARIABLE fi_unit-4 AS DECIMAL FORMAT ">>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY .76 NO-UNDO.

DEFINE VARIABLE fi_unit-5 AS DECIMAL FORMAT ">>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY .76 NO-UNDO.

DEFINE VARIABLE fi_unit-6 AS DECIMAL FORMAT ">>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY .76 NO-UNDO.

DEFINE VARIABLE fi_unit-7 AS DECIMAL FORMAT ">>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY .76 NO-UNDO.

DEFINE VARIABLE fi_unit-8 AS DECIMAL FORMAT ">>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY .76 NO-UNDO.

DEFINE VARIABLE fi_unit-9 AS DECIMAL FORMAT ">>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY .76 NO-UNDO.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 155 BY 16.19.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 5.24.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 4.52.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 6.43.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 1.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fold
     eb.spare-int-3 AT ROW 1.95 COL 142.8 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 44 FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1 TOOLTIP "Enter Quantity of Case Materials Per Case"
     eb.i-col AT ROW 1.19 COL 22 COLON-ALIGNED
          LABEL "Inks"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-pass AT ROW 1.19 COL 37 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-coat AT ROW 1.19 COL 51 COLON-ALIGNED
          LABEL "Coats"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-coat-p AT ROW 1.19 COL 66 COLON-ALIGNED
          LABEL "Passes"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-coldscr AT ROW 2.14 COL 15 COLON-ALIGNED NO-LABEL FORMAT "x(40)"
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     eb.i-ps2[1] AT ROW 4.1 COL 2 NO-LABEL FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .76
     FILL-IN-1 AT ROW 7.19 COL 107.4 COLON-ALIGNED NO-LABEL
     eb.i-code2[1] AT ROW 4.1 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY .76
     eb.i-dscr2[1] AT ROW 4.1 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY .76
     eb.i-%2[1] AT ROW 4.1 COL 53 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 6 BY .76
     fi_unit-1 AT ROW 4.1 COL 60 COLON-ALIGNED NO-LABEL
     fi_side-1 AT ROW 4.1 COL 67.6 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     eb.i-ps2[2] AT ROW 4.86 COL 2 NO-LABEL FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .76
     eb.i-code2[2] AT ROW 4.86 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY .76
     eb.i-dscr2[2] AT ROW 4.86 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY .76
     eb.i-%2[2] AT ROW 4.86 COL 53 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 6 BY .76
     fi_unit-2 AT ROW 4.86 COL 60 COLON-ALIGNED NO-LABEL
     fi_side-2 AT ROW 4.86 COL 67.6 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     eb.i-ps2[3] AT ROW 5.62 COL 2 NO-LABEL FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .76
     eb.i-code2[3] AT ROW 5.62 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY .76
     eb.i-dscr2[3] AT ROW 5.62 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY .76
     eb.i-%2[3] AT ROW 5.62 COL 53 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 6 BY .76
     fi_unit-3 AT ROW 5.62 COL 60 COLON-ALIGNED NO-LABEL
     fi_side-3 AT ROW 5.62 COL 67.6 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     eb.i-ps2[4] AT ROW 6.38 COL 2 NO-LABEL FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .76
     eb.i-code2[4] AT ROW 6.38 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY .76
     eb.i-dscr2[4] AT ROW 6.38 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY .76
     eb.i-%2[4] AT ROW 6.38 COL 53 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 6 BY .76
     fi_unit-4 AT ROW 6.38 COL 60 COLON-ALIGNED NO-LABEL
     fi_side-4 AT ROW 6.38 COL 67.6 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     eb.i-ps2[5] AT ROW 7.14 COL 2 NO-LABEL FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .76
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fold
     eb.i-code2[5] AT ROW 7.14 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY .76
     eb.i-dscr2[5] AT ROW 7.14 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY .76
     eb.i-%2[5] AT ROW 7.14 COL 53 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 6 BY .76
     fi_unit-5 AT ROW 7.14 COL 60 COLON-ALIGNED NO-LABEL
     fi_side-5 AT ROW 7.14 COL 67.6 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     eb.i-ps2[6] AT ROW 7.91 COL 2 NO-LABEL FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .76
     eb.i-code2[6] AT ROW 7.91 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY .76
     eb.i-dscr2[6] AT ROW 7.91 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY .76
     eb.i-%2[6] AT ROW 7.91 COL 53 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 6 BY .76
     fi_unit-6 AT ROW 7.91 COL 60 COLON-ALIGNED NO-LABEL
     fi_side-6 AT ROW 7.91 COL 67.6 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     eb.i-ps2[7] AT ROW 8.67 COL 2 NO-LABEL FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .76
     eb.i-code2[7] AT ROW 8.67 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY .76
     eb.i-dscr2[7] AT ROW 8.67 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY .76
     eb.i-%2[7] AT ROW 8.67 COL 53 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 6 BY .76
     fi_unit-7 AT ROW 8.67 COL 60 COLON-ALIGNED NO-LABEL
     fi_side-7 AT ROW 8.67 COL 67.6 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     eb.i-ps2[8] AT ROW 9.43 COL 2 NO-LABEL FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .76
     eb.i-code2[8] AT ROW 9.43 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY .76
     eb.i-dscr2[8] AT ROW 9.43 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY .76
     eb.i-%2[8] AT ROW 9.43 COL 53 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 6 BY .76
     fi_unit-8 AT ROW 9.43 COL 60 COLON-ALIGNED NO-LABEL
     fi_side-8 AT ROW 9.43 COL 67.6 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     eb.i-ps2[9] AT ROW 10.19 COL 2 NO-LABEL FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .76
     eb.i-code2[9] AT ROW 10.19 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY .76
     eb.i-dscr2[9] AT ROW 10.19 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY .76
     eb.i-%2[9] AT ROW 10.19 COL 53 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 6 BY .76
     fi_unit-9 AT ROW 10.19 COL 60 COLON-ALIGNED NO-LABEL
     fi_side-9 AT ROW 10.19 COL 67.6 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     eb.i-ps2[10] AT ROW 10.95 COL 2 NO-LABEL FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .76
     eb.i-code2[10] AT ROW 10.95 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY .76
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fold
     eb.i-dscr2[10] AT ROW 10.95 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY .76
     eb.i-%2[10] AT ROW 10.95 COL 53 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 6 BY .76
     fi_unit-10 AT ROW 10.95 COL 60 COLON-ALIGNED NO-LABEL
     fi_side-10 AT ROW 10.95 COL 67.6 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     eb.i-ps2[11] AT ROW 11.71 COL 2 NO-LABEL FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .76
     eb.i-code2[11] AT ROW 11.71 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY .76
     eb.i-dscr2[11] AT ROW 11.71 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY .76
     eb.i-%2[11] AT ROW 11.71 COL 53 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 6 BY .76
     fi_unit-11 AT ROW 11.71 COL 60 COLON-ALIGNED NO-LABEL
     fi_side-11 AT ROW 11.71 COL 67.6 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     eb.i-ps2[12] AT ROW 12.48 COL 2 NO-LABEL FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .76
     eb.i-code2[12] AT ROW 12.48 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY .76
     eb.i-dscr2[12] AT ROW 12.48 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY .76
     eb.i-%2[12] AT ROW 12.48 COL 53 COLON-ALIGNED NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 6 BY .76
     fi_unit-12 AT ROW 12.48 COL 60 COLON-ALIGNED NO-LABEL
     fi_side-12 AT ROW 12.48 COL 67.6 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     eb.i-ps2[13] AT ROW 13.19 COL 2 NO-LABEL FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .76
     eb.i-code2[13] AT ROW 13.19 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY .76
     eb.i-dscr2[13] AT ROW 13.19 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY .76
     eb.i-%2[13] AT ROW 13.19 COL 55 NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 6 BY .76
     fi_unit-13 AT ROW 13.19 COL 60 COLON-ALIGNED NO-LABEL
     fi_side-13 AT ROW 13.19 COL 67.6 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     eb.i-ps2[14] AT ROW 13.95 COL 2 NO-LABEL FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .76
     eb.i-code2[14] AT ROW 13.95 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY .76
     eb.i-dscr2[14] AT ROW 13.95 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY .76
     eb.i-%2[14] AT ROW 13.95 COL 55 NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 6 BY .76
     fi_unit-14 AT ROW 13.95 COL 60 COLON-ALIGNED NO-LABEL
     fi_side-14 AT ROW 13.95 COL 67.6 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     eb.i-ps2[15] AT ROW 14.71 COL 2 NO-LABEL FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .76
     eb.i-code2[15] AT ROW 14.71 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY .76
     eb.i-dscr2[15] AT ROW 14.71 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY .76
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fold
     eb.i-%2[15] AT ROW 14.67 COL 55 NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 6 BY .76
     fi_unit-15 AT ROW 14.67 COL 60 COLON-ALIGNED NO-LABEL
     fi_side-15 AT ROW 14.67 COL 67.6 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     eb.i-ps2[16] AT ROW 15.48 COL 2 NO-LABEL FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .76
     eb.i-code2[16] AT ROW 15.48 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY .76
     eb.i-dscr2[16] AT ROW 15.48 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY .76
     eb.i-%2[16] AT ROW 15.48 COL 55 NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 6 BY .76
     fi_unit-16 AT ROW 15.48 COL 60 COLON-ALIGNED NO-LABEL
     fi_side-16 AT ROW 15.48 COL 67.6 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     eb.i-ps2[17] AT ROW 16.19 COL 2 NO-LABEL FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY .76
     eb.i-code2[17] AT ROW 16.24 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY .76
     eb.i-dscr2[17] AT ROW 16.24 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY .76
     eb.i-%2[17] AT ROW 16.19 COL 55 NO-LABEL FORMAT ">>>"
          VIEW-AS FILL-IN 
          SIZE 6 BY .76
     fi_unit-17 AT ROW 16.24 COL 60 COLON-ALIGNED NO-LABEL
     fi_side-17 AT ROW 16.24 COL 67.6 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     eb.cas-no AT ROW 1.95 COL 87 COLON-ALIGNED
          LABEL "Pack Code"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.cas-len AT ROW 1.95 COL 105 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     eb.cas-wid AT ROW 1.95 COL 117 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     eb.cas-dep AT ROW 1.95 COL 129 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY .95
     eb.layer-pad AT ROW 2.91 COL 87 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.lp-len AT ROW 2.91 COL 105 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     eb.lp-wid AT ROW 2.91 COL 117.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.2 BY 1
     f-lp-dep AT ROW 2.91 COL 129 COLON-ALIGNED NO-LABEL
     eb.lp-up AT ROW 2.91 COL 142.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     eb.spare-char-3 AT ROW 2.91 COL 149 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 40 FORMAT "x(1)"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     eb.divider AT ROW 4 COL 87 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.div-len AT ROW 4 COL 105.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.8 BY 1
     eb.div-wid AT ROW 4.05 COL 117.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     f-div-dep AT ROW 4 COL 129 COLON-ALIGNED NO-LABEL
     eb.div-up AT ROW 4 COL 142.8 COLON-ALIGNED NO-LABEL FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fold
     eb.spare-char-4 AT ROW 4 COL 149 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 42 FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     eb.cas-wt AT ROW 6.19 COL 129 COLON-ALIGNED
          LABEL "Weight/Unit"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     eb.cas-cost AT ROW 5.14 COL 87 COLON-ALIGNED
          LABEL "Cost/Ea"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.cas-cnt AT ROW 5.1 COL 125 COLON-ALIGNED FORMAT ">>>,>>>"
          LABEL "Boxes/Code"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.cas-pal AT ROW 6.14 COL 87 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.tr-no AT ROW 7.91 COL 88 COLON-ALIGNED
          LABEL "Pallet #"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.tr-len AT ROW 8.86 COL 132.8 COLON-ALIGNED
          LABEL "Length"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     eb.tr-wid AT ROW 9.81 COL 132.8 COLON-ALIGNED
          LABEL "Width"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     eb.tr-dep AT ROW 10.76 COL 132.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     eb.tr-cost AT ROW 8.86 COL 88 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.tr-cnt AT ROW 9.81 COL 88 COLON-ALIGNED FORMAT ">,>>>,>>>"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.tr-cas AT ROW 10.76 COL 88 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     fi_prod-notes AT ROW 7.52 COL 122.2 COLON-ALIGNED
     eb.chg-method AT ROW 12.19 COL 94 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Prepaid", "P":U,
"Collect", "C":U,
"Bill", "B":U,
"Third Party", "T":U
          SIZE 52 BY .95
     eb.weight-m AT ROW 13.14 COL 97 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     eb.carrier AT ROW 14.1 COL 97 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     eb.carr-dscr AT ROW 14.1 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 29 BY 1
     eb.dest-code AT ROW 15.05 COL 97 COLON-ALIGNED HELP
          "Enter User Defined Delivery Code for this Ship-To Location"
          LABEL "Delivery Zone" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     eb.fr-out-c AT ROW 16 COL 97 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     eb.fr-out-m AT ROW 16 COL 129 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     fi_pr-type AT ROW 2.14 COL 1.4 NO-LABEL
     "Unit#" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 3.38 COL 62
     "S" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 3.38 COL 70.4 WIDGET-ID 2
     "PS" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 3.38 COL 2
     "%" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 3.38 COL 56
     "Description" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 3.38 COL 28
     "Freight Charge" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.19 COL 76
          FGCOLOR 9 
     "Code" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 3.38 COL 9
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fold
     "Qty" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.24 COL 144.8
     "Per" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 1.24 COL 150.4 WIDGET-ID 38
     "Press Type" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.33 COL 2.4
     "Length" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.24 COL 108.4
     "Width" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.24 COL 120.4
     "Depth" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.24 COL 131.8
     RECT-26 AT ROW 1 COL 1
     RECT-27 AT ROW 11.95 COL 74
     RECT-28 AT ROW 7.38 COL 74
     RECT-29 AT ROW 1 COL 74
     RECT-30 AT ROW 7.43 COL 109
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
         HEIGHT             = 22.52
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
/* SETTINGS FOR FRAME fold
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME fold:SCROLLABLE       = FALSE
       FRAME fold:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN eb.carr-dscr IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.carrier IN FRAME fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.cas-cnt IN FRAME fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN eb.cas-cost IN FRAME fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN eb.cas-dep IN FRAME fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN eb.cas-len IN FRAME fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN eb.cas-no IN FRAME fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN eb.cas-pal IN FRAME fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.cas-wid IN FRAME fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN eb.cas-wt IN FRAME fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR RADIO-SET eb.chg-method IN FRAME fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.dest-code IN FRAME fold
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN eb.div-up IN FRAME fold
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN f-div-dep IN FRAME fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN f-lp-dep IN FRAME fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_pr-type IN FRAME fold
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi_prod-notes IN FRAME fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_side-1 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_side-10 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_side-11 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_side-12 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_side-13 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_side-14 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_side-15 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_side-16 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_side-17 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_side-2 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_side-3 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_side-4 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_side-5 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_side-6 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_side-7 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_side-8 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_side-9 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_unit-1 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_unit-10 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_unit-11 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_unit-12 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_unit-13 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_unit-14 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_unit-15 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_unit-16 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_unit-17 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_unit-2 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_unit-3 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_unit-4 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_unit-5 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_unit-6 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_unit-7 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_unit-8 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_unit-9 IN FRAME fold
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN eb.fr-out-c IN FRAME fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.fr-out-m IN FRAME fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.i-%2[10] IN FRAME fold
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN eb.i-%2[11] IN FRAME fold
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN eb.i-%2[12] IN FRAME fold
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN eb.i-%2[13] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-%2[14] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-%2[15] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-%2[16] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-%2[17] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-%2[1] IN FRAME fold
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN eb.i-%2[2] IN FRAME fold
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN eb.i-%2[3] IN FRAME fold
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN eb.i-%2[4] IN FRAME fold
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN eb.i-%2[5] IN FRAME fold
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN eb.i-%2[6] IN FRAME fold
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN eb.i-%2[7] IN FRAME fold
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN eb.i-%2[8] IN FRAME fold
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN eb.i-%2[9] IN FRAME fold
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN eb.i-coat IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.i-coat-p IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.i-col IN FRAME fold
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.i-coldscr IN FRAME fold
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN eb.i-ps2[10] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-ps2[11] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-ps2[12] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-ps2[13] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-ps2[14] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-ps2[15] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-ps2[16] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-ps2[17] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-ps2[1] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-ps2[2] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-ps2[3] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-ps2[4] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-ps2[5] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-ps2[6] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-ps2[7] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-ps2[8] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.i-ps2[9] IN FRAME fold
   ALIGN-L EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN eb.spare-char-3 IN FRAME fold
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN eb.spare-char-4 IN FRAME fold
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN eb.spare-int-3 IN FRAME fold
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN eb.tr-cas IN FRAME fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.tr-cnt IN FRAME fold
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN eb.tr-cost IN FRAME fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.tr-dep IN FRAME fold
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN eb.tr-len IN FRAME fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN eb.tr-no IN FRAME fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN eb.tr-wid IN FRAME fold
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN eb.weight-m IN FRAME fold
   NO-ENABLE                                                            */
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
    def var lv-ind like style.industry no-undo.
    def var lv-handle as handle no-undo.
    def var char-val as cha no-undo.    
    lv-handle = focus:handle.

    DEF VAR v-cnt    AS INT NO-UNDO.
    DEF VAR v-cnt2   AS INT NO-UNDO.
    DEF VAR v-cnt3   AS INT NO-UNDO.
    DEF VAR v-loopct AS INT NO-UNDO.
    DEF VAR v-valhld AS CHAR NO-UNDO.


    v-loopct = INT(eb.i-col:SCREEN-VALUE) + 
               INT(eb.i-coat:SCREEN-VALUE).


    case focus:name :
         when "i-code2" then do:
             find style where style.company = eb.company and
                            style.style = eb.style
                            no-lock no-error.   
             if avail style then lv-ind = style.industry.
             else lv-ind = "".  

            /* gdm - 09150815 - NEW LOOK UP - MULTIPLE SELECTION SUPPORTED */
             run windows/l-item4.w (eb.company, lv-ind, "I",focus:screen-value, output char-val).             
             IF char-val <> "" THEN DO:

                 ASSIGN v-valhld = char-val.

                 DO v-cnt = FOCUS:INDEX  TO v-loopct:                     

                   CASE v-cnt:
                    WHEN 1 THEN 
                      ASSIGN 
                        eb.i-code2[1]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                        eb.i-dscr2[1]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                    WHEN 2 THEN 
                      ASSIGN 
                        eb.i-code2[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                        eb.i-dscr2[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                    WHEN 3 THEN 
                      ASSIGN 
                        eb.i-code2[3]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                        eb.i-dscr2[3]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                     WHEN 4 THEN
                      ASSIGN 
                        eb.i-code2[4]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                        eb.i-dscr2[4]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                     WHEN 5 THEN 
                      ASSIGN 
                        eb.i-code2[5]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                        eb.i-dscr2[5]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                     WHEN 6 THEN 
                      ASSIGN 
                        eb.i-code2[6]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                        eb.i-dscr2[6]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                     WHEN 7 THEN
                      ASSIGN 
                        eb.i-code2[7]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                        eb.i-dscr2[7]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                     WHEN 8 THEN 
                      ASSIGN 
                        eb.i-code2[8]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                        eb.i-dscr2[8]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                     WHEN 9 THEN 
                      ASSIGN 
                        eb.i-code2[9]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                        eb.i-dscr2[9]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                     WHEN 10 THEN 
                      ASSIGN 
                        eb.i-code2[10]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                        eb.i-dscr2[10]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                     WHEN 11 THEN 
                      ASSIGN 
                        eb.i-code2[11]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                        eb.i-dscr2[11]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                     WHEN 12 THEN 
                      ASSIGN 
                        eb.i-code2[12]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                        eb.i-dscr2[12]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                     WHEN 13 THEN 
                      ASSIGN 
                        eb.i-code2[13]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                        eb.i-dscr2[13]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                     WHEN 14 THEN 
                      ASSIGN 
                        eb.i-code2[14]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                        eb.i-dscr2[14]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                     WHEN 15 THEN 
                      ASSIGN 
                        eb.i-code2[15]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                        eb.i-dscr2[15]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                     WHEN 16 THEN 
                      ASSIGN 
                        eb.i-code2[16]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                        eb.i-dscr2[16]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                     WHEN 17 THEN 
                      ASSIGN 
                        eb.i-code2[17]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,v-valhld)
                        eb.i-dscr2[17]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,v-valhld).
                   END CASE .

                   ASSIGN v-valhld = SUBSTR(v-valhld,(LENGTH(ENTRY(1,v-valhld)) + LENGTH(ENTRY(2,v-valhld)) + 3)).

                   IF TRIM(v-valhld) EQ ""  THEN LEAVE.

                 END.

                 RUN getUnit# (FOCUS:INDEX).                                  
             END.
             return no-apply.
         end.
         when "i-dscr2" then do:
             find style where style.company = eb.company and
                            style.style = eb.style
                            no-lock no-error.   
             if avail style then lv-ind = style.industry.
             else lv-ind = "".  
             run windows/l-itmdsc.w (eb.company, lv-ind, "I",focus:screen-value, output char-val).
             if char-val <> "" then do:
                  assign focus:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(1,char-val).
                  case focus:index:        
                       when 1 then eb.i-code2[1]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 2 then eb.i-code2[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 3 then eb.i-code2[3]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 4 then eb.i-code2[4]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 5 then eb.i-code2[5]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 6 then eb.i-code2[6]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 7 then eb.i-code2[7]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 8 then eb.i-code2[8]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 9 then eb.i-code2[9]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 10 then eb.i-code2[10]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 11 then eb.i-code2[11]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 12 then eb.i-code2[12]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 13 then eb.i-code2[13]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 14 then eb.i-code2[14]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 15 then eb.i-code2[15]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 16 then eb.i-code2[16]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 17 then eb.i-code2[17]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       /*when 18 then eb.i-code2[18]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 19 then eb.i-code2[19]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).
                       when 20 then eb.i-code2[20]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(2,char-val).*/
                  end case.
             end.         
             return no-apply.
         end.
         when "cas-no" then do:
           find style where style.company = eb.company and
                            style.style = eb.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           run windows/l-item.w (eb.company,"","C",focus:screen-value, output char-val).
           if char-val <> "" AND eb.cas-no:SCREEN-VALUE NE entry(1,char-val) then do:
              eb.cas-no:SCREEN-VALUE = entry(1,char-val).
              APPLY "value-changed" TO eb.cas-no.
           end.   
           return no-apply.   
         end.   
         when "tr-no" then do:
           find style where style.company = eb.company and
                            style.style = eb.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           run windows/l-item.w (eb.company,"","D",focus:screen-value, output char-val).
           if char-val <> "" then do:
              find item where item.company = eb.company and
                              item.i-no = entry(1,char-val)
                              no-lock no-error.
              assign focus:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(1,char-val).
              if avail item then assign /*eb.cas-cost:Screen-value = */
                                        eb.tr-len:Screen-value = string(item.case-l)
                                        eb.tr-wid:Screen-value = string(item.case-w)
                                        eb.tr-dep:Screen-value = string(item.case-d)
                                        .
           end.
           return no-apply.   
        end.   
        when "carrier" then do:
             run windows/l-carrie.w  
                 (eb.company,eb.loc,focus:screen-value, output char-val).
             if char-val <> "" AND entry(1,char-val) NE focus:SCREEN-VALUE THEN DO:
                eb.carrier:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(1,char-val).
                RUN new-carrier.
             END.
             return no-apply.
        end.
        when "dest-code" then do:
           run windows/l-delzon.w 
              (eb.company,eb.loc,eb.carrier:SCREEN-VALUE IN FRAME {&FRAME-NAME},focus:SCREEN-VALUE IN FRAME {&FRAME-NAME}, output char-val).
           if char-val <> "" then 
              assign focus:SCREEN-VALUE IN FRAME {&FRAME-NAME} = entry(1,char-val).
           return no-apply.  
        end.
        when "layer-pad" then do:
           find style where style.company = eb.company and
                            style.style = eb.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           run windows/l-item.w (eb.company,"","5",focus:screen-value, output char-val).
           if char-val <> "" AND eb.cas-no:SCREEN-VALUE NE entry(1,char-val) then do:
              eb.layer-pad:SCREEN-VALUE = entry(1,char-val).
              APPLY "value-changed" TO eb.layer-pad.
           end.   
           return no-apply.   
         end.   
         when "divider" then do:
           find style where style.company = eb.company and
                            style.style = eb.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           run windows/l-item.w (eb.company,"","6",focus:screen-value, output char-val).
           if char-val <> "" AND eb.cas-no:SCREEN-VALUE NE entry(1,char-val) then do:
              eb.divider:SCREEN-VALUE = entry(1,char-val).
              APPLY "value-changed" TO eb.divider.
           end.   
           return no-apply.   
         end.   
    end case.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.carrier V-table-Win
ON LEAVE OF eb.carrier IN FRAME fold /* Carrier */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-carrier NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.carrier V-table-Win
ON VALUE-CHANGED OF eb.carrier IN FRAME fold /* Carrier */
DO:
  RUN new-carrier.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cas-cnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cas-cnt V-table-Win
ON VALUE-CHANGED OF eb.cas-cnt IN FRAME fold /* Boxes/Code */
DO:
  RUN calc-tr-cnt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cas-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cas-no V-table-Win
ON LEAVE OF eb.cas-no IN FRAME fold /* Pack Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cas-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cas-no V-table-Win
ON VALUE-CHANGED OF eb.cas-no IN FRAME fold /* Pack Code */
DO:
  IF eb.cas-no NE eb.cas-no:SCREEN-VALUE THEN
  DO:
     find item where item.company = eb.company and
          item.i-no = eb.cas-no:screen-value and
          item.mat-type = "C"
          no-lock no-error.
     if avail item then do:
          assign eb.cas-len:Screen-value = string(item.case-l)
                 eb.cas-wid:Screen-value = string(item.case-w)
                 eb.cas-dep:Screen-value = string(item.case-d)
                 eb.cas-pal:Screen-value = string(item.case-pall)
                 eb.cas-cnt:Screen-value = string(item.box-case)
                 eb.cas-wt:Screen-value = string(item.avg-w).
          RUN new-cas-pal.
     end.
  END.

  RUN enable-case-dims.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cas-pal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cas-pal V-table-Win
ON VALUE-CHANGED OF eb.cas-pal IN FRAME fold /* Cases/Pall */
DO:
  RUN new-cas-pal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cas-wt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cas-wt V-table-Win
ON LEAVE OF eb.cas-wt IN FRAME fold /* Weight/Unit */
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
ON return OF eb.chg-method IN FRAME fold /* chg-method */
DO:
  apply "tab" to self.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.dest-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.dest-code V-table-Win
ON LEAVE OF eb.dest-code IN FRAME fold /* Delivery Zone */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dest-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.divider
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.divider V-table-Win
ON LEAVE OF eb.divider IN FRAME fold /* Divider */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-divider NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.divider V-table-Win
ON VALUE-CHANGED OF eb.divider IN FRAME fold /* Divider */
DO:
  IF eb.divider NE eb.divider:SCREEN-VALUE THEN
  DO:
     find item where item.company = eb.company and
          item.i-no = eb.divider:screen-value and
          item.mat-type = "6"
          no-lock no-error.

     if avail item THEN
          assign eb.div-len:Screen-value = string(item.case-l)
                 eb.div-wid:Screen-value = string(item.case-w) 
                 f-div-dep:SCREEN-VALUE  = string(ITEM.case-d) 
                 /* do not copy over to divider value (AH task 07011009) */
                 eb.div-up:SCREEN-VALUE  = STRING(ITEM.box-case).
  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_side-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_side-2 V-table-Win
ON LEAVE OF fi_side-2 IN FRAME fold
DO:
   IF LASTKEY NE -1 THEN DO:
    {&methods/lValidateError.i YES}

      ASSIGN fi_side-2.

      IF fi_side-2 NE "" AND LOOKUP(fi_side-2,"F,B") EQ 0 THEN
      DO:
         MESSAGE "Invalid Value.  Valid Values are F or B."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         RETURN NO-APPLY.
      END.
  {&methods/lValidateError.i NO}
   END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_side-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_side-3 V-table-Win
ON LEAVE OF fi_side-3 IN FRAME fold
DO:
   IF LASTKEY NE -1 THEN DO:
    {&methods/lValidateError.i YES}
      ASSIGN fi_side-3.

      IF fi_side-3 NE "" AND LOOKUP(fi_side-3,"F,B") EQ 0 THEN
      DO:
         MESSAGE "Invalid Value.  Valid Values are F or B."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         RETURN NO-APPLY.
      END.
  {&methods/lValidateError.i NO}
   END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_side-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_side-4 V-table-Win
ON LEAVE OF fi_side-4 IN FRAME fold
DO:
   IF LASTKEY NE -1 THEN DO:
   {&methods/lValidateError.i YES}
      ASSIGN fi_side-4.

      IF fi_side-4 NE "" AND LOOKUP(fi_side-4,"F,B") EQ 0 THEN
      DO:
         MESSAGE "Invalid Value.  Valid Values are F or B."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         RETURN NO-APPLY.
      END.
   {&methods/lValidateError.i NO}
   END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_side-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_side-5 V-table-Win
ON LEAVE OF fi_side-5 IN FRAME fold
DO:
   IF LASTKEY NE -1 THEN DO:
   {&methods/lValidateError.i YES}
      ASSIGN fi_side-5.

      IF fi_side-5 NE "" AND LOOKUP(fi_side-5,"F,B") EQ 0 THEN
      DO:
         MESSAGE "Invalid Value.  Valid Values are F or B."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         RETURN NO-APPLY.
      END.
   {&methods/lValidateError.i NO}
   END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_side-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_side-6 V-table-Win
ON LEAVE OF fi_side-6 IN FRAME fold
DO:
  IF LASTKEY NE -1 THEN DO:
   {&methods/lValidateError.i YES}
     ASSIGN fi_side-6.

     IF fi_side-6 NE "" AND LOOKUP(fi_side-6,"F,B") EQ 0 THEN
     DO:
        MESSAGE "Invalid Value.  Valid Values are F or B."
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN NO-APPLY.
     END.
  {&methods/lValidateError.i NO}
   END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_side-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_side-7 V-table-Win
ON LEAVE OF fi_side-7 IN FRAME fold
DO:
   IF LASTKEY NE -1 THEN DO:
   {&methods/lValidateError.i YES}
      ASSIGN fi_side-7.

      IF fi_side-7 NE "" AND LOOKUP(fi_side-7,"F,B") EQ 0 THEN
      DO:
         MESSAGE "Invalid Value.  Valid Values are F or B."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         RETURN NO-APPLY.
      END.
  {&methods/lValidateError.i NO}
   END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_side-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_side-8 V-table-Win
ON LEAVE OF fi_side-8 IN FRAME fold
DO:
   IF LASTKEY NE -1 THEN DO:
    {&methods/lValidateError.i YES}
      ASSIGN fi_side-8.

      IF fi_side-8 NE "" AND LOOKUP(fi_side-8,"F,B") EQ 0 THEN
      DO:
         MESSAGE "Invalid Value.  Valid Values are F or B."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         RETURN NO-APPLY.
      END.
   {&methods/lValidateError.i NO}
   END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_side-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_side-9 V-table-Win
ON LEAVE OF fi_side-9 IN FRAME fold
DO:
   IF LASTKEY NE -1 THEN DO:
    {&methods/lValidateError.i YES}
      ASSIGN fi_side-9.

      IF fi_side-9 NE "" AND LOOKUP(fi_side-9,"F,B") EQ 0 THEN
      DO:
         MESSAGE "Invalid Value.  Valid Values are F or B."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         RETURN NO-APPLY.
      END.
   {&methods/lValidateError.i NO}
   END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-coat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-coat V-table-Win
ON VALUE-CHANGED OF eb.i-coat IN FRAME fold /* Coats */
DO:
  IF DEC({&self-name}:SCREEN-VALUE) EQ 0 THEN
    eb.i-coat-p:SCREEN-VALUE = "".
  ELSE
  IF DEC(eb.i-pass:SCREEN-VALUE) EQ 0 THEN
    eb.i-coat-p:SCREEN-VALUE = "1".

  {ce/def-inks.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-coat-p
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-coat-p V-table-Win
ON VALUE-CHANGED OF eb.i-coat-p IN FRAME fold /* Passes */
DO:
  {ce/def-inks.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code2[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[10] V-table-Win
ON LEAVE OF eb.i-code2[10] IN FRAME fold /* Code[10] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {ce/val-inks.i 10 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[10] V-table-Win
ON VALUE-CHANGED OF eb.i-code2[10] IN FRAME fold /* Code[10] */
DO:
  {est/new-inks.i 10 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code2[11]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[11] V-table-Win
ON LEAVE OF eb.i-code2[11] IN FRAME fold /* Code[11] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {ce/val-inks.i 11 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[11] V-table-Win
ON VALUE-CHANGED OF eb.i-code2[11] IN FRAME fold /* Code[11] */
DO:
  {est/new-inks.i 11 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code2[12]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[12] V-table-Win
ON LEAVE OF eb.i-code2[12] IN FRAME fold /* Code[12] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {ce/val-inks.i 12 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[12] V-table-Win
ON VALUE-CHANGED OF eb.i-code2[12] IN FRAME fold /* Code[12] */
DO:
  {est/new-inks.i 12 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code2[13]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[13] V-table-Win
ON LEAVE OF eb.i-code2[13] IN FRAME fold /* Code[13] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {ce/val-inks.i 13 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[13] V-table-Win
ON VALUE-CHANGED OF eb.i-code2[13] IN FRAME fold /* Code[13] */
DO:
  {est/new-inks.i 13 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code2[14]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[14] V-table-Win
ON LEAVE OF eb.i-code2[14] IN FRAME fold /* Code[14] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {ce/val-inks.i 13 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[14] V-table-Win
ON VALUE-CHANGED OF eb.i-code2[14] IN FRAME fold /* Code[14] */
DO:
  {est/new-inks.i 14 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code2[15]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[15] V-table-Win
ON LEAVE OF eb.i-code2[15] IN FRAME fold /* Code[15] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {ce/val-inks.i 15 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[15] V-table-Win
ON VALUE-CHANGED OF eb.i-code2[15] IN FRAME fold /* Code[15] */
DO:
  {est/new-inks.i 15 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code2[16]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[16] V-table-Win
ON LEAVE OF eb.i-code2[16] IN FRAME fold /* Code[16] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {ce/val-inks.i 16 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[16] V-table-Win
ON VALUE-CHANGED OF eb.i-code2[16] IN FRAME fold /* Code[16] */
DO:
  {est/new-inks.i 16 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code2[17]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[17] V-table-Win
ON LEAVE OF eb.i-code2[17] IN FRAME fold /* Code[17] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {ce/val-inks.i 17 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[17] V-table-Win
ON VALUE-CHANGED OF eb.i-code2[17] IN FRAME fold /* Code[17] */
DO:
  {est/new-inks.i 17 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code2[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[1] V-table-Win
ON LEAVE OF eb.i-code2[1] IN FRAME fold /* Code[1] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {ce/val-inks.i 1 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[1] V-table-Win
ON VALUE-CHANGED OF eb.i-code2[1] IN FRAME fold /* Code[1] */
DO:
  {est/new-inks.i 1 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code2[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[2] V-table-Win
ON LEAVE OF eb.i-code2[2] IN FRAME fold /* Code[2] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {ce/val-inks.i 2 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[2] V-table-Win
ON VALUE-CHANGED OF eb.i-code2[2] IN FRAME fold /* Code[2] */
DO:
  {est/new-inks.i 2 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code2[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[3] V-table-Win
ON LEAVE OF eb.i-code2[3] IN FRAME fold /* Code[3] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {ce/val-inks.i 3 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[3] V-table-Win
ON VALUE-CHANGED OF eb.i-code2[3] IN FRAME fold /* Code[3] */
DO:
  {est/new-inks.i 3 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code2[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[4] V-table-Win
ON LEAVE OF eb.i-code2[4] IN FRAME fold /* Code[4] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {ce/val-inks.i 4 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[4] V-table-Win
ON VALUE-CHANGED OF eb.i-code2[4] IN FRAME fold /* Code[4] */
DO:
  {est/new-inks.i 4 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code2[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[5] V-table-Win
ON LEAVE OF eb.i-code2[5] IN FRAME fold /* Code[5] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {ce/val-inks.i 5 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[5] V-table-Win
ON VALUE-CHANGED OF eb.i-code2[5] IN FRAME fold /* Code[5] */
DO:
  {est/new-inks.i 5 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code2[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[6] V-table-Win
ON LEAVE OF eb.i-code2[6] IN FRAME fold /* Code[6] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {ce/val-inks.i 6 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[6] V-table-Win
ON VALUE-CHANGED OF eb.i-code2[6] IN FRAME fold /* Code[6] */
DO:
  {est/new-inks.i 6 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code2[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[7] V-table-Win
ON LEAVE OF eb.i-code2[7] IN FRAME fold /* Code[7] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {ce/val-inks.i 7 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[7] V-table-Win
ON VALUE-CHANGED OF eb.i-code2[7] IN FRAME fold /* Code[7] */
DO:
  {est/new-inks.i 7 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code2[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[8] V-table-Win
ON LEAVE OF eb.i-code2[8] IN FRAME fold /* Code[8] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {ce/val-inks.i 8 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[8] V-table-Win
ON VALUE-CHANGED OF eb.i-code2[8] IN FRAME fold /* Code[8] */
DO:
  {est/new-inks.i 8 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code2[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[9] V-table-Win
ON LEAVE OF eb.i-code2[9] IN FRAME fold /* Code[9] */
DO:
  IF LASTKEY NE -1 THEN DO:
    {ce/val-inks.i 9 2}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code2[9] V-table-Win
ON VALUE-CHANGED OF eb.i-code2[9] IN FRAME fold /* Code[9] */
DO:
  {est/new-inks.i 9 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-col
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-col V-table-Win
ON VALUE-CHANGED OF eb.i-col IN FRAME fold /* Inks */
DO:
  IF DEC({&self-name}:SCREEN-VALUE) EQ 0 THEN
    eb.i-pass:SCREEN-VALUE = "".
  ELSE
  IF DEC(eb.i-pass:SCREEN-VALUE) EQ 0 THEN
    eb.i-pass:SCREEN-VALUE = "1".

  {ce/def-inks.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-pass
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-pass V-table-Win
ON VALUE-CHANGED OF eb.i-pass IN FRAME fold /* Passes */
DO:
  {ce/def-inks.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.layer-pad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.layer-pad V-table-Win
ON LEAVE OF eb.layer-pad IN FRAME fold /* Layer Pad */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-layer-pad NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.layer-pad V-table-Win
ON VALUE-CHANGED OF eb.layer-pad IN FRAME fold /* Layer Pad */
DO:
  IF eb.layer-pad NE eb.layer-pad:SCREEN-VALUE THEN
  DO:
     find item where item.company = eb.company and
          item.i-no = eb.layer-pad:screen-value and
          item.mat-type = "5"
          no-lock no-error.

     if avail item THEN
        assign eb.lp-len:Screen-value = string(item.case-l)
               eb.lp-wid:Screen-value = string(item.case-w) 
               f-lp-dep:SCREEN-VALUE  = STRING(ITEM.case-d) 
               eb.lp-up:SCREEN-VALUE  = STRING(ITEM.box-case).
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.spare-char-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.spare-char-3 V-table-Win
ON LEAVE OF eb.spare-char-3 IN FRAME fold /* spare-char-3 */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-per NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.spare-char-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.spare-char-4 V-table-Win
ON LEAVE OF eb.spare-char-4 IN FRAME fold /* spare-char-4 */
DO:
   IF LASTKEY NE -1 THEN DO:
    RUN valid-per NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.tr-cas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.tr-cas V-table-Win
ON VALUE-CHANGED OF eb.tr-cas IN FRAME fold /* # of Layers */
DO:
  /*RUN new-tr-cas.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.tr-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.tr-no V-table-Win
ON LEAVE OF eb.tr-no IN FRAME fold /* Pallet # */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-inks V-table-Win 
PROCEDURE assign-inks :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/      
  DEF BUFFER bf-eb FOR eb.


  DISABLE TRIGGERS FOR LOAD OF bf-eb.

  DO WITH FRAME {&FRAME-NAME}:
   find bf-eb where recid(bf-eb) = recid(eb).
   assign bf-eb.i-ps2[1] = int(eb.i-ps2[1]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[2] = int(eb.i-ps2[2]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[3] = int(eb.i-ps2[3]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[4] = int(eb.i-ps2[4]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[5] = int(eb.i-ps2[5]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[6] = int(eb.i-ps2[6]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[7] = int(eb.i-ps2[7]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[8] = int(eb.i-ps2[8]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[9] = int(eb.i-ps2[9]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[10] = int(eb.i-ps2[10]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[11] = int(eb.i-ps2[11]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[12] = int(eb.i-ps2[12]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[13] = int(eb.i-ps2[13]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[14] = int(eb.i-ps2[14]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[15] = int(eb.i-ps2[15]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[16] = int(eb.i-ps2[16]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[17] = int(eb.i-ps2[17]:screen-value in frame {&frame-name})
        /*  bf-eb.i-ps2[18] = int(eb.i-ps2[18]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[19] = int(eb.i-ps2[19]:screen-value in frame {&frame-name})
          bf-eb.i-ps2[20] = int(eb.i-ps2[20]:screen-value in frame {&frame-name})*/
          bf-eb.i-code2[1] = eb.i-code2[1]:screen-value 
          bf-eb.i-code2[2] = eb.i-code2[2]:screen-value 
          bf-eb.i-code2[3] = eb.i-code2[3]:screen-value 
          bf-eb.i-code2[4] = eb.i-code2[4]:screen-value 
          bf-eb.i-code2[5] = eb.i-code2[5]:screen-value 
          bf-eb.i-code2[6] = eb.i-code2[6]:screen-value 
          bf-eb.i-code2[7] = eb.i-code2[7]:screen-value 
          bf-eb.i-code2[8] = eb.i-code2[8]:screen-value 
          bf-eb.i-code2[9] = eb.i-code2[9]:screen-value 
          bf-eb.i-code2[10] = eb.i-code2[10]:screen-value
          bf-eb.i-code2[11] = eb.i-code2[11]:screen-value
          bf-eb.i-code2[12] = eb.i-code2[12]:screen-value 
          bf-eb.i-code2[13] = eb.i-code2[13]:screen-value
          bf-eb.i-code2[14] = eb.i-code2[14]:screen-value
          bf-eb.i-code2[15] = eb.i-code2[15]:screen-value 
          bf-eb.i-code2[16] = eb.i-code2[16]:screen-value
          bf-eb.i-code2[17] = eb.i-code2[17]:screen-value
   /*       bf-eb.i-code2[18] = eb.i-code2[18]:screen-value
          bf-eb.i-code2[19] = eb.i-code2[19]:screen-value
          bf-eb.i-code2[20] = eb.i-code2[20]:screen-value */
          bf-eb.i-dscr2[1] = eb.i-dscr2[1]:screen-value 
          bf-eb.i-dscr2[2] = eb.i-dscr2[2]:screen-value 
          bf-eb.i-dscr2[3] = eb.i-dscr2[3]:screen-value 
          bf-eb.i-dscr2[4] = eb.i-dscr2[4]:screen-value 
          bf-eb.i-dscr2[5] = eb.i-dscr2[5]:screen-value 
          bf-eb.i-dscr2[6] = eb.i-dscr2[6]:screen-value 
          bf-eb.i-dscr2[7] = eb.i-dscr2[7]:screen-value 
          bf-eb.i-dscr2[8] = eb.i-dscr2[8]:screen-value 
          bf-eb.i-dscr2[9] = eb.i-dscr2[9]:screen-value 
          bf-eb.i-dscr2[10] = eb.i-dscr2[10]:screen-value 
          bf-eb.i-dscr2[11] = eb.i-dscr2[11]:screen-value 
          bf-eb.i-dscr2[12] = eb.i-dscr2[12]:screen-value 
          bf-eb.i-dscr2[13] = eb.i-dscr2[13]:screen-value 
          bf-eb.i-dscr2[14] = eb.i-dscr2[14]:screen-value 
          bf-eb.i-dscr2[15] = eb.i-dscr2[15]:screen-value 
          bf-eb.i-dscr2[16] = eb.i-dscr2[16]:screen-value 
          bf-eb.i-dscr2[17] = eb.i-dscr2[17]:screen-value 
          /*bf-eb.i-dscr2[18] = eb.i-dscr2[18]:screen-value 
          bf-eb.i-dscr2[19] = eb.i-dscr2[19]:screen-value 
          bf-eb.i-dscr2[20] = eb.i-dscr2[20]:screen-value  */
          bf-eb.i-%2[1] = int(eb.i-%2[1]:screen-value )
          bf-eb.i-%2[2] = int(eb.i-%2[2]:screen-value )
          bf-eb.i-%2[3] = int(eb.i-%2[3]:screen-value )
          bf-eb.i-%2[4] = int(eb.i-%2[4]:screen-value )
          bf-eb.i-%2[5] = int(eb.i-%2[5]:screen-value )
          bf-eb.i-%2[6] = int(eb.i-%2[6]:screen-value )
          bf-eb.i-%2[7] = int(eb.i-%2[7]:screen-value )
          bf-eb.i-%2[8] = int(eb.i-%2[8]:screen-value )
          bf-eb.i-%2[9] = int(eb.i-%2[9]:screen-value )
          bf-eb.i-%2[10] = int(eb.i-%2[10]:screen-value )
          bf-eb.i-%2[11] = int(eb.i-%2[11]:screen-value )
          bf-eb.i-%2[12] = int(eb.i-%2[12]:screen-value )
          bf-eb.i-%2[13] = int(eb.i-%2[13]:screen-value )
          bf-eb.i-%2[14] = int(eb.i-%2[14]:screen-value )
          bf-eb.i-%2[15] = int(eb.i-%2[15]:screen-value )
          bf-eb.i-%2[16] = int(eb.i-%2[16]:screen-value )
          bf-eb.i-%2[17] = int(eb.i-%2[17]:screen-value )
          /*bf-eb.i-%2[18] = int(eb.i-%2[18]:screen-value )
          bf-eb.i-%2[19] = int(eb.i-%2[19]:screen-value )
          bf-eb.i-%2[20] = int(eb.i-%2[20]:screen-value )*/
          .
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-inks V-table-Win 
PROCEDURE disable-inks :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE eb.i-col
            eb.i-pass
            eb.i-coat
            eb.i-coat-p
            eb.i-coldscr
            eb.i-ps2[1 for 17]
            eb.i-code2[1 for 17]
            eb.i-dscr2[1 for 17]
            eb.i-%2[1 for 17]
            fi_unit-1
            fi_unit-2
            fi_unit-3
            fi_unit-4
            fi_unit-5
            fi_unit-6
            fi_unit-7
            fi_unit-8
            fi_unit-9
            fi_unit-10
            fi_unit-11
            fi_unit-12
            fi_unit-13
            fi_unit-14
            fi_unit-15
            fi_unit-16
            fi_unit-17
            fi_side-1
            fi_side-2
            fi_side-3
            fi_side-4
            fi_side-5
            fi_side-6
            fi_side-7
            fi_side-8
            fi_side-9
            fi_side-10
            fi_side-11
            fi_side-12
            fi_side-13
            fi_side-14
            fi_side-15
            fi_side-16
            fi_side-17.
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
  HIDE FRAME fold.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fgcolors-reset V-table-Win 
PROCEDURE fgcolors-reset :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  DEF BUFFER ink-eb FOR eb.
  DEF BUFFER ink-ef FOR ef.
  DEF BUFFER b-ref FOR reftable.

  DEF VAR lv-valid AS CHAR NO-UNDO.
  DEF VAR lv-type AS CHAR FORMAT "!" NO-UNDO.
  DEF VAR lv-list AS CHAR NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR v-side-count AS INT NO-UNDO.
  DEF VAR v-str AS CHAR NO-UNDO.

  FIND ink-eb WHERE ROWID(ink-eb) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL ink-eb THEN DO WITH FRAME {&FRAME-NAME}:
    FOR EACH itemfg-ink
        WHERE itemfg-ink.company EQ ink-eb.company
          AND itemfg-ink.i-no    EQ ink-eb.stock-no
        NO-LOCK,
        FIRST item
        WHERE item.company EQ itemfg-ink.company
          AND item.i-no    EQ itemfg-ink.rm-i-no
          AND INDEX(lv-pr-types,item.press-type) GT 0
        NO-LOCK
        BREAK BY item.press-type:

      IF FIRST-OF(item.press-type) THEN
        ASSIGN
         lv-valid = TRIM(lv-valid) + TRIM(item.press-type)
         lv-list  = TRIM(lv-list) +
                    TRIM(ENTRY(INDEX(lv-pr-types,item.press-type),lv-pr-list)) + ",".
    END.
    IF SUBSTR(lv-list,LENGTH(lv-list),1) EQ "," THEN
      SUBSTR(lv-list,LENGTH(lv-list),1) = "".

    lv-type = SUBSTR(lv-valid,1,1).

    IF NUM-ENTRIES(lv-list) GT 1 THEN DO:
      FOR EACH ink-ef
          WHERE ink-ef.company      EQ ink-eb.company
            AND ink-ef.est-no       EQ ink-eb.est-no
            AND ink-ef.form-no      EQ ink-eb.form-no
            AND TRIM(ink-ef.m-code) NE ""
          NO-LOCK,
          FIRST mach
          WHERE mach.company EQ ink-ef.company
            AND mach.loc     EQ ink-ef.loc
            AND mach.m-code  EQ ink-ef.m-code
            AND INDEX(lv-valid,mach.pr-type) GT 0
          NO-LOCK:
        lv-type = mach.pr-type.
        LEAVE.
      END.

      DO WHILE TRUE:
        MESSAGE "Select Press Type: " + TRIM(lv-list)
            UPDATE lv-type.

        IF INDEX(lv-valid,lv-type) GT 0 THEN LEAVE.
      END.
    END.

    RUN fg/setcolor.p (ROWID(ink-eb), lv-type).
      FIND CURRENT ink-eb EXCLUSIVE-LOCK NO-ERROR.
       DO:
          {ce/updunit#.i ink-eb}
       END.
      FIND CURRENT ink-eb NO-LOCK NO-ERROR.       
/*    END.*/
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-depth-reftable V-table-Win 
PROCEDURE find-depth-reftable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAM op-rowid AS ROWID NO-UNDO.

  DEF BUFFER b-eb FOR eb.
  DEF BUFFER b-rt FOR reftable.
  DEF BUFFER b-item FOR ITEM.

  FIND b-eb WHERE ROWID(b-eb) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-eb THEN DO TRANSACTION:
     FIND FIRST b-rt
          WHERE b-rt.reftable EQ "cedepth"
            AND b-rt.company  EQ b-eb.company
            AND b-rt.loc      EQ b-eb.est-no
            AND b-rt.code     EQ STRING(b-eb.form-no,"9999999999")
            AND b-rt.code2    EQ STRING(b-eb.blank-no,"9999999999")
          NO-LOCK NO-ERROR.
     IF NOT AVAIL b-rt THEN DO:
        CREATE b-rt.
        ASSIGN
           b-rt.reftable = "cedepth"
           b-rt.company  = b-eb.company
           b-rt.loc      = b-eb.est-no
           b-rt.code     = STRING(b-eb.form-no,"9999999999")
           b-rt.code2    = STRING(b-eb.blank-no,"9999999999").

        IF eb.layer-pad NE "" THEN
        DO:
           find FIRST b-item where
                b-item.company = b-eb.company and
                b-item.i-no = b-eb.layer-pad and
                b-item.mat-type = "5"
                no-lock no-error.

           IF AVAIL b-item THEN
           DO:
              ASSIGN
                 b-rt.val[1] = b-item.case-d
                 b-rt.val[2] = b-item.case-d.
              RELEASE b-item.
           END.
        END.
    END.

    op-rowid = ROWID(b-rt).
    RELEASE b-rt.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getObjectHandle V-table-Win 
PROCEDURE getObjectHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipName AS CHARACTER NO-UNDO.

  DEFINE OUTPUT PARAMETER opHandle AS HANDLE NO-UNDO.

  DEFINE VARIABLE current-widget AS HANDLE NO-UNDO.

  ASSIGN
    current-widget = FRAME {&FRAME-NAME}:HANDLE
    current-widget = current-widget:FIRST-CHILD
    current-widget = current-widget:FIRST-CHILD.
  DO WHILE current-widget NE ?:
    IF current-widget:TYPE EQ 'FILL-IN' AND
       current-widget:NAME EQ ipName AND
       current-widget:INDEX EQ ipIdx THEN DO:
      opHandle = current-widget.
      RETURN.
    END. /* type fill-in */
    current-widget = current-widget:NEXT-SIBLING.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getUnit# V-table-Win 
PROCEDURE getUnit# :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.

  DEFINE VARIABLE iPS2 AS HANDLE NO-UNDO.
  DEFINE VARIABLE iCode2 AS HANDLE NO-UNDO.
  DEFINE VARIABLE fiUnit AS HANDLE NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE rtRowID AS ROWID NO-UNDO.

  DEFINE BUFFER b-ef FOR ef.
  DEFINE BUFFER b-eb FOR eb.

  RUN getObjectHandle (ipIdx,'i-ps2',OUTPUT iPS2).
  RUN getObjectHandle (ipIdx,'i-code2',OUTPUT iCode2).
  RUN getObjectHandle (0,'fi_unit-' + STRING(ipIdx),OUTPUT fiUnit).

  IF iCode2:SCREEN-VALUE NE '' THEN
  FOR EACH b-ef OF eb NO-LOCK, EACH b-eb OF b-ef NO-LOCK WHERE ROWID(b-eb) NE ROWID(eb):
    DO i = 1 TO EXTENT(eb.i-code2):
      IF b-eb.i-ps2[i] NE INTEGER(iPS2:SCREEN-VALUE) OR
         b-eb.i-code2[i] NE iCode2:SCREEN-VALUE THEN NEXT.
      j = IF i LE 17 THEN 0 ELSE 1.
      fiUnit:SCREEN-VALUE = STRING(b-eb.unitNo[i],'>>>').
      RETURN.
    END. /* do i */
  END. /* each b-ef */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-ef FOR ef.
  DEF BUFFER b-eb FOR eb.
  DEF BUFFER b-rt FOR reftable.
  DEF BUFFER set-eb FOR eb.

  DEF VAR a AS INT NO-UNDO.
  DEF VAR b AS INT NO-UNDO.
  DEF VAR lv-unit-1 LIKE reftable.val EXTENT 20 NO-UNDO.
  DEF VAR lv-unit-2 LIKE reftable.val EXTENT 20 NO-UNDO.
  DEF VAR ll-ans AS LOG NO-UNDO.
  DEF VAR ll-assem AS LOG INIT ? NO-UNDO.
  DEF VAR v-side-string AS CHAR NO-UNDO.
  DEF VAR v-side-string-2 AS CHAR NO-UNDO.

  DEF VAR lv-side-1 AS CHAR NO-UNDO.
  DEF VAR lv-side-2 AS CHAR NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN new-carrier.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  do with frame {&frame-name} :
    assign eb.cas-no eb.cas-cost eb.cas-cnt eb.cas-len eb.cas-wid
           eb.cas-dep eb.cas-pal eb.cas-wt
           eb.tr-no
           eb.tr-cost eb.tr-cnt eb.tr-len eb.tr-wid eb.tr-dep
           eb.tr-cas 
           eb.carrier eb.carr-dscr eb.weight-m eb.dest-code
           eb.fr-out-c eb.fr-out-m eb.chg-method
           fi_prod-notes
           .
  end.

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

  FIND FIRST itemfg
      WHERE itemfg.company EQ eb.company
        AND itemfg.i-no    EQ eb.stock-no
        AND eb.stock-no    NE ""
      NO-ERROR.
  IF AVAIL itemfg THEN
  DO:
     itemfg.prod-notes = fi_prod-notes.
     FIND CURRENT itemfg NO-LOCK.
  END.

  if ll-unit-calc then do:
     def var lv-cas-pal like eb.cas-pal no-undo.
     def var lv-tr-cnt like eb.tr-cnt no-undo.
     def var lv-error as log no-undo.

     find xest where xest.company = eb.company and
                     xest.est-no = eb.est-no .
     find xeb where recid(xeb) = recid(eb).                
  END.

  IF ll-unit-calc OR ll-update-pack THEN DO:
    IF est.est-type EQ 2 AND eb.form-no NE 0               AND
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
         b-eb.tr-dep   = eb.tr-dep.
      END.
    END.
  END.

  FIND FIRST b-ef OF eb EXCLUSIVE NO-ERROR.
  IF AVAIL b-ef THEN DO:
    FIND FIRST b-eb OF b-ef WHERE ROWID(b-eb) NE ROWID(eb) NO-LOCK NO-ERROR.
    IF NOT AVAIL b-eb THEN
      ASSIGN
       b-ef.f-col    = eb.i-col
       b-ef.f-pass   = eb.i-pass
       b-ef.f-coat   = eb.i-coat
       b-ef.f-coat-p = eb.i-coat-p.
  END.

  ASSIGN
     lv-unit-1 = 0
     lv-side-1 = ""
     v-side-string = (IF fi_side-1 = "" THEN " " ELSE fi_side-1)
                   + (IF fi_side-2 = "" THEN " " ELSE fi_side-2)
                   + (IF fi_side-3 = "" THEN " " ELSE fi_side-3)
                   + (IF fi_side-4 = "" THEN " " ELSE fi_side-4)
                   + (IF fi_side-5 = "" THEN " " ELSE fi_side-5)
                   + (IF fi_side-6 = "" THEN " " ELSE fi_side-6)
                   + (IF fi_side-7 = "" THEN " " ELSE fi_side-7)
                   + (IF fi_side-8 = "" THEN " " ELSE fi_side-8)
                   + (IF fi_side-9 = "" THEN " " ELSE fi_side-9)
                   + (IF fi_side-10 = "" THEN " " ELSE fi_side-10)
                   + (IF fi_side-11 = "" THEN " " ELSE fi_side-11)
                   + (IF fi_side-12 = "" THEN " " ELSE fi_side-12).

  DO:
     ASSIGN
      eb.unitNo[1]  = fi_unit-1  
      eb.unitNo[2]  = fi_unit-2
      eb.unitNo[3]  = fi_unit-3
      eb.unitNo[4]  = fi_unit-4  
      eb.unitNo[5]  = fi_unit-5
      eb.unitNo[6]  = fi_unit-6
      eb.unitNo[7]  = fi_unit-7  
      eb.unitNo[8]  = fi_unit-8
      eb.unitNo[9]  = fi_unit-9
      eb.unitNo[10] = fi_unit-10
      eb.unitNo[11] = fi_unit-11
      eb.unitNo[12] = fi_unit-12
      eb.unitNo[13] = fi_unit-13
      eb.unitNo[14] = fi_unit-14
      eb.unitNo[15] = fi_unit-15
      eb.unitNo[16] = fi_unit-16
      eb.unitNo[17] = fi_unit-17
      eb.side[1] = fi_side-1
      eb.side[2] = fi_side-2
      eb.side[3] = fi_side-3
      eb.side[4] = fi_side-4
      eb.side[5] = fi_side-5
      eb.side[6] = fi_side-6
      eb.side[7] = fi_side-7
      eb.side[8] = fi_side-8
      eb.side[9] = fi_side-9
      eb.side[10] = fi_side-10
      eb.side[11] = fi_side-11
      eb.side[12] = fi_side-12
      eb.side[13] = fi_side-13
      eb.side[14] = fi_side-14
      eb.side[15] = fi_side-15
      eb.side[16] = fi_side-16
      eb.side[17] = fi_side-17.


     DO a = 1 TO 17:
        lv-unit-1[a] = eb.unitNo[a].
     END.

  END.


  RUN find-depth-reftable(ROWID(eb), OUTPUT lv-rowid).
  FIND reftable WHERE ROWID(reftable) EQ lv-rowid NO-ERROR.

  IF AVAIL reftable THEN DO:
     ASSIGN
        reftable.val[1] = f-lp-dep
        reftable.val[2] = f-div-dep.
     FIND CURRENT reftable NO-LOCK.
     RELEASE reftable.
  END.

  FOR EACH b-ef OF eb NO-LOCK,
      EACH b-eb OF b-ef
      WHERE ROWID(b-eb) NE ROWID(eb)
      NO-LOCK:

    ASSIGN
       lv-unit-2 = 0
       lv-side-2 = "".

    DO:
       DO a = 1 TO 17:
          lv-unit-2[a] = b-eb.unitNo[a].
       END.

       lv-side-2 = lv-side-2 + b-eb.side[1].
    END.

    DO a = 1 TO EXTENT(eb.i-code2):
       IF eb.i-code2[a] NE "" AND lv-unit-1[a] NE 0 THEN
       DO b = 1 TO EXTENT(b-eb.i-code2):
          IF b-eb.i-code2[b] EQ eb.i-code2[a] AND
             b-eb.i-ps2[b]   EQ eb.i-ps2[a]   THEN DO:
             ASSIGN
                lv-unit-2[b] = lv-unit-1[a]
                SUBSTRING(lv-side-2,b,1) = SUBSTRING(lv-side-1,a,1).
             LEAVE.
         END.
       END.
    END.

    DO a = 1 TO 20:
      b-eb.unitNo[a] = lv-unit-2[a].
      b-eb.side[a] = substring(lv-side-2,a,1).
    END.
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
  RUN update-ink.
  RUN disable-inks.

  DISABLE f-lp-dep f-div-dep WITH FRAME {&FRAME-NAME}.
  RUN release-shared-buffers.

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
  IF NOT AVAIL eb THEN RETURN.

  ASSIGN
   fi_unit-1  = 0
   fi_unit-2  = 0
   fi_unit-3  = 0
   fi_unit-4  = 0
   fi_unit-5  = 0
   fi_unit-6  = 0
   fi_unit-7  = 0
   fi_unit-8  = 0
   fi_unit-9  = 0
   fi_unit-10 = 0
   fi_unit-11 = 0
   fi_unit-12 = 0
   fi_unit-13 = 0
   fi_unit-14 = 0
   fi_unit-15 = 0
   fi_unit-16 = 0
   fi_unit-17 = 0
   fi_side-1  = ""
   fi_side-2  = ""
   fi_side-3  = ""
   fi_side-4  = ""
   fi_side-5  = ""
   fi_side-6  = ""
   fi_side-7  = ""
   fi_side-8  = ""
   fi_side-9  = ""
   fi_side-10 = ""
   fi_side-11 = ""
   fi_side-12 = ""
   fi_side-13 = ""
   fi_side-14 = ""
   fi_side-15 = ""
   fi_side-16 = ""
   fi_side-17 = ""
   f-lp-dep = 0
   f-div-dep = 0.

  IF NOT adm-new-record THEN DO:
     ASSIGN
        fi_unit-1  = eb.unitNo[1]
        fi_unit-2  = eb.unitNo[2]
        fi_unit-3  = eb.unitNo[3]
        fi_unit-4  = eb.unitNo[4]
        fi_unit-5  = eb.unitNo[5]
        fi_unit-6  = eb.unitNo[6]
        fi_unit-7  = eb.unitNo[7]
        fi_unit-8  = eb.unitNo[8]
        fi_unit-9  = eb.unitNo[9]
        fi_unit-10 = eb.unitNo[10]
        fi_unit-11 = eb.unitNo[11]
        fi_unit-12 = eb.unitNo[12]
        fi_unit-13 = eb.unitNo[13]
        fi_unit-14 = eb.unitNo[14]
        fi_unit-15 = eb.unitNo[15]
        fi_unit-16 = eb.unitNo[16]
        fi_unit-17 = eb.unitNo[17]
        fi_side-1  = eb.side[1]
        fi_side-2  = eb.side[2]
        fi_side-3  = eb.side[3]
        fi_side-4  = eb.side[4]
        fi_side-5  = eb.side[5]
        fi_side-6  = eb.side[6]
        fi_side-7  = eb.side[7]
        fi_side-8  = eb.side[8]
        fi_side-9  = eb.side[9]
        fi_side-10  = eb.side[10]
        fi_side-11  = eb.side[11]
        fi_side-12  = eb.side[12]
        fi_side-13  = eb.side[13]
        fi_side-14  = eb.side[14]
        fi_side-15  = eb.side[15]
        fi_side-16  = eb.side[16]
        fi_side-17  = eb.side[17]
         .


     RUN find-depth-reftable(ROWID(eb), OUTPUT lv-rowid).
     FIND reftable WHERE ROWID(reftable) EQ lv-rowid NO-ERROR.
     IF AVAIL reftable THEN
        ASSIGN
           f-lp-dep  = reftable.val[1]
           f-div-dep = reftable.val[2].
  END.

  FIND FIRST itemfg
      WHERE itemfg.company EQ eb.company
        AND itemfg.i-no    EQ eb.stock-no
        AND eb.stock-no    NE ""
      NO-LOCK NO-ERROR.
  fi_prod-notes = IF AVAIL itemfg THEN itemfg.prod-notes ELSE "".

  RUN dynamic-labels.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN show-pr-type.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li-num-of-code AS INT NO-UNDO.

{&methods/lValidateError.i YES}
  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:

    ASSIGN f-lp-dep f-div-dep.

    IF eb.form-no NE 0 THEN DO:
      {est/valinks2.i 1 2}
      {est/valinks2.i 2 2}
      {est/valinks2.i 3 2}
      {est/valinks2.i 4 2}
      {est/valinks2.i 5 2}
      {est/valinks2.i 6 2}
      {est/valinks2.i 7 2}
      {est/valinks2.i 8 2}
      {est/valinks2.i 9 2}
      {est/valinks2.i 10 2}
      {est/valinks2.i 11 2}
      {est/valinks2.i 12 2}
      {est/valinks2.i 13 2}
      {est/valinks2.i 14 2}
      {est/valinks2.i 15 2}
      {est/valinks2.i 16 2}
      {est/valinks2.i 17 2}
      /*{est/valinks2.i 18 2}
      {est/valinks2.i 19 2}
      {est/valinks2.i 20 2}*/
    END.

    {est/valinks3.i 1 2}
    {est/valinks3.i 2 2}
    {est/valinks3.i 3 2}
    {est/valinks3.i 4 2}
    {est/valinks3.i 5 2}
    {est/valinks3.i 6 2}
    {est/valinks3.i 7 2}
    {est/valinks3.i 8 2}
    {est/valinks3.i 9 2}
    {est/valinks3.i 10 2}
    {est/valinks3.i 11 2}
    {est/valinks3.i 12 2}
    {est/valinks3.i 13 2}
    {est/valinks3.i 14 2}
    {est/valinks3.i 15 2}
    {est/valinks3.i 16 2}
    {est/valinks3.i 17 2}
/*    {est/valinks3.i 18 2}
    {est/valinks3.i 19 2}
    {est/valinks3.i 20 2} */

    {est/valside.i 1}
    {est/valside.i 2}
    {est/valside.i 3}
    {est/valside.i 4}
    {est/valside.i 5}
    {est/valside.i 6}
    {est/valside.i 7}
    {est/valside.i 8}
    {est/valside.i 9}
    {est/valside.i 10}
    {est/valside.i 11}
    {est/valside.i 12}
    {est/valside.i 13}
    {est/valside.i 14}
    {est/valside.i 15}
    {est/valside.i 16}
    {est/valside.i 17}

    IF ll-unit-calc EQ NO AND ll-update-pack EQ NO THEN DO:
      RUN est/val-inks.p (FRAME {&FRAME-NAME}:HANDLE, 2, ?) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

      li-num-of-code = 0.
      if eb.i-code2[1]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[2]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[3]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[4]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[5]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[6]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[7]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[8]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[9]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[10]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[11]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[12]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[13]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[14]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[15]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[16]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[17]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      /*if eb.i-code2[18]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[19]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
      if eb.i-code2[20]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1*/.
 
      if li-num-of-code <> (integer(eb.i-col:screen-value) + 
                           integer(eb.i-coat:screen-value) )
      then do:
        message "Invalid Number of Color and Coating." view-as alert-box.
        apply "entry" to eb.i-col.
        return no-apply.
      end.
    END.

    ELSE DO:
      RUN valid-cas-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
      RUN valid-layer-pad NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
      RUN valid-divider NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
      RUN valid-per NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
      RUN valid-cas-wt NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

      if eb.tr-no:screen-value <> "" and
         not can-find(item where item.company = eb.company and item.i-no = eb.tr-no:screen-value)
      then do:
            message "Invalid Unit#. Try Help." view-as alert-box error.
            apply "entry" to eb.tr-no.
            return no-apply.
      end.

      RUN valid-carrier NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

      RUN valid-dest-code NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
  END.
{&methods/lValidateError.i NO}
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   ll-unit-calc   = NO
   ll-update-pack = NO.

  DISABLE f-lp-dep f-div-dep WITH FRAME {&FRAME-NAME}.

  RUN release-shared-buffers.

  RUN custom/framechk.p (2, FRAME {&FRAME-NAME}:HANDLE).

  IF framechk-i-changed THEN RUN est/updest3.p (ROWID(eb), ROWID(eb), ?).

  RUN update-ink.
  RUN disable-inks.

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
    lv-layers = 1.
    {sys/inc/roundup.i lv-layers}
    eb.tr-cas:SCREEN-VALUE = STRING(lv-layers).

    RUN calc-tr-cnt.
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
    eb.cas-pal:SCREEN-VALUE = STRING(INT(eb.tr-cas:SCREEN-VALUE)).
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

IF eb.form-no NE 0 THEN DO:
  {custom/checkuse.i}

  RUN custom/framechk.p (1, FRAME {&FRAME-NAME}:HANDLE).

  RUN reset-ink1 (ROWID(eb)).

  RUN custom/framechk.p (2, FRAME {&FRAME-NAME}:HANDLE).

  IF framechk-i-changed THEN RUN est/updest3.p (ROWID(eb), ROWID(eb), 1).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-ink1 V-table-Win 
PROCEDURE reset-ink1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/ 
   DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

   DEF BUFFER alt-item FOR item.
   DEF BUFFER ink-eb FOR eb.
   DEF BUFFER ink-ef FOR ef.
   DEF BUFFER b-ref FOR reftable.

   def var k as int no-undo.
   def var counter as int no-undo.
   def var i as int no-undo.
   def var j as int no-undo.
   def var save_id as recid no-undo.
   def var save_id2 as recid no-undo.
   def var choice as log no-undo.
   DEF VAR ll AS LOG NO-UNDO.
   DEF VAR li AS INT NO-UNDO.        
   DEF VAR v-str AS CHAR NO-UNDO.
   DEF VAR v-side-count AS INT NO-UNDO.

  FIND ink-eb WHERE ROWID(ink-eb) EQ ip-rowid NO-LOCK NO-ERROR.
  IF NOT AVAIL ink-eb THEN RETURN.

  ll = NO.  

  FIND FIRST style
      WHERE style.company EQ ink-eb.company
        AND style.style   EQ ink-eb.style
      NO-LOCK NO-ERROR.

  FIND FIRST ce-ctrl
      WHERE ce-ctrl.company EQ ink-eb.company
        AND ce-ctrl.loc     EQ ink-eb.loc
      NO-LOCK NO-ERROR.

  FIND FIRST ink-ef
      WHERE ink-ef.company EQ ink-eb.company
        AND ink-ef.est-no  EQ ink-eb.est-no
        AND ink-ef.form-no EQ ink-eb.form-no
      NO-LOCK NO-ERROR.

  RELEASE mach.
  RELEASE itemfg-ink.

  IF fgcolors-log AND TRIM(ink-eb.stock-no) NE "" AND AVAIL ink-ef AND ink-ef.m-code NE "" THEN
  FIND FIRST mach
      WHERE mach.company EQ ink-ef.company
        AND mach.loc     EQ ink-ef.loc
        AND mach.m-code  EQ ink-ef.m-code
      NO-LOCK NO-ERROR.

  IF AVAIL mach THEN
  FOR EACH itemfg-ink
      WHERE itemfg-ink.company EQ ink-eb.company
        AND itemfg-ink.i-no    EQ ink-eb.stock-no
      NO-LOCK,
      FIRST item
      WHERE item.company    EQ itemfg-ink.company
        AND item.i-no       EQ itemfg-ink.rm-i-no
        AND item.press-type EQ mach.pr-type
      NO-LOCK:
    LEAVE.
  END.

  IF AVAIL itemfg-ink THEN DO:
    MESSAGE "Use Colors in FG File?  (No will default from Style or CE Control File)"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
        UPDATE ll.

    IF ll THEN RUN fgcolors-reset (ip-rowid).
  END.

  IF NOT ll THEN DO:
    ll = YES.
    MESSAGE "Use Ink in Style File?  (No will default from CE Control File - " + TRIM(ce-ctrl.def-ink) + 
             " or " "Yes from Style - " + TRIM(style.material[2]) + ")" SKIP
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
        UPDATE ll.

    IF ll NE ? THEN DO:
      FIND CURRENT ink-eb.

      RELEASE item.
      RELEASE alt-item.

      if avail style AND ll then do:
         if k = 0 then k = integer(style.material[3]).

         RELEASE ITEM.

         IF style.material[2] NE "" THEN
            find first item where
                 item.company = ink-eb.company and
                 item.i-no = style.material[2]
                 no-lock no-error.

         if avail item then k = integer(style.material[3]).

         IF style.material[6] NE "" THEN
            find first alt-item where
                 alt-item.company  = ink-eb.company  and
                 alt-item.mat-type = "V"     and
                 alt-item.i-no     = style.material[6]
                 no-lock no-error.
      end.
      if not avail item or not avail alt-item or (k = 0) then do:
         if k = 0 then k = ce-ctrl.def-inkcov.
         if not avail item then do:
            find first item where item.company = ink-eb.company and
                       item.i-no = ce-ctrl.def-ink no-lock no-error.
         end.
         if not avail alt-item then
            find first alt-item where alt-item.company  = ink-eb.company  and
                                      alt-item.mat-type = "V"     and
                                      alt-item.i-no     = ce-ctrl.def-coat
                                      no-lock no-error.
      end.

      ASSIGN
      save_id = recid(item)
      save_id2 = recid(alt-item)
      j = (ink-eb.i-col + ink-eb.i-coat) / ink-eb.i-pass
      counter = 1
      choice = true.

      {sys/inc/roundup.i j}

/*    do i = 1 to 12:
       if ink-eb.i-code2[i] ne "" then do:
          choice = no.
          leave.
       end.
      end.     
 commented to recalc every time */

      if choice then do i = 1 to 20:
         if i le ink-eb.i-col then do:
              find item where recid(item) = save_id no-lock no-error.
             /* assign bf-eb.i-ps2[i]   = counter
                     bf-eb.i-code2[i] = item.i-no
                     bf-eb.i-dscr2[i] = item.est-dscr
                     bf-eb.i-%2[i]    = k.
             */
             IF AVAIL item THEN
             case string(i) :
                when "1" then assign ink-eb.i-ps2[1]   = counter
                                     ink-eb.i-code2[1] = item.i-no
                                     ink-eb.i-dscr2[1] = item.est-dscr
                                     ink-eb.i-%2[1]    = k.
                when "2" then assign ink-eb.i-ps2[2]   = counter
                                     ink-eb.i-code2[2] = item.i-no
                                     ink-eb.i-dscr2[2] = item.est-dscr
                                     ink-eb.i-%2[2]    = k.
                when "3" then assign ink-eb.i-ps2[3]   = counter
                                     ink-eb.i-code2[3] = item.i-no
                                     ink-eb.i-dscr2[3] = item.est-dscr
                                     ink-eb.i-%2[3]    = k.
                when "4" then assign ink-eb.i-ps2[4]   = counter
                                     ink-eb.i-code2[4] = item.i-no
                                     ink-eb.i-dscr2[4] = item.est-dscr
                                     ink-eb.i-%2[4]    = k.
                when "5" then assign ink-eb.i-ps2[5]   = counter
                                     ink-eb.i-code2[5] = item.i-no
                                     ink-eb.i-dscr2[5] = item.est-dscr
                                     ink-eb.i-%2[5]    = k.
                when "6" then assign ink-eb.i-ps2[6]   = counter
                                     ink-eb.i-code2[6] = item.i-no
                                     ink-eb.i-dscr2[6] = item.est-dscr
                                     ink-eb.i-%2[6]    = k.
                when "7" then assign ink-eb.i-ps2[7]   = counter
                                     ink-eb.i-code2[7] = item.i-no
                                     ink-eb.i-dscr2[7] = item.est-dscr
                                     ink-eb.i-%2[7]    = k.
                when "8" then assign ink-eb.i-ps2[8]   = counter
                                     ink-eb.i-code2[8] = item.i-no
                                     ink-eb.i-dscr2[8] = item.est-dscr
                                     ink-eb.i-%2[8]    = k.
                when "9" then assign ink-eb.i-ps2[9]   = counter
                                     ink-eb.i-code2[9] = item.i-no
                                     ink-eb.i-dscr2[9] = item.est-dscr
                                     ink-eb.i-%2[9]    = k.
                when "10" then assign ink-eb.i-ps2[10]   = counter
                                     ink-eb.i-code2[10] = item.i-no
                                     ink-eb.i-dscr2[10] = item.est-dscr
                                     ink-eb.i-%2[10]    = k.
                when "11" then assign ink-eb.i-ps2[11]   = counter
                                     ink-eb.i-code2[11] = item.i-no
                                     ink-eb.i-dscr2[11] = item.est-dscr
                                     ink-eb.i-%2[11]    = k.
                when "12" then assign ink-eb.i-ps2[12]   = counter
                                     ink-eb.i-code2[12] = item.i-no
                                     ink-eb.i-dscr2[12] = item.est-dscr
                                     ink-eb.i-%2[12]    = k.
                when "13" then assign ink-eb.i-ps2[13]   = counter
                                     ink-eb.i-code2[13] = item.i-no
                                     ink-eb.i-dscr2[13] = item.est-dscr
                                     ink-eb.i-%2[13]    = k.
                when "14" then assign ink-eb.i-ps2[14]   = counter
                                     ink-eb.i-code2[14] = item.i-no
                                     ink-eb.i-dscr2[14] = item.est-dscr
                                     ink-eb.i-%2[14]    = k.
                when "15" then assign ink-eb.i-ps2[15]   = counter
                                     ink-eb.i-code2[15] = item.i-no
                                     ink-eb.i-dscr2[15] = item.est-dscr
                                     ink-eb.i-%2[15]    = k.
                when "16" then assign ink-eb.i-ps2[16]   = counter
                                     ink-eb.i-code2[16] = item.i-no
                                     ink-eb.i-dscr2[16] = item.est-dscr
                                     ink-eb.i-%2[16]    = k.
                when "17" then assign ink-eb.i-ps2[17]   = counter
                                     ink-eb.i-code2[17] = item.i-no
                                     ink-eb.i-dscr2[17] = item.est-dscr
                                     ink-eb.i-%2[17]    = k.
                when "18" then assign ink-eb.i-ps2[18]   = counter
                                     ink-eb.i-code2[18] = item.i-no
                                     ink-eb.i-dscr2[18] = item.est-dscr
                                     ink-eb.i-%2[18]    = k.
                when "19" then assign ink-eb.i-ps2[19]   = counter
                                     ink-eb.i-code2[19] = item.i-no
                                     ink-eb.i-dscr2[19] = item.est-dscr
                                     ink-eb.i-%2[19]    = k.
                when "20" then assign ink-eb.i-ps2[20]   = counter
                                     ink-eb.i-code2[20] = item.i-no
                                     ink-eb.i-dscr2[20] = item.est-dscr
                                     ink-eb.i-%2[20]    = k.             
             end case.
         end.
         else if i > ink-eb.i-col and
                 i <= ink-eb.i-col + ink-eb.i-coat
         then do:
              find alt-item where recid(alt-item) = save_id2 no-lock no-error.
         /*     assign bf-eb.i-ps2[i]   = counter
                     bf-eb.i-code2[i] = alt-item.i-no
                     bf-eb.i-dscr2[i] = alt-item.est-dscr
                     bf-eb.i-%2[i]    = 100.
           */
              IF AVAIL alt-item THEN
              case string(i) :
                when "1" then assign ink-eb.i-ps2[1]   = counter
                                     ink-eb.i-code2[1] = alt-item.i-no
                                     ink-eb.i-dscr2[1] = alt-item.est-dscr
                                     ink-eb.i-%2[1]    = 100.
                when "2" then assign ink-eb.i-ps2[2]   = counter
                                     ink-eb.i-code2[2] = alt-item.i-no
                                     ink-eb.i-dscr2[2] = alt-item.est-dscr
                                     ink-eb.i-%2[2]    = 100.
                when "3" then assign ink-eb.i-ps2[3]   = counter
                                     ink-eb.i-code2[3] = alt-item.i-no
                                     ink-eb.i-dscr2[3] = alt-item.est-dscr
                                     ink-eb.i-%2[3]    = 100.
                when "4" then assign ink-eb.i-ps2[4]   = counter
                                     ink-eb.i-code2[4] = alt-item.i-no
                                     ink-eb.i-dscr2[4] = alt-item.est-dscr
                                     ink-eb.i-%2[4]    = 100.
                when "5" then assign ink-eb.i-ps2[5]   = counter
                                     ink-eb.i-code2[5] = alt-item.i-no
                                     ink-eb.i-dscr2[5] = alt-item.est-dscr
                                     ink-eb.i-%2[5]    = 100.
                when "6" then assign ink-eb.i-ps2[6]   = counter
                                     ink-eb.i-code2[6] = alt-item.i-no
                                     ink-eb.i-dscr2[6] = alt-item.est-dscr
                                     ink-eb.i-%2[6]    = 100.
                when "7" then assign ink-eb.i-ps2[7]   = counter
                                     ink-eb.i-code2[7] = alt-item.i-no
                                     ink-eb.i-dscr2[7] = alt-item.est-dscr
                                     ink-eb.i-%2[7]    = 100.
                when "8" then assign ink-eb.i-ps2[8]   = counter
                                     ink-eb.i-code2[8] = alt-item.i-no
                                     ink-eb.i-dscr2[8] = alt-item.est-dscr
                                     ink-eb.i-%2[8]    = 100.
                when "9" then assign ink-eb.i-ps2[9]   = counter
                                     ink-eb.i-code2[9] = alt-item.i-no
                                     ink-eb.i-dscr2[9] = alt-item.est-dscr
                                     ink-eb.i-%2[9]    = 100.
                when "10" then assign ink-eb.i-ps2[10]   = counter
                                     ink-eb.i-code2[10] = alt-item.i-no
                                     ink-eb.i-dscr2[10] = alt-item.est-dscr
                                     ink-eb.i-%2[10]    = 100.
                when "11" then assign ink-eb.i-ps2[11]   = counter
                                     ink-eb.i-code2[11] = alt-item.i-no
                                     ink-eb.i-dscr2[11] = alt-item.est-dscr
                                     ink-eb.i-%2[11]    = 100.
                when "12" then assign ink-eb.i-ps2[12]   = counter
                                     ink-eb.i-code2[12] = alt-item.i-no
                                     ink-eb.i-dscr2[12] = alt-item.est-dscr
                                     ink-eb.i-%2[12]    = 100.
                when "13" then assign ink-eb.i-ps2[13]   = counter
                                     ink-eb.i-code2[13] = alt-item.i-no
                                     ink-eb.i-dscr2[13] = alt-item.est-dscr
                                     ink-eb.i-%2[13]    = 100.
                when "14" then assign ink-eb.i-ps2[14]   = counter
                                     ink-eb.i-code2[14] = alt-item.i-no
                                     ink-eb.i-dscr2[14] = alt-item.est-dscr
                                     ink-eb.i-%2[14]    = 100.
                when "15" then assign ink-eb.i-ps2[15]   = counter
                                     ink-eb.i-code2[15] = alt-item.i-no
                                     ink-eb.i-dscr2[15] = alt-item.est-dscr
                                     ink-eb.i-%2[15]    = 100.
                when "16" then assign ink-eb.i-ps2[16]   = counter
                                     ink-eb.i-code2[16] = alt-item.i-no
                                     ink-eb.i-dscr2[16] = alt-item.est-dscr
                                     ink-eb.i-%2[16]    = 100.
                when "17" then assign ink-eb.i-ps2[17]   = counter
                                     ink-eb.i-code2[17] = alt-item.i-no
                                     ink-eb.i-dscr2[17] = alt-item.est-dscr
                                     ink-eb.i-%2[17]    = 100.
                when "18" then assign ink-eb.i-ps2[18]   = counter
                                     ink-eb.i-code2[18] = alt-item.i-no
                                     ink-eb.i-dscr2[18] = alt-item.est-dscr
                                     ink-eb.i-%2[18]    = 100.
                when "19" then assign ink-eb.i-ps2[19]   = counter
                                     ink-eb.i-code2[19] = alt-item.i-no
                                     ink-eb.i-dscr2[19] = alt-item.est-dscr
                                     ink-eb.i-%2[19]    = 100.
                when "20" then assign ink-eb.i-ps2[20]   = counter
                                     ink-eb.i-code2[20] = alt-item.i-no
                                     ink-eb.i-dscr2[20] = alt-item.est-dscr
                                     ink-eb.i-%2[20]    = 100.
             end.                   
         end.
         else if i > ink-eb.i-col + ink-eb.i-coat
         then do:
        /*    assign bf-eb.i-ps2[i]   = 0  
                     bf-eb.i-code2[i] = ""
                     bf-eb.i-dscr2[i] = "" 
                     bf-eb.i-%2[i]    = 0.  */
              case string(i) :
                   when "1" then assign ink-eb.i-ps2[1]   = 0
                                        ink-eb.i-code2[1] = ""
                                        ink-eb.i-dscr2[1] = ""
                                        ink-eb.i-%2[1]    = 0.
                   when "2" then assign ink-eb.i-ps2[2]   = 0
                                        ink-eb.i-code2[2] = ""
                                        ink-eb.i-dscr2[2] = ""
                                        ink-eb.i-%2[2]    = 0.
                   when "3" then assign ink-eb.i-ps2[3]   = 0
                                        ink-eb.i-code2[3] = ""
                                        ink-eb.i-dscr2[3] = ""
                                        ink-eb.i-%2[3]    = 0.
                   when "4" then assign ink-eb.i-ps2[4]   = 0
                                        ink-eb.i-code2[4] = ""
                                        ink-eb.i-dscr2[4] = ""
                                        ink-eb.i-%2[4]    = 0.
                   when "5" then assign ink-eb.i-ps2[5]   = 0
                                        ink-eb.i-code2[5] = ""
                                        ink-eb.i-dscr2[5] = ""
                                        ink-eb.i-%2[5]    = 0.
                   when "6" then assign ink-eb.i-ps2[6]   = 0
                                        ink-eb.i-code2[6] = ""
                                        ink-eb.i-dscr2[6] = ""
                                        ink-eb.i-%2[6]    = 0.
                   when "7" then assign ink-eb.i-ps2[7]   = 0
                                        ink-eb.i-code2[7] = ""
                                        ink-eb.i-dscr2[7] = ""
                                        ink-eb.i-%2[7]    = 0.
                   when "8" then assign ink-eb.i-ps2[8]   = 0
                                        ink-eb.i-code2[8] = ""
                                        ink-eb.i-dscr2[8] = ""
                                        ink-eb.i-%2[8]    = 0.
                   when "9" then assign ink-eb.i-ps2[9]   = 0
                                        ink-eb.i-code2[9] = ""
                                        ink-eb.i-dscr2[9] = ""
                                        ink-eb.i-%2[9]    = 0.
                   when "10" then assign ink-eb.i-ps2[10]   = 0
                                        ink-eb.i-code2[10] = ""
                                        ink-eb.i-dscr2[10] = ""
                                        ink-eb.i-%2[10]    = 0.
                   when "11" then assign ink-eb.i-ps2[11]   = 0
                                        ink-eb.i-code2[11] = ""
                                        ink-eb.i-dscr2[11] = ""
                                        ink-eb.i-%2[11]    = 0.
                   when "12" then assign ink-eb.i-ps2[12]   = 0
                                        ink-eb.i-code2[12] = ""
                                        ink-eb.i-dscr2[12] = ""
                                        ink-eb.i-%2[12]    = 0.
                   when "13" then assign ink-eb.i-ps2[13]   = 0
                                        ink-eb.i-code2[13] = ""
                                        ink-eb.i-dscr2[13] = ""
                                        ink-eb.i-%2[13]    = 0.
                   when "14" then assign ink-eb.i-ps2[14]   = 0
                                        ink-eb.i-code2[14] = ""
                                        ink-eb.i-dscr2[14] = ""
                                        ink-eb.i-%2[14]    = 0.
                   when "15" then assign ink-eb.i-ps2[15]   = 0
                                        ink-eb.i-code2[15] = ""
                                        ink-eb.i-dscr2[15] = ""
                                        ink-eb.i-%2[15]    = 0.
                   when "16" then assign ink-eb.i-ps2[16]   = 0
                                        ink-eb.i-code2[16] = ""
                                        ink-eb.i-dscr2[16] = ""
                                        ink-eb.i-%2[16]    = 0.
                   when "17" then assign ink-eb.i-ps2[17]   = 0
                                        ink-eb.i-code2[17] = ""
                                        ink-eb.i-dscr2[17] = ""
                                        ink-eb.i-%2[17]    = 0.
                   when "18" then assign ink-eb.i-ps2[18]   = 0
                                        ink-eb.i-code2[18] = ""
                                        ink-eb.i-dscr2[18] = ""
                                        ink-eb.i-%2[18]    = 0.
                   when "19" then assign ink-eb.i-ps2[19]   = 0
                                        ink-eb.i-code2[19] = ""
                                        ink-eb.i-dscr2[19] = ""
                                        ink-eb.i-%2[19]    = 0.
                   when "20" then assign ink-eb.i-ps2[20]   = 0
                                        ink-eb.i-code2[20] = ""
                                        ink-eb.i-dscr2[20] = ""
                                        ink-eb.i-%2[20]    = 0.
              end case.       

         end.



         if i modulo j = 0 then counter = counter + 1.
         if counter > ink-eb.i-pass then counter = ink-eb.i-pass.
      end.
      FIND CURRENT ink-eb EXCLUSIVE-LOCK NO-ERROR.         
         DO:
            {ce/updunit#.i ink-eb}
         END.
         
    END.

      FIND CURRENT ink-eb NO-LOCK NO-ERROR.
    END.


  RUN dispatch ("display-fields").

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


  IF est.est-type EQ 2 AND eb.form-no NE 0 THEN
  FIND FIRST b-eb
      WHERE b-eb.company EQ eb.company
        AND b-eb.est-no  EQ eb.est-no
        AND b-eb.eqty    EQ eb.eqty
        AND b-eb.cust-%  LT 0
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
            AND b-eb.cust-%  GE 0:

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

  op-update = NOT ll-ans OR eb.cust-% LT 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-pr-type V-table-Win 
PROCEDURE show-pr-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&frame-name}:
    RELEASE item.

    FIND FIRST item
      {sys/look/itemivW.i}
        AND item.i-no EQ eb.i-code2[1]:SCREEN-VALUE
      NO-LOCK NO-ERROR.
    fi_pr-type:SCREEN-VALUE = 
        IF AVAIL item AND INDEX(lv-pr-types,item.press-type) GT 0 THEN
          ENTRY(INDEX(lv-pr-types,item.press-type),lv-pr-list)
        ELSE "".
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

  RUN set-pack (OUTPUT ll-unit-calc).

  if eb.tr-cas = 0 then eb.tr-cas:screen-value in frame {&frame-name} = "1".

 enable   eb.cas-no eb.cas-cost eb.cas-cnt eb.cas-len eb.cas-wid
          eb.cas-dep 
          eb.layer-pad eb.lp-len eb.lp-wid eb.lp-up f-lp-dep
          eb.divider eb.div-len eb.div-wid eb.div-up f-div-dep
          eb.cas-pal eb.cas-wt
          eb.tr-no
          /*eb.tr-cost eb.tr-cnt eb.tr-len eb.tr-wid eb.tr-dep
          eb.tr-cas */
          eb.carrier eb.weight-m eb.dest-code
          eb.fr-out-c eb.fr-out-m eb.chg-method
          eb.spare-char-3 eb.spare-char-4 eb.spare-int-3
          /*lv-numstack lv-stackcode */
          with frame {&frame-name}.

  RUN enable-case-dims.

  RUN disable-inks.

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

  RUN custom/framechk.p (1, FRAME {&FRAME-NAME}:HANDLE).

  DO WITH FRAME {&FRAME-NAME}:
    ENABLE fi_unit-1
           fi_unit-2
           fi_unit-3
           fi_unit-4
           fi_unit-5
           fi_unit-6
           fi_unit-7
           fi_unit-8
           fi_unit-9
           fi_unit-10
           fi_unit-11
           fi_unit-12
           fi_unit-13
           fi_unit-14
           fi_unit-15
           fi_unit-16
           fi_unit-17

           fi_side-1
           fi_side-2
           fi_side-3
           fi_side-4
           fi_side-5
           fi_side-6
           fi_side-7
           fi_side-8
           fi_side-9
           fi_side-10
           fi_side-11
           fi_side-12
           fi_side-13
           fi_side-14
           fi_side-15
           fi_side-16
           fi_side-17

        /*    fi_unit-18
            fi_unit-19
            fi_unit-20 */.

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
            eb.weight-m
            eb.dest-code
            eb.fr-out-c 
            eb.fr-out-m
            eb.chg-method
            fi_prod-notes.
  END.

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

  RUN custom/framechk.p (1, FRAME {&FRAME-NAME}:HANDLE).

  RUN set-pack (OUTPUT ll-update-pack).

  if eb.tr-cas = 0 then eb.tr-cas:screen-value in frame {&frame-name} = "1".

  IF ll-update-pack THEN DO:
    enable eb.cas-no eb.cas-cost eb.cas-cnt eb.cas-len eb.cas-wid
           eb.cas-dep eb.cas-pal eb.cas-wt
           eb.layer-pad eb.lp-len eb.lp-wid  eb.lp-up f-lp-dep
           eb.divider eb.div-len eb.div-wid  eb.div-up f-div-dep 
           eb.tr-no eb.tr-cost eb.tr-cnt eb.tr-len eb.tr-wid eb.tr-dep
           eb.tr-cas
           eb.carrier eb.weight-m eb.dest-code
           eb.fr-out-c eb.fr-out-m eb.chg-method
           eb.spare-char-3 eb.spare-char-4 eb.spare-int-3
           fi_prod-notes WHEN eb.stock-no NE ""
           with frame {&frame-name}.
    RUN enable-case-dims.
  END.

  RUN disable-inks.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-divider V-table-Win 
PROCEDURE valid-divider :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
DO WITH FRAME {&FRAME-NAME}:
    if eb.divider:screen-value <> "" and
       not can-find(item where item.company = eb.company
                           and item.i-no = eb.divider:screen-value
                           and item.mat-type = "6")
    then do:
      message "Invalid " + TRIM(eb.divider:LABEL) + ", try help..."
          view-as alert-box.
      apply "entry" to eb.divider.
      return ERROR.
    end.
  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-layer-pad V-table-Win 
PROCEDURE valid-layer-pad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
DO WITH FRAME {&FRAME-NAME}:
    if eb.layer-pad:screen-value <> "" and
       not can-find(item where item.company = eb.company
                           and item.i-no = eb.layer-pad:screen-value
                           and item.mat-type = "5")
    then do:
      message "Invalid " + TRIM(eb.layer-pad:LABEL) + ", try help..."
          view-as alert-box.
      apply "entry" to eb.layer-pad.
      return ERROR.
    end.
  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-per V-table-Win 
PROCEDURE valid-per :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cDivPer AS CHAR NO-UNDO.
DEF VAR cLayPer AS CHAR NO-UNDO.
DEF VAR cMsg AS CHAR NO-UNDO.
DEF VAR cValidValues AS CHAR INIT "C,P" NO-UNDO.

  {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:
        IF eb.spare-char-3:SCREEN-VALUE = "" THEN eb.spare-char-3:SCREEN-VALUE = "C".
        IF eb.spare-char-4:SCREEN-VALUE = "" THEN eb.spare-char-4:SCREEN-VALUE = "C".
        ASSIGN 
            cMsg = " Qty Per must be C for Case or P for Pallet"
            cLayPer = CAPS(eb.spare-char-3:SCREEN-VALUE)
            cDivPer = CAPS(eb.spare-char-4:SCREEN-VALUE)
            eb.spare-char-3:SCREEN-VALUE = cLayPer
            eb.spare-char-4:SCREEN-VALUE = cDivPer
            .
        IF NOT LOOKUP(cLayPer, cValidValues) > 0 AND NOT cLayPer = "" THEN DO:
            MESSAGE "Layer Pad" + cMsg
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO eb.spare-char-3.
            RETURN ERROR.
        END.
        IF NOT LOOKUP(cDivPer, cValidValues) > 0 AND NOT cDivPer = "" THEN DO:
            MESSAGE "Divider" + cMsg
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO eb.spare-char-4.
            RETURN ERROR.
        END.
    END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

