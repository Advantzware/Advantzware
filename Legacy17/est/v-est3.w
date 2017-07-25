&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/<table>.w

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
def var ll-update-pack as log no-undo.
&scoped-define est-pack PACK  /* for disable pack */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES eb
&Scoped-define FIRST-EXTERNAL-TABLE eb


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR eb.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS eb.i-col eb.i-pass eb.i-coat eb.i-coat-p ~
eb.cas-no eb.tr-no eb.i-coldscr eb.cas-cost eb.tr-cost eb.cas-cnt eb.tr-cnt ~
eb.cas-len eb.tr-len eb.i-ps[1] eb.i-code[1] eb.i-dscr[1] eb.i-%[1] ~
eb.cas-wid eb.tr-wid eb.i-dscr[2] eb.i-%[2] eb.i-ps[2] eb.i-code[2] ~
eb.cas-dep eb.tr-dep eb.i-dscr[3] eb.i-%[3] eb.i-ps[3] eb.i-code[3] ~
eb.cas-pal eb.tr-cas eb.i-dscr[4] eb.i-%[4] eb.i-ps[4] eb.i-code[4] ~
eb.cas-wt eb.i-dscr[5] eb.i-%[5] eb.i-ps[5] eb.i-code[5] eb.i-dscr[6] ~
eb.i-%[6] eb.i-ps[6] eb.carrier eb.carr-dscr eb.i-code[6] eb.i-dscr[7] ~
eb.i-%[7] eb.i-ps[7] eb.i-code[7] eb.weight-m eb.i-dscr[8] eb.i-%[8] ~
eb.i-ps[8] eb.i-code[8] eb.chg-method eb.dest-code eb.i-dscr[9] eb.i-%[9] ~
eb.i-ps[9] eb.i-code[9] eb.fr-out-c eb.i-dscr[10] eb.i-%[10] eb.i-ps[10] ~
eb.i-code[10] eb.fr-out-m 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}i-col ~{&FP2}i-col ~{&FP3}~
 ~{&FP1}i-pass ~{&FP2}i-pass ~{&FP3}~
 ~{&FP1}i-coat ~{&FP2}i-coat ~{&FP3}~
 ~{&FP1}i-coat-p ~{&FP2}i-coat-p ~{&FP3}~
 ~{&FP1}cas-no ~{&FP2}cas-no ~{&FP3}~
 ~{&FP1}tr-no ~{&FP2}tr-no ~{&FP3}~
 ~{&FP1}i-coldscr ~{&FP2}i-coldscr ~{&FP3}~
 ~{&FP1}cas-cost ~{&FP2}cas-cost ~{&FP3}~
 ~{&FP1}tr-cost ~{&FP2}tr-cost ~{&FP3}~
 ~{&FP1}cas-cnt ~{&FP2}cas-cnt ~{&FP3}~
 ~{&FP1}tr-cnt ~{&FP2}tr-cnt ~{&FP3}~
 ~{&FP1}cas-len ~{&FP2}cas-len ~{&FP3}~
 ~{&FP1}tr-len ~{&FP2}tr-len ~{&FP3}~
 ~{&FP1}i-ps[1] ~{&FP2}i-ps[1] ~{&FP3}~
 ~{&FP1}i-code[1] ~{&FP2}i-code[1] ~{&FP3}~
 ~{&FP1}i-dscr[1] ~{&FP2}i-dscr[1] ~{&FP3}~
 ~{&FP1}i-%[1] ~{&FP2}i-%[1] ~{&FP3}~
 ~{&FP1}cas-wid ~{&FP2}cas-wid ~{&FP3}~
 ~{&FP1}tr-wid ~{&FP2}tr-wid ~{&FP3}~
 ~{&FP1}i-dscr[2] ~{&FP2}i-dscr[2] ~{&FP3}~
 ~{&FP1}i-%[2] ~{&FP2}i-%[2] ~{&FP3}~
 ~{&FP1}i-ps[2] ~{&FP2}i-ps[2] ~{&FP3}~
 ~{&FP1}i-code[2] ~{&FP2}i-code[2] ~{&FP3}~
 ~{&FP1}cas-dep ~{&FP2}cas-dep ~{&FP3}~
 ~{&FP1}tr-dep ~{&FP2}tr-dep ~{&FP3}~
 ~{&FP1}i-dscr[3] ~{&FP2}i-dscr[3] ~{&FP3}~
 ~{&FP1}i-%[3] ~{&FP2}i-%[3] ~{&FP3}~
 ~{&FP1}i-ps[3] ~{&FP2}i-ps[3] ~{&FP3}~
 ~{&FP1}i-code[3] ~{&FP2}i-code[3] ~{&FP3}~
 ~{&FP1}cas-pal ~{&FP2}cas-pal ~{&FP3}~
 ~{&FP1}tr-cas ~{&FP2}tr-cas ~{&FP3}~
 ~{&FP1}i-dscr[4] ~{&FP2}i-dscr[4] ~{&FP3}~
 ~{&FP1}i-%[4] ~{&FP2}i-%[4] ~{&FP3}~
 ~{&FP1}i-ps[4] ~{&FP2}i-ps[4] ~{&FP3}~
 ~{&FP1}i-code[4] ~{&FP2}i-code[4] ~{&FP3}~
 ~{&FP1}cas-wt ~{&FP2}cas-wt ~{&FP3}~
 ~{&FP1}i-dscr[5] ~{&FP2}i-dscr[5] ~{&FP3}~
 ~{&FP1}i-%[5] ~{&FP2}i-%[5] ~{&FP3}~
 ~{&FP1}i-ps[5] ~{&FP2}i-ps[5] ~{&FP3}~
 ~{&FP1}i-code[5] ~{&FP2}i-code[5] ~{&FP3}~
 ~{&FP1}i-dscr[6] ~{&FP2}i-dscr[6] ~{&FP3}~
 ~{&FP1}i-%[6] ~{&FP2}i-%[6] ~{&FP3}~
 ~{&FP1}i-ps[6] ~{&FP2}i-ps[6] ~{&FP3}~
 ~{&FP1}carrier ~{&FP2}carrier ~{&FP3}~
 ~{&FP1}carr-dscr ~{&FP2}carr-dscr ~{&FP3}~
 ~{&FP1}i-code[6] ~{&FP2}i-code[6] ~{&FP3}~
 ~{&FP1}i-dscr[7] ~{&FP2}i-dscr[7] ~{&FP3}~
 ~{&FP1}i-%[7] ~{&FP2}i-%[7] ~{&FP3}~
 ~{&FP1}i-ps[7] ~{&FP2}i-ps[7] ~{&FP3}~
 ~{&FP1}i-code[7] ~{&FP2}i-code[7] ~{&FP3}~
 ~{&FP1}weight-m ~{&FP2}weight-m ~{&FP3}~
 ~{&FP1}i-dscr[8] ~{&FP2}i-dscr[8] ~{&FP3}~
 ~{&FP1}i-%[8] ~{&FP2}i-%[8] ~{&FP3}~
 ~{&FP1}i-ps[8] ~{&FP2}i-ps[8] ~{&FP3}~
 ~{&FP1}i-code[8] ~{&FP2}i-code[8] ~{&FP3}~
 ~{&FP1}dest-code ~{&FP2}dest-code ~{&FP3}~
 ~{&FP1}i-dscr[9] ~{&FP2}i-dscr[9] ~{&FP3}~
 ~{&FP1}i-%[9] ~{&FP2}i-%[9] ~{&FP3}~
 ~{&FP1}i-ps[9] ~{&FP2}i-ps[9] ~{&FP3}~
 ~{&FP1}i-code[9] ~{&FP2}i-code[9] ~{&FP3}~
 ~{&FP1}fr-out-c ~{&FP2}fr-out-c ~{&FP3}~
 ~{&FP1}i-dscr[10] ~{&FP2}i-dscr[10] ~{&FP3}~
 ~{&FP1}i-%[10] ~{&FP2}i-%[10] ~{&FP3}~
 ~{&FP1}i-ps[10] ~{&FP2}i-ps[10] ~{&FP3}~
 ~{&FP1}i-code[10] ~{&FP2}i-code[10] ~{&FP3}~
 ~{&FP1}fr-out-m ~{&FP2}fr-out-m ~{&FP3}
&Scoped-define ENABLED-TABLES eb
&Scoped-define FIRST-ENABLED-TABLE eb
&Scoped-Define ENABLED-OBJECTS RECT-27 RECT-26 RECT-28 
&Scoped-Define DISPLAYED-FIELDS eb.i-col eb.i-pass eb.i-coat eb.i-coat-p ~
eb.cas-no eb.tr-no eb.i-coldscr eb.cas-cost eb.tr-cost eb.cas-cnt eb.tr-cnt ~
eb.cas-len eb.tr-len eb.i-ps[1] eb.i-code[1] eb.i-dscr[1] eb.i-%[1] ~
eb.cas-wid eb.tr-wid eb.i-dscr[2] eb.i-%[2] eb.i-ps[2] eb.i-code[2] ~
eb.cas-dep eb.tr-dep eb.i-dscr[3] eb.i-%[3] eb.i-ps[3] eb.i-code[3] ~
eb.cas-pal eb.tr-cas eb.i-dscr[4] eb.i-%[4] eb.i-ps[4] eb.i-code[4] ~
eb.cas-wt eb.i-dscr[5] eb.i-%[5] eb.i-ps[5] eb.i-code[5] eb.i-dscr[6] ~
eb.i-%[6] eb.i-ps[6] eb.carrier eb.carr-dscr eb.i-code[6] eb.i-dscr[7] ~
eb.i-%[7] eb.i-ps[7] eb.i-code[7] eb.weight-m eb.i-dscr[8] eb.i-%[8] ~
eb.i-ps[8] eb.i-code[8] eb.chg-method eb.dest-code eb.i-dscr[9] eb.i-%[9] ~
eb.i-ps[9] eb.i-code[9] eb.fr-out-c eb.i-dscr[10] eb.i-%[10] eb.i-ps[10] ~
eb.i-code[10] eb.fr-out-m 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */

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
DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 69 BY 15.24.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 24 BY 4.52.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 73 BY 15.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     eb.i-col AT ROW 1.71 COL 9 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-pass AT ROW 1.71 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-coat AT ROW 1.71 COL 42 COLON-ALIGNED
          LABEL "Coatings"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-coat-p AT ROW 1.71 COL 57 COLON-ALIGNED
          LABEL "Passes"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.cas-no AT ROW 1.71 COL 95 COLON-ALIGNED
          LABEL "Packing Code"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     eb.tr-no AT ROW 1.71 COL 121 COLON-ALIGNED
          LABEL "Unit #"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.i-coldscr AT ROW 2.67 COL 9 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     eb.cas-cost AT ROW 2.67 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     eb.tr-cost AT ROW 2.67 COL 121 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     eb.cas-cnt AT ROW 3.62 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     eb.tr-cnt AT ROW 3.62 COL 121 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     eb.cas-len AT ROW 4.57 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     eb.tr-len AT ROW 4.57 COL 121 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     eb.i-ps[1] AT ROW 4.81 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[1] AT ROW 4.81 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[1] AT ROW 4.81 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[1] AT ROW 4.81 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.cas-wid AT ROW 5.52 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     eb.tr-wid AT ROW 5.57 COL 121 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     eb.i-dscr[2] AT ROW 5.76 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[2] AT ROW 5.76 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[2] AT ROW 5.81 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[2] AT ROW 5.81 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.cas-dep AT ROW 6.48 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .95
     eb.tr-dep AT ROW 6.48 COL 121 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     eb.i-dscr[3] AT ROW 6.76 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[3] AT ROW 6.76 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[3] AT ROW 6.81 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[3] AT ROW 6.81 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.cas-pal AT ROW 7.43 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     eb.tr-cas AT ROW 7.43 COL 121 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     eb.i-dscr[4] AT ROW 7.76 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[4] AT ROW 7.76 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[4] AT ROW 7.81 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[4] AT ROW 7.91 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.cas-wt AT ROW 8.38 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     eb.i-dscr[5] AT ROW 8.76 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[5] AT ROW 8.76 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[5] AT ROW 8.81 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[5] AT ROW 8.91 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[6] AT ROW 9.76 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[6] AT ROW 9.76 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[6] AT ROW 9.81 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.carrier AT ROW 9.81 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     eb.carr-dscr AT ROW 9.81 COL 106 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     eb.i-code[6] AT ROW 9.91 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[7] AT ROW 10.76 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[7] AT ROW 10.76 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[7] AT ROW 10.81 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[7] AT ROW 10.91 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.weight-m AT ROW 11.24 COL 96 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     eb.i-dscr[8] AT ROW 11.76 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[8] AT ROW 11.76 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[8] AT ROW 11.81 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[8] AT ROW 11.91 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.chg-method AT ROW 11.95 COL 118 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Prepaid", "P":U,
"Collect", "C":U,
"Bill", "B":U,
"Third Party", "T":U
          SIZE 21 BY 3.57
     eb.dest-code AT ROW 12.19 COL 96 COLON-ALIGNED
          LABEL "Delivery Zone"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     eb.i-dscr[9] AT ROW 12.76 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     eb.i-%[9] AT ROW 12.76 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[9] AT ROW 12.81 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[9] AT ROW 12.91 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.fr-out-c AT ROW 13.14 COL 96 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     eb.i-dscr[10] AT ROW 13.76 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
     eb.i-%[10] AT ROW 13.76 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[10] AT ROW 13.86 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[10] AT ROW 13.91 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.fr-out-m AT ROW 14.1 COL 96 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     "Freight Charge" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.24 COL 120
          FGCOLOR 9 
     "Code" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 4.1 COL 15
     "PS" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 4.1 COL 4
     "Description" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 4.1 COL 36
     RECT-27 AT ROW 11.48 COL 117
     "%" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 4.1 COL 63
     RECT-26 AT ROW 1.24 COL 1
     RECT-28 AT ROW 1.24 COL 73
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.eb
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 17.14
         WIDTH              = 145.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN eb.cas-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.dest-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.i-coat IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.i-coat-p IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.i-coldscr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.tr-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
    def var lv-ind like style.industry no-undo.
    def var lv-handle as handle no-undo.
    def var char-val as cha no-undo.    
    lv-handle = focus:handle.

    case focus:name :
         when "i-code" then do:
             find style where style.company = eb.company and
                            style.style = eb.style
                            no-lock no-error.   
             if avail style then lv-ind = style.industry.
             else lv-ind = "".  
             run windows/l-item2.w (eb.company, lv-ind, "I",focus:screen-value, output char-val).
             if char-val <> "" then do:
                  assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                         .
                  case focus:index:        
                       when 1 then eb.i-dscr[1]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 2 then eb.i-dscr[2]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 3 then eb.i-dscr[3]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 4 then eb.i-dscr[4]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 5 then eb.i-dscr[5]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 6 then eb.i-dscr[6]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 7 then eb.i-dscr[7]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 8 then eb.i-dscr[8]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 9 then eb.i-dscr[9]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 10 then eb.i-dscr[10]:screen-value in frame {&frame-name} = entry(2,char-val) .
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
           if char-val <> "" then do:
              find item where item.company = eb.company and
                              item.i-no = entry(1,char-val)
                              no-lock no-error.
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
              if avail item then assign /*eb.cas-cost:Screen-value = */
                                        eb.cas-cnt:Screen-value = string(item.box-case)
                                        eb.cas-len:Screen-value = string(item.case-l)
                                        eb.cas-wid:Screen-value = string(item.case-w)
                                        eb.cas-dep:Screen-value = string(item.case-d)
                                        eb.cas-pal:Screen-value = string(item.case-pall)
                                        eb.cas-wt:Screen-value = string(item.avg-w)         
                                        .
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
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
              if avail item then assign /*eb.cas-cost:Screen-value = */
                                        eb.tr-len:Screen-value = string(item.case-l)
                                        eb.tr-wid:Screen-value = string(item.case-w)
                                        eb.tr-dep:Screen-value = string(item.case-d)
                                        .
           end.
           return no-apply.   
     end.   
     when "carrier" then do:
             run windows/l-carrier.w  
                 (eb.company,eb.loc,focus:screen-value, output char-val).
             if char-val <> "" then
                assign eb.carrier:screen-value in frame {&frame-name} = entry(1,char-val)
                       eb.carr-dscr:screen-value in frame {&frame-name} = entry(2,char-val)
                       .
             return no-apply.              

        end.



    end case.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.carrier V-table-Win
ON LEAVE OF eb.carrier IN FRAME F-Main /* Carrier */
DO:
    if lastkey <> -1 and eb.carrier:screen-value <> "" and
       not can-find(first carrier where carrier.carrier = eb.carrier:screen-value)
    then do:
     {&methods/lValidateError.i YES}
         message "Invalid Carrier. Try Help." view-as alert-box error.
         return no-apply.
    {&methods/lValidateError.i NO}
    end.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cas-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cas-no V-table-Win
ON LEAVE OF eb.cas-no IN FRAME F-Main /* Packing Code */
DO:

              find item where item.company = eb.company and
                              item.i-no = eb.cas-no:screen-value
                              no-lock no-error.
              if avail item then assign /*eb.cas-cost:Screen-value = */
                                        eb.cas-cnt:Screen-value = string(item.box-case)
                                        eb.cas-len:Screen-value = string(item.case-l)
                                        eb.cas-wid:Screen-value = string(item.case-w)
                                        eb.cas-dep:Screen-value = string(item.case-d)
                                        eb.cas-pal:Screen-value = string(item.case-pall)
                                        eb.cas-wt:Screen-value = string(item.avg-w)         
                                        .
             else if lastkey <> -1 and eb.cas-no:screen-value <> "" then do:
             {&methods/lValidateError.i YES}
                  message "Invalid Packing Code. Try Help." view-as alert-box.
                  return no-apply.
             {&methods/lValidateError.i NO}
             end.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.chg-method
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.chg-method V-table-Win
ON return OF eb.chg-method IN FRAME F-Main /* chg-method */
DO:
  apply "tab" to self.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[10] V-table-Win
ON LEAVE OF eb.i-code[10] IN FRAME F-Main /* Code[10] */
DO:  
   if eb.i-ps[10]:screen-value <> "0" and lastkey <> -1  then do:
    {&methods/lValidateError.i YES}
     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.

     if avail item then  eb.i-dscr[10]:screen-value in frame {&frame-name} = item.i-name.
  {&methods/lValidateError.i NO}  
  end.  
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[1] V-table-Win
ON LEAVE OF eb.i-code[1] IN FRAME F-Main /* Code[1] */
DO:  
  if eb.i-ps[1]:screen-value <> "0" and lastkey <> -1  then do:
   {&methods/lValidateError.i YES}
     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.

     if avail item then  eb.i-dscr[1]:screen-value in frame {&frame-name} = item.i-name.
  {&methods/lValidateError.i NO}
  end.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[2] V-table-Win
ON LEAVE OF eb.i-code[2] IN FRAME F-Main /* Code[2] */
DO:
  if eb.i-ps[2]:screen-value <> "0" and lastkey <> -1  then do:
   {&methods/lValidateError.i YES}
     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.

     if avail item then  eb.i-dscr[2]:screen-value in frame {&frame-name} = item.i-name.
  {&methods/lValidateError.i NO}
  end.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[3] V-table-Win
ON LEAVE OF eb.i-code[3] IN FRAME F-Main /* Code[3] */
DO:
  if eb.i-ps[3]:screen-value <> "0" and lastkey <> -1  then do:
    {&methods/lValidateError.i YES}
     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.

     if avail item then  eb.i-dscr[3]:screen-value in frame {&frame-name} = item.i-name.
  {&methods/lValidateError.i NO}
  end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[4] V-table-Win
ON LEAVE OF eb.i-code[4] IN FRAME F-Main /* Code[4] */
DO:
  if eb.i-ps[4]:screen-value <> "0" and lastkey <> -1  then do:
   {&methods/lValidateError.i YES}
     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.

     if avail item then  eb.i-dscr[4]:screen-value in frame {&frame-name} = item.i-name.
   {&methods/lValidateError.i NO}
  end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[5] V-table-Win
ON LEAVE OF eb.i-code[5] IN FRAME F-Main /* Code[5] */
DO:
  if eb.i-ps[5]:screen-value <> "0" and lastkey <> -1  then do:
   {&methods/lValidateError.i YES}

     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.

     if avail item then  eb.i-dscr[5]:screen-value in frame {&frame-name} = item.i-name.
  {&methods/lValidateError.i NO}
  end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[6] V-table-Win
ON LEAVE OF eb.i-code[6] IN FRAME F-Main /* Code[6] */
DO:
  if eb.i-ps[6]:screen-value <> "0" and lastkey <> -1  then do:
  {&methods/lValidateError.i YES}
     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.

     if avail item then  eb.i-dscr[6]:screen-value in frame {&frame-name} = item.i-name.
   {&methods/lValidateError.i NO}
  end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[7] V-table-Win
ON LEAVE OF eb.i-code[7] IN FRAME F-Main /* Code[7] */
DO:
  if eb.i-ps[7]:screen-value <> "0" and lastkey <> -1  then do:
   {&methods/lValidateError.i YES}
     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.

     if avail item then  eb.i-dscr[7]:screen-value in frame {&frame-name} = item.i-name.
   {&methods/lValidateError.i NO}
  end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[8] V-table-Win
ON LEAVE OF eb.i-code[8] IN FRAME F-Main /* Code[8] */
DO:
  if eb.i-ps[8]:screen-value <> "0" and lastkey <> -1  then do:
   {&methods/lValidateError.i YES}
     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.

     if avail item then  eb.i-dscr[8]:screen-value in frame {&frame-name} = item.i-name.
  {&methods/lValidateError.i NO}
  end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[9] V-table-Win
ON LEAVE OF eb.i-code[9] IN FRAME F-Main /* Code[9] */
DO:
  if eb.i-ps[9]:screen-value <> "0" and lastkey <> -1  then do:
  {&methods/lValidateError.i YES}
     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.

     if avail item then  eb.i-dscr[9]:screen-value in frame {&frame-name} = item.i-name.
  {&methods/lValidateError.i NO}
  end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.tr-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.tr-no V-table-Win
ON LEAVE OF eb.tr-no IN FRAME F-Main /* Unit # */
DO:
              find item where item.company = eb.company and
                              item.i-no = eb.tr-no:screen-value
                              no-lock no-error.
              if avail item then assign /*eb.cas-cost:Screen-value = */
                                        eb.tr-len:Screen-value = string(item.case-l)
                                        eb.tr-wid:Screen-value = string(item.case-w)
                                        eb.tr-dep:Screen-value = string(item.case-d)
                                        .
              else if lastkey <> -1 and eb.tr-no:screen-value <> "" 
              then do:   
               {&methods/lValidateError.i YES}           
                   message "Invalid Unit#. Try Help." view-as alert-box error.
                   return no-apply.
              {&methods/lValidateError.i NO}
              end.                          
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "eb"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "eb"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var li-num-of-code as int no-undo.   

  /* Code placed here will execute PRIOR to standard behavior. */
     li-num-of-code = 0.
     if eb.i-code[1]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[2]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[3]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[4]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[5]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[6]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[7]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[8]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[9]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[10]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
   {&methods/lValidateError.i YES}
     if li-num-of-code <> (integer(eb.i-col:screen-value) + 
                          integer(eb.i-coat:screen-value) )
     then do:
          message "Invalid Number of Color and Coating." view-as alert-box.
          apply "entry" to eb.i-col.
          return no-apply.
     end.                     
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
   {&methods/lValidateError.i NO}
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "eb"}

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
  ll-update-pack = yes.

 enable   eb.cas-no eb.cas-cost eb.cas-cnt eb.cas-len eb.cas-wid
          eb.cas-dep eb.cas-pal eb.cas-wt
          eb.tr-no eb.tr-cost eb.tr-cnt eb.tr-len eb.tr-wid eb.tr-dep
          eb.tr-cas
          eb.carrier eb.carr-dscr eb.weight-m eb.dest-code
          eb.fr-out-c eb.fr-out-m eb.chg-method
          with frame {&frame-name}.

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


