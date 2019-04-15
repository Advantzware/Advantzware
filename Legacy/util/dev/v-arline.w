&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE PARAMETER BUFFER ar-invl FOR ar-invl.
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ar-invl.actnum ar-invl.sname[1] ~
ar-invl.pr-qty-uom ar-invl.amt ar-invl.sname[2] ar-invl.pr-uom ~
ar-invl.amt-msf ar-invl.inv-qty ar-invl.sname[3] ar-invl.prep ar-invl.b-no ~
ar-invl.job-no ar-invl.std-fix-cost ar-invl.prep-amt ar-invl.billable ~
ar-invl.job-no2 ar-invl.std-lab-cost ar-invl.prep-charge ar-invl.blank-no ~
ar-invl.lab ar-invl.std-mat-cost ar-invl.prep-code ar-invl.bol-no ~
ar-invl.line ar-invl.std-tot-cost ar-invl.prep-cost ar-invl.cas-cnt ~
ar-invl.loc ar-invl.std-var-cost ar-invl.prep-dscr ar-invl.company ~
ar-invl.matl ar-invl.t-cost ar-invl.prom-code ar-invl.cons-uom ar-invl.misc ~
ar-invl.prom-date ar-invl.cost ar-invl.t-freight ar-invl.ord-line ~
ar-invl.qty ar-invl.cust-no ar-invl.t-freight-p ar-invl.ord-no ~
ar-invl.rec_key ar-invl.disc ar-invl.t-fuel ar-invl.p-c ar-invl.e-num ~
ar-invl.req-code ar-invl.t-weight ar-invl.pal-chg ar-invl.est-no ~
ar-invl.req-date ar-invl.tax ar-invl.pal-i-no ar-invl.est-type ~
ar-invl.unit-pr ar-invl.s-pct[1] ar-invl.part-no ar-invl.dscr[1] ~
ar-invl.upd-date ar-invl.s-pct[2] ar-invl.period ar-invl.dscr[2] ~
ar-invl.upd-time ar-invl.s-pct[3] ar-invl.po-no ar-invl.i-dscr ~
ar-invl.varoh ar-invl.sf-sht ar-invl.po-no-po ar-invl.fixoh ar-invl.x-no ~
ar-invl.ship-qty ar-invl.part-dscr2 ar-invl.form-no ar-invl.sman[1] ~
ar-invl.part-dscr1 ar-invl.i-name ar-invl.sman[2] ar-invl.i-no ~
ar-invl.po-no2 ar-invl.sman[3] ar-invl.inv-date ar-invl.posted ~
ar-invl.inv-i-no 
&Scoped-define ENABLED-TABLES ar-invl
&Scoped-define FIRST-ENABLED-TABLE ar-invl
&Scoped-Define ENABLED-OBJECTS RECT-19 btn-update btn-cancel btn-del ~
btn-exit 
&Scoped-Define DISPLAYED-FIELDS ar-invl.actnum ar-invl.inv-line ~
ar-invl.sname[1] ar-invl.pr-qty-uom ar-invl.amt ar-invl.inv-no ~
ar-invl.sname[2] ar-invl.pr-uom ar-invl.amt-msf ar-invl.inv-qty ~
ar-invl.sname[3] ar-invl.prep ar-invl.b-no ar-invl.job-no ~
ar-invl.std-fix-cost ar-invl.prep-amt ar-invl.billable ar-invl.job-no2 ~
ar-invl.std-lab-cost ar-invl.prep-charge ar-invl.blank-no ar-invl.lab ~
ar-invl.std-mat-cost ar-invl.prep-code ar-invl.bol-no ar-invl.line ~
ar-invl.std-tot-cost ar-invl.prep-cost ar-invl.cas-cnt ar-invl.loc ~
ar-invl.std-var-cost ar-invl.prep-dscr ar-invl.company ar-invl.matl ~
ar-invl.t-cost ar-invl.prom-code ar-invl.cons-uom ar-invl.misc ~
ar-invl.prom-date ar-invl.cost ar-invl.t-freight ar-invl.ord-line ~
ar-invl.qty ar-invl.cust-no ar-invl.t-freight-p ar-invl.ord-no ~
ar-invl.rec_key ar-invl.disc ar-invl.t-fuel ar-invl.p-c ar-invl.e-num ~
ar-invl.req-code ar-invl.t-weight ar-invl.pal-chg ar-invl.est-no ~
ar-invl.req-date ar-invl.tax ar-invl.pal-i-no ar-invl.est-type ~
ar-invl.unit-pr ar-invl.s-pct[1] ar-invl.part-no ar-invl.dscr[1] ~
ar-invl.upd-date ar-invl.s-pct[2] ar-invl.period ar-invl.dscr[2] ~
ar-invl.upd-time ar-invl.s-pct[3] ar-invl.po-no ar-invl.i-dscr ~
ar-invl.varoh ar-invl.sf-sht ar-invl.po-no-po ar-invl.fixoh ar-invl.x-no ~
ar-invl.ship-qty ar-invl.part-dscr2 ar-invl.form-no ar-invl.sman[1] ~
ar-invl.part-dscr1 ar-invl.i-name ar-invl.sman[2] ar-invl.i-no ~
ar-invl.po-no2 ar-invl.sman[3] ar-invl.inv-date ar-invl.posted ~
ar-invl.inv-i-no 
&Scoped-define DISPLAYED-TABLES ar-invl
&Scoped-define FIRST-DISPLAYED-TABLE ar-invl


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "C&ancel" 
     SIZE 22 BY 1.19.

DEFINE BUTTON btn-del 
     LABEL "D&elete" 
     SIZE 22 BY 1.19.

DEFINE BUTTON btn-exit 
     LABEL "E&xit" 
     SIZE 22 BY 1.19.

DEFINE BUTTON btn-update 
     LABEL "U&pdate" 
     SIZE 22 BY 1.19.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 172 BY 24.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     ar-invl.actnum AT ROW 2.43 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     ar-invl.inv-line AT ROW 2.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     ar-invl.sname[1] AT ROW 3.14 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ar-invl.pr-qty-uom AT ROW 3.38 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     ar-invl.amt AT ROW 3.43 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     ar-invl.inv-no AT ROW 3.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ar-invl.sname[2] AT ROW 4.14 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ar-invl.pr-uom AT ROW 4.33 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     ar-invl.amt-msf AT ROW 4.43 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     ar-invl.inv-qty AT ROW 4.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     ar-invl.sname[3] AT ROW 5.14 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ar-invl.prep AT ROW 5.33 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     ar-invl.b-no AT ROW 5.43 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ar-invl.job-no AT ROW 5.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ar-invl.std-fix-cost AT ROW 6.14 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     ar-invl.prep-amt AT ROW 6.33 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     ar-invl.billable AT ROW 6.43 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     ar-invl.job-no2 AT ROW 6.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     ar-invl.std-lab-cost AT ROW 7.14 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     ar-invl.prep-charge AT ROW 7.33 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ar-invl.blank-no AT ROW 7.43 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     ar-invl.lab AT ROW 7.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     ar-invl.std-mat-cost AT ROW 8.14 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     ar-invl.prep-code AT ROW 8.33 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     ar-invl.bol-no AT ROW 8.43 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     ar-invl.line AT ROW 8.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     ar-invl.std-tot-cost AT ROW 9.14 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     ar-invl.prep-cost AT ROW 9.33 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-invl.cas-cnt AT ROW 9.43 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     ar-invl.loc AT ROW 9.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     ar-invl.std-var-cost AT ROW 10.14 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 180.6 BY 28.76.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     ar-invl.prep-dscr AT ROW 10.33 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ar-invl.company AT ROW 10.43 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     ar-invl.matl AT ROW 10.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     ar-invl.t-cost AT ROW 11.14 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     ar-invl.prom-code AT ROW 11.33 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     ar-invl.cons-uom AT ROW 11.43 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     ar-invl.misc AT ROW 11.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     ar-invl.prom-date AT ROW 12.33 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-invl.cost AT ROW 12.43 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     ar-invl.t-freight AT ROW 12.43 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-invl.ord-line AT ROW 12.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     ar-invl.qty AT ROW 13.33 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     ar-invl.cust-no AT ROW 13.43 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     ar-invl.t-freight-p AT ROW 13.43 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     ar-invl.ord-no AT ROW 13.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ar-invl.rec_key AT ROW 14.33 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ar-invl.disc AT ROW 14.43 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     ar-invl.t-fuel AT ROW 14.43 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-invl.p-c AT ROW 14.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     ar-invl.e-num AT ROW 15.29 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ar-invl.req-code AT ROW 15.33 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     ar-invl.t-weight AT ROW 15.43 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-invl.pal-chg AT ROW 15.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     ar-invl.est-no AT ROW 16.24 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     ar-invl.req-date AT ROW 16.33 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-invl.tax AT ROW 16.43 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     ar-invl.pal-i-no AT ROW 16.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     ar-invl.est-type AT ROW 17.19 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.4 BY 1
     ar-invl.unit-pr AT ROW 17.43 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 180.6 BY 28.76.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     ar-invl.s-pct[1] AT ROW 17.52 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ar-invl.part-no AT ROW 17.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ar-invl.dscr[1] AT ROW 18.14 COL 24 COLON-ALIGNED
          LABEL "Cost UOM"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ar-invl.upd-date AT ROW 18.43 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-invl.s-pct[2] AT ROW 18.52 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ar-invl.period AT ROW 18.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     ar-invl.dscr[2] AT ROW 19.1 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     ar-invl.upd-time AT ROW 19.43 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-invl.s-pct[3] AT ROW 19.52 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ar-invl.po-no AT ROW 19.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ar-invl.i-dscr AT ROW 20.05 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ar-invl.varoh AT ROW 20.43 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     ar-invl.sf-sht AT ROW 20.52 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ar-invl.po-no-po AT ROW 20.67 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ar-invl.fixoh AT ROW 21 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     ar-invl.x-no AT ROW 21.43 COL 153 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     ar-invl.ship-qty AT ROW 21.57 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     ar-invl.part-dscr2 AT ROW 21.86 COL 63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ar-invl.form-no AT ROW 21.95 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     ar-invl.sman[1] AT ROW 22.67 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     ar-invl.part-dscr1 AT ROW 22.81 COL 63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ar-invl.i-name AT ROW 22.91 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ar-invl.sman[2] AT ROW 23.67 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     ar-invl.i-no AT ROW 23.86 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ar-invl.po-no2 AT ROW 23.86 COL 63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ar-invl.sman[3] AT ROW 24.67 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     ar-invl.inv-date AT ROW 24.81 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ar-invl.posted AT ROW 24.86 COL 63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 180.6 BY 28.76.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     ar-invl.inv-i-no AT ROW 25.76 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     btn-update AT ROW 27.67 COL 41
     btn-cancel AT ROW 27.67 COL 63
     btn-del AT ROW 27.67 COL 85
     btn-exit AT ROW 27.67 COL 107
     "AR Line Invoice Repair Utility" VIEW-AS TEXT
          SIZE 35 BY 1.19 AT ROW 1 COL 77
     RECT-19 AT ROW 2.19 COL 7
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 180.6 BY 28.76.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 28.76
         WIDTH              = 180.6
         MAX-HEIGHT         = 28.76
         MAX-WIDTH          = 180.6
         VIRTUAL-HEIGHT     = 28.76
         VIRTUAL-WIDTH      = 180.6
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
                                                                        */
/* SETTINGS FOR FILL-IN ar-invl.dscr[1] IN FRAME fMain
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-invl.inv-line IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ar-invl.inv-no IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME fMain /* Cancel */
DO:
   APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-del C-Win
ON CHOOSE OF btn-del IN FRAME fMain /* Delete */
DO:
    MESSAGE "Deletion of AR lines in NOT Allowed"
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-exit C-Win
ON CHOOSE OF btn-exit IN FRAME fMain /* Exit */
DO:
   APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-update C-Win
ON CHOOSE OF btn-update IN FRAME fMain /* Update */
DO:
  RUN update-ar-invl.
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

  DISPLAY ar-invl.inv-no 
          ar-invl.i-no
          WITH FRAME fmain.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  IF AVAILABLE ar-invl THEN 
    DISPLAY ar-invl.actnum ar-invl.inv-line ar-invl.sname[1] ar-invl.pr-qty-uom 
          ar-invl.amt ar-invl.inv-no ar-invl.sname[2] ar-invl.pr-uom 
          ar-invl.amt-msf ar-invl.inv-qty ar-invl.sname[3] ar-invl.prep 
          ar-invl.b-no ar-invl.job-no ar-invl.std-fix-cost ar-invl.prep-amt 
          ar-invl.billable ar-invl.job-no2 ar-invl.std-lab-cost 
          ar-invl.prep-charge ar-invl.blank-no ar-invl.lab ar-invl.std-mat-cost 
          ar-invl.prep-code ar-invl.bol-no ar-invl.line ar-invl.std-tot-cost 
          ar-invl.prep-cost ar-invl.cas-cnt ar-invl.loc ar-invl.std-var-cost 
          ar-invl.prep-dscr ar-invl.company ar-invl.matl ar-invl.t-cost 
          ar-invl.prom-code ar-invl.cons-uom ar-invl.misc ar-invl.prom-date 
          ar-invl.cost ar-invl.t-freight ar-invl.ord-line ar-invl.qty 
          ar-invl.cust-no ar-invl.t-freight-p ar-invl.ord-no ar-invl.rec_key 
          ar-invl.disc ar-invl.t-fuel ar-invl.p-c ar-invl.e-num ar-invl.req-code 
          ar-invl.t-weight ar-invl.pal-chg ar-invl.est-no ar-invl.req-date 
          ar-invl.tax ar-invl.pal-i-no ar-invl.est-type ar-invl.unit-pr 
          ar-invl.s-pct[1] ar-invl.part-no ar-invl.dscr[1] ar-invl.upd-date 
          ar-invl.s-pct[2] ar-invl.period ar-invl.dscr[2] ar-invl.upd-time 
          ar-invl.s-pct[3] ar-invl.po-no ar-invl.i-dscr ar-invl.varoh 
          ar-invl.sf-sht ar-invl.po-no-po ar-invl.fixoh ar-invl.x-no 
          ar-invl.ship-qty ar-invl.part-dscr2 ar-invl.form-no ar-invl.sman[1] 
          ar-invl.part-dscr1 ar-invl.i-name ar-invl.sman[2] ar-invl.i-no 
          ar-invl.po-no2 ar-invl.sman[3] ar-invl.inv-date ar-invl.posted 
          ar-invl.inv-i-no 
      WITH FRAME fMain IN WINDOW C-Win.
  ENABLE RECT-19 ar-invl.actnum ar-invl.sname[1] ar-invl.pr-qty-uom ar-invl.amt 
         ar-invl.sname[2] ar-invl.pr-uom ar-invl.amt-msf ar-invl.inv-qty 
         ar-invl.sname[3] ar-invl.prep ar-invl.b-no ar-invl.job-no 
         ar-invl.std-fix-cost ar-invl.prep-amt ar-invl.billable ar-invl.job-no2 
         ar-invl.std-lab-cost ar-invl.prep-charge ar-invl.blank-no ar-invl.lab 
         ar-invl.std-mat-cost ar-invl.prep-code ar-invl.bol-no ar-invl.line 
         ar-invl.std-tot-cost ar-invl.prep-cost ar-invl.cas-cnt ar-invl.loc 
         ar-invl.std-var-cost ar-invl.prep-dscr ar-invl.company ar-invl.matl 
         ar-invl.t-cost ar-invl.prom-code ar-invl.cons-uom ar-invl.misc 
         ar-invl.prom-date ar-invl.cost ar-invl.t-freight ar-invl.ord-line 
         ar-invl.qty ar-invl.cust-no ar-invl.t-freight-p ar-invl.ord-no 
         ar-invl.rec_key ar-invl.disc ar-invl.t-fuel ar-invl.p-c ar-invl.e-num 
         ar-invl.req-code ar-invl.t-weight ar-invl.pal-chg ar-invl.est-no 
         ar-invl.req-date ar-invl.tax ar-invl.pal-i-no ar-invl.est-type 
         ar-invl.unit-pr ar-invl.s-pct[1] ar-invl.part-no ar-invl.dscr[1] 
         ar-invl.upd-date ar-invl.s-pct[2] ar-invl.period ar-invl.dscr[2] 
         ar-invl.upd-time ar-invl.s-pct[3] ar-invl.po-no ar-invl.i-dscr 
         ar-invl.varoh ar-invl.sf-sht ar-invl.po-no-po ar-invl.fixoh 
         ar-invl.x-no ar-invl.ship-qty ar-invl.part-dscr2 ar-invl.form-no 
         ar-invl.sman[1] ar-invl.part-dscr1 ar-invl.i-name ar-invl.sman[2] 
         ar-invl.i-no ar-invl.po-no2 ar-invl.sman[3] ar-invl.inv-date 
         ar-invl.posted ar-invl.inv-i-no btn-update btn-cancel btn-del btn-exit 
      WITH FRAME fMain IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-ar-invl C-Win 
PROCEDURE update-ar-invl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* 07/11/08 COPY assignment */

DO WITH FRAME fmain:
  
    FIND CURRENT ar-invl EXCLUSIVE-LOCK NO-ERROR.

    ASSIGN

    ar-invl.prep-code      = ar-invl.prep-code:SCREEN-VALUE
    ar-invl.ord-no         = int(ar-invl.ord-no:SCREEN-VALUE)
    ar-invl.est-no         = ar-invl.est-no:SCREEN-VALUE
    ar-invl.part-no        = ar-invl.part-no:SCREEN-VALUE
    ar-invl.prep-cost      = dec(ar-invl.prep-cost:SCREEN-VALUE)
    ar-invl.cost           = dec(ar-invl.cost:SCREEN-VALUE)

    ar-invl.i-no           = ar-invl.i-no:SCREEN-VALUE
    ar-invl.est-type       = int(ar-invl.est-type:SCREEN-VALUE)
    ar-invl.t-weight       = int(ar-invl.t-weight:SCREEN-VALUE)

    ar-invl.pr-uom         = ar-invl.pr-uom:SCREEN-VALUE
    ar-invl.form-no        = int(ar-invl.form-no:SCREEN-VALUE)
    ar-invl.blank-no       = int(ar-invl.blank-no:SCREEN-VALUE)

    ar-invl.job-no2        = int(ar-invl.job-no2:SCREEN-VALUE)
    ar-invl.t-cost         = dec(ar-invl.t-cost:SCREEN-VALUE)
    ar-invl.i-dscr         = ar-invl.i-dscr:SCREEN-VALUE
    ar-invl.cust-no        = ar-invl.cust-no:SCREEN-VALUE
    ar-invl.amt            = dec(ar-invl.amt:SCREEN-VALUE)
    ar-invl.qty            = int(ar-invl.qty:SCREEN-VALUE)
    ar-invl.i-name         = ar-invl.i-name:SCREEN-VALUE
    ar-invl.tax            = IF ar-invl.tax:SCREEN-VALUE = "YES" THEN TRUE
                             ELSE FALSE
    ar-invl.prep           = IF ar-invl.prep:SCREEN-VALUE = "YES" THEN TRUE
                             ELSE FALSE
    ar-invl.misc           = IF ar-invl.misc:SCREEN-VALUE = "YES" THEN TRUE
                             ELSE FALSE

    ar-invl.billable       = IF ar-invl.billable:SCREEN-VALUE = "Y" THEN TRUE
                             ELSE FALSE
    ar-invl.prep-amt       = dec(ar-invl.prep-amt:SCREEN-VALUE)
    ar-invl.actnum         = ar-invl.actnum:SCREEN-VALUE
    ar-invl.prep-dscr      = ar-invl.prep-dscr:SCREEN-VALUE
    ar-invl.company        = ar-invl.company:SCREEN-VALUE
    ar-invl.disc           = dec(ar-invl.disc:SCREEN-VALUE)
    ar-invl.t-freight      = dec(ar-invl.t-freight:SCREEN-VALUE)
    ar-invl.unit-pr        = dec(ar-invl.unit-pr:SCREEN-VALUE)
    ar-invl.prep-charge    = ar-invl.prep-charge:SCREEN-VALUE
    ar-invl.po-no          = ar-invl.po-no:SCREEN-VALUE
    ar-invl.req-code       = ar-invl.req-code:SCREEN-VALUE
    ar-invl.req-date       = date(ar-invl.req-date:SCREEN-VALUE)
    ar-invl.prom-code      = ar-invl.prom-code:SCREEN-VALUE
    ar-invl.prom-date      = date(ar-invl.prom-date:SCREEN-VALUE)
    ar-invl.part-dscr1     = ar-invl.part-dscr1:SCREEN-VALUE
    ar-invl.part-dscr2     = ar-invl.part-dscr2:SCREEN-VALUE
    ar-invl.matl           = dec(ar-invl.matl:SCREEN-VALUE)
    ar-invl.lab            = dec(ar-invl.lab:SCREEN-VALUE)
    ar-invl.fixoh          = int(ar-invl.fixoh:SCREEN-VALUE)
    ar-invl.varoh          = dec(ar-invl.varoh:SCREEN-VALUE)
    ar-invl.cas-cnt        = int(ar-invl.cas-cnt:SCREEN-VALUE)
    ar-invl.ship-qty       = int(ar-invl.ship-qty:SCREEN-VALUE)
    ar-invl.inv-qty        = int(ar-invl.inv-qty:SCREEN-VALUE)
    ar-invl.e-num          = int(ar-invl.e-num:SCREEN-VALUE)
    ar-invl.job-no         = ar-invl.job-no:SCREEN-VALUE
    ar-invl.loc            = ar-invl.loc:SCREEN-VALUE
    ar-invl.period         = int(ar-invl.period:SCREEN-VALUE)
    ar-invl.posted         = IF ar-invl.posted:SCREEN-VALUE = "Y" THEN TRUE
                             ELSE FALSE
    ar-invl.s-pct[1]       = dec(ar-invl.s-pct[1]:SCREEN-VALUE)
    ar-invl.s-pct[2]       = dec(ar-invl.s-pct[2]:SCREEN-VALUE)
    ar-invl.s-pct[3]       = dec(ar-invl.s-pct[3]:SCREEN-VALUE)
    ar-invl.sman[1]        = ar-invl.sman[1]:SCREEN-VALUE
    ar-invl.sman[2]        = ar-invl.sman[2]:SCREEN-VALUE
    ar-invl.sman[3]        = ar-invl.sman[3]:SCREEN-VALUE
    ar-invl.sname[1]       = ar-invl.sname[1]:SCREEN-VALUE
    ar-invl.sname[2]       = ar-invl.sname[2]:SCREEN-VALUE
    ar-invl.sname[3]       = ar-invl.sname[3]:SCREEN-VALUE
    ar-invl.b-no           = int(ar-invl.b-no:SCREEN-VALUE)
    ar-invl.bol-no         = int(ar-invl.bol-no:SCREEN-VALUE)
    ar-invl.pr-qty-uom     = ar-invl.pr-qty-uom:SCREEN-VALUE
    ar-invl.cons-uom       = ar-invl.cons-uom:SCREEN-VALUE
    ar-invl.sf-sht         = dec(ar-invl.sf-sht:SCREEN-VALUE)
    ar-invl.amt-msf        = dec(ar-invl.amt-msf:SCREEN-VALUE)
    ar-invl.po-no-po       = int(ar-invl.po-no-po:SCREEN-VALUE)
    ar-invl.inv-date       = date(ar-invl.inv-date:SCREEN-VALUE)
    ar-invl.p-c            = IF ar-invl.p-c:SCREEN-VALUE = "C" THEN TRUE
                             ELSE FALSE
    ar-invl.inv-i-no       = ar-invl.inv-i-no:SCREEN-VALUE
    ar-invl.inv-line       = int(ar-invl.inv-line:SCREEN-VALUE)
    ar-invl.dscr[1]        = ar-invl.dscr[1]:SCREEN-VALUE
    ar-invl.dscr[2]        = ar-invl.dscr[2]:SCREEN-VALUE
    ar-invl.upd-date       = date(ar-invl.upd-date:SCREEN-VALUE)
    ar-invl.t-freight-p    = dec(ar-invl.t-freight-p:SCREEN-VALUE)
    ar-invl.ord-line       = int(ar-invl.ord-line:SCREEN-VALUE)
    ar-invl.pal-i-no       = ar-invl.pal-i-no:SCREEN-VALUE
    ar-invl.pal-chg        = ar-invl.pal-chg:SCREEN-VALUE
    ar-invl.t-fuel         = dec(ar-invl.t-fuel:SCREEN-VALUE)
    ar-invl.std-fix-cost   = dec(ar-invl.std-fix-cost:SCREEN-VALUE)
    ar-invl.std-lab-cost   = dec(ar-invl.std-lab-cost:SCREEN-VALUE)
    ar-invl.std-mat-cost   = dec(ar-invl.std-mat-cost:SCREEN-VALUE)
    ar-invl.std-tot-cost   = dec(ar-invl.std-tot-cost:SCREEN-VALUE)
    ar-invl.std-var-cost   = dec(ar-invl.std-var-cost:SCREEN-VALUE)
    ar-invl.po-no2         = ar-invl.po-no2:SCREEN-VALUE.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

