&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util\crtinv.w

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

{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}*/

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DO TRANSACTION:
   {sys/inc/invdate.i}
   {sys/inc/invlotline.i}
END.

{fg/fullset.i NEW}
{oe/oe-bolpi.i NEW}

def var v-process as log no-undo.
DEF VAR fg-uom-list AS cha NO-UNDO.
DEF VAR ll-calc-disc-FIRST AS LOG NO-UNDO.
DEF VAR v-cost AS DEC EXTENT 4 NO-UNDO.
DEF VAR v-basis LIKE sman.commbasis INIT "" NO-UNDO.
DEF VAR v-u-inv LIKE oe-ctrl.u-inv INIT NO.

DEF NEW SHARED TEMP-TABLE tt-bolh NO-UNDO LIKE oe-bolh.
DEF NEW SHARED TEMP-TABLE tt-boll NO-UNDO LIKE oe-boll.

DEF TEMP-TABLE tt-missing-inv
FIELD r-oe-boll AS ROWID.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.
v-u-inv = oe-ctrl.u-inv.

DEF BUFFER b-reftable3 FOR reftable.
DEF BUFFER b-reftable FOR reftable.
DEF BUFFER b-oe-ordl FOR oe-ordl.

RUN sys/ref/uom-ea.p (OUTPUT fg-uom-list).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-boll tt-missing-inv oe-bolh

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 oe-boll.bol-no oe-boll.ord-no ~
oe-boll.i-no oe-boll.tag oe-boll.loc oe-boll.loc-bin oe-boll.job-no ~
oe-boll.job-no2 oe-boll.qty 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH oe-boll NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH oe-boll NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 oe-boll
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 oe-boll


/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 oe-boll.bol-no oe-bolh.bol-date oe-bolh.bol-status oe-bolh.cust-no oe-bolh.deleted oe-bolh.INV-NO oe-bolh.ord-no oe-bolh.po-no oe-bolh.posted oe-bolh.release#   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4   
&Scoped-define SELF-NAME BROWSE-4
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH tt-missing-inv, ~
             FIRST oe-boll WHERE ROWID(oe-boll) = tt-missing-inv.r-oe-boll                     NO-LOCK, ~
             FIRST oe-bolh WHERE oe-bolh.company EQ cocode AND                           oe-bolh.bol-no EQ oe-boll.bol-no AND                           oe-bolh.posted EQ TRUE                     NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY {&SELF-NAME} FOR EACH tt-missing-inv, ~
             FIRST oe-boll WHERE ROWID(oe-boll) = tt-missing-inv.r-oe-boll                     NO-LOCK, ~
             FIRST oe-bolh WHERE oe-bolh.company EQ cocode AND                           oe-bolh.bol-no EQ oe-boll.bol-no AND                           oe-bolh.posted EQ TRUE                     NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 tt-missing-inv oe-boll oe-bolh
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 tt-missing-inv
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-4 oe-boll
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-4 oe-bolh


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BROWSE-4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 fi_bol-no-st fi_bol-no-end ~
btCheckBolRange BROWSE-4 fi_bol-no btn_bol BROWSE-2 btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 fi_bol-no-st fi_bol-no-end ~
fi_bol-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCheckBolRange 
     LABEL "Check BOL Range" 
     SIZE 20 BY 1.14.

DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn_bol 
     LABEL "Select BOL#" 
     SIZE 20 BY 1.14.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE fi_bol-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Posted BOL#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_bol-no-end AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "To BOL#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_bol-no-st AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "From BOL#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 17.14.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 16.91.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      oe-boll SCROLLING.

DEFINE QUERY BROWSE-4 FOR 
      tt-missing-inv, 
      oe-boll, 
      oe-bolh SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      oe-boll.bol-no COLUMN-LABEL "BOL#" FORMAT ">>>>>>>9":U
      oe-boll.ord-no FORMAT ">>>>>9":U
      oe-boll.i-no FORMAT "x(15)":U WIDTH 20
      oe-boll.tag COLUMN-LABEL "Tag#" FORMAT "x(15)":U WIDTH 20
      oe-boll.loc FORMAT "x(5)":U
      oe-boll.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U WIDTH 18
      oe-boll.job-no COLUMN-LABEL "Job#" FORMAT "x(6)":U
      oe-boll.job-no2 COLUMN-LABEL "" FORMAT "99":U WIDTH 5
      oe-boll.qty COLUMN-LABEL "Qty Shipped" FORMAT "->>,>>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 85 BY 11.43 ROW-HEIGHT-CHARS .57 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 C-Win _FREEFORM
  QUERY BROWSE-4 DISPLAY
      oe-boll.bol-no  oe-bolh.bol-date oe-bolh.bol-status 
           oe-bolh.cust-no oe-bolh.deleted  oe-bolh.INV-NO 
           oe-bolh.ord-no  oe-bolh.po-no    oe-bolh.posted 
           oe-bolh.release#
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86 BY 11.67
         TITLE "BOL Lines Found Not Invoiced" ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     FILL-IN-1 AT ROW 1.48 COL 53 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     fi_bol-no-st AT ROW 3.38 COL 4 WIDGET-ID 12
     fi_bol-no-end AT ROW 3.38 COL 33 WIDGET-ID 14
     btCheckBolRange AT ROW 3.38 COL 64 WIDGET-ID 16
     BROWSE-4 AT ROW 6.24 COL 3 WIDGET-ID 200
     fi_bol-no AT ROW 19.57 COL 11 WIDGET-ID 4
     btn_bol AT ROW 19.57 COL 42 WIDGET-ID 8
     BROWSE-2 AT ROW 22.19 COL 3 WIDGET-ID 100
     btn-process AT ROW 33.86 COL 23
     btn-cancel AT ROW 33.86 COL 55
     "Check Range of BOL for unposted BOL lines" VIEW-AS TEXT
          SIZE 44 BY .62 AT ROW 1.71 COL 5 WIDGET-ID 18
          FGCOLOR 9 
     "Select Line Items" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 21 COL 5 WIDGET-ID 10
          FGCOLOR 9 
     RECT-1 AT ROW 1.24 COL 2 WIDGET-ID 20
     RECT-2 AT ROW 18.62 COL 1 WIDGET-ID 22
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 34.81.


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
         TITLE              = "Create New Invoice from BOL Lines"
         HEIGHT             = 35
         WIDTH              = 91
         MAX-HEIGHT         = 43.52
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 43.52
         VIRTUAL-WIDTH      = 98.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-4 btCheckBolRange FRAME-A */
/* BROWSE-TAB BROWSE-2 btn_bol FRAME-A */
/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_bol-no IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_bol-no-end IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_bol-no-st IN FRAME FRAME-A
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "asi.oe-boll"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > asi.oe-boll.bol-no
"oe-boll.bol-no" "BOL#" ? "integer" ? ? ? ? ? ? no "" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = asi.oe-boll.ord-no
     _FldNameList[3]   > asi.oe-boll.i-no
"oe-boll.i-no" ? ? "character" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.oe-boll.tag
"oe-boll.tag" "Tag#" "x(15)" "character" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.oe-boll.loc
"oe-boll.loc" ? ? "character" ? ? ? ? ? ? no "" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.oe-boll.loc-bin
"oe-boll.loc-bin" "Bin" ? "character" ? ? ? ? ? ? no "" no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.oe-boll.job-no
"oe-boll.job-no" "Job#" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.oe-boll.job-no2
"oe-boll.job-no2" "" ? "integer" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.oe-boll.qty
"oe-boll.qty" "Qty Shipped" ? "integer" ? ? ? ? ? ? no "" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-missing-inv,
      FIRST oe-boll WHERE ROWID(oe-boll) = tt-missing-inv.r-oe-boll
                    NO-LOCK,
      FIRST oe-bolh WHERE oe-bolh.company EQ cocode AND
                          oe-bolh.bol-no EQ oe-boll.bol-no AND
                          oe-bolh.posted EQ TRUE
                    NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Create New Invoice from BOL Lines */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Create New Invoice from BOL Lines */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-4
&Scoped-define SELF-NAME BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-4 C-Win
ON MOUSE-SELECT-DBLCLICK OF BROWSE-4 IN FRAME FRAME-A /* BOL Lines Found Not Invoiced */
DO:
fi_bol-no:SCREEN-VALUE = string(oe-bolh.bol-no).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCheckBolRange
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCheckBolRange C-Win
ON CHOOSE OF btCheckBolRange IN FRAME FRAME-A /* Check BOL Range */
DO:
  ASSIGN fi_bol-no-st fi_bol-no-end.
    FILL-IN-1:HIDDEN = FALSE.
  FILL-IN-1:SCREEN-VALUE = "Checking BOLs".
  RUN check-range.
  FILL-IN-1:SCREEN-VALUE = "".
  FILL-IN-1:HIDDEN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
   run run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_bol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_bol C-Win
ON CHOOSE OF btn_bol IN FRAME FRAME-A /* Select BOL# */
DO:
   ASSIGN fi_bol-no.

   IF NOT CAN-FIND(FIRST oe-bolh WHERE
                   oe-bolh.company EQ cocode AND
                   oe-bolh.bol-no EQ fi_bol-no AND
                   oe-bolh.posted EQ YES) THEN
   DO:
       MESSAGE "Invalid BOL#."
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       LEAVE.
   END.

   CLOSE QUERY browse-2.

   OPEN QUERY browse-2 FOR EACH oe-boll WHERE
        oe-boll.company = cocode AND
        oe-boll.bol-no = fi_bol-no
        NO-LOCK.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
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
  /* check security */
  
  RUN enable_UI.
  FILL-IN-1:HIDDEN = TRUE.
  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-range C-Win 
PROCEDURE check-range :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR vi-cnt AS INT NO-UNDO.
EMPTY TEMP-TABLE tt-missing-inv.

FOR EACH oe-boll WHERE oe-boll.company EQ cocode
                   AND oe-boll.bol-no  GE fi_bol-no-st
                   AND oe-boll.bol-no  LE fi_bol-no-end
                   AND oe-boll.deleted EQ NO
                 NO-LOCK,
        FIRST oe-bolh WHERE
          oe-bolh.company EQ cocode         AND
          oe-bolh.bol-no  EQ oe-boll.bol-no AND
          oe-bolh.deleted EQ NO
         NO-LOCK.

      IF invlotline-log EQ NO THEN
         find first inv-line WHERE
              inv-line.r-no   eq inv-head.r-no
          and inv-line.ord-no eq oe-boll.ord-no
          and inv-line.b-no   eq oe-bolh.b-no
          and inv-line.i-no   eq oe-boll.i-no
          and inv-line.line   eq oe-boll.line
          and inv-line.po-no  eq oe-boll.po-no
          use-index r-no no-error.
      ELSE
      DO:
         IF oe-boll.lot-no <> "" THEN
          find first inv-line
              where inv-line.r-no   eq inv-head.r-no
                and inv-line.ord-no eq oe-boll.ord-no
                and inv-line.b-no   eq oe-bolh.b-no
                and inv-line.i-no   eq oe-boll.i-no
                and inv-line.line   eq oe-boll.line
                and inv-line.po-no  eq oe-boll.po-no
                AND inv-line.lot-no = oe-boll.lot-no                    
              use-index r-no no-error.
          ELSE
              find first inv-line
              where inv-line.r-no   eq inv-head.r-no
                and inv-line.ord-no eq oe-boll.ord-no
                and inv-line.b-no   eq oe-bolh.b-no
                and inv-line.i-no   eq oe-boll.i-no
                and inv-line.line   eq oe-boll.line
                and inv-line.po-no  eq oe-boll.po-no
              use-index r-no no-error.
          IF NOT AVAIL inv-line THEN DO:
              vi-cnt = vi-cnt + 1.
              CREATE tt-missing-inv.
              ASSIGN tt-missing-inv.r-oe-boll = ROWID(oe-boll).
          END.

      END.

END.
FIND FIRST tt-missing-inv NO-ERROR.
PAUSE BEFORE-HIDE.
IF AVAIL tt-missing-inv THEN DO:
    CLOSE QUERY browse-4.

OPEN QUERY browse-4 FOR EACH tt-missing-inv,
      FIRST oe-boll WHERE ROWID(oe-boll) = tt-missing-inv.r-oe-boll
                    NO-LOCK,
      FIRST oe-bolh WHERE oe-bolh.company EQ cocode AND
                          oe-bolh.bol-no EQ oe-boll.bol-no
                    NO-LOCK.


    /*
  FOR EACH tt-missing-inv,
      FIRST oe-boll WHERE ROWID(oe-boll) = tt-missing-inv.r-oe-boll
                    NO-LOCK,
      FIRST oe-bolh WHERE oe-bolh.company EQ cocode AND
                          oe-bolh.bol-no EQ oe-boll.bol-no
                    NO-LOCK:

      DISP oe-boll.bol-no  oe-bolh.bol-date oe-bolh.bol-status 
           oe-bolh.cust-no oe-bolh.deleted  oe-bolh.INV-NO 
           oe-bolh.ord-no  oe-bolh.po-no    oe-bolh.posted 
           oe-bolh.release#
          WITH FRAME f-missing WITH 10 DOWN WIDTH 250.
      DOWN WITH FRAME f-missing.

  END.
  HIDE FRAME f-missing.
  */
END.
ELSE
    MESSAGE "No missing invoices found for this range."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
PAUSE 0 BEFORE-HIDE.
MESSAGE "Done with BOL check."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY FILL-IN-1 fi_bol-no-st fi_bol-no-end fi_bol-no 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 fi_bol-no-st fi_bol-no-end btCheckBolRange BROWSE-4 
         fi_bol-no btn_bol BROWSE-2 btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DEF VAR v-index AS INT NO-UNDO.
   DEF VAR v-ref-no AS INT NO-UNDO.
   DEF VAR v-fob-code AS CHAR NO-UNDO.
   DEF VAR v-line-count AS INT NO-UNDO.
   DEF VAR v-start-pos AS INT NO-UNDO.
   DEF VAR li AS INT NO-UNDO.
   DEF VAR ls AS CHAR NO-UNDO.
   DEF VAR i AS INT NO-UNDO.

   session:set-wait-state("general").

   EMPTY TEMP-TABLE tt-bolh.
   EMPTY TEMP-TABLE tt-boll.
   FOR EACH tt-fg-set:
       DELETE tt-fg-set.
   END.

   DO WITH FRAME {&FRAME-NAME}:
   
   DO v-index = 1 TO browse-2:NUM-SELECTED-ROWS:
      browse-2:FETCH-SELECTED-ROW(v-index).

      IF v-index EQ 1 THEN
      DO:
  
         v-ref-no = next-value(inv_r_no_seq).

         FIND FIRST oe-bolh WHERE
              oe-bolh.company EQ cocode AND
              oe-bolh.bol-no EQ oe-boll.bol-no
              NO-LOCK NO-ERROR.

         IF AVAIL oe-bolh THEN
         DO:
            FIND FIRST shipto WHERE
                 shipto.company EQ oe-bolh.company AND
                 shipto.ship-id EQ oe-bolh.ship-id AND
                 shipto.cust-no EQ oe-bolh.cust-no AND
                 shipto.ship-no NE 1
                 USE-INDEX ship-id NO-LOCK NO-ERROR.
           
            IF NOT AVAIL shipto THEN
               FIND FIRST shipto WHERE
                    shipto.company EQ oe-bolh.company AND
                    shipto.cust-no EQ oe-bolh.cust-no
                    USE-INDEX ship-no
                    NO-LOCK NO-ERROR.
           
            FIND FIRST reftable WHERE
                 reftable.reftable EQ "oe-bolh.lot-no" AND
                 reftable.rec_key  EQ oe-bolh.rec_key
                 USE-INDEX rec_key
                 NO-LOCK NO-ERROR.
           
            IF AVAIL reftable THEN
               v-fob-code = reftable.CODE.
            ELSE
               v-fob-code = "".
            RELEASE reftable.
           
            FIND FIRST oe-ord WHERE
                  oe-ord.company EQ oe-boll.company AND
                  oe-ord.ord-no  EQ oe-boll.ord-no
                  NO-LOCK NO-ERROR.

            FIND FIRST cust WHERE
                 cust.company EQ oe-bolh.company AND
                 cust.cust-no EQ oe-bolh.cust-no
                 NO-LOCK NO-ERROR.

            CREATE inv-head.
            ASSIGN
              inv-head.sold-no      = shipto.ship-id
              inv-head.sold-name    = shipto.ship-name
              inv-head.sold-addr[1] = shipto.ship-addr[1]
              inv-head.sold-addr[2] = shipto.ship-addr[2]
              inv-head.sold-state   = shipto.ship-state
              inv-head.sold-city    = shipto.ship-city
              inv-head.sold-zip     = shipto.ship-zip
              inv-head.r-no         = v-ref-no
              inv-head.company      = oe-bolh.company
              inv-head.bol-no       = oe-bolh.bol-no
              inv-head.bill-to      = oe-bolh.cust-no
              inv-head.cust-no      = oe-bolh.cust-no
              inv-head.frt-pay      = oe-bolh.frt-pay
              inv-head.carrier      = oe-bolh.carrier
              inv-head.ship-i[1]    = oe-bolh.ship-i[1]
              inv-head.ship-i[2]    = oe-bolh.ship-i[2]
              inv-head.ship-i[3]    = oe-bolh.ship-i[3]
              inv-head.ship-i[4]    = oe-bolh.ship-i[4]
              inv-head.fob-code     = (IF v-fob-code <> "" THEN v-fob-code
                                       ELSE oe-ord.fob-code)
              inv-head.contact      = oe-ord.contact
              inv-head.terms        = oe-ord.terms
              inv-head.terms-d      = oe-ord.terms-d
              inv-head.f-bill       = NO
              inv-head.tax-gr       = IF AVAIL shipto AND shipto.tax-code NE ""
                                      THEN shipto.tax-code ELSE oe-ord.tax-gr
              inv-head.tot-ord      = 0
              inv-head.inv-no       = 0
              inv-head.stat         = ""
              inv-head.deleted      = NO
              inv-head.posted       = NO
              inv-head.inv-date     = IF invdate-chr EQ "Current" THEN TODAY
                                      ELSE oe-bolh.bol-date
              inv-head.cust-name    = cust.name
              inv-head.addr[1]      = cust.addr[1]
              inv-head.addr[2]      = cust.addr[2]
              inv-head.city         = cust.city
              inv-head.state        = cust.state
              inv-head.zip          = cust.zip
              inv-head.curr-code[1] = cust.curr-code.
            
            FIND FIRST usergrps WHERE
                 usergrps.usergrps = "IN"
                 NO-LOCK NO-ERROR.
            
            IF AVAIL usergrps AND TRIM(usergrps.users) NE "" THEN
            DO:
               ASSIGN
                v-line-count = 0
                v-start-pos  = 1.
            
               DO li = 1 TO LENGTH(usergrps.users):
                  ls = SUBSTR(usergrps.users,li,1).
                 
                  IF v-line-count < 5 AND ls EQ CHR(10) OR ls EQ CHR(13) THEN
                     ASSIGN
                        v-line-count = v-line-count + 1
                        inv-head.bill-i[v-line-count] = SUBSTR(usergrps.users,v-start-pos,li - v-start-pos)
                        v-start-pos = li + 1.
               
                  IF v-line-count < 5 AND li = LENGTH(usergrps.users) AND
                     NOT(ls EQ CHR(10) OR ls EQ CHR(13)) THEN
                     ASSIGN
                        v-line-count = v-line-count + 1
                        inv-head.bill-i[v-line-count] = SUBSTR(usergrps.users,v-start-pos,li - v-start-pos + 1).
               END.
               
               RELEASE usergrps.
            END.
            
            DO li = 1 TO 4:
               IF inv-head.bill-i[li] = "" THEN
                  inv-head.bill-i[li] = oe-ord.bill-i[li].
            END.

            IF oe-bolh.freight NE 0 AND inv-head.frt-pay EQ "B" THEN inv-head.f-bill = YES.

            for each oe-ordm WHERE
                oe-ordm.company eq oe-boll.company AND
                oe-ordm.ord-no  eq oe-boll.ord-no  AND
                oe-ordm.bill    eq "Y":
                create inv-misc.
                BUFFER-COPY oe-ordm EXCEPT rec_key TO inv-misc
                assign
                 inv-misc.r-no           = v-ref-no
                 inv-misc.posted         = no
                 inv-misc.deleted        = no
                 inv-misc.inv-i-no       = oe-ordm.ord-i-no
                 inv-misc.inv-line       = oe-ordm.ord-line
                 inv-misc.s-commbasis[1] = oe-ordm.commbasis[1]
                 oe-ordm.bill = "I".   /** Set billing flag to (I)nvoiced **/
            end.
         END.
      END. /*v-index eq 1*/

      RELEASE inv-line.

      IF invlotline-log EQ NO THEN
         find first inv-line WHERE
              inv-line.r-no   eq inv-head.r-no
          and inv-line.ord-no eq oe-boll.ord-no
          and inv-line.b-no   eq oe-bolh.b-no
          and inv-line.i-no   eq oe-boll.i-no
          and inv-line.line   eq oe-boll.line
          and inv-line.po-no  eq oe-boll.po-no
          use-index r-no no-error.
      ELSE
      DO:
         
          IF oe-boll.lot-no <> "" THEN
          find first inv-line
              where inv-line.r-no   eq inv-head.r-no
                and inv-line.ord-no eq oe-boll.ord-no
                and inv-line.b-no   eq oe-bolh.b-no
                and inv-line.i-no   eq oe-boll.i-no
                and inv-line.line   eq oe-boll.line
                and inv-line.po-no  eq oe-boll.po-no
                AND inv-line.lot-no = oe-boll.lot-no                    
              use-index r-no no-error.
          ELSE
              find first inv-line
              where inv-line.r-no   eq inv-head.r-no
                and inv-line.ord-no eq oe-boll.ord-no
                and inv-line.b-no   eq oe-bolh.b-no
                and inv-line.i-no   eq oe-boll.i-no
                and inv-line.line   eq oe-boll.line
                and inv-line.po-no  eq oe-boll.po-no
              use-index r-no no-error.
      END.

      IF NOT AVAIL inv-line THEN DO:
         CREATE inv-line.

         FIND FIRST oe-ordl WHERE
              oe-ordl.company EQ oe-boll.company AND
              oe-ordl.ord-no  EQ oe-boll.ord-no AND
              oe-ordl.line    EQ oe-boll.LINE AND
              oe-ordl.i-no    EQ oe-boll.i-no
              NO-LOCK NO-ERROR.

         FIND FIRST itemfg WHERE
              itemfg.company EQ oe-boll.company AND
              itemfg.i-no    EQ oe-boll.i-no
              NO-LOCK NO-ERROR.
           
         ASSIGN
          inv-line.r-no       = v-ref-no
          inv-line.company    = oe-bolh.company
          inv-line.ord-no     = oe-boll.ord-no
          inv-line.b-no       = oe-bolh.b-no
          inv-line.line       = oe-boll.line
          inv-line.i-no       = oe-boll.i-no
          inv-line.stat       = oe-boll.s-code
          inv-line.est-no     = oe-ordl.est-no
          inv-line.est-type   = oe-ord.est-type
          inv-line.ord-date   = oe-ord.ord-date
          inv-line.part-no    = oe-ordl.part-no
          inv-line.i-name     = oe-ordl.i-name
          inv-line.i-dscr     = oe-ordl.i-dscr
          inv-line.pr-uom     = oe-ordl.pr-uom
          inv-line.price      = oe-ordl.price
          inv-line.cas-cnt    = IF oe-ordl.pr-uom EQ "CS" THEN oe-ordl.cas-cnt
                                                          ELSE oe-boll.qty-case
          inv-line.req-code   = oe-ordl.req-code
          inv-line.req-date   = oe-ordl.req-date
          inv-line.prom-code  = oe-ordl.prom-code
          inv-line.prom-date  = oe-ordl.prom-date
          inv-line.part-dscr1 = oe-ordl.part-dscr1
          inv-line.part-dscr2 = oe-ordl.part-dscr2
          inv-line.po-no-po   = oe-ordl.po-no-po
          inv-line.e-num      = oe-ordl.e-num
          inv-line.form-no    = oe-ordl.form-no
          inv-line.blank-no   = oe-ordl.blank-no
          inv-line.j-no       = oe-ordl.j-no
          inv-line.job-no     = oe-ordl.job-no
          inv-line.job-no2    = oe-ordl.job-no2
          inv-line.tax        = oe-ordl.tax
          inv-line.disc       = oe-ordl.disc
          inv-line.qty        = oe-ordl.qty
          inv-line.p-c        = oe-boll.p-c
          inv-line.po-no      = oe-boll.po-no.
         
         
             IF oe-boll.zeroPrice EQ 1 THEN
                inv-line.price = 0.
             ELSE IF oe-boll.sell-price NE 0 THEN
                inv-line.price = oe-boll.sell-price.
             
         
          
          IF oe-boll.lot-no <> "" THEN
          DO:
             ASSIGN inv-line.lot-no  = oe-boll.lot-no.                          
          END.
      END.

      ASSIGN
         inv-line.t-weight      = inv-line.t-weight + oe-boll.weight
         inv-head.t-inv-weight  = inv-head.t-inv-weight + oe-boll.weight
         inv-line.t-freight     = inv-line.t-freight + oe-boll.freight
         inv-head.t-inv-freight = inv-head.t-inv-freight + oe-boll.freight.

      /* Moved to before extended price calc for inv-qty */
      /** Increase invoice Qty when invoice or invoice & ship **/
      IF oe-boll.s-code ne "S" and not oe-ordl.is-a-component then
         inv-line.inv-qty = inv-line.inv-qty + oe-boll.qty.
  
      /** Increase ship Qty when ship or invoice & ship **/
      if oe-boll.s-code ne "I" or
         can-find(first b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}) then
         inv-line.ship-qty = inv-line.ship-qty + oe-boll.qty.

      inv-line.t-price = inv-line.inv-qty / 1000 * inv-line.price.

      IF inv-line.pr-uom BEGINS "L" AND inv-line.pr-uom NE "LB" THEN
         inv-line.t-price = inv-line.price *
                            IF inv-line.inv-qty LT 0 THEN -1 ELSE 1.
      ELSE IF inv-line.pr-uom EQ "CS" THEN
         inv-line.t-price = inv-line.inv-qty /
                            (IF inv-line.cas-cnt NE 0 THEN
                                inv-line.cas-cnt
                            ELSE
                            IF itemfg.case-count NE 0 THEN
                               itemfg.case-count ELSE 1) *
                               inv-line.price.
                            ELSE IF LOOKUP(inv-line.pr-uom,fg-uom-list) GT 0 THEN
                               inv-line.t-price = inv-line.inv-qty * inv-line.price.
      ELSE
         FOR EACH uom
              WHERE uom.uom  EQ inv-line.pr-uom
                AND uom.mult NE 0
              NO-LOCK:
              inv-line.t-price = inv-line.inv-qty / uom.mult * inv-line.price.
              LEAVE.
         END.
      inv-line.t-price = ROUND(inv-line.t-price,2).

      IF inv-line.disc NE 0 THEN
         inv-line.t-price = 
               IF ll-calc-disc-first THEN 
                  (inv-line.t-price - ROUND(inv-line.t-price * inv-line.disc / 100,2))
      ELSE
         ROUND(inv-line.t-price * (1 - (inv-line.disc / 100)),2).

      RUN oe/invlcost.p (ROWID(inv-line),
                         OUTPUT v-cost[1], OUTPUT v-cost[2],
                         OUTPUT v-cost[3], OUTPUT v-cost[4],
                         OUTPUT inv-line.cost, OUTPUT inv-line.t-cost).

      do i = 1 to 3:          /** Calculate Commission Amount **/
         assign
           inv-line.sname[i]   = oe-ord.sname[i]
           inv-line.s-comm[i]  = oe-ordl.s-comm[i]
           inv-line.s-pct[i]   = oe-ordl.s-pct[i]
           inv-line.sman[i]    = oe-ordl.s-man[i].
      end.

      DO i = 1 TO EXTENT(inv-line.sman):    /** Calculate Commission Amount **/
         RUN custom/combasis.p (oe-boll.company, inv-line.sman[i], cust.type, itemfg.procat, 0,
                                cust.cust-no,
                                OUTPUT v-basis).

         IF v-basis EQ "G" THEN
            inv-line.comm-amt[i] = ROUND(((inv-line.t-price - inv-line.t-cost)
                                          * inv-line.s-comm[i]) / 100,2).
         ELSE
            inv-line.comm-amt[i] = ROUND((((inv-line.t-price
                                        * inv-line.s-pct[i]) / 100)
                                       * inv-line.s-comm[i]) / 100,2).
      END.

      if v-u-inv then do:
         {oe/oe-bolp.i "oe-ordl"}
      end.
   END. /*v-index*/
   END. /*do with frame*/

   session:set-wait-state("").

   message "Process Is Completed." view-as alert-box.
  
/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

