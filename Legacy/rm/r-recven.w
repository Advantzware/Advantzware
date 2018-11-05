&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: addon/rm/r-recven.w

  Description: Receive Vendor Tags

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
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

/*{sys/inc/var.i new shared} */
def new shared var cocode as cha no-undo.
def new shared var locode as cha no-undo.

assign
 cocode = gcompany
 locode = gloc.

find first loc  where
     loc.company eq cocode AND
     loc.loc eq locode
     no-lock no-error.

DEF VAR v-loadtag AS CHAR NO-UNDO INIT "ASI". /* sys ctrl option */
DEF VAR v-bin AS CHAR NO-UNDO. /* sys ctrl cption */

DEF BUFFER b-company FOR company.

DEF TEMP-TABLE tt-mat NO-UNDO
    FIELD frm LIKE job-mat.frm
    FIELD qty LIKE job-mat.qty
    INDEX frm frm.

DEF TEMP-TABLE tt-po NO-UNDO
    FIELD po-no AS INT
    FIELD LINE AS INT
    FIELD tot-rec-qty AS DEC
    FIELD cons-uom AS CHAR
    FIELD overrun-qty AS DEC
    INDEX po po-no ASC LINE ASC.

DEFINE TEMP-TABLE w-po NO-UNDO
  FIELD i-no LIKE po-ordl.i-no
  FIELD i-name LIKE po-ordl.i-name
  FIELD over-pct LIKE po-ord.over-pct
  FIELD cost LIKE po-ordl.cost
  FIELD po-no LIKE po-ord.po-no
  FIELD b-num LIKE po-ordl.b-num
  FIELD cons-cost LIKE po-ordl.cons-cost
  FIELD cons-qty LIKE po-ordl.cons-qty
  FIELD cons-uom LIKE po-ordl.cons-uom
  FIELD job-no LIKE po-ordl.job-no
  FIELD job-no2 LIKE po-ordl.job-no2
  FIELD ord-no LIKE po-ordl.ord-no
  FIELD ord-qty LIKE po-ordl.ord-qty
  FIELD pr-uom LIKE po-ordl.pr-uom
  FIELD s-len LIKE po-ordl.s-len
  FIELD s-num LIKE po-ordl.s-num
  FIELD s-wid LIKE po-ordl.s-wid
  FIELD loc AS CHAR
  FIELD loc-bin LIKE item.loc-bin
  FIELD LINE AS INT
  FIELD rcpt-qty LIKE rm-rctd.qty
  FIELD tag-date AS DATE
  FIELD total-tags AS INT
  FIELD overrun-qty AS INT
  FIELD setup AS DEC
  FIELD TYPE AS CHAR
  FIELD add-setup AS LOG
  INDEX po IS PRIMARY po-no ASC.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn_receive scr-vend-tag 
&Scoped-Define DISPLAYED-OBJECTS scr-vend-tag 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkWhsBin C-Win 
FUNCTION checkWhsBin RETURNS LOGICAL
  ( ipCompany AS CHARACTER,ipLoc AS CHARACTER,ipLocBin AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_receive 
     LABEL "Receive Vendor Tag" 
     SIZE 22 BY 1.14.

DEFINE VARIABLE scr-vend-tag AS CHARACTER FORMAT "X(256)":U 
     LABEL "Vendor Tag#" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     btn_receive AT ROW 2.1 COL 58.2 WIDGET-ID 4
     scr-vend-tag AT ROW 2.19 COL 15.8 COLON-ALIGNED WIDGET-ID 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 99.6 BY 10.14.


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
         TITLE              = "Receive Vendor Tags"
         HEIGHT             = 10.38
         WIDTH              = 100.2
         MAX-HEIGHT         = 10.38
         MAX-WIDTH          = 100.2
         VIRTUAL-HEIGHT     = 10.38
         VIRTUAL-WIDTH      = 100.2
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
   FRAME-NAME Custom                                                    */
ASSIGN
       btn_receive:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Receive Vendor Tags */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Receive Vendor Tags */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_receive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_receive C-Win
ON CHOOSE OF btn_receive IN FRAME FRAME-A /* Receive Vendor Tag */
DO:
   DEF VAR op-error AS LOG NO-UNDO.
   DEF VAR op-po-no AS INT NO-UNDO.
   DEF VAR op-po-line AS INT NO-UNDO.
   DEF VAR op-qty AS INT NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      RUN validate-tag-proc(OUTPUT op-po-no,
                            OUTPUT op-po-line,
                            OUTPUT op-qty,
                            OUTPUT op-error).

      IF NOT op-error THEN
         RUN process-tag-proc(INPUT op-po-no,
                              INPUT op-po-line,
                              INPUT op-qty).
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
/*{sys/inc/f3helpw.i} */
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



/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  FIND FIRST sys-ctrl NO-LOCK
      WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "LOADTAG" NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
     DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
         sys-ctrl.company  = gcompany
         sys-ctrl.name     = "LOADTAG"
         sys-ctrl.descrip  = "Special Load tag print options, e.g. barcode printer"
         sys-ctrl.char-fld = "ASI".
        MESSAGE "System control record NOT found. Please enter the load tag option"
                UPDATE sys-ctrl.char-fld.
        FIND CURRENT sys-ctrl NO-LOCK.
    END.

  ASSIGN
     v-loadtag = sys-ctrl.char-fld.

  FIND FIRST sys-ctrl NO-LOCK
       WHERE sys-ctrl.company EQ gcompany
       AND sys-ctrl.name    EQ "RMWHSBIN" NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
     DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
         sys-ctrl.company  = gcompany
         sys-ctrl.name     = "RMWHSBIN"
         sys-ctrl.descrip  = "Default Location for RM Warehouse / Bin?"
         sys-ctrl.char-fld = "RMITEM".
        FIND CURRENT sys-ctrl NO-LOCK.
    END.
  v-bin = sys-ctrl.char-fld.

  RUN enable_UI.

  APPLY "ENTRY" TO scr-vend-tag.

  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convert-vend-comp-curr C-Win 
PROCEDURE convert-vend-comp-curr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER ip-cost AS DEC DECIMALS 4 NO-UNDO.

  FIND FIRST vend WHERE
       vend.company EQ po-ord.company AND
       vend.vend-no EQ po-ord.vend-no
       NO-LOCK NO-ERROR.

  IF AVAIL vend THEN
  DO:
     FIND FIRST b-company WHERE
          b-company.company EQ cocode
          NO-LOCK.

     IF vend.curr-code NE b-company.curr-code THEN
     DO:
        FIND FIRST currency WHERE
             currency.company EQ po-ord.company AND
             currency.c-code EQ vend.curr-code
             NO-LOCK NO-ERROR.

        IF AVAIL currency THEN
        DO:
           ip-cost = ip-cost * currency.ex-rate.

           RELEASE currency.
        END.
     END.

     RELEASE b-company.
     RELEASE vend.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-loadtag-proc C-Win 
PROCEDURE create-loadtag-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR i AS INT NO-UNDO.
   DEF VAR tagNo AS CHAR NO-UNDO.
   DEF VAR ld AS DEC NO-UNDO.
   DEF VAR ipTagNo AS INT INIT 1 NO-UNDO.

   DO WHILE TRUE:
      tagNo = STRING(w-po.po-no,'9999999') + STRING(w-po.line,'999') + STRING(ipTagNo,'9999999').
      IF NOT CAN-FIND(FIRST loadtag
                      WHERE loadtag.company EQ cocode
                        AND loadtag.item-type EQ YES
                        AND loadtag.tag-no EQ tagNo) THEN LEAVE.
      ipTagNo = ipTagNo + 1.
   END. /* do while */

   CREATE loadtag.
   ASSIGN
    loadtag.company      = cocode
    loadtag.tag-no       = tagNo
    loadtag.item-type    = YES
    loadtag.po-no        = w-po.po-no
    loadtag.line         = w-po.line
    loadtag.job-no       = w-po.job-no
    loadtag.job-no2      = w-po.job-no2
    loadtag.form-no      = w-po.s-num
    loadtag.blank-no     = w-po.b-num
    loadtag.ord-no       = w-po.ord-no
    loadtag.i-no         = CAPS(w-po.i-no)
    loadtag.i-name       = w-po.i-name
    loadtag.qty          = w-po.ord-qty
    loadtag.qty-case     = w-po.rcpt-qty
    loadtag.case-bundle  = 1
    loadtag.pallet-count = w-po.rcpt-qty
    loadtag.loc          = w-po.loc
    loadtag.loc-bin      = w-po.loc-bin
    loadtag.tot-cases    = 0
    loadtag.sts          = "Printed"
    loadtag.tag-date     = TODAY
    loadtag.tag-time     = TIME
    loadtag.misc-char[1] = TRIM(scr-vend-tag).

  RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT i) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   CREATE rm-rctd.
   ASSIGN
    rm-rctd.r-no       = i
    rm-rctd.rct-date   = TODAY
    rm-rctd.company    = cocode
    rm-rctd.rita-code  = "R"
    rm-rctd.i-name     = w-po.i-name
    rm-rctd.i-no       = loadtag.i-no
    rm-rctd.job-no     = loadtag.job-no
    rm-rctd.job-no2    = loadtag.job-no2
    rm-rctd.po-no      = STRING(loadtag.po-no)
    rm-rctd.po-line    = loadtag.line
    rm-rctd.s-num      = w-po.s-num
    rm-rctd.b-num      = w-po.b-num
    rm-rctd.qty        = w-po.rcpt-qty
    rm-rctd.pur-uom    = w-po.cons-uom
    rm-rctd.cost       = w-po.cost
    rm-rctd.cost-uom   = w-po.pr-uom
    rm-rctd.loc        = loadtag.loc
    rm-rctd.loc-bin    = loadtag.loc-bin
    rm-rctd.tag        = loadtag.tag-no
    rm-rctd.user-id  = USERID("nosweat")
    rm-rctd.upd-date = TODAY
    rm-rctd.upd-time = TIME. 

   RELEASE rm-bin.
   IF w-po.po-no EQ 0 THEN
      FIND FIRST rm-bin NO-LOCK
          WHERE rm-bin.company EQ item.company
            AND rm-bin.loc     EQ loadtag.loc
            AND rm-bin.i-no    EQ loadtag.i-no
            AND rm-bin.loc-bin EQ loadtag.loc-bin
          NO-ERROR.

   IF AVAIL rm-bin THEN DO:
     FIND FIRST item NO-LOCK
         WHERE item.company EQ rm-rctd.company
           AND item.i-no    EQ rm-rctd.i-no
         USE-INDEX i-no NO-ERROR.
     ASSIGN
      rm-rctd.cost     = rm-bin.cost
      rm-rctd.cost-uom = item.cons-uom.
   END.

   RUN get-matrix.

   FIND CURRENT rm-rctd NO-LOCK NO-ERROR.
   FIND CURRENT loadtag NO-LOCK NO-ERROR.

   MESSAGE "Loadtag and Receipt Created."
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
  DISPLAY scr-vend-tag 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE btn_receive scr-vend-tag 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix C-Win 
PROCEDURE get-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE v-len LIKE po-ordl.s-len NO-UNDO.
   DEFINE VARIABLE v-wid LIKE po-ordl.s-len NO-UNDO.
   DEFINE VARIABLE v-dep LIKE po-ordl.s-len NO-UNDO. 
   DEFINE VARIABLE v-bwt LIKE po-ordl.s-len NO-UNDO.
   DEFINE VARIABLE lv-out-qty LIKE rm-rctd.qty NO-UNDO.
   DEFINE VARIABLE lv-out-cost LIKE rm-rctd.cost NO-UNDO.
   DEFINE VARIABLE lv-qty-uom LIKE rm-rctd.pur-uom NO-UNDO.
   DEFINE VARIABLE lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.

   FIND item NO-LOCK WHERE item.company EQ cocode
                       AND item.i-no EQ rm-rctd.i-no
                       USE-INDEX i-no NO-ERROR.
   IF NOT AVAIL item THEN LEAVE.

   IF item.cons-uom EQ '' THEN
      item.cons-uom = rm-rctd.pur-uom.

   ASSIGN
     lv-qty-uom = item.cons-uom
     lv-cost-uom = item.cons-uom
     v-dep = item.s-dep.

   FIND FIRST po-ordl WHERE
        po-ordl.company EQ rm-rctd.company AND
        po-ordl.po-no EQ INTEGER(rm-rctd.po-no) AND
        po-ordl.i-no EQ rm-rctd.i-no AND
        po-ordl.job-no EQ rm-rctd.job-no AND
        po-ordl.job-no2 EQ rm-rctd.job-no2 AND
        po-ordl.item-type EQ YES AND
        po-ordl.s-num EQ rm-rctd.s-num
        NO-LOCK NO-ERROR.

   IF AVAIL po-ordl THEN
   DO:
     ASSIGN
       v-len = po-ordl.s-len
       v-wid = po-ordl.s-wid
       v-bwt = 0.
     {rm/pol-dims.i}
   END.
   ELSE
   DO:
      FIND FIRST job WHERE
           job.company EQ cocode AND
           job.job-no EQ rm-rctd.job-no AND
           job.job-no2 EQ rm-rctd.job-no2
           NO-LOCK NO-ERROR.

      IF AVAIL job THEN
      DO:
         FIND FIRST job-mat NO-LOCK WHERE
              job-mat.company EQ rm-rctd.company AND
              job-mat.job EQ job.job AND
              job-mat.i-no EQ rm-rctd.i-no AND
              job-mat.frm EQ rm-rctd.s-num NO-ERROR.

         IF AVAIL job-mat THEN
            ASSIGN 
               v-len = job-mat.len
               v-wid = job-mat.wid
               v-bwt = job-mat.basis-w.
      END.

      IF v-len EQ 0 THEN v-len = IF AVAIL item THEN item.s-len ELSE 0.
      IF v-wid EQ 0 THEN v-wid = IF AVAIL item AND item.r-wid NE 0 THEN item.r-wid
                            ELSE IF AVAIL item THEN item.s-wid ELSE 0.
      IF v-bwt EQ 0 THEN v-bwt = IF AVAIL item THEN item.basis-w ELSE 0.

      ASSIGN
        lv-qty-uom = item.cons-uom
        lv-cost-uom = item.cons-uom.
   END.

   /* convert qty */
   RUN custom/convquom.p(gcompany,rm-rctd.pur-uom,lv-qty-uom,v-bwt,v-len,
                   INPUT v-wid,INPUT v-dep,INPUT rm-rctd.qty,OUTPUT lv-out-qty).

   /* convert cost */
   IF rm-rctd.cost-uom EQ 'L' THEN
      lv-out-cost = DEC(rm-rctd.cost) / lv-out-qty.
   ELSE
      RUN custom/convcuom.p(gcompany,rm-rctd.cost-uom,lv-cost-uom,
                            v-bwt,v-len,v-wid,v-dep,rm-rctd.cost,OUTPUT lv-out-cost).

   IF w-po.add-setup AND w-po.type NE "S" THEN
      lv-out-cost = lv-out-cost + (w-po.setup / lv-out-qty).

   ASSIGN
     rm-rctd.cost = lv-out-cost
     rm-rctd.cost-uom = lv-cost-uom
     rm-rctd.qty = lv-out-qty
     rm-rctd.pur-uom = lv-qty-uom.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-tag-proc C-Win 
PROCEDURE process-tag-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER ip-po-no AS INT NO-UNDO.
   DEF INPUT PARAMETER ip-po-line AS INT NO-UNDO.
   DEF INPUT PARAMETER ip-qty AS INT NO-UNDO.

   DEF VAR v-qty AS DEC NO-UNDO.
   DEF VAR v-overrun AS DEC NO-UNDO.
   DEF VAR choice2 AS LOG INIT YES NO-UNDO.

   FIND FIRST po-ordl WHERE
        po-ordl.company EQ cocode AND
        po-ordl.po-no EQ ip-po-no AND
        po-ordl.LINE EQ ip-po-line
        NO-LOCK NO-ERROR.

   IF NOT AVAIL po-ordl THEN
      LEAVE.

   EMPTY TEMP-TABLE w-po.

   FIND FIRST po-ord WHERE
        po-ord.company EQ cocode AND
        po-ord.po-no EQ po-ordl.po-no
        NO-LOCK.

   CREATE w-po.
   ASSIGN
     w-po.b-num = po-ordl.b-num
     w-po.cons-cost = po-ordl.cons-cost
     w-po.cons-qty = po-ordl.cons-qty
     w-po.cons-uom = po-ordl.cons-uom
     w-po.LINE     = po-ordl.LINE
     w-po.pr-uom = po-ordl.pr-uom
     w-po.i-name = po-ordl.i-name
     w-po.i-no = po-ordl.i-no
     w-po.job-no = po-ordl.job-no
     w-po.job-no2 = po-ordl.job-no2
     w-po.ord-no = po-ordl.ord-no
     w-po.ord-qty = po-ordl.ord-qty
     w-po.over-pct = po-ord.over-pct
     w-po.po-no = po-ord.po-no
     w-po.pr-uom = po-ordl.pr-uom
     w-po.s-len = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len
     w-po.s-num = po-ordl.s-num
     w-po.s-wid = po-ordl.s-wid
     w-po.tag-date = TODAY
     w-po.total-tags = 1
     w-po.rcpt-qty = ip-qty
     w-po.type = po-ord.type
     w-po.setup = po-ordl.setup.

   def var v-len like po-ordl.s-len no-undo.
   def var v-wid like po-ordl.s-len no-undo.
   def var v-dep like po-ordl.s-len no-undo. 
   def var v-bwt like po-ordl.s-len no-undo.
   def var lv-out-qty LIKE rm-rctd.qty no-undo.
   DEF VAR lv-qty-uom LIKE rm-rctd.pur-uom NO-UNDO.

   FIND FIRST ITEM where
        item.company eq po-ordl.company AND
        item.i-no eq po-ordl.i-no
        use-index i-no
        NO-LOCK no-error.

   assign
      lv-qty-uom  = po-ordl.pr-qty-uom
      v-len = po-ordl.s-len
      v-wid = po-ordl.s-wid
      v-bwt = 0.

   IF AVAIL ITEM THEN
   DO:
      {rm/pol-dims.i}
   END.

   IF w-po.cons-uom EQ lv-qty-uom THEN
      lv-out-qty = w-po.rcpt-qty.
   ELSE
      run custom/convquom.p (INPUT po-ordl.company,
                           INPUT w-po.cons-uom,
                           INPUT lv-qty-uom,
                           INPUT v-bwt,
                           INPUT v-len,
                           input v-wid,
                           input v-dep,
                           INPUT w-po.rcpt-qty,
                           output lv-out-qty).

   FIND FIRST po-ord WHERE
        po-ord.company EQ po-ordl.company AND
        po-ord.po-no EQ po-ordl.po-no
        NO-LOCK NO-ERROR.

   IF lv-out-qty LT po-ordl.ord-qty THEN
      w-po.cost = po-ordl.cost +
               (po-ordl.setup /
               ((po-ordl.t-cost - po-ordl.setup) / po-ordl.cost)).
   ELSE
      ASSIGN
         w-po.cost = po-ordl.cost
         w-po.add-setup = IF AVAIL po-ord AND po-ord.type NE "S" THEN YES
                          ELSE NO.

   RUN rm/getpocst.p (BUFFER po-ordl, w-po.pr-uom, INPUT-OUTPUT w-po.cost).

   RUN convert-vend-comp-curr(INPUT-OUTPUT w-po.cost).
   RUN convert-vend-comp-curr(INPUT-OUTPUT w-po.setup).
   RUN convert-vend-comp-curr(INPUT-OUTPUT w-po.cons-cost).

   FIND FIRST item WHERE
        item.company EQ cocode AND
        item.i-no EQ po-ordl.i-no
        NO-LOCK NO-ERROR.

   IF AVAIL item THEN
      ASSIGN
         w-po.loc = item.loc
         w-po.loc-bin = item.loc-bin
         w-po.cons-uom = item.cons-uom.

   ASSIGN
      v-overrun  = IF AVAIL po-ordl THEN po-ordl.over-pct
                   ELSE IF AVAIL po-ord  THEN po-ord.over-pct
                   ELSE IF AVAIL vend    THEN vend.over-pct
                   ELSE 0
      v-qty = po-ordl.ord-qty.

   IF w-po.cons-uom NE po-ordl.pr-qty-uom THEN
      RUN sys/ref/convquom.p(po-ordl.pr-qty-uom,
                             w-po.cons-uom, IF AVAIL ITEM THEN ITEM.basis-w ELSE 0,
                             w-po.s-len,  w-po.s-wid, IF AVAIL ITEM THEN item.s-dep ELSE 0,
                             v-qty, OUTPUT v-qty).

   w-po.overrun-qty = v-qty + (v-qty * (v-overrun / 100)).

   IF NOT checkWhsBin(cocode,w-po.loc,w-po.loc-bin) THEN
   DO:

     IF v-bin NE 'RMITEM' THEN
        ASSIGN
           w-po.loc = SUBSTR(v-bin,1,5)
           w-po.loc-bin = SUBSTR(v-bin,6).

     IF NOT checkWhsBin(cocode,w-po.loc,w-po.loc-bin) THEN
     DO:
        FIND FIRST rm-bin WHERE
             rm-bin.company EQ cocode AND
             rm-bin.loc EQ locode AND
             rm-bin.i-no EQ '' AND
             rm-bin.loc-bin NE ''
             NO-LOCK NO-ERROR.

        ASSIGN
          w-po.loc = IF AVAILABLE loc THEN loc.loc ELSE ''
          w-po.loc-bin = IF AVAILABLE rm-bin THEN rm-bin.loc-bin ELSE ''.
     END.
   END.

   IF NOT CAN-DO("SSLABEL,CentBox",v-loadtag) THEN
      w-po.total-tags = w-po.total-tags + 1.

   RUN tot-rec-qty-proc.

   IF CAN-FIND(FIRST tt-po WHERE
      tt-po.tot-rec-qty GT tt-po.overrun-qty) THEN
      MESSAGE "Receipt Qty Exceeds P.O Qty + Allowed Overrun%,  Continue?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice2.

   IF choice2 THEN
      RUN create-loadtag-proc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tot-rec-qty-proc C-Win 
PROCEDURE tot-rec-qty-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-qty-2 AS DEC NO-UNDO.

   EMPTY TEMP-TABLE tt-po.

   FIND FIRST tt-po WHERE
        tt-po.po-no EQ w-po.po-no AND
        tt-po.LINE  EQ w-po.LINE
        NO-ERROR.

   IF NOT AVAIL tt-po THEN
   DO:
      FIND FIRST po-ordl WHERE
           po-ordl.company EQ cocode AND
           po-ordl.po-no EQ w-po.po-no AND
           po-ordl.LINE EQ w-po.LINE
           NO-LOCK NO-ERROR.

      CREATE tt-po.
      ASSIGN tt-po.po-no = w-po.po-no
             tt-po.LINE  = w-po.LINE
             tt-po.cons-uom = w-po.cons-uom
             tt-po.tot-rec-qty = IF AVAIL po-ordl THEN po-ordl.t-rec-qty
                                 ELSE 0
             tt-po.overrun-qty = w-po.overrun-qty.

      IF AVAIL po-ordl AND w-po.cons-uom NE po-ordl.cons-uom THEN
      DO:
         FIND FIRST ITEM WHERE 
              ITEM.company EQ cocode AND
              ITEM.i-no EQ w-po.i-no
              NO-LOCK NO-ERROR.

         RUN sys/ref/convquom.p(po-ordl.cons-uom,
                                w-po.cons-uom, IF AVAIL ITEM THEN ITEM.basis-w ELSE 0,
                                w-po.s-len, w-po.s-wid, IF AVAIL ITEM THEN item.s-dep ELSE 0,
                                tt-po.tot-rec-qty, OUTPUT tt-po.tot-rec-qty).
      END.
   END.

   v-qty-2 = w-po.rcpt-qty * w-po.total-tags.

   IF w-po.cons-uom NE tt-po.cons-uom THEN
   DO:
      IF w-po.cons-uom NE tt-po.cons-uom THEN
      DO:
         FIND FIRST ITEM WHERE 
              ITEM.company EQ cocode AND
              ITEM.i-no EQ w-po.i-no
              NO-LOCK NO-ERROR.

         RUN sys/ref/convquom.p(w-po.cons-uom,
                                tt-po.cons-uom, IF AVAIL ITEM THEN ITEM.basis-w ELSE 0,
                                w-po.s-len, w-po.s-wid, IF AVAIL ITEM THEN item.s-dep ELSE 0,
                                v-qty-2, OUTPUT v-qty-2).
      END.
   END.

   tt-po.tot-rec-qty = tt-po.tot-rec-qty + v-qty-2.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-tag-proc C-Win 
PROCEDURE validate-tag-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF OUTPUT PARAMETER v-po-no AS INT NO-UNDO.
   DEF OUTPUT PARAMETER v-po-line AS INT NO-UNDO.
   DEF OUTPUT PARAMETER v-qty AS INT NO-UNDO.
   DEF OUTPUT PARAMETER op-error AS LOG NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      ASSIGN scr-vend-tag.

      ERROR-STATUS:ERROR = NO.

      v-po-no = INT(SUBSTR(scr-vend-tag,1,6)) NO-ERROR.

      IF ERROR-STATUS:ERROR THEN
         op-error = YES.

      IF op-error = NO THEN
      DO:
         v-po-line = INT(SUBSTR(scr-vend-tag,7,3)) NO-ERROR.

         IF ERROR-STATUS:ERROR THEN
            op-error = YES.
      END.

      IF op-error = NO THEN
      DO:
         v-qty = INT(SUBSTR(scr-vend-tag,10,5)) NO-ERROR.

         IF ERROR-STATUS:ERROR THEN
            op-error = YES.
      END.

      IF op-error = YES THEN
      DO:
         MESSAGE "Invalid Vendor Tag# Format."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.

         RETURN NO-APPLY.
      END.

      IF NOT CAN-FIND(FIRST po-ord WHERE
         po-ord.company EQ cocode AND
         po-ord.po-no EQ v-po-no) THEN
         DO:
            op-error = YES.
            MESSAGE "Invalid Purchase Order #."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN NO-APPLY.
         END.

      FIND FIRST po-ordl WHERE
           po-ordl.company EQ cocode AND
           po-ordl.po-no EQ v-po-no AND
           po-ordl.LINE EQ v-po-line
           NO-LOCK NO-ERROR.

      IF NOT AVAIL po-ordl THEN
      DO:
         op-error = YES.
         MESSAGE "Invalid Purchase Order Line #."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         RETURN NO-APPLY.
      END.
      ELSE IF po-ordl.item-type EQ NO THEN
      DO:
         op-error = YES.
         MESSAGE "Only Raw Materials can be Received."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkWhsBin C-Win 
FUNCTION checkWhsBin RETURNS LOGICAL
  ( ipCompany AS CHARACTER,ipLoc AS CHARACTER,ipLocBin AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN CAN-FIND(FIRST loc
                  WHERE loc.company EQ ipCompany
                    AND loc.loc EQ ipLoc) AND
         CAN-FIND(FIRST rm-bin
                  WHERE rm-bin.company EQ ipCompany
                    AND rm-bin.loc EQ ipLoc
                    AND rm-bin.i-no EQ ''
                    AND rm-bin.loc-bin EQ ipLocBin).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

