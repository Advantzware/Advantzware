&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: updvend#.w

  Description: Update Vendor Number

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
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
 cocode = gcompany
 locode = gloc.

DEF VAR v-process AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_vend-no end_vend-no ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_vend-no end_vend-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_vend-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Old Vendor #" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend-no AS CHARACTER FORMAT "x(8)":U 
     LABEL "New Vendor #" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 9.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_vend-no AT ROW 7.67 COL 37 COLON-ALIGNED
     end_vend-no AT ROW 10.52 COL 37 COLON-ALIGNED
     btn-process AT ROW 15.76 COL 21
     btn-cancel AT ROW 15.76 COL 53
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 17.52.

DEFINE FRAME FRAME-B
     "" VIEW-AS TEXT
          SIZE 3 BY .95 AT ROW 1.95 COL 1
          BGCOLOR 11 
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "" VIEW-AS TEXT
          SIZE 6.2 BY .95 AT ROW 2.91 COL 82.8
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 88.8 BY .95 AT ROW 1 COL 1
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 88.8 BY .95 AT ROW 3.76 COL 1
          BGCOLOR 11 
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "" VIEW-AS TEXT
          SIZE 7 BY .95 AT ROW 2.91 COL 1
          BGCOLOR 11 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 3.81
         BGCOLOR 11 .


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
         TITLE              = "Update Vendor Number"
         HEIGHT             = 17.71
         WIDTH              = 90.2
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 98.2
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
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
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Update Vendor Number */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Update Vendor Number */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-no C-Win
ON LEAVE OF begin_vend-no IN FRAME FRAME-A /* Old Vendor # */
DO:
  ASSIGN {&self-name}.

  {&self-name}:screen-value = CAPS({&self-name}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
    APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  IF NOT CAN-FIND(FIRST vend
                  WHERE vend.company EQ cocode
                    AND vend.vend-no EQ begin_vend-no) THEN DO:
    MESSAGE "You must enter a valid vendor number" VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO begin_vend-no.
    RETURN NO-APPLY.
  END.

  IF begin_vend-no EQ end_vend-no THEN
  DO:
    MESSAGE "Old and New Vendor #s are the same.  Cannot Process."
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY "entry" TO end_vend-no.
    RETURN NO-APPLY.
  END.

  IF CAN-FIND(FIRST vend
              WHERE vend.company EQ cocode
                AND vend.vend-no EQ end_vend-no) THEN DO:
    v-process = NO.

    MESSAGE "The new Vendor # already exists, merge old Vendor # into new Vendor #?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

    IF NOT v-process THEN RETURN NO-APPLY.
  END.

  v-process  = NO.

  MESSAGE "Are you sure you want change vendor number" TRIM(CAPS(begin_vend-no))
          "to" TRIM(CAPS(end_vend-no)) + "?"       
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

  IF v-process THEN RUN run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend-no C-Win
ON LEAVE OF end_vend-no IN FRAME FRAME-A /* New Vendor # */
DO:
  ASSIGN {&self-name}.

  {&self-name}:screen-value = CAPS({&self-name}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
    IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  RUN enable_UI.
  APPLY "entry" TO begin_vend-no.
  {methods/nowait.i}
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
  DISPLAY begin_vend-no end_vend-no 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_vend-no end_vend-no btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DEF VAR v-vend      LIKE vend.vend-no NO-UNDO.
DEF VAR v-new-vend  LIKE vend.vend-no NO-UNDO.
DEF BUFFER b-vend FOR vend.
DEF VAR old-rec-key AS CHAR NO-UNDO.

DEF BUFFER bf-vend FOR vend.  
DEF BUFFER bf-vend-new FOR vend. 
DEF BUFFER b-ap-inv FOR ap-inv.
DEF BUFFER b-ap-sel FOR ap-sel.
DEF BUFFER b-ap-dis FOR ap-dis.
DEF BUFFER b-po-ord FOR po-ord.
DEF BUFFER b-e-item-vend FOR e-item-vend.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b2-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b2-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.
DEF BUFFER b2-setup FOR reftable.
DEFINE BUFFER bf-vendItemCost FOR vendItemCost.

DISABLE TRIGGERS FOR LOAD OF vend.

ASSIGN
 v-vend     = begin_vend-no
 v-new-vend = end_vend-no.

SESSION:SET-WAIT-STATE("General").

FOR EACH ITEM WHERE
    ITEM.company EQ cocode AND
    ITEM.vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    DISPLAY ITEM.i-no WITH DOWN.

    ITEM.vend-no = v-new-vend.
END.

FOR EACH ITEM WHERE
    ITEM.company EQ cocode AND
    ITEM.vend2-no EQ v-vend
    EXCLUSIVE-LOCK:

    DISPLAY ITEM.i-no WITH DOWN.

    ITEM.vend2-no = v-new-vend.
END.

FOR EACH ef WHERE
    ef.company EQ cocode AND
    ef.vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    DISPLAY ef.est-no FORMAT "X(10)" WITH DOWN.

    ef.vend-no = v-new-vend.
END.

FOR EACH prep WHERE
    prep.company EQ cocode AND
    prep.vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    DISPLAY prep.CODE FORMAT "X(5)" WITH DOWN.
    prep.vend-no = v-new-vend.
END.

FOR EACH ap-ledger WHERE
    ap-ledger.company EQ cocode AND
    ap-ledger.vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    DISPLAY "AP Ledger Tran # " + STRING(ap-ledger.trnum)  WITH DOWN.
    ap-ledger.vend-no = v-new-vend.
END.

FOR EACH ap-chk WHERE
    ap-chk.company EQ cocode AND
    ap-chk.vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    DISPLAY "AP Check # " + STRING(ap-chk.check-no)  WITH DOWN.
    ap-chk.vend-no = v-new-vend.
END.

FOR EACH aphist WHERE
    aphist.company EQ cocode AND
    aphist.vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    aphist.vend-no = v-new-vend.
END.

FOR EACH ap-inv WHERE
    ap-inv.company EQ cocode AND
    ap-inv.vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    FIND FIRST bf-vend WHERE
          bf-vend.company EQ ap-inv.company AND
          bf-vend.vend-no EQ v-vend
          NO-LOCK NO-ERROR.

    old-rec-key = bf-vend.rec_key .

    DISPLAY "AP Invoice # " +  ap-inv.inv-no WITH DOWN.

    FOR EACH ap-invl WHERE
        ap-invl.i-no EQ ap-inv.i-no
        EXCLUSIVE-LOCK:

        ap-invl.vend-no = v-new-vend.
    END.

    IF NOT CAN-FIND(FIRST b-ap-inv WHERE
       b-ap-inv.company EQ ap-inv.company AND
       b-ap-inv.vend-no EQ v-new-vend AND
       b-ap-inv.inv-no EQ ap-inv.inv-no) THEN DO:
        ap-inv.vend-no = v-new-vend.

/*        FIND FIRST bf-vend-new WHERE                    */
/*            bf-vend-new.company EQ ap-inv.company AND   */
/*            bf-vend-new.vend-no EQ v-new-vend           */
/*            NO-LOCK NO-ERROR.                           */
/*                                                        */
/*        FOR EACH attach WHERE                           */
/*            attach.company = ap-inv.company AND         */
/*            attach.rec_key = old-rec-key EXCLUSIVE-LOCK:*/
/*                                                        */
/*           attach.rec_key = bf-vend-new.rec_key .       */
/*        END.                                            */
    END.
    ELSE DO:
       DELETE ap-inv.
    END.
END.

FOR EACH ap-sel WHERE
    ap-sel.company EQ cocode AND
    ap-sel.vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    IF NOT CAN-FIND(FIRST b-ap-sel WHERE
       b-ap-sel.company EQ ap-sel.company AND
       b-ap-sel.vend-no EQ v-new-vend AND
       b-ap-sel.inv-no EQ ap-sel.inv-no) THEN
       ap-sel.vend-no = v-new-vend.
    ELSE
       DELETE ap-sel.
END.

FOR EACH ap-pay WHERE
    ap-pay.company EQ cocode AND
    ap-pay.vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    DISPLAY "AP Pay Check # " + STRING(ap-pay.check-no) WITH DOWN.

    FOR EACH ap-payl WHERE
        ap-payl.c-no EQ ap-pay.c-no
        EXCLUSIVE-LOCK:

        ap-payl.vend-no = v-new-vend.
    END.

    ap-pay.vend-no = v-new-vend.
END.

FOR EACH ap-dis WHERE
    ap-dis.company EQ cocode AND
    ap-dis.vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    IF NOT CAN-FIND(FIRST b-ap-dis WHERE
       b-ap-dis.company EQ cocode AND
       b-ap-dis.vend-no EQ v-new-vend AND
       b-ap-dis.bank-code EQ ap-dis.bank-code AND
       b-ap-dis.check-no EQ ap-dis.check-no) THEN
       ap-dis.vend-no = v-new-vend.
    ELSE
       DELETE ap-dis.
END.

FOR EACH fg-hist WHERE
    fg-hist.company EQ cocode AND
    fg-hist.vend-number EQ v-vend
    EXCLUSIVE-LOCK:

    fg-hist.vend-number = v-new-vend.
END.

FOR EACH itemfg WHERE
    itemfg.company EQ cocode AND
    itemfg.vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    DISPLAY itemfg.i-no WITH DOWN.

    itemfg.vend-no = v-new-vend.
END.

FOR EACH itemfg WHERE
    itemfg.company EQ cocode AND
    itemfg.vend2-no EQ v-vend
    EXCLUSIVE-LOCK:

    DISPLAY itemfg.i-no WITH DOWN.

    itemfg.vend2-no = v-new-vend.
END.

FOR EACH oe-ordl WHERE
    oe-ordl.company EQ cocode AND
    oe-ordl.vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    DISPLAY "Order # " + STRING(oe-ordl.ord-no) WITH DOWN.

    oe-ordl.vend-no = v-new-vend.
END.

FOR EACH rm-rcpth WHERE
    rm-rcpth.company EQ cocode AND
    rm-rcpth.vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    rm-rcpth.vend-no = v-new-vend.
END.

FOR EACH po-ord WHERE
    po-ord.company EQ cocode AND
    po-ord.vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    DISPLAY "P.O. # " + STRING(po-ord.po-no) WITH DOWN.

    IF NOT CAN-FIND(FIRST b-po-ord WHERE
       b-po-ord.company EQ cocode AND
       b-po-ord.vend-no EQ v-new-vend AND
       b-po-ord.po-no   EQ po-ord.po-no) THEN
       DO:
            po-ord.vend-no = v-new-vend.
            FOR EACH po-ordl EXCLUSIVE-LOCK
                WHERE po-ordl.company EQ po-ord.company
                  AND po-ordl.po-no EQ po-ord.po-no:
                po-ordl.vend-no = v-new-vend.
            END.
            RELEASE po-ordl.
       END.
    ELSE
       DELETE po-ord.
END.

FOR EACH po-ord WHERE
    po-ord.company EQ cocode AND
    po-ord.cust-no EQ "" AND
    po-ord.TYPE EQ "D" AND
    po-ord.ship-id EQ v-vend
    EXCLUSIVE-LOCK:

    DISPLAY "P.O. # " + STRING(po-ord.po-no) WITH DOWN.
    po-ord.ship-id = v-new-vend.
    FOR EACH po-ordl EXCLUSIVE-LOCK
        WHERE po-ordl.company EQ po-ord.company
          AND po-ordl.po-no EQ po-ord.po-no:
          po-ordl.vend-no = v-new-vend.
    END.
    RELEASE po-ordl.
END.

FOR EACH po-rcpts WHERE
    po-rcpts.company EQ cocode AND
    po-rcpts.vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    DISPLAY "P.O. # " + STRING(po-ord.po-no) WITH DOWN.

    po-rcpts.vend-no = v-new-vend.
END.

FOR EACH e-item-vend WHERE
    e-item-vend.company EQ cocode AND
    e-item-vend.i-no GT "" AND
    e-item-vend.vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    DISPLAY "Vender Cost for Item # " + STRING(e-item-vend.i-no) WITH DOWN.


    IF NOT CAN-FIND(FIRST b-e-item-vend WHERE
       b-e-item-vend.company EQ cocode AND
       b-e-item-vend.i-no EQ e-item-vend.i-no AND
       b-e-item-vend.vend-no EQ v-new-vend) THEN
       e-item-vend.vend-no = v-new-vend.
    ELSE
       DELETE e-item-vend.

END.

FOR EACH e-itemfg-vend WHERE
    e-itemfg-vend.company EQ cocode AND
    e-itemfg-vend.i-no GT "" AND
    e-itemfg-vend.vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    DISPLAY "Vender Cost for Item # " + STRING(e-itemfg-vend.i-no) WITH DOWN.

    e-itemfg-vend.vend-no = v-new-vend.
END.

FOR EACH vendItemCost
        WHERE vendItemCost.company EQ cocode
        AND vendItemCost.vendorID  EQ v-vend         
        NO-LOCK:
        
        FIND FIRST bf-vendItemCost NO-LOCK 
            WHERE bf-vendItemCost.company EQ vendItemCost.Company
            AND bf-vendItemCost.itemType EQ vendItemCost.itemType
            AND bf-vendItemCost.itemID EQ vendItemCost.itemID
            AND bf-vendItemCost.vendorID EQ v-new-vend
            AND bf-vendItemCost.customerID EQ vendItemCost.customerID
            AND bf-vendItemCost.estimateNo EQ vendItemCost.estimateNo
            AND bf-vendItemCost.formNo EQ vendItemCost.formNo
            AND bf-vendItemCost.blankNo EQ vendItemCost.blankNo
            AND bf-vendItemCost.expirationDate EQ date(vendItemCost.expirationDate)
            AND bf-vendItemCost.effectiveDate EQ date(vendItemCost.effectiveDate)
            NO-ERROR.
        IF NOT AVAILABLE bf-vendItemCost then
        do:
            FIND CURRENT vendItemCost EXCLUSIVE-LOCK NO-ERROR.
            vendItemCost.vendorID = v-new-vend.
            FIND CURRENT vendItemCost NO-LOCK NO-ERROR.
        END.
        ELSE DO:
           FIND FIRST bf-vendItemCost EXCLUSIVE-LOCK
                WHERE ROWID(bf-vendItemCost) EQ rowid(vendItemCost) NO-ERROR.
           IF AVAILABLE bf-vendItemCost THEN
           DELETE bf-vendItemCost.           
        END.         
END.

FOR EACH EDAPCheck WHERE
    EDAPCheck.company EQ cocode AND
    EDAPCheck.Vendor EQ v-vend
    EXCLUSIVE-LOCK:

    EDAPCheck.Vendor = v-new-vend.
END.

FOR EACH EDAPCheck WHERE
    EDAPCheck.company EQ cocode AND
    EDAPCheck.From-Vendor EQ v-vend
    EXCLUSIVE-LOCK:

    EDAPCheck.From-Vendor = v-new-vend.
END.

FOR EACH EDCAT WHERE
    EDCAT.Vn-code EQ v-vend
    EXCLUSIVE-LOCK:

    EDCAT.Vn-code = v-new-vend.
END.

FOR EACH EDCAT WHERE
    EDCAT.Vendor EQ v-vend
    EXCLUSIVE-LOCK:

    EDCAT.Vendor = v-new-vend.
END.

FOR EACH EDIVTran WHERE
    EDIVTran.company EQ cocode AND
    EDIVTran.Vn-Code EQ v-vend
    EXCLUSIVE-LOCK:

    EDIVTran.Vn-Code = v-new-vend.
END.

FOR EACH EDIVTran WHERE
    EDIVTran.company EQ cocode AND
    EDIVTran.Vendor EQ v-vend
    EXCLUSIVE-LOCK:

    EDIVTran.Vendor = v-new-vend.
END.

FOR EACH EDMast WHERE
    EDMast.we-vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    EDMast.we-vend-no = v-new-vend.
END.

FOR EACH EDMast WHERE
    EDMast.Vendor EQ v-vend
    EXCLUSIVE-LOCK:

    EDMast.Vendor = v-new-vend.
END.

FOR EACH EDPOTran WHERE
    EDPOTran.vn-code EQ v-vend
    EXCLUSIVE-LOCK:

    EDPOTran.vn-code = v-new-vend.
END.

FOR EACH EDSHTran WHERE
    EDSHTran.vn-code EQ v-vend
    EXCLUSIVE-LOCK:

    EDSHTran.vn-code = v-new-vend.
END.

FOR EACH sys-ctrl-shipto WHERE
    sys-ctrl-shipto.company EQ cocode AND
    sys-ctrl-shipto.cust-vend EQ NO AND
    sys-ctrl-shipto.cust-vend-no EQ v-vend
    EXCLUSIVE-LOCK:

    sys-ctrl-shipto.cust-vend-no = v-new-vend.
END.

FIND FIRST vend WHERE
     vend.company EQ cocode AND
     vend.vend-no EQ v-vend
     EXCLUSIVE-LOCK NO-ERROR.

  
  IF AVAIL vend THEN DO:

    /* Used for attach records below */
    old-rec-key = vend.rec_key .
      
    FIND FIRST b-vend WHERE
         b-vend.company EQ cocode AND
         b-vend.vend-no EQ v-new-vend
        NO-LOCK NO-ERROR.                   
    IF AVAIL b-vend THEN DO:
        
      FOR EACH attach EXCLUSIVE-LOCK WHERE
        attach.company = cocode AND
        attach.rec_key = old-rec-key :

        attach.rec_key = b-vend.rec_key .
      END.    
                
      FOR EACH notes WHERE notes.rec_key EQ vend.rec_key:
        notes.rec_key = b-vend.rec_key.
      END.

      FOR EACH reftable WHERE
          reftable.reftable EQ "vend.poexport" AND
          reftable.company  EQ vend.company AND
          reftable.loc      EQ "" AND
          reftable.code     EQ vend.vend-no:
          DELETE reftable.
      END.

      DELETE vend.
    END.
    ELSE vend.vend-no = v-new-vend.
  END.


            
SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Complete..." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

