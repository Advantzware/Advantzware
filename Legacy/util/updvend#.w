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

assign
 cocode = gcompany
 locode = gloc.

def var v-process as log no-undo.

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
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

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
  assign {&self-name}.
  
  {&self-name}:screen-value = caps({&self-name}).
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
  if not can-find(first vend
                  where vend.company eq cocode
                    and vend.vend-no eq begin_vend-no) then do:
    message "You must enter a valid vendor number" view-as alert-box error.
    apply "entry" to begin_vend-no.
    return no-apply.
  end.
  
  IF begin_vend-no EQ end_vend-no THEN
  DO:
    MESSAGE "Old and New Vendor #s are the same.  Cannot Process."
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    apply "entry" to end_vend-no.
    return no-apply.
  END.

  if can-find(first vend
              where vend.company eq cocode
                and vend.vend-no eq end_vend-no) then do:
    v-process = no.
    
    message "The new Vendor # already exists, merge old Vendor # into new Vendor #?"
            view-as alert-box question button yes-no update v-process.
            
    if not v-process then return no-apply.
  end.

  v-process  = no.
   
  message "Are you sure you want change vendor number" trim(caps(begin_vend-no))
          "to" trim(caps(end_vend-no)) + "?"       
          view-as alert-box question button yes-no update v-process.
        
  if v-process then run run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend-no C-Win
ON LEAVE OF end_vend-no IN FRAME FRAME-A /* New Vendor # */
DO:
  assign {&self-name}.
  
  {&self-name}:screen-value = caps({&self-name}).
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
  apply "entry" to begin_vend-no.
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
def var v-vend      like vend.vend-no NO-UNDO.
def var v-new-vend  like vend.vend-no NO-UNDO.
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

DISABLE TRIGGERS FOR LOAD OF vend.

assign
 v-vend     = begin_vend-no
 v-new-vend = end_vend-no.

session:set-wait-state("General").
    
for each ITEM where
    ITEM.company eq cocode AND
    ITEM.vend-no eq v-vend
    EXCLUSIVE-LOCK:

    DISPLAY ITEM.i-no WITH DOWN.
        
    ITEM.vend-no = v-new-vend.
end.

for each ITEM where
    ITEM.company eq cocode AND
    ITEM.vend2-no eq v-vend
    EXCLUSIVE-LOCK:

    DISPLAY ITEM.i-no WITH DOWN.
        
    ITEM.vend2-no = v-new-vend.
end.

for each ef where
    ef.company eq cocode AND
    ef.vend-no eq v-vend
    EXCLUSIVE-LOCK:

    DISPLAY ef.est-no FORMAT "X(10)" WITH DOWN.
        
    ef.vend-no = v-new-vend.
end.

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
       b-ap-inv.inv-no EQ ap-inv.inv-no) THEN do:
        ap-inv.vend-no = v-new-vend.

        FIND FIRST bf-vend-new WHERE
            bf-vend-new.company EQ ap-inv.company AND
            bf-vend-new.vend-no EQ v-new-vend
            NO-LOCK NO-ERROR.

        FOR EACH attach WHERE
            attach.company = ap-inv.company and
            attach.rec_key = old-rec-key EXCLUSIVE-LOCK:
            
           attach.rec_key = bf-vend-new.rec_key .
       END.
    END.
    ELSE do:
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

for each itemfg where
    itemfg.company eq cocode AND
    itemfg.vend-no eq v-vend
    EXCLUSIVE-LOCK:

    DISPLAY itemfg.i-no WITH DOWN.
        
    itemfg.vend-no = v-new-vend.
end.

for each itemfg where
    itemfg.company eq cocode AND
    itemfg.vend2-no eq v-vend
    EXCLUSIVE-LOCK:

    DISPLAY itemfg.i-no WITH DOWN.
        
    itemfg.vend2-no = v-new-vend.
end.

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
       po-ord.vend-no = v-new-vend.
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
        
    FIND FIRST b-qty WHERE
         b-qty.reftable = "vend-qty" AND
         b-qty.company = e-item-vend.company AND
         b-qty.CODE    = e-item-vend.i-no AND
         b-qty.code2   = v-vend
         EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

    IF AVAIL b-qty THEN
    DO:
       FIND FIRST b2-qty WHERE
            b2-qty.reftable = "vend-qty" AND
            b2-qty.company = e-item-vend.company AND
            b2-qty.CODE    = e-item-vend.i-no AND
            b2-qty.code2   = v-new-vend
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

       IF AVAIL b2-qty THEN
          DELETE b-qty.
       ELSE
          b-qty.code2 = v-new-vend.
    END.

    FIND FIRST b-cost WHERE
         b-cost.reftable = "vend-cost" AND
         b-cost.company = e-item-vend.company AND
         b-cost.CODE    = e-item-vend.i-no AND
         b-cost.code2   = v-vend
         EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

    IF AVAIL b-cost THEN
    DO:
       FIND FIRST b2-cost WHERE
            b2-cost.reftable = "vend-cost" AND
            b2-cost.company = e-item-vend.company AND
            b2-cost.CODE    = e-item-vend.i-no AND
            b2-cost.code2   = v-new-vend
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

       IF AVAIL b2-cost THEN
          DELETE b-cost.
       ELSE
          b-cost.code2 = v-new-vend.
    END.

    FIND FIRST b-setup WHERE
         b-setup.reftable = "vend-setup" AND
         b-setup.company = e-item-vend.company AND
         b-setup.CODE    = e-item-vend.i-no AND
         b-setup.code2   = v-vend
         EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

    IF AVAIL b-setup THEN
    DO:
       FIND FIRST b2-setup WHERE
            b2-setup.reftable = "vend-setup" AND
            b2-setup.company = e-item-vend.company AND
            b2-setup.CODE    = e-item-vend.i-no AND
            b2-setup.code2   = v-new-vend
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

       IF AVAIL b2-setup THEN
          DELETE b-setup.
       ELSE
          b-setup.code2 = v-new-vend.
    END.

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

find first vend WHERE
     vend.company eq cocode AND
     vend.vend-no eq v-vend
     EXCLUSIVE-LOCK no-error.

  if avail vend then do:
    find first b-vend WHERE
         b-vend.company eq cocode AND
         b-vend.vend-no eq v-new-vend
        no-lock no-error.                   
    if avail b-vend then do:
      for each notes where notes.rec_key eq vend.rec_key:
        notes.rec_key = b-vend.rec_key.
      end.

      FOR EACH reftable WHERE
          reftable.reftable EQ "vend.poexport" AND
          reftable.company  EQ vend.company AND
          reftable.loc      EQ "" AND
          reftable.code     EQ vend.vend-no:
          DELETE reftable.
      END.

      delete vend.
    end.
    else vend.vend-no = v-new-vend.
  end.

session:set-wait-state("").

message trim(c-win:title) + " Process Complete..." view-as alert-box.
    
apply "close" to this-procedure.
  
/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

