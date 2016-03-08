&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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
/*
{methods/defines/hndldefs.i}
*/
{touch/touchdef.i}

{custom/gcompany.i}
{custom/gloc.i}
def var x as int no-undo.
def var y as int no-undo.
def var v-num as int no-undo.
def var v-text as char extent 4 no-undo.
def var v-num-mats as int no-undo.

DEFINE var Frame_Handle AS WIDGET-HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 Scan_label BUTTON-1 btn_create ~
btn_view btn_reset Btn_Close 
&Scoped-Define DISPLAYED-OBJECTS Scan_label v-itemfg v-item-name v-po-no ~
v-ord-no v-job-no v-job-no2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Close 
     LABEL "C&Lose" 
     SIZE 17 BY 1.91.

DEFINE BUTTON btn_create 
     LABEL "&Create Receipt" 
     SIZE 19 BY 1.91.

DEFINE BUTTON btn_reset 
     LABEL "&Reset" 
     SIZE 18 BY 1.91.

DEFINE BUTTON btn_view 
     LABEL "&View Receipt" 
     SIZE 19 BY 1.91.

DEFINE BUTTON BUTTON-1 
     LABEL "Simulate Label Scan" 
     SIZE 24 BY 1.

DEFINE BUTTON BUTTON-2 
     LABEL "Simulate Job# Scan" 
     SIZE 24 BY 1.

DEFINE VARIABLE Scan_Job AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scan Job#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE Scan_label AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scan Tag#" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE v-item-name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE v-itemfg AS CHARACTER FORMAT "X(256)":U 
     LABEL "FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE v-job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE v-job-no2 AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE v-ord-no AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Order#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE v-po-no AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "PO#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 1.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Scan_label AT ROW 1.71 COL 17 COLON-ALIGNED
     BUTTON-1 AT ROW 2.43 COL 87.6
     v-itemfg AT ROW 3.38 COL 17 COLON-ALIGNED
     v-item-name AT ROW 3.38 COL 32 COLON-ALIGNED NO-LABEL
     BUTTON-2 AT ROW 4.57 COL 87.6
     v-po-no AT ROW 4.81 COL 17 COLON-ALIGNED
     v-ord-no AT ROW 4.81 COL 45 COLON-ALIGNED
     v-job-no AT ROW 6.24 COL 17 COLON-ALIGNED
     v-job-no2 AT ROW 6.24 COL 33 COLON-ALIGNED NO-LABEL
     Scan_Job AT ROW 6.24 COL 93 COLON-ALIGNED
     btn_create AT ROW 9.81 COL 12
     btn_view AT ROW 9.81 COL 36
     btn_reset AT ROW 9.81 COL 59
     Btn_Close AT ROW 9.81 COL 95
     "-" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 6.48 COL 31
     RECT-3 AT ROW 1.24 COL 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120 BY 11.48
         DEFAULT-BUTTON Btn_Close.


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
         TITLE              = "Scan FG Receipt"
         COLUMN             = 49.6
         ROW                = 14.62
         HEIGHT             = 11.48
         WIDTH              = 120
         MAX-HEIGHT         = 18.95
         MAX-WIDTH          = 137.2
         VIRTUAL-HEIGHT     = 18.95
         VIRTUAL-WIDTH      = 137.2
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON BUTTON-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN Scan_Job IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Scan_Job:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN v-item-name IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-itemfg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-job-no IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-job-no2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-ord-no IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-po-no IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Scan FG Receipt */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Scan FG Receipt */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close C-Win
ON CHOOSE OF Btn_Close IN FRAME DEFAULT-FRAME /* CLose */
DO:
 
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_create
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_create C-Win
ON CHOOSE OF btn_create IN FRAME DEFAULT-FRAME /* Create Receipt */
DO:
   RUN validate-tag NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   /* need to check fg-rcpth*/
   RUN CREATE-receipt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_reset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_reset C-Win
ON CHOOSE OF btn_reset IN FRAME DEFAULT-FRAME /* Reset */
DO:
   CLEAR FRAME {&FRAME-NAME} NO-PAUSE.
   scan_label:SCREEN-VALUE = "".
   APPLY "entry" TO scan_label.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_view
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_view C-Win
ON CHOOSE OF btn_view IN FRAME DEFAULT-FRAME /* View Receipt */
DO:
   RUN validate-tag NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN view-receipt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Simulate Label Scan */
DO:

/*
  assign Scan_Label:screen-value = "7375/200c 60x80/500.0/00737501".
*/
  /*
  FIND LAST po-ordl WHERE po-ordl.company = gcompany
                      AND po-ordl.deleted = no and
                          po-ordl.stat <> "C" and
                          po-ordl.item-type = NO NO-LOCK NO-ERROR.
  IF AVAIL po-ordl THEN scan_label = STRING(po-ordl.i-no,"x(15)") + "0001".
  ELSE assign Scan_Label:screen-value = "10x10x10       0001".

  assign v-text = ""
         v-num = 1.
  do x = 1 to length(trim(Scan_Label:screen-value)):
    if substring(Scan_Label:screen-value,x,1) ne "/" then
      assign v-text[v-num] = v-text[v-num] + substring(Scan_Label:screen-value,x,1).
    else
      v-num = v-num + 1.
  end.

  find first po-ordl where po-ordl.company eq gcompany
                       and po-ordl.po-no eq int(v-text[1])
                       and po-ordl.i-no eq v-text[2] 
                     no-lock no-error.
  if not avail po-ordl then
  do:
    message "Item is not on PO. Please Check Label." view-as alert-box.
    assign Scan_Label:screen-value = ""
           Scan_Job:screen-value = "".
  end.
  else
    APPLY "TAB".
    */
    FIND LAST loadtag WHERE loadtag.company = gcompany
                        AND loadtag.item-type = NO NO-LOCK NO-ERROR.
    IF AVAIL loadtag THEN 
        ASSIGN scan_label = loadtag.tag-no
               v-itemfg = loadtag.i-no
               v-item-name = loadtag.i-name
               v-po-no = loadtag.po-no
               v-ord-no = loadtag.ord-no
               v-job-no = loadtag.job-no
               v-job-no2 = loadtag.job-no2
               .
    DISPLAY scan_label v-itemfg v-item-name v-po-no v-ord-no v-job-no
            v-job-no2 WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Scan_label
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Scan_label C-Win
ON HELP OF Scan_label IN FRAME DEFAULT-FRAME /* Scan Tag# */
DO:
    DEF VAR lookup-recid AS RECID NO-UNDO.
    RUN fg/l-tag.w (gcompany,NO,FOCUS:SCREEN-VALUE, OUTPUT lookup-recid ).
    IF lookup-recid <> ? THEN DO:
       FIND loadtag WHERE RECID(loadtag) = lookup-recid NO-LOCK NO-ERROR.
       IF AVAIL loadtag THEN 
           ASSIGN scan_label:SCREEN-VALUE = loadtag.tag-no
                  v-itemfg:SCREEN-VALUE = loadtag.i-no
                  v-item-name:SCREEN-VALUE = loadtag.i-name
                  v-po-no:SCREEN-VALUE = STRING(loadtag.po-no)
                  v-ord-no:SCREEN-VALUE = STRING(loadtag.ord-no)
                  v-job-no:SCREEN-VALUE = STRING(loadtag.job-no)
                  v-job-no2:SCREEN-VALUE = STRING(loadtag.job-no2)
                  .
                             

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Scan_label C-Win
ON LEAVE OF Scan_label IN FRAME DEFAULT-FRAME /* Scan Tag# */
DO:
   /*DEF VAR lv-value AS cha NO-UNDO.

   ASSIGN scan_label.
   FIND FIRST loadtag WHERE loadtag.company = gcompany
                        AND loadtag.item-type = NO
                        AND loadtag.tag-no = scan_label  NO-ERROR.
   IF NOT AVAIL loadtag THEN DO:
       MESSAGE "Invalid Tag#. Try again." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO scan_label.
       RETURN NO-APPLY.
   END.
   IF loadtag.qty = 0 AND loadtag.partial = 0 THEN DO:
      RUN windows/d-msg.w ("Please Enter Partial Unit Count", INPUT-OUTPUT lv-value).
      IF lv-value <> "?" THEN loadtag.partial = INT(lv-value).
   END.
   FIND FIRST fg-rctd WHERE fg-rctd.company = gcompany
                        AND fg-rctd.i-no = loadtag.i-no
                        AND fg-rctd.tag = loadtag.tag-no
                        NO-LOCK NO-ERROR.
   IF AVAIL fg-rctd THEN DO:
     MESSAGE "This Item already received. Try another tag." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO scan_label.
     RETURN.

   END.
   */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Scan_label C-Win
ON VALUE-CHANGED OF Scan_label IN FRAME DEFAULT-FRAME /* Scan Tag# */
DO:
    IF SELF:SCREEN-VALUE = "RESET" THEN DO:       
       APPLY "choose" TO btn_reset.
       SELF:SCREEN-VALUE = "".
       RETURN NO-APPLY.
    END.
    ELSE IF SELF:SCREEN-VALUE = "CREATE" THEN DO:       
       APPLY "choose" TO btn_create.
       SELF:SCREEN-VALUE = "".
       RETURN NO-APPLY.
    END.
    ELSE IF SELF:SCREEN-VALUE = "VIEW" THEN DO:
       APPLY "choose" TO btn_view.
       SELF:SCREEN-VALUE = "".
       RETURN NO-APPLY.
    END.
    ELSE IF SELF:SCREEN-VALUE = "CLOSE" THEN DO:
       APPLY "choose" TO btn_close.
       SELF:SCREEN-VALUE = "".
       RETURN NO-APPLY.
    END.
  /*  ELSE DO:
        APPLY "leave" TO SELF.
        RETURN NO-APPLY.
    END.
 */   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

{custom/getcmpny.i}
{custom/getloc.i}

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
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-receipt C-Win 
PROCEDURE create-receipt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.

  lv-rno = 0.
  FOR EACH fg-rctd WHERE fg-rctd.company EQ gcompany NO-LOCK BY fg-rctd.r-no DESC:
      lv-rno = fg-rctd.r-no.
      LEAVE.
  END.

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.
  
  CREATE fg-rctd.
  assign fg-rctd.company = gcompany
         fg-rctd.r-no    = lv-rno + 1
         fg-rctd.rita-code = "R"
         fg-rctd.rct-date = TODAY
         fg-rctd.trans-time = TIME
         fg-rctd.units-pallet =  loadtag.case-bundle
         ext-cost = 0
         fg-rctd.po-no = string(loadtag.po-no)
         fg-rctd.job-no = loadtag.job-no
         fg-rctd.job-no2 = loadtag.job-no2
         fg-rctd.i-no = loadtag.i-no
         fg-rctd.i-name = loadtag.i-name
         fg-rctd.loc = loadtag.loc
         fg-rctd.loc-bin = loadtag.loc-bin
         fg-rctd.qty-case = loadtag.qty-case
         fg-rctd.cases-unit = LOADtag.case-bundle  /* pallet-count??*/
         fg-rctd.t-qty = loadtag.pallet-count /*qty*/
         fg-rctd.partial = loadtag.partial
         fg-rctd.tag = loadtag.tag-no
         fg-rctd.cases = loadtag.case-bundle
        /* fg-rctd.cost-uom
           fg-rctd.std-cost 
          */
         .

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
  DISPLAY Scan_label v-itemfg v-item-name v-po-no v-ord-no v-job-no v-job-no2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-3 Scan_label BUTTON-1 btn_create btn_view btn_reset Btn_Close 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-tag C-Win 
PROCEDURE validate-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-value AS cha NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN scan_label.
      FIND FIRST loadtag WHERE loadtag.company = gcompany
                        AND loadtag.item-type = NO
                        AND loadtag.tag-no = scan_label  NO-ERROR.
      IF NOT AVAIL loadtag THEN DO:
         MESSAGE "Invalid Tag#. Try again." VIEW-AS ALERT-BOX ERROR.
         scan_label:SCREEN-VALUE = "".
         APPLY "entry" TO scan_label.
         RETURN error.
      END.      
      FIND FIRST fg-rctd WHERE fg-rctd.company = gcompany
                           AND fg-rctd.i-no = loadtag.i-no
                           AND fg-rctd.tag = loadtag.tag-no
                           NO-LOCK NO-ERROR.
      IF AVAIL fg-rctd THEN DO:
         MESSAGE "This Item already received. Try another tag." VIEW-AS ALERT-BOX ERROR.
         scan_label:SCREEN-VALUE = "".
         APPLY "entry" TO scan_label.
         RETURN ERROR.
      END.
      IF loadtag.qty = 0 AND loadtag.partial = 0 THEN DO:
         RUN windows/d-msg.w ("Please Enter Partial Unit Count", INPUT-OUTPUT lv-value).
         IF lv-value <> "?" THEN loadtag.partial = INT(lv-value).
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE view-receipt C-Win 
PROCEDURE view-receipt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN fg/fg-rcpt.w.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

