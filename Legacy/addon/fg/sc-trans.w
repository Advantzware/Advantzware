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
{methods/defines/hndldefs.i}

{custom/gcompany.i}
{custom/gloc.i}
def var x as int no-undo.
def var y as int no-undo.
def var v-num as int no-undo.
def var v-text as char extent 4 no-undo.
def var v-num-mats as int no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 Scan_label To_Whse To_Bin btn_create ~
btn_view btn_reset Btn_Close 
&Scoped-Define DISPLAYED-OBJECTS Scan_label lv-Qty v-itemfg v-item-name ~
v-po-no v-ord-no v-job-no v-job-no2 From_Whse From_Bin To_Whse To_Bin 

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
     SIZE 21 BY 1.91.

DEFINE BUTTON btn_create 
     LABEL "&Create Transfer" 
     SIZE 19 BY 1.91.

DEFINE BUTTON btn_reset 
     LABEL "&Reset" 
     SIZE 18 BY 1.91.

DEFINE BUTTON btn_view 
     LABEL "&View Transfer" 
     SIZE 19 BY 1.91.

DEFINE BUTTON BUTTON-1 
     LABEL "Simulate Tag Scan" 
     SIZE 24 BY 2.62.

DEFINE BUTTON BUTTON-2  NO-FOCUS
     LABEL "Simulate Qty" 
     SIZE 24 BY 2.62.

DEFINE VARIABLE From_Bin AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE From_Tag AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tag" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE From_Whse AS CHARACTER FORMAT "X(256)":U 
     LABEL "Whse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-Qty AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Qty" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE Scan_label AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scan Tag#" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE To_Bin AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE To_Tag AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tag" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE To_Whse AS CHARACTER FORMAT "X(256)":U 
     LABEL "Whse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

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

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 1.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-2 AT ROW 9.1 COL 107
     Scan_label AT ROW 1.48 COL 17 COLON-ALIGNED
     BUTTON-1 AT ROW 2.19 COL 106
     lv-Qty AT ROW 3.14 COL 17 COLON-ALIGNED
     v-itemfg AT ROW 4.33 COL 17 COLON-ALIGNED
     v-item-name AT ROW 4.33 COL 32 COLON-ALIGNED NO-LABEL
     v-po-no AT ROW 5.52 COL 17 COLON-ALIGNED
     v-ord-no AT ROW 5.52 COL 45 COLON-ALIGNED
     v-job-no AT ROW 6.71 COL 17 COLON-ALIGNED
     v-job-no2 AT ROW 6.71 COL 33 COLON-ALIGNED NO-LABEL
     From_Whse AT ROW 8.62 COL 36 COLON-ALIGNED
     From_Bin AT ROW 8.62 COL 60 COLON-ALIGNED
     From_Tag AT ROW 8.62 COL 85 COLON-ALIGNED
     To_Whse AT ROW 11.24 COL 36 COLON-ALIGNED
     To_Bin AT ROW 11.24 COL 60 COLON-ALIGNED
     To_Tag AT ROW 11.24 COL 85 COLON-ALIGNED
     btn_create AT ROW 13.86 COL 14
     btn_view AT ROW 13.86 COL 38
     btn_reset AT ROW 13.86 COL 61
     Btn_Close AT ROW 13.86 COL 104
     "Transfer From" VIEW-AS TEXT
          SIZE 19 BY .95 AT ROW 8.62 COL 8
          FONT 6
     "Transfer To" VIEW-AS TEXT
          SIZE 19 BY .95 AT ROW 11.24 COL 8
          FONT 6
     RECT-6 AT ROW 1.24 COL 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 130.4 BY 15.62
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
         TITLE              = "Add Transfer"
         HEIGHT             = 15.62
         WIDTH              = 133.4
         MAX-HEIGHT         = 20.38
         MAX-WIDTH          = 133.4
         VIRTUAL-HEIGHT     = 20.38
         VIRTUAL-WIDTH      = 133.4
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
                                                                        */
/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-1:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN From_Bin IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN From_Tag IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       From_Tag:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN From_Whse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-Qty IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN To_Tag IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       To_Tag:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

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
ON END-ERROR OF C-Win /* Add Transfer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Add Transfer */
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
ON CHOOSE OF btn_create IN FRAME DEFAULT-FRAME /* Create Transfer */
DO:
   DEF VAR lv-value AS cha NO-UNDO.

   ASSIGN scan_label to_whse TO_bin.
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
   IF AVAIL fg-rctd AND fg-rctd.rita-code <> "P" THEN DO:
     MESSAGE "This Item already entered for Transfer. Try another tag." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO scan_label.
     RETURN.

   END.
   /* need to check fg-rcpth*/
   RUN CREATE-transfer.
   CLEAR FRAME {&FRAME-NAME} NO-PAUSE.
   APPLY "entry" TO scan_label.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_reset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_reset C-Win
ON CHOOSE OF btn_reset IN FRAME DEFAULT-FRAME /* Reset */
DO:
   CLEAR FRAME {&FRAME-NAME} NO-PAUSE.
   APPLY "entry" TO scan_label.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_view
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_view C-Win
ON CHOOSE OF btn_view IN FRAME DEFAULT-FRAME /* View Transfer */
DO:
   ASSIGN scan_label.
   FIND FIRST loadtag WHERE loadtag.company = gcompany
                        AND loadtag.item-type = NO
                        AND loadtag.tag-no = scan_label NO-LOCK NO-ERROR.
   IF NOT AVAIL loadtag THEN DO:
       MESSAGE "Invalid Tag#. Try again." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO scan_label.
       RETURN NO-APPLY.
   END.
   RUN view-transfer.
   CLEAR FRAME {&FRAME-NAME} NO-PAUSE.
   APPLY "entry" TO scan_label.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Simulate Tag Scan */
DO:

/*
  assign Scan_Label:screen-value = "7375/200c 60x80/500.0/00737501".
*/
  assign Scan_Label:screen-value = "7375/200c 60x80/500.0/00737501".

  assign v-text = ""
         v-num = 1.
  do x = 1 to length(trim(Scan_Label:screen-value)):
    if substring(Scan_Label:screen-value,x,1) ne "/" then
      assign v-text[v-num] = v-text[v-num] + substring(Scan_Label:screen-value,x,1).
    else
      v-num = v-num + 1.
  end.

  assign Scan_Label:screen-value = v-text[2]
         lv-qty:screen-value = v-text[3].

  find first item where item.company eq gcompany
                    and item.i-no eq v-text[2] 
                  no-lock no-error.
  find first rm-bin where rm-bin.company eq gcompany
                      and rm-bin.i-no eq v-text[2]
                      and rm-bin.tag eq v-text[4]
                    no-lock no-error.
  if not avail item or not avail rm-bin then
  do:
    message "Invalid Item/Tag Entered. Please Check Label." view-as alert-box.
    assign Scan_Label:screen-value = ""
           From_Whse:screen-value = "" 
           From_Bin:screen-value = "" 
           From_Tag:screen-value = "" 
           lv-qty:screen-value = "".
  end.
  else
  do:
    assign From_Whse:screen-value = rm-bin.loc
           From_Bin:screen-value = rm-bin.loc-bin 
           From_Tag:screen-value = rm-bin.tag.
    APPLY "TAB".
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Simulate Qty */
DO: 
/*
  assign Scan_Job:screen-value = "5414-0".
  
  assign v-job-no = ""
         v-job-no2 = "".
  do x = 1 to length(trim(Scan_Job:screen-value)):
    if substring(trim(Scan_Job:screen-value),x,1) ne "-" then
      v-job-no = v-job-no + substring(trim(Scan_Job:screen-value),x,1).
    else
      leave.
  end.

  y = x + 1.  
  do x = y to length(trim(Scan_Job:screen-value)):
      v-job-no2 = v-job-no2 + substring(trim(Scan_Job:screen-value),x,1).
  end.
  
  v-job-no =  fill(" ",6 - length(trim(string(v-job-no)))) +
                        trim(string(v-job-no)).

  find first job-mat where job-mat.company eq gcompany 
                       and job-mat.job-no eq v-job-no
                       and job-mat.job-no2 eq int(v-job-no2)
                       and job-mat.i-no eq v-text[2]
                     no-lock no-error.
  if not avail job-mat then
  do:
    message "Item is not on Job. Please Check Label and Job Number." view-as alert-box.
    assign Scan_Label:screen-value = ""
           Scan_Job:screen-value = "".
    APPLY 'TAB'.
  end.
  
  x = 1.
  FIND LAST rm-rcpt USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAILABLE rm-rcpt THEN
    x = rm-rcpt.r-no + 1.
  FIND LAST rm-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAILABLE rm-rcpth AND rm-rcpth.r-no >= x THEN
    x = rm-rcpth.r-no + 1.

  create rm-rcpt.
  assign rm-rcpt.r-no = x
         rm-rcpt.trans-date = today
         rm-rcpt.rita-code = "I"
         rm-rcpt.company = gcompany
         rm-rcpt.loc = gloc 
         rm-rcpt.i-no = v-text[2]
         rm-rcpt.job-no = v-job-no
         rm-rcpt.job-no2 = int(v-job-no2).
  
  create rm-rdtl.
  assign rm-rdtl.r-no       = rm-rcpt.r-no
         rm-rdtl.company    = gcompany
         rm-rdtl.loc        = gloc
         rm-rdtl.rita-code  = "I"
         rm-rdtl.job-no     = rm-rcpt.job-no
         rm-rdtl.job-no2    = rm-rcpt.job-no2.

  find first rm-bin where rm-bin.company eq gcompany
                      and rm-bin.i-no eq rm-rcpt.i-no
                      and rm-bin.loc eq gloc
                      and rm-bin.tag eq v-text[4] no-lock no-error.
  if avail rm-bin then
    assign rm-rdtl.tag = v-text[4]
           rm-rdtl.loc = rm-bin.loc
           rm-rdtl.loc-bin = rm-bin.loc-bin
           rm-rdtl.cost = rm-bin.cost
           rm-rdtl.qty = rm-bin.qty.

  /* Find out if we need Form/Blank/Pass */
  for each job-mat where job-mat.company eq gcompany
                      and job-mat.i-no eq rm-rcpt.i-no
                      and job-mat.job-no eq rm-rcpt.job-no
                      and job-mat.job-no2 eq rm-rcpt.job-no2
                    no-lock:
   
    assign v-num-mats = v-num-mats + 1.
  end.
  
  if v-num-mats eq 1 then
  do:
    find first job-mat where job-mat.company eq gcompany
                         and job-mat.i-no eq rm-rcpt.i-no
                         and job-mat.job-no eq rm-rcpt.job-no
                         and job-mat.job-no2 eq rm-rcpt.job-no2
                       no-lock no-error.
    if avail job-mat then
      assign rm-rdtl.s-num = job-mat.frm
             rm-rdtl.b-num = job-mat.blank-no
             rm-rdtl.pass = job-mat.pass.
  end.
  else
  do:
    ENABLE Scan_form with frame {&frame-name}.
    Scan_Form:hidden eq false.
    ENABLE Scan_Blank with frame {&frame-name}.
    Scan_Blank:hidden eq false.
    ENABLE Scan_Pass with frame {&frame-name}.
    Scan_Pass:hidden eq false.
  end.
  
  APPLY "TAB".
*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME From_Bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL From_Bin C-Win
ON LEAVE OF From_Bin IN FRAME DEFAULT-FRAME /* Bin */
DO:
  find first rm-bin where rm-bin.company eq gcompany
                      and rm-bin.loc     eq From_Whse:screen-value
                      and rm-bin.loc-bin eq From_Bin:screen-value
                      and rm-bin.i-no    eq Scan_Label:screen-value
                 no-lock no-error.
  if not avail rm-bin then
  do:
    message "Invalid Warehouse and/or Bin Entered. Please Try Again." view-as alert-box.
    assign From_Bin:screen-value = "".
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME From_Tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL From_Tag C-Win
ON LEAVE OF From_Tag IN FRAME DEFAULT-FRAME /* Tag */
DO:
  find first rm-bin where rm-bin.company eq gcompany
                      and rm-bin.loc     eq From_Whse:screen-value
                      and rm-bin.loc-bin eq From_Bin:screen-value
                      and rm-bin.i-no    eq Scan_Label:screen-value
                      and rm-bin.tag     eq From_Tag:screen-value
                 no-lock no-error.
  if not avail rm-bin then
  do:
    message "Tag Number does not Exist in this Location. Please Try Again." view-as alert-box.
    assign From_Tag:screen-value = "".
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME From_Whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL From_Whse C-Win
ON LEAVE OF From_Whse IN FRAME DEFAULT-FRAME /* Whse */
DO:
  find first loc where loc.company eq gcompany
                   and loc.loc eq From_Whse:screen-value
                 no-lock no-error.
  if not avail loc then
  do:
    message "Invalid Warehouse Entered. Please Try Again." view-as alert-box.
    assign From_Whse:screen-value = "".
  end.

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
                  
                  lv-qty:SCREEN-VALUE = STRING(loadtag.qty)
                  FROM_whse:SCREEN-VALUE = loadtag.loc
                  FROM_bin:SCREEN-VALUE = loadtag.loc-bin

                  .
                             

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Scan_label C-Win
ON VALUE-CHANGED OF Scan_label IN FRAME DEFAULT-FRAME /* Scan Tag# */
DO:
    IF SELF:SCREEN-VALUE = "RESET" THEN DO:
        APPLY "choose" TO btn_reset.
        RETURN NO-APPLY.
     END.
     IF SELF:SCREEN-VALUE = "CREATE" THEN DO:
        APPLY "choose" TO btn_create.
        RETURN NO-APPLY.
     END.
     IF SELF:SCREEN-VALUE = "VIEW" THEN DO:
        APPLY "choose" TO btn_view.
        RETURN NO-APPLY.
     END.
     IF SELF:SCREEN-VALUE = "CLOSE" THEN DO:
        APPLY "choose" TO btn_close.
        RETURN NO-APPLY.
     END.
    
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME To_Bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL To_Bin C-Win
ON LEAVE OF To_Bin IN FRAME DEFAULT-FRAME /* Bin */
DO:
/*
  assign rm-rdtl.b-num = int(Scan_Blank:screen-value).
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME To_Tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL To_Tag C-Win
ON LEAVE OF To_Tag IN FRAME DEFAULT-FRAME /* Tag */
DO:
/*
  assign rm-rdtl.pass = int(Scan_Pass:screen-value).
  assign Scan_Label:screen-value = ""
         Scan_Job:screen-value = ""
         Scan_Form:screen-value = ""
         Scan_Blank:screen-value = ""
         Scan_Pass:screen-value = "".
  
  APPLY 'TAB'.
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME To_Whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL To_Whse C-Win
ON LEAVE OF To_Whse IN FRAME DEFAULT-FRAME /* Whse */
DO:
  find first loc where loc.company eq gcompany
                   and loc.loc eq To_Whse:screen-value
                 no-lock no-error.
  if not avail loc then
  do:
    message "Invalid Warehouse Entered. Please Try Again." view-as alert-box.    
    assign To_Whse:screen-value = "".
    RETURN NO-APPLY.
  end.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-transfer C-Win 
PROCEDURE create-transfer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.

  
  /* Code placed here will execute PRIOR to standard behavior. */

  lv-rno = 0.
  FOR EACH fg-rctd WHERE fg-rctd.company EQ gcompany NO-LOCK
      BY fg-rctd.r-no DESC:
      lv-rno = fg-rctd.r-no.
      LEAVE.
  END.

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

  CREATE fg-rctd.
  assign fg-rctd.company = gcompany
         fg-rctd.r-no    = lv-rno + 1
         fg-rctd.rita-code = "T"
         fg-rctd.s-num  = 0
         fg-rctd.rct-date = today
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
         fg-rctd.loc2 = TO_whse
         fg-rctd.loc-bin2 = TO_bin
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
  DISPLAY Scan_label lv-Qty v-itemfg v-item-name v-po-no v-ord-no v-job-no 
          v-job-no2 From_Whse From_Bin To_Whse To_Bin 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-6 Scan_label To_Whse To_Bin btn_create btn_view btn_reset 
         Btn_Close 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE view-transfer C-Win 
PROCEDURE view-transfer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 RUN fg/fg-trans.w .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

