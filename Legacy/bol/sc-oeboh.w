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
def var v-job-no like rm-rcpt.job-no no-undo.
def var v-job-no2 as char format "x(2)" no-undo.

DEFINE var Frame_Handle AS WIDGET-HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Scan_Tag BUTTON-1 Btn_Next Btn_view ~
Btn_reset Btn_Close RECT-9 
&Scoped-Define DISPLAYED-OBJECTS Scan_Tag Scan_order v-cust-no v-cust-name ~
v_itemfg v_count 

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
     SIZE 18 BY 1.91.

DEFINE BUTTON Btn_Next 
     LABEL "Next &BOL" 
     SIZE 15 BY 1.91.

DEFINE BUTTON Btn_order 
     LABEL "Next &Order" 
     SIZE 15 BY 1.91.

DEFINE BUTTON Btn_reset 
     LABEL "&Reset" 
     SIZE 15 BY 1.91.

DEFINE BUTTON Btn_view 
     LABEL "&View BOL" 
     SIZE 15 BY 1.91.

DEFINE BUTTON BUTTON-1 
     LABEL "Simulate Tag  Scan" 
     SIZE 24 BY 1.

DEFINE BUTTON BUTTON-2 
     LABEL "Simulate Job# Scan" 
     SIZE 24 BY 1.

DEFINE VARIABLE Scan_order AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scan Order#" 
     VIEW-AS FILL-IN 
     SIZE 21.4 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE Scan_Tag AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scan Tag#" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE v-cust-name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE v-cust-no AS CHARACTER FORMAT "X(256)":U 
     LABEL "Customer" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE v_count AS CHARACTER FORMAT "X(4)":U 
     LABEL "Count" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE v_itemfg AS CHARACTER FORMAT "X(15)":U 
     LABEL "Fg Item#" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 74 BY 1.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Scan_Tag AT ROW 1.48 COL 15 COLON-ALIGNED
     BUTTON-1 AT ROW 1.48 COL 85
     Scan_order AT ROW 3.14 COL 15 COLON-ALIGNED
     v-cust-no AT ROW 4.33 COL 15 COLON-ALIGNED
     v-cust-name AT ROW 4.33 COL 36.4 COLON-ALIGNED NO-LABEL
     v_itemfg AT ROW 5.76 COL 15 COLON-ALIGNED
     v_count AT ROW 5.76 COL 60 COLON-ALIGNED
     BUTTON-2 AT ROW 9.1 COL 85
     Btn_Next AT ROW 11.71 COL 13
     Btn_view AT ROW 11.71 COL 31
     Btn_reset AT ROW 11.71 COL 50
     Btn_order AT ROW 11.71 COL 73
     Btn_Close AT ROW 11.71 COL 91
     RECT-9 AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.8 BY 13.19
         DEFAULT-BUTTON Btn_Next.


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
         TITLE              = "Scan  Order"
         COLUMN             = 35.4
         ROW                = 10.24
         HEIGHT             = 13.19
         WIDTH              = 110.8
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
                                                                        */
/* SETTINGS FOR BUTTON Btn_order IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Btn_order:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN Scan_order IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-cust-name IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-cust-no IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_count IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_itemfg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Scan  Order */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Scan  Order */
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
    /*
  if avail rm-rcpt and rm-rcpt.i-no ne "" then
  do:
    find first job-mat where job-mat.company eq gcompany
                         and job-mat.i-no eq rm-rcpt.i-no
                         and job-mat.job-no eq rm-rcpt.job-no
                         and job-mat.job-no2 eq rm-rcpt.job-no2
                         and job-mat.frm eq int(Scan_Form:screen-value)
                         and job-mat.blank-no eq int(Scan_Blank:screen-value)
                         and job-mat.pass eq int(Scan_Pass:screen-value)
                       no-lock no-error.
    if not avail job-mat then
    do:
      message "Job Material Record not Found.  Please Try Again." view-as alert-box.
      for each rm-rdtl where rm-rdtl.company eq gcompany
                         and rm-rdtl.r-no eq rm-rcpt.r-no:
        delete rm-rdtl.
      end.
      delete rm-rcpt.
    end.
  end.
  */
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Next C-Win
ON CHOOSE OF Btn_Next IN FRAME DEFAULT-FRAME /* Next BOL */
DO:
    /* need to create records oe-bolh,oe-boll...*/
    FIND FIRST oe-ord WHERE oe-ord.company = gcompany
                        AND oe-ord.ord-no = int(scan_order:SCREEN-VALUE)
                        NO-LOCK NO-ERROR.
    IF NOT AVAIL oe-ord THEN DO:
       MESSAGE "Invalid Order#" VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO scan_order.
       RETURN NO-APPLY.
    END.

    FIND FIRST cust WHERE cust.company = gcompany
                      AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.
    ASSIGN v-cust-no = oe-ord.cust-no
           v-cust-name = IF AVAIL cust THEN cust.NAME ELSE "".
    DISP v-cust-no v-cust-name WITH FRAME {&FRAME-NAME}.
/*
    FIND FIRST oe-ordl WHERE oe-ordl.ord-no = INT(scan_order:SCREEN-VALUE)
                        AND oe-ordl.i-no = SELF:SCREEN-VALUE
                        NO-LOCK NO-ERROR.
    IF NOT AVAIL oe-ordl THEN DO:
       MESSAGE "Invalid FG Item#." VIEW-AS ALERT-BOX.
       APPLY "entry" TO scan_itemfg.
       RETURN NO-APPLY.
    END.
*/
    MESSAGE "Create New BOL?" VIEW-AS ALERT-BOX QUESTION BUTTON yes-no UPDATE ll-ans AS LOG.

    IF ll-ans THEN RUN create-bol.

    CLEAR FRAME {&FRAME-NAME} NO-PAUSE.
    APPLY "entry" TO scan_order.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_order
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_order C-Win
ON CHOOSE OF Btn_order IN FRAME DEFAULT-FRAME /* Next Order */
DO:
    /*
    /* need to create records oe-bolh,oe-boll...*/
    FIND FIRST oe-ord WHERE oe-ord.company = gcompany
                        AND oe-ord.ord-no = int(scan_order:SCREEN-VALUE)
                        NO-LOCK NO-ERROR.
    IF NOT AVAIL oe-ord THEN DO:
       MESSAGE "Invalid Order#" VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO scan_order.
       RETURN NO-APPLY.
    END.

    FIND FIRST cust WHERE cust.company = gcompany
                      AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.
    ASSIGN v-cust-no = oe-ord.cust-no
           v-cust-name = IF AVAIL cust THEN cust.NAME ELSE "".
    DISP v-cust-no v-cust-name WITH FRAME {&FRAME-NAME}.
/*
    FIND FIRST oe-ordl WHERE oe-ordl.ord-no = INT(scan_order:SCREEN-VALUE)
                        AND oe-ordl.i-no = SELF:SCREEN-VALUE
                        NO-LOCK NO-ERROR.
    IF NOT AVAIL oe-ordl THEN DO:
       MESSAGE "Invalid FG Item#." VIEW-AS ALERT-BOX.
       APPLY "entry" TO scan_itemfg.
       RETURN NO-APPLY.
    END.
*/
    MESSAGE "Create New BOL?" VIEW-AS ALERT-BOX QUESTION BUTTON yes-no UPDATE ll-ans AS LOG.

    IF ll-ans THEN RUN create-bol.

    CLEAR FRAME {&FRAME-NAME} NO-PAUSE.
    APPLY "entry" TO scan_order.
   */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_reset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_reset C-Win
ON CHOOSE OF Btn_reset IN FRAME DEFAULT-FRAME /* Reset */
DO:
    CLEAR FRAME {&FRAME-NAME} NO-PAUSE.
    APPLY "entry" TO scan_order.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_view
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_view C-Win
ON CHOOSE OF Btn_view IN FRAME DEFAULT-FRAME /* View BOL */
DO:
    ASSIGN scan_order v_itemfg.   
    RUN view-bol.

    CLEAR FRAME {&FRAME-NAME} NO-PAUSE.
    APPLY "entry" TO scan_order.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Simulate Tag  Scan */
DO:

/*
  assign scan_order:screen-value = "7375/200c 60x80/500.0/00737501".
*/
  FIND LAST oe-ordl NO-LOCK NO-ERROR.
  IF AVAIL oe-ordl THEN  scan_tag = STRING(oe-ordl.ord-no) + "/" + 
                                    string(oe-ordl.i-no,"x(15)") +
                                    "00001".
  ELSE assign scan_tag = "532/10x10x10       00001".

  assign v-text = ""
         v-num = 1.
  do x = 1 to length(trim(scan_tag)):
    if substring(scan_tag,x,1) ne "/" then
      assign v-text[v-num] = v-text[v-num] + substring(scan_tag,x,1).
    else
      v-num = v-num + 1.
  end.
  ASSIGN scan_order:SCREEN-VALUE = v-text[1]
         scan_tag:SCREEN-VALUE = v-text[2]
        /* v_count:SCREEN-VALUE = v-text[3]*/ .

  find first oe-ordl where oe-ordl.company eq gcompany
                       and oe-ordl.ord-no eq int(scan_order:SCREEN-VALUE)
                       and oe-ordl.i-no eq substring(scan_tag:screen-value,1,15) 
                     no-lock no-error.
  if not avail oe-ordl then
  do:
    message "Item is not on Order. Please Check Order# and Tag#." view-as alert-box.
    assign Scan_order:screen-value = ""
           Scan_tag:screen-value = "".
  end.
  else
    APPLY "TAB".

    APPLY "leave" TO scan_order.
    APPLY "leave" TO scan_tag.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Simulate Job# Scan */
DO: 
    /*
  assign scan_itemfg:screen-value = "10x10x10".
  
  assign v-job-no = ""
         v-job-no2 = ""
         .
         
  do x = 1 to length(trim(scan_itemfg:screen-value)):
    if substring(trim(scan_itemfg:screen-value),x,1) ne "-" then
      v-job-no = v-job-no + substring(trim(scan_itemfg:screen-value),x,1).
    else
      leave.
  end.

  y = x + 1.  
  do x = y to length(trim(scan_itemfg:screen-value)):
      v-job-no2 = v-job-no2 + substring(trim(scan_itemfg:screen-value),x,1).
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
    assign scan_order:screen-value = ""
           scan_itemfg:screen-value = "".
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
  find first job where job.company  eq gcompany
                   and job.job-no   eq rm-rcpt.job-no
                   and job.job-no2 eq rm-rcpt.job-no2
                 no-lock no-error.
                 
  for each job-mat where job-mat.company eq gcompany
                     and job-mat.job     eq job.job 
                     and job-mat.i-no    eq rm-rcpt.i-no
                     and job-mat.job-no  eq rm-rcpt.job-no
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
    do:
      assign rm-rdtl.s-num = job-mat.frm
             rm-rdtl.b-num = job-mat.blank-no
             rm-rdtl.pass = job-mat.pass.
    end.
  end.
  else
  do:
    run value("touch/numeric.w") PERSISTENT SET h_numeric (Frame_Handle).
    /*ENABLE Scan_form with frame {&frame-name}.
    Scan_Form:hidden eq false. */
/*
    ENABLE Scan_Blank with frame {&frame-name}.
    Scan_Blank:hidden eq false.
*/
/*    ENABLE Scan_Pass with frame {&frame-name}.
    Scan_Pass:hidden eq false. */
  end.
  
  APPLY "TAB".
*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Scan_order
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Scan_order C-Win
ON HELP OF Scan_order IN FRAME DEFAULT-FRAME /* Scan Order# */
DO:
   /* run windows/l-ordrel.w ...*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Scan_order C-Win
ON LEAVE OF Scan_order IN FRAME DEFAULT-FRAME /* Scan Order# */
DO:
    FIND FIRST oe-ord WHERE oe-ord.company = gcompany
                        AND oe-ord.ord-no = int(scan_order:SCREEN-VALUE)
                        NO-LOCK NO-ERROR.
    IF NOT AVAIL oe-ord THEN DO:
       IF LASTKEY = -1 THEN RETURN.
       MESSAGE "Invalid Order#" VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.

    FIND FIRST cust WHERE cust.company = gcompany
                      AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.
    ASSIGN v-cust-no = oe-ord.cust-no
           v-cust-name = IF AVAIL cust THEN cust.NAME ELSE "".
    DISP v-cust-no v-cust-name WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Scan_Tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Scan_Tag C-Win
ON HELP OF Scan_Tag IN FRAME DEFAULT-FRAME /* Scan Tag# */
DO:
     DEF VAR lookup-recid AS RECID NO-UNDO.
     RUN fg/l-tag.w (gcompany,NO,FOCUS:SCREEN-VALUE, OUTPUT lookup-recid ).
     IF lookup-recid <> ? THEN DO:
        FIND loadtag WHERE RECID(loadtag) = lookup-recid NO-LOCK NO-ERROR.
        IF AVAIL loadtag THEN 
            ASSIGN scan_tag:SCREEN-VALUE = loadtag.tag-no
                   scan_order:SCREEN-VALUE = string(loadtag.ord-no)
                   v_itemfg:SCREEN-VALUE = loadtag.i-no
                 /*  v-item-name:SCREEN-VALUE = loadtag.i-name
                   v-po-no:SCREEN-VALUE = STRING(loadtag.po-no)
                   v-ord-no:SCREEN-VALUE = STRING(loadtag.ord-no)
                   v-job-no:SCREEN-VALUE = STRING(loadtag.job-no)
                   v-job-no2:SCREEN-VALUE = STRING(loadtag.job-no2)*/
                   .
     END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Scan_Tag C-Win
ON LEAVE OF Scan_Tag IN FRAME DEFAULT-FRAME /* Scan Tag# */
DO:
   ASSIGN v_itemfg = SUBSTRING(SELF:SCREEN-VALUE,1,15)
          v_count = SUBSTRING(SELF:SCREEN-VALUE,16,5)
          .
   DISP v_itemfg v_count WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Scan_Tag C-Win
ON VALUE-CHANGED OF Scan_Tag IN FRAME DEFAULT-FRAME /* Scan Tag# */
DO:
    FIND FIRST loadtag WHERE loadtag.company = gcompany
                         AND loadtag.item-type = NO
                         AND loadtag.tag-no = SELF:SCREEN-VALUE
                         NO-LOCK NO-ERROR.
    IF AVAIL loadtag AND loadtag.ord-no = 0 THEN DO:
       scan_order:SENSITIVE = YES.
       APPLY "entry" TO scan_order.
       RETURN NO-APPLY.
    END.

    IF SELF:SCREEN-VALUE = "RESET" THEN DO:
       APPLY "choose" TO btn_reset.
       RETURN NO-APPLY.
    END.
    IF SELF:SCREEN-VALUE = "CREATE" THEN DO:
       APPLY "choose" TO btn_next.
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


&Scoped-define SELF-NAME v_count
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_count C-Win
ON LEAVE OF v_count IN FRAME DEFAULT-FRAME /* Count */
DO:
   IF LASTKEY = -1 THEN RETURN.
   FIND FIRST oe-ordl WHERE oe-ordl.ord-no = INT(scan_order:SCREEN-VALUE)
                        AND oe-ordl.i-no = SELF:SCREEN-VALUE
                        NO-LOCK NO-ERROR.
   IF NOT AVAIL oe-ordl THEN DO:
       MESSAGE "Invalid FG Item#." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v_itemfg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_itemfg C-Win
ON LEAVE OF v_itemfg IN FRAME DEFAULT-FRAME /* Fg Item# */
DO:
   IF LASTKEY = -1 THEN RETURN.
   FIND FIRST oe-ordl WHERE oe-ordl.ord-no = INT(scan_order:SCREEN-VALUE)
                        AND oe-ordl.i-no = SELF:SCREEN-VALUE
                        NO-LOCK NO-ERROR.
   IF NOT AVAIL oe-ordl THEN DO:
       MESSAGE "Invalid FG Item#." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-bol C-Win 
PROCEDURE create-bol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var li-next-bol as int no-undo.
  DEF VAR v-n-bol LIKE oe-ctrl.n-bol NO-UNDO.

  /* need to create oe-relh, oe-rell ??*/

  find last oe-bolh use-index b-no no-lock no-error.
  li-next-bol = if avail oe-bolh then oe-bolh.b-no + 1 else 1.

  find first oe-ctrl where oe-ctrl.company eq gcompany exclusive.
  do while true:
     assign v-n-bol       = oe-ctrl.n-bol
            oe-ctrl.n-bol = v-n-bol + 1.
   
     if oe-ctrl.n-bol gt 999999 then oe-ctrl.n-bol = 1.  
     find first oe-bolh  where oe-bolh.company eq gcompany
                         and oe-bolh.bol-no  eq v-n-bol
                use-index bol-no no-lock no-error.
     if not avail oe-bolh then leave.
  end.
  CREATE oe-bolh.
  assign oe-bolh.company = gcompany
         oe-bolh.loc = gloc
         oe-bolh.b-no = li-next-bol
         oe-bolh.bol-date = today
         oe-bolh.bol-no = v-n-bol 
         oe-bolh.cust-no = v-cust-no
         .
         
  FIND FIRST oe-ord WHERE oe-ord.company = gcompany
                      AND oe-ord.ord-no = int(scan_order:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                      NO-LOCK NO-ERROR.
  FIND FIRST cust WHERE cust.company = gcompany
                      AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.
  FIND FIRST shipto WHERE shipto.company = gcompany
                      AND shipto.cust-no = cust.cust-no NO-LOCK NO-ERROR.
  IF AVAILABLE shipto THEN DO:
      ASSIGN oe-bolh.ship-no = shipto.ship-no
             oe-bolh.ship-id = shipto.ship-id
             oe-bolh.carrier = oe-ord.carrier
             oe-bolh.ship-i[1] = shipto.notes[1]
             oe-bolh.ship-i[2] = shipto.notes[2]
             oe-bolh.ship-i[3] = shipto.notes[3]
             oe-bolh.ship-i[4] = shipto.notes[4]
             oe-bolh.frt-pay = oe-ord.frt-pay
    
             /* oe-bolh.traier
                oe-bolh.release#
                oe-bolh.frt-pay .... */
             .
     RUN CopyShipNote (shipto.rec_key, oe-bolh.rec_key).
  END.
  CREATE oe-boll.
  ASSIGN oe-boll.b-no = oe-bolh.b-no
         oe-boll.bol-no = oe-bolh.bol-no
         oe-boll.r-no = oe-bolh.r-no
         oe-boll.ord-no = oe-ord.ord-no
         oe-boll.i-no = v_itemfg
         .
  FIND FIRST itemfg WHERE itemfg.company = gcompany
                      AND itemfg.i-no = oe-boll.i-no NO-LOCK NO-ERROR.
  ASSIGN oe-boll.loc = itemfg.def-loc
         oe-boll.loc-bin = itemfg.def-loc-bin
         .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyShipNote d-oeitem
PROCEDURE CopyShipNote PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Copies Ship Note from rec_key to rec_key
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcRecKeyFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcRecKeyTo AS CHARACTER NO-UNDO.

DEFINE VARIABLE hNotesProcs AS HANDLE NO-UNDO.

    RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.  

    RUN CopyShipNote IN hNotesProcs (ipcRecKeyFrom, ipcRecKeyTo).
    
    DELETE OBJECT hNotesProcs.   

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
  DISPLAY Scan_Tag Scan_order v-cust-no v-cust-name v_itemfg v_count 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Scan_Tag BUTTON-1 Btn_Next Btn_view Btn_reset Btn_Close RECT-9 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE view-bol C-Win 
PROCEDURE view-bol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  FIND FIRST oe-boll WHERE oe-boll.company = gcompany AND
                           oe-boll.ord-no = int(scan_order) AND
                           oe-boll.i-no = v_itemfg NO-LOCK NO-ERROR.
  IF AVAIL oe-boll THEN DO:
     FIND oe-bolh OF oe-boll NO-LOCK.
     RUN bol/d-oebolh.w (RECID(oe-bolh)).
  END.
  ELSE MESSAGE "No Bill of Lading available for the order " scan_order "and item" 
         v_itemfg VIEW-AS ALERT-BOX ERROR.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

