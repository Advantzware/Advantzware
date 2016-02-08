&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: pc\r-pcpost.w

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
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i "new shared" }

assign
 cocode = gcompany
 locode = gloc.


def var fdate       like pc-prdh.trans-date init today  format "99/99/9999" NO-UNDO.
def var tdate       like fdate NO-UNDO.
def var fshift      like pc-prdh.shift      init 1      format ">>" NO-UNDO.
def var tshift      like fshift             init 99 NO-UNDO.
def var fmach       like pc-prdh.m-code NO-UNDO.
def var tmach       like fmach              init "zzzzzz" NO-UNDO.

def NEW shared var v-post-date as date init today NO-UNDO.

def var v-dunne     as   log init no NO-UNDO.
def var v-up-hs     like eb.num-up NO-UNDO.
def var v-up        like eb.num-up NO-UNDO.
def var v-out       like est-op.n-out NO-UNDO.
def var v-on        like eb.num-up NO-UNDO.
def var v-autopost  as   log NO-UNDO.
def var v-auto-bin  like sys-ctrl.char-fld no-undo.
def var v-rm-fg     as   log NO-UNDO.
def var v-tot-rm    like mat-act.qty NO-UNDO.
def var v-tot-fg    like mch-act.qty NO-UNDO.
def var v-loc       like fg-bin.loc NO-UNDO.
def var v-loc-bin   like fg-bin.loc-bin NO-UNDO.

def TEMP-TABLE w-job NO-UNDO field job like job.job.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "AUTOPOST"
    no-lock no-error.

if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "AUTOPOST"
   sys-ctrl.descrip = "Autopost to Finished Goods Receipts?".
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
assign
 v-autopost = sys-ctrl.log-fld
 v-auto-bin = sys-ctrl.char-fld.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "RM=FG"
    no-lock no-error.

if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "RM=FG"
   sys-ctrl.descrip = "Validate RM issues = FG Produced Plus Waste?"
   sys-ctrl.log-fld = no.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
v-rm-fg = sys-ctrl.log-fld.
DO TRANSACTION:
    {sys/inc/tspost.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_mach end_mach ~
begin_pc-date end_pc-date begin_shift end_shift rd-dest lines-per-page ~
td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_mach end_mach begin_pc-date ~
end_pc-date begin_shift end_shift rd-dest lines-per-page td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)" 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_pc-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_shift AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Beginning Shift" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_pc-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_shift AS INTEGER FORMAT ">9" INITIAL 99 
     LABEL "Ending Shift" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 55 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 92 BY 5.48.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 11.43.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_mach AT ROW 4.33 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Machine Code"
     end_mach AT ROW 4.33 COL 69 COLON-ALIGNED HELP
          "Enter Ending Machine Code"
     begin_pc-date AT ROW 5.29 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_pc-date AT ROW 5.29 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_shift AT ROW 6.24 COL 27 COLON-ALIGNED HELP
          "Enter Beginning BOL Number"
     end_shift AT ROW 6.24 COL 69 COLON-ALIGNED HELP
          "Enter Ending Shift"
     rd-dest AT ROW 13.86 COL 11 NO-LABEL
     lines-per-page AT ROW 14.57 COL 73 COLON-ALIGNED
     td-show-parm AT ROW 16 COL 58
     btn-ok AT ROW 19.81 COL 19
     btn-cancel AT ROW 19.81 COL 57
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13.14 COL 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     RECT-6 AT ROW 12.67 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 21.57.


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
         TITLE              = "Post Production to WIP"
         HEIGHT             = 21.81
         WIDTH              = 95.8
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
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
IF NOT C-Win:LOAD-ICON("images\progress":U) THEN
    MESSAGE "Unable to load icon: images\progress"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
ASSIGN 
       begin_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_pc-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_shift:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_pc-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_shift:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Post Production to WIP */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Post Production to WIP */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* Beginning Machine */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_pc-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_pc-date C-Win
ON LEAVE OF begin_pc-date IN FRAME FRAME-A /* Beginning Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_shift C-Win
ON LEAVE OF begin_shift IN FRAME FRAME-A /* Beginning Shift */
DO:
     assign {&self-name}.
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


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DEF VAR ll-post AS LOG NO-UNDO.


  assign rd-dest.
       
  MESSAGE "Are you sure you wish to " + c-win:title
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll-post.

  IF ll-post THEN DO:
    run run-report. 

    case rd-dest:
         when 1 then run output-to-printer.
         when 2 then run output-to-screen.
         when 3 then run output-to-file.
    end case. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach C-Win
ON LEAVE OF end_mach IN FRAME FRAME-A /* Ending Machine */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_pc-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_pc-date C-Win
ON LEAVE OF end_pc-date IN FRAME FRAME-A /* Ending Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_shift C-Win
ON LEAVE OF end_shift IN FRAME FRAME-A /* Ending Shift */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    assign {&self-name}.
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
   
  assign
   begin_pc-date = date(1,1,year(today))
   end_pc-date   = today.

  RUN init-sys-ctrl.

  RUN enable_UI.
  
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
  DISPLAY begin_mach end_mach begin_pc-date end_pc-date begin_shift end_shift 
          rd-dest lines-per-page td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_mach end_mach begin_pc-date end_pc-date 
         begin_shift end_shift rd-dest lines-per-page td-show-parm btn-ok 
         btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-sys-ctrl C-Win 
PROCEDURE init-sys-ctrl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "AUTOPOST"
    no-lock no-error.

if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "AUTOPOST"
   sys-ctrl.descrip = "Autopost to Finished Goods Receipts".
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
assign
 v-autopost = sys-ctrl.log-fld
 v-auto-bin = sys-ctrl.char-fld.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "RM=FG"
    no-lock no-error.

if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "RM=FG"
   sys-ctrl.descrip = "Validate RM issues = FG Produced Plus Waste?"
   sys-ctrl.log-fld = no.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
v-rm-fg = sys-ctrl.log-fld.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-file C-Win 
PROCEDURE output-to-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
          
     if init-dir = "" then init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE list-name
         TITLE      "Enter Listing Name to SAVE AS ..."
         FILTERS    "Listing Files (*.rpt)" "*.rpt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         ASK-OVERWRITE
    /*     CREATE-TEST-FILE*/
         SAVE-AS
         USE-FILENAME
   
         UPDATE OKpressed.
         
     IF NOT OKpressed THEN  RETURN NO-APPLY.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.
  
/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-screen C-Win 
PROCEDURE output-to-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  run scr-rpt.w (list-name,c-win:title). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
DEF VAR tmp-dir AS cha NO-UNDO.
DEF VAR v-est-type LIKE est.est-type NO-UNDO.

{sys/inc/print1.i}
{sys/inc/outprint.i value(lines-per-page)}
if td-show-parm then run show-param. 

 ASSIGN fmach = begin_mach
        tmach = end_mach
        fdate = begin_pc-date
        tdate = end_pc-date
        fshift = begin_shift
        tshift = END_shift.

postit:
do transaction on error undo postit, leave postit:
   transblok:
   for each pc-prdh
          where pc-prdh.company    eq cocode
            and pc-prdh.m-code     ge fmach
            and pc-prdh.m-code     le tmach
            and pc-prdh.trans-date ge fdate
            and pc-prdh.trans-date le tdate
            and pc-prdh.shift      ge fshift
            and pc-prdh.shift      le tshift
          use-index date-idx

          break by pc-prdh.m-code:
          
         find first mach
             {sys/ref/machW.i}
               and mach.m-code eq pc-prdh.m-code
             no-lock no-error.
             
         if avail mach then
         for each pc-prdd
             where pc-prdd.company eq cocode
               and pc-prdd.m-code  eq pc-prdh.m-code
               and pc-prdd.op-date eq pc-prdh.trans-date
               and pc-prdd.shift   eq pc-prdh.shift
               and ((pc-prdd.stopp - pc-prdd.start
                                   ne 0) or
                    (pc-prdd.qty   ne 0) or
                    (pc-prdd.waste ne 0)),

             first job
             where job.company eq cocode
               AND job.job     EQ pc-prdd.job
               and job.job-no  eq pc-prdd.job-no
               and job.job-no2 eq pc-prdd.job-no2
          on error undo postit, LEAVE:
             
           find first w-job where w-job.job eq job.job no-error.
           if not avail w-job then create w-job.
           w-job.job = job.job.

           assign
            v-up  = 1
            v-out = 1
            v-on  = 1.

           FIND FIRST est
               WHERE est.company EQ job.company
                 AND est.est-no  EQ job.est-no
               NO-LOCK NO-ERROR.
           v-est-type = IF AVAIL est THEN est.est-type ELSE 1.
           IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

           if avail est and INDEX("AP",mach.p-type) LE 0 then do:
             run sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, output v-up).

             find first ef
                 where ef.company = est.company
                   AND ef.est-no   eq est.est-no
                   and ef.form-no eq pc-prdd.frm
                 no-lock no-error.
                 
             IF AVAIL ef THEN DO:
               RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).  
               v-on = v-up * v-on.
             END.
                      
             find first est-op
                 where est-op.company = est.company
                   AND est-op.est-no   eq est.est-no
                   and est-op.s-num   eq pc-prdd.frm
                   and (est-op.b-num  eq pc-prdd.blank-no or
                        pc-prdd.blank-no eq 0)
                   and est-op.m-code  eq pc-prdh.m-code
                   and est-op.op-pass eq pc-prdd.pass
                   and est-op.dept    eq pc-prdd.dept
                   and est-op.line    lt 500
                 no-lock no-error.

             if ((avail est-op) and est-op.op-sb)           or
                ((not avail est-op) and mach.p-type ne "B") then do:

               if avail est-op then
                 run sys/inc/numout.p (recid(est-op), output v-out).

               else v-out = 1.
               
               v-up = v-up * v-out.
             end.

             else v-up = 1.

             v-on = v-on / v-up.
           end.
           
           v-up-hs = 1.

           if pc-prdd.dept eq "HS" and
              avail est            and
              mach.therm           and
              mach.p-type eq "S"   then
             run sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, output v-up-hs).

           if v-rm-fg then do:      /* validate rm > fg produced + waste */
             assign
              v-tot-rm = 0
              v-tot-fg = (pc-prdd.qty + pc-prdd.waste) / v-up-hs.

             for each mch-act
                 where mch-act.company  eq cocode
                   and mch-act.job      eq job.job
                   and mch-act.job-no   eq job.job-no
                   and mch-act.job-no2  eq job.job-no2
                   and mch-act.frm      eq pc-prdd.frm
                   and mch-act.blank-no eq pc-prdd.blank-no
                   and mch-act.pass     eq pc-prdd.pass
                   and mch-act.m-code   eq pc-prdh.m-code
                 use-index job no-lock:
               v-tot-fg = v-tot-fg + (mch-act.qty + mch-act.waste).
             end.

             v-tot-fg = v-tot-fg / v-on.

             release job-mat.
             for each job-mat
                 where job-mat.company eq cocode
                   and job-mat.job     eq job.job
                   and job-mat.job-no  eq job.job-no
                   and job-mat.job-no2 eq job.job-no2
                   and job-mat.frm     eq pc-prdd.frm
                 no-lock,
                 first item
                 where item.company    eq cocode
                   and item.i-no       eq job-mat.i-no
                   and item.mat-type   eq "B"
                   and item.i-code     eq "R"
                 no-lock:
               leave.
             end.

             if avail job-mat then do:
               for each mat-act
                   where mat-act.company eq cocode
                     and mat-act.job     eq job.job
                     and mat-act.job-no  eq job.job-no
                     and mat-act.job-no2 eq job.job-no2
                     and mat-act.i-no    eq job-mat.i-no
                     and mat-act.s-num   eq job-mat.frm
                     and mat-act.b-num   eq job-mat.blank-no
                   use-index job no-lock:
                 v-tot-rm = v-tot-rm + mat-act.qty.
               end.

               if v-tot-fg gt v-tot-rm then next.
             end.
           end.
           
           {pc/pcmchact.i}
           
           if pc-prdd.complete and v-autopost then do:
             FIND LAST job-mch
                 WHERE job-mch.company EQ pc-prdd.company
                   AND job-mch.job     EQ job.job
                   AND job-mch.job-no  EQ pc-prdd.job-no
                   AND job-mch.job-no2 EQ pc-prdd.job-no2
                   AND (job-mch.frm    EQ pc-prdd.frm OR v-est-type EQ 2)
                 USE-INDEX line-idx NO-LOCK NO-ERROR.

             IF job-mch.blank-no NE 0 THEN
             FIND LAST job-mch
                 WHERE job-mch.company   EQ pc-prdd.company
                   AND job-mch.job       EQ job.job
                   AND job-mch.job-no    EQ pc-prdd.job-no
                   AND job-mch.job-no2   EQ pc-prdd.job-no2
                   AND (job-mch.frm      EQ pc-prdd.frm OR v-est-type EQ 2)
                   AND (job-mch.blank-no EQ pc-prdd.blank-no OR
                        mach.p-type      NE "B"              OR
                        v-est-type       EQ 1)
                 USE-INDEX line-idx NO-LOCK NO-ERROR.
                 
             if avail job-mch                    and
                job-mch.m-code eq pc-prdd.m-code and
                job-mch.pass   eq pc-prdd.pass   and
                pc-prdd.code   eq "RUN"          then
             for each job-hdr
                where job-hdr.company   eq cocode
                  and job-hdr.job-no    eq job-mch.job-no
                  and job-hdr.job-no2   eq job-mch.job-no2
                  and (job-mch.frm      eq pc-prdd.frm OR
                       v-est-type       EQ 2)
                  and (job-hdr.blank-no eq job-mch.blank-no or
                       job-mch.blank-no eq 0)
                no-lock:

                find first itemfg
                    where itemfg.company    eq cocode
                      and itemfg.i-no       eq job-hdr.i-no
                      and itemfg.case-count gt 0
                    no-lock no-error.

                if not avail itemfg then next transblok.

                x = 1.
                FOR EACH fg-rctd no-lock BY fg-rctd.r-no DESC:
                  LEAVE.
                END.
                if avail fg-rctd then x = fg-rctd.r-no.

                find last fg-rcpth use-index r-no no-lock no-error.
                if avail fg-rcpth and fg-rcpth.r-no GT x then x = fg-rcpth.r-no.

                create fg-rctd.
                assign
                 fg-rctd.r-no       = X + 1
                 fg-rctd.rct-date   = pc-prdd.op-date
                 fg-rctd.trans-time = pc-prdd.op-time
                 fg-rctd.company    = cocode
                 fg-rctd.loc        = locode
                 fg-rctd.rita-code  = "R"
                 fg-rctd.i-name     = itemfg.i-name
                 fg-rctd.i-no       = job-hdr.i-no
                 fg-rctd.job-no     = pc-prdd.job-no
                 fg-rctd.job-no2    = pc-prdd.job-no2.
                 
                assign
                 v-up  = 1
                 v-out = 1.
                 
                if avail est and index("APB",mach.p-type) le 0 then do:
                  run sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, output v-up).
                 
                  find first est-op
                     where est-op.company = est.company
                       AND est-op.est-no   eq est.est-no
                       and est-op.s-num   eq pc-prdd.frm
                       and (est-op.b-num  eq pc-prdd.blank-no or
                            pc-prdd.blank-no eq 0)
                       and est-op.m-code  eq pc-prdh.m-code
                       and est-op.op-pass eq pc-prdd.pass
                       and est-op.dept    eq pc-prdd.dept
                       and est-op.line    lt 500
                     no-lock no-error.
                  if avail est-op and est-op.n-out ne 0 then
                    v-out = est-op.n-out.
                end.
                 
                ASSIGN
                 fg-rctd.b-num    = pc-prdd.blank-no
                 fg-rctd.s-num    = pc-prdd.frm
                 fg-rctd.t-qty    = pc-prdd.qty / v-up-hs * v-out * v-up
                 fg-rctd.cost-uom = itemfg.prod-uom
                 fg-rctd.std-cost = job-hdr.std-tot-cost
                 fg-rctd.ext-cost = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
                 fg-rctd.qty-case = itemfg.case-count
                 fg-rctd.partial  = (fg-rctd.t-qty modulo itemfg.case-count)
                 fg-rctd.cases    = int((fg-rctd.t-qty / itemfg.case-count) - .5).

                if fg-rctd.t-qty le 0 then fg-rctd.cases = 0.
                
                RUN fg/autopost.p (ROWID(itemfg), pc-prdd.job-no, pc-prdd.job-no2,
                                   OUTPUT fg-rctd.loc, OUTPUT fg-rctd.loc-bin).
             end.
           end.
           
           delete pc-prdd.
         end. /* for each pc-prdd */

         find first pc-prdd
             where pc-prdd.company eq cocode
               and pc-prdd.m-code  eq pc-prdh.m-code
               and pc-prdd.op-date eq pc-prdh.trans-date
               and pc-prdd.shift   eq pc-prdh.shift
             no-lock no-error.
         if not avail pc-prdd then delete pc-prdh.
      end. /* for each pc-prdh */

      for each w-job,
          first job where job.company eq cocode
                      and job.job     eq w-job.job
          no-lock:
          run jc/job-cls2.p (recid(job)).
      end.
      v-dunne = yes.
end. /* postit */
MESSAGE 1 VIEW-AS ALERT-BOX.

if not v-dunne then
   MESSAGE "   ERRORS ENCOUNTERED...  POSTING ABORTED !   "
          VIEW-AS ALERT-BOX ERROR.

/* end ---------------------------------- copr. 1994  advanced software, inc. */

                                
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param C-Win 
PROCEDURE show-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-frame-hdl as handle no-undo.
  def var lv-group-hdl as handle no-undo.
  def var lv-field-hdl as handle no-undo.
  def var lv-field2-hdl as handle no-undo.
  def var parm-fld-list as cha no-undo.
  def var parm-lbl-list as cha no-undo.
  def var i as int no-undo.
  def var lv-label as cha.
  
  lv-frame-hdl = frame {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:first-child.
  lv-field-hdl = lv-group-hdl:first-child .
  
  do while true:
     if not valid-handle(lv-field-hdl) then leave.
     if lookup(lv-field-hdl:private-data,"parm") > 0
        then do:
           if lv-field-hdl:label <> ? then 
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + "," 
                     .
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     .
              lv-field2-hdl = lv-group-hdl:first-child.
              repeat:
                  if not valid-handle(lv-field2-hdl) then leave. 
                  if lv-field2-hdl:private-data = lv-field-hdl:name then do:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".
                  end.
                  lv-field2-hdl = lv-field2-hdl:next-sibling.                 
              end.       
           end.                 
        end.            
     lv-field-hdl = lv-field-hdl:next-sibling.   
  end.

  put space(28)
      "< Selection Parameters >"
      skip(1).
  
  do i = 1 to num-entries(parm-fld-list,","):
    if entry(i,parm-fld-list) ne "" or
       entry(i,parm-lbl-list) ne "" then do:
       
      lv-label = fill(" ",34 - length(trim(entry(i,parm-lbl-list)))) +
                 trim(entry(i,parm-lbl-list)) + ":".
                 
      put lv-label format "x(35)" at 5
          space(1)
          trim(entry(i,parm-fld-list)) format "x(40)"
          skip.              
    end.
  end.
 
  put fill("-",80) format "x(80)" skip.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

