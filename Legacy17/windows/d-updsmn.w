&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: windows\d-updsmn.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-cust-no AS cha NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
ASSIGN cocode = g_company
       locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_est end_est begin_cust ~
end_cust tb_slsmn btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_est end_est begin_cust end_cust ~
lbl_slsmn tb_slsmn v-status 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-GO 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_est AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_est AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_slsmn AS CHARACTER FORMAT "X(256)":U INITIAL "Update Sales Rep & Commission?" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE v-status AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 88 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 6.19.

DEFINE VARIABLE tb_slsmn AS LOGICAL INITIAL yes 
     LABEL "Update Sales Rep & Commission?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_est AT ROW 2.67 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Estimate Number"
     end_est AT ROW 2.67 COL 67 COLON-ALIGNED HELP
          "Enter Ending Estimate Number"
     begin_cust AT ROW 3.86 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 3.86 COL 67 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     lbl_slsmn AT ROW 5.76 COL 30 COLON-ALIGNED NO-LABEL
     tb_slsmn AT ROW 5.76 COL 67
     v-status AT ROW 7.67 COL 2 NO-LABEL
     btn-process AT ROW 9.33 COL 21
     btn-cancel AT ROW 9.33 COL 60
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.71 COL 6
     RECT-17 AT ROW 1.24 COL 2
     SPACE(2.39) SKIP(3.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Update Sales Rep and Commission".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN lbl_slsmn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-status IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Update Sales Rep and Commission */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process Dialog-Frame
ON CHOOSE OF btn-process IN FRAME Dialog-Frame /* Start Process */
DO:
  DEF VAR v-process AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  IF tb_slsmn THEN
    MESSAGE "Are you sure you want to update Estimates and Quote" 
          " for the selected parameters?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.
        
  IF v-process THEN RUN run-process.
  ELSE APPLY "choose" TO btn-cancel.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  ASSIGN begin_cust = ip-cust-no
         END_cust = ip-cust-no.
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY begin_est end_est begin_cust end_cust lbl_slsmn tb_slsmn v-status 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-17 begin_est end_est begin_cust end_cust tb_slsmn btn-process 
         btn-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process Dialog-Frame 
PROCEDURE run-process :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER xeb FOR eb.
DEF BUFFER xest FOR est.
DEF BUFFER xef FOR ef.
def var fest        as   char               format ">>>>>".
def var test        like fest               init 99999.
def var fcus        like cust.cust-no.
def var tcus        like fcus               init "zzzzzzzz".
def var fupd        as   char               format "!" init "B".

def var v-cas-pal   like xeb.cas-pal.
def var v-tr-cnt    like xeb.tr-cnt.
def var v-numstacks as   int.
def var v-stackcode as   char.
def var v-error     as   log.
def var i           as   int.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-eqty LIKE est-qty.eqty NO-UNDO.
def var v-recid    as   recid no-undo.
DEF VAR ld-markup AS DEC NO-UNDO.

session:set-wait-state("General").
  
assign
 fest = fill(" ",8 - length(trim(string(begin_est,">>>>>>>>")))) +
        trim(string(begin_est,">>>>>>>>"))
 test = fill(" ",8 - length(trim(string(end_est,">>>>>>>>")))) +
        trim(string(end_est,">>>>>>>>"))
 fcus = begin_cust
 tcus = end_cust
 fupd = if tb_slsmn THEN "S" ELSE ""
      /*    if tb_pallet then "B" else "S"
        else
          if tb_pallet then "P" else " ". */.


/*{util/updest.i}

                                     IF begin_mach NE "" THEN
                                     FIND FIRST mach
                                         WHERE mach.company EQ cocode
                                           AND mach.m-code  EQ begin_mach
                                         NO-LOCK NO-ERROR.
*/
IF fupd = "S" THEN DO:
 
 for each xest where xest.company eq cocode
                 and xest.est-no  ge fest
                 and xest.est-no  le test NO-LOCK:
     FOR each xeb where xeb.company eq xest.company
                    and xeb.est-no  eq xest.est-no
                    and xeb.cust-no ge fcus
                    and xeb.cust-no le tcus,
       first cust where cust.company eq cocode
                      and cust.cust-no eq xeb.cust-no no-lock:
    
           xeb.sman = "".

           RUN ce/markup.p (xeb.company, ROWID(xeb), OUTPUT ld-markup).

           run sys/inc/getsmncm.p (xeb.cust-no,
                                   INPUT-OUTPUT xeb.sman,
                                   xeb.procat,
                                   ld-markup,
                                   OUTPUT xeb.comm).
       END.
     v-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Updating Estimate " + trim(xest.est-no).
 END.
 FOR EACH quotehd WHERE quotehd.company = cocode
                    AND quotehd.est-no GE fest
                    AND quotehd.est-no LE test
                    AND quotehd.cust-no GE fcus
                    AND quotehd.cust-no LE tcus,
     first cust where cust.company eq cocode
                  and cust.cust-no eq quotehd.cust-no NO-LOCK:
     IF quotehd.sman <> cust.sman THEN DO: 
        quotehd.sman = cust.sman.
        v-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Updating Quote " + STRING(quotehd.q-no).
     END.
 END.

END.
/*
      if (fupd eq "B" or fupd eq "P")                 and
         cust.pallet ne "" and cust.case-bundle ne "" then do:
   
        find first item
                     where item.company eq cocode
                    and item.i-no    eq cust.case-bundle
                                             no-lock no-error.
                                         if avail item then do:
                                           xeb.cas-no = cust.case-bundle.

                                           find first e-item
                                               where e-item.company eq cocode
                                                 and e-item.i-no    eq item.i-no
                                               no-lock no-error.
                                           find first itemfg
                                               where itemfg.company eq cocode
                                                 and itemfg.i-no    eq xeb.stock-no
                                               no-lock no-error.
                                           if avail e-item then
                                             assign
                                              xeb.cas-len = e-item.case-l
                                              xeb.cas-wid = e-item.case-w
                                              xeb.cas-dep = e-item.case-d
                                              xeb.cas-wt  = e-item.avg-w
                                              xeb.cas-pal = e-item.case-pall
                                              xeb.cas-cnt = if avail itemfg and xest.est-type le 4 then
                                                             itemfg.case-count else e-item.box-case.

                                           if xeb.cas-len eq 0 then xeb.cas-len = item.case-l.
                                           if xeb.cas-wid eq 0 then xeb.cas-wid = item.case-w.
                                           if xeb.cas-dep eq 0 then xeb.cas-dep = item.case-d.
                                           if xeb.cas-wt  eq 0 then xeb.cas-wt  = item.avg-w.
                                           if xeb.cas-pal eq 0 then xeb.cas-pal = item.case-pall.
                                           if xeb.cas-cnt eq 0 then xeb.cas-cnt =
                                                                         if avail itemfg and xest.est-type le 4 then
                                                                           itemfg.case-count else item.box-case.
                                         end.

                                         find first item
                                             where item.company eq cocode
                                               and item.i-no    eq cust.pallet
                                             no-lock no-error.
                                         if avail item then
                                           assign
                                            xeb.tr-no  = item.i-no
                                            xeb.tr-len = item.case-l
                                            xeb.tr-wid = item.case-w
                                            xeb.tr-dep = item.case-d.

                                         v-error = yes.

                                         if xest.est-type ge 5 then
                                           run cec/kpallet.p(input recid(xeb),
                                                             output v-cas-pal,
                                                             output v-tr-cnt,
                                                             output v-numstacks,
                                                             output v-stackcode,
                                                             output v-error).

                                         if not v-error then
                                           assign
                                            xeb.cas-pal = v-cas-pal
                                            xeb.tr-cnt  = v-tr-cnt.

                                         else
                                         if xeb.cas-cnt ne 0 and xeb.cas-pal ne 0 then
                                           xeb.tr-cnt = xeb.cas-cnt * xeb.cas-pal.
                                       end.
                                      END.
                                      if fupd eq "B" or fupd eq "S" then do:
                                          FOR EACH quotehd WHERE quotehd.company = cocode
                                                             AND quotehd.est-no GE fest
                                                             AND quotehd.est-no LE test
                                                             AND quotehd.cust-no GE fcus
                                                             AND quotehd.cust-no LE tcus,
                                              first cust where cust.company eq cocode
                                                           and cust.cust-no eq quotehd.cust-no NO-LOCK:
                                              IF quotehd.sman <> cust.sman THEN quotehd.sman = cust.sman.
                                          END.
                                      END.


                                       IF AVAIL mach THEN
                                       FOR EACH est-qty
                                           WHERE est-qty.company EQ xest.company
                                             AND est-qty.est-no  EQ xest.est-no
                                           NO-LOCK,
                                           EACH ef
                                           WHERE ef.company EQ est-qty.company
                                             AND ef.est-no  EQ est-qty.est-no
                                             AND ef.eqty    EQ est-qty.eqty
                                           NO-LOCK,
                                           EACH eb
                                           WHERE eb.company EQ ef.company
                                             AND eb.est-no  EQ ef.est-no
                                             AND eb.eqty    EQ ef.eqty
                                             AND eb.form-no EQ ef.form-no
                                             AND eb.cust-no GE fcus
                                             AND eb.cust-no LE tcus
                                           NO-LOCK
                                           BREAK BY (IF xest.est-type EQ 1 THEN est-qty.eqty ELSE 1)
                                                 BY eb.form-no
                                                 BY eb.blank-no:

                                         fil_id = ?.

                                         IF (mach.p-type EQ "B" AND FIRST-OF(eb.blank-no)) OR
                                            (mach.p-type NE "B" AND FIRST-OF(eb.form-no))  THEN DO:

                                           lv-eqty = 0.

                                           FOR EACH est-op
                                               WHERE est-op.company EQ est-qty.company
                                                 AND est-op.est-no  EQ est-qty.est-no
                                                 AND est-op.line    LT 500
                                               BY est-op.qty:
                                             lv-eqty = est-op.qty.
                                             LEAVE.
                                           END.

                                           li = 1.
                                           FOR EACH est-op
                                               WHERE est-op.company EQ xest.company
                                                 AND est-op.est-no  EQ xest.est-no
                                                 AND est-op.line    LT 500
                                               BY est-op.line DESC:
                                             li = est-op.line + 1.
                                             LEAVE.
                                           END.

                                           CREATE est-op.
                                           ASSIGN
                                            est-op.company    = xest.company
                                            est-op.est-no     = xest.est-no
                                            est-op.m-code     = mach.m-code
                                            est-op.auto       = NO
                                            est-op.line       = li
                                            est-op.s-num      = eb.form-no
                                            est-op.b-num      = IF mach.p-type EQ "B" THEN eb.blank-no
                                                                ELSE
                                                                IF xest.est-type EQ 5 THEN 1 ELSE 0
                                            est-op.op-pass    = 1
                                            est-op.qty        = IF xest.est-type LE 1 OR
                                                                   xest.est-type EQ 5 OR
                                                                   xest.est-type EQ 6 THEN est-qty.eqty ELSE lv-eqty
                                            est-op.d-seq      = mach.d-seq
                                            est-op.dept       = mach.dept[1]
                                            est-op.op-sb      = mach.p-type ne "B"
                                            est-op.m-code     = mach.m-code
                                            est-op.m-dscr     = mach.m-dscr
                                            est-op.op-spoil   = mach.run-spoil
                                            est-op.op-crew[1] = mach.mr-crusiz
                                            est-op.op-crew[2] = mach.run-crusiz.

                                           RUN est/getcrusz.p (ROWID(mach), ROWID(eb), est-op.dept, "M R",
                                                               INPUT-OUTPUT est-op.op-crew[1]).

                                           RUN est/getcrusz.p (ROWID(mach), ROWID(eb), est-op.dept, "RUN",
                                                               INPUT-OUTPUT est-op.op-crew[2]).

                                           ASSIGN
                                            est-op.op-rate[1] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[1]) + 
                                                                mach.mr-varoh  + mach.mr-fixoh
                                            est-op.op-rate[2] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[2]) + 
                                                                mach.run-varoh + mach.run-fixoh.

                                           li = 0.
                                           FOR EACH xop
                                               WHERE xop.company EQ est-op.company
                                                 AND xop.est-no  EQ est-op.est-no
                                                 AND xop.qty     EQ est-op.qty
                                                 AND xop.s-num   EQ est-op.s-num
                                                 AND xop.b-num   EQ est-op.b-num
                                                 AND xop.dept    EQ est-op.dept
                                                 AND xop.line    LT 500
                                               NO-LOCK:
                                             IF RECID(xop) EQ RECID(est-op) THEN LEAVE.
                                             li = li + 1.
                                           END.
                                           est-op.op-pass = li + 1.

                                           li = 0.
                                           FOR EACH xop
                                               WHERE xop.company EQ est-op.company
                                                 AND xop.est-no  EQ est-op.est-no
                                                 AND xop.line    LT 500
                                               BY xop.qty BY xop.s-num BY xop.b-num BY xop.d-seq BY xop.op-pass:

                                             {sys/inc/outstrPL.i xop SHARE}
                                             ASSIGN
                                              li       = li + 1
                                              xop.line = li.

                                             IF AVAIL reftable THEN reftable.loc = STRING(xop.line,"9999999999"). 
                                           END.

                                           fil_id  = RECID(est-op).
                                         END.

                                         IF LAST((IF xest.est-type EQ 1 THEN est-qty.eqty ELSE 1)) AND
                                            fil_id NE ?                                            THEN DO:

                                           v-recid = fil_id.

                                           FOR EACH xef 
                                               WHERE xef.company EQ est-qty.company
                                                 AND xef.est-no  EQ est-qty.est-no:
                                             xef.op-lock = NO.
                                           END.    

                                           IF xest.est-type LE 4 THEN
                                             RUN ce/mach-rek.p (?).
                                           ELSE
                                             RUN cec/mach-rek.p.

                                           fil_id = v-recid.

                                           FOR EACH xef 
                                               WHERE xef.company EQ est-qty.company
                                                 AND xef.est-no  EQ est-qty.est-no:
                                             xef.op-lock = YES.
                                           END.
                                         END.
                                       END.
                                     end.

*/
STATUS DEFAULT "".

SESSION:SET-WAIT-STATE("").

MESSAGE " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "go" TO FRAME {&frame-name}.
  
/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

