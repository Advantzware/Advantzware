&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: fxRecKey.p

  Description: fix blank rec_key fields in all ASI database tables

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 12.28.2005
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
{sys/inc/varasgn.i}

DEF TEMP-TABLE tt-job-hdr LIKE job-hdr FIELD last-mach AS LOG.

SESSION:SET-WAIT-STATE('').

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_date end_date msg btnStartFix ~
btnCancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date msg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD nextRecKey Dialog-Frame 
FUNCTION nextRecKey RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 17 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnStartFix 
     LABEL "Create &Receipts" 
     SIZE 17 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE msg AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 83 BY 9.52
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/02/06 
     LABEL "Date From:" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 01/11/06 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_date AT ROW 1.24 COL 15 COLON-ALIGNED
     end_date AT ROW 1.24 COL 43 COLON-ALIGNED
     msg AT ROW 2.67 COL 1 NO-LABEL
     btnStartFix AT ROW 2.67 COL 87
     btnCancel AT ROW 4.57 COL 87
     SPACE(2.19) SKIP(6.57)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Create FG Receipts for the last machine"
         DEFAULT-BUTTON btnStartFix CANCEL-BUTTON btnCancel.


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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       msg:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Create FG Receipts for the last machine */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStartFix
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartFix Dialog-Frame
ON CHOOSE OF btnStartFix IN FRAME Dialog-Frame /* Create Receipts */
DO:
  ASSIGN  begin_date END_date .
  DISABLE {&SELF-NAME} btnCancel WITH FRAME {&FRAME-NAME}.
  RUN msg ('Creating FG Receipts ...' + CHR(10)).
  RUN create-receipts.
  RUN msg ('Done.' + CHR(10)).  
  btnCancel:LABEL = '&Close'.
  ENABLE btnCancel WITH FRAME {&FRAME-NAME}.
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
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-receipts Dialog-Frame 
PROCEDURE create-receipts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def var v-loc       like fg-bin.loc NO-UNDO.
def var v-loc-bin   like fg-bin.loc-bin NO-UNDO.
def var v-up-hs     like eb.num-up NO-UNDO.
def var v-up        like eb.num-up NO-UNDO.
def var v-out       like est-op.n-out NO-UNDO.
def var v-on        like eb.num-up NO-UNDO.
def var v-est-type  like est.est-type NO-UNDO.
def var v-trnum like gl-ctrl.trnum no-undo.


for each mch-act where mch-act.company eq g_company and
                       mch-act.opn = YES AND
                       mch-act.op-date ge begin_date and
                       mch-act.op-date le END_date
                       no-lock,
    first mach {sys/ref/machW.i}
          and mach.m-code eq mch-act.m-code no-lock,
    first job where job.company eq cocode
         and job.job     eq mch-act.job
         and job.job-no  eq mch-act.job-no
         and job.job-no2 eq mch-act.job-no2    :

    ASSIGN v-up  = 1
           v-out = 1
           v-on  = 1.

    FIND FIRST est WHERE est.company EQ job.company
                     AND est.est-no  EQ job.est-no NO-LOCK NO-ERROR.
    v-est-type = IF AVAIL est THEN est.est-type ELSE 1.
    IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.
    
   v-up-hs = 1.
   if mch-act.dept eq "HS" and
      avail est            and
      mach.therm           and
      mach.p-type eq "S"   then
         run sys/inc/numup.p (mch-act.company, est.est-no, mch-act.frm, output v-up-hs).

   assign
   v-up  = 1
   v-out = 1
   v-on  = 1.

  if avail est and INDEX("AP",mach.p-type) LE 0 then do:
    run sys/inc/numup.p (est.company, est.est-no, mch-act.frm, output v-up).

    find first ef
        where ef.company eq est.company
          and ef.est-no  eq est.est-no
          and ef.form-no eq mch-act.frm
        no-lock no-error.

    IF AVAIL ef THEN DO:
      RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).
      v-on = v-up * v-on.
    END.
                      
    find first est-op
        where est-op.company eq est.company
          and est-op.est-no  eq est.est-no
          and est-op.s-num   eq mch-act.frm
          and (est-op.b-num  eq mch-act.blank-no or
               mch-act.blank-no eq 0)
          and est-op.m-code  eq mch-act.m-code
          and est-op.op-pass eq mch-act.pass
          and est-op.dept    eq mch-act.dept
          and est-op.line    lt 500
        no-lock no-error.

    if ((avail est-op) and est-op.op-sb)           or
       ((not avail est-op) and mach.p-type ne "B") then do:

      if avail est-op THEN run sys/inc/numout.p (recid(est-op), output v-out).
      else v-out = 1.
               
      v-up = v-up * v-out.
    end.
    else v-up = 1.
    v-on = v-on / v-up.
  end.
           
  v-up-hs = 1.

  if mch-act.dept eq "HS" and
     avail est            and
     mach.therm           and
     mach.p-type eq "S"   then
    run sys/inc/numup.p (est.company, est.est-no, mch-act.frm, output v-up-hs).

  /*if mch-act.complete then*/ do:
      assign
       v-up  = 1
       v-out = 1.
                 
      if avail est and index("APB",mach.p-type) le 0 then do:
        run sys/inc/numup.p (est.company, est.est-no, mch-act.frm, output v-up).
                 
        find first est-op
            where est-op.company eq est.company
              and est-op.est-no  eq est.est-no
              and est-op.s-num   eq mch-act.frm
              and (est-op.b-num  eq mch-act.blank-no or
                   mch-act.blank-no eq 0)
              and est-op.m-code  eq mch-act.m-code
              and est-op.op-pass eq mch-act.pass
              and est-op.dept    eq mch-act.dept
              and est-op.line    lt 500
            no-lock no-error.
        if avail est-op and est-op.n-out ne 0 then v-out = est-op.n-out.
      end.

    RUN pcprdd4u (ROWID(mch-act)).

    for each tt-job-hdr,
        first itemfg where itemfg.company    eq cocode
          and itemfg.i-no       eq tt-job-hdr.i-no
          and itemfg.case-count gt 0
        no-lock:

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
       fg-rctd.rct-date   = mch-act.op-date
       fg-rctd.trans-time = mch-act.op-time
       fg-rctd.company    = cocode
       fg-rctd.rita-code  = "R"
       fg-rctd.i-name     = itemfg.i-name
       fg-rctd.i-no       = tt-job-hdr.i-no
       fg-rctd.job-no     = mch-act.job-no
       fg-rctd.job-no2    = mch-act.job-no2.
                  
      assign
       v-up  = 1
       v-out = 1.
                 
      if avail est and index("APB",mach.p-type) le 0 then do:
        run sys/inc/numup.p (est.company, est.est-no, mch-act.frm, output v-up).
        find first est-op
            where est-op.company eq est.company
              and est-op.est-no  eq est.est-no
              and est-op.s-num   eq mch-act.frm
              and (est-op.b-num  eq mch-act.blank-no or
                   mch-act.blank-no eq 0)
              and est-op.m-code  eq mch-act.m-code
              and est-op.op-pass eq mch-act.pass
              and est-op.dept    eq mch-act.dept
              and est-op.line    lt 500
            no-lock no-error.
        if avail est-op and est-op.n-out ne 0 then v-out = est-op.n-out.
      end.

RUN msg ("Item: " + fg-rctd.i-no + " Job: " + fg-rctd.job-no + "-" + STRING(fg-rctd.job-no2,"99") + " Machine: " + mch-act.m-code + CHR(10)) .


      ASSIGN
       fg-rctd.b-num      = mch-act.blank-no
       fg-rctd.s-num      = mch-act.frm
       fg-rctd.t-qty      = mch-act.qty / v-up-hs * v-out * v-up
       fg-rctd.pur-uom    = itemfg.prod-uom
       fg-rctd.cost-uom   = itemfg.prod-uom
       fg-rctd.std-cost   = tt-job-hdr.std-tot-cost
       fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
       fg-rctd.qty-case   = itemfg.case-count
       fg-rctd.partial    = fg-rctd.t-qty modulo itemfg.case-count
       fg-rctd.cases      = trunc(fg-rctd.t-qty / itemfg.case-count,0)
       fg-rctd.cases-unit = 1.

      if fg-rctd.t-qty le 0 then fg-rctd.cases = 0.

      RELEASE fg-bin.

      FIND FIRST reftable NO-LOCK
          WHERE reftable.reftable EQ "pc/pcprddu3.p"
            AND reftable.company  EQ pc-prdd.company
            AND reftable.code     EQ /*pc-prdd.rec_key*/ STRING(RECID(pc-prdd))
          NO-ERROR.

      IF AVAIL reftable THEN DO:
        ASSIGN
         fg-rctd.cases      = reftable.val[1]
         fg-rctd.qty-case   = reftable.val[2]
         fg-rctd.cases-unit = reftable.val[3]
         fg-rctd.partial    = fg-rctd.t-qty - (fg-rctd.cases * fg-rctd.qty-case).
        
        FIND FIRST fg-bin 
            WHERE fg-bin.rec_key EQ reftable.code2 /*RECID(fg-bin) EQ INT(reftable.code2)*/ 
            NO-LOCK NO-ERROR.
      END.

      IF AVAIL fg-bin THEN
        ASSIGN
         v-loc              = fg-bin.loc
         v-loc-bin          = fg-bin.loc-bin
         fg-rctd.tag        = fg-bin.tag
         fg-rctd.cases      = reftable.val[1]
         fg-rctd.qty-case   = reftable.val[2]
         fg-rctd.cases-unit = reftable.val[3]
         fg-rctd.partial    = fg-rctd.t-qty - (fg-rctd.cases * fg-rctd.qty-case).
                
      ELSE
        RUN fg/autopost.p (ROWID(itemfg), fg-rctd.job-no, fg-rctd.job-no2,
                           OUTPUT v-loc, OUTPUT v-loc-bin).

      ASSIGN
       fg-rctd.loc     = v-loc
       fg-rctd.loc-bin = v-loc-bin.

      FIND FIRST fg-bin
          WHERE fg-bin.company EQ fg-rctd.company
            AND fg-bin.i-no    EQ fg-rctd.i-no
            AND fg-bin.job-no  EQ mch-act.job-no
            AND fg-bin.job-no2 EQ mch-act.job-no2
            AND fg-bin.loc     EQ fg-rctd.loc
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin
            AND fg-bin.tag     EQ fg-rctd.tag
          NO-LOCK NO-ERROR.

      IF AVAIL fg-bin THEN fg-rctd.cases-unit = fg-bin.cases-unit.

      RUN fg/comprcpt.p (ROWID(fg-rctd)).
    end.
  END.

 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY begin_date end_date msg 
      WITH FRAME Dialog-Frame.
  ENABLE begin_date end_date msg btnStartFix btnCancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE msg Dialog-Frame 
PROCEDURE msg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipText AS CHARACTER NO-UNDO.

  msg:SCREEN-VALUE IN FRAME {&FRAME-NAME} = msg:SCREEN-VALUE + ipText.
  msg:MOVE-TO-EOF().
  PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pcprdd4u Dialog-Frame 
PROCEDURE pcprdd4u :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAM v-rowid AS ROWID.

DEF VAR v-est-type LIKE est.est-type NO-UNDO.
DEF VAR v-assembled AS LOG NO-UNDO.
DEF VAR ll-set-mach AS LOG NO-UNDO.

DEF BUFFER b-mach FOR mach.

/*{pc/pcprdd4u.i}*/


FOR EACH tt-job-hdr:
  DELETE tt-job-hdr.
END.

{sys/inc/autopdc.i}
{sys/inc/tspostfg.i}
{sys/inc/fgrecpt.i}

IF fgrecpt-char EQ "AUTOPOST" THEN DO:
  FIND FIRST mach
      {sys/look/machW.i}
        AND mach.m-code EQ mch-act.m-code
      NO-LOCK.

  FIND FIRST job
      WHERE job.company EQ mch-act.company
        AND job.job-no  EQ mch-act.job-no
        AND job.job-no2 EQ mch-act.job-no2
      USE-INDEX job-no NO-LOCK NO-ERROR.

  FIND FIRST est
      WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
      NO-LOCK NO-ERROR.
  v-est-type = IF AVAIL est THEN est.est-type ELSE 1.

  IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

  v-assembled = NO.

  IF v-est-type EQ 2 THEN
  FOR EACH job-hdr
      WHERE job-hdr.company  EQ cocode
        AND job-hdr.job      EQ mch-act.job
        AND job-hdr.job-no   EQ mch-act.job-no
        AND job-hdr.job-no2  EQ mch-act.job-no2
      NO-LOCK,
      FIRST itemfg
      WHERE itemfg.company   EQ cocode
        AND itemfg.i-no      EQ job-hdr.i-no
        AND itemfg.isaset
      NO-LOCK:

    ASSIGN
     v-assembled = YES
     v-est-type  = 4
     ll-set-mach = INDEX("AP",mach.p-type) GT 0.

    LEAVE.
  END.

  FOR EACH job-mch
      WHERE job-mch.company   EQ mch-act.company
        AND job-mch.job       EQ mch-act.job
        AND job-mch.job-no    EQ mch-act.job-no
        AND job-mch.job-no2   EQ mch-act.job-no2
        AND (job-mch.frm      EQ mch-act.frm OR v-est-type EQ 2)
        AND LOOKUP(job-mch.m-code,autopdc)
                              EQ 0
        AND autopdc           NE "*"
      USE-INDEX line-idx NO-LOCK,

      FIRST b-mach
      WHERE b-mach.company EQ job-mch.company
        AND b-mach.loc     EQ locode
        AND b-mach.m-code  EQ job-mch.m-code
        AND ((INDEX("AP",b-mach.p-type) LE 0 AND NOT ll-set-mach) OR
             (INDEX("AP",b-mach.p-type) GT 0 AND ll-set-mach))
      NO-LOCK

      BY job-mch.line DESC:
    LEAVE.
  END.

  IF AVAIL job-mch AND job-mch.blank-no NE 0 THEN
  FOR EACH job-mch
      WHERE job-mch.company   EQ mch-act.company
        AND job-mch.job       EQ mch-act.job
        AND job-mch.job-no    EQ mch-act.job-no
        AND job-mch.job-no2   EQ mch-act.job-no2
        AND (job-mch.frm      EQ mch-act.frm OR v-est-type EQ 2)
        AND LOOKUP(job-mch.m-code,autopdc)
                              EQ 0
        AND autopdc           NE "*"
        AND (job-mch.blank-no EQ mch-act.blank-no OR
             mach.p-type      NE "B"              OR
             v-est-type       EQ 1)
      USE-INDEX line-idx NO-LOCK,

      FIRST b-mach
      WHERE b-mach.company EQ job-mch.company
        AND b-mach.loc     EQ locode
        AND b-mach.m-code  EQ job-mch.m-code
        AND ((INDEX("AP",b-mach.p-type) LE 0 AND NOT ll-set-mach) OR
             (INDEX("AP",b-mach.p-type) GT 0 AND ll-set-mach))
      NO-LOCK

      BY job-mch.line DESC:
    LEAVE.
  END.

  IF AVAIL job-mch                              AND
            /*mch-act.code     EQ "RUN"                */
      CAN-FIND(FIRST job-code WHERE job-code.CODE = mch-act.CODE AND job-code.cat = "RUN")
       AND
     ((job-mch.m-code EQ mch-act.m-code AND
       job-mch.pass   EQ mch-act.pass)      OR
      (AVAIL mach                       AND
       (job-mch.dept  EQ mach.dept[1] OR
        job-mch.dept  EQ mach.dept[2] OR
        job-mch.dept  EQ mach.dept[3] OR
        job-mch.dept  EQ mach.dept[4])))        THEN DO:
     
    IF v-assembled AND NOT ll-set-mach THEN
    FOR EACH reftable
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job-mch.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job-mch.job,"999999999")
          AND reftable.val[12]  EQ job-mch.frm
          AND (reftable.val[13] EQ job-mch.blank-no OR
               job-mch.blank-no EQ 0)
        NO-LOCK:

      CREATE tt-job-hdr.
      ASSIGN
       tt-job-hdr.company      = reftable.company
       tt-job-hdr.job          = job-mch.job
       tt-job-hdr.job-no       = job-mch.job-no
       tt-job-hdr.job-no2      = job-mch.job-no2
       tt-job-hdr.frm          = reftable.val[12]
       tt-job-hdr.blank-no     = reftable.val[13]
       tt-job-hdr.i-no         = reftable.code2
       tt-job-hdr.std-lab-cost = reftable.val[1]
       tt-job-hdr.std-mat-cost = reftable.val[2]
       tt-job-hdr.std-var-cost = reftable.val[3]
       tt-job-hdr.std-fix-cost = reftable.val[4]
       tt-job-hdr.std-tot-cost = reftable.val[5]
       tt-job-hdr.last-mach    = job-mch.m-code EQ mch-act.m-code AND
                                 job-mch.pass   EQ mch-act.pass.
    END.

    FIND FIRST tt-job-hdr NO-ERROR.
    IF NOT AVAIL tt-job-hdr THEN
    FOR EACH job-hdr
        WHERE job-hdr.company   EQ cocode
          AND job-hdr.job-no    EQ job-mch.job-no
          AND job-hdr.job-no2   EQ job-mch.job-no2
          AND (job-hdr.frm      EQ mch-act.frm OR
               v-est-type       EQ 2           OR
               ll-set-mach)
          AND (job-hdr.blank-no EQ job-mch.blank-no OR
               job-mch.blank-no EQ 0                OR
               ll-set-mach)
        NO-LOCK:
      CREATE tt-job-hdr.
      BUFFER-COPY job-hdr TO tt-job-hdr
      ASSIGN
       tt-job-hdr.last-mach = job-mch.m-code EQ mch-act.m-code AND
                              job-mch.pass   EQ mch-act.pass.
    END.
  END.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION nextRecKey Dialog-Frame 
FUNCTION nextRecKey RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
  RETURN STRING(TODAY,'99999999') + STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),'99999999').

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

