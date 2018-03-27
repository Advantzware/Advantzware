&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: util\replacestyle.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEF VAR k_frac as dec init 6.25 no-undo.
{sys/inc/f16to32.i}
{cec/descalc.i "new"}

def new shared buffer xest    for est.
def new shared buffer xef     for ef.
def new shared buffer xeb     for eb.
DEF VAR ld-k-wid-array LIKE eb.k-wid-array2 NO-UNDO.
DEF VAR ld-k-len-array LIKE eb.k-len-array2 NO-UNDO.
def new shared temp-table formule field formule as dec extent 12.
DEF VAR lv-panels as log no-undo.
DEF VAR ll-blank-size-changed AS LOG NO-UNDO.

DO TRANSACTION:
   {sys/inc/ceroute.i C}
END.

def TEMP-TABLE w-box-h NO-UNDO like box-design-hdr.
def TEMP-TABLE w-box-l NO-UNDO like box-design-line.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-old-style v-new-style v-cust-no ~
v-recalc-estimates Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS v-old-style v-new-style v-cust-no ~
v-recalc-estimates v-status 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE v-cust-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Customer #" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.1
     FONT 6 NO-UNDO.

DEFINE VARIABLE v-new-style AS CHARACTER FORMAT "X(6)":U 
     LABEL "New Style" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.1
     FONT 6 NO-UNDO.

DEFINE VARIABLE v-old-style AS CHARACTER FORMAT "X(6)":U 
     LABEL "Old Style" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.1
     FONT 6 NO-UNDO.

DEFINE VARIABLE v-status AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 80 BY 1.1 NO-UNDO.

DEFINE VARIABLE v-recalc-estimates AS LOGICAL INITIAL yes 
     LABEL "Auto Recalc Estimates?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     v-old-style AT ROW 1.76 COL 19 COLON-ALIGNED
     v-new-style AT ROW 3 COL 19 COLON-ALIGNED
     v-cust-no AT ROW 4.1 COL 19 COLON-ALIGNED WIDGET-ID 2
     v-recalc-estimates AT ROW 5.76 COL 19 WIDGET-ID 4
     Btn_OK AT ROW 8.62 COL 21
     Btn_Cancel AT ROW 8.62 COL 38.6
     v-status AT ROW 10.76 COL 1 NO-LABEL
     SPACE(0.79) SKIP(0.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 2
         TITLE "Replace Style on Corrugated Estimates"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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

/* SETTINGS FOR FILL-IN v-status IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Replace Style on Corrugated Estimates */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    ASSIGN {&DISPLAYED-OBJECTS}.
           
    MESSAGE "Are you sure you wish to Replace Styles?"
       VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
    IF ll-ans THEN
       RUN replace-styles-proc.
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

  find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "PANELS"
             no-lock no-error.
  if not avail sys-ctrl then do transaction:
     create sys-ctrl.
     assign sys-ctrl.company = cocode
            sys-ctrl.name    = "PANELS"
            sys-ctrl.descrip = "CE Lock=Yes Panel Size Popup when Overriding W&L?"
            sys-ctrl.log-fld = yes.
  end.
  lv-panels = sys-ctrl.log-fld.

  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-box1 Dialog-Frame 
PROCEDURE build-box1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR char-hdl AS cha NO-UNDO.
   DEF VAR lv-eb-form-no AS INT NO-UNDO.
   DEF VAR lv-eb-blank-no AS INT NO-UNDO.

   IF NOT AVAIL xeb THEN RETURN.

   def buffer xbox-design-hdr  for box-design-hdr.
   def buffer xbox-design-line for box-design-line.

   cocode = xeb.company.

   EMPTY TEMP-TABLE w-box-h.
   EMPTY TEMP-TABLE w-box-l.

   for each box-design-hdr where
       box-design-hdr.design-no = 0 and
       box-design-hdr.company = xeb.company AND
       box-design-hdr.est-no = xeb.est-no AND
       box-design-hdr.form-no   eq xeb.form-no AND
       box-design-hdr.blank-no  eq xeb.blank-no
       no-lock:

       create w-box-h.
       buffer-copy box-design-hdr to w-box-h.

       FOR EACH box-design-line OF box-design-hdr NO-LOCK:
           CREATE w-box-l.
           BUFFER-COPY box-design-line TO w-box-l.
       END.
   end.

   {cec/est-6del.i}

   find first xbox-design-hdr where
        xbox-design-hdr.design-no eq style.design-no AND
        xbox-design-hdr.company   eq style.company and
        xbox-design-hdr.est-no    eq ""
        no-lock no-error.

   if avail xbox-design-hdr then do:
      
      run cec/descalc.p (recid(xest), recid(xeb)).
      
      create box-design-hdr.
      assign  box-design-hdr.design-no   = 0
              box-design-hdr.company = xeb.company
              box-design-hdr.est-no      = xeb.est-no
              box-design-hdr.form-no     = xeb.form-no
              box-design-hdr.blank-no    = xeb.blank-no
              box-design-hdr.description = if avail xbox-design-hdr then
                                             xbox-design-hdr.description else ""
              box-design-hdr.lscore      = v-lscore-c
              box-design-hdr.lcum-score  = v-lcum-score-c
              box-design-hdr.wscore = xbox-design-hdr.wscore
              box-design-hdr.wcum-score = xbox-design-hdr.wcum-score
              box-design-hdr.box-text = xbox-design-hdr.box-text
              box-design-hdr.box-image = xbox-design-hdr.box-image
              box-design-hdr.box-3d-image = xbox-design-hdr.box-3d-image.
           
      for each xbox-design-line of xbox-design-hdr no-lock:
          create box-design-line.
          assign box-design-line.design-no  = box-design-hdr.design-no
                 box-design-line.company = box-design-hdr.company
                 box-design-line.est-no      = box-design-hdr.est-no
                 box-design-line.form-no    = box-design-hdr.form-no
                 box-design-line.blank-no   = box-design-hdr.blank-no
                 box-design-line.line-no    = xbox-design-line.line-no
                 box-design-line.line-text  = xbox-design-line.line-text.
         
          find first w-box-design-line
               where w-box-design-line.line-no eq box-design-line.line-no   no-error.
         
          if avail w-box-design-line then
             assign  box-design-line.wscore     = w-box-design-line.wscore-c
                     box-design-line.wcum-score = w-box-design-line.wcum-score-c.
      end.
   end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-blank-size Dialog-Frame 
PROCEDURE calc-blank-size :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   def buffer bf-eb for eb .
   DEF VAR i as int no-undo.
   DEF VAR j as int no-undo.
   DEF VAR v-score-char like v-lscore-c extent 100.

   find FIRST xest where 
        xest.company = eb.company and
        xest.est-no = eb.est-no
        no-lock no-error.

   find xef where recid(xef) = recid(ef) no-lock.
   find xeb where recid(xeb) = recid(eb) no-lock.
   
   ASSIGN
      ld-k-wid-array = 0
      ld-k-len-array = 0
      lv-k-wid-scr-type = ""
      lv-k-len-scr-type = "".

   DO i = 1 TO EXTENT(eb.k-wid-array2):
      ASSIGN
      ld-k-wid-array[i]    = eb.k-wid-array2[i]
      lv-k-wid-scr-type[i] = eb.k-wid-scr-type2[i].
   END.
   DO i = 1 TO EXTENT(eb.k-len-array2):
      ASSIGN
      ld-k-len-array[i]    = eb.k-len-array2[i]
      lv-k-len-scr-type[i] = eb.k-len-scr-type2[i].
   END.

   if style.type <> "F" then
      run calc-blank-size2. 

   {cec/msfcalc.i}
   run est/u2kinc1c.p (recid(eb)).
   run est/u2kinc2c.p (recid(eb)).
   find first formule no-lock no-error.
   find bf-eb of eb exclusive-lock.

   assign 
       bf-eb.t-wid = formule.formule[1]
       bf-eb.t-len = formule.formule[2]
       bf-eb.t-sqin = formule.formule[7] * formule.formule[8]
       bf-eb.k-wid-array2 = 0
       bf-eb.k-len-array2 = 0.

   if not lv-panels or style.type = "F" then 
      assign bf-eb.k-wid-array2[1] = bf-eb.t-wid
             bf-eb.k-len-array2[1] = bf-eb.t-len.
   else do:
      run cec/descalc.p (recid(xest),recid(xeb)).

      DO i = 1 TO EXTENT(xeb.k-wid-scr-type2):
        ASSIGN
         xeb.k-wid-scr-type2[i] = lv-k-wid-scr-type[i]
         xeb.k-len-scr-type2[i] = lv-k-len-scr-type[i].
      END.

      if v-lscore-c begins "No" then
         assign  xeb.k-wid-array2[1] = xeb.t-wid
                 xeb.k-len-array2[1] = xeb.t-len.
      else do:
        i = 0.
        for each w-box-design-line:
           ASSIGN
           i = i + 1
           xeb.k-wid-array2[i] = w-box-design-line.wscore-d.
           {sys/inc/k16bb.i xeb.k-wid-array2[i]}
        end.
        assign  v-score-char    = ""
                j               = 1.

        do i = 1 to 80:
          if substr(v-lscore-c,i,1) ne "" then do:
             v-score-char[j] = v-score-char[j] + substr(v-lscore-c,i,1).
             if substr(v-lscore-c,i + 1,1) eq "" then
                assign  v-score-char[j] = trim(v-score-char[j])
                        j = j + 1.
          end.
          if j gt EXTENT(xeb.k-len-array2) then leave.
        end.
        DO i = 1 TO EXTENT(xeb.k-len-array2):
           xeb.k-len-array2[i] = dec(v-score-char[i]).
           {sys/inc/k16bb.i xeb.k-len-array2[i]}.
        end.
      end.  /* else v-lscore */
    end. /* panels or not foam */
   
   IF NOT ll-blank-size-changed THEN
   DO i = 1 TO EXTENT(ld-k-wid-array):
     IF xeb.k-wid-array2[i] NE ld-k-wid-array[i] THEN DO:
       ll-blank-size-changed = YES.
       LEAVE.
     END.
   END.

   IF NOT ll-blank-size-changed THEN
   DO i = 1 TO EXTENT(ld-k-len-array):
      IF xeb.k-len-array2[i] NE ld-k-len-array[i] THEN DO:
         ll-blank-size-changed = YES.
         LEAVE.
      END.
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-blank-size2 Dialog-Frame 
PROCEDURE calc-blank-size2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND CURRENT eb.
   
   find xeb where recid(xeb) = recid(eb) no-lock.

   IF style.material[7] ne "" then do:
      eb.adhesive = style.material[7].
      if eb.gluelap ne 0 then eb.lin-in = eb.dep.
   end.

   {est/u2estc.i eb.gluelap 1}
   {est/u2estc.i eb.k-wid 2}

   find first item where
        item.company = est.company AND
        item.i-no eq eb.adhesive
        no-lock no-error.
   if avail item and eb.adhesive ne "NO JOINT" then do:
      if item.mat-type eq "G" then do:
         if eb.tab-in then do:
            {est/u2estc.i eb.k-len 3}
         end.
         else do:
            eb.tab-in = no.
            {est/u2estc.i eb.k-len 4}
         end.
      end.
      else if item.mat-type eq "S" then do:
           if eb.tab-in then do:
              {est/u2estc.i eb.k-len 5}
           end.
           else do:
              eb.tab-in = no.
              {est/u2estc.i eb.k-len 6}
           end.
      end.
      else if item.mat-type eq "T" then do:
           eb.tab-in = ?.
           {est/u2estc.i eb.k-len 7}
      end.
    end.
    else do:
         eb.tab-in = ?.
         {est/u2estc.i eb.k-len 7}
    end.

    if eb.len eq eb.wid
    then do:
         {est/u2estc.i eb.k-wid 2 dim-fit}
    end.
    else do:
         {est/u2estc.i eb.k-wid 2}
    end.
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
  DISPLAY v-old-style v-new-style v-cust-no v-recalc-estimates v-status 
      WITH FRAME Dialog-Frame.
  ENABLE v-old-style v-new-style v-cust-no v-recalc-estimates Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE one-eb-on-ef Dialog-Frame 
PROCEDURE one-eb-on-ef :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAM op-one-eb AS LOG NO-UNDO.

  DEF BUFFER b-ac-eb FOR eb.
  DEF BUFFER b-ac-ef FOR ef.


  FIND b-ac-ef WHERE ROWID(b-ac-ef) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-ac-ef THEN
  FIND b-ac-eb
      WHERE b-ac-eb.company EQ b-ac-ef.company
        AND b-ac-eb.est-no  EQ b-ac-ef.est-no
        AND b-ac-eb.eqty    EQ b-ac-ef.eqty
        AND b-ac-eb.form-no EQ b-ac-ef.form-no
      NO-LOCK NO-ERROR.

  op-one-eb = AVAIL b-ac-eb.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE replace-styles-proc Dialog-Frame 
PROCEDURE replace-styles-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER bf-style FOR style.
   DEF BUFFER bf-reftable FOR reftable.
   DEF BUFFER bf-flute FOR flute.
   DEF BUFFER bf-stack-flute FOR stack-flute.
   DEF BUFFER bf-routing-mtx FOR routing-mtx.
   DEF BUFFER bf-box-design-hdr FOR box-design-hdr.
   DEF BUFFER b-set for eb.
   DEF BUFFER b-eb FOR eb.
   DEF VAR v-old-t-wid LIKE eb.t-wid NO-UNDO.
   DEF VAR v-old-t-len LIKE eb.t-len NO-UNDO.
   DEF VAR v-old-wid LIKE eb.wid NO-UNDO.
   DEF VAR v-old-len LIKE eb.len NO-UNDO.
   DEF VAR v-old-dep LIKE eb.dep NO-UNDO.
   DEF VAR ll-one-eb-on-ef AS LOG NO-UNDO.
   DEF VAR ll-set AS LOG NO-UNDO.

   SESSION:SET-WAIT-STATE("general").

   IF NOT CAN-FIND(FIRST style WHERE
      style.company EQ cocode AND
      style.style EQ v-old-style AND
      style.industry EQ "2") THEN
      DO:
         MESSAGE "Invalid Old Style."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO v-old-style IN FRAME {&FRAME-NAME}.
         LEAVE.
      END.

   FIND FIRST style WHERE
        style.company EQ cocode AND
        style.style EQ v-new-style AND
        style.industry EQ "2"
        NO-LOCK NO-ERROR.
    
   IF NOT AVAIL style THEN
      DO:
         MESSAGE "Invalid New Style."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO v-new-style IN FRAME {&FRAME-NAME}.
         LEAVE.
      END.

   IF v-cust-no EQ "" OR NOT CAN-FIND(FIRST cust WHERE
      cust.company EQ cocode AND
      cust.cust-no EQ v-cust-no) THEN
      DO:
         MESSAGE "Invalid Customer #."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO v-cust-no IN FRAME {&FRAME-NAME}.
         LEAVE.
      END.

    FOR EACH eb WHERE
        eb.company EQ cocode AND
        eb.cust-no EQ v-cust-no AND
        eb.style EQ v-old-style:

        eb.style = v-new-style.

        IF v-recalc-estimates THEN
        DO:
           ASSIGN
              ll-blank-size-changed = NO
              v-old-t-wid = eb.t-wid
              v-old-t-len = eb.t-len
              v-old-wid = eb.wid
              v-old-len = eb.len
              v-old-dep = eb.dep.

           FIND FIRST ef WHERE
                ef.company EQ cocode AND
                ef.est-no EQ eb.est-no AND
                ef.form-no EQ eb.form-no.

           assign
              eb.adhesive = style.material[7]
              eb.gluelap = style.dim-gl
              eb.fpanel = style.dim-pan5
              eb.lock = style.dim-fit
              eb.tuck = style.dim-tk
              eb.sty-lock = NO.

           RUN calc-blank-size.

           IF style.TYPE EQ "F" THEN
              ASSIGN
                 eb.t-wid = eb.wid
                 eb.t-len = eb.len
                 eb.t-dep = eb.dep.

           IF v-cecscrn-char NE "Decimal" THEN
              ASSIGN
                 eb.t-wid = TRUNC(eb.t-wid * li-16-32,0) / li-16-32
                 eb.t-len = TRUNC(eb.t-len * li-16-32,0) / li-16-32.

           IF style.TYPE EQ "F" THEN
              eb.t-sqin = eb.t-len * eb.t-wid.

           IF NOT ll-blank-size-changed AND
              (eb.t-wid NE v-old-t-wid OR  
               eb.t-len NE v-old-t-len OR
               eb.wid   NE v-old-wid OR
               eb.len   NE v-old-len OR
               eb.dep   NE v-old-dep) THEN
              ll-blank-size-changed = YES.

           RUN one-eb-on-ef (ROWID(ef), OUTPUT ll-one-eb-on-ef).

           IF ll-one-eb-on-ef AND ll-blank-size-changed THEN DO:
              ef.lsh-lock = NO.    
              RUN update-sheet.    
           END.

           RUN reset-box-design.

           FIND FIRST est WHERE
                est.company EQ eb.company AND
                est.est-no EQ eb.est-no
                NO-LOCK.

           IF eb.stock-no NE "" THEN
           DO:
              FIND FIRST itemfg WHERE
                   itemfg.company EQ eb.company AND
                   itemfg.i-no EQ eb.stock-no
                   NO-ERROR.

              IF AVAIL itemfg THEN
              DO:
                 itemfg.style = eb.style.
                 RELEASE itemfg.
              END.
           END.

           /* update set info from eb for two piece box*/
           IF AVAIL est AND est.est-type = 6 THEN DO:
              FIND FIRST b-set WHERE
                   b-set.company = est.company AND
                   b-set.est-no = est.est-no AND
                   b-set.form-no = 0
                   NO-ERROR.
              
              li = 0.
              FOR EACH b-eb WHERE
                  b-eb.company EQ est.company AND
                  b-eb.est-no  EQ est.est-no AND
                  b-eb.form-no NE 0 NO-LOCK:
                  li = li + 1.
              END.
              ll-set = li GT 1.
              IF NOT ll-set THEN DO: /* it's two piece box */ 
                 IF NOT AVAIL b-set THEN DO:
                    CREATE b-set.
                    ASSIGN
                       b-set.est-type = 6
                       b-set.company  = eb.company
                       b-set.loc      = eb.loc
                       b-set.e-num    = eb.e-num
                       b-set.est-no   = eb.est-no
                       b-set.form-no  = 0
                       b-set.blank-no = 0
                       b-set.est-int  = INT(eb.est-no).
                 END.
                 IF NEW b-set THEN
                    ASSIGN
                       b-set.stock-no = eb.stock-no
                       b-set.part-no = eb.part-no
                       b-set.part-dscr1 = eb.part-dscr1
                       b-set.part-dscr2 = eb.part-dscr2
                       b-set.procat = eb.procat
                       b-set.len = eb.len
                       b-set.wid = eb.wid
                       b-set.dep = eb.dep.
              END.

              RELEASE b-set.
           END.
        END.
    END.

    MESSAGE "Replace Style on Corrugated Estimates Completed."
        VIEW-AS ALERT-BOX BUTTONS OK.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-box-design Dialog-Frame 
PROCEDURE reset-box-design :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   find xeb where recid(xeb) = recid(eb) no-lock.

   find first xest where
        xest.company = xeb.company and
        xest.est-no = xeb.est-no
        no-lock.

    find first xef where
         xef.company = xeb.company AND
         xef.est-no  = xeb.est-no AND
         xef.form-no eq xeb.form-no
         no-lock.

    RUN build-box1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-sheet Dialog-Frame 
PROCEDURE update-sheet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   find xef where recid(xef) = recid(ef).
   find xeb where recid(xeb) = recid(eb).

   assign xef.n-out  = 0
          xef.n-out-l = 0
          xef.n-out-d = 0
          xef.gsh-len = 0
          xef.gsh-wid = 0
          xef.gsh-dep = 0
          xef.nsh-len = 0
          xef.nsh-wid = 0
          xef.nsh-dep = 0
          xef.trim-w = 0
          xef.trim-l = 0
          xef.trim-d = 0
          xeb.num-len = 0
          xeb.num-wid = 0
          xeb.num-dep = 0.
   
   IF style.TYPE NE "F" THEN DO:
     {sys/inc/ceroute1.i w id l en}
   END.
      
   RUN cec/calc-dim.p.
  
   IF ceroute-chr NE "" THEN DO:
     FIND FIRST mach
         WHERE mach.company EQ ef.company
           AND mach.loc     EQ ef.loc
           AND mach.m-code  EQ ceroute-chr
           AND mach.dept[1] EQ "CR"
         NO-LOCK NO-ERROR.
     IF AVAIL mach THEN DO:
       ASSIGN
        xef.m-code   = ceroute-chr
        xef.lsh-lock = NO
        xeb.num-wid  = 1
        xeb.num-len  = 1.
  
       RUN cec/calc-dim1.p NO-ERROR.
  
       ASSIGN
        xef.gsh-len = xef.gsh-len - (xef.nsh-len * xef.n-out-l)
        xef.n-out-l = 1
        xef.gsh-len = xef.gsh-len + (xef.nsh-len * xef.n-out-l).
  
       IF ceroute-int NE 0 AND ceroute-int LT xef.gsh-wid THEN
         ASSIGN
          xef.n-out   = TRUNC(ceroute-int / xef.nsh-wid,0)
          xef.gsh-wid = xef.n-out * xef.nsh-wid + (mach.min-trimw * 2).
     END.
   END.
  
   find xef where recid(xef) = recid(ef) no-lock.
   find xeb where recid(xeb) = recid(eb) no-lock.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

