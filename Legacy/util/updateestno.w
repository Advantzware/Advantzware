&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: util\updateestno.w
  
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
&Scoped-Define ENABLED-OBJECTS begin_est new_est Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_est new_est v-status 

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

DEFINE VARIABLE begin_est AS CHARACTER FORMAT "X(8)" 
     LABEL "Old Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE new_est AS CHARACTER FORMAT "X(8)" 
     LABEL "New Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE v-status AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 80 BY 1.1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_est AT ROW 1.71 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Estimate" WIDGET-ID 6
     new_est AT ROW 3.38 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Estimate" WIDGET-ID 8
     Btn_OK AT ROW 8.62 COL 21
     Btn_Cancel AT ROW 8.62 COL 38.6
     v-status AT ROW 10.76 COL 1 NO-LABEL
     SPACE(0.79) SKIP(0.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 2
         TITLE "Update Estimate Number"
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

ASSIGN 
       begin_est:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       new_est:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR FILL-IN v-status IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Update Estimate Number */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est Dialog-Frame
ON HELP OF begin_est IN FRAME Dialog-Frame /* Old Estimate# */
DO:
     DEF VAR char-val AS cha NO-UNDO.
     DEF var lv-eb-tmpid as recid no-undo.
    
     run windows/l-est.w (cocode,locode,focus:screen-value, output char-val).
              
     if char-val <> "" then do:                 
        FIND FIRST eb WHERE string(RECID(eb)) = (char-val) NO-LOCK NO-ERROR.
        IF AVAIL eb THEN ASSIGN FOCUS:SCREEN-VALUE = eb.est-no
                                       lv-eb-tmpid = RECID(eb)    
                            begin_est:SCREEN-VALUE = eb.est-no.
                            
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est Dialog-Frame
ON LEAVE OF begin_est IN FRAME Dialog-Frame /* Old Estimate# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    ASSIGN {&DISPLAYED-OBJECTS}.
           
    IF TRIM(begin_est) EQ "" OR
       TRIM(NEW_est) EQ "" THEN
       DO:
          MESSAGE "Old or New Estimate # cannot be blank."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          RETURN.
       END.

    MESSAGE "Are you sure you wish to Replace Old Estimate# with New Estimate #?"
       VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.

    IF ll-ans THEN
       RUN replace-est-proc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME new_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL new_est Dialog-Frame
ON LEAVE OF new_est IN FRAME Dialog-Frame /* New Estimate# */
DO:
     assign {&self-name}.
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
  DISPLAY begin_est new_est v-status 
      WITH FRAME Dialog-Frame.
  ENABLE begin_est new_est Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE replace-est-proc Dialog-Frame 
PROCEDURE replace-est-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DISABLE TRIGGERS FOR LOAD OF e-item-vend.
   DISABLE TRIGGERS FOR LOAD OF e-itemfg-vend.
   DISABLE TRIGGERS FOR LOAD OF eb.
   DISABLE TRIGGERS FOR LOAD OF reftable.
   DISABLE TRIGGERS FOR LOAD OF ef.
   DISABLE TRIGGERS FOR LOAD OF ef-nsh.
   DISABLE TRIGGERS FOR LOAD OF est-flm.
   DISABLE TRIGGERS FOR LOAD OF est-inst.
   DISABLE TRIGGERS FOR LOAD OF est-op.
   DISABLE TRIGGERS FOR LOAD OF est-prep.
   DISABLE TRIGGERS FOR LOAD OF est-qty.
   DISABLE TRIGGERS FOR LOAD OF est-summ.
   DISABLE TRIGGERS FOR LOAD OF notes.
   DISABLE TRIGGERS FOR LOAD OF probe.
   DISABLE TRIGGERS FOR LOAD OF probeit.
   DISABLE TRIGGERS FOR LOAD OF notes.
   DISABLE TRIGGERS FOR LOAD OF box-design-line.
   DISABLE TRIGGERS FOR LOAD OF box-design-hdr.
   DISABLE TRIGGERS FOR LOAD OF est.

   SESSION:SET-WAIT-STATE("general").

   ASSIGN
      begin_est  = FILL(" ",8 - LENGTH(TRIM(begin_est))) + TRIM(begin_est)
      new_est    = FILL(" ",8 - LENGTH(TRIM(new_est))) + TRIM(new_est).

   FOR EACH e-item-vend WHERE
       e-item-vend.company EQ cocode AND
       e-item-vend.est-no EQ new_est:

       DELETE e-item-vend.
   END.

   FOR EACH e-item-vend WHERE
       e-item-vend.company EQ cocode AND
       e-item-vend.est-no EQ begin_est:

       e-item-vend.est-no = new_est.
   END.

   FOR EACH e-itemfg-vend WHERE
       e-itemfg-vend.company EQ cocode AND
       e-itemfg-vend.est-no EQ new_est:       

       DELETE e-itemfg-vend.
   END.

   FOR EACH e-itemfg-vend WHERE
       e-itemfg-vend.company EQ cocode AND
       e-itemfg-vend.est-no EQ begin_est:       

       e-itemfg-vend.est-no = new_est.
   END.

   FOR EACH eb WHERE
       eb.company EQ cocode AND
       eb.est-no EQ new_est:

       DELETE eb.
   END.

   FOR EACH eb WHERE
       eb.company EQ cocode AND
       eb.est-no EQ begin_est:

       eb.est-no = new_est.
   END.

   FOR EACH ef WHERE
       ef.company EQ cocode AND
       ef.est-no EQ new_est:

       for each reftable WHERE
           reftable.reftable eq "EST-MISC" AND
           reftable.company  eq ef.company AND
           reftable.loc      eq ef.loc AND
           reftable.code     eq trim(ef.est-no) + string(ef.form-no,"/99"):
           DELETE reftable.
       END.

       DELETE ef.
   END.

   FOR EACH ef WHERE
       ef.company EQ cocode AND
       ef.est-no EQ begin_est:

       for each reftable WHERE
           reftable.reftable eq "EST-MISC" AND
           reftable.company  eq ef.company AND
           reftable.loc      eq ef.loc AND
           reftable.code     eq trim(ef.est-no) + string(ef.form-no,"/99"):
           
           reftable.CODE = trim(new_est) + string(ef.form-no,"/99").
       END.

       ef.est-no = new_est.

   END.

   FOR EACH ef-nsh WHERE
       ef-nsh.company EQ cocode AND
       ef-nsh.est-no EQ new_est:

       DELETE ef-nsh.
   END.

   FOR EACH ef-nsh WHERE
       ef-nsh.company EQ cocode AND
       ef-nsh.est-no EQ begin_est:

       ef-nsh.est-no = new_est.
   END.

   FOR EACH est-flm WHERE
       est-flm.company EQ cocode AND
       est-flm.est-no EQ new_est:

       DELETE est-flm.
   END.

   FOR EACH est-flm WHERE
       est-flm.company EQ cocode AND
       est-flm.est-no EQ begin_est:

       est-flm.est-no = new_est.
   END.

   FOR EACH est-inst WHERE
       est-inst.company EQ cocode AND
       est-inst.est-no EQ new_est:

       DELETE est-inst.
   END.

   FOR EACH est-inst WHERE
       est-inst.company EQ cocode AND
       est-inst.est-no EQ begin_est:

       est-inst.est-no = new_est.
   END.

   FOR EACH est-op WHERE
       est-op.company EQ cocode AND
       est-op.est-no EQ new_est:

       DELETE est-op.
   END.

   FOR EACH est-op WHERE
       est-op.company EQ cocode AND
       est-op.est-no EQ begin_est:

       est-op.est-no = new_est.
   END.

   FOR EACH est-prep WHERE
       est-prep.company EQ cocode AND
       est-prep.est-no EQ new_est:

       DELETE est-prep.
   END.

   FOR EACH est-prep WHERE
       est-prep.company EQ cocode AND
       est-prep.est-no EQ begin_est:

       est-prep.est-no = new_est.
   END.

   FOR EACH est-qty WHERE
       est-qty.company EQ cocode AND
       est-qty.est-no EQ new_est:

       DELETE est-qty.
   END.

   FOR EACH est-qty WHERE
       est-qty.company EQ cocode AND
       est-qty.est-no EQ begin_est:

       est-qty.est-no = new_est.
   END.

   FOR EACH est-summ WHERE
       est-summ.company EQ cocode AND
       est-summ.est-no EQ new_est:

       DELETE est-summ.
   END.

   FOR EACH est-summ WHERE
       est-summ.company EQ cocode AND
       est-summ.est-no EQ begin_est:

       est-summ.est-no = new_est.
   END.

   FOR EACH probe WHERE
       probe.company EQ cocode AND
       probe.est-no EQ new_est:

       DELETE probe.
   END.

   FOR EACH probe WHERE
       probe.company EQ cocode AND
       probe.est-no EQ begin_est:

       probe.est-no = new_est.
   END.

   FOR EACH probeit WHERE
       probeit.company EQ cocode AND
       probeit.est-no EQ new_est:

       DELETE probeit.
   END.

   FOR EACH probeit WHERE
       probeit.company EQ cocode AND
       probeit.est-no EQ begin_est:

       probeit.est-no = new_est.
   END.

   FOR EACH box-design-line WHERE
       box-design-line.company EQ cocode AND
       box-design-line.est-no EQ new_est:

       DELETE box-design-line.
   END.

   FOR EACH box-design-line WHERE
       box-design-line.company EQ cocode AND
       box-design-line.est-no EQ begin_est:

       box-design-line.est-no = new_est.
   END.

   FOR EACH box-design-hdr WHERE
       box-design-hdr.company EQ cocode AND
       box-design-hdr.est-no EQ new_est:

       DELETE box-design-hdr.
   END.

   FOR EACH box-design-hdr WHERE
       box-design-hdr.company EQ cocode AND
       box-design-hdr.est-no EQ begin_est:

       box-design-hdr.est-no = new_est.
   END.

   FOR EACH est WHERE
       cocode EQ cocode AND
       est.est-no EQ new_est:

       DELETE est.
   END.

   FOR EACH est WHERE
       cocode EQ cocode AND
       est.est-no EQ begin_est:

       est.est-no = new_est.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "gsa-fm" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ "" AND
       reftable.code     EQ new_est:

       DELETE reftable.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "gsa-fm" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ "" AND
       reftable.code     EQ begin_est:

       reftable.CODE = new_est.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "est/probeset.i" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ "" AND
       reftable.code     EQ new_est:

       DELETE reftable.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "est/probeset.i" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ "" AND
       reftable.code     EQ begin_est:

       reftable.CODE = NEW_est.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "probe-ref" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ "" AND
       reftable.code     EQ new_est:

       DELETE reftable.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "probe-ref" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ "" AND
       reftable.code     EQ begin_est:

       reftable.CODE = NEW_est.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "ce/com/probemk.p" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ new_est:

       DELETE reftable.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "ce/com/probemk.p" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ begin_est:

       reftable.loc = NEW_est.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "probe.per-msf" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ "" AND
       reftable.code     EQ new_est:

       DELETE reftable.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "probe.per-msf" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ "" AND
       reftable.code     EQ begin_est:

       reftable.CODE = NEW_est.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "probe.per-ref" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ "" AND
       reftable.code     EQ new_est:

       DELETE reftable.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "probe.per-ref" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ "" AND
       reftable.code     EQ begin_est:

       reftable.CODE = NEW_est.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "probe.per-board" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ "" AND
       reftable.code     EQ new_est:

       DELETE reftable.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "probe.per-board" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ "" AND
       reftable.code     EQ begin_est:

       reftable.CODE = NEW_est.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "ce/com/selwhif1.w" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ new_est:

       DELETE reftable.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "ce/com/selwhif1.w" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ begin_est:

       reftable.loc = NEW_est.
   END.

  
   FOR EACH reftable WHERE
       reftable.reftable EQ "est/getqty.w" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ locode AND
       reftable.code     EQ new_est:

       DELETE reftable.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "est/getqty.w" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ locode AND
       reftable.code     EQ begin_est:

       reftable.CODE = NEW_est.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "est\d-multbl.w" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ locode AND
       reftable.code     EQ new_est:

       DELETE reftable.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "est\d-multbl.w" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ locode AND
       reftable.code     EQ begin_est:

       reftable.CODE = NEW_est.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "est/d-grpcst.w" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ locode AND
       reftable.code     EQ new_est:

       DELETE reftable.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "est/d-grpcst.w" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ locode AND
       reftable.code     EQ begin_est:

       reftable.CODE = NEW_est.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "est/getqty.w2" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ "" AND
       reftable.code     EQ new_est:

       DELETE reftable.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "est/getqty.w2" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ "" AND
       reftable.code     EQ begin_est:

       reftable.CODE = NEW_est.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "PLATE/FOUNTAIN" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ new_est:

       DELETE reftable.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "PLATE/FOUNTAIN" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ begin_est:

       reftable.loc = NEW_est.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "ce/v-est3.w Unit#" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ new_est:

       DELETE reftable.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "ce/v-est3.w Unit#" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ begin_est:

       reftable.loc = NEW_est.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "cedepth" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ new_est:

       DELETE reftable.
   END.

   FOR EACH reftable WHERE
       reftable.reftable EQ "cedepth" AND
       reftable.company  EQ cocode AND
       reftable.loc      EQ begin_est:

       reftable.loc = NEW_est.
   END.

   MESSAGE "Replace Estimate # Completed."
        VIEW-AS ALERT-BOX BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

