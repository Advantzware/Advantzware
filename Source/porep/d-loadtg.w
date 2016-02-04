&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: porep\d-loadtg.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{rmrep/rmloadtg.i}
{custom/globdefs.i}

DEF VAR calcTags AS LOG NO-UNDO INIT YES. /* sys ctrl option */
DEF VAR autoCopy AS LOG NO-UNDO.
DEF VAR copyRowID AS ROWID NO-UNDO.

DEFINE VARIABLE receiptQty LIKE w-po.rcpt-qty NO-UNDO.
DEFINE VARIABLE partialQty LIKE w-po.partial NO-UNDO.
DEFINE VARIABLE totalTags LIKE w-po.total-tags NO-UNDO.
DEFINE VARIABLE poCost LIKE w-po.cost NO-UNDO.
DEFINE VARIABLE costUOM LIKE w-po.pr-uom NO-UNDO.
DEFINE VARIABLE consUOM LIKE w-po.cons-uom NO-UNDO.

DEF TEMP-TABLE tt-po NO-UNDO
    FIELD po-no AS INT
    FIELD LINE AS INT
    FIELD tot-rec-qty AS DEC
    FIELD cons-uom AS CHAR
    FIELD overrun-qty AS DEC
    INDEX po po-no ASC LINE ASC.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES w-po

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 w-po.po-no w-po.ord-qty w-po.pr-qty-uom w-po.line w-po.job-no w-po.job-no2 NO-LABEL w-po.vend-no w-po.i-no w-po.s-wid w-po.s-len w-po.rcpt-qty w-po.partial w-po.total-tags w-po.overrun-qty w-po.tot-rec-qty w-po.cons-uom   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 w-po.rcpt-qty   w-po.partial   w-po.cons-uom   w-po.total-tags   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 w-po
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 w-po
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH w-po BY w-po.po-no BY w-po.line
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH w-po BY w-po.po-no BY w-po.line.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 w-po
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 w-po


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 btnCancel btn_save btn_copy ~
btn_delete Btn_OK btnAutoCopy 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAutoCopy 
     LABEL "&Auto Copy Off" 
     SIZE 20 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON btnCancel 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.62.

DEFINE BUTTON btn_copy 
     LABEL "&Copy" 
     SIZE 15 BY 1.62.

DEFINE BUTTON btn_delete 
     LABEL "&Delete" 
     SIZE 15 BY 1.62.

DEFINE BUTTON Btn_OK 
     LABEL "&Create Tags" 
     SIZE 18 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON btn_save 
     LABEL "&Save" 
     SIZE 15 BY 1.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      w-po SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 DISPLAY
      w-po.po-no LABEL 'PO#'
  w-po.ord-qty LABEL 'PO Qty'
  w-po.pr-qty-uom LABEL 'UOM'
  w-po.line LABEL 'Line'
  w-po.job-no  LABEL '  Job#'
  w-po.job-no2 NO-LABEL FORMAT '99'
  w-po.vend-no LABEL 'Vendor'
  w-po.i-no LABEL 'Item #'
  w-po.s-wid LABEL 'Width'  FORMAT '>>,>>9.9999'
  w-po.s-len LABEL 'Length' FORMAT '>>,>>9.9999'
  w-po.rcpt-qty COLUMN-LABEL 'PO/Tag Qty!Received'
  w-po.partial COLUMN-LABEL 'Partial!Tag'
  w-po.total-tags LABEL 'Tags'
  w-po.overrun-qty COLUMN-LABEL "Overrun!Qty"
  w-po.tot-rec-qty COLUMN-LABEL "MU8 + PO Qty!Rec."
  w-po.cons-uom COLUMN-LABEL 'Stock!UOM'
  ENABLE
    w-po.rcpt-qty 
    w-po.partial
    w-po.cons-uom
    w-po.total-tags
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 142 BY 14.05
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     btnCancel AT ROW 15.29 COL 5
     btn_save AT ROW 15.29 COL 27
     btn_copy AT ROW 15.29 COL 49
     btn_delete AT ROW 15.29 COL 71
     Btn_OK AT ROW 15.29 COL 93
     btnAutoCopy AT ROW 15.29 COL 118
     SPACE(5.19) SKIP(0.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 0
         TITLE "Loadtag Creation Detail".


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
/* BROWSE-TAB BROWSE-1 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH w-po BY w-po.po-no BY w-po.line.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Loadtag Creation Detail */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  APPLY "choose" TO btn_save.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON ENTRY OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  RUN saveValues.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAutoCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAutoCopy Dialog-Frame
ON CHOOSE OF btnAutoCopy IN FRAME Dialog-Frame /* Auto Copy Off */
DO:
  ASSIGN
    autoCopy = NOT autoCopy
    SELF:LABEL = '~&Auto Copy ' + STRING(autoCopy,'On/Off').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* Cancel */
DO:
  IF NOT AVAIL w-po THEN RETURN NO-APPLY.
  IF autoCopy AND copyRowID EQ ROWID(w-po) THEN DELETE w-po.
  ELSE RUN resetValues.
  {&OPEN-QUERY-{&BROWSE-NAME}}
  APPLY 'CHOOSE' TO btn_save.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_copy Dialog-Frame
ON CHOOSE OF btn_copy IN FRAME Dialog-Frame /* Copy */
DO:
  IF NOT AVAIL w-po THEN RETURN NO-APPLY.
  RUN copy-wpo.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  REPOSITION {&BROWSE-NAME} TO ROWID copyRowID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_delete Dialog-Frame
ON CHOOSE OF btn_delete IN FRAME Dialog-Frame /* Delete */
DO:
  IF NOT AVAIL w-po THEN RETURN NO-APPLY.
  MESSAGE "Are you sure you want to delete? " VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-ans AS LOG.             
  IF v-ans THEN RUN del-wpo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Create Tags */
DO:
  APPLY 'GO' TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_save Dialog-Frame
ON CHOOSE OF btn_save IN FRAME Dialog-Frame /* Save */
DO:
  IF NOT AVAIL w-po THEN RETURN NO-APPLY.
  RUN update-wpo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


ON 'RETURN' OF w-po.rcpt-qty IN BROWSE {&BROWSE-NAME} DO:
  APPLY 'LEAVE' TO w-po.rcpt-qty IN BROWSE {&BROWSE-NAME}.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'LEAVE' OF w-po.rcpt-qty IN BROWSE {&BROWSE-NAME} DO:
  IF SELF:MODIFIED THEN RUN calc-total.
END.

ON 'RETURN' OF w-po.partial IN BROWSE {&BROWSE-NAME} DO:
  APPLY 'LEAVE' TO w-po.partial IN BROWSE {&BROWSE-NAME}.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'LEAVE' OF w-po.partial IN BROWSE {&BROWSE-NAME} DO:
  IF SELF:MODIFIED THEN RUN calc-total.
END.

ON 'RETURN' OF w-po.cons-uom IN BROWSE {&BROWSE-NAME} DO:
  APPLY 'LEAVE' TO w-po.cons-uom IN BROWSE {&BROWSE-NAME}.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'LEAVE' OF w-po.cons-uom IN BROWSE {&BROWSE-NAME} DO:
  RUN valid-uom.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  RUN get-matrix.
END.

ON 'RETURN' OF w-po.total-tags IN BROWSE {&BROWSE-NAME} DO:
  APPLY 'LEAVE' TO w-po.total-tags IN BROWSE {&BROWSE-NAME}.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ g_company
      AND sys-ctrl.name    EQ 'RMTAGS' NO-ERROR.
  IF AVAIL sys-ctrl THEN
     calcTags = sys-ctrl.log-fld.

  BROWSE {&browse-name}:NUM-LOCKED-COLUMNS = 1.

  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE autoCopy Dialog-Frame 
PROCEDURE autoCopy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    APPLY 'CHOOSE' TO btn_copy.
    APPLY 'ENTRY' TO w-po.rcpt-qty IN BROWSE {&BROWSE-NAME}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-total Dialog-Frame 
PROCEDURE calc-total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAIL w-po THEN
  DO:
    ASSIGN
      w-po.partial = INT(w-po.partial:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      w-po.rcpt-qty
      w-po.total-tags = 1.
    IF calcTags THEN
    w-po.total-tags:SCREEN-VALUE = STRING(
                                   (w-po.ord-qty / w-po.rcpt-qty + .49) +
                                   (IF w-po.partial GT 0 THEN 1 ELSE 0)
                                   ).
    DISPLAY w-po.total-tags WITH BROWSE {&BROWSE-NAME}.

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-wpo Dialog-Frame 
PROCEDURE copy-wpo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-wpo FOR w-po.

  CREATE bf-wpo.
  BUFFER-COPY w-po TO bf-wpo.
  ASSIGN
    bf-wpo.total-tags = 1
    bf-wpo.partial = 0
    bf-wpo.rcpt-qty = 0
    copyRowID = ROWID(bf-wpo).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE del-wpo Dialog-Frame 
PROCEDURE del-wpo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DELETE w-po.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}

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
  ENABLE BROWSE-1 btnCancel btn_save btn_copy btn_delete Btn_OK btnAutoCopy 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix Dialog-Frame 
PROCEDURE get-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE v-len LIKE po-ordl.s-len NO-UNDO.
  DEFINE VARIABLE v-wid LIKE po-ordl.s-len NO-UNDO.
  DEFINE VARIABLE v-dep LIKE po-ordl.s-len NO-UNDO. 
  DEFINE VARIABLE v-bwt LIKE po-ordl.s-len NO-UNDO.
  DEFINE VARIABLE lv-out-qty LIKE w-po.rcpt-qty NO-UNDO.
  DEFINE VARIABLE lv-out-cost LIKE w-po.cost NO-UNDO.
  DEFINE VARIABLE lv-qty-uom LIKE w-po.cons-uom NO-UNDO.
  DEFINE VARIABLE lv-cost-uom LIKE w-po.pr-uom NO-UNDO.
  DEFINE VARIABLE cocode AS CHARACTER NO-UNDO.

  ASSIGN
    cocode = w-po.company
    w-po.cons-uom = w-po.cons-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.

  FIND item NO-LOCK WHERE item.company EQ cocode
                      AND item.i-no EQ w-po.i-no
                      USE-INDEX i-no NO-ERROR.
  IF NOT AVAIL item THEN LEAVE.

  IF item.cons-uom EQ '' THEN
     item.cons-uom = w-po.cons-uom.

  ASSIGN
    lv-qty-uom = item.cons-uom
    lv-cost-uom = item.cons-uom
    v-dep = item.s-dep.

  FIND FIRST po-ordl NO-LOCK WHERE po-ordl.company EQ w-po.company
                               AND po-ordl.po-no EQ INTEGER(w-po.po-no)
                               AND po-ordl.i-no EQ w-po.i-no
                               AND po-ordl.job-no EQ w-po.job-no
                               AND po-ordl.job-no2 EQ w-po.job-no2
                               AND po-ordl.item-type EQ YES
                               AND po-ordl.s-num EQ w-po.s-num NO-ERROR.
  IF AVAIL po-ordl THEN
  DO:
    ASSIGN
      v-len = po-ordl.s-len
      v-wid = po-ordl.s-wid
      v-bwt = 0.
    {rm/pol-dims.i}
  END.
  ELSE
  DO:
    FIND FIRST job NO-LOCK WHERE job.company EQ cocode
                             AND job.job-no EQ w-po.job-no
                             AND job.job-no2 EQ w-po.job-no2 NO-ERROR.
    IF AVAIL job THEN
    DO:
      FIND FIRST job-mat NO-LOCK WHERE job-mat.company EQ w-po.company
                                   AND job-mat.job EQ job.job
                                   AND job-mat.i-no EQ w-po.i-no
                                   AND job-mat.frm EQ w-po.s-num NO-ERROR.
      IF AVAIL job-mat THEN
      ASSIGN 
        v-len = job-mat.len
        v-wid = job-mat.wid
        v-bwt = job-mat.basis-w.
    END.
    IF v-len EQ 0 THEN v-len = IF AVAIL item THEN item.s-len ELSE 0.
    IF v-wid EQ 0 THEN v-wid = IF AVAIL item AND item.r-wid NE 0 THEN item.r-wid
                          ELSE IF AVAIL item THEN item.s-wid ELSE 0.
    IF v-bwt EQ 0 THEN v-bwt = IF AVAIL item THEN item.basis-w ELSE 0.
    ASSIGN
      lv-qty-uom = item.cons-uom
      lv-cost-uom = item.cons-uom.
  END.

  /* convert qty */
  RUN custom/convquom.p(cocode,w-po.cons-uom,lv-qty-uom,v-bwt,v-len,
                  INPUT v-wid,INPUT v-dep,INPUT w-po.rcpt-qty,OUTPUT lv-out-qty).

  /*
  /* convert cost */
  IF w-po.pr-uom EQ 'L' THEN
     lv-out-cost = DEC(w-po.cost) / lv-out-qty.
  ELSE
     RUN custom/convcuom.p(cocode,w-po.pr-uom,lv-cost-uom,
                        v-bwt,v-len,v-wid,v-dep,w-po.cost,OUTPUT lv-out-cost).
  */
  ASSIGN
    /*w-po.cost = lv-out-cost
    w-po.pr-uom = lv-cost-uom*/
    w-po.rcpt-qty = lv-out-qty
    w-po.cons-uom = lv-qty-uom.
  DISPLAY w-po.rcpt-qty w-po.cons-uom WITH BROWSE {&BROWSE-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetValues Dialog-Frame 
PROCEDURE resetValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    w-po.rcpt-qty = receiptQty
    w-po.partial = partialQty
    w-po.total-tags = totalTags
    w-po.cost = poCost
    w-po.pr-uom = costUOM
    w-po.cons-uom = consUOM.

  DISPLAY w-po.rcpt-qty w-po.partial
    w-po.cons-uom w-po.total-tags WITH BROWSE {&BROWSE-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveValues Dialog-Frame 
PROCEDURE saveValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    receiptQty = w-po.rcpt-qty
    partialQty = w-po.partial
    totalTags = w-po.total-tags
    poCost = w-po.cost
    costUOM = w-po.pr-uom
    consUOM = w-po.cons-uom .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tot-rec-qty-proc Dialog-Frame 
PROCEDURE tot-rec-qty-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-qty-2 AS DEC NO-UNDO.

   EMPTY TEMP-TABLE tt-po.

   FOR EACH w-po WHERE
       w-po.po-no NE 0:

       FIND FIRST tt-po WHERE
            tt-po.po-no EQ w-po.po-no AND
            tt-po.LINE  EQ w-po.LINE
            NO-ERROR.

       IF NOT AVAIL tt-po THEN
       DO:
          FIND FIRST po-ordl WHERE
               po-ordl.company EQ g_company AND
               po-ordl.po-no EQ w-po.po-no AND
               po-ordl.LINE EQ w-po.LINE
               NO-LOCK NO-ERROR.

          CREATE tt-po.
          ASSIGN tt-po.po-no = w-po.po-no
                 tt-po.LINE  = w-po.LINE
                 tt-po.cons-uom = w-po.cons-uom
                 tt-po.tot-rec-qty = IF AVAIL po-ordl THEN po-ordl.t-rec-qty
                                     ELSE 0
                 tt-po.overrun-qty = w-po.overrun-qty.

          IF AVAIL po-ordl AND w-po.cons-uom NE po-ordl.cons-uom THEN
          DO:
             FIND FIRST ITEM WHERE 
                  ITEM.company EQ g_company AND
                  ITEM.i-no EQ w-po.i-no
                  NO-LOCK NO-ERROR.

             RUN sys/ref/convquom.p(po-ordl.cons-uom,
                                    w-po.cons-uom, IF AVAIL ITEM THEN ITEM.basis-w ELSE 0,
                                    w-po.s-len, w-po.s-wid, IF AVAIL ITEM THEN item.s-dep ELSE 0,
                                    tt-po.tot-rec-qty, OUTPUT tt-po.tot-rec-qty).
          END.
       END.

       v-qty-2 = ((w-po.rcpt-qty * w-po.total-tags) + w-po.partial).

       IF w-po.cons-uom NE tt-po.cons-uom THEN
       DO:
          IF w-po.cons-uom NE tt-po.cons-uom THEN
          DO:
             FIND FIRST ITEM WHERE 
                  ITEM.company EQ g_company AND
                  ITEM.i-no EQ w-po.i-no
                  NO-LOCK NO-ERROR.

             RUN sys/ref/convquom.p(w-po.cons-uom,
                                    tt-po.cons-uom, IF AVAIL ITEM THEN ITEM.basis-w ELSE 0,
                                    w-po.s-len, w-po.s-wid, IF AVAIL ITEM THEN item.s-dep ELSE 0,
                                    v-qty-2, OUTPUT v-qty-2).
          END.
       END.

       tt-po.tot-rec-qty = tt-po.tot-rec-qty + v-qty-2.
   END.

   FOR EACH tt-po,
       EACH w-po WHERE
            w-po.po-no EQ tt-po.po-no AND
            w-po.LINE  EQ tt-po.LINE:

       w-po.tot-rec-qty = tt-po.tot-rec-qty.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-wpo Dialog-Frame 
PROCEDURE update-wpo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF btn_save:LABEL IN FRAME {&FRAME-NAME} EQ '&Save' THEN DO:
    ASSIGN
      w-po.partial = INT(w-po.partial:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      w-po.rcpt-qty
      w-po.total-tags
      w-po.rcpt-qty:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES
      w-po.partial:READ-ONLY = YES
      w-po.cons-uom:READ-ONLY = YES
      w-po.total-tags:READ-ONLY = YES
      btn_save:LABEL IN FRAME {&FRAME-NAME} = '&Update'
      btnCancel:HIDDEN = YES
      btnCancel:SENSITIVE = NO.

    RUN tot-rec-qty-proc.

    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  END.
  ELSE
  DO:
    ASSIGN
      btn_save:LABEL = '&Save'
      w-po.rcpt-qty:READ-ONLY IN BROWSE {&BROWSE-NAME} = NO
      w-po.partial:READ-ONLY = NO
      w-po.cons-uom:READ-ONLY = NO
      w-po.total-tags:READ-ONLY = NO
      btnCancel:HIDDEN = NO
      btnCancel:SENSITIVE = YES.
    APPLY 'ENTRY' TO w-po.rcpt-qty IN BROWSE {&BROWSE-NAME}.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom Dialog-Frame 
PROCEDURE valid-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-uom-list AS CHAR INIT ["EA,TON,MSF,MSH,LB,LF"] NO-UNDO.
  DEF VAR lv-uom AS CHAR NO-UNDO.
  DEF VAR lv-uom-help AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    lv-uom = w-po.cons-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.

    FIND FIRST item NO-LOCK
        WHERE item.company EQ w-po.company
          AND item.i-no EQ w-po.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        NO-ERROR.

    IF AVAIL item THEN RUN sys/ref/uom-rm.p (INPUT item.mat-type,OUTPUT lv-uom-list).

    lv-uom-help = "Must enter one of the following as the UOM: " + lv-uom-list.

    IF INDEX(lv-uom-list,lv-uom) LE 0 THEN DO:
      MESSAGE TRIM(lv-uom-help) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO w-po.cons-uom IN BROWSE {&BROWSE-NAME}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

