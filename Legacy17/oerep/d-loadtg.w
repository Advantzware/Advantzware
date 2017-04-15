&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: oerep\d-loadtg.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/* Mod 01     Task   10111315     */

{oerep/r-loadtg.i}
{custom/globdefs.i}

DEF VAR autoCopy AS LOG NO-UNDO.
DEF VAR copyRowID AS ROWID NO-UNDO.
DEF VAR glQtyOK AS LOG NO-UNDO. /*global holder of tag qty ok*/

DEFINE VARIABLE partialQty LIKE w-ord.partial NO-UNDO.
DEFINE VARIABLE totalTags LIKE w-ord.total-tags NO-UNDO.
DEFINE VARIABLE totalUnit LIKE w-ord.total-unit NO-UNDO.
DEFINE VARIABLE pcsValue LIKE w-ord.pcs NO-UNDO.
DEFINE VARIABLE bundleValue LIKE w-ord.bundle NO-UNDO.
DEFINE VARIABLE glTotalTagsChanged AS LOGICAL     NO-UNDO.

DEF var v-loadtag  AS char NO-UNDO INIT "ASI".  /* sys ctrl option */
DEF var v-tags AS DEC NO-UNDO INIT 1.  /* sys ctrl option */
/*
DEF VAR v-ord-copied AS LOG NO-UNDO.
DEF VAR v-copied-ord-no AS INT NO-UNDO.
DEF VAR v-is-update AS LOG INIT YES NO-UNDO.
*/

/* gdm - 07170905*/
{sys\inc\BOLWeight.i}

IF NOT BOLWt-log THEN RUN calc-weight-all.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES w-ord

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 w-ord.ord-no w-ord.job-no w-ord.job-no2 NO-LABEL w-ord.cust-no w-ord.i-no w-ord.ord-qty w-ord.over-pct w-ord.pcs "Unit Count" w-ord.bundle Unit" */ "Units/!Pallet" w-ord.partial w-ord.total-unit w-ord.total-tags /* gdm 09210907 */ w-ord.unit-wt w-ord.pallt-wt w-ord.lot w-ord.i-name w-ord.cust-po-no   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 w-ord.over-pct ~
 w-ord.pcs ~
 w-ord.bundle ~
 w-ord.partial ~
 w-ord.total-unit ~
 w-ord.total-tags ~
 w-ord.unit-wt ~
  w-ord.pallt-wt ~
 w-ord.lot /*"Bdl/Case" AT 46  "Total#" AT 55  "Total Qty" AT 65 SKIP  "Order#"  "Cust # "  "Item #"  "Ord Qty" TO 44  "Count" AT 46  "Bdl/Case" AT 55  "Per Unit" AT 65  "Tags " TO 80 SKIP */   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 w-ord
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 w-ord
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH w-ord. RUN calc-total
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH w-ord. RUN calc-total.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 w-ord
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 w-ord


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


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CheckTotals Dialog-Frame 
FUNCTION CheckTotals RETURNS LOGICAL
    ( INPUT ipxTotalPerUnit AS INTEGER,
      INPUT ipxTotalTags AS INTEGER,
      INPUT ipxOrderQtyPlusOver AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
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
      w-ord SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 DISPLAY
      w-ord.ord-no  LABEL "Order#"
      w-ord.job-no  LABEL "  Job#"
      w-ord.job-no2 NO-LABEL FORMAT "99"
      w-ord.cust-no LABEL "Cust #"
      w-ord.i-no LABEL "Item #"
      w-ord.ord-qty COLUMN-LABEL "Ord Qty"
      w-ord.over-pct FORM ">>9.99" COLUMN-LABEL "Overrun%"
      w-ord.pcs FORM ">>>,>>9" COLUMN-LABEL /*"Bdl/Case!Count"*/ "Unit Count"
      w-ord.bundle FORM ">>>,>>9" COLUMN-LABEL /*"Bdl/Case!Per Unit" */ "Units/!Pallet"
      w-ord.partial COLUMN-LABEL "Partial"
      w-ord.total-unit FORM ">,>>>,>>9" COLUMN-LABEL "Total Qty!Per Unit"
      w-ord.total-tags COLUMN-LABEL "No. of!Tags" /* gdm 09210907 */
      w-ord.unit-wt COLUMN-LABEL "Unit!Wt"
      w-ord.pallt-wt COLUMN-LABEL "Pallet!Wt"
      w-ord.lot FORMAT "X(20)" COLUMN-LABEL "FG Lot#"
      w-ord.i-name LABEL "Item Name"
      w-ord.cust-po-no LABEL "Customer PO#"
      ENABLE
        w-ord.over-pct
        w-ord.pcs
        w-ord.bundle
        w-ord.partial
        w-ord.total-unit 
        w-ord.total-tags 
        w-ord.unit-wt   
        w-ord.pallt-wt
        w-ord.lot
/*"Bdl/Case" AT 46
  "Total#" AT 55
  "Total Qty" AT 65 SKIP
  "Order#"
  "Cust #  "
  "Item #"
  "Ord Qty" TO 44
  "Count" AT 46
  "Bdl/Case" AT 55
  "Per Unit" AT 65
  "Tags " TO 80 SKIP
 */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 235 BY 14.05
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
     SPACE(98.00) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 0
         TITLE "Loadtag Creation Detail"
         DEFAULT-BUTTON Btn_OK.


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
OPEN QUERY {&SELF-NAME} FOR EACH w-ord.
RUN calc-total.
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
  IF NOT AVAIL w-ord THEN RETURN NO-APPLY.
  IF autoCopy AND copyRowID EQ ROWID(w-ord) THEN DELETE w-ord.
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
  IF NOT AVAIL w-ord THEN RETURN NO-APPLY.
  RUN copy-word.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  REPOSITION {&BROWSE-NAME} TO ROWID copyRowID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_delete Dialog-Frame
ON CHOOSE OF btn_delete IN FRAME Dialog-Frame /* Delete */
DO:
  IF NOT AVAIL w-ord THEN RETURN NO-APPLY.
  MESSAGE "Are you sure you want to delete? " VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-ans AS LOG.             
  IF v-ans THEN RUN del-word.
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
  IF NOT AVAIL w-ord THEN RETURN NO-APPLY.
   IF btn_save:LABEL IN FRAME {&FRAME-NAME} = "&Save" AND
      int(w-ord.bundle:SCREEN-VALUE IN BROWSE {&browse-name}) = 0 and
      LOOKUP(v-loadtag,"SSLabel,SSBarone") > 0 THEN
   DO:
       MESSAGE "Units/Pallet can not be 0. " VIEW-AS ALERT-BOX.
       APPLY "entry" TO w-ord.bundle.
       RETURN NO-APPLY.
   END.
   RUN update-word.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
/* ON 'VALUE-CHANGED' OF w-ord.over-pct IN BROWSE {&BROWSE-NAME} DO:     */
/*   w-ord.ord-qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} =               */
/*       STRING(w-ord.qty-before * (1 + (DEC(SELF:SCREEN-VALUE) / 100)), */
/*              w-ord.ord-qty:FORMAT IN BROWSE {&BROWSE-NAME}).          */
/*                                                                       */
/*   RUN calc-total.                                                     */
/*                                                                       */
/*   RETURN NO-APPLY.                                                    */
/* END.                                                                  */

ON 'RETURN' OF w-ord.over-pct IN BROWSE {&BROWSE-NAME} DO:
  APPLY 'LEAVE' TO w-ord.over-pct IN BROWSE {&BROWSE-NAME}.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'LEAVE' OF w-ord.over-pct IN BROWSE {&BROWSE-NAME} DO:
  w-ord.ord-qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} =
      STRING(w-ord.qty-before * (1 + (DEC(SELF:SCREEN-VALUE) / 100)),
             w-ord.ord-qty:FORMAT IN BROWSE {&BROWSE-NAME}).
  IF SELF:MODIFIED THEN RUN calc-total.
END.

ON 'RETURN' OF w-ord.pcs IN BROWSE {&BROWSE-NAME} DO:
  APPLY 'LEAVE' TO w-ord.pcs IN BROWSE {&BROWSE-NAME}.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'LEAVE' OF w-ord.pcs IN BROWSE {&BROWSE-NAME} DO:
  IF SELF:MODIFIED THEN RUN calc-total.
END.

ON 'RETURN' OF w-ord.bundle IN BROWSE {&BROWSE-NAME} DO:
  APPLY 'LEAVE' TO w-ord.bundle IN BROWSE {&BROWSE-NAME}.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'LEAVE' OF w-ord.bundle IN BROWSE {&BROWSE-NAME} DO:
  IF LASTKEY <> -1 AND int(w-ord.bundle:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) = 0
     AND LOOKUP(v-loadtag,'SSLabel,SSBarone') > 0 
  THEN DO:
     MESSAGE 'Units/Pallet must be entered.' VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.
  IF SELF:MODIFIED THEN RUN calc-total.
END.

ON 'RETURN' OF w-ord.total-unit IN BROWSE {&BROWSE-NAME} DO:
  APPLY 'LEAVE' TO w-ord.total-unit IN BROWSE {&BROWSE-NAME}.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

ON 'LEAVE' OF w-ord.total-unit IN BROWSE {&BROWSE-NAME} DO:
  IF SELF:MODIFIED THEN RUN calc-total.
END.

ON 'RETURN' OF w-ord.partial IN BROWSE {&BROWSE-NAME} DO:
  APPLY 'LEAVE' TO w-ord.partial IN BROWSE {&BROWSE-NAME}.
  IF autoCopy THEN RUN autoCopy.
  ELSE APPLY 'CHOOSE' TO btn_save IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.
ON 'LEAVE' OF w-ord.total-tags IN BROWSE {&BROWSE-NAME} DO:
  IF SELF:MODIFIED THEN DO:
      glTotalTagsChanged = YES.
/*         RUN calc-total. */
        ASSIGN w-ord.ord-qty = dec(w-ord.ord-qty:SCREEN-VALUE IN BROWSE {&browse-name})
           w-ord.total-tags = int(w-ord.total-tags:SCREEN-VALUE IN BROWSE {&browse-name}).
        IF NOT glQtyOK  AND NOT CheckTotals(w-ord.total-unit, 
                                          w-ord.total-tags, 
                                          w-ord.ord-qty) THEN DO:
            MESSAGE "Total Tag Qty: Number of Tags (" w-ord.total-tags ") x Count on Each Tag (" w-ord.total-unit ")" SKIP 
              "exceeds the Order Overrun Allowed (" w-ord.ord-qty ")." SKIP
              "Continue?"
              VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE glQtyOK.      
            IF NOT glQtyOK THEN RETURN NO-APPLY.
        END. /*NOT glQtyOK and NOT checkTotals*/
    END. /*self:Modified*/
END.
ON 'LEAVE' OF w-ord.partial IN BROWSE {&BROWSE-NAME} DO:
  IF SELF:MODIFIED THEN RUN calc-total.
END.

/* gdm - */
ON 'ENTRY' OF w-ord.unit-wt   IN BROWSE {&BROWSE-NAME} DO:

   IF NOT BOLWt-log THEN DO:
     APPLY "entry"  TO w-ord.lot IN BROWSE {&browse-NAME}.
     RETURN NO-APPLY.
   END.
   
END.

ON 'LEAVE' OF w-ord.unit-wt   IN BROWSE {&BROWSE-NAME} DO:

  IF SELF:MODIFIED THEN RUN calc-weight.

END.

ON 'ENTRY' OF w-ord.pallt-wt   IN BROWSE {&BROWSE-NAME} DO:

   IF NOT BOLWt-log THEN DO:
     APPLY "entry"  TO w-ord.lot IN BROWSE {&browse-NAME}.
     RETURN NO-APPLY.
   END.
END.

/* gdm - end */

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
         AND sys-ctrl.name EQ 'LOADTAG' NO-ERROR.
  IF AVAIL sys-ctrl THEN
  ASSIGN
    v-loadtag = sys-ctrl.char-fld
    v-tags = sys-ctrl.dec-fld.    

  RUN enable_UI.
  IF AVAIL w-ord AND w-ord.i-no NE '' THEN
        RUN displayUNNotes(INPUT g_company,
                           INPUT w-ord.i-no).

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
    APPLY 'ENTRY' TO w-ord.pcs IN BROWSE {&BROWSE-NAME}.
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
   IF AVAIL w-ord THEN DO:
      IF int(w-ord.bundle:SCREEN-VALUE IN BROWSE {&browse-name}) = 0 AND
         int(w-ord.pcs:SCREEN-VALUE IN BROWSE {&browse-name}) <> 0
      THEN w-ord.bundle:SCREEN-VALUE IN BROWSE {&browse-name}
           = STRING(INT(w-ord.ord-qty:SCREEN-VALUE IN BROWSE {&browse-name}) 
                    / int(w-ord.pcs:SCREEN-VALUE IN BROWSE {&browse-name})).
    
      ASSIGN
            w-ord.pcs        = int(w-ord.pcs:SCREEN-VALUE IN BROWSE {&browse-name})
            w-ord.bundle     = int(w-ord.bundle:SCREEN-VALUE)
            w-ord.partial =  INT(w-ord.partial:SCREEN-VALUE)
            w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial.
      /*
      IF v-loadtag = "SSLABEL" OR v-loadtag = "SSBARONE" THEN DO:
          
         IF w-ord.ord-qty MOD w-ord.total-unit <> 0 AND
            NOT v-ord-copied AND w-ord.bundle <> 0
            AND (v-copied-ord-no <> w-ord.ord-no OR v-copied-ord-no = 0)
         THEN DO:
            RUN copy-word.            
         END. 
         w-ord.total-tags = TRUNCATE(w-ord.ord-qty / w-ord.total-unit,0).
      END.
      ELSE */

      IF v-tags NE ? AND NOT glTotalTagsChanged THEN
        w-ord.total-tags = IF w-ord.total-unit <> 0 AND v-tags <> 0 THEN ((INT(w-ord.ord-qty:SCREEN-VALUE IN BROWSE {&browse-name}) / w-ord.total-unit) + .49) 
                           ELSE 1.

      IF int(w-ord.partial:SCREEN-VALUE IN BROWSE {&browse-name}) < 0 THEN w-ord.partial = 0.
      IF int(w-ord.partial:SCREEN-VALUE IN BROWSE {&browse-name}) > 0 THEN w-ord.total-tags = 1.
           

        DISPLAY w-ord.total-unit w-ord.total-tags w-ord.partial
             WITH BROWSE {&browse-name}.

   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-weight Dialog-Frame 
PROCEDURE calc-weight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR v-weight-100 LIKE itemfg.weight-100 NO-UNDO.

IF BOLWt-log  AND
   (w-ord.unit-wt:SCREEN-VALUE IN BROWSE {&browse-name} NE "" OR w-ord.unit-wt:SCREEN-VALUE NE "0")
  THEN DO:

  ASSIGN w-ord.unit-wt.

  ASSIGN w-ord.pallt-wt = w-ord.bundle * w-ord.unit-wt
         w-ord.pallt-wt:SCREEN-VALUE = STRING(w-ord.pallt-wt).


END.
ELSE DO:

 FIND FIRST itemfg NO-LOCK
    WHERE itemfg.company EQ g_company
      AND itemfg.i-no EQ w-ord.i-no NO-ERROR.
  IF AVAIL itemfg THEN ASSIGN v-weight-100 = itemfg.weight-100.

  IF v-weight-100 > 0 
    THEN ASSIGN w-ord.unit-wt  = v-weight-100
                w-ord.pallt-wt = w-ord.bundle * v-weight-100.
    ELSE ASSIGN w-ord.unit-wt  = 0
                w-ord.pallt-wt = 0.

END.
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-weight-all Dialog-Frame 
PROCEDURE calc-weight-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-weight-100 LIKE itemfg.weight-100 NO-UNDO.

FOR EACH w-ord:

  FIND FIRST itemfg NO-LOCK
    WHERE itemfg.company EQ g_company
      AND itemfg.i-no EQ w-ord.i-no NO-ERROR.
  IF AVAIL itemfg THEN ASSIGN v-weight-100 = itemfg.weight-100.

  IF v-weight-100 > 0 
    THEN ASSIGN w-ord.unit-wt  = v-weight-100
                w-ord.pallt-wt = w-ord.bundle * v-weight-100.
    ELSE ASSIGN w-ord.unit-wt  = 0
                w-ord.pallt-wt = 0.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-word Dialog-Frame 
PROCEDURE copy-word :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-word FOR w-ord.

  CREATE bf-word.
  BUFFER-COPY w-ord TO bf-word.
  ASSIGN bf-word.total-tags = 1
        /* bf-word.bundle = 0
         bf-word.partial = 0
         bf-word.total-unit = 0*/     /* Mod 1  Task 10111315 */
         copyRowID = ROWID(bf-word).
/* Mod 01 */  
  /*IF lookup(v-loadtag,"SSLABEL,CentBox,SSBARONE") > 0 THEN DO:
     FIND itemfg WHERE itemfg.company = g_company AND
                       itemfg.i-no = w-ord.i-no NO-LOCK NO-ERROR.
     IF AVAIL itemfg THEN FIND style WHERE style.company = g_company
                                       AND style.style = itemfg.style NO-LOCK NO-ERROR.
     IF AVAIL style AND style.industry = "2" THEN bf-word.pcs = 0.
  END. */    /* Mod 1  Task 10111315 */ 
  /*
  v-ord-copied = YES.
  v-copied-ord-no = w-ord.ord-no.
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE del-word Dialog-Frame 
PROCEDURE del-word :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DELETE w-ord.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayUNNotes Dialog-Frame 
PROCEDURE displayUNNotes :
/*------------------------------------------------------------------------------
  Purpose:    Displays UN Notes when editing loadtags        
  Parameters:  Company & FG Item Number
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER ipcINo AS CHARACTER NO-UNDO.

DEFINE VARIABLE cMsg AS CHARACTER   NO-UNDO.
DEFINE BUFFER bf-itemfg FOR itemfg.
DEFINE BUFFER bf-notes FOR notes.

ASSIGN 
    cMsg = ""
    .
FIND FIRST bf-itemfg 
    WHERE bf-itemfg.company EQ ipcCompany
      AND bf-itemfg.i-no EQ ipcINo
    NO-LOCK NO-ERROR.
IF AVAIL bf-itemfg THEN DO:
    FOR EACH bf-notes 
        WHERE bf-notes.rec_key EQ bf-itemfg.rec_key
          AND bf-notes.note_code EQ "UN"
        NO-LOCK
        BY bf-notes.note_date
        BY bf-notes.note_time:
        cMsg = cMsg + bf-notes.note_text + CHR(13).
    END.
    IF cMsg NE "" THEN DO:
        MESSAGE cMSG
            VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Unitization Notes".
    END.
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetValues Dialog-Frame 
PROCEDURE resetValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    w-ord.partial = partialQty
    w-ord.total-tags = totalTags
    w-ord.total-unit = totalUnit
    w-ord.pcs = pcsValue
    w-ord.bundle = bundleValue.
  DISPLAY w-ord.partial w-ord.total-unit
    w-ord.pcs w-ord.bundle w-ord.total-tags WITH BROWSE {&BROWSE-NAME}.

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
  IF AVAIL w-ord THEN
  ASSIGN
    partialQty = w-ord.partial
    totalTags = w-ord.total-tags
    totalUnit = w-ord.total-unit
    pcsValue = w-ord.pcs
    bundleValue = w-ord.bundle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-word Dialog-Frame 
PROCEDURE update-word :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF btn_save:LABEL IN FRAME {&FRAME-NAME} = "&Save" THEN DO:
/*       IF lookup(v-loadtag,"SSLABEL,SSBARONE,CentBox") > 0 AND v-tags EQ 1 THEN DO: */
         /* 10/05/05  Joe told Yoosun to comment this out task#10050509
         IF  w-ord.total-unit <> 0 AND
             w-ord.bundle <> 0 AND
             w-ord.ord-qty MOD w-ord.total-unit <> 0 AND
            NOT v-ord-copied AND w-ord.bundle <> 0
            AND (v-copied-ord-no <> w-ord.ord-no OR v-copied-ord-no = 0)
         THEN DO:
           RUN copy-word.
         END. */
/*          w-ord.total-tags = IF w-ord.total-unit <> 0 AND v-tags <> 0 THEN TRUNCATE(w-ord.ord-qty / w-ord.total-unit,0) */
/*                             ELSE 1.                                                                                    */
/*       END.                                                                                                             */
      DISPLAY w-ord.total-tags WITH BROWSE {&BROWSE-NAME}.
      ASSIGN
        w-ord.pcs:READ-ONLY IN BROWSE {&BROWSE-NAME} = yes
        w-ord.bundle:READ-ONLY = yes
        w-ord.partial:READ-ONLY = yes
        w-ord.total-unit:READ-ONLY = yes 
        w-ord.total-tags:READ-ONLY = yes
        btnCancel:HIDDEN = YES
        btnCancel:SENSITIVE = NO
        /*v-is-update = NO*/
        btn_save:LABEL IN FRAME {&FRAME-NAME} = "&Update".
      {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
   END.
   ELSE DO:
      ASSIGN 
        btn_save:LABEL = "&Save"
        w-ord.pcs:READ-ONLY IN BROWSE {&BROWSE-NAME} = NO
        w-ord.bundle:READ-ONLY = NO
        w-ord.partial:READ-ONLY = NO
        w-ord.total-unit:READ-ONLY = NO 
        w-ord.total-tags:READ-ONLY = NO
        btnCancel:HIDDEN = NO
        btnCancel:SENSITIVE = YES.
      IF AVAIL w-ord AND w-ord.i-no NE '' THEN 
          RUN displayUNNotes(INPUT g_company,
                   INPUT w-ord.i-no).
      APPLY "entry" TO w-ord.pcs IN BROWSE {&BROWSE-NAME}.
   END.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CheckTotals Dialog-Frame 
FUNCTION CheckTotals RETURNS LOGICAL
    ( INPUT ipxTotalPerUnit AS INTEGER,
      INPUT ipxTotalTags AS INTEGER,
      INPUT ipxOrderQtyPlusOver AS DECIMAL) :
  /*------------------------------------------------------------------------------
    Purpose:  Checks to make sure that total qty of tags printed doesn't exceed 
            allowable overrun
      Notes:  
  ------------------------------------------------------------------------------*/
    DEF VAR lOK AS LOGICAL NO-UNDO.

    lOK = NO.
    IF (ipxTotalPerUnit * ipxTotalTags) LE ipxOrderQtyPlusOver THEN
      lOK = YES.

      RETURN lOK.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

