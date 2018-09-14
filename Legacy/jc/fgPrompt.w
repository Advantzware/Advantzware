&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: fgPrompt.w

  Description: prompt for fg item value

  Input Parameters: company, init fg item value

  Output Parameters: entered fg item value

  Author: Ron Stark

  Created: 9.6.2005
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipCustNo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipPartNo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipRecid  AS RECID NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopStockNo AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipCompany AS CHARACTER NO-UNDO INITIAL '001'.
DEFINE VARIABLE ipCustNo AS CHARACTER NO-UNDO INITIAL 'ATT1000'.
DEFINE VARIABLE ipPartNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE ipRecid  AS RECID NO-UNDO.
DEFINE VARIABLE iopStockNo AS CHARACTER NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */
def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
DEF NEW SHARED VAR s-est-no AS cha NO-UNDO.  /* for fgadd2.p */

DEF VAR cocode AS CHAR NO-UNDO.
DEF VAR locode AS CHAR NO-UNDO.
{custom/globdefs.i}

assign cocode = g_company
       locode = g_loc.

 {oe/oe-sysct1.i NEW}

 find first sys-ctrl where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGITEM#"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "FGITEM#"
   sys-ctrl.descrip = "Order Entry default FG Item Number from Estimate?"
   sys-ctrl.log-fld = yes.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
assign
 v-est-fg  = sys-ctrl.log-fld
 v-est-fg1 = sys-ctrl.char-fld.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES itemfg

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 itemfg.i-no itemfg.i-name ~
itemfg.part-no itemfg.est-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3 
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH itemfg ~
      WHERE itemfg.company EQ ipCompany ~
AND itemfg.cust-no EQ ipCustNo ~
AND (itemfg.i-no BEGINS stock-no ~
OR stock-no EQ '') NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY BROWSE-3 FOR EACH itemfg ~
      WHERE itemfg.company EQ ipCompany ~
AND itemfg.cust-no EQ ipCustNo ~
AND (itemfg.i-no BEGINS stock-no ~
OR stock-no EQ '') NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 itemfg
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 itemfg


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn_itemfg stock-no btnOK btn_cancel part-no ~
BROWSE-3 
&Scoped-Define DISPLAYED-OBJECTS stock-no part-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnOK AUTO-GO 
     LABEL "&OK" 
     SIZE 13 BY 1
     BGCOLOR 8 .

DEFINE BUTTON btn_cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 13 BY 1
     BGCOLOR 8 .

DEFINE BUTTON btn_itemfg 
     IMAGE-UP FILE "Graphics/32x32/plus.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Add" 
     SIZE 11 BY 1.8.

DEFINE VARIABLE part-no AS CHARACTER FORMAT "x(15)" 
     LABEL "Cust Part#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE stock-no LIKE eb.stock-no
     LABEL "Enter New &FG Item" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
      itemfg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 Dialog-Frame _STRUCTURED
  QUERY BROWSE-3 NO-LOCK DISPLAY
      itemfg.i-no COLUMN-LABEL "Existing!FG Items" FORMAT "x(15)":U
      itemfg.i-name FORMAT "x(30)":U
      itemfg.part-no FORMAT "x(12)":U
      itemfg.est-no FORMAT "x(8)":U WIDTH 11
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 105 BY 11.43
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btn_itemfg AT ROW 1.19 COL 3 WIDGET-ID 4
     stock-no AT ROW 1.24 COL 36 COLON-ALIGNED HELP
          ""
          LABEL "Enter New &FG Item"
     btnOK AT ROW 1.24 COL 65.6
     btn_cancel AT ROW 1.24 COL 79.4 WIDGET-ID 2
     part-no AT ROW 2.1 COL 36.2 COLON-ALIGNED
     BROWSE-3 AT ROW 3.1 COL 1.6
     SPACE(2.59) SKIP(0.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "FG Item Not Defined"
         DEFAULT-BUTTON btnOK CANCEL-BUTTON btn_cancel.


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
/* BROWSE-TAB BROWSE-3 part-no Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN stock-no IN FRAME Dialog-Frame
   LIKE = asi.eb. EXP-LABEL EXP-SIZE                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _TblList          = "asi.itemfg"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "itemfg.company EQ ipCompany
AND itemfg.cust-no EQ ipCustNo
AND (itemfg.i-no BEGINS stock-no
OR stock-no EQ '')"
     _FldNameList[1]   > asi.itemfg.i-no
"itemfg.i-no" "Existing!FG Items" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = asi.itemfg.i-name
     _FldNameList[3]   = asi.itemfg.part-no
     _FldNameList[4]   > asi.itemfg.est-no
"itemfg.est-no" ? "x(8)" "character" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* FG Item Not Defined */
DO:
   ASSIGN
       iopStockNo = "" .
  /*APPLY "CHOOSE" TO Btn_Cancel .*/
  APPLY "END-ERROR":U TO SELF.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
&Scoped-define SELF-NAME BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-3 IN FRAME Dialog-Frame
DO:
  APPLY 'VALUE-CHANGED':U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 Dialog-Frame
ON ENTRY OF BROWSE-3 IN FRAME Dialog-Frame
DO:
  APPLY 'VALUE-CHANGED':U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 Dialog-Frame
ON VALUE-CHANGED OF BROWSE-3 IN FRAME Dialog-Frame
DO:
  ASSIGN
    stock-no:SCREEN-VALUE = itemfg.i-no
    stock-no.
    part-no:HIDDEN = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
   DEF BUFFER bff-eb FOR eb .
   FIND FIRST eb WHERE RECID(eb) = ipRecid NO-LOCK NO-ERROR.

  IF AVAILABLE itemfg THEN DO:
      
      IF AVAIL eb THEN
          IF eb.est-no NE itemfg.est THEN do:
              
              IF itemfg.est-no NE '' THEN DO:
                  MESSAGE 'Item Exists for another Estimate.'
                      VIEW-AS ALERT-BOX TITLE 'Estimate: ' + LEFT-TRIM(itemfg.est-no).
                  RETURN NO-APPLY.
               END. /* if est-no */
               ELSE IF itemfg.part-no NE ipPartNo THEN DO:
                   MESSAGE 'Customer Part# does not match Estimates Customer Part#.'
                       VIEW-AS ALERT-BOX TITLE 'Cust Part#: ' + itemfg.part-no +
                       ' - Est Part#: ' + ipPartNo.
               RETURN NO-APPLY.
               END. /* if part-no */
       END. /* not equal est-no */
  END. /* if avail */
  iopStockNo = stock-no.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_cancel Dialog-Frame
ON CHOOSE OF btn_cancel IN FRAME Dialog-Frame /* Cancel */
DO:
     ASSIGN
       iopStockNo = "" .
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_itemfg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_itemfg Dialog-Frame
ON CHOOSE OF btn_itemfg IN FRAME Dialog-Frame /* Cancel */
DO:
  DEF BUFFER bf-eb-chk FOR eb .
  DEF VAR v-est-fgitem AS CHAR NO-UNDO .
  FIND FIRST eb WHERE RECID(eb) = ipRecid NO-LOCK NO-ERROR.

  IF AVAIL eb THEN
      ASSIGN
      part-no:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      part-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = eb.part-no .
  
  FOR EACH bf-eb-chk WHERE bf-eb-chk.company = eb.company
      AND bf-eb-chk.est-no = eb.est-no 
      AND bf-eb-chk.part-no = eb.part-no
      AND bf-eb-chk.stock-no <> "" NO-LOCK:
      ASSIGN
         v-est-fgitem = bf-eb-chk.stock-no .
  END.

  IF v-est-fgitem <> "" THEN DO:
      FIND CURRENT eb EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL eb THEN
          eb.stock = v-est-fgitem .
  END.
  
  RUN set-auto-add-item .
  IF AVAIL eb THEN ASSIGN
      stock-no:SCREEN-VALUE = eb.stock-no 
      stock-no = eb.stock-no  .

  {&OPEN-QUERY-{&BROWSE-NAME}}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stock-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stock-no Dialog-Frame
ON LEAVE OF stock-no IN FRAME Dialog-Frame /* Enter New FG Item */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stock-no Dialog-Frame
ON RETURN OF stock-no IN FRAME Dialog-Frame /* Enter New FG Item */
DO:
  APPLY 'CHOOSE':U TO btnOK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stock-no Dialog-Frame
ON VALUE-CHANGED OF stock-no IN FRAME Dialog-Frame /* Enter New FG Item */
DO:
  ASSIGN {&SELF-NAME}.
  {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the Dialog-Box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    FIND FIRST eb WHERE RECID(eb) = ipRecid NO-LOCK NO-ERROR.
  stock-no:SCREEN-VALUE = iopStockNo.
  part-no:HIDDEN = FALSE.
  IF AVAIL eb THEN
  part-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = eb.part-no .
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-create-item Dialog-Frame 
PROCEDURE auto-create-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER ipc-i-no AS CHAR NO-UNDO.
  DEF VAR lv-i-no              AS CHAR NO-UNDO.
  DEF VAR i                    AS INT  NO-UNDO.
  DEF BUFFER bf-eb FOR eb.

  IF NOT AVAIL xest AND AVAIL(eb) THEN DO:
      FIND FIRST xest WHERE xest.company = eb.company 
                        AND xest.est-no  = eb.est-no
                      NO-LOCK NO-ERROR.
  END.
  FIND xeb WHERE ROWID(xeb) = ROWID(eb) EXCLUSIVE-LOCK.
  
  IF NOT AVAIL xest THEN
      RETURN.
  IF NOT AVAIL xeb THEN
      RETURN.

  
  /* Make sure set header is created with a '00' */
  IF xest.est-type eq 2 or xest.est-type eq 6 then do:
      FIND FIRST bf-eb WHERE bf-eb.company = xest.company
                         AND bf-eb.est-no  = xest.est-no
                         AND bf-eb.form-no = 0
                       EXCLUSIVE-LOCK NO-ERROR.
      
      IF AVAIL bf-eb AND xeb.stock-no = "" THEN DO:
         RUN fg/GetFGItemID.p (ROWID(bf-eb), "", OUTPUT lv-i-no). 
                  
         xeb.stock-no = lv-i-no.
         
        FIND xeb WHERE ROWID(xeb) = ROWID(bf-eb) NO-LOCK.
        /*RUN fg/ce-addfg.p (bf-eb.stock-no).*/
        FIND itemfg WHERE itemfg.company = xest.company
                      AND itemfg.i-no = bf-eb.stock-no
                    NO-ERROR.
        IF AVAIL itemfg THEN
            itemfg.isaset = TRUE.
        RELEASE itemfg.
      END.
  END.
  /* From oe/d-oeitem.w */
  
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
  DISPLAY stock-no part-no 
      WITH FRAME Dialog-Frame.
  ENABLE btn_itemfg stock-no btnOK btn_cancel part-no BROWSE-3 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-auto-add-item Dialog-Frame 
PROCEDURE set-auto-add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR lv-i-no LIKE eb.stock-no NO-UNDO.
  DEF VAR lv-num-created AS INT NO-UNDO.
  DEF VAR lvr-eb AS ROWID NO-UNDO.
  DEF BUFFER bf-est FOR est.
  DEFINE BUFFER bf-setheader-eb FOR eb.
  
  DEF VAR l-est-type AS INT NO-UNDO.

 IF AVAIL eb THEN DO:
     FIND FIRST xest WHERE xest.company = eb.company
         AND xest.est-no = eb.est-no
         NO-LOCK NO-ERROR.
  END.

  IF NOT AVAIL xeb AND avail(eb) THEN
    FIND xeb WHERE rowid(xeb) = ROWID(eb) NO-LOCK NO-ERROR.

  IF NOT AVAIL xest THEN
      RETURN.
  
/*  IF xest.est-type eq 2 or xest.est-type eq 6 THEN DO:*/
/*    lv-num-created = lv-num-created + 1.              */
/*    RUN auto-create-item (INPUT lv-i-no).             */
/*  END.                                                */
/*                                                      */
/*  /* Process regular items */                         */
/*  ELSE                                                */
  DO :
      FIND bf-est WHERE bf-est.company EQ eb.company
          AND bf-est.est-no  EQ eb.est-no
          NO-LOCK NO-ERROR.
      IF AVAIL bf-est THEN
          l-est-type = bf-est.est-type.
      
      FIND xeb WHERE ROWID(xeb) = ROWID(eb) EXCLUSIVE-LOCK.
      FIND FIRST xest WHERE xest.company = xeb.company
          AND xest.est-no  = xeb.est-no
          NO-LOCK NO-ERROR.
      
       IF xeb.stock-no = "" THEN do:
            IF l-est-type EQ 2 OR l-est-type EQ 6 THEN 
                FIND FIRST bf-setheader-eb NO-LOCK 
                    WHERE bf-setheader-eb.company EQ xeb.company
                    AND bf-setheader-eb.est-no EQ xeb.est-no
                    AND bf-setheader-eb.form-no EQ 0
                    AND ROWID(bf-setheader-eb) NE ROWID(xeb)
                    NO-ERROR.
          RUN fg/GetFGItemID.p (ROWID(xeb), (IF AVAILABLE bf-setheader-eb THEN bf-setheader-eb.stock-no ELSE ""), OUTPUT lv-i-no). 
          ASSIGN xeb.stock-no = lv-i-no .
                /*RUN fg/ce-addfg.p (xeb.stock-no).*/
      END.
  END.

  
          
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

