&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File              : util/fixfghis.w

  Description       : Fix FG History Clear Vendor P.O.#

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE TEMP-TABLE ttToDel
    FIELD po-no     LIKE oe-ord.po-no
    FIELD cust-no   LIKE oe-ord.cust-no
    FIELD ord-row   AS ROWID
    FIELD numOfDups AS INTEGER.
DEFINE BUFFER bf-oe-ord FOR oe-ord.
DEFINE VARIABLE iDupCnt AS INTEGER NO-UNDO.
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-process AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_cust btSearch fiOrdDate tgAllowBlankPO ~
edDuplicates btnOk BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust fiOrdDate tgAllowBlankPO ~
edDuplicates scr-text 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnOk AUTO-GO 
     LABEL "Delete Duplicates" 
     SIZE 22 BY 1.14.

DEFINE BUTTON btSearch 
     LABEL "Search for Duplicates" 
     SIZE 29 BY 1.14.

DEFINE VARIABLE edDuplicates AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 75 BY 12.62 NO-UNDO.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(256)":U 
     LABEL "Customer #" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiOrdDate AS DATE FORMAT "99/99/99":U INITIAL ? 
     LABEL "Order Date" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE scr-text AS CHARACTER FORMAT "X(256)":U INITIAL "Delete Duplicate Order Web Orders" 
      VIEW-AS TEXT 
     SIZE 45 BY .62 NO-UNDO.

DEFINE VARIABLE tgAllowBlankPO AS LOGICAL INITIAL no 
     LABEL "Allow Blank PO Numbers?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     begin_cust AT ROW 2.43 COL 17 COLON-ALIGNED WIDGET-ID 6
     btSearch AT ROW 2.43 COL 37 WIDGET-ID 4
     fiOrdDate AT ROW 3.43 COL 17 COLON-ALIGNED WIDGET-ID 10
     tgAllowBlankPO AT ROW 4.62 COL 19 WIDGET-ID 8
     edDuplicates AT ROW 5.76 COL 3 NO-LABEL WIDGET-ID 2
     btnOk AT ROW 18.62 COL 14
     BtnCancel AT ROW 18.62 COL 40
     scr-text AT ROW 1.48 COL 3 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 78.4 BY 18.91.


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
         TITLE              = "Delete Duplicate Web Orders"
         HEIGHT             = 18.91
         WIDTH              = 78.4
         MAX-HEIGHT         = 18.91
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 18.91
         VIRTUAL-WIDTH      = 80
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       BtnCancel:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

ASSIGN 
       btnOk:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

/* SETTINGS FOR FILL-IN scr-text IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Delete Duplicate Web Orders */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Delete Duplicate Web Orders */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON CHOOSE OF BtnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* Delete Duplicates */
DO:
   DEF BUFFER b-oe-ordl FOR oe-ordl.

   DO WITH FRAME {&FRAME-NAME}:

     

      SESSION:SET-WAIT-STATE("general"). 

      iDupCnt = 0.
      FOR EACH ttToDel, EACH oe-ord EXCLUSIVE-LOCK WHERE ROWID(oe-ord) EQ ttToDel.ord-row:
           FIND FIRST oe-rell NO-LOCK 
               WHERE oe-rell.company EQ oe-ord.company
                 AND oe-rell.ord-no  EQ oe-ord.ord-no
               NO-ERROR. 
           IF AVAILABLE oe-rell THEN NEXT.
               
           FOR EACH oe-rel
               WHERE oe-rel.company EQ cocode
               AND oe-rel.ord-no  EQ oe-ord.ord-no:
               DELETE oe-rel.
           END. /* oe-rell */  
           FOR EACH oe-ordl
               WHERE oe-ordl.company EQ cocode
               AND oe-ordl.ord-no  EQ oe-ord.ord-no
               exclusive:
               DELETE oe-ordl.
           END.          
           DELETE oe-ord.
      END.      

      MESSAGE "Deletion Complete."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

      SESSION:SET-WAIT-STATE(""). 
      edDuplicates:SCREEN-VALUE = "".
      DISABLE btnOk.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSearch C-Win
ON CHOOSE OF btSearch IN FRAME DEFAULT-FRAME /* Search for Duplicates */
DO:
  DEFINE VAR iTotDups AS INT NO-UNDO.
  ASSIGN begin_cust tgAllowBlankPO fiOrdDate.
  edDuplicates:INSERT-STRING("Customer" + " " + "PO Number" 
                 + " " + "Count"
                 + CHR(10)). 
  FOR EACH oe-ord NO-LOCK 
    WHERE oe-ord.company EQ cocode
      AND oe-ord.stat EQ 'W' 
      AND oe-ord.ord-date EQ fiOrdDate
      AND oe-ord.opened EQ TRUE
      AND (oe-ord.po-no GT "" OR tgAllowBlankPO)
      AND oe-ord.cust-no EQ begin_cust
    BREAK BY oe-ord.cust-no
    BY oe-ord.po-no:
        
    IF FIRST-OF(oe-ord.po-no) THEN 
    DO:
        iDupCnt = 0.
        FOR EACH bf-oe-ord NO-LOCK
            WHERE bf-oe-ord.company EQ oe-ord.company 
              AND bf-oe-ord.stat EQ 'W'
              AND bf-oe-ord.ord-date EQ fiOrdDate
              AND bf-oe-ord.po-no EQ oe-ord.po-no
              AND bf-oe-ord.cust-no EQ oe-ord.cust-no
              AND bf-oe-ord.opened EQ TRUE
              AND rowid(bf-oe-ord) NE rowid(oe-ord):
            iDupCnt = iDupCnt + 1.
            CREATE ttToDel.
            ASSIGN 
                ttToDel.po-no   = bf-oe-ord.po-no
                ttToDel.cust-no = bf-oe-ord.cust-no
                ttToDel.ord-row = ROWID(bf-oe-ord)
                .
        END.
        IF iDupCnt GT 0 THEN DO:
            iTotDups = iTotDups + iDupCnt.
            edDuplicates:INSERT-STRING(oe-ord.cust-no + "    " + oe-ord.po-no 
                 + "   " + STRING(iDupCnt)
                 + CHR(10)).            
            
        END.
    END. /* first-of po */
  
  END. /* each ord */
  IF iTotDups EQ 0 THEN 
    MESSAGE "No duplicates found!" VIEW-AS ALERT-BOX.

END. /* Do */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

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
  DISPLAY begin_cust fiOrdDate tgAllowBlankPO edDuplicates scr-text 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE begin_cust btSearch fiOrdDate tgAllowBlankPO edDuplicates btnOk 
         BtnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

