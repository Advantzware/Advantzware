&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
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
{custom/globdefs.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

def var v-process as log no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn-process btn-cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     btn-process AT ROW 5.86 COL 21
     btn-cancel AT ROW 5.86 COL 53
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 14.52.

DEFINE FRAME FRAME-B
     "Please review error file ~"c:~\tmp~\vwDataTransfer-error.txt~"." VIEW-AS TEXT
          SIZE 80.2 BY 1.14 AT ROW 3.52 COL 6.8 WIDGET-ID 4
          FGCOLOR 12 FONT 5
     "Item File Maintenance (W-F-1)" VIEW-AS TEXT
          SIZE 57.4 BY 1.05 AT ROW 2.24 COL 7.2 WIDGET-ID 2
          FGCOLOR 12 FONT 5
     "This utility will move data from the Customer Inventory (W-F-5) into" VIEW-AS TEXT
          SIZE 78.2 BY .95 AT ROW 1.43 COL 7.2
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 3.81
         BGCOLOR 11 .


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
         TITLE              = "Tranfer Data W-F-5 To New DB Tables W-F-1"
         HEIGHT             = 7.19
         WIDTH              = 90.2
         MAX-HEIGHT         = 45.05
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.05
         VIRTUAL-WIDTH      = 256
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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Tranfer Data W-F-5 To New DB Tables W-F-1 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Tranfer Data W-F-5 To New DB Tables W-F-1 */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
    apply "close" to this-procedure.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:

   DEF VAR v-cnt  AS INT NO-UNDO.
   DEF VAR v-file AS CHAR INIT "c:\tmp\vwDataTransfer-error.txt" NO-UNDO.
   DEF VAR v-ok   AS LOGICAL NO-UNDO.

   OUTPUT TO VALUE(v-file).

   RUN check-connect-rfg-db.

   SESSION:SET-WAIT-STATE("General"). 

   FOR EACH cust-itm NO-LOCK:
      v-ok = TRUE.
      FIND FIRST vend-code-cust-xref WHERE vend-code-cust-xref.company = cust-itm.company
                                       AND vend-code-cust-xref.cust-no = cust-itm.cust-no NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(vend-code-cust-xref) THEN DO:
         PUT UNFORMATTED "Invalid Suppliers A/R Code '" + cust-itm.cust-no + "' for FG Item '" + cust-itm.i-no + "'." SKIP.
         v-ok = FALSE.
      END.

      FIND vend-plant WHERE vend-plant.company  = cust-itm.company
                        AND vend-plant.plant-id = cust-itm.loc NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(vend-plant) THEN DO:
         PUT UNFORMATTED "Invalid Customers Plant ID '" + cust-itm.loc + "' for Suppliers A/R Code '"  + cust-itm.cust-no + "' FG Item '" + cust-itm.i-no + "'." SKIP.
         v-ok = FALSE.
      END.

      FIND FIRST itemfg WHERE itemfg.company = cust-itm.company
                          AND itemfg.cust-no = cust-itm.cust-no
                          AND itemfg.i-no    = cust-itm.i-no NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(itemfg) THEN
         FIND FIRST itemfg WHERE itemfg.company = cust-itm.company
                             AND itemfg.i-no    = cust-itm.i-no NO-LOCK NO-ERROR.

         IF NOT AVAILABLE(itemfg) THEN DO:
            PUT UNFORMATTED "Invalid FG Item '" + cust-itm.i-no + "' for Suppliers A/R Code '" + cust-itm.cust-no + "' Plant ID '" + cust-itm.loc + "'." SKIP.
            v-ok = FALSE.
         END.

      FIND FIRST vend-whse-item WHERE vend-whse-item.company = cust-itm.company
                                  AND vend-whse-item.cust-no = cust-itm.cust-no
                                  AND vend-whse-item.fg-item-no = cust-itm.i-no NO-LOCK NO-ERROR.
      IF AVAILABLE(vend-whse-item) THEN DO:
         PUT UNFORMATTED "FG Item already '" + cust-itm.i-no + "' exists for Suppliers A/R Code '" + cust-itm.cust-no + "' Plant ID '" + cust-itm.loc + "'." SKIP.
         v-ok = FALSE.
      END.

      IF v-ok = TRUE THEN DO:
         v-cnt = v-cnt + 1.
         CREATE vend-whse-item.
         ASSIGN 
            vend-whse-item.annual-usage-qty  = cust-itm.consum      
            vend-whse-item.company           = cust-itm.company 
            vend-whse-item.create-date       = TODAY
            vend-whse-item.create-time       = TIME
            vend-whse-item.create-userid     = USERID("nosweat")
            vend-whse-item.cust-no           = cust-itm.cust-no 
            vend-whse-item.cust-part-no      = itemfg.part-no  
            vend-whse-item.fg-item-no        = cust-itm.i-no
            vend-whse-item.plant-tot-oh-qty  = cust-itm.qty
            vend-whse-item.upd-date          = TODAY
            vend-whse-item.upd-time          = TIME
            vend-whse-item.upd-userid        = USERID("nosweat")
            vend-whse-item.vendor-code       = vend-plant.vendor-code
            vend-whse-item.vendor-dept-code  = vend-plant.vendor-dept-code
            vend-whse-item.vendor-plant-code = vend-plant.plant-id
            vend-whse-item.ship-id           = vend-plant.ship-id
            vend-whse-item.ship-no           = vend-plant.ship-no               
            vend-whse-item.rec_key           = STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + STRING(TIME).
      END.

      STATUS DEFAULT "Processing.... " + TRIM(STRING(v-cnt)).
   END.

   STATUS DEFAULT "".

   SESSION:SET-WAIT-STATE("").

   OUTPUT CLOSE.

   RUN disconnect-db.

   MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

   APPLY "close" TO THIS-PROCEDURE.  


    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

  WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-connect-rfg-db C-Win 
PROCEDURE check-connect-rfg-db :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* connect to rfq database - start */

IF NOT CONNECTED('rfq') AND SEARCH('addon\rfq.pf') NE ? THEN
   CONNECT -pf VALUE(SEARCH('addon\rfq.pf')) NO-ERROR.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disconnect-db C-Win 
PROCEDURE disconnect-db :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* disconnect to rfq database */
IF CONNECTED('rfq') THEN DISCONNECT rfq.

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
  ENABLE btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

