&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: windows/notes.w

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
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

&IF DEFINED(UIB_is_Running) = 0 &THEN
DEFINE INPUT PARAMETER ip-rec_key AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ip-header AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ip-rec_key AS CHARACTER NO-UNDO.
DEFINE VARIABLE ip-header AS CHARACTER NO-UNDO.
FIND FIRST prgrms NO-LOCK.
ASSIGN
  ip-rec_key = prgrms.rec_key
  ip-header = {methods/headers/prgrms.i}.
&ENDIF

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES notes
&Scoped-define FIRST-EXTERNAL-TABLE notes


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR notes.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES itemfg

/* Definitions for FRAME message-frame                                  */
&Scoped-define QUERY-STRING-message-frame FOR EACH itemfg SHARE-LOCK
&Scoped-define OPEN-QUERY-message-frame OPEN QUERY message-frame FOR EACH itemfg SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-message-frame itemfg
&Scoped-define FIRST-TABLE-IN-QUERY-message-frame itemfg


/* Standard List Definitions                                            */
&Scoped-Define DISPLAYED-OBJECTS headervalue 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_specnot2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_specnot2-2 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE headervalue AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 148 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-no AS CHARACTER FORMAT "x(15)" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.19
     BGCOLOR 8 FONT 6.

DEFINE VARIABLE lbl_i-no AS CHARACTER FORMAT "X(256)":U INITIAL "     FG Item#:" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.19
     BGCOLOR 8 FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY message-frame FOR 
      itemfg SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     headervalue AT ROW 23.62 COL 2 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150 BY 24
         BGCOLOR 15 .

DEFINE FRAME message-frame
     lbl_i-no AT ROW 1.24 COL 1 NO-LABEL
     fi_i-no AT ROW 1.24 COL 18 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 46 ROW 2.91
         SIZE 104 BY 1.43
         BGCOLOR 15 .

DEFINE FRAME OPTIONS-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1
         SIZE 149 BY 1.91
         BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: NOSWEAT.notes
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 2
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Spec Notes"
         HEIGHT             = 23.91
         WIDTH              = 150
         MAX-HEIGHT         = 26.62
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 26.62
         VIRTUAL-WIDTH      = 160
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Win:LOAD-ICON("adeicon\edit%":U) THEN
    MESSAGE "Unable to load icon: adeicon\edit%"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{methods/template/windows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME message-frame:FRAME = FRAME F-Main:HANDLE
       FRAME OPTIONS-FRAME:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME OPTIONS-FRAME:MOVE-BEFORE-TAB-ITEM (FRAME message-frame:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FILL-IN headervalue IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME message-frame
                                                                        */
/* SETTINGS FOR FILL-IN fi_i-no IN FRAME message-frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lbl_i-no IN FRAME message-frame
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME OPTIONS-FRAME
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME message-frame
/* Query rebuild information for FRAME message-frame
     _TblList          = "ASI.itemfg"
     _Query            is NOT OPENED
*/  /* FRAME message-frame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME OPTIONS-FRAME
/* Query rebuild information for FRAME OPTIONS-FRAME
     _Query            is NOT OPENED
*/  /* FRAME OPTIONS-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Spec Notes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Spec Notes */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
run pre-spec NO-ERROR.
IF ERROR-STATUS:ERROR THEN do:
   RETURN ERROR.    
END.


/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 141.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/smartmsg.w':U ,
             INPUT  FRAME message-frame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_smartmsg ).
       RUN set-position IN h_smartmsg ( 1.00 , 73.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Browse Notes|View Notes' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.14 , 2.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 20.24 , 148.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_smartmsg ,
             lbl_i-no:HANDLE IN FRAME message-frame , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             FRAME message-frame:HANDLE , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/specnot2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_specnot2-2 ).
       RUN set-position IN h_specnot2-2 ( 5.05 , 6.00 ) NO-ERROR.
       RUN set-size IN h_specnot2-2 ( 18.10 , 138.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartNavBrowser h_specnot2-2. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_specnot2-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_specnot2-2 ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/specnot2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_specnot2 ).
       RUN set-position IN h_specnot2 ( 5.05 , 9.00 ) NO-ERROR.
       /* Size in UIB:  ( 14.29 , 127.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 20.76 , 4.00 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 2.14 , 38.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav ).
       RUN set-position IN h_p-updsav ( 20.76 , 92.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav ( 2.14 , 56.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_specnot2. */
       RUN add-link IN adm-broker-hdl ( h_p-updsav , 'TableIO':U , h_specnot2 ).
       RUN add-link IN adm-broker-hdl ( h_specnot2-2 , 'Record':U , h_specnot2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_specnot2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico ,
             h_specnot2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updsav ,
             h_p-navico , 'AFTER':U ).
    END. /* Page 2 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "notes"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "notes"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY headervalue 
      WITH FRAME F-Main IN WINDOW W-Win.
  VIEW FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW FRAME OPTIONS-FRAME IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-OPTIONS-FRAME}
  DISPLAY lbl_i-no fi_i-no 
      WITH FRAME message-frame IN WINDOW W-Win.
  ENABLE lbl_i-no 
      WITH FRAME message-frame IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-message-frame}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-ip-rec_key W-Win 
PROCEDURE Get-ip-rec_key :
/*------------------------------------------------------------------------------
  Purpose:     Provide ip-rec_key to browser
  Parameters:  OUTPUT ip-rec_key
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-rec_key AS CHARACTER NO-UNDO.

  op-rec_key = ip-rec_key.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-spec W-Win 
PROCEDURE pre-spec :
/*------------------------------------------------------------------------------
  Purpose:  /* can get called from estimate and  order   */
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var li-num-item as int no-undo.
def var ls-item-list as cha no-undo.
def var ls-item-selected as cha no-undo.

FIND FIRST po-ordl WHERE po-ordl.rec_key = ip-rec_key NO-LOCK NO-ERROR.
IF AVAIL po-ordl THEN DO:
   IF po-ordl.item-type THEN /* RM*/
        FIND FIRST ITEM WHERE ITEM.company = po-ordl.company 
                          AND ITEM.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
   ELSE FIND FIRST ITEMfg WHERE ITEMfg.company = po-ordl.company 
                          AND ITEMfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
END.
ELSE find FIRST item where item.rec_key eq ip-rec_key no-lock no-error.

IF AVAIL item THEN
  ASSIGN
   lbl_i-no = "    RM Item#:"
   fi_i-no  = item.i-no
   ip-rec_key = item.rec_key .

ELSE DO:

  IF NOT AVAIL itemfg THEN DO:
    find first est where est.rec_key = ip-rec_key no-lock no-error.

    if avail est then do:
       assign li-num-item = 0
            ls-item-list = "" .
       for each eb where eb.company = est.company and
                       eb.est-no = est.est-no no-lock:
         IF eb.stock-no <> "" THEN
            assign li-num-item = li-num-item + 1
                   ls-item-list = ls-item-list + eb.stock-no + ",".               
       end.     
       IF ls-item-list = "" THEN DO:
          MESSAGE "No FG Item entered. " VIEW-AS ALERT-BOX ERROR.
          RETURN ERROR.
       END.

       if li-num-item > 1 then do:
          run windows/d-specit.w (li-num-item, ls-item-list, output ls-item-selected).
          ls-item-list = ls-item-selected. 
       end.    
       find itemfg where itemfg.company = est.company 
                      and itemfg.i-no = entry(1,ls-item-list)
                      no-lock no-error.
       ip-rec_key = if avail itemfg then itemfg.rec_key else ip-rec_key.
    end.  /* avail est */
    else  do:
      find first oe-ord where oe-ord.rec_key = ip-rec_key no-lock no-error.

      if avail oe-ord then do:  /* from order */
         assign li-num-item = 0
                ls-item-list = "" .
         for each oe-ordl of oe-ord no-lock:
             IF oe-ordl.i-no <> "" then
                   assign li-num-item = li-num-item + 1
                          ls-item-list = ls-item-list + oe-ordl.i-no + ",".               
         end.     
         IF ls-item-list = "" THEN DO:
            MESSAGE "No FG Item entered. " VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
         END.
         if li-num-item > 1 then do:
            run windows/d-specit.w (li-num-item, ls-item-list, output ls-item-selected).
            ls-item-list = ls-item-selected. 
         end.
         find itemfg where itemfg.company = oe-ord.company 
                       and itemfg.i-no = entry(1,ls-item-list)
                       no-lock no-error.
         ip-rec_key = if avail itemfg then itemfg.rec_key else ip-rec_key.      
         
      end.
      ELSE IF CAN-FIND(FIRST quotehd WHERE quotehd.rec_key = ip-rec_key)  THEN DO:
           find first quotehd where quotehd.rec_key = ip-rec_key no-lock no-error.
           if avail quotehd then do:  /* from quote */
              assign li-num-item = 0
                     ls-item-list = "" .
              for each quoteitm of quotehd no-lock:
                     IF quoteitm.i-no <> "" then
                           assign li-num-item = li-num-item + 1
                                  ls-item-list = ls-item-list + quoteitm.i-no + ",".               
              end.     
              IF ls-item-list = "" THEN DO:
                    MESSAGE "No FG Item entered. " VIEW-AS ALERT-BOX ERROR.
                    RETURN ERROR.
              END.
              if li-num-item > 1 then do:
                    run windows/d-specit.w (li-num-item, ls-item-list, output ls-item-selected).
                    ls-item-list = ls-item-selected. 
              end.
              find itemfg where itemfg.company = quotehd.company 
                            and itemfg.i-no = entry(1,ls-item-list) no-lock no-error.
              ip-rec_key = if avail itemfg then itemfg.rec_key else ip-rec_key.      
           END.
      END.
      ELSE IF CAN-FIND(FIRST oe-relh WHERE oe-relh.rec_key = ip-rec_key)  THEN DO:
           find first oe-relh where oe-relh.rec_key = ip-rec_key no-lock no-error.
           if avail oe-relh then do:  /* from quote */
              assign li-num-item = 0
                     ls-item-list = "" .
              for each oe-rell of oe-relh no-lock:
                     IF oe-rell.i-no <> "" then
                           assign li-num-item = li-num-item + 1
                                  ls-item-list = ls-item-list + oe-rell.i-no + ",".
              end.     
              IF ls-item-list = "" THEN DO:
                    MESSAGE "No FG Item entered. " VIEW-AS ALERT-BOX ERROR.
                    RETURN ERROR.
              END.
              if li-num-item > 1 then do:
                    run windows/d-specit.w (li-num-item, ls-item-list, output ls-item-selected).
                    ls-item-list = ls-item-selected. 
              end.
              find itemfg where itemfg.company = oe-relh.company 
                            and itemfg.i-no = entry(1,ls-item-list) no-lock no-error.
              ip-rec_key = if avail itemfg then itemfg.rec_key else ip-rec_key.      
           END.
      END.
      ELSE IF CAN-FIND(FIRST oe-rell WHERE oe-rell.rec_key = ip-rec_key)  THEN DO:
           find first oe-rell where oe-rell.rec_key = ip-rec_key no-lock no-error.
           IF AVAIL oe-rell THEN
           DO:
              find itemfg where itemfg.company = oe-rell.company 
                            and itemfg.i-no = oe-rell.i-no no-lock no-error.
              ip-rec_key = if avail itemfg then itemfg.rec_key else ip-rec_key.
           END.
      END.
      else do:  /* line items of est or order */
          find first oe-ordl where oe-ordl.rec_key = ip-rec_key no-lock no-error.
          if avail oe-ordl then ls-item-list = oe-ordl.i-no.
          else do:
               find first eb where eb.rec_key = ip-rec_key no-lock no-error.
               if avail eb then ls-item-list = eb.stock-no.
               ELSE DO:
                   FIND FIRST itemfg WHERE itemfg.rec_key = ip-rec_key NO-LOCK NO-ERROR.
                   IF AVAIL itemfg THEN ASSIGN ls-item-list = itemfg.i-no.
               END.
          end.
          IF ls-item-list = "" THEN DO:
             MESSAGE "No FG Item entered. " VIEW-AS ALERT-BOX ERROR.
             RETURN ERROR.
          END.
          if avail oe-ordl or avail eb then do:
             find itemfg where itemfg.company = g_company 
                            and itemfg.i-no = entry(1,ls-item-list)
                            no-lock no-error.
             ip-rec_key = if avail itemfg then itemfg.rec_key else ip-rec_key.      
          end.
      end.
    END.
  END. /* not avail itemfg */
  
  IF AVAIL itemfg THEN
    ASSIGN
     lbl_i-no = "     FG Item#"
     fi_i-no  = itemfg.i-no
     ip-rec_key = itemfg.rec_key .

  ELSE DO:
    /* FIND itemfg WHERE itemfg.rec_key EQ ip-rec_key NO-LOCK NO-ERROR. */
    MESSAGE "No FG Item entered. " VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.        
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "notes"}
  {src/adm/template/snd-list.i "itemfg"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

