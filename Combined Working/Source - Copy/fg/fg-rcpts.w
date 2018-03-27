&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: addon\fg\fg-rcpt.w
          
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
DEF INPUT PARAM ip-do-what AS cha NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR lvlAutoAdd AS LOG NO-UNDO.
DEF VAR lvcReturnChar AS CHAR NO-UNDO.
DEF VAR lvlRecFound AS LOG NO-UNDO.
{custom/gcompany.i}

&scoped-define asi-exit asi-exit

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-rcptds AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-rcptds AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updba2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updcdc-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 129.8 BY 13.05
         BGCOLOR 15 .

DEFINE FRAME FRAME-A
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         THREE-D 
         AT COL 31 ROW 1
         SIZE 99 BY 1.19
         BGCOLOR 15  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
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
         TITLE              = "WAREHOUSE TRANSACTION RECEIPTS(FINISHED GOODS)"
         HEIGHT             = 12.48
         WIDTH              = 130.4
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = NO
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Win:LOAD-ICON("adeicon\progress":U) THEN
    MESSAGE "Unable to load icon: adeicon\progress"
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
ASSIGN FRAME FRAME-A:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME FRAME-A
   UNDERLINE                                                            */
ASSIGN 
       FRAME FRAME-A:SENSITIVE        = FALSE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* WAREHOUSE TRANSACTION RECEIPTS(FINISHED GOODS) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
    
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* WAREHOUSE TRANSACTION RECEIPTS(FINISHED GOODS) */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  /*DEF VAR lv-can-exit AS LOG NO-UNDO.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"can-exit-source", OUTPUT char-hdl).
  RUN can-exit IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-can-exit).
  IF NOT lv-can-exit THEN RETURN NO-apply.*/

  RUN do-cancel IN h_p-updba2.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

{custom/getcmpny.i}
RUN sys/ref/nk1look.p (gcompany, "SSFGSCAN", "I", NO, NO, "", "", 
                       OUTPUT lvcReturnChar, OUTPUT lvlRecFound).
IF lvlRecFound THEN
	lvlAutoAdd = IF INT(lvcReturnChar) = 0 THEN YES ELSE NO.

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
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Receipts|Set Parts' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 12.38 , 129.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             FRAME FRAME-A:HANDLE , 'BEFORE':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/smartmsg.w':U ,
             INPUT  FRAME FRAME-A:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_smartmsg ).
       RUN set-position IN h_smartmsg ( 1.00 , 58.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'addon/fg/b-rcptds.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Key-Name = ,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_b-rcptds ).
       RUN set-position IN h_b-rcptds ( 2.43 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-rcptds ( 7.14 , 112.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-updba2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updba2 ).
       RUN set-position IN h_p-updba2 ( 10.05 , 11.00 ) NO-ERROR.
       RUN set-size IN h_p-updba2 ( 1.67 , 68.00  ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 10.52 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       /* Links to SmartBrowser h_b-rcptds. */
       RUN add-link IN adm-broker-hdl ( h_p-updba2 , 'TableIO':U , h_b-rcptds ).
       RUN add-link IN adm-broker-hdl ( h_b-rcptds , 'can-exit':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-rcptds ,
             FRAME FRAME-A:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updba2 ,
             h_b-rcptds , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_p-updba2 , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'addon/fg/f-rcptds.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Key-Name = ,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_f-rcptds ).
       RUN set-position IN h_f-rcptds ( 2.91 , 4.00 ) NO-ERROR.
       RUN set-size IN h_f-rcptds ( 10.24 , 125.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updcdc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updcdc-2 ).
       RUN set-position IN h_p-updcdc-2 ( 10.52 , 16.00 ) NO-ERROR.
       RUN set-size IN h_p-updcdc-2 ( 1.76 , 82.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartNavBrowser h_f-rcptds. */
       RUN add-link IN adm-broker-hdl ( h_b-rcptds , 'linker':U , h_f-rcptds ).
       RUN add-link IN adm-broker-hdl ( h_p-updcdc-2 , 'TableIO':U , h_f-rcptds ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_f-rcptds ,
             FRAME FRAME-A:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updcdc-2 ,
             h_f-rcptds , 'AFTER':U ).
    END. /* Page 2 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page EQ 0 
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asi-exit W-Win 
PROCEDURE asi-exit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*DEF VAR lv-can-exit AS LOG NO-UNDO.
   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"can-exit-source", OUTPUT char-hdl).
   RUN can-exit IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-can-exit).*/
   
   /*IF NOT lv-can-exit THEN RETURN ERROR.*/
   RUN do-cancel IN h_p-updba2.

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
  VIEW FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW FRAME FRAME-A IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  FRAME FRAME-A:SENSITIVE = NO.
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-do-what W-Win 
PROCEDURE get-do-what :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-do-what AS cha NO-UNDO.

  op-do-what = ip-do-what.
  IF op-do-what = "Delete" THEN
     {&WINDOW-NAME}:TITLE = "Delete Finished Goods".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view W-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF lvlAutoAdd THEN DO:
      {methods/selectTab.i 1}
      RUN scan-next IN h_b-rcptds.
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setUserExit W-Win 
PROCEDURE setUserExit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR char-hdl AS cha NO-UNDO.
  
   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"cancel-item-target", OUTPUT char-hdl).
   RUN cancel-item IN WIDGET-HANDLE(char-hdl).
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

