&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: windows/<table>.w

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

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE vlSelected          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lv-sys-ctrl-rec-key AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-att               AS LOG       NO-UNDO.

{methods\defines\phone.i &new="NEW"}

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE h_Browse01 h_setting

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Record-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES setting
&Scoped-define FIRST-EXTERNAL-TABLE setting


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR setting.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE W-Win        AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_attach     AS HANDLE        NO-UNDO.
DEFINE VARIABLE h_exit       AS HANDLE        NO-UNDO.
DEFINE VARIABLE h_export     AS HANDLE        NO-UNDO.
DEFINE VARIABLE h_folder     AS HANDLE        NO-UNDO.
DEFINE VARIABLE h_options    AS HANDLE        NO-UNDO.
DEFINE VARIABLE h_p-navico   AS HANDLE        NO-UNDO.
DEFINE VARIABLE h_p-navico-2 AS HANDLE        NO-UNDO.
DEFINE VARIABLE h_p-updsav   AS HANDLE        NO-UNDO.
DEFINE VARIABLE h_p-updsav-2 AS HANDLE        NO-UNDO.
DEFINE VARIABLE h_smartmsg   AS HANDLE        NO-UNDO.
DEFINE VARIABLE h_setting    AS HANDLE        NO-UNDO.
DEFINE VARIABLE h_setting-2  AS HANDLE        NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 150 BY 23.81
    BGCOLOR 15 .

DEFINE FRAME message-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 90 ROW 2.91
    SIZE 61 BY 1.43
    BGCOLOR 15 .

DEFINE FRAME OPTIONS-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 2 ROW 1
    SIZE 148 BY 1.91
    BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: ASI.setting
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW W-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "Setting Maintenance"
        HEIGHT             = 23.81
        WIDTH              = 180
        MAX-HEIGHT         = 320
        MAX-WIDTH          = 320
        VIRTUAL-HEIGHT     = 320
        VIRTUAL-WIDTH      = 320
        RESIZE             = YES
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
ASSIGN 
    FRAME message-frame:FRAME = FRAME F-Main:HANDLE
    FRAME OPTIONS-FRAME:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN 
    XXTABVALXX = FRAME OPTIONS-FRAME:MOVE-BEFORE-TAB-ITEM (FRAME message-frame:HANDLE)
    /* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME message-frame
                                                                        */
/* SETTINGS FOR FRAME OPTIONS-FRAME
                                                                        */
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME message-frame
/* Query rebuild information for FRAME message-frame
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
ON END-ERROR OF W-Win /* System Control Parameters Maintenance */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* System Control Parameters Maintenance */
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

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
{custom/initializeprocs.i}
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
    DEFINE VARIABLE adm-current-page AS INTEGER NO-UNDO.

    RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
    ASSIGN 
        adm-current-page = INTEGER(RETURN-VALUE).

    CASE adm-current-page: 

        WHEN 0 THEN 
            DO:
                RUN init-object IN THIS-PROCEDURE (
                    INPUT  'smartobj/smartmsg.w':U ,
                    INPUT  FRAME message-frame:HANDLE ,
                    INPUT  '':U ,
                    OUTPUT h_smartmsg ).
                RUN set-position IN h_smartmsg ( 1.24 , 3.00 ) NO-ERROR.
                /* Size in UIB:  ( 1.14 , 32.00 ) */

                RUN init-object IN THIS-PROCEDURE (
                    INPUT  'smartobj/attach.w':U ,
                    INPUT  FRAME OPTIONS-FRAME:HANDLE ,
                    INPUT  '':U ,
                    OUTPUT h_attach ).
                RUN set-position IN h_attach ( 1.00 , 85.20 ) NO-ERROR.
                /* Size in UIB:  ( 1.81 , 7.80 ) */

                RUN init-object IN THIS-PROCEDURE (
                    INPUT  'adm/objects/folder.w':U ,
                    INPUT  FRAME F-Main:HANDLE ,
                    INPUT  'FOLDER-LABELS = ':U + 'Browse|Setting' + ',
                     FOLDER-TAB-TYPE = 2':U ,
                    OUTPUT h_folder ).
                RUN set-position IN h_folder ( 3.14 , 2.00 ) NO-ERROR.
                RUN set-size IN h_folder ( 21.67 , 178.00 ) NO-ERROR.

                RUN init-object IN THIS-PROCEDURE (
                    INPUT  'smartobj/options.w':U ,
                    INPUT  FRAME OPTIONS-FRAME:HANDLE ,
                    INPUT  '':U ,
                    OUTPUT h_options ).
                RUN set-position IN h_options ( 1.00 , 93.00 ) NO-ERROR.
                /* Size in UIB:  ( 1.81 , 55.80 ) */

                RUN init-object IN THIS-PROCEDURE (
                    INPUT  'smartobj/exit.w':U ,
                    INPUT  FRAME OPTIONS-FRAME:HANDLE ,
                    INPUT  '':U ,
                    OUTPUT h_exit ).
                RUN set-position IN h_exit ( 1.00 , 141.00 ) NO-ERROR.
                /* Size in UIB:  ( 1.81 , 7.80 ) */

                /* Links to SmartObject h_attach. */
                RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'attach':U , h_attach ).

                /* Links to SmartFolder h_folder. */
                RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).
                RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'udficon':U , h_options ).
                /* Adjust the tab order of the smart objects. */
                RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
                    FRAME message-frame:HANDLE , 'AFTER':U ).
                RUN adjust-tab-order IN adm-broker-hdl ( h_options ,
                    h_attach , 'AFTER':U ).
                RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
                    h_options , 'AFTER':U ).
            END. /* Page 0 */
        WHEN 1 THEN 
            DO:
                RUN init-object IN THIS-PROCEDURE (
                    INPUT  'viewers/export.w':U ,
                    INPUT  FRAME OPTIONS-FRAME:HANDLE ,
                    INPUT  '':U ,
                    OUTPUT h_export ).
                RUN set-position IN h_export ( 1.00 , 77.40 ) NO-ERROR.
                /* Size in UIB:  ( 1.81 , 7.80 ) */

                RUN init-object IN THIS-PROCEDURE (
                    INPUT  'browsers/setting.w':U ,
                    INPUT  FRAME F-Main:HANDLE ,
                    INPUT  'Layout = ':U ,
                    OUTPUT h_setting ).
                RUN set-position IN h_setting ( 4.81 , 3.00 ) NO-ERROR.
                RUN set-size IN h_setting ( 19.52 , 178.00 ) NO-ERROR.

                /* Initialize other pages that this page requires. */
                RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

                /* Links to SmartObject h_export. */
                RUN add-link IN adm-broker-hdl ( h_setting , 'export-xl':U , h_export ).

                /* Links to SmartNavBrowser h_setting. */
       //RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_setting ).
                RUN add-link IN adm-broker-hdl ( h_setting , 'Record':U , THIS-PROCEDURE ).

                /* Adjust the tab order of the smart objects. */
                RUN adjust-tab-order IN adm-broker-hdl ( h_setting ,
                    h_folder , 'AFTER':U ).
            END. /* Page 1 */
        WHEN 2 THEN 
            DO:
                RUN init-object IN THIS-PROCEDURE (
                    INPUT  'viewers/setting.w':U ,
                    INPUT  FRAME F-Main:HANDLE ,
                    INPUT  '':U ,
                    OUTPUT h_setting-2 ).
                RUN set-position IN h_setting-2 ( 5.05 , 7.00 ) NO-ERROR.
                RUN set-size IN h_setting-2 ( 9.76 , 138.00 ) NO-ERROR.
                /* Position in AB:  ( 5.05 , 7.00 ) */
                /* Size in UIB:  ( 9.76 , 138.00 ) */  
                    
                /* Initialize other pages that this page requires. */
                RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

                /* Links to  h_setting-2. */
       //RUN add-link IN adm-broker-hdl ( h_p-updsav , 'TableIO':U , h_setting-2 ).
                RUN add-link IN adm-broker-hdl ( h_setting , 'Record':U , h_setting-2 ).
                RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'getsec':U , h_setting-2 ). 
         
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

    /* Create a list of all the tables that we need to get.            */
    {src/adm/template/row-list.i "setting"}

    /* Get the record ROWID's from the RECORD-SOURCE.                  */
    {src/adm/template/row-get.i}

    /* FIND each record specified by the RECORD-SOURCE.                */
    {src/adm/template/row-find.i "setting"}

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
    VIEW FRAME F-Main IN WINDOW W-Win.
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    VIEW FRAME OPTIONS-FRAME IN WINDOW W-Win.
    {&OPEN-BROWSERS-IN-QUERY-OPTIONS-FRAME}
    VIEW FRAME message-frame IN WINDOW W-Win.
    {&OPEN-BROWSERS-IN-QUERY-message-frame}
    VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
    /*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE viCurrPage   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hPgmSecurity AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lResult      AS LOG       NO-UNDO.
    DEFINE VARIABLE cSysNmae     AS CHARACTER NO-UNDO.
  
    /* Code placed here will execute PRIOR to standard behavior. */
    RUN GET-ATTRIBUTE('CURRENT-PAGE').

    viCurrPage = INT(RETURN-VALUE).
                                     
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .
    IF viCurrPage = 2 THEN 
    DO:
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'getsec-target':U, OUTPUT char-hdl).       
    END.

    /* Code placed here will execute AFTER standard behavior.    */
    {methods/winReSizePgChg.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available W-Win 
PROCEDURE local-row-available :
    /*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

 

    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attach-target':U, OUTPUT char-hdl).
  
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Select_att W-Win 
PROCEDURE Select_att :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    {methods/select_attcust.i rec_key_value header_value 0}

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
    {src/adm/template/snd-list.i "setting"}

    /* Deal with any unexpected table requests before closing.           */
    {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-s-rec_key W-Win 
PROCEDURE set-s-rec_key :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rec_key AS CHARACTER NO-UNDO.

    s-rec_key = ip-rec_key.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE value-changed-proc W-Win 
PROCEDURE value-changed-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

