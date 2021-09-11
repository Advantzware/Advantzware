&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: windows/setting-dialog.w

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
DEFINE INPUT PARAMETER ipoSetting AS system.Setting NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Record-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_exit-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_scopefilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_setting AS HANDLE NO-UNDO.
DEFINE VARIABLE h_setting-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_setting-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_setting-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_settingfilter AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     SPACE(182.00) SKIP(30.10)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 
         TITLE "Program Setting" WIDGET-ID 100.

DEFINE FRAME Options-frame
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 182 BY 2
         BGCOLOR 21  WIDGET-ID 200.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Design Page: 1
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME Options-frame:FRAME = FRAME D-Dialog:HANDLE.

/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME Options-frame
                                                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Program Setting */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
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
             INPUT  FRAME Options-frame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit-2 ).
       RUN set-position IN h_exit-2 ( 1.00 , 174.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder2.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Setting|Scope' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder2 ).
       RUN set-position IN h_folder2 ( 3.14 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder2 ( 27.62 , 182.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder2. */
       RUN add-link IN adm-broker-hdl ( h_folder2 , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder2 ,
             FRAME Options-frame:HANDLE , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/settingfilter.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_settingfilter ).
       RUN set-position IN h_settingfilter ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 180.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/setting.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  'SAVE-TYPE = DATABASE,
                     BROWSE-COLUMNS = settingName|description|settingValue|scopeTable|scopeField1|scopeField2|scopeField3|inactive|settingUser|programID,
                     BROWSE-COLUMNS-DISPLAY = settingName|description|settingValue,
                     HIDE-SEARCH = TRUE,
                     HIDE-SETTING-FILTER = TRUE,
                     HIDE-SCOPE-FILTER = TRUE,
                     SETTING-FILTER-TYPE = SettingType,
                     LOAD-DATA-FROM-TT = TRUE':U ,
             OUTPUT h_setting ).
       RUN set-position IN h_setting ( 8.95 , 2.00 ) NO-ERROR.
       RUN set-size IN h_setting ( 22.05 , 181.00 ) NO-ERROR.

       /* Links to SmartBrowser h_setting. */
       RUN add-link IN adm-broker-hdl ( h_settingfilter , 'Search':U , h_setting ).
       RUN add-link IN adm-broker-hdl ( h_settingfilter , 'State':U , h_setting ).
       RUN add-link IN adm-broker-hdl ( h_setting , 'Record':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_settingfilter ,
             h_folder2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_setting ,
             h_settingfilter , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/setting.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_setting-4 ).
       RUN set-position IN h_setting-4 ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.67 , 180.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/scopefilter.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_scopefilter ).
       RUN set-position IN h_scopefilter ( 11.29 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.52 , 180.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/setting.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_setting-2 ).
       RUN set-position IN h_setting-2 ( 15.91 , 107.20 ) NO-ERROR.
       /* Size in UIB:  ( 14.71 , 70.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/setting.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  'SAVE-TYPE = DATABASE,
                     BROWSE-COLUMNS = settingName|description|settingValue|scopeTable|scopeField1|scopeField2|scopeField3|inactive|settingUser|programID,
                     BROWSE-COLUMNS-DISPLAY = scopeTable|scopeField1|scopeField2|scopeField3|inactive|settingUser|programID,
                     HIDE-SEARCH = FALSE,
                     HIDE-SETTING-FILTER = TRUE,
                     HIDE-SCOPE-FILTER = FALSE,
                     SETTING-FILTER-TYPE = Setting,
                     LOAD-DATA-FROM-TT = TRUE':U ,
             OUTPUT h_setting-3 ).
       RUN set-position IN h_setting-3 ( 15.95 , 2.00 ) NO-ERROR.
       RUN set-size IN h_setting-3 ( 14.57 , 104.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_setting-4. */
       RUN add-link IN adm-broker-hdl ( h_setting , 'Record':U , h_setting-4 ).

       /* Links to SmartViewer h_setting-2. */
       RUN add-link IN adm-broker-hdl ( h_setting , 'Setting':U , h_setting-2 ).
       RUN add-link IN adm-broker-hdl ( h_setting-3 , 'Record':U , h_setting-2 ).

       /* Links to SmartBrowser h_setting-3. */
       RUN add-link IN adm-broker-hdl ( h_scopefilter , 'Search':U , h_setting-3 ).
       RUN add-link IN adm-broker-hdl ( h_scopefilter , 'State':U , h_setting-3 ).
       RUN add-link IN adm-broker-hdl ( h_setting , 'Record':U , h_setting-3 ).
       RUN add-link IN adm-broker-hdl ( h_setting-2 , 'State':U , h_setting-3 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_setting-4 ,
             h_folder2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_scopefilter ,
             h_setting-4 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_setting-2 ,
             h_scopefilter , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_setting-3 ,
             h_setting-2 , 'AFTER':U ).
    END. /* Page 2 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
  HIDE FRAME Options-frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
  VIEW FRAME Options-frame.
  {&OPEN-BROWSERS-IN-QUERY-Options-frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSetting D-Dialog 
PROCEDURE GetSetting :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opoSetting AS system.Setting NO-UNDO.

    opoSetting = ipoSetting.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Select_Exit D-Dialog 
PROCEDURE Select_Exit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    APPLY "GO" TO SELF.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
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

