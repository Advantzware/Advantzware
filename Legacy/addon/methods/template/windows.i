&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : windows.i
    Purpose     : Smart Windows Include for Internal Procedures

    Syntax      : {methods/template/windows.i}

    Description : Smart Windows Include for Internal Procedures

    Author(s)   : Ron Stark
    Created     : 02/07/98
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

DEFINE VARIABLE rec_key_value AS CHARACTER NO-UNDO.
DEFINE VARIABLE header_value AS CHARACTER NO-UNDO.

{methods/defines/noreckey.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 17.67
         WIDTH              = 47.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

ON ESC OF {&WINDOW-NAME} ANYWHERE DO:
  RUN Select_Exit.
  RETURN NO-APPLY.
END.

{methods/menus/wndwmenu.i}
{methods/enhance.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Allow-Create Include 
PROCEDURE Allow-Create :
/*------------------------------------------------------------------------------
  Purpose:     Check Security Access to Allow Create
  Parameters:  OUTPUT allow-flag
  Notes:       
------------------------------------------------------------------------------*/
  &Scoped-define ACCESSTYPE create
  {methods/template/security.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Allow-Delete Include 
PROCEDURE Allow-Delete :
/*------------------------------------------------------------------------------
  Purpose:     Check Security Access to Allow Delete
  Parameters:  OUTPUT allow-flag
  Notes:       
------------------------------------------------------------------------------*/
  &Scoped-define ACCESSTYPE delete
  {methods/template/security.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Allow-Update Include 
PROCEDURE Allow-Update :
/*------------------------------------------------------------------------------
  Purpose:     Check Security Access to Allow Update
  Parameters:  OUTPUT allow-flag
  Notes:       
------------------------------------------------------------------------------*/
  &Scoped-define ACCESSTYPE update
  {methods/template/security.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-g_rec_key Include 
PROCEDURE Get-g_rec_key :
/*------------------------------------------------------------------------------
  Purpose:     Get rec_key value from g_rec_key string
  Parameters:  op-rec_key
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-rec_key AS CHARACTER NO-UNDO.

  IF g_rec_key NE "" THEN
  op-rec_key = ENTRY(NUM-ENTRIES(g_rec_key),g_rec_key).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-Header Include 
PROCEDURE Get-Header :
/*------------------------------------------------------------------------------
  Purpose:     Get header_value
  Parameters:  op-header
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-header AS CHARACTER NO-UNDO.

  op-header = header_value.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-Rec-Key Include 
PROCEDURE Get-Rec-Key :
/*------------------------------------------------------------------------------
  Purpose:     Get rec_key_value
  Parameters:  op-rec_key
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-rec_key AS CHARACTER NO-UNDO.

  op-rec_key = rec_key_value.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init-Options-Panel Include 
PROCEDURE Init-Options-Panel :
/*------------------------------------------------------------------------------
  Purpose:     Initialize Options Panel Buttons
  Parameters:  on-off flags for each button
  Notes:       called from smartobj/options.w
------------------------------------------------------------------------------*/
&IF "{&NOMENUS}" NE "yes" &THEN
  DEFINE OUTPUT PARAMETER search-button AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER list-button AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER notes-button AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER misc_fields-button AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER spec-note-button AS LOGICAL NO-UNDO.
  DEFINE VARIABLE listname AS CHARACTER NO-UNDO.

  ASSIGN
    listname = SUBSTR("{&FIRST-EXTERNAL-TABLE}",1,7) + "_"
    search-button = IF SEARCH("searches/{&FIRST-EXTERNAL-TABLE}.r") = ? AND
                       SEARCH("searches/{&FIRST-EXTERNAL-TABLE}.p") = ? THEN no
                    ELSE yes
    list-button = IF SEARCH("listobjs/" + listname + ".r") = ? AND
                     SEARCH("listobjs/" + listname + ".w") = ? THEN no
                  ELSE yes
    notes-button = IF /*INDEX("{&NORECKEY}","{&FIRST-EXTERNAL-TABLE}")*/
                       lookup("{&FIRST-EXTERNAL-TABLE}","{&NORECKEY}"," ") = 0 THEN yes
                   ELSE no
    misc_fields-button = IF b-prgrms.mfgroup = "" THEN no ELSE yes
    spec-note-button = lookup("{&FIRST-EXTERNAL-TABLE}","est,item,itemfg,cust,vend,oe-ord,job,pc-prdd,pc-prdh,oe-ordl,po-ordl,quotehd") > 0
    .

  &Scoped-define MENUITEM search
  IF NOT {&MENUITEM}-button THEN
  ASSIGN
    MENU-ITEM m_{&MENUITEM}:SENSITIVE IN MENU MENU-BAR-{&WINDOW-NAME} = no
    MENU-ITEM p_{&MENUITEM}:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = no.
  &Scoped-define MENUITEM list
  IF NOT {&MENUITEM}-button THEN
  ASSIGN
    MENU-ITEM m_{&MENUITEM}:SENSITIVE IN MENU MENU-BAR-{&WINDOW-NAME} = no
    MENU-ITEM p_{&MENUITEM}:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = no.
  &Scoped-define MENUITEM notes
  IF NOT {&MENUITEM}-button THEN
  ASSIGN
    MENU-ITEM m_{&MENUITEM}:SENSITIVE IN MENU MENU-BAR-{&WINDOW-NAME} = no
    MENU-ITEM p_{&MENUITEM}:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = no.
  &Scoped-define MENUITEM misc_fields
  IF NOT {&MENUITEM}-button THEN
  ASSIGN
    MENU-ITEM m_{&MENUITEM}:SENSITIVE IN MENU MENU-BAR-{&WINDOW-NAME} = no
    MENU-ITEM p_{&MENUITEM}:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = no.
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize Include 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */
  IF access-close THEN
  APPLY 'CLOSE' TO THIS-PROCEDURE.

  &IF "{&FIRST-EXTERNAL-TABLE}" = "address" OR
      "{&FIRST-EXTERNAL-TABLE}" = "note" OR
      "{&FIRST-EXTERNAL-TABLE}" = "phone" &THEN
  {methods/windows/initial/{&FIRST-EXTERNAL-TABLE}.i}
  &ENDIF
  {methods/template/local/wndwinit.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MF-Message Include 
PROCEDURE MF-Message :
/*------------------------------------------------------------------------------
  Purpose:     Display Message if Misc Flds Data exists for selected record
  Parameters:  can-find(first mfvalue of selected record)
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-misc-flds AS LOGICAL NO-UNDO.

  RUN Show-MF-Message IN h_smartmsg (ip-misc-flds).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Notes-Message Include 
PROCEDURE Notes-Message :
/*------------------------------------------------------------------------------
  Purpose:     Display Message if Notes exist for selected record
  Parameters:  can-find(first notes of selected record)
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-notes AS LOGICAL NO-UNDO.

  RUN Show-Notes-Message IN h_smartmsg (ip-notes).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reset-g_rec_key Include 
PROCEDURE Reset-g_rec_key :
/*------------------------------------------------------------------------------
  Purpose:     Reset rec_key by removing last entry from string
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF g_rec_key = "" THEN
  RETURN.
  ENTRY(NUM-ENTRIES(g_rec_key),g_rec_key) = "".
  IF LENGTH(g_rec_key) GT 0 THEN
  SUBSTR(g_rec_key,LENGTH(g_rec_key),1) = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Run-Search Include 
PROCEDURE Run-Search :
/*------------------------------------------------------------------------------
  Purpose:     Run Search program from with record-source (browser) program
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  IF CAN-DO(g_developer,USERID("NOSWEAT")) THEN
  DO:
    MESSAGE "Update Search Program?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE updatesrch AS LOGICAL.
    IF updatesrch THEN
    DO:
      RUN Get_Procedure IN Persistent-Handle ("searches.",OUTPUT run-proc,no).
      IF run-proc NE "" THEN
      RUN VALUE(run-proc) ("{&FIRST-EXTERNAL-TABLE}.").
    END.
  END.
  {methods/run_link.i "RECORD-SOURCE" "Run-Search"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus Include 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     Call to RECORD-SOURCE (usually main browser)
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/run_link.i "RECORD-SOURCE" "Set-Focus"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Rec-Key_Header Include 
PROCEDURE Set-Rec-Key_Header :
/*------------------------------------------------------------------------------
  Purpose:     Set rec_key_value from currently selected record's rec_key value
  Parameters:  ip-rec_key
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER ip-rec_key AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-header AS CHARACTER NO-UNDO.

  ASSIGN
    rec_key_value = ip-rec_key
    header_value = ip-header.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set_Viewer_Mode Include 
PROCEDURE Set_Viewer_Mode :
/*------------------------------------------------------------------------------
  Purpose:     on action of browser put viewer to browser into selected mode
  Parameters:  INPUT browser's handle and mode
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER browser-handle AS WIDGET-HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER viewer-mode AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  IF VALID-HANDLE(adm-broker-hdl) THEN
  DO:
    RUN get-link-handle IN adm-broker-hdl
        (browser-handle,'RECORD-TARGET':U,OUTPUT char-hdl).
    DO i = 1 TO NUM-ENTRIES(char-hdl):
      phandle = WIDGET-HANDLE(ENTRY(i,char-hdl)).
      IF INDEX(phandle:FILE-NAME,'viewers') NE 0 THEN
      DO:
        CASE viewer-mode:
          WHEN 'add' OR WHEN 'copy' THEN
          DO:
            RUN Add-Not-Allowed IN phandle NO-ERROR.
            IF NOT ERROR-STATUS:ERROR THEN
            NEXT.
          END.
          WHEN 'delete' THEN
          DO:
            RUN Delete-Not-Allowed IN phandle NO-ERROR.
            IF NOT ERROR-STATUS:ERROR THEN
            NEXT.
          END.
          WHEN 'update' THEN
          DO:
            RUN Update-Not-Allowed IN phandle NO-ERROR.
            IF NOT ERROR-STATUS:ERROR THEN
            NEXT.
          END.
        END CASE.
        RUN get-link-handle IN adm-broker-hdl
            (phandle,'TABLEIO':U,OUTPUT char-hdl).
        DO i = 1 TO NUM-ENTRIES(char-hdl):
          phandle = WIDGET-HANDLE(char-hdl).
          CASE viewer-mode:
            WHEN 'add' THEN
            RUN notify IN phandle ('add-record').
            WHEN 'copy' THEN
            RUN notify IN phandle ('copy-record').
            WHEN 'delete' THEN
            RUN notify IN phandle ('delete-record').
            WHEN 'update' THEN
            RUN new-state IN phandle ('update-begin').
          END.
          LEAVE.
        END.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

