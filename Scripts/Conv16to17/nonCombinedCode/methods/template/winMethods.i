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
{methods/prgsecur.i "WIN"}
{methods/defines/winReSize.i}

DEFINE VARIABLE rec_key_value AS CHARACTER NO-UNDO.
DEFINE VARIABLE header_value AS CHARACTER NO-UNDO.

DEFINE VARIABLE misc_rec_key_value AS CHARACTER NO-UNDO.
DEFINE VARIABLE misc_header_value AS CHARACTER NO-UNDO.

{methods/defines/noreckey.i}

{custom/resizdef.i}  /* resizing window definition include */

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
         HEIGHT             = 23.81
         WIDTH              = 46.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include


/* ***************************  Main Block  *************************** */

{methods/menus/{&winMethods}.i}
{methods/enhance.i}
{sys/inc/f3helpw.i}
{custom/resizmx.i}
{custom/resizrs.i}

ON WINDOW-MAXIMIZED OF {&WINDOW-NAME} DO:
  RUN winReSize.
END.

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
  DEFINE OUTPUT PARAMETER search-button    AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER list-button      AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER notes-button     AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER udf-button       AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER spec-note-button AS LOGICAL NO-UNDO.
  
  DEFINE VARIABLE listname AS CHARACTER NO-UNDO.

  ASSIGN
    listname         = SUBSTR("{&FIRST-EXTERNAL-TABLE}",1,7) + "_"
    search-button    = SEARCH("searches/{&FIRST-EXTERNAL-TABLE}.r") NE ? OR 
                       SEARCH("searches/{&FIRST-EXTERNAL-TABLE}.p") NE ? 
    list-button      = SEARCH("listobjs/" + listname + ".r") NE ? OR
                       SEARCH("listobjs/" + listname + ".w") NE ? 
    notes-button     = LOOKUP("{&FIRST-EXTERNAL-TABLE}","{&NORECKEY}"," ") EQ 0 
    spec-note-button = LOOKUP("{&FIRST-EXTERNAL-TABLE}","est,item,itemfg,cust,vend,oe-ord,job,pc-prdd,pc-prdh,oe-ordl,po-ordl,quotehd,oe-relh") GT 0
    udf-button       = CAN-FIND(FIRST mfgroup WHERE LOOKUP(v-prgmname,mfgroup.mfgroup_data,"|") NE 0)    
    .

  &Scoped-define MENUITEM search
  IF NOT {&MENUITEM}-button THEN
  ASSIGN
    MENU-ITEM m_{&MENUITEM}:SENSITIVE IN MENU MENU-BAR-{&WINDOW-NAME}   = NO 
    MENU-ITEM p_{&MENUITEM}:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = NO .
  &Scoped-define MENUITEM list
  IF NOT {&MENUITEM}-button THEN
  ASSIGN
    MENU-ITEM m_{&MENUITEM}:SENSITIVE IN MENU MENU-BAR-{&WINDOW-NAME}   = NO 
    MENU-ITEM p_{&MENUITEM}:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = NO .
  &Scoped-define MENUITEM notes
  IF NOT {&MENUITEM}-button THEN
  ASSIGN
    MENU-ITEM m_{&MENUITEM}:SENSITIVE IN MENU MENU-BAR-{&WINDOW-NAME}   = NO 
    MENU-ITEM p_{&MENUITEM}:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = NO .
  &Scoped-define MENUITEM udf
  IF NOT {&MENUITEM}-button THEN
  ASSIGN
    MENU-ITEM m_{&MENUITEM}:SENSITIVE IN MENU MENU-BAR-{&WINDOW-NAME}   = NO
    MENU-ITEM p_{&MENUITEM}:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = NO .
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
  IF access-close THEN DO:  /* YSK  not leave window on after closed */
      APPLY 'CLOSE' TO THIS-PROCEDURE.
      RETURN.
  END.

  &IF "{&FIRST-EXTERNAL-TABLE}" EQ "address" OR
      "{&FIRST-EXTERNAL-TABLE}" EQ "notes" OR
      "{&FIRST-EXTERNAL-TABLE}" EQ "phone" OR
      "{&FIRST-EXTERNAL-TABLE}" EQ "oe-ord" &THEN
    {methods/windows/initial/{&FIRST-EXTERNAL-TABLE}.i}
  &ENDIF
  
        /* check if maximized for user */
  IF SEARCH('users/' + USERID('ASI') + '/' + v-prgmname + 'winReSize') NE ? OR
        /* check if maximized for all users */
     SEARCH('users/' + v-prgmname + 'winReSize') NE ? OR
        /* check if maximized for all users all programs */
     SEARCH('users/winReSizeAll') NE ? OR 
     VALID-OBJECT (oForm) THEN DO:
    {&WINDOW-NAME}:WINDOW-STATE = 1.
    RUN winReSize.
    {methods/winReSizePgChg.i}
  END.

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

  IF CAN-FIND(FIRST mfgroup
              WHERE LOOKUP(v-prgmname,mfgroup.mfgroup_data,"|") NE 0) THEN
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUDFActive Include
PROCEDURE pUDFActive:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opUDFActive AS LOGICAL NO-UNDO.
    
    opUDFActive = CAN-FIND(FIRST mfgroup WHERE LOOKUP(v-prgmname,mfgroup.mfgroup_data,"|") NE 0) .

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
  IF CAN-DO(g_developer,USERID("ASI")) THEN DO:
    MESSAGE "Update Search Program?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE updatesrch AS LOGICAL.
    IF updatesrch THEN DO:
      RUN Get_Procedure IN Persistent-Handle ("searches.",OUTPUT run-proc,no).
      IF run-proc NE "" THEN
      RUN VALUE(run-proc) ("{&FIRST-EXTERNAL-TABLE}.").
    END.
  END.
  {methods/run_link.i "RECORD-SOURCE" "Run-Search"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_appl Include
PROCEDURE select_appl :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
run windows/utillook.w.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_dept Include
PROCEDURE select_dept :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    RUN Get_Procedure IN Persistent-Handle ('specnote.',OUTPUT run-proc,no).
    IF run-proc NE '' THEN {methods/smartrun.i (rec_key_value,header_value)} .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_frac Include
PROCEDURE select_frac :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    {methods/calcBtnLink.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_help Include
PROCEDURE select_help :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
/*  run system/asihelp.w.   LATER */

  APPLY "entry" TO FRAME {&frame-name}.

  APPLY KEYCODE ("f3") TO FRAME {&frame-name}.
  RETURN NO-APPLY .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_home Include
PROCEDURE select_home :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
 run get-attribute ("IS-Home").

 if return-value = ? or return-value = "?" then do:
    run set-attribute-list ("Is-Home = Home ") .
    {methods/run_link.i "RECORD-SOURCE" "dispatch" "('get-first')"}
 end.
 else do:
      if return-value = "Home" then   do:
          run set-attribute-list ("Is-Home = End ") .
         {methods/run_link.i "RECORD-SOURCE" "dispatch" "('get-last')"}
      end.
      else do:
           run set-attribute-list ("Is-Home = Home ") .
           {methods/run_link.i "RECORD-SOURCE" "dispatch" "('get-first')"}
      end.
 end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_spec Include
PROCEDURE select_spec :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  &IF DEFINED(item_spec) &THEN   /* for fg or rm item spec note */
    &IF '{&winMethods}' EQ 'wndwmenu' &THEN
      IF "{&item_spec}" = "Customer" THEN DO:
         RUN Get_Procedure IN Persistent-Handle ('specnot3.',OUTPUT run-proc,no).
         IF run-proc NE '' THEN {methods/smartrun.i (rec_key_value,header_value)} .
      END.
      ELSE IF "{&item_spec}" = "Vendor" THEN DO:
         RUN Get_Procedure IN Persistent-Handle ('specnot4.',OUTPUT run-proc,no).
         IF run-proc NE '' THEN {methods/smartrun.i (rec_key_value,header_value)} .
      END.
      ELSE DO:
         RUN Get_Procedure IN Persistent-Handle ('specnot2.',OUTPUT run-proc,no).
         IF INDEX(PROGRAM-NAME(1),"est/w-est") GT 0 OR
            index(program-name(1),"oe/w-order") GT 0 OR
            index(program-name(1),"oeinq/w-ordinq") GT 0 OR
            index(program-name(1),"windows/itemfg") GT 0 THEN
            RUN windows/specnot2.w(rec_key_value,header_value).
         ELSE
         IF run-proc NE '' THEN {methods/smartrun.i (rec_key_value,header_value)} .
      END.
    &ELSE
      IF "{&item_spec}" = "Customer" THEN
      RUN Get_Procedure IN Persistent-Handle ('specnot3.',OUTPUT run-proc,no).
      ELSE IF "{&item_spec}" = "Vendor" THEN
      RUN Get_Procedure IN Persistent-Handle ('specnot4.',OUTPUT run-proc,no).
      ELSE RUN Get_Procedure IN Persistent-Handle ('specnot2.',OUTPUT run-proc,no).
      IF run-proc NE '' THEN {methods/smartrun.i (rec_key_value,header_value)} .
    &ENDIF
  &ELSE
    RUN Get_Procedure IN Persistent-Handle ('specnote.',OUTPUT run-proc,no).
    IF run-proc NE '' THEN {methods/smartrun.i (rec_key_value,header_value)} .
  &ENDIF

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Misc-Rec-Key_Header Include
PROCEDURE Set-Misc-Rec-Key_Header :
/*------------------------------------------------------------------------------
  Purpose:     Set rec_key_value from currently selected record's rec_key value
  Parameters:  ip-rec_key
  Notes:
------------------------------------------------------------------------------*/
  &IF '{&winMethods}' NE 'wndwmnu2' &THEN
    DEFINE INPUT PARAMETER ip-rec_key AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-header AS CHARACTER NO-UNDO.

    ASSIGN
      misc_rec_key_value = ip-rec_key
      misc_header_value = ip-header.
  &ENDIF

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UDF Include
PROCEDURE UDF :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE isRunning AS LOGICAL NO-UNDO.

    FOR EACH mfgroup NO-LOCK:
        IF LOOKUP(v-prgmname,mfgroup.mfgroup_data,"|") EQ 0 THEN NEXT.
        LEAVE.
    END. /* each mfgroup */

    IF AVAILABLE(mfgroup) THEN DO:
        RUN Running_Procedures IN Persistent-Handle ("mfvalues.",OUTPUT isRunning).
        RUN UDF/mfvalues.w (ENTRY(1,mfgroup.mfgroup_data,"|"),
                            rec_key_value,
                            header_value,
                            h_smartmsg).
    END. /* avail mfgroup */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE winReSize Include
PROCEDURE winReSize :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  &IF DEFINED(winReSize) NE 0 &THEN
    DEFINE VARIABLE hPixels            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE wPixels            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE noReSize           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE noReSizeName       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE screenRatio        AS DECIMAL   NO-UNDO INITIAL 1.
    DEFINE VARIABLE winReSizeDat       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE currentWidget      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iOldRowDiff        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOldColDiff        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iWinKitCurrentPage AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hWinKitFrame       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE oWinKitControl     AS System.Windows.Forms.Control NO-UNDO.    

    ASSIGN
        iOldRowDiff = rowDiff + 1
        iOldColDiff = colDiff
        .

    IF NOT VALID-OBJECT (oForm) AND {&WINDOW-NAME}:WINDOW-STATE NE 1 THEN RETURN.

    winReSizeDat = 'users/' + USERID('ASI') + '/winReSize.dat'.
    IF SEARCH(winReSizeDat) NE ? THEN DO:
        INPUT FROM VALUE(winReSizeDat).
        IMPORT ^ screenRatio.
        INPUT CLOSE.
    END.

    ASSIGN
      hPixels = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
      wPixels = FRAME {&FRAME-NAME}:WIDTH-PIXELS
      rowDiff = FRAME {&FRAME-NAME}:HEIGHT
      colDiff = FRAME {&FRAME-NAME}:WIDTH
      {&WINDOW-NAME}:HEIGHT-PIXELS = hPixels + ({&WINDOW-NAME}:HEIGHT-PIXELS - hPixels) * screenRatio
      {&WINDOW-NAME}:WIDTH-PIXELS = wPixels + ({&WINDOW-NAME}:WIDTH-PIXELS - wPixels) * screenRatio
      {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - 40
      {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
      {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
      FRAME {&FRAME-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
      FRAME {&FRAME-NAME}:WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
      FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
      FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS
      hPixels = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - hPixels
      wPixels = FRAME {&FRAME-NAME}:WIDTH-PIXELS - wPixels
      rowDiff = FRAME {&FRAME-NAME}:HEIGHT - rowDiff
      colDiff = FRAME {&FRAME-NAME}:WIDTH - colDiff
      FRAME message-frame:WIDTH-PIXELS = FRAME message-frame:WIDTH-PIXELS + wPixels
      .

   IF VALID-OBJECT (oForm) AND rowDiff EQ 0 AND colDiff EQ 0 THEN DO:
        ASSIGN
            rowDiff = iOldRowDiff
            colDiff = iOldColDiff
            .
        RETURN .
   END.

   RUN GET-ATTRIBUTE('CURRENT-PAGE').
   iWinKitCurrentPage = INTEGER(RETURN-VALUE).

   IF VALID-OBJECT (oFormControl) THEN
   ASSIGN
       hWinKitFrame   = oFormControl:GetTabPageFrame (iWinKitCurrentPage)
       oWinKitControl = oFormControl:GetTabPageControl (iWinKitCurrentPage)
       iPage1ColDiff = (oWinKitControl:Width - hWinKitFrame:WIDTH-PIXELS) // SESSION:PIXELS-PER-COLUMN
       iPage1RowDiff = (oWinKitControl:Height - hWinKitFrame:HEIGHT-PIXELS) // SESSION:PIXELS-PER-ROW
       .
    IF VALID-HANDLE(h_folder) THEN
    RUN set-size IN h_folder (FRAME {&FRAME-NAME}:HEIGHT - 1,FRAME {&FRAME-NAME}:WIDTH - 1) NO-ERROR.

    {methods/sizeBrowse.i 01}
    {methods/sizeBrowse.i 02}
    {methods/sizeBrowse.i 03}
  &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE winSize Include
PROCEDURE winSize :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opRowDiff AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opColDiff AS DECIMAL NO-UNDO.

  ASSIGN
    opRowDiff = rowDiff
    opColDiff = colDiff.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

