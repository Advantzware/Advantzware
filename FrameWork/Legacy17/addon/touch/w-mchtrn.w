&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: addon/touch/w-mchtrn.w

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
DEFINE INPUT PARAMETER h_calling_window AS WIDGET-HANDLE NO-UNDO.
&ELSE
DEFINE VARIABLE h_calling_window AS WIDGET-HANDLE NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE company_code AS CHARACTER NO-UNDO.
DEFINE VARIABLE machine_code AS CHARACTER NO-UNDO.
DEFINE VARIABLE job_number AS CHARACTER NO-UNDO.
DEFINE VARIABLE job_sub AS CHARACTER NO-UNDO.
DEFINE VARIABLE form_number AS CHARACTER NO-UNDO.
DEFINE VARIABLE blank_number AS CHARACTER NO-UNDO.
DEFINE VARIABLE pass_sequence AS CHARACTER NO-UNDO.
DEFINE VARIABLE label_language AS CHARACTER NO-UNDO.

IF VALID-HANDLE(h_calling_window) THEN DO:
  RUN Get_Value IN h_calling_window ('company_code':U,OUTPUT company_code).
  RUN Get_Value IN h_calling_window ('machine_code':U,OUTPUT machine_code).
  RUN Get_Value IN h_calling_window ('job_number':U,OUTPUT job_number).
  RUN Get_Value IN h_calling_window ('job_sub':U,OUTPUT job_sub).
  RUN Get_Value IN h_calling_window ('form_number':U,OUTPUT form_number).
  RUN Get_Value IN h_calling_window ('blank_number':U,OUTPUT blank_number).
  RUN Get_Value IN h_calling_window ('pass_sequence':U,OUTPUT pass_sequence).
  RUN Get_Value IN h_calling_window ('label_language':U,OUTPUT label_language).
END.

&Scoped-define ENHANCE no

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
&Scoped-define EXTERNAL-TABLES machtran
&Scoped-define FIRST-EXTERNAL-TABLE machtran


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR machtran.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_attach AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-mchtr2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_machtran AS HANDLE NO-UNDO.
DEFINE VARIABLE h_miscfldsad AS HANDLE NO-UNDO.
DEFINE VARIABLE h_optnote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_optonote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_optreljob AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-tchupd AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updca2-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119.2 BY 11.76
         BGCOLOR 15 .

DEFINE FRAME message-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 46 ROW 1
         SIZE 56 BY 1.91
         BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: machtran
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
         TITLE              = "Machine Transactions"
         COLUMN             = 32.6
         ROW                = 16.62
         HEIGHT             = 11.76
         WIDTH              = 119.2
         MAX-HEIGHT         = 11.76
         MAX-WIDTH          = 119.2
         VIRTUAL-HEIGHT     = 11.76
         VIRTUAL-WIDTH      = 119.2
         RESIZE             = NO
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}
{methods/template/windows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME message-frame:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME message-frame
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





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Machine Transactions */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Machine Transactions */
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

&SCOPED-DEFINE sysCtrlCompany company_code
&SCOPED-DEFINE sysCtrlName MiscJobCL
&SCOPED-DEFINE mfRecKey company_code + "|jh" + STRING(job-hdr.j-no)
&SCOPED-DEFINE mfHeader " Job: " + job-hdr.job-no + "-" + STRING(job-hdr.job-no2) + " Item#: " + job-hdr.i-no
{methods/miscfldsad.i}

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
             INPUT  'smartobj/smartmsg.w':U ,
             INPUT  FRAME message-frame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_smartmsg ).
       RUN set-position IN h_smartmsg ( 1.00 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/optreljob.w':U ,
             INPUT  FRAME message-frame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_optreljob ).
       RUN set-position IN h_optreljob ( 1.00 , 32.60 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/attach.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_attach ).
       RUN set-position IN h_attach ( 1.00 , 103.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/optnote.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_optnote ).
       RUN set-position IN h_optnote ( 1.00 , 111.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/optonote.w':U ,
             INPUT  FRAME message-frame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_optonote ).
       RUN set-position IN h_optonote ( 1.00 , 41.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/miscfldsad.w':U ,
             INPUT  FRAME message-frame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_miscfldsad ).
       RUN set-position IN h_miscfldsad ( 1.00 , 49.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Browse Trans|View Tran' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 1.71 , 2.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 10.95 , 117.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartObject h_attach. */
       RUN add-link IN adm-broker-hdl ( h_b-mchtr2 , 'attach':U , h_attach ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_optreljob ,
             h_smartmsg , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_attach ,
             FRAME message-frame:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_optnote ,
             h_attach , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_optonote ,
             h_optreljob , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_miscfldsad ,
             h_optonote , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             h_optnote , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'addon/touch/b-mchtr2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-mchtr2 ).
       RUN set-position IN h_b-mchtr2 ( 3.14 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 7.86 , 115.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updca2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updca2-2 ).
       RUN set-position IN h_p-updca2-2 ( 11.00 , 40.00 ) NO-ERROR.
       RUN set-size IN h_p-updca2-2 ( 1.43 , 38.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartNavBrowser h_b-mchtr2. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_b-mchtr2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updca2-2 , 'TableIO':U , h_b-mchtr2 ).
       RUN add-link IN adm-broker-hdl ( h_b-mchtr2 , 'Record':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-mchtr2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updca2-2 ,
             h_b-mchtr2 , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/machtran.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_machtran ).
       RUN set-position IN h_machtran ( 3.14 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 7.38 , 115.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 10.52 , 3.00 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 1.91 , 38.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-tchupd.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-tchupd ).
       RUN set-position IN h_p-tchupd ( 10.52 , 59.00 ) NO-ERROR.
       RUN set-size IN h_p-tchupd ( 1.76 , 59.20 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_machtran. */
       RUN add-link IN adm-broker-hdl ( h_b-mchtr2 , 'Record':U , h_machtran ).
       RUN add-link IN adm-broker-hdl ( h_p-tchupd , 'TableIO':U , h_machtran ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_machtran ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico ,
             h_machtran , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-tchupd ,
             h_p-navico , 'AFTER':U ).
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
  {src/adm/template/row-list.i "machtran"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "machtran"}

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
  VIEW FRAME message-frame IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-message-frame}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-Values W-Win 
PROCEDURE Get-Values :
/*------------------------------------------------------------------------------
  Purpose:     pass values to calling browser
  Parameters:  output company,machine,job,sub,form,blank,pass
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-company_code AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER op-machine_code AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER op-job_number AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER op-job_sub AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER op-form_number AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER op-blank_number AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER op-pass_sequence AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER op-label_language AS CHARACTER NO-UNDO.

  ASSIGN
    op-company_code = company_code
    op-machine_code = machine_code
    op-job_number = job_number
    op-job_sub = job_sub
    op-form_number = form_number
    op-blank_number = blank_number
    op-pass_sequence = pass_sequence
    op-label_language = label_language.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Label_Language W-Win 
PROCEDURE Label_Language :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER current-frame AS WIDGET-HANDLE NO-UNDO.

  RUN Label_Language IN h_calling_window (current-frame).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectMiscFlds W-Win 
PROCEDURE selectMiscFlds :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN sysCtrlMiscFlds (OUTPUT miscFlds).
  IF NOT miscFlds THEN RETURN.

  IF AVAIL machtran AND machtran.job_number <> "" THEN
  DO:
     FIND FIRST job-hdr WHERE
          job-hdr.company EQ machtran.company AND
          job-hdr.job-no = machtran.job_number AND
          job-hdr.job-no2 = machtran.job_sub AND
          job-hdr.frm = machtran.form_number AND
          job-hdr.blank-no = machtran.blank_number
          NO-LOCK NO-ERROR.

     IF NOT AVAIL job-hdr THEN
        FIND FIRST job-hdr WHERE
             job-hdr.company = machtran.company AND
             job-hdr.job-no = machtran.job_number AND
             job-hdr.job-no2 = machtran.job_sub AND
             job-hdr.frm = machtran.form_number
             NO-LOCK NO-ERROR.

     IF AVAIL job-hdr THEN
        RUN nosweat/mfvalad.p (sys-ctrl.char-fld,{&mfRecKey},{&mfHeader},h_smartmsg).
  END.
  ELSE
  DO:
     FIND FIRST job-hdr WHERE
          job-hdr.company EQ company_code AND
          job-hdr.job-no = job_number AND
          job-hdr.job-no2 = INT(job_sub) AND
          job-hdr.frm = INT(form_number) AND
          job-hdr.blank-no = INT(blank_number)
          NO-LOCK NO-ERROR.

     IF NOT AVAIL job-hdr THEN
        FIND FIRST job-hdr WHERE
             job-hdr.company EQ company_code AND
             job-hdr.job-no = job_number AND
             job-hdr.job-no2 = INT(job_sub) AND
             job-hdr.frm = INT(form_number)
             NO-LOCK NO-ERROR.

     IF AVAIL job-hdr THEN
        RUN nosweat/mfvalad.p (sys-ctrl.char-fld,{&mfRecKey},{&mfHeader},h_smartmsg).
  END.

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
  DEF VAR adm-current-page AS INT NO-UNDO.

  IF NOT AVAIL machtran THEN RETURN.

  rec_key_value = machtran.rec_key.

  {methods/select_att.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_dept2 W-Win 
PROCEDURE select_dept2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*IF NOT AVAIL machtran  THEN RETURN.*/

  /*DEF BUFFER bf-mtran FOR machtran.*/

  IF AVAIL machtran AND machtran.job_number <> "" THEN DO:     

     /*FIND bf-mtran WHERE RECID(bf-mtran) = RECID(machtran) EXCLUSIVE-LOCK.*/
     FIND FIRST job WHERE job.company EQ machtran.company AND
                          job.job-no = machtran.job_number AND
                          job.job-no2 = machtran.job_sub NO-LOCK NO-ERROR.

     /*IF AVAIL job THEN bf-mtran.rec_key = job.rec_key.*/

     /*rec_key_value = machtran.rec_key.*/
     rec_key_value = job.rec_key.

     /*RUN windows/specnote.w (rec_key_value,HEADER_value).  */
     RUN windows/specnott.w (rec_key_value,HEADER_value,machine_code,ForM_number).

  END.
  ELSE IF NOT AVAIL machtran THEN
  DO:
     FIND FIRST job WHERE
          job.company EQ company_code AND
          job.job-no = job_number AND
          job.job-no2 = INT(job_sub)
          NO-LOCK NO-ERROR.

     IF AVAIL job THEN
     DO:        
        RUN windows/specnott.w (job.rec_key,header_value,machine_code,form_number).       
     END.
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_jobrel W-Win 
PROCEDURE select_jobrel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

     FIND FIRST job WHERE
          job.company EQ company_code AND
          job.job-no = job_number AND
          job.job-no2 = INT(job_sub)
          NO-LOCK NO-ERROR.

     IF AVAIL job THEN
     DO:
       rec_key_value = job.rec_key.             
       RUN jcrep/r-tickt2.w (job_number, int(job_sub)) NO-ERROR.
     END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_ONote W-Win 
PROCEDURE select_ONote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cJobKey AS CHAR NO-UNDO.

  IF AVAIL machtran AND machtran.job_number <> "" THEN DO:
     FIND FIRST job WHERE job.company EQ machtran.company AND
                          job.job-no = machtran.job_number AND
                          job.job-no2 = machtran.job_sub NO-LOCK NO-ERROR.


     ASSIGN cJobKey = job.company + string(job.job,"9999999") /*job.rec_key*/ 
            rec_key_value = job.rec_key
            HEADER_value =  STRING(job.job) /*job.job-no + STRING(job.job-no2)*/.     

     {methods/select_ONote.i rec_key_value header_value machine_code form_number}


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
  {src/adm/template/snd-list.i "machtran"}

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

