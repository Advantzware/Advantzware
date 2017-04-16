&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS F-Frame-Win 
/*------------------------------------------------------------------------

  File: lstframe.w

  Description: from cntnrfrm.w - ADM SmartFrame Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>


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

{methods/defines/hndlset.i}

DEFINE VARIABLE page_no AS INTEGER INITIAL 1 NO-UNDO.
DEFINE VARIABLE listname AS CHARACTER INITIAL "listobjs/prgrms_.w" NO-UNDO.
DEFINE VARIABLE progname AS CHARACTER NO-UNDO.
DEFINE VARIABLE output-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE spooled AS LOGICAL NO-UNDO.
DEFINE VARIABLE quick-print AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Next_Page Btn_Cancel RECT-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_listrqst AS HANDLE NO-UNDO.
DEFINE VARIABLE h_output AS HANDLE NO-UNDO.
DEFINE VARIABLE h_show AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Next_Page 
     LABEL "&Next" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Previous_Page 
     LABEL "&Back" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Shell 
     LABEL "Create &Shell" 
     SIZE 15 BY 1.14.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54.8 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_Shell AT ROW 12.19 COL 2 HELP
          "Create List Logic Shell"
     Btn_Previous_Page AT ROW 12.19 COL 68 HELP
          "Return to Previous Step"
     Btn_Next_Page AT ROW 12.19 COL 84 HELP
          "Continue to Next Step"
     Btn_Cancel AT ROW 12.19 COL 106 HELP
          "CANCEL/CLOSE List Request"
     RECT-1 AT ROW 11.95 COL 67
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 122 BY 12.57.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Design Page: 3
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW F-Frame-Win ASSIGN
         HEIGHT             = 12.57
         WIDTH              = 122.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB F-Frame-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}
{methods/enhance.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW F-Frame-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE                                                          */
/* SETTINGS FOR BUTTON Btn_Previous_Page IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Shell IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Shell:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel F-Frame-Win
ON CHOOSE OF Btn_Cancel IN FRAME F-Main /* Cancel */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "local-exit"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Next_Page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Next_Page F-Frame-Win
ON CHOOSE OF Btn_Next_Page IN FRAME F-Main /* Next */
DO:
  page_no = page_no + 1.
  RUN Change-Page.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Previous_Page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Previous_Page F-Frame-Win
ON CHOOSE OF Btn_Previous_Page IN FRAME F-Main /* Back */
DO:
  page_no = page_no - 1.
  RUN Change-Page.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Shell
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Shell F-Frame-Win
ON CHOOSE OF Btn_Shell IN FRAME F-Main /* Create Shell */
DO:
  RUN Create-List-Logic-Shell IN h_listrqst (progname).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK F-Frame-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN dispatch IN THIS-PROCEDURE ('initialize':U).
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects F-Frame-Win  _ADM-CREATE-OBJECTS
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

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
           &IF DEFINED(UIB_is_Running) ne 0 &THEN
             INPUT  'listobjs/prgrms_.w':U ,
           &ELSE
             INPUT listname ,
           &ENDIF
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_listrqst ).
       RUN set-position IN h_listrqst ( 1.00 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 10.95 , 120.00 ) */

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/show.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_show ).
       RUN set-position IN h_show ( 1.95 , 13.00 ) NO-ERROR.
       /* Size in UIB:  ( 8.81 , 97.00 ) */

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/output.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_output ).
       RUN set-position IN h_output ( 3.38 , 24.00 ) NO-ERROR.
       /* Size in UIB:  ( 5.95 , 76.00 ) */

       /* Adjust the tab order of the smart objects. */
    END. /* Page 3 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available F-Frame-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Change-Page F-Frame-Win 
PROCEDURE Change-Page :
/*------------------------------------------------------------------------------
  Purpose:     Change Page
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE output-where AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    CASE page_no:
      WHEN 1 THEN
      DISABLE Btn_Previous_Page.
      WHEN 2 THEN
      DO:
        ENABLE Btn_Previous_Page.
        Btn_Next_Page:LABEL = "&Next".
      END.
      WHEN 3 THEN
      Btn_Next_Page:LABEL = "&Finish".
      WHEN 4 THEN /* finish */
      DO:
        RUN Output-Where IN h_output (OUTPUT output-where,OUTPUT spooled).
        {methods/wait.i}
        IF quick-print THEN
        output-where = 1.
        CASE output-where:
          WHEN 1 THEN
          RUN Output-to-Printer.
          WHEN 2 THEN
          RUN Output-to-Screen.
          WHEN 3 THEN
          RUN Output-to-File.
          WHEN 4 THEN
          RUN Output-to-Viper.
        END CASE.
        {methods/nowait.i}
        IF RETURN-VALUE = "CANCEL" THEN
        DO:
          page_no = 3.
          RETURN.
        END.
        ASSIGN
          Btn_Next_Page:LABEL = "&Next"
          page_no = 1.
        DISABLE Btn_Previous_Page.
      END.
    END CASE.
    RUN SELECT-PAGE (page_no).
    IF page_no = 3 THEN
    APPLY "ENTRY" TO Btn_Next_Page.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI F-Frame-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI F-Frame-Win  _DEFAULT-ENABLE
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
  ENABLE Btn_Next_Page Btn_Cancel RECT-1 
      WITH FRAME F-Main.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Export-Values F-Frame-Win 
PROCEDURE Export-Values :
/*------------------------------------------------------------------------------
  Purpose:     Export Screen Values
  Parameters:  smart object handle
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER shandle AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE fhandle AS WIDGET-HANDLE NO-UNDO.

  OUTPUT TO VALUE(output-name) APPEND.
  RUN Frame-Handle IN shandle (OUTPUT fhandle).
  ASSIGN
    current-widget = fhandle
    current-widget = current-widget:FIRST-CHILD
    current-widget = current-widget:FIRST-CHILD.

  DO WHILE current-widget NE ?:
    IF CAN-DO("export,save",current-widget:PRIVATE-DATA) THEN
    EXPORT current-widget:SCREEN-VALUE.

    current-widget = current-widget:NEXT-SIBLING.
  END.
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init-Show-Parameters F-Frame-Win 
PROCEDURE Init-Show-Parameters :
/*------------------------------------------------------------------------------
  Purpose:     Act as go between for h_show and h_listrqst
  Parameters:  OUTPUT show parameter settings.
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER notes-init AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER miscflds-init AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER addresses-init AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER phones-init AS LOGICAL NO-UNDO.

  RUN Init-Show-Parameters IN h_listrqst
      (OUTPUT notes-init,
       OUTPUT miscflds-init,
       OUTPUT addresses-init,
       OUTPUT phones-init).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize F-Frame-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {methods/run_link.i "CONTAINER-SOURCE" "List-Name" "(OUTPUT listname)"}
  {methods/run_link.i "CONTAINER-SOURCE" "Program-Name" "(OUTPUT progname)"}
  IF {methods/chkdevid.i} THEN
  DO:
    Btn_Shell:VISIBLE IN FRAME {&FRAME-NAME} = YES.
    ENABLE Btn_Shell WITH FRAME {&FRAME-NAME}.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Output-to-File F-Frame-Win 
PROCEDURE Output-to-File :
/*------------------------------------------------------------------------------
  Purpose:     Request Listing to Disk File
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
  DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

  {methods/run_link.i "CONTAINER-SOURCE" "Output-Name" "(OUTPUT output-name)"}
  IF NOT VALID-HANDLE(adm-broker-hdl) THEN
  output-name = "Program Master List.rpt".
  init-dir = "users\" + USERID("NOSWEAT").
  SYSTEM-DIALOG GET-FILE output-name
      TITLE      "Choose Listing to SAVE ..."
      FILTERS    "Listing Files (*.rpt)" "*.rpt"
      INITIAL-DIR init-dir
      ASK-OVERWRITE
      USE-FILENAME
      UPDATE OKpressed.
  IF NOT OKpressed THEN
  RETURN "CANCEL".
  OUTPUT TO VALUE(output-name).
  OUTPUT CLOSE.
  RUN Process-Selections.
  RUN Run-Listing.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Output-to-Printer F-Frame-Win 
PROCEDURE Output-to-Printer :
/*------------------------------------------------------------------------------
  Purpose:     Request Listing to Printer
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
  DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(130)" NO-UNDO.
  DEFINE VARIABLE result    AS LOGICAL NO-UNDO.

  {methods/run_link.i "CONTAINER-SOURCE" "Output-Name" "(OUTPUT output-name)"}
  IF NOT VALID-HANDLE(adm-broker-hdl) THEN
  output-name = "Program Master List.rpt".
  IF NOT quick-print THEN
  DO:
    FONT-TABLE:NUM-ENTRIES = 20.
    SYSTEM-DIALOG FONT 10 FIXED-ONLY.
    SYSTEM-DIALOG PRINTER-SETUP UPDATE OKpressed.
    IF NOT OKpressed THEN
    RETURN "CANCEL".
  END.
  FIND FIRST config NO-LOCK.
  output-name = IF NOT spooled THEN "users~/" + USERID("NOSWEAT") + "~/" + output-name
                ELSE config.spool_dir + "~/" + output-name.
  OUTPUT TO VALUE(output-name).
  OUTPUT CLOSE.
  RUN Process-Selections.
  IF spooled THEN
  DO:
    output-name = REPLACE(output-name,".rpt",".spl").
    OUTPUT TO VALUE(output-name).
    EXPORT
      REPLACE(REPLACE(output-name,".spl",""),config.spool_dir + "/","")
      USERID("NOSWEAT")
      progname
      SESSION:PRINTER-CONTROL-HANDLE.
    OUTPUT CLOSE.
    RUN Export-Values (h_output).
    MESSAGE "Spool Request Submitted!" VIEW-AS ALERT-BOX INFORMATION.
    RETURN.
  END.
  RUN Run-Listing.
  IF RETURN-VALUE = "MISSING" THEN
  RETURN.

  /* Use Progress Print. Always use Font#10 in Registry (set above) */
  RUN 'adecomm/_osprint.p' (INPUT ?, INPUT output-name,
                            INPUT 10, INPUT 0, INPUT 0, INPUT 0, OUTPUT result).
/*
  OUTPUT TO PRINTER.
  INPUT FROM VALUE(output-name) NO-ECHO.
  REPEAT:
    IMPORT UNFORMATTED list-text.
    IF ASC(SUBSTR(list-text,1,1)) = 12 THEN
    DO:
      PAGE.
      list-text = SUBSTR(list-text,2).
    END.
    IF list-text NE "" THEN
    PUT UNFORMATTED list-text SKIP.
    ELSE
    PUT UNFORMATTED SKIP(1).
    list-text = "".
  END.
  INPUT CLOSE.
  OUTPUT CLOSE.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Output-to-Screen F-Frame-Win 
PROCEDURE Output-to-Screen :
/*------------------------------------------------------------------------------
  Purpose:     Request Listing to Screen
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/run_link.i "CONTAINER-SOURCE" "Output-Name" "(OUTPUT output-name)"}
  output-name = "users~/" + USERID("NOSWEAT") + "~/" + output-name.
  IF NOT VALID-HANDLE(adm-broker-hdl) THEN
  output-name = "users~/" + USERID("NOSWEAT") + "~/Program Master List.rpt".
  OUTPUT TO VALUE(output-name).
  OUTPUT CLOSE.
  RUN Process-Selections.
  RUN Run-Listing.
  IF RETURN-VALUE = "MISSING" THEN
  RETURN.
  {methods/run_link.i "CONTAINER-SOURCE" "Output-Name" "(OUTPUT output-name)"}
  IF NOT VALID-HANDLE(adm-broker-hdl) THEN
  output-name = "Program Master List.rpt".
  RUN Get_Procedure IN Persistent-Handle ("scr_view.",OUTPUT run-proc,no).

  IF run-proc NE "" THEN
  {methods/smartrun.i (output-name)}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Output-to-Viper F-Frame-Win 
PROCEDURE Output-to-Viper :
/*------------------------------------------------------------------------------
  Purpose:     Request Listing to Screen
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/run_link.i "CONTAINER-SOURCE" "Output-Name" "(OUTPUT output-name)"}
  output-name = "users~/" + USERID("NOSWEAT") + "~/" + output-name.
  IF NOT VALID-HANDLE(adm-broker-hdl) THEN
  output-name = "users~/" + USERID("NOSWEAT") + "~/Program Master List.rpt".
  OUTPUT TO VALUE(output-name).
  OUTPUT CLOSE.
  RUN Process-Selections.
  RUN Run-Listing.
  IF RETURN-VALUE = "MISSING" THEN
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Process-Selections F-Frame-Win 
PROCEDURE Process-Selections :
/*------------------------------------------------------------------------------
  Purpose:     Process Selections on each screen.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN Export-Values (h_output).
  RUN Export-Values (h_show).
  RUN Export-Values (h_listrqst).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Quick-Print F-Frame-Win 
PROCEDURE Quick-Print :
/*------------------------------------------------------------------------------
  Purpose:     Quick Print Button pressed in smart-object output.w
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN SELECT-PAGE (2).
  RUN SELECT-PAGE (3).
  ASSIGN
    quick-print = yes
    page_no = 4.
  RUN Change-Page.
  quick-print = no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Run-Listing F-Frame-Win 
PROCEDURE Run-Listing :
/*------------------------------------------------------------------------------
  Purpose:     Run Listing Logic
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF SEARCH("lstlogic/" + progname + "p") = ? AND
     SEARCH("lstlogic/" + progname + "r") = ? THEN
  DO:
    MESSAGE "List Logic 'lstlogic/" + progname + "' Missing!"
        VIEW-AS ALERT-BOX ERROR.
    RETURN "MISSING".
  END.
  RUN VALUE("lstlogic/" + progname + "p") (output-name).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records F-Frame-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartFrame, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus F-Frame-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     Set Focus called by initiating program
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/setfocus.i Btn_Next_Page}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed F-Frame-Win 
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

