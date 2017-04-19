&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

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

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
def input param ip-field as cha no-undo.
def input param ip-table as cha no-undo.
def input param ip-value as cha no-undo.
def input param ip-frame as cha no-undo.
def input param ip-language as cha no-undo.
DEF OUTPUT PARAM op-ed-text AS cha NO-UNDO.

def buffer bf-msg for help-msg.
def var ls-line-no as cha no-undo.
def var is-frame-help as log no-undo.
DEFINE VARIABLE fr-msg  AS CHARACTER NO-UNDO.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-updated ed-text Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS lv-help-title lv-program lv-frame-name ~
lv-updated ed-text 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&Save" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE ed-text AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 500000 SCROLLBAR-VERTICAL LARGE
     SIZE 116 BY 19.29
     FONT 0 NO-UNDO.

DEFINE VARIABLE lv-frame-name AS CHARACTER FORMAT "X(256)":U 
     LABEL "Frame-Name" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE lv-help-title AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 67 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE lv-program AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE lv-updated AS LOGICAL INITIAL no 
     LABEL "Don't update via Advantzware Patch" 
     VIEW-AS TOGGLE-BOX
     SIZE 50 BY .81
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     lv-help-title AT ROW 1.48 COL 3 COLON-ALIGNED NO-LABEL
     lv-program AT ROW 1.48 COL 72 COLON-ALIGNED NO-LABEL
     lv-frame-name AT ROW 2.67 COL 16 COLON-ALIGNED
     lv-updated AT ROW 2.91 COL 63
     ed-text AT ROW 3.86 COL 4 NO-LABEL
     Btn_OK AT ROW 23.38 COL 26
     Btn_Cancel AT ROW 23.38 COL 63
     SPACE(43.79) SKIP(0.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Help Information"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

ASSIGN 
       ed-text:RETURN-INSERTED IN FRAME D-Dialog  = TRUE.

/* SETTINGS FOR FILL-IN lv-frame-name IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-help-title IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-program IN FRAME D-Dialog
   NO-ENABLE                                                            */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Help Information */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* Save */
DO:
    DEF VAR lv-dum AS LOG NO-UNDO.
   /* mod - sewa for Web Services task 08211210 */
    DEF VAR vconn AS CHAR  NO-UNDO.
    DEFINE VARIABLE vhWebService AS HANDLE NO-UNDO.
    DEFINE VARIABLE vhSalesSoap AS HANDLE NO-UNDO.
    DEFINE VARIABLE parameters1 AS LONGCHAR NO-UNDO.
    /*mod- sewa*/

    message "Are you sure you want to update Help Information" view-as alert-box question
             button yes-no update ll-ans as log.
    if ll-ans then  DO WITH FRAME {&FRAME-NAME}:
       IF AVAIL asihlp.hlp-head THEN DO:
       def buffer bf-hlp-head for asihlp.hlp-head. 
       find bf-hlp-head where recid(bf-hlp-head) = recid(hlp-head).
  /*     lv-dum = ed-text:SAVE-FILE(bf-hlp-head.help-txt). */

       ASSIGN bf-hlp-head.help-txt = ed-text:SCREEN-VALUE.              
              bf-hlp-head.UPDATEd = lv-updated .
       END.
/* mod - sewa for Web Services task 08211210 */
       ASSIGN op-ed-text = STRING(ed-text:SCREEN-VALUE).

      find first sys-ctrl NO-LOCK
           WHERE sys-ctrl.name    eq "AsiHelpService"
             AND sys-ctrl.company EQ g_company NO-ERROR.
        IF AVAIL sys-ctrl THEN
            ASSIGN vconn = sys-ctrl.char-fld .
        ELSE
            vconn = "".

      CREATE SERVER vhWebService.
      vhWebService:CONNECT(vconn) NO-ERROR.

      IF NOT vhWebService:CONNECTED() THEN
          DO:
          STOP.
      END.

      RUN Service1Soap SET vhSalesSoap ON vhWebService .
      RUN HelpUpdate IN vhSalesSoap(INPUT string(fr-msg),INPUT STRING(ed-text:SCREEN-VALUE),INPUT STRING(lv-updated),  OUTPUT parameters1).

    end.  /*mod-sewa */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ed-text
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ed-text D-Dialog
ON LEAVE OF ed-text IN FRAME D-Dialog
DO:
  /*  
    assign ed-text .
  */  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-updated
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-updated D-Dialog
ON VALUE-CHANGED OF lv-updated IN FRAME D-Dialog /* Don't update via Advantzware Patch */
DO:
   ASSIGN lv-updated.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
/* mod - sewa for Web Services task 08211210 */
DEF VAR vconn AS CHAR  NO-UNDO.
DEFINE VARIABLE vhWebService AS HANDLE NO-UNDO.
DEFINE VARIABLE vhSalesSoap AS HANDLE NO-UNDO.
DEFINE VARIABLE parameters1 AS LONGCHAR NO-UNDO.
DEFINE VARIABLE parameters2 AS LONGCHAR NO-UNDO.
DEFINE VARIABLE parameters3 AS LONGCHAR NO-UNDO.
DEFINE VARIABLE fr-title  AS CHARACTER NO-UNDO.
DEFINE VARIABLE fr-txt  AS CHARACTER NO-UNDO.
DEFINE VARIABLE fr-fram  AS CHARACTER NO-UNDO.
DEFINE VARIABLE fr-log   AS CHAR NO-UNDO .
DEFINE VARIABLE fr-field  AS CHARACTER NO-UNDO.
DEFINE VARIABLE fr-file  AS CHARACTER NO-UNDO.
/*mod-sewa*/

is-frame-help = no.
find first asihlp.hlp-head where asihlp.hlp-head.fld-name = ip-value and
                                 asihlp.hlp-head.fil-name = ip-table /*and
                                 asihlp.hlp-head.frm-name = ip-frame   */
                                 no-lock no-error.
if avail asihlp.hlp-head then is-frame-help = yes.  /* same field but different help by business */
else do:
find first asihlp.hlp-head where asihlp.hlp-head.fld-name = ip-field
                                 no-lock no-error.
if not avail asihlp.hlp-head then do:                                /* program-name */
   find first asihlp.hlp-head where asihlp.hlp-head.fld-name matches ("*" +  ip-frame + "*")  no-lock no-error.
   if not avail asihlp.hlp-head then do: 
      if ip-table <> "" then message "Help For " string(ip-table) + "." + string(ip-field) ": No Detail Help Information Available!." view-as alert-box error.
      else message "Help For "  string(ip-frame)  ": No Detail Help Information Available!." view-as alert-box error.
      return .
   end.  
   is-frame-help = yes.
end.
end.  /* else no ip-frame */
/*
find first help-msg where asihlp.help-msg.msg-number = asihlp.hlp-head.msg-num no-lock no-error.
*/ 

/* mod - sewa for Web Services task 08211210 */
find first sys-ctrl NO-LOCK
     WHERE sys-ctrl.name    eq "AsiHelpService"
       AND sys-ctrl.company EQ g_company NO-ERROR.
  IF AVAIL sys-ctrl THEN
      ASSIGN vconn = sys-ctrl.char-fld .
  ELSE
      vconn = "".

      CREATE SERVER vhWebService.
      vhWebService:CONNECT(vconn) NO-ERROR.

IF NOT vhWebService:CONNECTED() THEN
DO:
    IF AVAIL asihlp.hlp-head THEN do:
    if is-frame-help then  
        lv-help-title = "Help For " + (if asihlp.hlp-head.frm-title <> "" then asihlp.hlp-head.frm-title
                                    else substring(hlp-head.help-txt,1,30) ) 
                                    + "   " + asihlp.hlp-head.fil-name + "." + asihlp.hlp-head.fld-name .

    else if asihlp.hlp-head.frm-title = "" then 
        lv-help-title = "Help For " + ip-value.
    else lv-help-title = "Help On " + asihlp.hlp-head.frm-title +  " For " + ip-value.

    assign ed-text = ""
           ls-line-no = "".
    /*       
    for each bf-msg where bf-msg.msg-number = asihlp.hlp-head.msg-num :
        ed-text = ed-text + bf-msg.msg-txt + chr(13).    
        ls-line-no = ls-line-no + string(bf-msg.msg-line) + ",".
    end. 
    */
    lv-frame-name = ip-frame.
    ASSIGN ed-text = asihlp.hlp-head.help-txt
           lv-updated = asihlp.hlp-head.updated
           op-ed-text = STRING(ed-text).

    END. /* avail hlp-head*/
END. /* WebService not conn */

ELSE DO: /*  WebService conn */

    RUN Service1Soap SET vhSalesSoap ON vhWebService .
    RUN HelpCtrl IN vhSalesSoap(INPUT string(ip-value),INPUT STRING(ip-table),INPUT STRING(ip-frame),INPUT STRING(""),  OUTPUT parameters1,OUTPUT parameters2,OUTPUT parameters3).

    ed-text = parameters2.
    fr-field = STRING(entry(1,parameters1)) NO-ERROR.
    fr-file  = STRING(entry(3,parameters1)) NO-ERROR.
    fr-title = STRING(entry(4,parameters1)) NO-ERROR.
    fr-fram  = STRING(entry(2,parameters1)) NO-ERROR.
    fr-log   = STRING(entry(6,parameters1)) NO-ERROR.
    fr-msg   = STRING(entry(5,parameters1)) NO-ERROR.
    fr-txt = parameters2 .

    IF fr-log = "Yes" THEN
        is-frame-help = YES.
    ELSE
        is-frame-help  = NO.

    if is-frame-help then  
        lv-help-title = "Help For " + (if fr-title <> "" then fr-title
                                 else substring(fr-txt,1,30) )
                                + "   " + ip-table + "." + ip-value .                    
    else if fr-title = "" then 
        lv-help-title = "Help For " + ip-value.
    else lv-help-title = "Help On " + fr-title +  " For " + ip-value.

    lv-frame-name =  ip-frame.
    ASSIGN op-ed-text = STRING(fr-txt).
END.  /* webservices conn*/   /*mod-sewa */

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
  DISPLAY lv-help-title lv-program lv-frame-name lv-updated ed-text 
      WITH FRAME D-Dialog.
  ENABLE lv-updated ed-text Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
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

