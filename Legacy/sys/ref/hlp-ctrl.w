&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

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
{custom/globdefs.i}

def input param ip-field as cha no-undo.
def input param ip-table as cha no-undo.
def input param ip-value as cha no-undo.
def input param ip-frame as cha no-undo.
def input param ip-language as cha no-undo.
def var is-frame-help as log no-undo.
DEF VAR ll-secure AS LOG NO-UNDO.
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR tmp-dir AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ed-text Btn_OK btn-print btn-update 
&Scoped-Define DISPLAYED-OBJECTS lv-help-title lv-program lv-frame-name ~
ed-text 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-update 
     LABEL "Update Help" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-print 
    LABEL "Print" 
    SIZE 15 BY 1.14.  

DEFINE BUTTON Btn_OK AUTO-END-KEY 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE ed-text AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 300000 SCROLLBAR-VERTICAL
     SIZE 115 BY 18.57
     FONT 0 NO-UNDO.

DEFINE VARIABLE lv-frame-name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 67 BY 1 NO-UNDO.

DEFINE VARIABLE lv-help-title AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 67 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE lv-program AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     lv-help-title AT ROW 1.48 COL 3 COLON-ALIGNED NO-LABEL
     lv-program AT ROW 1.48 COL 72 COLON-ALIGNED NO-LABEL
     lv-frame-name AT ROW 2.67 COL 3 COLON-ALIGNED NO-LABEL
     ed-text AT ROW 3.86 COL 4 NO-LABEL
     Btn_OK AT ROW 22.91 COL 25
     btn-print AT ROW 22.91 COL 55
     btn-update AT ROW 22.91 COL 78
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119.6 BY 23.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Help Information"
         HEIGHT             = 23.38
         WIDTH              = 119.6
         MAX-HEIGHT         = 23.38
         MAX-WIDTH          = 119.6
         VIRTUAL-HEIGHT     = 23.38
         VIRTUAL-WIDTH      = 119.6
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
ASSIGN 
       ed-text:RETURN-INSERTED IN FRAME F-Main  = TRUE
       ed-text:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN lv-frame-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-help-title IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-program IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Help Information */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Help Information */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-update W-Win
ON CHOOSE OF btn-update IN FRAME F-Main /* Update Help */
DO:
    /* need security check */
    def var lv-password as cha no-undo.
    DEF VAR op-ed-text AS cha NO-UNDO.
  /*  
    message "Please Enter Password : " update lv-password.
    if lv-password <> "yorkie" then do:
       message "Security Error.  Contact System Administrator!" view-as alert-box error.
       return .
    end.
  */  

    ASSIGN ll-secure = NO
           op-ed-text = ed-text.

    FIND FIRST users NO-LOCK
     WHERE users.user_id EQ USERID(LDBNAME(1)) NO-ERROR.

IF AVAIL users AND users.securityLevel GE 900 THEN
    ASSIGN ll-secure = YES.

    IF NOT ll-secure THEN RUN sys/ref/uphlp-pass.w (3, OUTPUT ll-secure).

    IF ll-secure EQ YES THEN
    run sys/ref/hlpupd2.p (ip-field,ip-table,ip-value,ip-frame,ip-language,OUTPUT op-ed-text).

/* === re-display    ==========*/
  /*find first asihlp.hlp-head where hlp-head.fld-name = ip-VALUE  and
                                 hlp-head.fil-name = ip-table    
                                 no-lock no-error.
  if not avail hlp-head then do:
     find first asihlp.hlp-head where hlp-head.fld-name = ip-VALUE  no-lock no-error.     
     if avail hlp-head then do:
        ed-text = hlp-head.help-txt.
        display ed-text with frame {&frame-name}.   
     end.  
  end.
  else do:
        ed-text = hlp-head.help-txt.
        display ed-text with frame {&frame-name}.   
  end.*/

    ed-text = op-ed-text.
        display ed-text with frame {&frame-name}. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-print W-Win
ON CHOOSE OF btn-print IN FRAME F-Main /* Update Help */
DO:
    {sys/inc/print1.i}
    {sys/inc/outprint.i value(99)}

    PUT SKIP SPACE(28)
        lv-help-title 
        SKIP
        SPACE(28) lv-program  /*br*/
        SKIP
        SPACE(28) lv-frame-name  /*br*/  .

     PUT SKIP(1)
     ed-text:SCREEN-VALUE FORMAT "x(10000)" .

    RUN custom/prntproc.p (list-name,INT(11),"P").
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
is-frame-help = no.

/* mod - sewa for Web Services task 08211210 */
DEF VAR vconn AS CHAR  NO-UNDO.
DEF VAR vclint AS CHAR NO-UNDO.
DEFINE VARIABLE vhWebService AS HANDLE NO-UNDO.
DEFINE VARIABLE vhSalesSoap AS HANDLE NO-UNDO.
DEFINE VARIABLE parameters1 AS LONGCHAR NO-UNDO.
DEFINE VARIABLE parameters2 AS LONGCHAR NO-UNDO.
DEFINE VARIABLE fr-title  AS CHARACTER NO-UNDO.
DEFINE VARIABLE fr-txt  AS CHARACTER NO-UNDO.
DEFINE VARIABLE fr-fram  AS CHARACTER NO-UNDO.
DEFINE VARIABLE fr-log   AS CHAR NO-UNDO .
DEFINE VARIABLE fr-field  AS CHARACTER NO-UNDO.
DEFINE VARIABLE fr-file  AS CHARACTER NO-UNDO.
DEFINE VARIABLE fr-flags AS CHAR NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.

find first sys-ctrl NO-LOCK
     WHERE sys-ctrl.name    eq "AsiHelpClientID"
       AND sys-ctrl.company EQ g_company  no-error.
  IF AVAIL sys-ctrl THEN
        vclint = sys-ctrl.char-fld  .
  ELSE DO:
      RUN sys/ref/nk1look.p (INPUT g_company, "AsiHelpClientID", "C" /* Logical */, NO /* check by cust */, 
      INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
      OUTPUT cRtnChar, OUTPUT lRecFound).
  END.
  RELEASE sys-ctrl .

find first sys-ctrl NO-LOCK
     WHERE sys-ctrl.name    eq "AsiHelpService"
       AND sys-ctrl.company EQ g_company NO-ERROR.
  IF AVAIL sys-ctrl THEN
      ASSIGN vconn = sys-ctrl.char-fld .
  ELSE do:
      vconn = "".
      RUN sys/ref/nk1look.p (INPUT g_company, "AsiHelpService", "C" /* Logical */, NO /* check by cust */, 
      INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
      OUTPUT cRtnChar, OUTPUT lRecFound).
  END.
  RELEASE sys-ctrl .

      CREATE SERVER vhWebService.
      vhWebService:CONNECT(vconn) NO-ERROR.

IF NOT vhWebService:CONNECTED() THEN
DO:

    find first hlp-head where hlp-head.fld-name = ip-value and
                                 hlp-head.fil-name = ip-table /*and
                                 hlp-head.frm-name = ip-frame */
                                 no-lock no-error.
    if avail hlp-head then do:
        is-frame-help = yes.
    end.

    else do:
        find first hlp-head where hlp-head.fld-name = ip-value no-lock no-error.
        if not avail hlp-head then do:                                /* program-name */
            find first hlp-head where hlp-head.fld-name matches ("*" +  ip-frame + "*")  no-lock no-error.
          if not avail hlp-head then do: 
              ip-frame = replace(ip-frame,".w",".r").
              find first hlp-head where hlp-head.fld-name matches ("*" +  ip-frame + "*")  no-lock no-error.
            if not avail hlp-head then do:
                if ip-table <> "" then message "Help For " string(ip-table) + "." + string(ip-field) ": No Detail Help Information Available!." view-as alert-box error.
                else message "Help For "  string(ip-frame)  ": No Detail Help Information Available!." view-as alert-box error.
                return .
            end.
            else is-frame-help = yes.   
        end.  
        is-frame-help = yes.
      end.
    end.  /* else no ip-frame */

    /*
    find first help-msg where asihlp.help-msg.msg-number = hlp-head.msg-num no-lock no-error.
    */       
    /*          
    message "help: " is-frame-help  ip-field ip-table ip-frame view-as alert-box.
    */

    if is-frame-help then  
        lv-help-title = "Help For " + (if hlp-head.frm-title <> "" then hlp-head.frm-title
                                 else substring(hlp-head.help-txt,1,30) )
                                 + "   " + hlp-head.fil-name + "." + hlp-head.fld-name
                                 .                    
    else if hlp-head.frm-title = "" then 
        lv-help-title = "Help For " + ip-value.
    else lv-help-title = "Help On " + hlp-head.frm-title +  " For " + ip-value.

    lv-frame-name = "Frame Name: " + ip-frame.
    lv-program = "Procedure: " + substring(program-name(2),index(program-name(2)," ")).
    ed-text = hlp-head.help-txt.

END.  /* WebService no conn*/



ELSE DO: /*WebService conn*/

  RUN Service1Soap SET vhSalesSoap ON vhWebService .
  RUN HelpCtrl IN vhSalesSoap(INPUT string(ip-value),INPUT STRING(ip-table),INPUT STRING(ip-frame), INPUT STRING(vclint), OUTPUT parameters1,OUTPUT parameters2,OUTPUT fr-flags).

  ed-text = parameters2.
  fr-field = STRING(entry(1,parameters1)) NO-ERROR.
  fr-file  = STRING(entry(3,parameters1)) NO-ERROR.
  fr-title = STRING(entry(4,parameters1)) NO-ERROR.
  fr-fram  = STRING(entry(2,parameters1)) NO-ERROR.
  fr-log   = STRING(entry(5,parameters1)) NO-ERROR.
  fr-txt = parameters2 .

  IF fr-flags = "No" OR  fr-flags = "" THEN do:
       MESSAGE "Support money not paid, can't access help" VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

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

   lv-frame-name = "Frame Name: " + ip-frame.
   lv-program = "Procedure: " + substring(program-name(2),index(program-name(2)," ")).
END. /* WebService conn*/
     /* mod-sewa*/     
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
  DISPLAY lv-help-title lv-program lv-frame-name ed-text 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE ed-text Btn_OK btn-print btn-update 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .
  
    FIND FIRST users NO-LOCK
     WHERE users.user_id EQ USERID(LDBNAME(1)) NO-ERROR.

    IF AVAIL users AND users.securityLevel LE 999 THEN
     btn-update:VISIBLE IN FRAME f-main = NO.

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

