&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: sys/ref/hlpd.w

  Description: 

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 5.10.2018 (converted from a dialog to a window)

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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
def input param ip-db as cha no-undo.
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

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ed-text Btn_OK btn-print btn-update ~
btDataDigger 
&Scoped-Define DISPLAYED-OBJECTS lv-help-title lv-program lv-frame-name ~
ed-text 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-btDataDigger 
       MENU-ITEM m_ProTools2    LABEL "ProTools"      
       MENU-ITEM m_Program_Stack LABEL "Program Stack" 
       MENU-ITEM m_Trans        LABEL "Transactions and Locks Window".


/* Definitions of the field level widgets                               */
DEFINE BUTTON btDataDigger 
     LABEL "Admin" 
     SIZE 11 BY 1.14.

DEFINE BUTTON btn-print 
     LABEL "Print" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-update 
     LABEL "Update Help" 
     SIZE 18 BY 1.14.

DEFINE BUTTON Btn_OK 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE ed-text AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 500000 SCROLLBAR-VERTICAL
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
     SIZE 47 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     lv-help-title AT ROW 1.24 COL 2 NO-LABEL WIDGET-ID 14
     lv-program AT ROW 1.24 COL 68 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     lv-frame-name AT ROW 2.43 COL 2 NO-LABEL WIDGET-ID 12
     ed-text AT ROW 3.62 COL 2 NO-LABEL WIDGET-ID 10
     Btn_OK AT ROW 22.43 COL 55 WIDGET-ID 8
     btn-print AT ROW 22.43 COL 71 WIDGET-ID 4
     btn-update AT ROW 22.43 COL 87 WIDGET-ID 6
     btDataDigger AT ROW 22.43 COL 106 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 117.8 BY 22.86 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Help Information"
         HEIGHT             = 22.86
         WIDTH              = 117.8
         MAX-HEIGHT         = 22.86
         MAX-WIDTH          = 117.8
         VIRTUAL-HEIGHT     = 22.86
         VIRTUAL-WIDTH      = 117.8
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       btDataDigger:POPUP-MENU IN FRAME DEFAULT-FRAME       = MENU POPUP-MENU-btDataDigger:HANDLE.

ASSIGN 
       ed-text:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE
       ed-text:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lv-frame-name IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lv-help-title IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lv-program IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Help Information */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Help Information */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDataDigger
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDataDigger C-Win
ON CHOOSE OF btDataDigger IN FRAME DEFAULT-FRAME /* Admin */
DO:
    RUN datadigger/datadigger.r.
    /*
    MESSAGE 
        "Datadigger is replaced with the query tool on the advantzware menu."
        VIEW-AS ALERT-BOX.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-print C-Win
ON CHOOSE OF btn-print IN FRAME DEFAULT-FRAME /* Print */
DO:
    {sys/inc/print1.i}
    {sys/inc/outprint.i value(99)}

    PUT SKIP SPACE(28)
        lv-help-title 
        SKIP
        SPACE(28) lv-program  /*br*/
        SKIP
        SPACE(28) lv-frame-name  /*br*/ .

    PUT SKIP(1)
        ed-text:SCREEN-VALUE FORMAT "x(10000)" .

     RUN custom/prntproc.p (list-name,INT(11),"P").
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-update C-Win
ON CHOOSE OF btn-update IN FRAME DEFAULT-FRAME /* Update Help */
DO:
    /* need security check */
    def var lv-password as cha no-undo.
    DEF VAR op-ed-text AS cha NO-UNDO.
    DEFINE VARIABLE hPgmSecurity AS HANDLE NO-UNDO.
    DEFINE VARIABLE lResult AS LOG NO-UNDO.

    ASSIGN op-ed-text = ed-text.

    /* need security check */
    RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
    RUN epCanAccess IN hPgmSecurity ("sys/ref/hlp.w", "Access1", OUTPUT lResult).
    DELETE OBJECT hPgmSecurity.
    
    /* If not automatically cleared by security level, ask for password */
    IF lResult THEN DO:
        run sys/ref/hlpupd.w (ip-field,ip-table,ip-db,ip-frame,ip-language,OUTPUT op-ed-text).
    END.

        ed-text = op-ed-text.
        display ed-text with frame {&frame-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_ProTools2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_ProTools2 C-Win
ON CHOOSE OF MENU-ITEM m_ProTools2 /* ProTools */
DO:
  RUN protools\_protool.w.
END.

&Scoped-define SELF-NAME m_Trans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Trans Dialog-Frame
ON CHOOSE OF MENU-ITEM m_Trans /* Transactions and Locks Window */
DO:
  RUN util/wlockmon.r.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
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
DEFINE VARIABLE fr-field  AS CHARACTER NO-UNDO.
DEFINE VARIABLE fr-file  AS CHARACTER NO-UNDO.
DEFINE VARIABLE fr-flags AS CHAR NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.

find first sys-ctrl NO-LOCK
     WHERE sys-ctrl.name    eq "AsiHelpClientID"
       AND sys-ctrl.company EQ g_company NO-ERROR .
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

    MESSAGE "Unable to connect to the Advantzware Documentation/Help Server." SKIP
            "Please report the issue to Advantzware support" SKIP
            "AsiHelpService:" + STRING(vconn) 
         VIEW-AS ALERT-BOX INFO .

     find first hlp-head where hlp-head.fld-name = ip-field and
                                 hlp-head.fil-name = ip-table and
                                 hlp-head.frm-name = ip-frame
                                 no-lock no-error.
    if avail hlp-head then is-frame-help = yes.
    else do:
    find first hlp-head where hlp-head.fld-name = ip-field and
                                     hlp-head.fil-name = ip-table
                                     no-lock no-error.
    if not avail hlp-head then do: /* program-name */    
       find first hlp-head where hlp-head.fld-name = ip-field NO-LOCK NO-ERROR.
       if not avail hlp-head then do:
           find first hlp-head where hlp-head.fld-name matches ("*" +  ip-frame + "*")  no-lock no-error.
           if not avail hlp-head then do: 
              ip-frame = replace(ip-frame,".w",".r").
              find first hlp-head where hlp-head.fld-name matches ("*" +  ip-frame + "*")  no-lock no-error.
              if not avail hlp-head then do:           
                    if ip-table <> "" then message "Help For " string(ip-table) + "." + string(ip-field) ": No Detail Help Information Available!." view-as alert-box error.
                    else message "Help For "  string(ip-frame)  ": No Detail Help Information Available!." view-as alert-box error.
                    return .                
              end.
           end.  
        END.
        is-frame-help = yes.
     end.
  end.  /* else no ip-frame */

    if is-frame-help then  
        lv-help-title = "Help For " + (if hlp-head.frm-title <> "" then hlp-head.frm-title
                                 else substring(hlp-head.help-txt,1,30) )
                                 + "   " + hlp-head.fil-name + "." + hlp-head.fld-name
                                 .                    
    else if hlp-head.frm-title = "" then 
        lv-help-title = "Help For " + ip-db + "." + ip-table + "." + ip-field.
    else lv-help-title = "Help On " + hlp-head.frm-title +  " For " +
                    ip-db + "." + ip-table + "." + ip-field.
    lv-frame-name = "Frame Name: " + ip-frame.
    lv-program = "Procedure: " + substring(program-name(2),index(program-name(2)," ")).
    ed-text = hlp-head.help-txt.

END. /* WebService no conn*/

 ELSE DO:  /* WebService conn*/ 
     IF ip-table EQ ? THEN ip-table = "" .
     IF ip-field EQ ? THEN ip-field = "" .
     IF ip-frame EQ ? THEN ip-frame = "" . 
    RUN Service1Soap SET vhSalesSoap ON vhWebService .
    RUN HelpMain IN vhSalesSoap(INPUT string(ip-field),INPUT STRING(ip-table),INPUT STRING(ip-frame), INPUT STRING(vclint),  OUTPUT parameters1,OUTPUT parameters2,OUTPUT fr-flags).

    ed-text = parameters2.
    fr-field = STRING(entry(1,parameters1)) NO-ERROR.
    fr-file  = STRING(entry(3,parameters1)) NO-ERROR.
    fr-title = STRING(entry(4,parameters1)) NO-ERROR.
    fr-fram  = STRING(entry(2,parameters1)) NO-ERROR.
    fr-txt = parameters2 .

    IF fr-flags = "No" OR  fr-flags = "" THEN do:
       MESSAGE "Support money not paid, can't access help" VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    IF ip-field <> "" AND ip-table <> "" AND fr-fram = "" THEN
        is-frame-help = NO.
    ELSE is-frame-help = YES.

         if is-frame-help then  
             lv-help-title = "Help For " + (if fr-title <> "" then fr-title
                 else substring(fr-txt,1,30) )
                 + "   " + fr-file + "." + fr-field .                    

                 else if fr-title = "" then 
                     lv-help-title = "Help For " + ip-db + "." + ip-table + "." + ip-field.
                 ELSE lv-help-title = "Help On " + fr-title +  " For " +
                      ip-db + "." + ip-table + "." + ip-field .
                      
                 lv-frame-name = "Frame Name: " + ip-frame.
                 lv-program = "Procedure: " + substring(program-name(2),index(program-name(2)," ")).
    END.  /* WebService is conn*/     /*mod-sewa  */
          
  RUN enable_UI.

    DEF VAR hPgmSecurity AS HANDLE NO-UNDO.
    DEF VAR lResult AS LOG NO-UNDO.
    RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
    RUN epCanAccess IN hPgmSecurity ("sys/ref/hlp.w", "Access2", OUTPUT lResult).
    DELETE OBJECT hPgmSecurity.
    IF NOT lResult THEN ASSIGN
        btn-update:VISIBLE IN FRAME {&frame-name} = NO.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE ed-text Btn_OK btn-print btn-update btDataDigger 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

