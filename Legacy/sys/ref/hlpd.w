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

DEF INPUT PARAMETER ip-field as cha no-undo.
DEF INPUT PARAMETER ip-table as cha no-undo.
DEF INPUT PARAMETER ip-db as cha no-undo.
DEF INPUT PARAMETER ip-frame as cha no-undo.
DEF INPUT PARAMETER ip-language as cha no-undo.
DEF VAR is-frame-help as log no-undo.
DEF VAR ll-secure AS LOG NO-UNDO.
DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHARACTER NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS ed-text btProTools btLockMon btDataDigger ~
btUpdateHelp btPrint BtOK 
&Scoped-Define DISPLAYED-OBJECTS lv-help-title lv-program lv-frame-name ~
ed-text 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btDataDigger 
     LABEL "DataDigger" 
     SIZE 14 BY 1.14.

DEFINE BUTTON btLockMon 
     LABEL "Lock Monitor" 
     SIZE 14 BY 1.14.

DEFINE BUTTON BtOK 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btPrint 
     LABEL "Print" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btProTools 
     LABEL "ProTools" 
     SIZE 14 BY 1.14.

DEFINE BUTTON btUpdateHelp 
     LABEL "Update Help" 
     SIZE 18 BY 1.14.

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
     btProTools AT ROW 22.43 COL 2 WIDGET-ID 2
     btLockMon AT ROW 22.43 COL 17 WIDGET-ID 18
     btDataDigger AT ROW 22.43 COL 32 WIDGET-ID 20
     btUpdateHelp AT ROW 22.43 COL 47 WIDGET-ID 6
     btPrint AT ROW 22.43 COL 86 WIDGET-ID 4
     BtOK AT ROW 22.43 COL 102 WIDGET-ID 8
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
ON CHOOSE OF btDataDigger IN FRAME DEFAULT-FRAME /* DataDigger */
OR CHOOSE OF btLockMon
OR CHOOSE of btOK
OR CHOOSE OF btPrint
OR CHOOSE OF btProTools
OR CHOOSE OF btUpdateHelp
DO:

    DEF VAR op-ed-text AS CHAR NO-UNDO.

    CASE SELF:NAME:
        WHEN "btDataDigger" THEN
            RUN datadigger/datadigger.r.
        WHEN "btLockMon" THEN
            RUN util/wlockmon.r.
        WHEN "btOK" THEN DO:
            APPLY "CLOSE":U TO THIS-PROCEDURE.
        END.
        WHEN "btPrint" THEN DO:
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
        WHEN "btProTools" THEN.
        WHEN "btUpdateHelp" THEN DO:
            ASSIGN 
                op-ed-text = ed-text.
            RUN sys/ref/hlpupd.w (ip-field,ip-table,ip-db,ip-frame,ip-language,OUTPUT op-ed-text).
            ASSIGN
                ed-text = op-ed-text.
            DISPLAY ed-text WITH FRAME {&frame-name}.
        END.
    END CASE.
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
    DEF VAR vhWebService AS HANDLE NO-UNDO.
    DEF VAR vhSalesSoap AS HANDLE NO-UNDO.
    DEF VAR parameters1 AS LONGCHAR NO-UNDO.
    DEF VAR parameters2 AS LONGCHAR NO-UNDO.
    DEF VAR fr-title  AS CHARACTER NO-UNDO.
    DEF VAR fr-txt  AS CHARACTER NO-UNDO.
    DEF VAR fr-fram  AS CHARACTER NO-UNDO.
    DEF VAR fr-field  AS CHARACTER NO-UNDO.
    DEF VAR fr-file  AS CHARACTER NO-UNDO.
    DEF VAR fr-flags AS CHAR NO-UNDO.
    DEF VAR cRtnChar AS CHARACTER NO-UNDO.
    DEF VAR lRecFound AS LOGICAL NO-UNDO.

    FIND FIRST sys-ctrl NO-LOCK WHERE 
        sys-ctrl.name EQ "AsiHelpClientID" AND 
        sys-ctrl.company EQ g_company NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN
        vclint = sys-ctrl.char-fld.
    ELSE DO:
        RUN sys/ref/nk1look.p (INPUT g_company, 
                               "AsiHelpClientID", 
                               "C" /* Logical */, 
                               NO /* check by cust */, 
                               YES /* use cust not vendor */, 
                               "" /* cust */, 
                               "" /* ship-to*/,
                               OUTPUT cRtnChar, 
                               OUTPUT lRecFound).
    END.
    RELEASE sys-ctrl .

    FIND FIRST sys-ctrl NO-LOCK WHERE 
        sys-ctrl.name EQ "AsiHelpService" AND 
        sys-ctrl.company EQ g_company 
        NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN 
        vconn = sys-ctrl.char-fld .
    ELSE DO:
        ASSIGN 
            vconn = "".
        RUN sys/ref/nk1look.p (INPUT g_company, 
                                "AsiHelpService", 
                                "C" /* Logical */, 
                                NO /* check by cust */, 
                                YES /* use cust not vendor */, 
                                "" /* cust */, 
                                "" /* ship-to*/,
                                OUTPUT cRtnChar, 
                                OUTPUT lRecFound).
    END.
    RELEASE sys-ctrl .

    CREATE SERVER vhWebService.
    vhWebService:CONNECT(vconn) NO-ERROR.

    IF NOT vhWebService:CONNECTED() THEN DO: 
        MESSAGE 
            "Unable to connect to the Advantzware Documentation/Help Server." SKIP
            "Please report the issue to Advantzware support" SKIP
            "AsiHelpService:" + STRING(vconn) 
            VIEW-AS ALERT-BOX INFO .

        FIND FIRST hlp-head NO-LOCK WHERE
            hlp-head.fld-name EQ ip-field AND
            hlp-head.fil-name EQ ip-table AND
            hlp-head.frm-name EQ ip-frame
            NO-ERROR.
        IF AVAIL hlp-head THEN ASSIGN
            is-frame-help = yes.
        ELSE DO:
            FIND FIRST hlp-head NO-LOCK WHERE 
                hlp-head.fld-name EQ ip-field AND
                hlp-head.fil-name EQ ip-table
                NO-ERROR.
            IF NOT AVAIL hlp-head THEN DO: /* program-name */    
                FIND FIRST hlp-head NO-LOCK WHERE 
                    hlp-head.fld-name EQ ip-field 
                    NO-ERROR.
                IF NOT AVAIL hlp-head THEN DO:
                    FIND FIRST hlp-head NO-LOCK WHERE
                        hlp-head.fld-name matches ("*" +  ip-frame + "*")
                        NO-ERROR.
                    IF NOT AVAIL hlp-head THEN DO: 
                        ASSIGN
                            ip-frame = REPLACE(ip-frame,".w",".r").
                        FIND FIRST hlp-head NO-LOCK WHERE 
                            hlp-head.fld-name MATCHES ("*" +  ip-frame + "*")  
                            NO-ERROR.
                        IF NOT AVAIL hlp-head THEN DO:           
                            IF ip-table <> "" THEN MESSAGE 
                                "Help For " string(ip-table) + "." + string(ip-field) ": No Detail Help Information Available!." 
                                VIEW-AS ALERT-BOX ERROR.
                            ELSE MESSAGE
                                "Help For "  string(ip-frame)  ": No Detail Help Information Available!." 
                                VIEW-AS ALERT-BOX ERROR.
                            RETURN.                
                        END.
                    END.  
                END.
                ASSIGN 
                    is-frame-help = yes.
            END.
        END.  /* else no ip-frame */

        IF is-frame-help THEN ASSIGN
            lv-help-title = "Help For " + (if hlp-head.frm-title <> "" then hlp-head.frm-title
                            else substring(hlp-head.help-txt,1,30) )
                            + "   " + hlp-head.fil-name + "." + hlp-head.fld-name.
        ELSE IF hlp-head.frm-title EQ "" THEN ASSIGN
            lv-help-title = "Help For " + ip-db + "." + ip-table + "." + ip-field.
                            else lv-help-title = "Help On " + hlp-head.frm-title +  " For " +
                            ip-db + "." + ip-table + "." + ip-field.
        ASSIGN 
            lv-frame-name = "Frame Name: " + ip-frame
            lv-program = "Procedure: " + substring(program-name(2),index(program-name(2)," "))
            ed-text = hlp-head.help-txt.

    END. /* WebService no conn*/
    ELSE DO:  /* WebService conn*/ 
        IF ip-table EQ ? THEN ASSIGN
            ip-table = "" .
        IF ip-field EQ ? THEN ASSIGN 
            ip-field = "" .
        IF ip-frame EQ ? THEN ASSIGN 
            ip-frame = "" . 
        RUN Service1Soap SET vhSalesSoap ON vhWebService .
        RUN HelpMain IN vhSalesSoap(INPUT string(ip-field),INPUT STRING(ip-table),INPUT STRING(ip-frame), INPUT STRING(vclint),  OUTPUT parameters1,OUTPUT parameters2,OUTPUT fr-flags).

        ASSIGN 
            ed-text = parameters2
            fr-field = STRING(entry(1,parameters1))
            fr-file  = STRING(entry(3,parameters1))
            fr-title = STRING(entry(4,parameters1))
            fr-fram  = STRING(entry(2,parameters1))
            fr-txt = parameters2 NO-ERROR.

        IF fr-flags EQ "No" 
        OR fr-flags EQ "" THEN DO:
            MESSAGE 
                "Support money not paid, can't access help." 
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.

        IF ip-field NE "" 
        AND ip-table NE "" 
        AND fr-fram = "" THEN ASSIGN
            is-frame-help = NO.
        ELSE ASSIGN 
            is-frame-help = YES.

        IF is-frame-help THEN ASSIGN
            lv-help-title = "Help For " + (if fr-title <> "" then fr-title
                            else substring(fr-txt,1,30) )
                        + "   " + fr-file + "." + fr-field .                    
        ELSE IF fr-title EQ "" THEN ASSIGN 
            lv-help-title = "Help For " + ip-db + "." + ip-table + "." + ip-field.
        ELSE ASSIGN 
            lv-help-title = "Help On " + fr-title +  " For " + ip-db + "." + ip-table + "." + ip-field .
                      
        ASSIGN
            lv-frame-name = "Frame Name: " + ip-frame
            lv-program = "Procedure: " + substring(program-name(2),index(program-name(2)," ")).
    END.  /* WebService is conn*/     /*mod-sewa  */
          
  RUN enable_UI.

    DEF VAR hPgmSecurity AS HANDLE NO-UNDO.
    DEF VAR lResult AS LOG NO-UNDO.
    RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.

    RUN epCanAccess IN hPgmSecurity ("sys/ref/hlp.w", "ProTools", OUTPUT lResult).
    IF NOT lResult THEN ASSIGN btProTools:VISIBLE = FALSE.
    ELSE ASSIGN
        btProTools:SENSITIVE = FALSE.
        
    RUN epCanAccess IN hPgmSecurity ("sys/ref/hlp.w", "LockMon", OUTPUT lResult).
    IF NOT lResult THEN ASSIGN btLockMon:VISIBLE = FALSE.

    RUN epCanAccess IN hPgmSecurity ("sys/ref/hlp.w", "DataDigger", OUTPUT lResult).
    IF NOT lResult THEN ASSIGN btDataDigger:VISIBLE = FALSE.

    RUN epCanAccess IN hPgmSecurity ("sys/ref/hlp.w", "UpdateHelp", OUTPUT lResult).
    IF NOT lResult THEN ASSIGN btUpdateHelp:VISIBLE = FALSE.

    DELETE OBJECT hPgmSecurity.
    
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
  ENABLE ed-text btProTools btLockMon btDataDigger btUpdateHelp btPrint BtOK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

