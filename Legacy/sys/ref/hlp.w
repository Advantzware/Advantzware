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

DEF INPUT PARAM ip-field AS cha NO-UNDO.
DEF INPUT PARAM ip-table AS cha NO-UNDO.
DEF INPUT PARAM ip-db AS cha NO-UNDO.
DEF INPUT PARAM ip-frame AS cha NO-UNDO.
DEF INPUT PARAM ip-language AS cha NO-UNDO.
DEF VAR is-frame-help AS LOG NO-UNDO.
DEF VAR ll-secure AS LOG NO-UNDO.
DEF VAR list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR tmp-dir AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

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
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-btDataDigger 
       MENU-ITEM m_ProTools2    LABEL "ProTools"      
       MENU-ITEM m_Program_Stack LABEL "Program Stack" 
       MENU-ITEM m_Trans        LABEL "Transactions and Locks Window".

DEFINE MENU POPUP-MENU-lv-program 
       MENU-ITEM m_ProTools     LABEL "ProTools"      .


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

DEFINE BUTTON Btn_OK AUTO-END-KEY 
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
     btDataDigger AT ROW 22.91 COL 106 WIDGET-ID 2
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
   FRAME-NAME                                                           */
ASSIGN 
       btDataDigger:POPUP-MENU IN FRAME F-Main       = MENU POPUP-MENU-btDataDigger:HANDLE.

ASSIGN 
       ed-text:RETURN-INSERTED IN FRAME F-Main  = TRUE
       ed-text:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN lv-frame-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-help-title IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-program IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       lv-program:POPUP-MENU IN FRAME F-Main       = MENU POPUP-MENU-lv-program:HANDLE.

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


&Scoped-define SELF-NAME btDataDigger
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDataDigger W-Win
ON CHOOSE OF btDataDigger IN FRAME F-Main /* Admin */
DO:
  MESSAGE "Datadigger is replaced with the query tool on the advantzware menu."
  VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-print W-Win
ON CHOOSE OF btn-print IN FRAME F-Main /* Print */
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
          ed-text:SCREEN-VALUE FORMAT "x(10000)"  .

    RUN custom/prntproc.p (list-name,INT(11),"P").
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-update W-Win
ON CHOOSE OF btn-update IN FRAME F-Main /* Update Help */
DO:
    DEF VAR op-ed-text AS cha NO-UNDO.
    ASSIGN 
        op-ed-text = ed-text.

    /* need security check */
    DEF VAR hPgmSecurity AS HANDLE NO-UNDO.
    DEF VAR lResult AS LOG NO-UNDO.
    RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
    RUN epCanAccess IN hPgmSecurity ("sys/ref/hlp.w", "Access1", OUTPUT lResult).
    DELETE OBJECT hPgmSecurity.
    
    /* If not automatically cleared by security level, ask for password */
    IF lResult THEN DO:
       
            RUN sys/ref/hlpupd.w (ip-field,ip-table,ip-db,ip-frame,ip-language,OUTPUT op-ed-text).
    END.

    /* === re-display    ==========*/
    assign
        ed-text = op-ed-text .
    DISPLAY ed-text WITH FRAME {&frame-name}.   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_ProTools2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_ProTools2 W-Win
ON CHOOSE OF MENU-ITEM m_ProTools2 /* ProTools */
DO:
  RUN protools\_protool.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
is-frame-help = NO.
IF ip-table = ? THEN ip-table = "".

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

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.name    EQ "AsiHelpClientID"
       AND sys-ctrl.company EQ g_company  NO-ERROR.
  IF AVAIL sys-ctrl THEN
        vclint = sys-ctrl.char-fld  .
  ELSE DO:
      RUN sys/ref/nk1look.p (INPUT g_company, "AsiHelpClientID", "C" /* Logical */, NO /* check by cust */, 
            INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
            OUTPUT cRtnChar, OUTPUT lRecFound).
  END.
  RELEASE sys-ctrl .

FIND FIRST sys-ctrl  NO-LOCK
     WHERE sys-ctrl.name    EQ "AsiHelpService"
       AND sys-ctrl.company EQ g_company  NO-ERROR.
  IF AVAIL sys-ctrl THEN
      ASSIGN vconn = sys-ctrl.char-fld .
  ELSE DO:
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

     FIND FIRST hlp-head WHERE hlp-head.fld-name = ip-field AND
                                 hlp-head.fil-name = ip-table AND
                                 hlp-head.frm-name = ip-frame
                                 NO-LOCK NO-ERROR.
     IF AVAIL hlp-head THEN DO:
         is-frame-help = YES.
     END.
     ELSE DO:
         FIND FIRST hlp-head WHERE hlp-head.fld-name = ip-field AND
                                 hlp-head.fil-name = ip-table
                                 NO-LOCK NO-ERROR.
         IF NOT AVAIL hlp-head THEN DO:                                /* program-name */
             FIND FIRST hlp-head WHERE hlp-head.fld-name = ip-field NO-LOCK NO-ERROR.     
             IF NOT AVAIL hlp-head THEN DO:
                 FIND FIRST hlp-head WHERE hlp-head.fld-name MATCHES ("*" +  ip-frame + "*")  NO-LOCK NO-ERROR.
                 IF NOT AVAIL hlp-head THEN DO: 
                     ip-frame = REPLACE(ip-frame,".w",".r").
                     FIND FIRST hlp-head WHERE hlp-head.fld-name MATCHES ("*" +  ip-frame + "*")  NO-LOCK NO-ERROR.
                     IF NOT AVAIL hlp-head THEN DO:         
                         IF ip-table <> "" THEN MESSAGE "Help For " STRING(ip-table) + "." + string(ip-field) ": No Detail Help Information Available!." VIEW-AS ALERT-BOX ERROR.
                         ELSE MESSAGE "Help For " ip-field "," STRING(ip-frame)  ": No Detail Help Information Available!." VIEW-AS ALERT-BOX ERROR.
                         RETURN .
                     END.
                 END.
           END.  
          is-frame-help = YES.
       END.
     END.  /* else no ip-frame */

     /*
     find first help-msg where asihlp.help-msg.msg-number = hlp-head.msg-num no-lock no-error.
     */       

     /*          
     message "help: " is-frame-help  ip-field ip-table ip-frame view-as alert-box.
     */

     IF is-frame-help THEN  
         lv-help-title = "Help For " + (IF hlp-head.frm-title <> "" THEN hlp-head.frm-title
                                 ELSE SUBSTRING(hlp-head.help-txt,1,30) )
                                 + "   " + hlp-head.fil-name + "." + hlp-head.fld-name
                                 . 
     ELSE IF hlp-head.frm-title = "" THEN 
         lv-help-title = "Help For " + ip-db + "." + ip-table + "." + ip-field.
     ELSE lv-help-title = "Help On " + hlp-head.frm-title +  " For " +
         ip-db + "." + ip-table + "." + ip-field.

      lv-frame-name = "Frame Name: " + ip-frame.
      lv-program = "Procedure: " + substring(PROGRAM-NAME(2),INDEX(PROGRAM-NAME(2)," ")).
      ed-text = hlp-head.help-txt.

    END. /* WebService no conn*/

    ELSE DO:
       
    RUN Service1Soap SET vhSalesSoap ON vhWebService .
    RUN HelpMain IN vhSalesSoap(INPUT STRING(ip-field),INPUT STRING(ip-table),INPUT STRING(ip-frame),INPUT STRING(vclint),  OUTPUT parameters1,OUTPUT parameters2,OUTPUT fr-flags ).
     
    ed-text = parameters2.
    fr-field = STRING(ENTRY(1,parameters1)) NO-ERROR.
    fr-file  = STRING(ENTRY(3,parameters1)) NO-ERROR.
    fr-title = STRING(ENTRY(4,parameters1)) NO-ERROR.
    fr-fram  = STRING(ENTRY(2,parameters1)) NO-ERROR.
    fr-txt = parameters2 .


    IF fr-flags = "No" OR  fr-flags = "" THEN DO:
        MESSAGE "Support money not paid, can't access help" VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    IF ip-field <> "" AND ip-table <> "" AND fr-fram = "" THEN
        is-frame-help = NO.
    ELSE is-frame-help = YES.

         IF is-frame-help THEN  
             lv-help-title = "Help For " + (IF fr-title <> "" THEN fr-title
                 ELSE SUBSTRING(fr-txt,1,30) )
                 + "   " + fr-file + "." + fr-field .                    

                 ELSE IF fr-title = "" THEN 
                     lv-help-title = "Help For " + ip-db + "." + ip-table + "." + ip-field.
                 ELSE lv-help-title = "Help On " + fr-title +  " For " +
                      ip-db + "." + ip-table + "." + ip-field .
                      
                 lv-frame-name = "Frame Name: " + ip-frame.
                 lv-program = "Procedure: " + substring(PROGRAM-NAME(2),INDEX(PROGRAM-NAME(2)," ")).
    END.  /* WebService is conn*/
/* mod-sewa */

{&WINDOW-NAME}:MOVE-TO-TOP().

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
  ENABLE ed-text Btn_OK btn-print btn-update btDataDigger 
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    DEF VAR hPgmSecurity AS HANDLE NO-UNDO.
    DEF VAR lResult AS LOG NO-UNDO.
    RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
    RUN epCanAccess IN hPgmSecurity ("sys/ref/hlp.w", "Access2", OUTPUT lResult).
    DELETE OBJECT hPgmSecurity.
    IF NOT lResult THEN ASSIGN
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

