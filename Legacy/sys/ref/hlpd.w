&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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

DEF INPUT PARAMETER ip-field AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-table AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-db AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-frame AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-language AS CHAR NO-UNDO.
DEF VAR is-frame-help AS LOG NO-UNDO.
DEF VAR ll-secure AS LOG NO-UNDO.
DEF VAR list-name AS CHAR NO-UNDO.
DEF VAR init-dir AS CHARACTER NO-UNDO.
DEF VAR tmp-dir AS CHAR NO-UNDO.
DEFINE VARIABLE idx AS INTEGER NO-UNDO.

SESSION:SET-WAIT-STATE("").

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ed-text btUpdateHelp btPrint Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS lv-help-title lv-program lv-frame-name ~
lv-version ed-text 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-END-KEY 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btPrint 
     LABEL "Print" 
     SIZE 15 BY 1.14.

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

DEFINE VARIABLE lv-version AS CHARACTER FORMAT "x(100)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     lv-help-title AT ROW 1.24 COL 2 NO-LABEL WIDGET-ID 14
     lv-program AT ROW 1.24 COL 68 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     lv-frame-name AT ROW 2.43 COL 2 NO-LABEL WIDGET-ID 12
     lv-version AT ROW 2.43 COL 68 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     ed-text AT ROW 3.62 COL 2 NO-LABEL WIDGET-ID 10
     btUpdateHelp AT ROW 22.43 COL 67 WIDGET-ID 6
     btPrint AT ROW 22.43 COL 86 WIDGET-ID 4
     Btn_OK AT ROW 22.43 COL 102
     SPACE(1.79) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Help Information"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_OK WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

ASSIGN 
       ed-text:RETURN-INSERTED IN FRAME gDialog  = TRUE
       ed-text:READ-ONLY IN FRAME gDialog        = TRUE.

/* SETTINGS FOR FILL-IN lv-frame-name IN FRAME gDialog
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lv-help-title IN FRAME gDialog
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lv-program IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-version IN FRAME gDialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Help Information */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrint gDialog
ON CHOOSE OF btPrint IN FRAME gDialog /* Print */
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



&Scoped-define SELF-NAME btUpdateHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdateHelp gDialog
ON CHOOSE OF btUpdateHelp IN FRAME gDialog /* Update Help */
DO:

    DEF VAR op-ed-text AS CHAR NO-UNDO.

    CASE SELF:NAME:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */
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

   
        RUN sys/ref/nk1look.p (INPUT g_company, "AsiHelpClientID", "C" /* Logical */, 
                               NO /* check by cust */, YES /* use cust not vendor */, 
                               "" /* cust */, "" /* ship-to*/,
                               OUTPUT cRtnChar, OUTPUT lRecFound).
        vclint = cRtnChar .
    
        RUN sys/ref/nk1look.p (INPUT g_company, "AsiHelpService", "C" /* Logical */, 
                               NO /* check by cust */, YES /* use cust not vendor */, 
                               "" /* cust */, "" /* ship-to*/,
                               OUTPUT cRtnChar, OUTPUT lRecFound).
        vconn = cRtnChar .
     
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
            idx = IF INDEX(PROGRAM-NAME(2)," ") LT 1 THEN 1 ELSE INDEX(PROGRAM-NAME(2)," ")
            lv-frame-name = "Frame Name: " + ip-frame
            lv-program = "Procedure: " + substring(program-name(2),idx)
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
            idx = IF INDEX(PROGRAM-NAME(2)," ") LT 1 THEN 1 ELSE INDEX(PROGRAM-NAME(2)," ")
            lv-frame-name = "Frame Name: " + ip-frame
            lv-program = "Procedure: " + substring(program-name(2),idx).

         
         RUN HelpVersion IN vhSalesSoap( OUTPUT parameters1).
         ASSIGN lv-version = "Current Version available: " +  parameters1. 

    END.  /* WebService is conn*/     /*mod-sewa  */
          
  RUN enable_UI.

    DEF VAR hPgmSecurity AS HANDLE NO-UNDO.
    DEF VAR lResult AS LOG NO-UNDO.
    RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.

    RUN epCanAccess IN hPgmSecurity ("sys/ref/hlp.w", "UpdateHelp", OUTPUT lResult).
    IF NOT lResult THEN ASSIGN btUpdateHelp:VISIBLE = FALSE.

    DELETE OBJECT hPgmSecurity.
    /*
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
        
    */
END.
{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
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
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
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
  DISPLAY lv-help-title lv-program lv-frame-name lv-version ed-text 
      WITH FRAME gDialog.
  ENABLE ed-text btUpdateHelp btPrint Btn_OK 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

