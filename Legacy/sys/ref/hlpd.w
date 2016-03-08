&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

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

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ed-text Btn_OK btn-print btn-update 
&Scoped-Define DISPLAYED-OBJECTS lv-help-title lv-program lv-frame-name ~
ed-text 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-update 
     LABEL "Update Help" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-print 
    LABEL "Print" 
    SIZE 15 BY 1.14. 

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE ed-text AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
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

DEFINE FRAME Dialog-Frame
     lv-help-title AT ROW 1.48 COL 2 COLON-ALIGNED NO-LABEL
     lv-program AT ROW 1.48 COL 70 COLON-ALIGNED NO-LABEL
     lv-frame-name AT ROW 2.67 COL 2 COLON-ALIGNED NO-LABEL
     ed-text AT ROW 3.86 COL 4 NO-LABEL
     Btn_OK AT ROW 22.91 COL 25
     btn-print AT ROW 22.91 COL 55
     btn-update AT ROW 22.91 COL 78
     SPACE(24.59) SKIP(0.22)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Help Information".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       ed-text:RETURN-INSERTED IN FRAME Dialog-Frame  = TRUE
       ed-text:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN lv-frame-name IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-help-title IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-program IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Help Information */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-update Dialog-Frame
ON CHOOSE OF btn-update IN FRAME Dialog-Frame /* Update Help */
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

    IF NOT ll-secure THEN RUN sys/ref/uphlp-pass.w (3, OUTPUT ll-secure).

    IF ll-secure EQ YES THEN
    run sys/ref/hlpupd.p (ip-field,ip-table,ip-db,ip-frame,ip-language,OUTPUT op-ed-text).
/* === re-display    ==========*/
  /*find first asihlp.hlp-head where hlp-head.fld-name = ip-field and
                                 hlp-head.fil-name = ip-table
                                 no-lock no-error.
  if not avail hlp-head then do:
     find first hlp-head where hlp-head.fld-name = ip-field NO-LOCK NO-ERROR.     
     IF NOT AVAIL hlp-head THEN
        find first hlp-head where hlp-head.fld-name matches ("*" +  ip-frame + "*")  no-lock no-error.
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-print Dialog-Frame
ON CHOOSE OF btn-print IN FRAME Dialog-Frame /* print Help */
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


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

find first sys-ctrl  WHERE sys-ctrl.name    eq "AsiHelpClientID"
        no-lock no-error.
  IF AVAIL sys-ctrl THEN
        vclint = sys-ctrl.char-fld  .
  RELEASE sys-ctrl .

find first sys-ctrl  WHERE sys-ctrl.name    eq "AsiHelpService"
        no-lock no-error.
  IF AVAIL sys-ctrl THEN
      ASSIGN vconn = sys-ctrl.char-fld .
  ELSE
      vconn = "".
  RELEASE sys-ctrl .


      CREATE SERVER vhWebService.
      vhWebService:CONNECT(vconn) NO-ERROR.

IF NOT vhWebService:CONNECTED() THEN
    DO: 

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
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
      WITH FRAME Dialog-Frame.
  ENABLE ed-text Btn_OK btn-print btn-update 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

