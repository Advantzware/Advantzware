&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
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
def input param ip-recid as recid no-undo.
def var lv-mail-recid as recid no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mailcont

/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define FIELDS-IN-QUERY-D-Dialog mailcont.first-name ~
mailcont.middle-initial mailcont.last-name mailcont.cust-no ~
mailcont.cust-name mailcont.contact-title 
&Scoped-define ENABLED-FIELDS-IN-QUERY-D-Dialog mailcont.first-name ~
mailcont.middle-initial mailcont.last-name mailcont.cust-no ~
mailcont.cust-name mailcont.contact-title 
&Scoped-define ENABLED-TABLES-IN-QUERY-D-Dialog mailcont
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-D-Dialog mailcont
&Scoped-define QUERY-STRING-D-Dialog FOR EACH mailcont SHARE-LOCK
&Scoped-define OPEN-QUERY-D-Dialog OPEN QUERY D-Dialog FOR EACH mailcont SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-D-Dialog mailcont
&Scoped-define FIRST-TABLE-IN-QUERY-D-Dialog mailcont


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS mailcont.first-name mailcont.middle-initial ~
mailcont.last-name mailcont.cust-no mailcont.cust-name ~
mailcont.contact-title 
&Scoped-define ENABLED-TABLES mailcont
&Scoped-define FIRST-ENABLED-TABLE mailcont
&Scoped-Define ENABLED-OBJECTS Btn_Cancel Btn_OK RECT-8 
&Scoped-Define DISPLAYED-FIELDS mailcont.first-name mailcont.middle-initial ~
mailcont.last-name mailcont.cust-no mailcont.cust-name ~
mailcont.contact-title 
&Scoped-define DISPLAYED-TABLES mailcont
&Scoped-define FIRST-DISPLAYED-TABLE mailcont


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 87 BY 8.81.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY D-Dialog FOR 
      mailcont SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     mailcont.first-name AT ROW 1.71 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     mailcont.middle-initial AT ROW 2.67 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     mailcont.last-name AT ROW 3.62 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     mailcont.cust-no AT ROW 4.57 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.4 BY 1
     mailcont.cust-name AT ROW 5.52 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
     mailcont.contact-title AT ROW 6.48 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     Btn_Cancel AT ROW 11.24 COL 70
     Btn_OK AT ROW 11.48 COL 18
     RECT-8 AT ROW 1 COL 2
     SPACE(11.39) SKIP(6.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Mail List Addition"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _TblList          = "mailcont"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON HELP OF FRAME D-Dialog /* Mail List Addition */
DO:
    def var char-val as cha no-undo.
    lv-mail-recid = ?.
    run windows/l-contac.w (output char-val).
    if char-val <> "" then do:
       find contact where string(recid(contact)) = char-val no-lock no-error.
       if avail contact then 
           assign lv-mail-recid = recid(contact)
                  mailcont.cust-no:screen-value in frame {&frame-name} = contact.cust-no
                  mailcont.first-name:screen-value = contact.first-name
                  mailcont.middle-initial:screen-value = contact.middle-initial 
                  mailcont.last-name:screen-value = contact.last-name
                  mailcont.contact-title:screen-value = contact.contact-title
                  mailcont.cust-name:screen-value = contact.cust-name                  
                  .
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Mail List Addition */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
   do with frame {&frame-name}:
      if lv-mail-recid = ? then do:
         find first contact where contact.first-name = mailcont.first-name:screen-value 
                                  and contact.last-name =  mailcont.last-name:screen-value
                                  and contact.cust-no =  mailcont.cust-no:screen-value 
                          no-lock no-error.        
         if not avail contact then do:
             message "Invalid Contacts. Try Help." view-as alert-box error.
             apply "entry" to mailcont.last-name.
             return no-apply.
         end.
         lv-mail-recid = recid(contact).                             
      end.
      create mailcont.
      assign mailcont.first-name 
             mailcont.last-name
             mailcont.cust-no
             mailcont.cust-name
             mailcont.contact-title
             mailcont.middle-initial
             .
   end.   
      assign mailcont.list-no = maillist.list-no
             mailcont.contact-rec = lv-mail-recid
             .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{sys/inc/f3help.i}

find maillist where recid(maillist) = ip-recid no-error.
if not avail maillist then do:
   message "Mail List is not available." view-as alert-box error.
   apply "go" to this-procedure.
   quit.
end.
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
  IF AVAILABLE mailcont THEN 
    DISPLAY mailcont.first-name mailcont.middle-initial mailcont.last-name 
          mailcont.cust-no mailcont.cust-name mailcont.contact-title 
      WITH FRAME D-Dialog.
  ENABLE mailcont.first-name mailcont.middle-initial mailcont.last-name 
         mailcont.cust-no mailcont.cust-name mailcont.contact-title Btn_Cancel 
         Btn_OK RECT-8 
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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "mailcont"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

