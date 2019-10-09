&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: oerep/r-bolx.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/* {custom/globdefs.i} */
{methods/prgsecur.i}

{sys/inc/var.i new shared}

{oerep\r-bolx.i}

DEF VAR v-dir AS CHAR FORMAT "X(100)" NO-UNDO.

assign
 cocode = g_company
 locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tg_print-logo tg_print-addr scr-logo ~
scr-name scr-addr-1 scr-addr-2 scr-city scr-state scr-zip scr-phone-area ~
scr-phone-num scr-fax-area scr-fax-num scr-note-1 scr-note-2 Btn_OK RECT-42 
&Scoped-Define DISPLAYED-OBJECTS tg_print-logo tg_print-addr scr-logo ~
scr-name scr-addr-1 scr-addr-2 scr-city scr-state scr-zip scr-phone-area ~
scr-phone-num scr-fax-area scr-fax-num scr-note-1 scr-note-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE scr-addr-1 AS CHARACTER FORMAT "X(30)":U 
     LABEL "Addr 1" 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE scr-addr-2 AS CHARACTER FORMAT "X(30)":U 
     LABEL "Addr 2" 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE scr-city AS CHARACTER FORMAT "X(15)":U 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE scr-fax-area AS CHARACTER FORMAT "(xxx)":U 
     LABEL "Fax#" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE scr-fax-num AS CHARACTER FORMAT "xxx-xxxx":U 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE scr-logo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Logo" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 NO-UNDO.

DEFINE VARIABLE scr-name AS CHARACTER FORMAT "X(30)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE scr-note-1 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Note 1" 
     VIEW-AS FILL-IN 
     SIZE 58.4 BY 1 NO-UNDO.

DEFINE VARIABLE scr-note-2 AS CHARACTER FORMAT "X(60)":U 
     LABEL "Note 2" 
     VIEW-AS FILL-IN 
     SIZE 58.4 BY 1 NO-UNDO.

DEFINE VARIABLE scr-phone-area AS CHARACTER FORMAT "(xxx)":U 
     LABEL "Phone#" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE scr-phone-num AS CHARACTER FORMAT "xxx-xxxx":U 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE scr-state AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE scr-zip AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 12.62.

DEFINE VARIABLE tg_print-addr AS LOGICAL INITIAL no 
     LABEL "Print Address?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE tg_print-logo AS LOGICAL INITIAL no 
     LABEL "Print Logo?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     tg_print-logo AT ROW 1.48 COL 3 WIDGET-ID 30
     tg_print-addr AT ROW 1.48 COL 23 WIDGET-ID 32
     scr-logo AT ROW 2.91 COL 9.4 COLON-ALIGNED WIDGET-ID 6
     scr-name AT ROW 4.57 COL 9.6 COLON-ALIGNED WIDGET-ID 8
     scr-addr-1 AT ROW 5.57 COL 9.6 COLON-ALIGNED WIDGET-ID 10
     scr-addr-2 AT ROW 6.57 COL 9.6 COLON-ALIGNED WIDGET-ID 12
     scr-city AT ROW 7.57 COL 9.6 COLON-ALIGNED WIDGET-ID 14
     scr-state AT ROW 7.57 COL 33 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     scr-zip AT ROW 7.57 COL 39.8 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     scr-phone-area AT ROW 8.57 COL 9.6 COLON-ALIGNED WIDGET-ID 18
     scr-phone-num AT ROW 8.57 COL 16.8 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     scr-fax-area AT ROW 9.57 COL 9.6 COLON-ALIGNED WIDGET-ID 26
     scr-fax-num AT ROW 9.57 COL 16.8 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     scr-note-1 AT ROW 10.95 COL 9.6 COLON-ALIGNED WIDGET-ID 34
     scr-note-2 AT ROW 12 COL 9.6 COLON-ALIGNED WIDGET-ID 36
     Btn_OK AT ROW 14.1 COL 27
     RECT-42 AT ROW 1 COL 1
     SPACE(0.00) SKIP(2.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Freight BOL Options"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Freight BOL Options */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN {&displayed-objects}.

     IF tg_print-logo AND 
       TRIM(scr-logo) EQ "" THEN DO:
     
        MESSAGE 
          "Please enter a path location for the logo."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

        APPLY "entry" TO scr-logo.
        RETURN NO-APPLY.

     END.

     FIND FIRST tt-bolx NO-ERROR.

     IF NOT AVAIL tt-bolx THEN
        CREATE tt-bolx.

     ASSIGN
        tt-bolx.print-logo = tg_print-logo
        tt-bolx.print-addr = tg_print-addr
        tt-bolx.logo-file = scr-logo
        tt-bolx.NAME = scr-name
        tt-bolx.addr-1 = scr-addr-1
        tt-bolx.addr-2 = scr-addr-2
        tt-bolx.city = scr-city
        tt-bolx.state = scr-state
        tt-bolx.zip = scr-zip
        tt-bolx.phone-area = scr-phone-area
        tt-bolx.phone-num = scr-phone-num
        tt-bolx.fax-area = scr-fax-area
        tt-bolx.fax-num = scr-fax-num
        tt-bolx.note-1 = scr-note-1
        tt-bolx.note-2 = scr-note-2.

     RELEASE tt-bolx.
  END.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-logo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-logo Dialog-Frame
ON HELP OF scr-logo IN FRAME Dialog-Frame /* Logo */
DO:
   DEF VAR chFile AS CHAR FORMAT "X(80)" NO-UNDO.
   DEF VAR ll-ok AS LOG NO-UNDO.
   
   DO WITH FRAME {&FRAME-NAME}:
      system-dialog get-file chFile 
                    title "Select Logo File"
                    filters "Graphic Images (*.bmp *.jpg *.jpeg *.gif *.png) " "*.bmp,*.jpg,*.jpeg,*.gif,*.png"
                    initial-dir v-dir
                    MUST-EXIST
                    USE-FILENAME
                    UPDATE ll-ok.

      IF ll-ok THEN
         scr-logo:SCREEN-VALUE = chFile.
   END.
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

   FIND FIRST tt-bolx NO-ERROR.


   RUN enable_UI.

   {custom/usrprint.i}
   APPLY 'ENTRY' TO scr-logo .

   FIND FIRST sys-ctrl-shipto WHERE
        sys-ctrl-shipto.company EQ cocode AND
        sys-ctrl-shipto.NAME = "BOLFMTX" AND
        sys-ctrl-shipto.cust-vend = YES AND
        sys-ctrl-shipto.cust-vend-no = ""
        NO-LOCK NO-ERROR.

   IF AVAIL sys-ctrl-shipto THEN
      v-dir = sys-ctrl-shipto.char-fld.

   IF NOT AVAIL tt-bolx THEN
   DO:
      FIND FIRST cust WHERE
           cust.company EQ cocode AND
           cust.ACTIVE EQ "X"
           NO-LOCK NO-ERROR.
     
      IF AVAIL cust THEN
         ASSIGN
            scr-name:SCREEN-VALUE = cust.NAME
            scr-addr-1:SCREEN-VALUE = cust.addr[1]
            scr-addr-2:SCREEN-VALUE = cust.addr[2]
            scr-city:SCREEN-VALUE = cust.city
            scr-state:SCREEN-VALUE = cust.state
            scr-zip:SCREEN-VALUE = cust.zip
            scr-phone-area:SCREEN-VALUE = cust.area-code
            scr-phone-num:SCREEN-VALUE = cust.phone
            scr-fax-area:SCREEN-VALUE = SUBSTR(cust.fax,1,3)
            scr-fax-num:SCREEN-VALUE = SUBSTR(cust.fax,4).

/* gdm - 04130902 
      FIND FIRST sys-ctrl WHERE
           sys-ctrl.company EQ cocode AND
           sys-ctrl.NAME = "BOLFMTX"
           NO-LOCK NO-ERROR.

      IF AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "FIBREX" THEN
         scr-logo:SCREEN-VALUE = "P:\rcode\images\fibrelog.bmp".
*/
   END.
   ELSE
      ASSIGN tg_print-logo:SCREEN-VALUE = STRING(tt-bolx.print-logo)
             tg_print-addr:SCREEN-VALUE = STRING(tt-bolx.print-addr)
             scr-logo:SCREEN-VALUE = tt-bolx.logo-file
             scr-name:SCREEN-VALUE = tt-bolx.NAME
             scr-addr-1:SCREEN-VALUE = tt-bolx.addr-1
             scr-addr-2:SCREEN-VALUE = tt-bolx.addr-2
             scr-city:SCREEN-VALUE = tt-bolx.city
             scr-state:SCREEN-VALUE = tt-bolx.state 
             scr-zip:SCREEN-VALUE = tt-bolx.zip
             scr-phone-area:SCREEN-VALUE = tt-bolx.phone-area
             scr-phone-num:SCREEN-VALUE = tt-bolx.phone-num
             scr-fax-area:SCREEN-VALUE = tt-bolx.fax-area
             scr-fax-num:SCREEN-VALUE = tt-bolx.fax-num
             scr-note-1:SCREEN-VALUE = tt-bolx.note-1
             scr-note-2:SCREEN-VALUE = tt-bolx.note-2.

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
  DISPLAY tg_print-logo tg_print-addr scr-logo scr-name scr-addr-1 scr-addr-2 
          scr-city scr-state scr-zip scr-phone-area scr-phone-num scr-fax-area 
          scr-fax-num scr-note-1 scr-note-2 
      WITH FRAME Dialog-Frame.
  ENABLE tg_print-logo tg_print-addr scr-logo scr-name scr-addr-1 scr-addr-2 
         scr-city scr-state scr-zip scr-phone-area scr-phone-num scr-fax-area 
         scr-fax-num scr-note-1 scr-note-2 Btn_OK RECT-42 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

