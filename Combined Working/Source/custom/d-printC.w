&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/*DEF INPUT PARAMETER ip-print-file AS cha NO-UNDO.*/
DEF OUTPUT PARAM op-printer-name AS cha NO-UNDO.
DEF OUTPUT PARAM op-copied AS INT NO-UNDO.
DEF VAR ip-print-file AS cha NO-UNDO.
DEF VAR lv-printer-list AS cha NO-UNDO.
DEF VAR lv-printer-names AS cha NO-UNDO.

DEF VAR i AS INT NO-UNDO.
DEF VAR lv-dummy AS LOG NO-UNDO.

DEF TEMP-TABLE tt-printer FIELD prt-num AS INT
                          FIELD prt-port AS cha
                          FIELD prt-name AS cha.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cb-printer v-copies Btn_OK Btn_Cancel RECT-1 ~
RECT-2 
&Scoped-Define DISPLAYED-OBJECTS cb-printer lv-port v-copies 

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

DEFINE VARIABLE cb-printer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 59 BY 1 NO-UNDO.

DEFINE VARIABLE lv-port AS CHARACTER FORMAT "X(256)":U 
     LABEL "Where" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 NO-UNDO.

DEFINE VARIABLE v-copies AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 1 
     LABEL "Number of copies" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY 5.71.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 32 BY 2.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     cb-printer AT ROW 2.19 COL 13 COLON-ALIGNED
     lv-port AT ROW 3.86 COL 13 COLON-ALIGNED
     v-copies AT ROW 9.57 COL 69 COLON-ALIGNED
     Btn_OK AT ROW 14.1 COL 16
     Btn_Cancel AT ROW 14.1 COL 57
     RECT-1 AT ROW 1.48 COL 3
     RECT-2 AT ROW 8.86 COL 50
     " Printer" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.24 COL 6
     " Copies" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 8.62 COL 53
     SPACE(24.39) SKIP(6.80)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Print"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN lv-port IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Print */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   ASSIGN cb-printer v-copies.
  
  
   DEF VAR lv-prt-cmd AS cha NO-UNDO.
   DEF VAR lv-prt-port AS cha NO-UNDO.
   /*
   lv-prt-cmd = "command /c copy c:\temp\text.txt " + cb-printer.
   */
  /* 
   lv-prt-port = ENTRY(INDEX(lv-printer-names,cb-printer),lv-printer-list).
   MESSAGE INDEX(lv-printer-names,cb-printer) skip
         lv-prt-port VIEW-AS ALERT-BOX.
  */
   FIND FIRST tt-printer WHERE tt-printer.prt-name = cb-printer NO-LOCK NO-ERROR.
   IF AVAIL tt-printer THEN DO:
      lv-port = tt-printer.prt-port.
      lv-prt-port = lv-port.
  END.
  ASSIGN op-printer-name = cb-printer
         op-copied = v-copies.

/*
  IF cb-printer BEGINS "\\" THEN
  DO /*i = 1 TO v-copies: */ :
      op-printer-name = cb-printer.
      op-copied = v-copies.
      /*OS-COPY VALUE(ip-print-file) VALUE(cb-printer).      */
     /* OS-COMMAND VALUE ("command /c copy " + ip-print-file + " " + cb-printer). */

  END.
  ELSE IF lv-prt-port BEGINS "usb" THEN DO:
      op-printer-name = cb-printer.
      op-copied = v-copies.
      /*
      def VAR lv-input AS cha FORM "x(80)" NO-UNDO.
      OUTPUT TO PRINTER VALUE(cb-printer) .
      INPUT FROM VALUE(ip-print-file) NO-ECHO.
      REPEAT:
          IMPORT UNFORMATTED lv-input.
          PUT lv-input SKIP. /* with unformatted - make worse */
      END.
      input close.
      output close.
      */
      /*
      def VAR lv-input AS cha FORM "x(80)" NO-UNDO.
      OUTPUT TO VALUE(cb-printer).
      INPUT FROM VALUE(ip-print-file) NO-ECHO.
      REPEAT:
          IMPORT UNFORMATTED lv-input.
          PUT lv-input SKIP. /* with unformatted - make worse */
      END.
      INPUT CLOSE.
      output close.
        */
  END.
  
  ELSE DO:
      op-printer-name = lv-port.
      op-copied = v-copies.      
      
   END.
*/

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-printer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-printer Dialog-Frame
ON VALUE-CHANGED OF cb-printer IN FRAME Dialog-Frame /* Name */
DO:
   ASSIGN cb-printer.
   /*
   lv-port = ENTRY(INDEX(lv-printer-names,cb-printer),lv-printer-list).
   DISP lv-port WITH FRAME {&FRAME-NAME}.
   */
   FIND FIRST tt-printer WHERE tt-printer.prt-name = cb-printer NO-LOCK NO-ERROR.
   IF AVAIL tt-printer THEN DO:
      lv-port = tt-printer.prt-port.
      DISP lv-port WITH FRAME {&FRAME-NAME}.
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

  /*lv-printer-list = SESSION:GET-PRINTERS(). */
   RUN custom/printapi.p (OUTPUT lv-printer-list, OUTPUT lv-printer-names).
 /* display printer port
   DO i = 1 TO NUM-ENTRIES(lv-printer-list):
     lv-dummy = cb-printer:ADD-LAST(ENTRY(i,lv-printer-list)).
  END.
  */
   DO i = 1 TO NUM-ENTRIES(lv-printer-names):
     
     FIND FIRST tt-printer WHERE tt-printer.prt-name = ENTRY(i,lv-printer-names)
                           NO-LOCK NO-ERROR.
     IF NOT AVAIL tt-printer THEN DO:
        lv-dummy = cb-printer:ADD-LAST(ENTRY(i,lv-printer-names)).
        CREATE tt-printer.
        ASSIGN tt-printer.prt-num  = i
               tt-printer.prt-name = ENTRY(i,lv-printer-names)
               tt-printer.prt-port = ENTRY(i,lv-printer-list).
     
     END.
  END.

  

  RUN enable_UI.

  cb-printer:SCREEN-VALUE = SESSION:PRINTER-NAME. 
  lv-port:SCREEN-VALUE = SESSION:PRINTER-port. 

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
  DISPLAY cb-printer lv-port v-copies 
      WITH FRAME Dialog-Frame.
  ENABLE cb-printer v-copies Btn_OK Btn_Cancel RECT-1 RECT-2 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

