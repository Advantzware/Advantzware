&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File              : oerep\d-fibreci.w

  Description       : Dialog-Box for Commercial Invoice User Input

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters */
DEF INPUT PARAM icInvoiceForm   AS CHAR NO-UNDO.
DEF INPUT PARAM ip-total-pallets AS INT NO-UNDO.

def SHARED TEMP-TABLE w-comm-bol NO-UNDO field bol-no as INT INDEX bol-no bol-no.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_InvoiceNum fi_TotalPallets fi_Date ~
fi_TrailerNum fi_Notes1 fi_Notes2 fi_Notes3 fi_Notes4 fi_Notes5 fi_Notes6 ~
Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_InvoiceNum fi_TotalPallets fi_Date ~
fi_TrailerNum fi_Notes1 fi_Notes2 fi_Notes3 fi_Notes4 fi_Notes5 fi_Notes6 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi_Date AS DATE FORMAT "99/99/9999":U 
     LABEL "Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_InvoiceNum AS CHARACTER FORMAT "X(20)":U 
     LABEL "Invoice #" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE fi_Notes1 AS CHARACTER FORMAT "x(60)":U 
     LABEL "Note 1" 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1 NO-UNDO.

DEFINE VARIABLE fi_Notes2 AS CHARACTER FORMAT "x(60)":U 
     LABEL "Note 2" 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1 NO-UNDO.

DEFINE VARIABLE fi_Notes3 AS CHARACTER FORMAT "x(60)":U 
     LABEL "Note 3" 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1 NO-UNDO.

DEFINE VARIABLE fi_Notes4 AS CHARACTER FORMAT "x(60)":U 
     LABEL "Note 4" 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1 NO-UNDO.

DEFINE VARIABLE fi_Notes5 AS CHARACTER FORMAT "x(60)":U 
     LABEL "Note 5" 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1 NO-UNDO.

DEFINE VARIABLE fi_Notes6 AS CHARACTER FORMAT "x(60)":U 
     LABEL "Note 6" 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1 NO-UNDO.

DEFINE VARIABLE fi_TotalPallets AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Total Pallets" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_TrailerNum AS CHARACTER FORMAT "X(20)":U 
     LABEL "Trailer #" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fi_InvoiceNum AT ROW 2.05 COL 19.8 COLON-ALIGNED
     fi_TotalPallets AT ROW 2.05 COL 58.6 COLON-ALIGNED
     fi_Date AT ROW 3.33 COL 19.8 COLON-ALIGNED
     fi_TrailerNum AT ROW 3.33 COL 58.6 COLON-ALIGNED
     fi_Notes1 AT ROW 6.19 COL 10 COLON-ALIGNED
     fi_Notes2 AT ROW 7.43 COL 10 COLON-ALIGNED
     fi_Notes3 AT ROW 8.67 COL 10 COLON-ALIGNED
     fi_Notes4 AT ROW 9.91 COL 10 COLON-ALIGNED
     fi_Notes5 AT ROW 11.14 COL 10 COLON-ALIGNED
     fi_Notes6 AT ROW 12.38 COL 10 COLON-ALIGNED
     Btn_OK AT ROW 14.62 COL 30.8
     Btn_Cancel AT ROW 14.62 COL 47
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 92 BY 16.19.


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
         TITLE              = "Commercial Invoice"
         HEIGHT             = 16.19
         WIDTH              = 92
         MAX-HEIGHT         = 16.95
         MAX-WIDTH          = 92
         VIRTUAL-HEIGHT     = 16.95
         VIRTUAL-WIDTH      = 92
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
                                                                        */
ASSIGN
       Btn_Cancel:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


ASSIGN
       Btn_OK:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Commercial Invoice */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Commercial Invoice */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  RUN ExecuteForm.

  APPLY 'CLOSE':U TO THIS-PROCEDURE.
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
  RUN enable_UI.

  ASSIGN
     fi_Date:SCREEN-VALUE = STRING (TODAY,'99/99/9999')
     fi_TotalPallets:SCREEN-VALUE = STRING(ip-total-pallets).

  SESSION:SET-WAIT-STATE ("").

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
  DISPLAY fi_InvoiceNum fi_TotalPallets fi_Date fi_TrailerNum fi_Notes1 
          fi_Notes2 fi_Notes3 fi_Notes4 fi_Notes5 fi_Notes6 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fi_InvoiceNum fi_TotalPallets fi_Date fi_TrailerNum fi_Notes1 
         fi_Notes2 fi_Notes3 fi_Notes4 fi_Notes5 fi_Notes6 Btn_OK Btn_Cancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExecuteForm C-Win 
PROCEDURE ExecuteForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  CASE icInvoiceForm:

    WHEN 'FIBREMEXICO':U THEN DO WITH FRAME {&FRAME-NAME}:

      RUN oerep\fibreci_MEX-XL.p (INPUT   fi_InvoiceNum:SCREEN-VALUE,
                                  INPUT         fi_Date:SCREEN-VALUE,
                                  INPUT INTEGER(fi_TotalPallets:SCREEN-VALUE),
                                  INPUT   fi_TrailerNum:SCREEN-VALUE,
                                  INPUT       fi_Notes1:SCREEN-VALUE,
                                  INPUT       fi_Notes2:SCREEN-VALUE,
                                  INPUT       fi_Notes3:SCREEN-VALUE,
                                  INPUT       fi_Notes4:SCREEN-VALUE,
                                  INPUT       fi_Notes5:SCREEN-VALUE,
                                  INPUT       fi_Notes6:SCREEN-VALUE).
    END.

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

