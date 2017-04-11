&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipcDataGridDat AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipcDataGridDat AS CHARACTER NO-UNDO.
ipcDataGridDat = "C:\Advantzware\v17\Resources\dataGrid\browsers\mstd.dat".
&ENDIF

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS queryString selectedFields indexedFields ~
nonIndexedFields btnOK btnCancel 
&Scoped-Define DISPLAYED-OBJECTS queryString indexText selectedFields ~
indexedFields nonIndexedFields generated 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 queryString indexText selectedFields indexedFields ~
nonIndexedFields 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U
     LABEL "" 
     SIZE 8 BY 1.9
     BGCOLOR 8 .

DEFINE VARIABLE indexText AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 50 BY 28.33
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE queryString AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 120 BY 7.14
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE generated AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 48 BY .62
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE indexedFields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 33 BY 20 NO-UNDO.

DEFINE VARIABLE nonIndexedFields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 33 BY 20 NO-UNDO.

DEFINE VARIABLE selectedFields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 33 BY 20 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     queryString AT ROW 1.71 COL 1 NO-LABEL WIDGET-ID 2
     indexText AT ROW 1.71 COL 121 NO-LABEL WIDGET-ID 4
     selectedFields AT ROW 10.05 COL 1 NO-LABEL WIDGET-ID 10
     indexedFields AT ROW 10.05 COL 35 NO-LABEL WIDGET-ID 6
     nonIndexedFields AT ROW 10.05 COL 69 NO-LABEL WIDGET-ID 8
     btnOK AT ROW 28.14 COL 103
     btnCancel AT ROW 28.14 COL 112
     generated AT ROW 1 COL 49 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     "Table / Indexes / Record Count" VIEW-AS TEXT
          SIZE 32 BY .62 AT ROW 1 COL 122 WIDGET-ID 20
     "Indexed Fields" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 9.33 COL 36 WIDGET-ID 12
     "Non-Indexed Fields" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 9.33 COL 70 WIDGET-ID 14
     "Selected Fields" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 9.33 COL 2 WIDGET-ID 16
     "Query" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1 COL 2 WIDGET-ID 18
     SPACE(161.59) SKIP(28.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Data Grd Dat"
         CANCEL-BUTTON btnCancel WIDGET-ID 100.


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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN generated IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST indexedFields IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR EDITOR indexText IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR SELECTION-LIST nonIndexedFields IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR EDITOR queryString IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR SELECTION-LIST selectedFields IN FRAME Dialog-Frame
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Data Grd Dat */
DO:
    APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame
DO:
    RUN pSetDataGridDat.
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
  RUN enable_UI.
  FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE + ": " + ipcDataGridDat.
  RUN pGetDataGridDat.
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
  DISPLAY queryString indexText selectedFields indexedFields nonIndexedFields 
          generated 
      WITH FRAME Dialog-Frame.
  ENABLE queryString selectedFields indexedFields nonIndexedFields btnOK 
         btnCancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDataGridDat Dialog-Frame 
PROCEDURE pGetDataGridDat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cColumns     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIndexed     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNonIndexed  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cGenerated   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIndexText   AS CHARACTER NO-UNDO.

    INPUT FROM VALUE(ipcDataGridDat) NO-ECHO.
    IMPORT UNFORMATTED cQueryString.
    IMPORT UNFORMATTED cColumns.
    IMPORT ^.
    IMPORT UNFORMATTED cIndexed.
    IMPORT UNFORMATTED cNonIndexed.
    IMPORT ^.
    IMPORT UNFORMATTED cGenerated.
    IMPORT ^.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            generated:SCREEN-VALUE      = cGenerated
            queryString:SCREEN-VALUE    = cQueryString
            cIndexed                    = REPLACE(cIndexed, "Indexed: ", "")
            indexedFields:LIST-ITEMS    = cIndexed
            cNonIndexed                 = REPLACE(cNonIndexed, "Non-Idx: ", "")
            nonIndexedFields:LIST-ITEMS = cNonIndexed
            selectedFields:LIST-ITEMS   = cColumns
            .
        REPEAT:
            IMPORT UNFORMATTED cIndexText.
            indexText:SCREEN-VALUE = indexText:SCREEN-VALUE + CHR(10) + cIndexText.
        END. /* repeat */
    END. /* with frame */
    INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetDataGridDat Dialog-Frame 
PROCEDURE pSetDataGridDat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        OUTPUT TO VALUE(ipcDataGridDat).
        PUT UNFORMATTED
            queryString:SCREEN-VALUE SKIP
            selectedFields:LIST-ITEMS SKIP(1)
            "Indexed: " indexedFields:LIST-ITEMS SKIP
            "Non-Idx: " nonIndexedFields:LIST-ITEMS SKIP(1)
            "Generated " STRING (TODAY ,"99.99.9999") " @ "
            STRING (TIME ,"hh:mm:ss am") " by: " USERID ("ASI") SKIP
            indexText:SCREEN-VALUE
            .
        OUTPUT CLOSE.
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

