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

DEF STREAM st-line .
{custom/globdefs.i}
{custom/gcompany.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}
  DEF TEMP-TABLE tmp-stack-size NO-UNDO LIKE stack-size.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-file rsSelect fiRmItem Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS lv-file rsSelect fiRmItem 

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

DEFINE VARIABLE fiRmItem AS CHARACTER FORMAT "X(256)":U 
     LABEL "RM Item" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE lv-file AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1 NO-UNDO.

DEFINE VARIABLE rsSelect AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Import Records", 2,
"Export Records", 1
     SIZE 34 BY 2.86 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     lv-file AT ROW 2.91 COL 8.6 COLON-ALIGNED NO-LABEL
     rsSelect AT ROW 4.81 COL 11 NO-LABEL WIDGET-ID 2
     fiRmItem AT ROW 8.38 COL 8.2 COLON-ALIGNED WIDGET-ID 6
     Btn_OK AT ROW 10.05 COL 15
     Btn_Cancel AT ROW 10.05 COL 43
     "(Blank for All)" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 8.62 COL 34 WIDGET-ID 8
     "Enter Stacks Data File Path" VIEW-AS TEXT
          SIZE 46 BY 1.1 AT ROW 1.71 COL 4
     SPACE(22.19) SKIP(9.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Dump Flute File"
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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Dump Flute File */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR lv-size-recid AS RECID NO-UNDO.
  DEF VAR li-next-line# LIKE stack-size.line# NO-UNDO.

  
  DEF VAR cFile AS CHAR FORMAT "x(40)".
  DEF VAR cRMItem AS CHAR FORMAT "x(18)".
  DEF VAR lDumpLoad AS LOG.

  ASSIGN lv-file rsSelect fiRmItem.

  
  DEF VAR lv-file2 AS cha NO-UNDO.
  
  SESSION:SET-WAIT-STATE("general"). 
  lv-file2 = lv-file.
  SUBSTRING(lv-file2,LENGTH(lv-file2),1) = "2".

    

    lDumpLoad = IF rsSelect = 1 THEN YES ELSE NO.
    cRmItem = fiRmItem.
    
    CASE lDumpLoad:
      WHEN YES THEN DO:

    
        OUTPUT TO VALUE(lv-file) .
        IF ERROR-STATUS:ERROR THEN DO:
          MESSAGE "Could not output data to this file, please try again"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN.
        END.
    
        FOR EACH stack-size WHERE stack-size.company = cocode
          AND (IF cRMItem = "" THEN TRUE ELSE stack-size.pallet = cRMItem)
          NO-LOCK:
          EXPORT stack-size.
        END.
        OUTPUT CLOSE.
      END.
      WHEN NO THEN DO:

        INPUT FROM VALUE(lv-file).
        REPEAT:
          CREATE tmp-stack-size.
          IMPORT tmp-stack-size.
          FOR EACH stack-size WHERE
              stack-size.company = tmp-stack-size.company AND
              stack-size.loc =  tmp-stack-size.loc AND
              stack-size.pallet = tmp-stack-size.pallet
              EXCLUSIVE-LOCK:
    
              DELETE stack-size.
          END.
        END.
        INPUT CLOSE. 
        FOR EACH tmp-stack-size WHERE tmp-stack-size.company GT ""
            AND tmp-stack-size.loc GT ""
            AND tmp-stack-size.pallet GT "":
              CREATE stack-size.
              BUFFER-COPY tmp-stack-size TO stack-size.
              RELEASE stack-size.
        END.
        
      END.
      OTHERWISE DO:
        RETURN.
      END.
    END CASE.
    


    SESSION:SET-WAIT-STATE("").
    IF lDumpLoad THEN
      MESSAGE "Dump completed." VIEW-AS ALERT-BOX.
    ELSE
      MESSAGE "Load completed." VIEW-AS ALERT-BOX.
    APPLY "go" TO FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-file Dialog-Frame
ON HELP OF lv-file IN FRAME Dialog-Frame
DO:
   def var ls-filename as cha no-undo.
   def var ll-ok as log no-undo.
   
   system-dialog get-file ls-filename 
                 title "Select File to insert"
                 filters "Data Files    (*.dat)" "*.dat",
                         "Text files (*.txt)" "*.txt",                         
                         "All Files    (*.*) " "*.*"
                 initial-dir "boximage\"
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.
      
    IF ll-ok THEN self:screen-value = ls-filename.
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
  lv-file = ".\stacks.dat".
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
  DISPLAY lv-file rsSelect fiRmItem 
      WITH FRAME Dialog-Frame.
  ENABLE lv-file rsSelect fiRmItem Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

