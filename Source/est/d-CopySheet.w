&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
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
DEFINE INPUT  PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE ghBrowse2 AS HANDLE NO-UNDO.
DEFINE VARIABLE ghBrowse3 AS HANDLE NO-UNDO.
DEFINE VARIABLE ghQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE ghtteb AS HANDLE NO-UNDO.
DEFINE VARIABLE iCounter AS INTEGER NO-UNDO.
DEFINE VARIABLE iCountBlank AS INTEGER NO-UNDO.
DEFINE VARIABLE ghColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE ghColumn2 AS HANDLE NO-UNDO.
DEFINE VARIABLE giFormCopyFrom AS INTEGER NO-UNDO.
DEFINE VARIABLE giBlankCopyFrom AS INTEGER NO-UNDO.
DEFINE VARIABLE gcFormCopyTo AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcBlankCopyTo AS CHARACTER NO-UNDO.

DEFINE BUFFER buf-eb FOR eb.
DEFINE BUFFER buf2-eb FOR eb.
DEFINE BUFFER buf-ef FOR ef.
DEFINE BUFFER buf2-ef FOR ef.

DEFINE TEMP-TABLE tteb NO-UNDO 
                  LIKE eb
                  FIELD lSelectable AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tteb

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tteb.lSelectable tteb.Form-No tteb.Blank-No tteb.Part-No tteb.Part-Dscr1   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 tteb.lSelectable   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 tteb
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 tteb
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tteb       WHERE tteb.est-no EQ ipcEstimateNo NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tteb       WHERE tteb.est-no EQ ipcEstimateNo NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tteb
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tteb


/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 tteb.lSelectable tteb.Form-No tteb.Blank-No tteb.Part-No tteb.Part-Dscr1   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3 tteb.lSelectable   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-3 tteb
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-3 tteb
&Scoped-define SELF-NAME BROWSE-3
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH tteb       WHERE tteb.est-no EQ ipcEstimateNo NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY {&SELF-NAME} FOR EACH tteb       WHERE tteb.est-no EQ ipcEstimateNo NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 tteb
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 tteb


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-2}~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 BROWSE-3 TOGGLE-DIE TOGGLE-CAD ~
Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS TOGGLE-DIE TOGGLE-CAD 

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
     LABEL "Copy" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE TOGGLE-CAD AS LOGICAL INITIAL no 
     LABEL "Copy CAD#" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.2 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-DIE AS LOGICAL INITIAL no 
     LABEL "Copy Die#" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.2 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tteb SCROLLING.

DEFINE QUERY BROWSE-3 FOR 
      tteb SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 Dialog-Frame _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      tteb.lSelectable COLUMN-LABEL "Select" VIEW-AS TOGGLE-BOX 
      tteb.Form-No COLUMN-LABEL "Form" FORMAT ">9"
      tteb.Blank-No COLUMN-LABEL "Blank" FORMAT ">9"
      tteb.Part-No COLUMN-LABEL "Part" FORMAT "X(15)"
      tteb.Part-Dscr1 COLUMN-LABEL "Description" FORMAT "X(30)"
      ENABLE tteb.lSelectable
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 67 BY 6.91 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 Dialog-Frame _FREEFORM
  QUERY BROWSE-3 NO-LOCK DISPLAY
      tteb.lSelectable COLUMN-LABEL "Select" VIEW-AS TOGGLE-BOX 
      tteb.Form-No COLUMN-LABEL "Form" FORMAT ">9"
      tteb.Blank-No COLUMN-LABEL "Blank" FORMAT ">9"
      tteb.Part-No COLUMN-LABEL "Part" FORMAT "X(15)"
      tteb.Part-Dscr1 COLUMN-LABEL "Description" FORMAT "X(30)"
      ENABLE tteb.lSelectable
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 67 BY 6.91 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-2 AT ROW 2.91 COL 4 WIDGET-ID 200
     BROWSE-3 AT ROW 2.91 COL 84 WIDGET-ID 300
     TOGGLE-DIE AT ROW 10.52 COL 116 WIDGET-ID 6
     TOGGLE-CAD AT ROW 11.71 COL 116 WIDGET-ID 8
     Btn_OK AT ROW 13.38 COL 56
     Btn_Cancel AT ROW 13.38 COL 85
     "Copy From Item" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 1.95 COL 4.6 WIDGET-ID 2
     "Copy To Item(s)" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 1.95 COL 84 WIDGET-ID 4
     SPACE(51.79) SKIP(12.52)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Estimate Item Copy"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
/* BROWSE-TAB BROWSE-2 TEXT-2 Dialog-Frame */
/* BROWSE-TAB BROWSE-3 BROWSE-2 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH eb
      WHERE est-no EQ ipcEstimateNo NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "est-no EQ ipcEstimateNo"
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH eb
      WHERE est-no EQ ipcEstimateNo NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "est-no EQ ipcEstimateNo"
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Estimate Item Copy */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 Dialog-Frame
ON VALUE-CHANGED OF lSelectable IN BROWSE BROWSE-2
DO:  
    DEFINE BUFFER buf-tteb FOR tteb.

    DO iCounter = 1 TO ghBrowse2:NUM-COLUMNS:
        ghColumn = ghBrowse2:GET-BROWSE-COLUMN(iCounter).
        IF ghColumn:NAME = "lSelectable" AND ghColumn:SCREEN-VALUE = "YES"  THEN 
            ghColumn:READ-ONLY = TRUE.
        ELSE IF ghColumn:NAME = "Form-No" THEN 
            ASSIGN giFormCopyFrom = INTEGER (ghColumn:SCREEN-VALUE). 
        ELSE IF ghColumn:NAME = "Blank-No" THEN 
            ASSIGN giBlankCopyFrom = INTEGER (ghColumn:SCREEN-VALUE).   
        ELSE NEXT.
    END.
    
    FIND FIRST buf-tteb NO-LOCK
        WHERE buf-tteb.form-no EQ giFormCopyFrom
        AND buf-tteb.blank-no EQ giBlankCopyFrom NO-ERROR.
        
    IF AVAILABLE buf-tteb THEN 
        OPEN QUERY Browse-3 
        FOR EACH tteb WHERE ROWID(tteb) NE  ROWID (buf-tteb)
        NO-LOCK INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME BROWSE-3
&Scoped-define SELF-NAME BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 Dialog-Frame
ON VALUE-CHANGED OF lSelectable IN BROWSE BROWSE-3
DO:
    DO iCounter = 1 TO ghBrowse3:NUM-COLUMNS:
        ghColumn2 = ghBrowse3:GET-BROWSE-COLUMN(iCounter).
        IF ghColumn2:NAME = "Form-No" THEN 
            ASSIGN gcFormCopyTo = gcFormCopyTo + "," + ghColumn2:SCREEN-VALUE. 
        IF ghColumn2:NAME = "Blank-No" THEN 
            ASSIGN gcBlankCopyTo = gcBlankCopyTo + "," + ghColumn2:SCREEN-VALUE.   
    END.
    ASSIGN gcFormCopyTo = TRIM(gcFormCopyTo, ",")
           gcBlankCopyTo =  TRIM(gcBlankCopyTo, ",").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Copy */
DO: 
    FIND FIRST buf-eb NO-LOCK  
        WHERE  buf-eb.est-no EQ ipcEstimateNo
          AND  buf-eb.form-no EQ giFormCopyFrom
          AND  buf-eb.blank-no EQ giBlankCopyFrom NO-ERROR.
    IF AVAILABLE buf-eb THEN 
    DO:       
        REPEAT iCountBlank = 1 TO NUM-ENTRIES(gcFormCopyTo):
            FIND FIRST buf2-eb EXCLUSIVE-LOCK 
                WHERE  buf2-eb.est-no EQ ipcEstimateNo
                AND  buf2-eb.form-no EQ INTEGER (ENTRY (iCountBlank, gcFormCopyTo))
                AND  buf2-eb.blank-no EQ INTEGER (ENTRY (iCountBlank, gcBlankCopyTo)) NO-ERROR.
            IF AVAILABLE buf2-eb THEN 
            DO:
                IF TOGGLE-DIE:SCREEN-VALUE = "YES" THEN
                ASSIGN buf2-eb.die-no = buf-eb.die-no.
                
                IF TOGGLE-CAD:SCREEN-VALUE = "YES" THEN
                ASSIGN buf2-eb.cad-no = buf-eb.cad-no.
            END. 
        END. 
    END.          
    RELEASE buf-eb.
    RELEASE buf2-eb.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
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
   ghBrowse2 = BROWSE Browse-2:HANDLE. 
   ghBrowse3 = BROWSE Browse-3:HANDLE. 
   FOR EACH eb NO-LOCK 
      WHERE eb.est-no = ipcEstimateNo:
          CREATE tteb.
          BUFFER-COPY eb TO tteb. 
   END.
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
  DISPLAY TOGGLE-DIE TOGGLE-CAD 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-2 BROWSE-3 TOGGLE-DIE TOGGLE-CAD Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

