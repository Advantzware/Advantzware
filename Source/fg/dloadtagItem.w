&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

{fg/ttLoadTagItem.i shared}

DEFINE VARIABLE ll-secure AS LOGICAL NO-UNDO.
DEFINE VARIABLE cColumnLabel AS CHARACTER NO-UNDO .
ASSIGN 
    cocode = g_company
    locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES w-fg-rctd

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 w-fg-rctd.IS-SELECTED w-fg-rctd.i-no w-fg-rctd.tag w-fg-rctd.cases w-fg-rctd.qty-case w-fg-rctd.partial w-fg-rctd.t-qty
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3 w-fg-rctd.IS-SELECTED  
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-3 w-fg-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-3 w-fg-rctd
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH w-fg-rctd NO-LOCK w-fg-rctd.frm
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY BROWSE-3 FOR EACH w-fg-rctd NO-LOCK w-fg-rctd.frm .
&Scoped-define TABLES-IN-QUERY-BROWSE-3 w-fg-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 w-fg-rctd


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 BROWSE-3 btn-Select-all btn-process ~
btn-Deselect-all btn-cancel btn-reset

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

               
/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY
    LABEL "Ca&ncel" 
    SIZE 14 BY 1.14.

DEFINE BUTTON btn-process  
    LABEL "Save" 
    SIZE 14 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON btn-reset  
    LABEL "Reset" 
    SIZE 14 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON btn-Deselect-all 
     LABEL "Deselect All" 
     SIZE 12.5 BY 1.14.

DEFINE BUTTON btn-Select-all 
     LABEL "Select All" 
     SIZE 11 BY 1.14.

DEFINE RECTANGLE RECT-17
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 100 BY 12.67.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
    w-fg-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 Dialog-Frame _STRUCTURED
    QUERY BROWSE-3 NO-LOCK DISPLAY
      w-fg-rctd.IS-SELECTED VIEW-AS TOGGLE-BOX
      w-fg-rctd.i-no COLUMN-LABEL "Item" FORMAT "x(15)":U WIDTH 18
      w-fg-rctd.tag COLUMN-LABEL "Tag" FORMAT "x(20)":U WIDTH 27
      w-fg-rctd.cases COLUMN-LABEL "Units" FORMAT ">>>,>>9":U WIDTH 8.8
      w-fg-rctd.qty-case COLUMN-LABEL "Qty/Unit" FORMAT ">>>,>>9":U
      w-fg-rctd.partial COLUMN-LABEL "Partial" FORMAT ">>>,>>9":U
      w-fg-rctd.t-qty COLUMN-LABEL "Total Qty" FORMAT "->>,>>>,>>9":U
    
      ENABLE w-fg-rctd.IS-SELECTED
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 98 BY 10.05
         BGCOLOR 8  ROW-HEIGHT-CHARS .62 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    BROWSE-3 AT ROW 1.44 COL 2
    btn-Select-all AT ROW 12.24 COL 2.8 WIDGET-ID 2
    btn-Deselect-all AT ROW 12.24 COL 14.2 WIDGET-ID 4
    btn-process AT ROW 12.24 COL 54.2
    btn-reset AT ROW 12.24 COL 69.5
    btn-cancel AT ROW 12.24 COL 84.8
    RECT-17 AT ROW 1.19 COL 1.6
    SPACE(1.99) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    TITLE "Select Tags to Include on transfer BOL".


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
/* BROWSE-TAB BROWSE-3 1 Dialog-Frame */
ASSIGN 
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.



/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _TblList          = "asi.w-fg-rctd"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   >  w-fg-rctd.IS-SELECTED
" w-fg-rctd.IS-SELECTED" ? ? "logical" ? ? ? ? ? ? no ? no no "7.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > w-fg-rctd.i-no
"w-fg-rctd.i-no" "Item" "x(15)" "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > w-fg-rctd.tag
"w-fg-rctd.tag" "Tag" "x(20)" "character" ? ? ? ? ? ? no ? no no "27" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > w-fg-rctd.cases
"w-fg-rctd.cases" "Units" ">>>,>>9" "integer" ? ? ? ? ? ? no ? no no "8.8" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > w-fg-rctd.qty-case
"w-fg-rctd.qty-case" "Qty/Unit" ">>>,>>9" "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > w-fg-rctd.partail
"w-fg-rctd.partial" "Partial" ">>>,>>9" "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
 _FldNameList[7]   > w-fg-rctd.t-qty
"w-fg-rctd.t-qty" "Total Qty" "->>,>>>,>>9" "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME 

 
        


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Utility: Restore deleted orders */
    DO:
       FOR EACH w-fg-rctd:
        w-fg-rctd.IS-SELECTED = TRUE.
       END.

       APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
&Scoped-define SELF-NAME BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 Dialog-Frame
ON MOUSE-SELECT-CLICK OF BROWSE-3 IN FRAME Dialog-Frame
    DO:
 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 Dialog-Frame
ON ROW-LEAVE OF BROWSE-3 IN FRAME Dialog-Frame
    DO:
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME BROWSE-3
&Scoped-define SELF-NAME BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 Dialog-Frame
ON ROW-DISPLAY OF BROWSE-3 IN FRAME Dialog-Frame
DO:   
    IF AVAIL w-fg-rctd THEN DO:
                
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process Dialog-Frame
ON CHOOSE OF btn-process IN FRAME Dialog-Frame /* Delete */
    DO:
        DEFINE VARIABLE v-process AS LOG NO-UNDO.
        DEFINE VARIABLE lAskComm AS LOGICAL NO-UNDO.
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
  
        
        i = 0 .
        FOR EACH w-fg-rctd WHERE w-fg-rctd.IS-SELECTED:
            i = i + 1.          
        END. 

        IF i = 0  THEN 
        DO:
            MESSAGE "Please Select Record ..."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK .
            RETURN .
        END.               
        
        APPLY 'GO' TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-reset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-reset Dialog-Frame
ON CHOOSE OF btn-reset IN FRAME Dialog-Frame /* Reset */
    DO:
       FOR EACH w-fg-rctd:
        w-fg-rctd.IS-SELECTED = TRUE.
       END.

       RUN open-query .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Delete */
    DO:
       FOR EACH w-fg-rctd:
        w-fg-rctd.IS-SELECTED = TRUE.
       END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn-Deselect-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Deselect-all Dialog-Frame
ON CHOOSE OF btn-Deselect-all IN FRAME Dialog-Frame /* Deselect All */
DO:
       FOR EACH w-fg-rctd:
        w-fg-rctd.IS-SELECTED = FALSE.
       END.

       RUN open-query .
       
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn-Select-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Select-all Dialog-Frame
ON CHOOSE OF btn-Select-all IN FRAME Dialog-Frame /* Select All */
DO:
       FOR EACH w-fg-rctd:
        w-fg-rctd.IS-SELECTED = TRUE.
       END.

       RUN open-query .
      
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
{sys/inc/f3helpw.i}

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.

    CLOSE QUERY BROWSE-3.      
    DO WITH FRAME {&FRAME-NAME}:           
         
        OPEN QUERY BROWSE-3 FOR EACH w-fg-rctd
            NO-LOCK BY w-fg-rctd.i-no BY w-fg-rctd.tag.        
    END. 

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
    ENABLE BROWSE-3 btn-process btn-cancel btn-reset RECT-17 btn-Select-all btn-Deselect-all
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
/*{&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query Dialog-Frame 
PROCEDURE open-query :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
------------------------------------------------------------------------------*/
    
    CLOSE QUERY BROWSE-3.
    OPEN QUERY BROWSE-3 FOR EACH w-fg-rctd
        NO-LOCK BY w-fg-rctd.i-no BY w-fg-rctd.tag.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */



