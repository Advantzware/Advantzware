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
DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER ipiJobNo2 AS INTEGER NO-UNDO .
DEFINE INPUT PARAMETER ipRowId AS ROWID NO-UNDO .
DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO .

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

{jc/ttJobItem.i SHARED}

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
&Scoped-define INTERNAL-TABLES tt-job-item

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 tt-job-item.IS-SELECTED tt-job-item.frm tt-job-item.blank-no tt-job-item.rm-i-no tt-job-item.mach-id tt-job-item.mat-alloc
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3 tt-job-item.IS-SELECTED  
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-3 tt-job-item
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-3 tt-job-item
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH tt-job-item NO-LOCK tt-job-item.frm
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY BROWSE-3 FOR EACH tt-job-item NO-LOCK tt-job-item.frm .
&Scoped-define TABLES-IN-QUERY-BROWSE-3 tt-job-item
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 tt-job-item


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 BROWSE-3 btn-Select-all btn-process ~
btn-Deselect-all btn-cancel 

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
    LABEL "Delete" 
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
    SIZE 70 BY 12.67.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
    tt-job-item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 Dialog-Frame _STRUCTURED
    QUERY BROWSE-3 NO-LOCK DISPLAY
    tt-job-item.IS-SELECTED COLUMN-LABEL ''  VIEW-AS TOGGLE-BOX 
    tt-job-item.frm FORMAT ">>>" COLUMN-LABEL "S" WIDTH 7
    tt-job-item.blank-no FORMAT ">>>" COLUMN-LABEL "B" WIDTH 7
    tt-job-item.rm-i-no FORMAT "X(20)" 
    tt-job-item.mach-id FORMAT "x(20)"
    tt-job-item.mat-alloc 
    
      ENABLE tt-job-item.IS-SELECTED
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 68 BY 10.05
         BGCOLOR 8  ROW-HEIGHT-CHARS .62 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    BROWSE-3 AT ROW 1.44 COL 2
    btn-Select-all AT ROW 12.24 COL 2.8 WIDGET-ID 2
     btn-Deselect-all AT ROW 12.24 COL 14.2 WIDGET-ID 4
    btn-process AT ROW 12.24 COL 37.2
    btn-cancel AT ROW 12.24 COL 54.8
    RECT-17 AT ROW 1.19 COL 1.6
    SPACE(1.99) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    TITLE "Delete Materials".


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

IF ipcType EQ "job-mch" THEN DO:
    tt-job-item.rm-i-no:VISIBLE IN BROWSE BROWSE-3 = FALSE .
    tt-job-item.mat-alloc:VISIBLE IN BROWSE BROWSE-3 = FALSE .
END.
ELSE tt-job-item.mach-id:VISIBLE IN BROWSE BROWSE-3 = FALSE .



/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _TblList          = "asi.oe-ord"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "asi.oe-ord.company = g_company
and oe-ord.stat = ""D"""
     _FldNameList[1]   >  tt-job-item.IS-SELECTED
" tt-job-item.IS-SELECTED" ? ? "logical" ? ? ? ? ? ? no ? no no "7.6" yes no no "U" "" ""
     _FldNameList[2]   > tt-job-item.frm
"tt-job-item.frm" ? ? "Integer" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[3]   > tt-job-item.blank-no
"tt-job-item.blank-no" ? ? "integer" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[4]   > tt-job-item.rm-i-no
"tt-job-item.rm-i-no" ? ? "Character" ? ? ? ? ? ? no ? no no ? no no no "U" "" ""
     _FldNameList[5]   > tt-job-item.mach-id
"tt-job-item.mach-id" ? ? "Character" ? ? ? ? ? ? no ? no no ? no no no "U" "" ""
     _FldNameList[6]   > tt-job-item.mat-alloc
"tt-job-item.mat-alloc" ? ? "Logical" ? ? ? ? ? ? no ? no no ? no no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME 

 
        


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Utility: Restore deleted orders */
    DO:
       FOR EACH tt-job-item:
        tt-job-item.IS-SELECTED = FALSE.
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
    IF AVAIL tt-job-item THEN DO:
        IF tt-job-item.mat-alloc = YES THEN DO:
            tt-job-item.IS-SELECTED:BGCOLOR IN BROWSE {&BROWSE-NAME} = 2 .
            tt-job-item.frm:BGCOLOR IN BROWSE {&BROWSE-NAME}  = 2 .
            tt-job-item.blank-no:BGCOLOR IN BROWSE {&BROWSE-NAME} = 2 .
            tt-job-item.rm-i-no:BGCOLOR IN BROWSE {&BROWSE-NAME} = 2 .            
            tt-job-item.mat-alloc:BGCOLOR IN BROWSE {&BROWSE-NAME} = 2 .
        END.
        
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
        FOR EACH tt-job-item WHERE tt-job-item.IS-SELECTED:
            i = i + 1.
          IF tt-job-item.mat-alloc THEN
          ASSIGN
            lAskComm = YES .
        END.

        IF i = 0  THEN 
        DO:
            MESSAGE "Please Select Record ..."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK .
            RETURN .
        END.
        
       IF lAskComm THEN
       DO:
             MESSAGE "Material is committed. Are you sure you want to " FRAME {&FRAME-NAME}:TITLE 
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.
            IF NOT v-process THEN RETURN NO-APPLY .
       END.
       ELSE do:
          MESSAGE "Are you sure you want to " FRAME {&FRAME-NAME}:TITLE 
                 VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.
          IF NOT v-process THEN RETURN NO-APPLY .
        END.
        
        APPLY 'GO' TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Delete */
    DO:
       FOR EACH tt-job-item:
        tt-job-item.IS-SELECTED = FALSE.
       END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn-Deselect-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Deselect-all Dialog-Frame
ON CHOOSE OF btn-Deselect-all IN FRAME Dialog-Frame /* Deselect All */
DO:
       FOR EACH tt-job-item:
        tt-job-item.IS-SELECTED = FALSE.
       END.

       RUN open-query .
       
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn-Select-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Select-all Dialog-Frame
ON CHOOSE OF btn-Select-all IN FRAME Dialog-Frame /* Select All */
DO:
       FOR EACH tt-job-item:
        tt-job-item.IS-SELECTED = TRUE.
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
    RUN build-table. 
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST tt-job-item
            WHERE  tt-job-item.tt-rowid EQ ipRowId NO-ERROR .
        IF AVAILABLE tt-job-item THEN
            tt-job-item.IS-SELECTED = TRUE .
         
        OPEN QUERY BROWSE-3 FOR EACH tt-job-item
            NO-LOCK BY tt-job-item.frm.
      IF ipcType EQ "job-mch" THEN DO:
       FRAME {&FRAME-NAME}:TITLE = "Delete Routing" .
      END.
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
    ENABLE BROWSE-3 btn-process btn-cancel RECT-17 btn-Select-all btn-Deselect-all
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
/*{&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table Dialog-Frame 
PROCEDURE build-table :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEF VAR ll AS LOG NO-UNDO.    
    DO WITH FRAME {&FRAME-NAME}:
      EMPTY TEMP-TABLE tt-job-item . 
      IF ipcType EQ "job-mat" THEN do:
        FOR EACH job-mat NO-LOCK
            WHERE job-mat.company = cocode
            AND job-mat.job-no = ipcJobNo 
            AND job-mat.job-no2 = ipiJobNo2 
            BY job-mat.frm BY job-mat.blank-no :
            ll = NO .
            /*RUN jc/maydeletejob-mat.p (BUFFER job-mat, OUTPUT ll).*/
       
            FIND FIRST tt-job-item WHERE tt-job-item.tt-rowid = ROWID(job-mat)
                NO-ERROR.
            IF NOT AVAILABLE tt-job-item THEN 
            DO:
                CREATE tt-job-item.
                ASSIGN 
                    tt-job-item.tt-rowid = ROWID(job-mat)
                    tt-job-item.frm      = job-mat.frm  
                    tt-job-item.blank-no = job-mat.blank-no
                    tt-job-item.rm-i-no  = job-mat.rm-i-no
                    tt-job-item.mat-alloc = job-mat.all-flg.
            END.
        END.
      END. /* Job-mat*/
      ELSE IF ipcType EQ "job-mch" THEN DO:
        FOR EACH job-mch NO-LOCK
            WHERE job-mch.company = cocode
            AND job-mch.job-no = ipcJobNo 
            AND job-mch.job-no2 = ipiJobNo2 
            BY job-mch.frm BY job-mch.blank-no :
       
            FIND FIRST tt-job-item WHERE tt-job-item.tt-rowid = ROWID(job-mch)
                NO-ERROR.
            IF NOT AVAILABLE tt-job-item THEN 
            DO:
                CREATE tt-job-item.
                ASSIGN 
                    tt-job-item.tt-rowid = ROWID(job-mch)
                    tt-job-item.frm      = job-mch.frm  
                    tt-job-item.blank-no = job-mch.blank-no
                    tt-job-item.rm-i-no   = job-mch.m-code
                    tt-job-item.mach-id   = job-mch.m-code .
            END.
        END.
      END. /* ipcType EQ "job-mch"*/
    END.

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
    OPEN QUERY BROWSE-3 FOR EACH tt-job-item
        NO-LOCK BY tt-job-item.frm.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */



