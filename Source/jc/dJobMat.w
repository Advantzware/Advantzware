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

USING system.SharedConfig.

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ipType AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iprwRowId AS ROWID NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiForm AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiBlank AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcRmItem AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcRmItemDesc AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopdAllocation AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopdAvailQty AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE scInstance AS CLASS system.SharedConfig NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK RECT-30 Btn_Cancel RECT-21 iForm ~
iBlank rmItem dAllocation  
&Scoped-Define DISPLAYED-OBJECTS iForm iBlank rmItem cItemDesc dAllocation ~
dAvailQty 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
    IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
    LABEL "Cancel" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
    IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U NO-FOCUS FLAT-BUTTON
    LABEL "&Save" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .

DEFINE VARIABLE cItemDesc   AS CHARACTER FORMAT "X(35)":U 
    VIEW-AS FILL-IN 
    SIZE 38.8 BY 1 
    BGCOLOR 15 FGCOLOR 1 NO-UNDO.

DEFINE VARIABLE dAllocation AS DECIMAL   FORMAT "->>,>>>,>>9.99<<<<":U INITIAL 0 
    LABEL "Allocation" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE dAvailQty   AS DECIMAL   FORMAT "->>>,>>>,>>9.99<<<<":U INITIAL 0 
    LABEL "Qty Available" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 
    BGCOLOR 15 FGCOLOR 1 NO-UNDO.

DEFINE VARIABLE iBlank      AS INTEGER   FORMAT ">>":U INITIAL 0 
    LABEL "Blank" 
    VIEW-AS FILL-IN 
    SIZE 6.6 BY 1 NO-UNDO.

DEFINE VARIABLE iForm       AS INTEGER   FORMAT ">9":U INITIAL 0 
    LABEL "Form" 
    VIEW-AS FILL-IN 
    SIZE 6.6 BY 1
    BGCOLOR 15 FGCOLOR 1 NO-UNDO.

DEFINE VARIABLE rmItem      AS CHARACTER FORMAT "X(10)" 
    LABEL "Item No" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE RECTANGLE RECT-21
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 19 BY 2.38
    BGCOLOR 15 .

DEFINE RECTANGLE RECT-30
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 90.2 BY 6.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    Btn_OK AT ROW 8 COL 73 WIDGET-ID 74
    Btn_Cancel AT ROW 8 COL 81.6 WIDGET-ID 68
    iForm AT ROW 1.71 COL 15 COLON-ALIGNED WIDGET-ID 66
    iBlank AT ROW 1.71 COL 37.2 COLON-ALIGNED WIDGET-ID 60
    rmItem AT ROW 3.43 COL 15 COLON-ALIGNED
    cItemDesc AT ROW 3.43 COL 37 COLON-ALIGNED NO-LABELS
    dAllocation AT ROW 5.29 COL 15 COLON-ALIGNED WIDGET-ID 62
    dAvailQty AT ROW 5.29 COL 54.6 COLON-ALIGNED WIDGET-ID 64
    RECT-30 AT ROW 1.19 COL 1.2 WIDGET-ID 56
    RECT-21 AT ROW 7.71 COL 71.6 WIDGET-ID 72
    SPACE(3.19) SKIP(0.38)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6
    TITLE "Add New Material" WIDGET-ID 100.


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
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.

/* SETTINGS FOR FILL-IN cItemDesc IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Add New Material */
    DO:     
        IF VALID-OBJECT(scInstance) THEN
        DELETE OBJECT scInstance NO-ERROR.
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
    DO:
        ASSIGN
            iopiForm       = 0
            iopiBlank      = 0
            iopcRmItem     = ""
            iopdAllocation = 0.  
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Save */
    DO:
        DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO.
        IF ipType EQ "Add" THEN
        DO:        
            RUN valid-blank-no (OUTPUT lReturnError)NO-ERROR.
            IF lReturnError THEN RETURN NO-APPLY.
           
            RUN valid-frm (OUTPUT lReturnError)NO-ERROR.
            IF lReturnError THEN RETURN NO-APPLY. 
        END.
              
        RUN valid-rm-i-no (OUTPUT lReturnError)NO-ERROR.
        IF lReturnError THEN RETURN NO-APPLY.
       
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN
                iopiForm       = INTEGER(iForm:SCREEN-VALUE)
                iopiBlank      = INTEGER(iBlank:SCREEN-VALUE)
                iopcRmItem     = rmItem:SCREEN-VALUE 
                iopdAllocation = DECIMAL(dAllocation:SCREEN-VALUE).
        END. 
        oplCreated = YES.
        APPLY "go" TO FRAME {&FRAME-NAME}.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME rmItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rmItem Dialog-Frame
ON HELP OF rmItem IN FRAME Dialog-Frame /* Item */
    DO:
        DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO. 
        
        scInstance = SharedConfig:instance.
        scInstance:SetValue("ShowOnlyRealMat","Yes").
  
        RUN windows/l-item5.w (ipCompany,rmItem:SCREEN-VALUE , OUTPUT cReturnValue).
        IF cReturnValue <> "" THEN 
        DO:
            FIND ITEM WHERE RECID(ITEM) EQ int(cReturnValue) NO-LOCK NO-ERROR.
            IF AVAILABLE ITEM THEN 
            DO:
                ASSIGN
                    rmItem:SCREEN-VALUE    = ITEM.i-no
                    cItemDesc:SCREEN-VALUE = ITEM.i-dscr
                    dAvailQty:SCREEN-VALUE = STRING(ITEM.q-onh - item.q-comm).
            END.
        END.
        scInstance:DeleteValue(INPUT "ShowOnlyRealMat").
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
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
    FIND FIRST job NO-LOCK WHERE ROWID(job) EQ iprwRowId NO-ERROR.

    ASSIGN   
        iForm       = (iopiForm)
        iBlank      = (iopiBlank)     
        rmItem      = iopcRmItem   
        dAllocation = (iopdAllocation)
        dAvailQty   = (iopdAvailQty)
        cItemDesc   = iopcRmItemDesc.

    RUN enable_UI.
  
    IF ipType EQ "View" THEN
        ASSIGN
            iForm:SENSITIVE IN FRAME {&FRAME-NAME}       = NO 
            iBlank:SENSITIVE IN FRAME {&FRAME-NAME}      = NO   
            rmItem:SENSITIVE IN FRAME {&FRAME-NAME}      = NO 
            dAllocation:SENSITIVE IN FRAME {&FRAME-NAME} = NO .
    ELSE IF ipType EQ "Update" THEN
        DO:
            ASSIGN
                iForm:SENSITIVE IN FRAME {&FRAME-NAME}  = NO 
                iBlank:SENSITIVE IN FRAME {&FRAME-NAME} = NO . 
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
    DISPLAY iForm iBlank rmItem cItemDesc dAllocation dAvailQty 
        WITH FRAME Dialog-Frame.
    ENABLE Btn_OK RECT-30 Btn_Cancel RECT-21 iForm iBlank rmItem dAllocation           
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-blank-no Dialog-Frame 
PROCEDURE valid-blank-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE li-est-type LIKE est.est-type NO-UNDO.
 
    RELEASE job-hdr.
    RELEASE eb.
        
    IF li-est-type NE 1 THEN
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST est NO-LOCK
            WHERE est.company EQ ipCompany
            AND est.est-no  EQ job.est-no
            NO-ERROR.
        ASSIGN
      
            li-est-type = IF AVAILABLE est THEN est.est-type ELSE 0
            li-est-type = li-est-type - (IF li-est-type GT 4 THEN 4 ELSE 0).
    
        IF INT(iBlank:SCREEN-VALUE ) NE 0 THEN 
        DO:  
       
            IF li-est-type EQ 2 OR li-est-type EQ 6 THEN
                FIND FIRST eb
                    WHERE eb.company  EQ est.company
                    AND eb.est-no   EQ est.est-no
                    AND eb.blank-no EQ INT(iBlank:SCREEN-VALUE )
                    NO-LOCK NO-ERROR.
            ELSE
                FIND FIRST job-hdr
                    WHERE job-hdr.company  EQ job.company
                    AND job-hdr.job      EQ job.job 
                    AND job-hdr.job-no   EQ job.job-no
                    AND job-hdr.job-no2  EQ job.job-no2
                    AND job-hdr.frm      EQ INT(iForm:SCREEN-VALUE )
                    AND job-hdr.blank-no EQ INT(iBlank:SCREEN-VALUE )
                    NO-LOCK NO-ERROR.
            
            IF NOT AVAILABLE job-hdr AND NOT AVAILABLE eb THEN 
            DO:
                MESSAGE "Must enter a valid blank..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO iBlank.
                opReturnError = YES.
                RETURN .
            END.
        END. 
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-frm Dialog-Frame 
PROCEDURE valid-frm :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE li-est-type LIKE est.est-type NO-UNDO.
  
    RELEASE job-hdr.
    RELEASE ef.

    DO WITH FRAME {&frame-name}:
  
        FIND FIRST est NO-LOCK
            WHERE est.company EQ ipCompany
            AND est.est-no  EQ job.est-no
            NO-ERROR.
        ASSIGN
      
            li-est-type = IF AVAILABLE est THEN est.est-type ELSE 0
            li-est-type = li-est-type - (IF li-est-type GT 4 THEN 4 ELSE 0).
      
        IF li-est-type EQ 2 OR li-est-type EQ 6 THEN
            FIND FIRST ef
                WHERE ef.company EQ est.company
                AND ef.est-no  EQ est.est-no
                AND ef.form-no EQ INT(iForm:SCREEN-VALUE )
                NO-LOCK NO-ERROR.
        ELSE
            FIND FIRST job-hdr
                WHERE job-hdr.company EQ ipCompany
                AND job-hdr.job     EQ job.job
                AND job-hdr.job-no  EQ job.job-no
                AND job-hdr.job-no2 EQ job.job-no2
                AND job-hdr.frm     EQ INT(iForm:SCREEN-VALUE )
                NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-hdr AND NOT AVAILABLE ef THEN 
        DO:
            MESSAGE "Must enter a valid form..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO iForm.
            opReturnError = YES.
            RETURN .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-rm-i-no Dialog-Frame 
PROCEDURE valid-rm-i-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        rmItem:SCREEN-VALUE = CAPS(rmItem:SCREEN-VALUE).

        IF NOT CAN-FIND(FIRST ITEM
            WHERE (item.company = ipCompany)
            AND item.i-no EQ rmItem:SCREEN-VALUE)
            THEN 
        DO:
            MESSAGE "Must enter a valid RM..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO rmItem.
            opReturnError = YES.
            RETURN.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
