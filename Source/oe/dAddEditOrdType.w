&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: est\dAddEditOrdType.w
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.   /* add,update,view */ 
DEFINE INPUT-OUTPUT PARAMETER opiOrderTypeID AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER opcItemDscr AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER opcTypeSource AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER opiTypeSeq AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER oplInactive AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER opcTypeEst AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER opiTypeColor AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER oplUpdate AS LOGICAL NO-UNDO.
    
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.

RUN spGetSessionParam ("Company", OUTPUT cCompany).

DEFINE VARIABLE char-val        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lRecFound       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cRtnChar        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cNewOrderEntry  AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cCompany, "NewOrderEntry", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    cNewOrderEntry = cRtnChar NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Done Btn_Cancel tb_inactive ~
item-dscr type-source type-color type-est type-seq RECT-21 RECT-38 
&Scoped-Define DISPLAYED-OBJECTS type-id tb_inactive item-dscr type-source ~
type-color type-est type-seq fiBGColor fiFileLoc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bResetColor 
     LABEL "(Reset)" 
     SIZE 10 BY .71
     FONT 0.

DEFINE BUTTON Btn_Cancel 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
     LABEL "&Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U NO-FOCUS FLAT-BUTTON
     LABEL "&Save" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE VARIABLE type-source AS CHARACTER FORMAT "X(10)":U 
     LABEL "Order Type Source" 
     VIEW-AS COMBO-BOX INNER-LINES 9
     LIST-ITEM-PAIRS "Customer","Customer",
                     "Estimate","Estimate",
                     "Quote","Quote",
                     "File","File",
                     "Order","Order",
                     "Web","Web",
                     "Future Use","Future Use"
     DROP-DOWN-LIST
     SIZE 21.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fiBGColor AS CHARACTER FORMAT "X(256)":U INITIAL "Color" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 8.8 BY .95 NO-UNDO.       

DEFINE VARIABLE fiFileLoc AS CHARACTER FORMAT "X(256)":U INITIAL "" 
     LABEL "File Path"
     VIEW-AS FILL-IN  
     SIZE 60.8 BY .95 NO-UNDO.

DEFINE VARIABLE item-dscr AS CHARACTER FORMAT "X(32)":U 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE type-color AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Color" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE type-est AS CHARACTER FORMAT "X(1)":U 
     LABEL "Estimate Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "All","",
                     "Single","1",
                     "Set","2",
                     "Tandem","3",
                     "Distribution","4"
     DROP-DOWN-LIST 
     SIZE 21.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE type-id AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Type Id" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE type-seq AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Sequence" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE rBgColor
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 9 BY 1.91
     BGCOLOR 21 FGCOLOR 21 .

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 19 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 124.8 BY 8.62
     BGCOLOR 15 .

DEFINE VARIABLE tb_inactive AS LOGICAL INITIAL no 
     LABEL "Inactive" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 10.57 COL 108
     Btn_Done AT ROW 10.86 COL 109
     Btn_Cancel AT ROW 10.57 COL 117
     type-id AT ROW 2 COL 18.6 COLON-ALIGNED WIDGET-ID 208
     tb_inactive AT ROW 2 COL 47.8 WIDGET-ID 338
     item-dscr AT ROW 3.24 COL 18.6 COLON-ALIGNED WIDGET-ID 210
     type-source AT ROW 3.24 COL 93.2 COLON-ALIGNED WIDGET-ID 328
     type-color AT ROW 4.67 COL 44.8 COLON-ALIGNED WIDGET-ID 330
     type-est AT ROW 4.57 COL 93.2 COLON-ALIGNED WIDGET-ID 336
     type-seq AT ROW 4.57 COL 18.6 COLON-ALIGNED WIDGET-ID 332
     fiBGColor AT ROW 6.1 COL 14.2 COLON-ALIGNED NO-LABEL WIDGET-ID 344
     bResetColor AT ROW 8.24 COL 28.2 WIDGET-ID 342
     "Order Type" VIEW-AS TEXT
          SIZE 14 BY .71 AT ROW 1 COL 5 WIDGET-ID 206
     RECT-21 AT ROW 10.33 COL 107
     RECT-38 AT ROW 1.43 COL 1.2
     rBgColor AT ROW 7.24 COL 16.6 WIDGET-ID 340
     fiFileLoc AT ROW 9.0 COL 45.6 
     SPACE(10.39) SKIP(1.89)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Add/Update Order Type".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bResetColor IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiBGColor IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
   
ASSIGN 
       fiBGColor:READ-ONLY IN FRAME Dialog-Frame        = TRUE.
       
/* SETTINGS FOR FILL-IN fiFileLoc IN FRAME Dialog-Frame
   NO-ENABLE                                                            */ 
   
ASSIGN 
       fiFileLoc:READ-ONLY IN FRAME Dialog-Frame        = TRUE.   

/* SETTINGS FOR RECTANGLE rBgColor IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tb_inactive:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       type-color:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN type-id IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
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
ON HELP OF FRAME Dialog-Frame /* Add/Update Order Type */
DO:
        DEFINE VARIABLE char-val   AS cha    NO-UNDO.
        DEFINE VARIABLE lv-handle  AS HANDLE NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID  NO-UNDO .
        
        CASE FOCUS:NAME :
            WHEN "rmItemID" THEN 
                DO:
                    
                END.
            
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Add/Update Order Type */
ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Add/Update Order Type */
DO:
        
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.   
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bResetColor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bResetColor Dialog-Frame
ON CHOOSE OF bResetColor IN FRAME Dialog-Frame /* (Reset) */
DO:
       
        ASSIGN 
            rBgColor:FGCOLOR = ?
            rBgColor:BGCOLOR = ?
            type-color:SCREEN-VALUE = ?.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:              
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done Dialog-Frame
ON CHOOSE OF Btn_Done IN FRAME Dialog-Frame /* Done */
DO:
        
  &IF DEFINED (adm-panel) NE 0 &THEN
        RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
        APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Save */
DO:
        DEFINE VARIABLE ld              AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE lValidateResult AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lError          AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.         
        DEFINE VARIABLE iOrderTypeId    AS INTEGER   NO-UNDO.        
        IF ipcType EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.              
       
        DO TRANSACTION:           

            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&displayed-objects}.
            END.            
        END.  
        
        ASSIGN
            opiOrderTypeID  = type-id        
            opcItemDscr     = item-dscr      
            opcTypeSource   = type-source   
            opiTypeColor    = type-color     
            opiTypeSeq      = type-seq       
            opcTypeEst      = type-est         
            oplInactive     = tb_inactive    
            oplUpdate       = YES 
            .   
                
        APPLY "go" TO FRAME {&FRAME-NAME}.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rBgColor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rBgColor Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF rBgColor IN FRAME Dialog-Frame
DO:
    DEFINE VARIABLE red   AS INTEGER NO-UNDO.
    DEFINE VARIABLE blue  AS INTEGER NO-UNDO INITIAL 127.
    DEFINE VARIABLE green AS INTEGER NO-UNDO INITIAL 127.
    DEFINE VARIABLE ix    AS INTEGER NO-UNDO.
    DEFINE VARIABLE lSave AS LOG NO-UNDO. 
    DEFINE VARIABLE iReturnValue AS INTEGER NO-UNDO.
   
    RUN windows/lColor.w( INPUT integer(type-color:SCREEN-VALUE), OUTPUT iReturnValue).      
    
        ASSIGN 
            rBgColor:FGCOLOR = iReturnValue
            rBgColor:BGCOLOR = iReturnValue
            type-color:SCREEN-VALUE = STRING(iReturnValue).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-color
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-color Dialog-Frame
ON LEAVE OF type-color IN FRAME Dialog-Frame /* Color */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-est Dialog-Frame
ON LEAVE OF type-est IN FRAME Dialog-Frame /* Estimate Type */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
            
        END.
               
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-id Dialog-Frame
ON LEAVE OF type-id IN FRAME Dialog-Frame /* Type Id */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-seq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-seq Dialog-Frame
ON LEAVE OF type-seq IN FRAME Dialog-Frame /* Sequence */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-source
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-source Dialog-Frame
ON VALUE-CHANGED OF type-source IN FRAME Dialog-Frame /* Order Type Source */
DO:          
        RUN pSetField.                
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpd.i} 
SESSION:DATA-ENTRY-RETURN = YES.       

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK: 
        
    IF ipcType NE "view" THEN 
    DO: 
        
        RUN enable_UI.
        RUN display-item. 
        
        btn_done:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    END.
    ELSE 
    DO:
        RUN display-item.
        ASSIGN 
            btn_done:HIDDEN IN FRAME {&FRAME-NAME} = NO.
        btn_done:SENSITIVE                        = YES.
        btn_ok:HIDDEN                             = YES.
        btn_cancel:HIDDEN                         = YES.
    END.
    RUN pSetField.
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item Dialog-Frame 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
                          Purpose:     
                          PARAMs:  <none>
                          Notes:       
         ------------------------------------------------------------------------------*/              
        
    ASSIGN
        type-id       = opiOrderTypeID
        item-dscr     = opcItemDscr
        type-source   = opcTypeSource
        type-color    = opiTypeColor
        type-seq      = opiTypeSeq
        type-est      = opcTypeEst  
        tb_inactive   = oplInactive
        .    
      
    IF ipcType EQ "Copy" OR ipcType EQ "Add" THEN 
    DO:
       ASSIGN
           type-id = CURRENT-VALUE(orderTypeID_seq) + 1
           type-seq = 1           
           rBgColor:FGCOLOR IN FRAME {&frame-name} = ?
           rBgColor:BGCOLOR IN FRAME {&frame-name} = ?
           type-color:SCREEN-VALUE IN FRAME {&frame-name} = ?
           type-source = "Customer"
           type-color = ?.
           .
    END.
    
    DISPLAY   
        type-id item-dscr type-source type-color type-seq 
        type-est tb_inactive
        WITH FRAME Dialog-Frame.       
                                            
    IF ipcType NE "view" THEN 
    DO:
        ENABLE  Btn_Cancel Btn_OK WITH FRAME Dialog-Frame.
        ASSIGN 
            rBgColor:SENSITIVE IN FRAME {&frame-name} = TRUE
            bResetColor:SENSITIVE IN FRAME {&frame-name} = TRUE.
    END.

    type-color:HIDDEN IN FRAME {&frame-name} = TRUE.
    type-seq:SENSITIVE IN FRAME {&frame-name} = FALSE.
    VIEW FRAME {&FRAME-NAME}.       
    
    ASSIGN 
        rBgColor:FGCOLOR = type-color
        rBgColor:BGCOLOR = type-color.
        
    APPLY "entry" TO item-dscr IN FRAME {&FRAME-NAME}.
   

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
  DISPLAY type-id tb_inactive item-dscr type-source type-color type-est type-seq 
          fiBGColor fiFileLoc
      WITH FRAME Dialog-Frame.
  ENABLE Btn_OK Btn_Done Btn_Cancel tb_inactive item-dscr type-source 
         type-color type-est type-seq RECT-21 RECT-38 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit Dialog-Frame 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
                 Purpose:
                 Notes:
                ------------------------------------------------------------------------------*/


    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

/* Code placed here will execute AFTER standard behavior.    */
    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetField Dialog-Frame 
PROCEDURE pSetField :
/*------------------------------------------------------------------------------
                      Purpose:     
                      PARAMs:  <none>
                      Notes:       
     ------------------------------------------------------------------------------*/   
                                
    IF type-source:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "Estimate" THEN 
    DO:
        type-est:HIDDEN IN FRAME {&FRAME-NAME} = NO.
        fiFileLoc:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    END.
    ELSE IF type-source:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "File" THEN
    DO:
       type-est:HIDDEN IN FRAME {&FRAME-NAME} = YES.
       fiFileLoc:HIDDEN IN FRAME {&FRAME-NAME} = NO.
       fiFileLoc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cNewOrderEntry.
    END.
    ELSE 
    DO:    
     type-est:HIDDEN IN FRAME {&FRAME-NAME} = YES.
     fiFileLoc:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    END. 
     
     IF INTEGER(type-id:SCREEN-VALUE IN FRAME {&FRAME-NAME}) GE 1 AND INTEGER(type-id:SCREEN-VALUE IN FRAME {&FRAME-NAME}) LE 9 THEN
     DO:
        DISABLE item-dscr type-source  WITH FRAME Dialog-Frame .            
     END.
     
     
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

