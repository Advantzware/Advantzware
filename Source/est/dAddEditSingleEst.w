&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: est\dAddEditComp.w
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ip-recid  AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-rowid  AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER ip-type   AS CHARACTER NO-UNDO.   /* add,update,view */
DEFINE OUTPUT PARAMETER op-rowid AS ROWID     NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i new shared}
{est\ttInputEst.i}

ASSIGN 
    cocode = g_company
    locode = g_loc.

DEFINE VARIABLE char-val        AS CHARACTER NO-UNDO.

DEFINE VARIABLE lv-item-recid   AS RECID     NO-UNDO.
DEFINE VARIABLE ll-order-warned AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ll-new-record   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ilogic          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMaterialType   AS CHARACTER INITIAL "C,5,6,M,D" NO-UNDO .
DEFINE VARIABLE k_frac          AS DECIMAL   INIT 6.25 NO-UNDO.
DEFINE VARIABLE v-count         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iBlank          AS INTEGER   NO-UNDO INITIAL 1.

{Inventory/ttInventory.i "NEW SHARED"}

{sys/inc/f16to32.i}

IF v-cecscrn-dec THEN
DO:
    DEFINE TEMP-TABLE tt-64-dec NO-UNDO
        FIELD DEC AS DECIMAL DECIMALS 6.

    DO v-count = 0 TO 63:
        CREATE tt-64-dec.
        tt-64-dec.DEC = v-count / 64.0.
        RELEASE tt-64-dec.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cCustPart iNumOnWidth Btn_OK Btn_Cancel ~
Btn_Done RECT-21 RECT-38 
&Scoped-Define DISPLAYED-OBJECTS cCustPart iNumOnWidth style-cod style-dscr ~
fg-cat cat-dscr len wid dep item-name item-dscr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
     LABEL "&Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Save" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE VARIABLE cat-dscr AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cCustPart AS CHARACTER FORMAT "X(15)":U 
     LABEL "Cust Part#" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE dep AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "D" 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fg-cat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Category" 
     VIEW-AS FILL-IN 
     SIZE 14.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iNumOnWidth AS INTEGER FORMAT ">>9":U INITIAL 1 
     LABEL "# Up" 
     VIEW-AS FILL-IN 
     SIZE 8.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE item-dscr AS CHARACTER FORMAT "X(30)":U 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE item-name AS CHARACTER FORMAT "X(30)":U 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE len AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "L" 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE style-cod AS CHARACTER FORMAT "X(8)":U 
     LABEL "Style Code" 
     VIEW-AS FILL-IN 
     SIZE 14.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE style-dscr AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE wid AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "W" 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 19 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 92.8 BY 9.57
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     cCustPart AT ROW 1.71 COL 15 COLON-ALIGNED WIDGET-ID 88
     iNumOnWidth AT ROW 2.91 COL 15 COLON-ALIGNED WIDGET-ID 178
     Btn_OK AT ROW 11.71 COL 76
     style-cod AT ROW 8.14 COL 15 COLON-ALIGNED WIDGET-ID 180
     style-dscr AT ROW 8.14 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 182
     Btn_Cancel AT ROW 11.71 COL 85
     fg-cat AT ROW 9.38 COL 15 COLON-ALIGNED WIDGET-ID 196
     cat-dscr AT ROW 9.43 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 214
     len AT ROW 4.1 COL 15 COLON-ALIGNED WIDGET-ID 190
     wid AT ROW 4.1 COL 30.2 COLON-ALIGNED WIDGET-ID 194
     dep AT ROW 4.1 COL 44.8 COLON-ALIGNED WIDGET-ID 192
     item-name AT ROW 5.52 COL 15 COLON-ALIGNED WIDGET-ID 322
     item-dscr AT ROW 6.76 COL 15 COLON-ALIGNED WIDGET-ID 210
     Btn_Done AT ROW 12 COL 77
     "Part Number Entry" VIEW-AS TEXT
          SIZE 25 BY .71 AT ROW 1 COL 5 WIDGET-ID 206
     RECT-21 AT ROW 11.48 COL 75
     RECT-38 AT ROW 1.43 COL 1.2
     SPACE(1.19) SKIP(3.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Add/Update Mold".


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
/*{methods/template/viewer.i} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cat-dscr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dep IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fg-cat IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN item-dscr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN item-name IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN len IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN style-cod IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN style-dscr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN wid IN FRAME Dialog-Frame
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
ON HELP OF FRAME Dialog-Frame /* Add/Update Mold */
DO:
        DEFINE VARIABLE char-val   AS cha    NO-UNDO.
        DEFINE VARIABLE lv-handle  AS HANDLE NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID  NO-UNDO .
        
        CASE FOCUS:NAME :
            WHEN "cCustPart" THEN 
                DO:
                    RUN windows/l-partno.w (cocode, cCustPart:SCREEN-VALUE, OUTPUT char-val).
                    cCustPart:SCREEN-VALUE = ENTRY(1,char-val).
                    RUN pDisplayPart(cocode, cCustPart:SCREEN-VALUE).
                END.
            
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Add/Update Mold */
ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Add/Update Mold */
DO:
            
        IF AVAILABLE ttInputEst THEN
            op-rowid = ROWID(ttInputEst) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST ttInputEst EXCLUSIVE-LOCK
                WHERE RECID(ttInputEst) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE ttInputEst THEN DELETE ttInputEst .
            op-rowid = ? .
        END.
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.

    /*APPLY "END-ERROR":U TO SELF.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
        
    
        IF AVAILABLE ttInputEst THEN
            op-rowid = ROWID(ttInputEst) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST ttInputEst EXCLUSIVE-LOCK
                WHERE RECID(ttInputEst) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE ttInputEst THEN DELETE ttInputEst .
            op-rowid = ? .
        END. 
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
        DEFINE VARIABLE dCostStorage    AS DECIMAL   NO-UNDO .
        DEFINE VARIABLE dCostHandling   AS DECIMAL   NO-UNDO .
        DEFINE VARIABLE hftp            AS HANDLE    NO-UNDO.

        
        IF ip-type EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.
                
        RUN valid-NumUp(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY. 
        
       
        DO TRANSACTION:           

            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&displayed-objects}.
            END.            
        END.
        IF ip-type EQ "Add" THEN
        DO:
            CREATE ttInputEst.
            ASSIGN
                ttInputEst.cEstType = "NewSetEstimate"
                ttInputEst.cSetType = "Set"
                ttInputEst.cCompany = cocode .
        END.
        
        ASSIGN
            ttInputEst.iFormNo          = 1
            ttInputEst.iBlankNo         = iBlank
            ttInputEst.cPartID          = cCustPart
            ttInputEst.cPartName        = item-name
            ttInputEst.cPartDescription = item-dscr
            ttInputEst.dLength          = len
            ttInputEst.dWidth           = wid
            ttInputEst.dDepth           = dep
            ttInputEst.cCategory        = fg-cat
            ttInputEst.cStyle           = style-cod
            ttInputEst.iNumOnWidth      = iNumOnWidth
            ttInputEst.dQtyPerSet       = 1
            ttInputEst.lPurchased       = FALSE
            .           
        
        op-rowid = ROWID(ttInputEst).
        
        APPLY "go" TO FRAME {&FRAME-NAME}.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iNumOnWidth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iNumOnWidth Dialog-Frame
ON LEAVE OF iNumOnWidth IN FRAME Dialog-Frame /* # Up */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
         RUN valid-NumUp(OUTPUT lError) NO-ERROR.
         IF lError THEN RETURN NO-APPLY.           
        END.
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

    FIND FIRST eb NO-LOCK
        WHERE ROWID(eb) EQ ip-rowid NO-ERROR .
    
    IF ip-type EQ "copy" THEN lv-item-recid = ip-recid.

    /*IF ip-recid EQ ? THEN 
    DO:
        RUN create-item.
    END.
    ELSE FIND estPacking NO-LOCK WHERE RECID(estPacking) EQ ip-recid NO-ERROR.*/

    IF ip-type NE "view" THEN 
    DO: 
        
        RUN enable_UI.
        RUN display-item.

        ASSIGN 
            ll-order-warned = NO.
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
    /*FIND CURRENT estPacking NO-LOCK NO-ERROR .*/
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
    DEFINE BUFFER bf-ttInputEst FOR ttInputEst .
    FIND FIRST ttInputEst WHERE RECID(ttInputEst) EQ ip-recid NO-ERROR. 
    IF AVAILABLE ttInputEst THEN 
        ASSIGN
            iBlank     = ttInputEst.iBlankNo
            iNumOnWidth = ttInputEst.iNumOnWidth
            cCustPart  = ttInputEst.cPartID
            style-cod  = ttInputEst.cStyle
            fg-cat     = ttInputEst.cCategory
            item-name  = ttInputEst.cPartName
            item-dscr  = ttInputEst.cPartDescription
            len        = ttInputEst.dLength
            wid        = ttInputEst.dWidth
            dep        = ttInputEst.dDepth
            .     
    IF ip-type EQ "Add" THEN 
    DO:
        FOR EACH bf-ttInputEst NO-LOCK BREAK BY bf-ttInputEst.iBlank DESC:
            iBlank      =  bf-ttInputEst.iBlank + 1 .
            LEAVE.    
        END.          
    END.
       
    FIND FIRST style NO-LOCK WHERE style.company = cocode
        AND style.style EQ style-cod NO-ERROR .
        
    IF AVAILABLE style THEN
        ASSIGN style-dscr = style.dscr .
    FIND FIRST fgcat NO-LOCK WHERE fgcat.company = cocode
        AND fgcat.procat EQ fg-cat NO-ERROR .
    IF AVAILABLE fgcat THEN
        ASSIGN cat-dscr = fgcat.dscr .
            
    
    DISPLAY   
        iBlank cCustPart  
        style-cod style-dscr fg-cat cat-dscr item-name item-dscr 
        len wid dep
        WITH FRAME Dialog-Frame.       
   
        
    IF ip-type NE "view" THEN 
    DO:
        ENABLE  Btn_Cancel Btn_OK WITH FRAME Dialog-Frame.
    END.

    VIEW FRAME {&FRAME-NAME}. 
    APPLY "entry" TO FRAME {&FRAME-NAME}.
   

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
  DISPLAY cCustPart iNumOnWidth style-cod style-dscr fg-cat cat-dscr len wid dep 
          item-name item-dscr 
      WITH FRAME Dialog-Frame.
  ENABLE cCustPart iNumOnWidth Btn_OK Btn_Cancel Btn_Done RECT-21 RECT-38 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayPart Dialog-Frame 
PROCEDURE pDisplayPart PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCustPart AS CHARACTER NO-UNDO.

IF ipcCustPart NE "" THEN DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST eb NO-LOCK 
        WHERE eb.company EQ ipcCompany
        AND eb.part-no EQ ipcCustPart
        NO-ERROR.
    IF AVAILABLE eb THEN DO:
        ASSIGN 
            len:SCREEN-VALUE = STRING(eb.len)
            wid:SCREEN-VALUE = STRING(eb.wid)
            dep:SCREEN-VALUE = STRING(eb.dep)
            item-name:SCREEN-VALUE = eb.part-dscr1
            item-dscr:SCREEN-VALUE = eb.part-dscr2
            style-cod:SCREEN-VALUE = eb.style
            fg-cat:SCREEN-VALUE = eb.procat
            .
        IF eb.style NE "" THEN 
            FIND FIRST style NO-LOCK 
                WHERE style.company EQ ipcCompany
                AND style.style = eb.style
                NO-ERROR.
        IF AVAILABLE style THEN 
            ASSIGN 
                style-dscr:SCREEN-VALUE = style.dscr.
        IF eb.procat NE "" THEN 
            FIND FIRST fgcat NO-LOCK 
                 WHERE fgcat.company EQ ipcCompany
                 AND fgcat.procat EQ eb.procat
                 NO-ERROR.
         IF AVAILABLE fgcat THEN 
            ASSIGN 
                cat-dscr:SCREEN-VALUE = fgcat.dscr.
    END.
            
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-NumUp Dialog-Frame 
PROCEDURE valid-NumUp :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        IF INTEGER(iNumOnWidth:SCREEN-VALUE)  LE 0 THEN 
        DO:
            MESSAGE "# Up must be greater then 0..." VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO iNumOnWidth.
            oplOutError = YES .
        END.
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

