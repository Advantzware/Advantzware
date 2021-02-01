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
DEFINE INPUT PARAMETER ip-type   AS CHARACTER NO-UNDO.   /* add,update,view */ 
DEFINE INPUT PARAMETER ipiTargetCycles AS INTEGER NO-UNDO.  
DEFINE INPUT PARAMETER ipdBlankSqFt AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER op-rowid AS ROWID     NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i new shared}
{est/ttInputEst.i}

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

{Inventory/ttInventory.i "NEW SHARED"}

{sys/inc/f16to32.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cStock iMolds Btn_OK Btn_Done Btn_Cancel ~
RECT-21 RECT-39 tb_default 
&Scoped-Define DISPLAYED-OBJECTS cStock iMolds tb_default 

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

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
     LABEL "&Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U NO-FOCUS FLAT-BUTTON
     LABEL "&Save" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE VARIABLE cStock AS CHARACTER FORMAT "X(15)":U 
     LABEL "FG Item" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iMolds AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Molds" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 19 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 95.8 BY 3.14
     BGCOLOR 15 .

DEFINE VARIABLE tb_default AS LOGICAL INITIAL no 
     LABEL "Key Item" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.2 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     cStock AT ROW 1.91 COL 15 COLON-ALIGNED WIDGET-ID 176
     iMolds AT ROW 1.86 COL 59 COLON-ALIGNED WIDGET-ID 322
     Btn_OK AT ROW 4.71 COL 78.6
     Btn_Done AT ROW 5 COL 79.6
     Btn_Cancel AT ROW 4.71 COL 87.6
     tb_default AT ROW 1.76 COL 80 WIDGET-ID 324
     RECT-21 AT ROW 4.48 COL 77.6
     RECT-39 AT ROW 1.19 COL 1.2 WIDGET-ID 2
     SPACE(0.79) SKIP(2.80)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Add/Update Item".


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

ASSIGN 
       RECT-39:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       tb_default:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

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
ON HELP OF FRAME Dialog-Frame /* Add/Update Item */
DO:
        DEFINE VARIABLE char-val   AS cha    NO-UNDO.
        DEFINE VARIABLE lv-handle  AS HANDLE NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID  NO-UNDO .
        
        CASE FOCUS:NAME :
            WHEN "cStock" THEN 
                DO:
                    RUN windows/l-itemfg.w  (cocode,"",cStock:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> ""  THEN 
                    DO:
                        cStock:SCREEN-VALUE  = ENTRY(1,char-val).
                        APPLY "entry" TO cStock.                  
                    END.    
                END.
            
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Add/Update Item */
ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Add/Update Item */
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

        DEFINE BUFFER bf-ttInputEst FOR ttInputEst.
        IF ip-type EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.
        
        RUN valid-itemfg(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY.
        
        RUN valid-molds(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY.
        
               
        DO TRANSACTION:           

            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&displayed-objects}.
            END.            
        END.
        IF ip-type EQ "Add" OR ip-type EQ "copy" THEN
        DO:
            CREATE ttInputEst.
            ASSIGN
                ttInputEst.cEstType = "MoldTandem"
                ttInputEst.cSetType = "MoldEstTandem"
                ttInputEst.cCompany = cocode .
        END.
              
        ASSIGN
            ttInputEst.cStockNo  = cStock
            ttInputEst.iMolds    = iMolds 
            ttInputEst.iQuantityYield = ttInputEst.iMolds * ipiTargetCycles

            ttInputEst.lKeyItem = tb_default .                           
                   
        FIND FIRST itemfg NO-LOCK 
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no EQ cStock NO-ERROR .
        IF AVAILABLE itemfg THEN
        DO:
            ASSIGN
                ttInputEst.cPartName = itemfg.i-name 
                ttInputEst.cFgEstNo  = itemfg.est-no
                ttInputEst.dSqFt     = itemfg.t-sqft * iMolds .               
        END.
        IF tb_default THEN
        DO:
          FOR EACH bf-ttInputEst 
              WHERE bf-ttInputEst.cStockNo NE cStock:
              ASSIGN 
                  ttInputEst.lKeyItem = NO .
          END.             
        END.
        
        op-rowid = ROWID(ttInputEst).
        
        APPLY "go" TO FRAME {&FRAME-NAME}.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cStock
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStock Dialog-Frame
ON LEAVE OF cStock IN FRAME Dialog-Frame /* FG Item */
DO:
        DEFINE VARIABLE lValidateResult AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-itemfg(OUTPUT lValidateResult) NO-ERROR.
            IF lValidateResult THEN RETURN NO-APPLY. 
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iMolds
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iMolds Dialog-Frame
ON LEAVE OF iMolds IN FRAME Dialog-Frame /* Molds */
DO:
        DEFINE VARIABLE lValidateResult AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-molds(OUTPUT lValidateResult) NO-ERROR.
            IF lValidateResult THEN RETURN NO-APPLY. 
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
            cStock = ttInputEst.cStockNo
            iMolds = ttInputEst.iMolds
            tb_default = ttInputEst.lKeyItem
            .
   
    IF ip-type EQ "Add" THEN 
    DO:
       
    END.             
    
    DISPLAY   
        cStock iMolds tb_default WITH FRAME Dialog-Frame.    
        
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
  DISPLAY cStock iMolds tb_default 
      WITH FRAME Dialog-Frame.
  ENABLE cStock iMolds Btn_OK Btn_Done Btn_Cancel RECT-21 RECT-39 tb_default 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-itemfg Dialog-Frame 
PROCEDURE valid-itemfg :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE lActive AS LOG       NO-UNDO.
    DEFINE VARIABLE cMsg    AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttInputEst FOR ttInputEst.
    
    DO WITH FRAME {&FRAME-NAME}:
    
        IF NOT CAN-FIND(FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no  EQ cStock:SCREEN-VALUE) THEN 
        DO:
            cMsg =  "Invalid FG Item, try help..." .           
        END.
      
        RUN fg/GetItemfgActInact.p (INPUT cocode,
            INPUT cStock:SCREEN-VALUE,
            OUTPUT lActive).
        IF NOT lActive THEN
            cMsg = cStock:SCREEN-VALUE + " has InActive Status. Job cannot be placed for the Inactive Item.".
                    
        IF cMsg NE "" THEN
        DO:
            MESSAGE cMsg VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cStock .
            oplOutError = YES .
            RETURN.
        END.
        
        FIND FIRST bf-ttInputEst NO-LOCK
            WHERE bf-ttInputEst.cStockNo EQ cStock:SCREEN-VALUE NO-ERROR.
            
        IF AVAIL bf-ttInputEst AND ip-type NE "update" THEN
        DO:
            MESSAGE "Item: " bf-ttInputEst.cStockNo " already added to Head." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cStock .
            oplOutError = YES .            
        END.         
    END.      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-molds Dialog-Frame 
PROCEDURE valid-molds :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        IF INTEGER(iMolds:SCREEN-VALUE)  LE 0 THEN 
        DO:
            MESSAGE "Molds must be greater then 0..." VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO iMolds .
            oplOutError = YES .
        END.
    END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

