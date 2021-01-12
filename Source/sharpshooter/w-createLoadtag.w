&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: sharpshooter/w-createLoadtag.w

  Description: Creates a load tag from different sources

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
USING jc.JobHeader.
USING fg.ItemFG.

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
 
DEFINE VARIABLE oJobHeader AS JobHeader NO-UNDO.
DEFINE VARIABLE oItemFG    AS ItemFG    NO-UNDO.

RUN spGetSessionParam ("Company", OUTPUT cCompany).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-34 rHighlight btJob btPO ~
btRelease btReturn btReprint btSplit btPrint 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-loadtags-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fgfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_qtyunits AS HANDLE NO-UNDO.
DEFINE VARIABLE h_userfields AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCreate 
     LABEL "Create" 
     SIZE 20 BY 2.24.

DEFINE BUTTON btJob 
     LABEL "Job" 
     SIZE 22.4 BY 1.43.

DEFINE BUTTON btPO 
     LABEL "PO" 
     SIZE 22.4 BY 1.43.

DEFINE BUTTON btPrint 
     IMAGE-UP FILE "Graphics/32x32/printer.ico":U
     LABEL "Print" 
     SIZE 13 BY 10.33.

DEFINE BUTTON btRelease 
     LABEL "Release" 
     SIZE 22.4 BY 1.43.

DEFINE BUTTON btReprint 
     LABEL "Re-Print" 
     SIZE 22.4 BY 1.43.

DEFINE BUTTON btReturn 
     LABEL "Return" 
     SIZE 22.4 BY 1.43.

DEFINE BUTTON btSplit 
     LABEL "Split" 
     SIZE 22.4 BY 1.43.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 203.2 BY 18.62
     BGCOLOR 1 FGCOLOR 1 .

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 203.2 BY 1.67
     BGCOLOR 3 .

DEFINE RECTANGLE rHighlight
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 23.2 BY 1.67
     BGCOLOR 14 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btJob AT ROW 1.38 COL 4.2 WIDGET-ID 2
     btPO AT ROW 1.38 COL 28.6 WIDGET-ID 24
     btRelease AT ROW 1.38 COL 52.6 WIDGET-ID 48
     btReturn AT ROW 1.38 COL 76.6 WIDGET-ID 50
     btReprint AT ROW 1.38 COL 100.4 WIDGET-ID 52
     btSplit AT ROW 1.38 COL 124.2 WIDGET-ID 54
     btCreate AT ROW 3.14 COL 186.2 WIDGET-ID 58
     btPrint AT ROW 21.62 COL 194.2 WIDGET-ID 30
     RECT-1 AT ROW 2.86 COL 3.8 WIDGET-ID 4
     RECT-34 AT ROW 1.29 COL 3.8 WIDGET-ID 18
     rHighlight AT ROW 1.29 COL 3.8 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 208 BY 31.86
         BGCOLOR 15 FONT 17 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Create LoadTag"
         HEIGHT             = 31.86
         WIDTH              = 208
         MAX-HEIGHT         = 32.81
         MAX-WIDTH          = 208
         VIRTUAL-HEIGHT     = 32.81
         VIRTUAL-WIDTH      = 208
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON btCreate IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Create LoadTag */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Create LoadTag */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCreate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCreate W-Win
ON CHOOSE OF btCreate IN FRAME F-Main /* Create */
DO:
    RUN state-changed (
        INPUT THIS-PROCEDURE,
        INPUT "create-tags"
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btJob W-Win
ON CHOOSE OF btJob IN FRAME F-Main /* Job */
DO:
    RUN select-page(1).

    rHighlight:X = SELF:X - 2.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPO W-Win
ON CHOOSE OF btPO IN FRAME F-Main /* PO */
DO:
    RUN select-page(2).
    
    rHighlight:X = SELF:X - 2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrint W-Win
ON CHOOSE OF btPrint IN FRAME F-Main /* Print */
DO:
    RUN state-changed (
        INPUT THIS-PROCEDURE,
        INPUT "print-tags"
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRelease
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRelease W-Win
ON CHOOSE OF btRelease IN FRAME F-Main /* Release */
DO:
    RUN select-page(3).

    rHighlight:X = SELF:X - 2.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReprint W-Win
ON CHOOSE OF btReprint IN FRAME F-Main /* Re-Print */
DO:
    RUN select-page(5).
    
    rHighlight:X = SELF:X - 2.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReturn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReturn W-Win
ON CHOOSE OF btReturn IN FRAME F-Main /* Return */
DO:
    RUN select-page(4).

    rHighlight:X = SELF:X - 2.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSplit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSplit W-Win
ON CHOOSE OF btSplit IN FRAME F-Main /* Split */
DO:
    RUN select-page(6).

    rHighlight:X = SELF:X - 2.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
{custom/initializeprocs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/b-loadtags.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-loadtags-3 ).
       RUN set-position IN h_b-loadtags-3 ( 21.67 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-loadtags-3 ( 10.29 , 190.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-loadtags-3. */
       RUN add-link IN adm-broker-hdl ( h_b-loadtags-3 , 'LOADTAG':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-loadtags-3 ,
             btPrint:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/jobfilter.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_jobfilter ).
       RUN set-position IN h_jobfilter ( 3.14 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 3.33 , 76.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/fgfilter.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_fgfilter ).
       RUN set-position IN h_fgfilter ( 7.10 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 3.76 , 85.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/qtyunits.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_qtyunits ).
       RUN set-position IN h_qtyunits ( 11.14 , 4.80 ) NO-ERROR.
       /* Size in UIB:  ( 9.24 , 131.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/userfields.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_userfields ).
       RUN set-position IN h_userfields ( 11.24 , 138.20 ) NO-ERROR.
       /* Size in UIB:  ( 9.62 , 68.00 ) */

       /* Links to SmartObject h_jobfilter. */
       RUN add-link IN adm-broker-hdl ( h_jobfilter , 'JOB':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_jobfilter , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_fgfilter. */
       RUN add-link IN adm-broker-hdl ( h_fgfilter , 'FGITEM':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_fgfilter , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_qtyunits. */
       RUN add-link IN adm-broker-hdl ( h_qtyunits , 'QTY':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_userfields. */
       RUN add-link IN adm-broker-hdl ( h_userfields , 'State':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_userfields , 'USERFIELD':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_jobfilter ,
             btSplit:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_fgfilter ,
             btCreate:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_qtyunits ,
             h_fgfilter , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_userfields ,
             h_qtyunits , 'AFTER':U ).
    END. /* Page 1 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  ENABLE RECT-1 RECT-34 rHighlight btJob btPO btRelease btReturn btReprint 
         btSplit btPrint 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

    DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

    DEFINE VARIABLE cJobNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFormNo  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBlankNo AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cItemID  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iQuantity          AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQuantityInSubUnit AS INTEGER NO-UNDO.
    DEFINE VARIABLE iSubUnitsPerUnit   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCopies            AS INTEGER NO-UNDO.

    DEFINE VARIABLE cUserField1      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserField2      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserField3      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserFieldValue1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserFieldValue2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserFieldValue3 AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dOvers AS DECIMAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.    
    
    CASE p-state:
        WHEN "job-invalid" THEN DO:
            {methods/run_link.i "QTY-SOURCE" "DisableQuantities"}
            {methods/run_link.i "FGITEM-SOURCE" "DisableFGItem"}   
            {methods/run_link.i "USERFIELD-SOURCE" "DisableUserFields"}
            btCreate:SENSITIVE = FALSE.         
        END.
        WHEN "job-valid" THEN DO:
            
            {methods/run_link.i "JOB-SOURCE" "GetJobHeader" "(OUTPUT oJobHeader)"}
            
            ASSIGN
                cJobNo   = oJobHeader:GetValue("JobNo")
                iJobNo2  = INTEGER(oJobHeader:GetValue("JobNo2"))
                iFormNo  = INTEGER(oJobHeader:GetValue("FormNo"))
                iBlankNo = INTEGER(oJobHeader:GetValue("BlankNo"))
                .

            {methods/run_link.i "FGITEM-SOURCE" "UpdateItemForJob" "(INPUT cCompany, INPUT cJobNo, INPUT iJobNo2, INPUT iFormNo, INPUT iBlankNo)"}
        END.
        WHEN "fgitem-valid" THEN DO:
            
            {methods/run_link.i "QTY-SOURCE" "EnableQuantities"}
            {methods/run_link.i "USERFIELD-SOURCE" "EnableUserFields"}
            
            {methods/run_link.i "FGITEM-SOURCE" "GetItemFG" "(OUTPUT oItemFG)"}
            {methods/run_link.i "JOB-SOURCE" "GetJobHeader" "(OUTPUT oJobHeader)"}
            
            ASSIGN
                iQuantityInSubUnit = INTEGER(oItemFG:GetValue("QuantityInSubUnit"))
                iSubUnitsPerUnit   = INTEGER(oItemFG:GetValue("SubUnitsPerUnit"))
/*                iQuantity        = INTEGER(oJobHeader:GetValue("Quantity"))*/
                iQuantity          = iQuantityInSubUnit * iSubUnitsPerUnit
                .
            
            {methods/run_link.i "QTY-SOURCE" "SetQuantities" "(INPUT iQuantity, INPUT iQuantityInSubUnit, INPUT iSubUnitsPerUnit, INPUT 0)"}
            {methods/run_link.i "QTY-SOURCE" "SetOvers" "(INPUT 0)"} 
                       
            btCreate:SENSITIVE = TRUE.         
        END.
        WHEN "overs-changed" THEN DO:
            {methods/run_link.i "USERFIELD-SOURCE" "GetOvers" "(OUTPUT dOvers)" }
            
            {methods/run_link.i "QTY-SOURCE" "SetOvers" "(INPUT dOvers)"}
        END.
        WHEN "create-tags" THEN DO:
            {methods/run_link.i "JOB-SOURCE" "GetJobHeader" "(OUTPUT oJobHeader)"}
            
            ASSIGN
                cJobNo   = oJobHeader:GetValue("JobNo")
                iJobNo2  = INTEGER(oJobHeader:GetValue("JobNo2"))
                iFormNo  = INTEGER(oJobHeader:GetValue("FormNo"))
                iBlankNo = INTEGER(oJobHeader:GetValue("BlankNo"))
                .
            
            {methods/run_link.i "FGITEM-SOURCE" "GetItemFG" "(OUTPUT oItemFG)"}
            
            cItemID = oItemFG:GetValue("ItemID").
            
            {methods/run_link.i "QTY-SOURCE" "GetQuantities" "(OUTPUT iQuantity, OUTPUT iQuantityInSubUnit, OUTPUT iSubUnitsPerUnit, OUTPUT iCopies)"}
            {methods/run_link.i "USERFIELD-SOURCE" "GetUserFields" "(OUTPUT cUserField1, OUTPUT cUserField2, OUTPUT cUserField3, OUTPUT cUserFieldValue1, OUTPUT cUserFieldValue2, OUTPUT cUserFieldValue3)" }
            
            {methods/run_link.i "LOADTAG-SOURCE" "BuildLoadTagsFromJob" "(INPUT cCompany, INPUT cJobNo, INPUT iJobNo2, INPUT iFormNo, INPUT iBlankNo, INPUT cItemID, INPUT iQuantity, INPUT iQuantityInSubUnit, INPUT iSubUnitsPerUnit, INPUT iCopies, INPUT cUserField1, INPUT cUserField2, INPUT cUserField3, INPUT cUserFieldValue1, INPUT cUserFieldValue2, INPUT cUserFieldValue3)" }
        END. 
        WHEN "print-tags" THEN DO:
            {methods/run_link.i "LOADTAG-SOURCE" "CreateLoadTagFromTT"}
            
            MESSAGE "Process complete!"
            VIEW-AS ALERT-BOX INFORMATION.
        END.
    END CASE.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

