&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
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

  File: oe\wAddOrder.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
     
/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER oprwRowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
/* Required for run_link.i */
DEFINE VARIABLE char-hdl         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE pHandle          AS HANDLE       NO-UNDO.

DEFINE VARIABLE hdJobProcs       AS HANDLE       NO-UNDO.
DEFINE VARIABLE cJobNo2ListItems AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iCount           AS INTEGER      NO-UNDO.
DEFINE VARIABLE cFormattedJobno  AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cCompany         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cLocation        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE giOrdType        AS INTEGER      NO-UNDO.
DEFINE VARIABLE gcOrdTypeSource  AS CHARACTER    NO-UNDO.

DEFINE VARIABLE oJobHeader       AS jc.JobHeader NO-UNDO.

RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.

DEFINE VARIABLE cNewOrderEntry AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRtnChar       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lOeprompt      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE dPrice         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cPrUom         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iQty           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iQuoteNumber   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChoice        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lValidPoNo     AS LOG       NO-UNDO.

RUN spGetSessionParam("Company", OUTPUT cCompany).
RUN spGetSessionParam("Location", OUTPUT cLocation).  

{oe\ttInputOrd.i}
              
RUN sys/ref/nk1look.p (INPUT cCompany, "OEPROMPT", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lOeprompt = LOGICAL(cRtnChar) NO-ERROR.
    
RUN sys/ref/nk1look.p (INPUT cCompany, "NewOrderEntry", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    cNewOrderEntry = cRtnChar . 
    
DEFINE VARIABLE hOrderEntryProcs AS HANDLE NO-UNDO. 
RUN oe/OrderEntryProcs.p PERSISTENT SET hOrderEntryProcs.    

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Record-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES orderType
&Scoped-define FIRST-EXTERNAL-TABLE orderType


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR orderType.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btExit ~
btnFirst btnLast btnNext btnPrevious btnExitText fiOrderSource fiCustPo ~
btClear btnClearText btBack btNext btCancel btAdd btCreateOrder btUpdate ~
btAdvanced btCopy btDelete fiQuoteNo
&Scoped-Define DISPLAYED-OBJECTS fiOrdTypeLabel fiOrdType fiOrderSourceLabel ~
fiOrderSource fiQuoteNoLabel fiQuoteNo fiCustPoLabel fiCustPo btnExitText statusMessage ~
btnClearText   


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD quoteForEstimateExists D-Dialog 
FUNCTION quoteForEstimateExists RETURNS LOGICAL
    ( /* parameter-definitions */ )  FORWARD.
  

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE W-Win           AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */ 
DEFINE VARIABLE h_b-order-type  AS HANDLE        NO-UNDO. 
DEFINE VARIABLE h_b-sel-estitem AS HANDLE        NO-UNDO.
DEFINE VARIABLE h_b-add-order   AS HANDLE        NO-UNDO.
DEFINE VARIABLE h_ordHeaderInfo AS HANDLE        NO-UNDO.
DEFINE VARIABLE h_ordLineInfo   AS HANDLE        NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btClear 
    IMAGE-UP FILE "Graphics/32x32/back_white.png":U
    IMAGE-INSENSITIVE FILE "Graphics/32x32/back_white.png":U NO-FOCUS FLAT-BUTTON
    LABEL "Reset" 
    SIZE 8 BY 1.91.

DEFINE BUTTON btExit 
    IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
    LABEL "" 
    SIZE 8 BY 1.91.  
     
DEFINE BUTTON btNext  NO-FOCUS
    LABEL "Next" 
    SIZE 20 BY 1.52
    FONT 17.

DEFINE BUTTON btBack  NO-FOCUS
    LABEL "Back" 
    SIZE 20 BY 1.52
    FONT 17.
     
 
DEFINE BUTTON btAdvanced  NO-FOCUS
    LABEL "Advanced" 
    SIZE 20 BY 1.52
    FONT 17.
  
DEFINE BUTTON btCancel  NO-FOCUS
    LABEL "Cancel" 
    SIZE 20 BY 1.52
    FONT 17.
  
DEFINE BUTTON btAdd  NO-FOCUS
    LABEL "Add " 
    SIZE 20 BY 1.52
    FONT 17.
 
DEFINE BUTTON btUpdate  NO-FOCUS
    LABEL "Update" 
    SIZE 20 BY 1.52
    FONT 17.      
  
DEFINE BUTTON btCopy  NO-FOCUS
    LABEL "Copy" 
    SIZE 20 BY 1.52
    FONT 17.
     
DEFINE BUTTON btDelete  NO-FOCUS
    LABEL "Delete" 
    SIZE 20 BY 1.52
    FONT 17.     
     
DEFINE BUTTON btCreateOrder  NO-FOCUS
    LABEL "Create Order" 
    SIZE 30 BY 1.52
    FONT 17.     

DEFINE BUTTON btnFirst 
    IMAGE-UP FILE "Graphics/32x32/first.png":U NO-FOCUS FLAT-BUTTON
    LABEL "First" 
    SIZE 8 BY 1.91 TOOLTIP "First".

DEFINE BUTTON btnLast 
    IMAGE-UP FILE "Graphics/32x32/last.png":U NO-FOCUS FLAT-BUTTON
    LABEL "Last" 
    SIZE 8 BY 1.91 TOOLTIP "Last".

DEFINE BUTTON btnNext 
    IMAGE-UP FILE "Graphics/32x32/next.png":U NO-FOCUS FLAT-BUTTON
    LABEL "Next" 
    SIZE 8 BY 1.91 TOOLTIP "Next".

DEFINE BUTTON btnPrevious 
    IMAGE-UP FILE "Graphics/32x32/previous.png":U NO-FOCUS FLAT-BUTTON
    LABEL "Prev" 
    SIZE 8 BY 1.91 TOOLTIP "Previous". 

DEFINE VARIABLE btnClearText       AS CHARACTER FORMAT "X(256)":U INITIAL "RESET" 
    VIEW-AS TEXT 
    SIZE 12 BY 1.43
    BGCOLOR 21 NO-UNDO.

DEFINE VARIABLE btnExitText        AS CHARACTER FORMAT "X(256)":U INITIAL "EXIT" 
    VIEW-AS TEXT 
    SIZE 9 BY 1.43
    BGCOLOR 21 NO-UNDO.

DEFINE VARIABLE fiOrderSource      AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 21.8 BY 1.43
    BGCOLOR 15 FGCOLOR 0 FONT 38 NO-UNDO.

DEFINE VARIABLE fiOrderSourceLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Customer:" 
    VIEW-AS FILL-IN 
    SIZE 22.2 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiCustPo           AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 21.8 BY 1.43
    BGCOLOR 15 FGCOLOR 0 FONT 38 NO-UNDO.

DEFINE VARIABLE fiCustPoLabel      AS CHARACTER FORMAT "X(256)":U INITIAL "Customer PO#:" 
    VIEW-AS FILL-IN 
    SIZE 32.6 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiOrdType          AS CHARACTER FORMAT "X(256)":U INITIAL "1"
    VIEW-AS FILL-IN 
    SIZE 27 BY 1.43
    FONT 38 NO-UNDO.

DEFINE VARIABLE fiOrdTypeLabel     AS CHARACTER FORMAT "X(256)":U INITIAL "Order Type:" 
    VIEW-AS FILL-IN 
    SIZE 26.4 BY 1.38 NO-UNDO.

DEFINE VARIABLE statusMessage      AS CHARACTER FORMAT "X(256)":U INITIAL "STATUS MESSAGE" 
    VIEW-AS TEXT 
    SIZE 136 BY 1.43 NO-UNDO.
          
DEFINE VARIABLE fiQuoteNo          AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 21.8 BY 1.43
    BGCOLOR 15 FGCOLOR 0 FONT 38 NO-UNDO.

DEFINE VARIABLE fiQuoteNoLabel     AS CHARACTER FORMAT "X(256)":U INITIAL "Quote:" 
    VIEW-AS FILL-IN 
    SIZE 20.6 BY 1.43 NO-UNDO.     
     

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main     
    btExit AT ROW 1 COL 197 WIDGET-ID 126
    btnFirst AT ROW 10.67 COL 197 WIDGET-ID 44
    btnLast AT ROW 16.38 COL 197 WIDGET-ID 46
    btnNext AT ROW 14.48 COL 197 WIDGET-ID 42
    btnPrevious AT ROW 12.57 COL 197 WIDGET-ID 40
    fiOrdTypeLabel AT ROW 4.52 COL 2 NO-LABELS WIDGET-ID 94
    fiOrdType AT ROW 4.52 COL 22.6 COLON-ALIGNED NO-LABELS WIDGET-ID 104
    fiOrderSourceLabel AT ROW 4.52 COL 46 NO-LABELS WIDGET-ID 112
    fiOrderSource AT ROW 4.52 COL 64.2 COLON-ALIGNED NO-LABELS WIDGET-ID 110
     
    fiQuoteNoLabel AT ROW 4.52 COL 100.2 COLON-ALIGNED NO-LABELS
    fiQuoteNo AT ROW 4.52 COL 115.2 COLON-ALIGNED NO-LABELS
    fiCustPoLabel AT ROW 4.52 COL 150 COLON-ALIGNED NO-LABELS WIDGET-ID 108
    fiCustPo AT ROW 4.52 COL 180.6 COLON-ALIGNED NO-LABELS WIDGET-ID 106
     
    btBack AT ROW 7.33 COL 10
    btNext AT ROW 7.33 COL 185 WIDGET-ID 118
    btCancel AT ROW 7.33 COL 215 
    btAdvanced AT ROW 7.33 COL 200
    btAdd AT ROW 30.33 COL 3 
    btUpdate AT ROW 30.33 COL 23
    btCopy AT ROW 30.33 COL 43
    btDelete AT ROW 30.33 COL 63
    btCreateOrder AT ROW 32.33 COL 235 
     
    btnExitText AT ROW 1.24 COL 187 NO-LABELS WIDGET-ID 24
    statusMessage AT ROW 30.76 COL 3 NO-LABELS WIDGET-ID 28     
     
    btClear AT ROW 3.14 COL 197 WIDGET-ID 146
    btnClearText AT ROW 3.33 COL 184.2 NO-LABELS WIDGET-ID 148
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 204.8 BY 36.19
    BGCOLOR 21 FGCOLOR 15 FONT 38 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: ASI.job
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW W-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "Add New Order"
        HEIGHT             = 32.81
        WIDTH              = 204.8
        MAX-HEIGHT         = 48.76
        MAX-WIDTH          = 273.2
        VIRTUAL-HEIGHT     = 48.76
        VIRTUAL-WIDTH      = 273.2
        CONTROL-BOX        = NO
        MIN-BUTTON         = NO
        MAX-BUTTON         = NO
        RESIZE             = NO
        SCROLL-BARS        = NO
        STATUS-AREA        = NO
        BGCOLOR            = ?
        FGCOLOR            = ?
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN btnClearText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN btnExitText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiOrderSource IN FRAME F-Main
   EXE-LABEL                                                            */
/* SETTINGS FOR FILL-IN fiOrderSourceLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiCustPo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fiCustPoLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiOrdType IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiOrdTypeLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN statusMessage IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */    
/* SETTINGS FOR FILL-IN fiQuoteNo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiQuoteNoLabel IN FRAME F-Main
   NO-ENABLE                                                            */   
   
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
    THEN W-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Job Inquiry */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Job Inquiry */
    DO:
        /* This ADM code must be left here in order for the SmartWindow
           and its descendents to terminate properly on exit. */
        IF VALID-HANDLE(hdJobProcs) THEN
            DELETE PROCEDURE hdJobProcs.
        EMPTY TEMP-TABLE ttInputOrdLine .
        EMPTY TEMP-TABLE ttInputOrd .
        EMPTY TEMP-TABLE ttEstItem.
        IF VALID-HANDLE(hOrderEntryProcs) THEN
            DELETE OBJECT hOrderEntryProcs.
      
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btClear W-Win
ON CHOOSE OF btClear IN FRAME F-Main /* Reset */
    DO:
        EMPTY TEMP-TABLE ttInputOrdLine .
        EMPTY TEMP-TABLE ttInputOrd .
        EMPTY TEMP-TABLE ttEstItem.
        RUN select-page(1).        
        RUN pStatusMessage ("", 0).
        fiOrderSource:SCREEN-VALUE = "".
        fiCustPo:SCREEN-VALUE = "".
        APPLY "entry" TO fiOrderSource IN FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNext W-Win
ON CHOOSE OF btNext IN FRAME F-Main /* Next */
    DO:
        DEFINE VARIABLE lComplete        AS LOGICAL NO-UNDO.
        DEFINE VARIABLE rwRowid          AS ROWID   NO-UNDO.
        DEFINE VARIABLE lError           AS LOGICAL NO-UNDO.
   
        DEFINE VARIABLE adm-current-page AS INTEGER NO-UNDO.

        RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
        ASSIGN 
            adm-current-page = INTEGER(RETURN-VALUE).
   
        IF adm-current-page EQ 1 THEN
        DO:
            IF fiCustPo:HIDDEN IN FRAME {&FRAME-NAME} EQ YES THEN
                ASSIGN
                    fiCustPo:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = "".
        
            IF orderType.orderTypeSource EQ "Customer" THEN
            DO:   
                RUN valid-cust-no(OUTPUT lError) NO-ERROR.
                IF lError THEN RETURN NO-APPLY .
            END.
            ELSE IF orderType.orderTypeSource EQ "Estimate" THEN
                DO:   
                    RUN valid-est-no(OUTPUT lError) NO-ERROR.
                    IF lError THEN RETURN NO-APPLY .
                END.
        
            RUN valid-po-no(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY.
        
        
            IF orderType.orderTypeSource EQ "Estimate" THEN
                RUN select-page(2).
            ELSE IF orderType.orderTypeSource EQ "Customer" THEN
                    RUN select-page(3).
        END.
        ELSE RUN select-page(3).   
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btBack W-Win
ON CHOOSE OF btBack IN FRAME F-Main /* Back */
    DO:
        DEFINE VARIABLE lComplete        AS LOGICAL NO-UNDO.
        DEFINE VARIABLE rwRowid          AS ROWID   NO-UNDO.
        DEFINE VARIABLE adm-current-page AS INTEGER NO-UNDO.

        RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
        ASSIGN 
            adm-current-page = INTEGER(RETURN-VALUE).
   
        IF adm-current-page EQ 2 THEN
        DO:
            RUN select-page(1).
        END.
        ELSE IF adm-current-page EQ 3 THEN
            DO:
                IF orderType.orderTypeSource EQ "Customer" THEN
                    RUN select-page(1).
                ELSE IF orderType.orderTypeSource EQ "Estimate" THEN
                        RUN select-page(2).
            END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAdvanced
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAdvanced W-Win
ON CHOOSE OF btAdvanced IN FRAME F-Main /* Advanced */
    DO:
        DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO.
        DEFINE VARIABLE lError   AS LOGICAL NO-UNDO.
        DEFINE BUFFER bff-ttInputOrdLine FOR ttInputOrdLine .
    
        RUN pGetOrderTempTable IN h_b-add-order ( OUTPUT TABLE ttInputOrd).
        RUN oe/dEditOrdHeader.w ( INPUT-OUTPUT TABLE ttInputOrd) .     
     
        RUN pUpdateTempTable IN h_b-add-order( INPUT TABLE ttInputOrd).   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel W-Win
ON CHOOSE OF btCancel IN FRAME F-Main /* Cancel */
    DO:
        DEFINE VARIABLE lComplete AS LOGICAL NO-UNDO.
        DEFINE VARIABLE rwRowid   AS ROWID   NO-UNDO.
   
        RUN pStatusMessage ("", 0).
   
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAdd W-Win
ON CHOOSE OF btAdd IN FRAME F-Main /* Add */
    DO:
        DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO.
        DEFINE VARIABLE lError   AS LOGICAL NO-UNDO.
        DEFINE BUFFER bff-ttInputOrdLine FOR ttInputOrdLine .
    
        FIND FIRST eb NO-LOCK 
            WHERE eb.est-no EQ cCompany NO-ERROR.
                       
        RUN oe/dAddEditOrdLine.w (?,ROWID(eb),"Add","",
            "",
            "",
            "",
            NO, OUTPUT lv-rowid) . 
        FIND FIRST bff-ttInputOrdLine NO-LOCK
            WHERE bff-ttInputOrdLine.Company EQ cCompany
            AND ROWID(bff-ttInputOrdLine) EQ lv-rowid NO-ERROR .
    //IF AVAILABLE bff-ttInputOrdLine THEN
     //   RUN repo-query (lv-rowid). 
     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdate W-Win
ON CHOOSE OF btUpdate IN FRAME F-Main /* Update */
    DO:
     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btCreateOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCreateOrder W-Win
ON CHOOSE OF btCreateOrder IN FRAME F-Main /* Create Order */
    DO:

        DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO .
        DEFINE VARIABLE iCount   AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cMassage AS CHARACTER NO-UNDO.
        DEFINE VARIABLE rwRowid  AS ROWID     NO-UNDO.
        iCount = 0.
   
        RUN pGetOrderTempTable IN h_b-add-order ( OUTPUT TABLE ttInputOrd).
        RUN pGetOrderLineTempTable IN h_b-add-order ( OUTPUT TABLE ttInputOrdLine).
   
        FOR EACH ttInputOrdLine NO-LOCK :
            iCount = iCount + 1.
        END.
   

        IF iCount EQ 0 THEN 
        DO:      
            RUN pStatusMessage ("Please add atleast one order line", 3).
            RETURN.
        END. 
   
        RUN OrderEntry_SaveData IN hOrderEntryProcs(INPUT TABLE ttInputOrd, INPUT TABLE ttInputOrdLine, OUTPUT lError, OUTPUT cMassage, OUTPUT rwRowid).
   
        IF NOT lError THEN
        DO:
            oprwRowid = rwRowid.
        END.                            
    
        APPLY "close" TO THIS-PROCEDURE.         
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit W-Win
ON CHOOSE OF btExit IN FRAME F-Main
    DO:
        APPLY "CLOSE":U TO THIS-PROCEDURE.
    
        RETURN.    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearText W-Win
ON MOUSE-SELECT-CLICK OF btnClearText IN FRAME F-Main
    DO:
        APPLY "CHOOSE" TO btClear.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExitText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExitText W-Win
ON MOUSE-SELECT-CLICK OF btnExitText IN FRAME F-Main
    DO:
        RUN dispatch ("exit").
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst W-Win
ON CHOOSE OF btnFirst IN FRAME F-Main /* First */
    DO:
        RUN pNavigate (SELF).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLast W-Win
ON CHOOSE OF btnLast IN FRAME F-Main /* Last */
    DO:
        RUN pNavigate (SELF).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext W-Win
ON CHOOSE OF btnNext IN FRAME F-Main /* Next */
    DO:
        RUN pNavigate (SELF).
        RUN pValueChanged IN h_b-order-type.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevious W-Win
ON CHOOSE OF btnPrevious IN FRAME F-Main /* Prev */
    DO:
        RUN pNavigate (SELF).
        RUN pValueChanged IN h_b-order-type.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiOrderSource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiOrderSource W-Win
ON HELP OF fiOrderSource IN FRAME F-Main /* Source */
    DO:
    
        DEFINE VARIABLE char-val     AS cha       NO-UNDO.
        DEFINE VARIABLE look-recid   AS RECID     NO-UNDO.
        DEFINE VARIABLE chFile       AS CHARACTER FORMAT "X(100)" NO-UNDO.
        DEFINE VARIABLE lOkflg       AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
        
        FIND FIRST orderType NO-LOCK
            WHERE orderType.company EQ cCompany
            AND orderType.orderTypeID EQ giOrdType NO-ERROR.
        IF NOT AVAILABLE orderType THEN RETURN.
        
        IF orderType.orderTypeSource EQ "Customer" THEN
        DO:          
            RUN windows/l-custact.w (cCompany,"", OUTPUT char-val, OUTPUT look-recid).
            IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
                ASSIGN
                    fiOrderSource:screen-value = ENTRY(1,char-val)                
                    .     
        END.
        ELSE IF orderType.orderTypeSource EQ "Estimate" THEN
            DO:
                RUN windows/l-est.w (cCompany,cLocation,"", OUTPUT char-val).
                FIND FIRST eb NO-LOCK WHERE RECID(eb) = INT(char-val) NO-ERROR.
                IF AVAILABLE eb THEN 
                DO:
                    ASSIGN
                        fiOrderSource:screen-value = eb.est-no  .
                            
                END.
            END.
            ELSE IF orderType.orderTypeSource EQ "File" THEN
                DO:             
                    SYSTEM-DIALOG GET-FILE chFile 
                        TITLE "Select File"
                        FILTERS "All Files (*.*)" "*.*" 
                        INITIAL-DIR cNewOrderEntry
                        MUST-EXIST
                        USE-FILENAME
                        UPDATE lOkflg.                     
              
                    ASSIGN
                        fiOrderSource:screen-value = chFile  .                    
                END. 
                ELSE IF orderType.orderTypeSource EQ "Order" THEN
                    DO:
                        RUN system/openlookup.p (
                            cCompany, 
                            "", /* lookup field */
                            27,   /* Subject ID */
                            "",  /* User ID */
                            0,   /* Param value ID */
                            OUTPUT returnFields, 
                            OUTPUT lookupField, 
                            OUTPUT recVal
                            ). 
                        ASSIGN
                            fiOrderSource:screen-value = chFile  .                         
                    END.
        APPLY "entry" TO fiOrderSource IN FRAME {&FRAME-NAME}.      
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiOrderSource W-Win
ON LEAVE OF fiOrderSource IN FRAME F-Main /* Customer ID# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            iQuoteNumber = 0.
            RUN pStatusMessage ("", 0).
            RUN pCheckPo NO-ERROR. 
            fiOrderSource:BGCOLOR  = 10.            
        END.
           
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustPo W-Win
ON LEAVE OF fiCustPo IN FRAME F-Main /* Customer PO# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:               
            fiCustPo:BGCOLOR  = 10.            
        END.
           
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

{sharpshooter/pStatusMessage.i}

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
    DEFINE VARIABLE adm-current-page AS INTEGER NO-UNDO.

    RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
    ASSIGN 
        adm-current-page = INTEGER(RETURN-VALUE).

    CASE adm-current-page: 

        WHEN 0 THEN 
            DO:          

            END. /* Page 0 */     
        WHEN 1 THEN 
            DO:              
                RUN init-object IN THIS-PROCEDURE (
                    INPUT  'oe/b-order-type.w':U ,
                    INPUT  FRAME F-Main:HANDLE ,
                    INPUT  'Layout = ':U ,
                    OUTPUT h_b-order-type ).
                RUN set-position IN h_b-order-type ( 9.29 , 2.00 ) NO-ERROR.
                RUN set-size IN h_b-order-type ( 10.48 , 195.00 ) NO-ERROR.       
              
                /* Initialize other pages that this page requires. */
                RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.   
      
                /* Links to SmartBrowser h_b-order-type. */
                RUN add-link IN adm-broker-hdl ( h_b-order-type , 'Record':U , THIS-PROCEDURE ).
     
                RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'PAGE_1':U , h_b-order-type ).        
       
            END. /* Page 1 */  
        WHEN 2 THEN 
            DO:
    
                RUN init-object IN THIS-PROCEDURE (
                    INPUT  'oe/ordHeaderInfo.w':U ,
                    INPUT  FRAME F-Main:HANDLE ,
                    INPUT  '':U ,
                    OUTPUT h_ordHeaderInfo ).
                RUN set-position IN h_ordHeaderInfo ( 2.76 , 4.40 ) NO-ERROR.
                /* Size in UIB:  ( 2.05 , 136.60 ) */
      
                RUN init-object IN THIS-PROCEDURE (
                    INPUT  'oe/b-sel-estitem.w':U ,
                    INPUT  FRAME F-Main:HANDLE ,
                    INPUT  'Layout = ':U ,
                    OUTPUT h_b-sel-estitem ).
                RUN set-position IN h_b-sel-estitem ( 10.29 , 2.00 ) NO-ERROR.
                RUN set-size IN h_b-sel-estitem ( 20.48 , 195.00 ) NO-ERROR.

                /* Initialize other pages that this page requires. */
                RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.    
       
                /* Links to SmartBrowser h_b-sel-estitem. */        
                RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'PAGE_2':U , h_b-sel-estitem ).
      
            END. /* Page 2 */
        WHEN 3 THEN 
            DO:
    
                RUN init-object IN THIS-PROCEDURE (
                    INPUT  'oe/ordLineInfo.w':U ,
                    INPUT  FRAME F-Main:HANDLE ,
                    INPUT  '':U ,
                    OUTPUT h_ordLineInfo ).
                RUN set-position IN h_ordLineInfo ( 2.76 , 4.40 ) NO-ERROR.
                /* Size in UIB:  ( 2.05 , 136.60 ) */    
    
                RUN init-object IN THIS-PROCEDURE (
                    INPUT  'oe/b-add-order.w':U ,
                    INPUT  FRAME F-Main:HANDLE ,
                    INPUT  'Layout = ':U ,
                    OUTPUT h_b-add-order ).
                RUN set-position IN h_b-add-order ( 10.33 , 2.00 ) NO-ERROR.
                RUN set-size IN h_b-add-order ( 19.05 , 195.00 ) NO-ERROR.
              
                /* Links to SmartBrowser h_b-add-order. */     
                RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'PAGE_3':U , h_b-add-order ).   
       
            END. /* Page 3 */

    END CASE.
    /* Select a Startup page. */
    IF adm-current-page EQ 0 
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

    /* Create a list of all the tables that we need to get.            */
    {src/adm/template/row-list.i "orderType"}  

    /* Get the record ROWID's from the RECORD-SOURCE.                  */
    {src/adm/template/row-get.i}

    /* FIND each record specified by the RECORD-SOURCE.                */
    {src/adm/template/row-find.i "orderType"} 

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
    DISPLAY fiOrdTypeLabel fiOrdType fiOrderSourceLabel fiOrderSource fiCustPoLabel 
        fiCustPo btnExitText statusMessage fiQuoteNo fiQuoteNoLabel 
        btnClearText           
        WITH FRAME F-Main IN WINDOW W-Win.
    ENABLE btExit btnFirst btnLast btnNext btAdd btCreateOrder
        btUpdate btnPrevious btnExitText btBack btNext btCancel  
        fiCustPo fiOrderSource btClear btnClearText btAdvanced
        btCopy btDelete
        WITH FRAME F-Main IN WINDOW W-Win.
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE adm-current-page AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dCol             AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dColTmp          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dRow             AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dHeight          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dWidth           AS DECIMAL   NO-UNDO.
  
    DEFINE VARIABLE iSourceID        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cSourceType      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSourceValue     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerPo      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustNo          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ship-to          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtDueDate        AS DATE      NO-UNDO.
    DEFINE VARIABLE iOver            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iUnder           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cDueCode         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iQuote           AS INTEGER   NO-UNDO.
  
    DEFINE BUFFER bf-ttEstItem FOR ttEstItem.

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
    ASSIGN 
        adm-current-page = INTEGER(RETURN-VALUE).
        
    DO WITH FRAME {&FRAME-NAME}:
  
        CASE adm-current-page:
            WHEN 1 THEN 
                DO:
                    ASSIGN
                        fiOrdTypeLabel:HIDDEN     = NO
                        fiOrdType:HIDDEN          = NO
                        fiOrderSourceLabel:HIDDEN = NO
                        fiOrderSource:HIDDEN      = NO                  
                        fiCustPoLabel:HIDDEN      = NO
                        fiCustPo:HIDDEN           = NO                
                        btBack:HIDDEN             = YES
                        btNext:HIDDEN             = NO
                        btCancel:HIDDEN           = NO
                        btAdvanced:HIDDEN         = YES
                        btCreateOrder:HIDDEN      = YES
                        btAdd:HIDDEN              = YES
                        btUpdate:HIDDEN           = YES
                        btCopy:HIDDEN             = YES
                        btDelete:HIDDEN           = YES
                        . 
                    IF iQuoteNumber NE 0 THEN
                        ASSIGN
                            fiQuoteNo:HIDDEN      = NO
                            fiQuoteNoLabel:HIDDEN = NO.
                    ELSE 
                        ASSIGN
                            fiQuoteNo:HIDDEN      = YES
                            fiQuoteNoLabel:HIDDEN = YES.
              
                    RUN pValueChanged IN h_b-order-type.
                END.    
            WHEN 2 THEN 
                DO:
                    RUN pStatusMessage ("", 0).
                    RUN pBuildTable IN h_b-sel-estitem( INPUT fiOrderSource:SCREEN-VALUE , INPUT fiCustPo:SCREEN-VALUE, INPUT dPrice, INPUT cPrUom, INPUT iQty, INPUT iQuoteNumber, INPUT-OUTPUT TABLE ttEstItem  ). 
                    FIND FIRST bf-ttEstItem NO-LOCK 
                        WHERE TRIM(bf-ttEstItem.estNo) EQ trim(fiOrderSource:SCREEN-VALUE) NO-ERROR .
             
                    IF AVAILABLE bf-ttEstItem THEN
                        ASSIGN
                            cCustNo = bf-ttEstItem.estCust
                            ship-to = bf-ttEstItem.estShipId.
             
                    RUN pFillValue IN h_ordHeaderInfo( INPUT fiOrderSource:SCREEN-VALUE, INPUT cCustNo , INPUT ship-to, INPUT fiCustPo:SCREEN-VALUE).
            
                    RUN get-position IN h_b-sel-estitem ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
                    RUN get-size IN h_b-sel-estitem ( OUTPUT dHeight , OUTPUT dWidth ) NO-ERROR.
                    ASSIGN
                        dCol    = {&WINDOW-NAME}:WIDTH  - 8
                        dHeight = {&WINDOW-NAME}:HEIGHT - dRow - 1.33
                        dWidth  = dCol - 3
                        .
                    RUN set-size IN h_b-sel-estitem ( dHeight , dWidth ) NO-ERROR.
                    ASSIGN
                        dRow                      = {&WINDOW-NAME}:HEIGHT - 1
                        fiOrdTypeLabel:HIDDEN     = YES
                        fiOrdType:HIDDEN          = YES
                        fiOrderSourceLabel:HIDDEN = YES
                        fiOrderSource:HIDDEN      = YES
                        fiQuoteNo:HIDDEN          = YES
                        fiQuoteNoLabel:HIDDEN     = YES
                        fiCustPoLabel:HIDDEN      = YES
                        fiCustPo:HIDDEN           = YES                
                        btBack:HIDDEN             = NO
                        btNext:HIDDEN             = NO
                        btCancel:HIDDEN           = NO 
                        btAdvanced:HIDDEN         = YES
                        btCreateOrder:HIDDEN      = YES
                        btAdd:HIDDEN              = YES
                        btUpdate:HIDDEN           = YES
                        btCopy:HIDDEN             = YES
                        btDelete:HIDDEN           = YES
                        . 
                           
                END.
            WHEN 3 THEN 
                DO:
                    RUN pStatusMessage ("", 0).
                    ASSIGN
                        fiOrdTypeLabel:HIDDEN     = YES
                        fiOrdType:HIDDEN          = YES
                        fiOrderSourceLabel:HIDDEN = YES
                        fiOrderSource:HIDDEN      = YES
                        fiQuoteNo:HIDDEN          = YES
                        fiQuoteNoLabel:HIDDEN     = YES
                        fiCustPoLabel:HIDDEN      = YES
                        fiCustPo:HIDDEN           = YES   
                        btBack:HIDDEN             = NO
                        btNext:HIDDEN             = YES
                        btCancel:HIDDEN           = YES 
                        btAdvanced:HIDDEN         = NO
                        btCreateOrder:HIDDEN      = NO
                        btAdd:HIDDEN              = NO
                        btUpdate:HIDDEN           = NO
                        btCopy:HIDDEN             = NO
                        btDelete:HIDDEN           = NO
                        . 
                
                    IF VALID-HANDLE(h_b-sel-estitem) THEN   
                        RUN pGetTable IN h_b-sel-estitem( OUTPUT TABLE ttEstItem  ). 
                
                    RUN pBuildTable IN h_b-add-order(INPUT TABLE ttEstItem, INPUT fiOrderSource:SCREEN-VALUE , INPUT fiCustPo:SCREEN-VALUE,
                        OUTPUT dtDueDate, OUTPUT iOver, OUTPUT iUnder ). 
            
                    FIND FIRST bf-ttEstItem NO-LOCK 
                        WHERE TRIM(bf-ttEstItem.estNo) EQ trim(fiOrderSource:SCREEN-VALUE) NO-ERROR .
             
                    IF AVAILABLE bf-ttEstItem THEN
                        ASSIGN
                            cCustNo  = bf-ttEstItem.estCust
                            ship-to  = bf-ttEstItem.estShipId             
                            cDueCode = "On"
                            iQuote   = bf-ttEstItem.estQuote.
            
                    RUN pFillValue IN h_ordLineInfo(INPUT fiOrderSource:SCREEN-VALUE, INPUT cCustNo , INPUT ship-to, INPUT fiCustPo:SCREEN-VALUE,
                        INPUT iQuote, INPUT cCustNo, INPUT cDueCode, INPUT dtDueDate, INPUT iOver, INPUT iUnder).
            
                    RUN get-position IN h_b-add-order ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
                    RUN get-size IN h_b-add-order ( OUTPUT dHeight , OUTPUT dWidth ) NO-ERROR.
                    ASSIGN
                        dCol    = {&WINDOW-NAME}:WIDTH  - 8
                        dHeight = {&WINDOW-NAME}:HEIGHT - dRow - 1.33
                        dWidth  = dCol - 3
                        .
                    RUN set-size IN h_b-add-order ( (dHeight - 2) , dWidth ) NO-ERROR.
                END.
        END CASE.
     
    END. /* with frame */  
              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy W-Win 
PROCEDURE local-destroy :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    IF VALID-HANDLE(hdJobProcs) THEN
        DELETE PROCEDURE hdJobProcs.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

/* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
    /*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    RUN pWinReSize.

    

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .
  
    /* Code placed here will execute AFTER standard behavior.    */
    FIND FIRST company NO-LOCK 
        WHERE company.company EQ cCompany NO-ERROR .
    IF AVAILABLE company THEN            
        {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " - " + DYNAMIC-FUNCTION("sfVersion") + " - " 
            + STRING(company.name) + " - " + cLocation.

        
    RUN pStatusMessage ("", 0).
    btBack:HIDDEN IN FRAME {&FRAME-NAME} = YES .
    btAdvanced:HIDDEN IN FRAME {&FRAME-NAME} = YES .
    btCreateOrder:HIDDEN IN FRAME {&FRAME-NAME} = YES .
    btAdd:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    btUpdate:HIDDEN IN FRAME {&FRAME-NAME} = YES .
    btCopy:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    btDelete:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    fiQuoteNo:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    fiQuoteNoLabel:HIDDEN IN FRAME {&FRAME-NAME} = YES.         
   
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNavigate W-Win 
PROCEDURE pNavigate :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphNavPanel AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE hdCurrentBrowse AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cHandle         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLink           AS CHARACTER NO-UNDO.

    RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).   

    cLink = "PAGE_" + STRING(RETURN-VALUE) + "-TARGET".
 
    RUN get-link-handle IN adm-broker-hdl (
        INPUT THIS-PROCEDURE,
        INPUT cLink,
        OUTPUT cHandle
        ).
  
    IF NUM-ENTRIES(cHandle) LT 2 THEN
        hdCurrentBrowse = WIDGET-HANDLE(cHandle).
   
    IF VALID-HANDLE(hdCurrentBrowse) THEN
        RUN dispatch IN hdCurrentBrowse ("get-" + iphNavPanel:LABEL).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize W-Win 
PROCEDURE pWinReSize :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dCol    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dColTmp AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dRow    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dHeight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dWidth  AS DECIMAL NO-UNDO.

    SESSION:SET-WAIT-STATE("General").
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            {&WINDOW-NAME}:ROW                 = 1
            {&WINDOW-NAME}:COL                 = 1
            {&WINDOW-NAME}:VIRTUAL-HEIGHT      = SESSION:HEIGHT - 1
            {&WINDOW-NAME}:VIRTUAL-WIDTH       = SESSION:WIDTH  - 1
            {&WINDOW-NAME}:HEIGHT              = {&WINDOW-NAME}:VIRTUAL-HEIGHT
            {&WINDOW-NAME}:WIDTH               = {&WINDOW-NAME}:VIRTUAL-WIDTH
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT         = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH          = {&WINDOW-NAME}:WIDTH
            statusMessage:ROW                  = {&WINDOW-NAME}:HEIGHT - .86
            dCol                               = {&WINDOW-NAME}:WIDTH  - 8
            btExit:COL                         = dCol - 1
            btnFirst:COL                       = dCol - 1
            btnPrevious:COL                    = dCol - 1
            btnNext:COL                        = dCol - 1
            btnLast:COL                        = dCol - 1
            btnExitText:COL                    = dCol - 9
            btnClearText:COL                   = dCol - 13
            btClear:COL                        = dCol - 1             
            btCreateOrder:ROW                  = {&WINDOW-NAME}:HEIGHT - 1.96
            btAdd:ROW                          = {&WINDOW-NAME}:HEIGHT - 2.96
            btUpdate:ROW                       = {&WINDOW-NAME}:HEIGHT - 2.96
            btCopy:ROW                         = {&WINDOW-NAME}:HEIGHT - 2.96
            btDelete:ROW                       = {&WINDOW-NAME}:HEIGHT - 2.96
            .
        dRow = {&WINDOW-NAME}:HEIGHT - 1. 
        
        RUN get-position IN h_b-order-type ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
        RUN get-size IN h_b-order-type ( OUTPUT dHeight , OUTPUT dWidth ) NO-ERROR.
        ASSIGN
            dHeight = {&WINDOW-NAME}:HEIGHT - dRow - 1.33
            dWidth  = dCol - 3
            .      
        RUN set-size IN h_b-order-type ( dHeight , dWidth ) NO-ERROR.             
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

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

    /* Define variables needed by this internal procedure.               */
    {src/adm/template/snd-head.i}

    /* For each requested table, put it's ROWID in the output list.      */
    {src/adm/template/snd-list.i "orderType"}

    /* Deal with any unexpected table requests before closing.           */
    {src/adm/template/snd-end.i}

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
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.    
    
/*CASE p-state:
    WHEN "job-valid" THEN
        RUN pJobScan.
    WHEN "job-error" THEN
        RUN pJobError.
END CASE. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pStatusClear W-Win 
PROCEDURE pStatusClear :
    /* -----------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/
    RUN pStatusMessage("","0").
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pChangeTypeSource W-Win 
PROCEDURE pChangeTypeSource :
    /* -----------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTypeSourceDesc AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOrderType AS INTEGER NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        fiOrdType:SCREEN-VALUE = STRING(ipcOrderType).
        fiOrderSourceLabel:SCREEN-VALUE = ipcTypeSourceDesc.
        giOrdType = ipcOrderType.
        gcOrdTypeSource = ipcTypeSourceDesc.
    END.    
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckPo W-Win 
PROCEDURE pCheckPo :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/        
    DO WITH FRAME {&FRAME-NAME}:
       
        fiCustPo:HIDDEN = YES.
        fiCustPoLabel:HIDDEN = YES.
        IF orderType.orderTypeSource EQ "Estimate" THEN
        DO:
            FIND FIRST eb NO-LOCK
                WHERE eb.company EQ cCompany
                AND eb.est-no  EQ FILL(" ",8 - LENGTH(TRIM(fiOrderSource:SCREEN-VALUE))) + TRIM(fiOrderSource:SCREEN-VALUE)
                NO-ERROR.
            IF AVAILABLE eb THEN
            DO:
                FIND FIRST cust NO-LOCK
                    WHERE cust.company EQ eb.company
                    AND cust.cust-no EQ eb.cust-no
                    AND cust.po-mandatory 
                    NO-ERROR.
                IF AVAILABLE cust THEN
                DO:
                    ASSIGN 
                        fiCustPo:HIDDEN      = NO
                        fiCustPoLabel:HIDDEN = NO
                        fiCustPo:SENSITIVE   = YES.
                    APPLY "entry" TO fiCustPo .                    
                    
                END.  
                     
                IF quoteForEstimateExists() THEN
                    RUN oe/d-quotedprices.w("",cCompany,
                        cLocation,
                        eb.est-no,
                        eb.cust-no,
                        eb.part-no,
                        eb.stock-no,
                        INPUT-OUTPUT dPrice,
                        INPUT-OUTPUT cPrUom,
                        INPUT-OUTPUT iQty,
                        INPUT-OUTPUT iQuoteNumber,
                        OUTPUT cChoice).  
                IF cChoice EQ "ok" THEN
                DO:
                    fiQuoteNo:HIDDEN = NO .
                    fiQuoteNoLabel:HIDDEN = NO.
                    fiQuoteNo:SCREEN-VALUE = STRING(iQuoteNumber).
                END.                                                                       
                
            END.
        END.
        ELSE IF orderType.orderTypeSource EQ "Customer" THEN
            DO:
                FIND FIRST cust NO-LOCK
                    WHERE cust.company EQ cCompany
                    AND cust.cust-no EQ fiOrderSource:SCREEN-VALUE
                    AND cust.po-mandatory 
                    NO-ERROR.
                IF AVAILABLE cust THEN
                DO:
                    ASSIGN 
                        fiCustPo:HIDDEN      = NO
                        fiCustPoLabel:HIDDEN = NO.
                    APPLY "entry" TO fiCustPo .                
                END.  
            END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no W-Win 
PROCEDURE valid-cust-no :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST cust
            WHERE cust.company  EQ cCompany
            AND cust.cust-no   EQ fiOrderSource:SCREEN-VALUE)  THEN 
        DO:               
            RUN pStatusMessage ("Invalid Customer, try help...", 3).
            fiOrderSource:BGCOLOR = 12.
            APPLY "entry" TO fiOrderSource .
            oplOutError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-est-no W-Win 
PROCEDURE valid-est-no :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:  
        IF fiOrderSource:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST est NO-LOCK
                WHERE est.company EQ cCompany
                AND est.est-no  EQ FILL(" ",8 - LENGTH(TRIM(fiOrderSource:SCREEN-VALUE))) + TRIM(fiOrderSource:SCREEN-VALUE)
                NO-ERROR.
            IF NOT AVAILABLE est THEN 
            DO:
                                
                RUN pStatusMessage ("Invalid Estimate#, try help...", 3).
                fiOrderSource:BGCOLOR = 12.
                APPLY "entry" TO fiOrderSource.
                oplOutError = YES.
                RETURN.
            END.             
      
        END.  
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no W-Win 
PROCEDURE valid-po-no :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE cCustomerNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ll-ans      AS LOGICAL   NO-UNDO.
    DEFINE BUFFER b-oe-ordl FOR oe-ordl.
    
    DO WITH FRAME {&FRAME-NAME}:     
         
        IF orderType.orderTypeSource EQ "Estimate" AND fiOrderSource:SCREEN-VALUE NE "" THEN
        DO:
            FIND FIRST eb NO-LOCK
                WHERE eb.company EQ cCompany
                AND eb.est-no  EQ FILL(" ",8 - LENGTH(TRIM(fiOrderSource:SCREEN-VALUE))) + TRIM(fiOrderSource:SCREEN-VALUE)
                NO-ERROR.
            IF AVAILABLE eb THEN
                cCustomerNo = eb.cust-no.
        END.
        ELSE cCustomerNo = fiOrderSource:SCREEN-VALUE.        
    
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ cCompany
            AND cust.cust-no EQ cCustomerNo
            AND cust.cust-no NE ""
            AND cust.po-mandatory
            NO-ERROR.
                    
        IF AVAILABLE cust AND TRIM(fiCustPo:SCREEN-VALUE) EQ "" THEN 
        DO:            
            
            RUN pStatusMessage ("PO# is mandatory for Customer " + cust.cust-no, 3).
            fiCustPo:BGCOLOR = 12.    
            APPLY "entry" TO fiCustPo.
            oplOutError = YES . 
        END.
        
        IF NOT lValidPoNo AND lOeprompt AND fiCustPo:SCREEN-VALUE NE "" THEN
            FIND FIRST b-oe-ordl
                WHERE b-oe-ordl.company EQ cCompany
                AND b-oe-ordl.po-no   EQ fiCustPo:SCREEN-VALUE
                AND b-oe-ordl.cust-no EQ fiOrderSource:SCREEN-VALUE            
                NO-LOCK NO-ERROR.

        IF AVAILABLE b-oe-ordl THEN 
        DO:             
            RUN sharpshooter\messageDialog.w( 
                "Customer PO already exists for Order/Item - " + TRIM(STRING(b-oe-ordl.ord-no,">>>>>>>>")) + "/" +
                TRIM(b-oe-ordl.i-no) + " ." + "Do you want to continue?",
                YES,
                YES,
                NO,
                OUTPUT ll-ans
                ).
            IF NOT ll-ans THEN 
            DO:
                APPLY "entry" TO fiCustPo.
                oplOutError = YES.
            END.
            ELSE lValidPoNo = YES.
        END.          
        RELEASE cust.    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION quoteForEstimateExists D-Dialog 
FUNCTION quoteForEstimateExists RETURNS LOGICAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  Returns logical based on existence of quote for input estimate
        Notes:  
    ------------------------------------------------------------------------------*/
 
    RETURN CAN-FIND(FIRST quotehd WHERE 
        quotehd.company EQ cCompany            
        AND quotehd.est-no EQ FILL(" ",8 - LENGTH(TRIM(fiOrderSource:SCREEN-VALUE IN FRAME {&FRAME-NAME}))) + TRIM(fiOrderSource:SCREEN-VALUE IN FRAME {&FRAME-NAME})).
    

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
