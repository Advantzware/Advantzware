&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: inventory/attach-viewer.w

  Description: Screen to display attachments for a job

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
&SCOPED-DEFINE WIDGET-FRAME widgetFrame

{methods/defines/globdefs.i}

DEFINE TEMP-TABLE ttAttachments NO-UNDO
    FIELDS ttOrder       AS INTEGER
    FIELDS ttAttachFile  AS CHARACTER
    FIELDS ttEstimate    AS CHARACTER 
    FIELDS ttItem        AS CHARACTER
    FIELDS ttOpenWith    AS CHARACTER
    FIELDS ttApplication AS CHARACTER
    FIELDS ttCreateDate  AS DATE
    FIELDS ttReckey      AS CHARACTER
    FIELDS ttFileFound   AS LOGICAL
    .
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE cCompany         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation        AS CHARACTER NO-UNDO.

{inventory/ttInventory.i "NEW SHARED"}

RUN inventory/InventoryProcs PERSISTENT SET hdInventoryProcs.

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
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 btExit btSearch fiLoadTag ~
fiJobNo fiJobNo2 
&Scoped-Define DISPLAYED-OBJECTS fiLoadTag fiJobNo fiJobNo2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btExit 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "Exit" 
     SIZE 11 BY 2.62.

DEFINE BUTTON btSearch 
     IMAGE-UP FILE "Graphics/32x32/magnifying_glass.ico":U
     LABEL "Search" 
     SIZE 20 BY 3.52.

DEFINE VARIABLE fiBlank AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Blank No" 
     VIEW-AS FILL-IN 
     SIZE 8.4 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiForm AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Form No" 
     VIEW-AS FILL-IN 
     SIZE 8.4 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiJobNo AS CHARACTER FORMAT "X(20)":U 
     LABEL "Job" 
     VIEW-AS FILL-IN 
     SIZE 46.4 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiJobNo2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.4 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiLoadTag AS CHARACTER FORMAT "X(256)":U 
     LABEL "Load Tag" 
     VIEW-AS FILL-IN 
     SIZE 81 BY 1.43
     FONT 36 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 126 BY 6.19
     BGCOLOR 1 FGCOLOR 1 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 142 BY 21.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btExit AT ROW 1.43 COL 148.2 WIDGET-ID 20
     btSearch AT ROW 1.52 COL 107.4 WIDGET-ID 16
     fiLoadTag AT ROW 1.57 COL 19.6 COLON-ALIGNED WIDGET-ID 2
     fiJobNo AT ROW 3.57 COL 19.6 COLON-ALIGNED WIDGET-ID 4
     fiJobNo2 AT ROW 3.57 COL 68.6 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     fiForm AT ROW 5.48 COL 19.6 COLON-ALIGNED WIDGET-ID 10
     fiBlank AT ROW 5.48 COL 46.8 COLON-ALIGNED WIDGET-ID 12
     "-" VIEW-AS TEXT
          SIZE 2 BY .91 AT ROW 3.81 COL 68.4 WIDGET-ID 8
     RECT-1 AT ROW 1.24 COL 3 WIDGET-ID 14
     RECT-2 AT ROW 7.62 COL 3 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1 FONT 36 WIDGET-ID 100.

DEFINE FRAME widgetFrame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 7.76
         SCROLLABLE SIZE 140 BY 21.43
         BGCOLOR 15  WIDGET-ID 200.


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
         TITLE              = "Job Attachment Viewer"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 31.81
         MAX-WIDTH          = 187.8
         VIRTUAL-HEIGHT     = 31.81
         VIRTUAL-WIDTH      = 187.8
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* REPARENT FRAME */
ASSIGN FRAME widgetFrame:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fiBlank IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fiBlank:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiForm IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fiForm:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FRAME widgetFrame
   Size-to-Fit                                                          */
ASSIGN 
       FRAME widgetFrame:HEIGHT           = 21.43
       FRAME widgetFrame:WIDTH            = 140.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Job Attachment Viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Job Attachment Viewer */
DO:
    IF VALID-HANDLE(hdInventoryProcs) THEN
        DELETE PROCEDURE hdInventoryProcs.
            
    /* This ADM code must be left here in order for the SmartWindow
       and its descendents to terminate properly on exit. */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit W-Win
ON CHOOSE OF btExit IN FRAME F-Main /* Exit */
DO:
    IF VALID-HANDLE(hdInventoryProcs) THEN
        DELETE PROCEDURE hdInventoryProcs.

    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSearch W-Win
ON CHOOSE OF btSearch IN FRAME F-Main /* Search */
DO:
    DEFINE VARIABLE iSequence        AS INTEGER NO-UNDO.
    DEFINE VARIABLE iSeperatorWidth  AS INTEGER NO-UNDO INITIAL 10.
    DEFINE VARIABLE iSeperatorHeight AS INTEGER NO-UNDO INITIAL 10.
    DEFINE VARIABLE iStartPositionX  AS INTEGER NO-UNDO INITIAL 10.
    DEFINE VARIABLE iStartPositionY  AS INTEGER NO-UNDO INITIAL 10.
    DEFINE VARIABLE iRowHeight       AS INTEGER NO-UNDO INITIAL 120.        
    DEFINE VARIABLE hdWidget         AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hdSideLabel      AS HANDLE  NO-UNDO.
    
    DEFINE VARIABLE cItemList   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRecKeyList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cImageFile  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValidFile  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    DEFINE BUFFER bf-job     FOR job.
    
    IF fiJobNo:SCREEN-VALUE EQ "" THEN
        RETURN.

    EMPTY TEMP-TABLE ttAttachments.
    
    DELETE WIDGET-POOL "attachment-widgets" NO-ERROR.
    CREATE WIDGET-POOL "attachment-widgets" PERSISTENT.
        
    /* Reset frame's virtual height to original height. This will remove any scrollbars
       created from the previous search */
    FRAME {&WIDGET-FRAME}:HANDLE:VIRTUAL-HEIGHT-PIXELS = FRAME {&WIDGET-FRAME}:HANDLE:HEIGHT-PIXELS. 
    
    FOR EACH bf-job-hdr NO-LOCK
        WHERE bf-job-hdr.company EQ cCompany
          AND bf-job-hdr.job-no  EQ fiJobNo:SCREEN-VALUE 
          AND bf-job-hdr.job-no2 EQ INTEGER(fiJobNo2:SCREEN-VALUE),
        EACH bf-job NO-LOCK
        WHERE bf-job.company EQ bf-job-hdr.company
          AND bf-job.job     EQ bf-job-hdr.job
          AND bf-job.job-no  EQ bf-job-hdr.job-no
          AND bf-job.job-no2 EQ bf-job-hdr.job-no2:
        IF bf-job-hdr.i-no <> "" THEN DO:
            FIND FIRST bf-itemfg NO-LOCK
                 WHERE bf-itemfg.company EQ bf-job-hdr.company 
                   AND bf-itemfg.i-no    EQ bf-job-hdr.i-no
                 NO-ERROR.            
            IF AVAILABLE bf-itemfg AND LOOKUP(bf-itemfg.rec_key, cRecKeyList) EQ 0 THEN
                ASSIGN
                    cRecKeyList = cRecKeyList + "," + bf-itemfg.rec_key
                    cItemList   = cItemList + "," + bf-job-hdr.i-no.
                    .
                            
            IF LOOKUP(bf-job.rec_key, cRecKeyList) EQ 0 THEN
                cRecKeyList = cRecKeyList + "," + bf-job.rec_key.
        END.
    END.
    
    ASSIGN
        cReckeyList = TRIM(cReckeyList, ",")
        cItemList   = TRIM(cItemList, ",")
        .

    DO iIndex = 1 TO NUM-ENTRIES(cItemList):            
        FOR EACH attach NO-LOCK
            WHERE attach.company EQ cCompany
              AND attach.i-no    EQ ENTRY(iIndex, cItemList):
    
            iSequence = iSequence + 1.
    
            RUN FileSys_ValidateFile(
                INPUT  attach.attach-file,
                OUTPUT lValidFile,
                OUTPUT cMessage
                ) NO-ERROR.

            CREATE ttAttachments.
            ASSIGN
                ttAttachments.ttOrder       = iSequence
                ttAttachments.ttAttachFile  = attach.attach-file
                ttAttachments.ttEstimate    = attach.est-no
                ttAttachments.ttItem        = attach.i-no
                ttAttachments.ttOpenWith    = attach.run-program
                ttAttachments.ttApplication = attach.run-application
                ttAttachments.ttCreateDate  = attach.creat-date
                ttAttachments.ttRecKey      = attach.rec_key
                ttAttachments.ttFileFound   = lValidFile
                .
        END.          
    END.
          
    SESSION:SET-WAIT-STATE("GENERAL").
    DO WITH FRAME {&WIDGET-FRAME}:        
        FOR EACH ttAttachments:
        
            IF ttAttachments.ttOrder * iRowHeight GT FRAME {&WIDGET-FRAME}:HANDLE:VIRTUAL-HEIGHT-PIXELS THEN
                FRAME {&WIDGET-FRAME}:HANDLE:VIRTUAL-HEIGHT-PIXELS = ttAttachments.ttOrder * iRowheight + iSeperatorHeight.

            CREATE RECT hdWidget IN WIDGET-POOL "attachment-widgets"
            ASSIGN 
                X             = iStartPositionX - 4
                Y             = ((ttAttachments.ttOrder - 1) * iRowHeight) + iStartPositionY - 4
                FRAME         = FRAME {&WIDGET-FRAME}:HANDLE
                WIDTH-PIXELS  = 108
                HEIGHT-PIXELS = 108
                FGCOLOR       = 4
                BGCOLOR       = 15
                VISIBLE       = TRUE
                EDGE-PIXELS   = 4
                FILLED        = TRUE
                .

            CREATE IMAGE hdWidget IN WIDGET-POOL "attachment-widgets"
            ASSIGN
                X             = iStartPositionX
                Y             = ((ttAttachments.ttOrder - 1) * iRowHeight) + iStartPositionY
                FRAME         = FRAME {&WIDGET-FRAME}:HANDLE
                WIDTH-PIXELS  = 100
                HEIGHT-PIXELS = 100
                VISIBLE       = TRUE
                SENSITIVE     = TRUE
                BGCOLOR       = 1
                HIDDEN        = FALSE
                TOOLTIP       = "Click to open file"
                PRIVATE-DATA  = STRING(ttAttachments.ttOrder)
                TRIGGERS:
                     ON LEFT-MOUSE-CLICK PERSISTENT RUN pImageClicked IN THIS-PROCEDURE (INPUT hdWidget:PRIVATE-DATA)).
                END TRIGGERS.
                .
            
            IF ttAttachments.ttFileFound THEN DO:
                CASE ttAttachments.ttApplication:
                    WHEN "Word" THEN
                        cImageFile = "Graphics/32x32/docx.jpg".
                    WHEN "Excel" THEN
                        cImageFile = "Graphics/32x32/xls.jpg".
                    WHEN "Acrobat" THEN
                        cImageFile = "Graphics/32x32/pdf.jpg".
                    WHEN "MS Paint" THEN DO:
                        IF ttAttachments.ttFileFound THEN 
                            ASSIGN
                                hdWidget:STRETCH-TO-FIT = TRUE
                                hdWidget:RETAIN-SHAPE   = TRUE
                                cImageFile              = ttAttachments.ttAttachFile
                                .
                    END.
                    WHEN "Photo Shop" THEN
                        cImageFile = "Graphics/32x32/paperclip.ico".
                    WHEN "Notepad" THEN
                        cImageFile = "Graphics/32x32/document_attachment.ico".
                    WHEN "Wordpad" THEN
                        cImageFile = "Graphics/32x32/document_attachment.ico".
                    WHEN "Internet Explorer" THEN
                        cImageFile = "Graphics/32x32/window_explorer.ico".
                    WHEN "Windows Default" THEN
                        cImageFile = "Graphics/32x32/window_dialog.ico".
                    OTHERWISE
                        cImageFile = "Graphics/32x32/question.ico".               
                END CASE.
            END.
            ELSE
                cImageFile = "Graphics/32x32/error.ico".
            
            IF cImageFile NE "" THEN            
                hdWidget:LOAD-IMAGE(cImageFile).

            CREATE FILL-IN hdWidget IN WIDGET-POOL "attachment-widgets"
            ASSIGN
                X             = iSeperatorWidth + 112
                Y             = ((ttAttachments.ttOrder - 1) * iRowHeight) + iStartPositionY
                FRAME         = FRAME {&WIDGET-FRAME}:HANDLE
                WIDTH-PIXELS  = 500
                HEIGHT-PIXELS = 25
                DATA-TYPE     = "CHARACTER"
                FORMAT        = "X(100)"
                VISIBLE       = TRUE
                FONT          = 6
                SCREEN-VALUE  = "Attachment File:  " + ttAttachments.ttAttachFile
                .

            CREATE FILL-IN hdWidget IN WIDGET-POOL "attachment-widgets"
            ASSIGN
                X             = iSeperatorWidth + 112
                Y             = ((ttAttachments.ttOrder - 1) * iRowHeight) + iStartPositionY + 37
                FRAME         = FRAME {&WIDGET-FRAME}:HANDLE
                WIDTH-PIXELS  = 200
                HEIGHT-PIXELS = 25
                DATA-TYPE     = "CHARACTER"
                FORMAT        = "X(100)"
                VISIBLE       = TRUE
                FONT          = 6
                SCREEN-VALUE  = "Estimate:  " + ttAttachments.ttEstimate
                .

            CREATE FILL-IN hdWidget IN WIDGET-POOL "attachment-widgets"
            ASSIGN
                X             = iSeperatorWidth + 350
                Y             = ((ttAttachments.ttOrder - 1) * iRowHeight) + iStartPositionY + 37
                FRAME         = FRAME {&WIDGET-FRAME}:HANDLE
                WIDTH-PIXELS  = 200
                HEIGHT-PIXELS = 25
                DATA-TYPE     = "CHARACTER"
                FORMAT        = "X(100)"
                VISIBLE       = TRUE
                FONT          = 6
                SCREEN-VALUE  = "Item:  " + ttAttachments.ttItem
                .

            CREATE FILL-IN hdWidget IN WIDGET-POOL "attachment-widgets"
            ASSIGN
                X             = iSeperatorWidth + 112
                Y             = ((ttAttachments.ttOrder - 1) * iRowHeight) + iStartPositionY + 75
                FRAME         = FRAME {&WIDGET-FRAME}:HANDLE
                WIDTH-PIXELS  = 200
                HEIGHT-PIXELS = 25
                DATA-TYPE     = "CHARACTER"
                FORMAT        = "X(100)"
                VISIBLE       = TRUE
                FONT          = 6
                SCREEN-VALUE  = "Create Date:  " 
                              + (IF ttAttachments.ttCreateDate NE ? THEN
                                     STRING(ttAttachments.ttCreateDate)
                                 ELSE
                                     "?")
                .
            
            CREATE RECT hdWidget IN WIDGET-POOL "attachment-widgets"
            ASSIGN
                X             = iStartPositionX - 4
                Y             = ((ttAttachments.ttOrder) * iRowHeight)
                FRAME         = FRAME {&WIDGET-FRAME}:HANDLE
                WIDTH-PIXELS  = 690
                HEIGHT-PIXELS = 1
                FGCOLOR       = 0
                VISIBLE       = TRUE
                EDGE-PIXELS   = 1
                .                
        END.
    END.
    SESSION:SET-WAIT-STATE("").
    
    RELEASE bf-job-hdr.
    RELEASE bf-itemfg.
    RELEASE bf-job.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLoadTag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLoadTag W-Win
ON LEAVE OF fiLoadTag IN FRAME F-Main /* Load Tag */
DO:
    DEFINE VARIABLE lValidTag AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobID    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobID2   AS INTEGER   NO-UNDO.
    
    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
        
    cItemType = "RM".
    
    RUN ValidateLoadTag IN hdInventoryProcs (
        INPUT  cCompany,
        INPUT  cItemType,
        INPUT  SELF:SCREEN-VALUE,
        OUTPUT lValidTag,
        OUTPUT cMessage
        ) NO-ERROR.

    IF NOT lValidTag THEN DO:
        cItemType = "FG".
        RUN ValidateLoadTag IN hdInventoryProcs (
            INPUT  cCompany,
            INPUT  cItemType,
            INPUT  SELF:SCREEN-VALUE,
            OUTPUT lValidTag,
            OUTPUT cMessage
            ) NO-ERROR.
    END.
    
    IF NOT lValidTag THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        SELF:SCREEN-VALUE = "".
        RETURN.            
    END.
    
    RUN Inventory_GetLoadTagJob IN hdInventoryProcs (
        INPUT  cCompany,
        INPUT  cItemType,
        INPUT  SELF:SCREEN-VALUE,
        OUTPUT cJobID,
        OUTPUT iJobID2,
        OUTPUT lValidTag,
        OUTPUT cMessage
        ) NO-ERROR.        

    IF cJobID EQ "" THEN DO:
        MESSAGE "Job is empty for the load tag"
            VIEW-AS ALERT-BOX ERROR.
        SELF:SCREEN-VALUE = "".
        RETURN.
    END.
    
    ASSIGN
        fiJobNo:SCREEN-VALUE  = cJobID
        fiJobNo2:SCREEN-VALUE = STRING(iJobID2, "99")
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

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
  DISPLAY fiLoadTag fiJobNo fiJobNo2 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-1 RECT-2 btExit btSearch fiLoadTag fiJobNo fiJobNo2 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW FRAME widgetFrame IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-widgetFrame}
  VIEW W-Win.
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
  
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .
  
    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pImageClicked W-Win 
PROCEDURE pImageClicked :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcAttachmentID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cCommand AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    FIND FIRST ttAttachments 
         WHERE ttAttachments.ttOrder EQ INTEGER(ipcAttachmentID)
         NO-ERROR.
    IF NOT AVAILABLE ttAttachments THEN
        RETURN.
                 
    IF NOT ttAttachments.ttFileFound THEN DO:
        MESSAGE "Attachment file '" + ttAttachments.ttAttachFile + "' is not valid or deleted!"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    /* Command to open file with window default option */       
    RUN OS_RunFile (
        INPUT  ttAttachments.ttAttachFile,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ) NO-ERROR.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit W-Win 
PROCEDURE pInit PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
        cCompany  = g_company
        cLocation = g_loc
        .

    FIND FIRST company NO-LOCK 
         WHERE company.company EQ cCompany
         NO-ERROR .
    IF AVAILABLE company THEN
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                         + " - {&awversion}" + " - " 
                         + STRING(company.name) + " - " + cLocation.
    
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
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

