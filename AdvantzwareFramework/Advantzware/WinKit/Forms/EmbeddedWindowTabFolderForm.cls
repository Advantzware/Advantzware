/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : EmbeddedWindowTabFolderForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 03 14:12:13 CET 2008
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Advantzware.WinKit.Forms.*                   FROM PROPATH .
USING Consultingwerk.*                             FROM PROPATH .
USING Consultingwerk.Framework.*                   FROM PROPATH .
USING Consultingwerk.WindowIntegrationKit.Forms.*  FROM PROPATH .
USING Consultingwerk.WindowIntegrationKit.Helper.* FROM PROPATH .
USING Consultingwerk.Util.*                        FROM PROPATH . 
USING Infragistics.Win.UltraWinTabControl.*        FROM ASSEMBLY . 
USING Infragistics.Win.UltraWinToolbars.*          FROM ASSEMBLY . 
USING Progress.Lang.*                              FROM PROPATH .
USING System.Drawing.*                             FROM ASSEMBLY .         
USING System.Windows.Forms.*                       FROM ASSEMBLY .

CLASS Advantzware.WinKit.Forms.EmbeddedWindowTabFolderForm 
    INHERITS EmbeddedWindowTabFolderFormDesigner 
    IMPLEMENTS IEmbeddedWindowForm, IFormWithUltraToolbarsManager, IEmbeddedWindowTabFolderForm,
               INotes
        USE-WIDGET-POOL :

    DEFINE VARIABLE WinContainer AS Progress.Windows.WindowContainer NO-UNDO.
        
    DEFINE PRIVATE VARIABLE deltaCol AS INTEGER NO-UNDO . 
    DEFINE PRIVATE VARIABLE deltaRow AS INTEGER NO-UNDO .       
    
    DEFINE VARIABLE iWindowStartWidth  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iWindowStartHeight AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE oTimer          AS System.Windows.Forms.Timer NO-UNDO . 
    DEFINE VARIABLE oToolClickTimer AS System.Windows.Forms.Timer NO-UNDO .
    
    DEFINE VARIABLE lSelectingTab AS LOGICAL NO-UNDO INIT FALSE .
    
    DEFINE TEMP-TABLE ttFolderPages NO-UNDO 
        FIELD PageNum AS INTEGER 
        FIELD WindowContainer AS Progress.Lang.Object 
        FIELD TabPage         AS Progress.Lang.Object
        FIELD WindowHandle    AS HANDLE 
        FIELD FrameHandle     AS HANDLE 
        FIELD Finalized       AS LOGICAL INIT FALSE 
        INDEX PageNum IS UNIQUE PRIMARY PageNum . 
    
    DEFINE VARIABLE oCustomizer AS Consultingwerk.WindowIntegrationKit.ITabFolderCustomizer NO-UNDO .
      
    // ******************************************************
    // Begin Notes etc. implementation
    // ******************************************************
             
    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY HasAttachments AS LOGICAL NO-UNDO 
    GET.
    SET (arg AS LOGICAL):
        ultraStatusBar1:Panels ["Attachments"]:Checked = arg . 
    
        THIS-OBJECT:HasAttachments = arg .
    END SET . 

    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY HasCustomFields AS LOGICAL NO-UNDO 
    GET.
    SET (arg AS LOGICAL):
        ultraStatusBar1:Panels ["CustomFields"]:Checked = arg . 
    
        THIS-OBJECT:HasCustomFields = arg .
    END SET . 

    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY HasNotes AS LOGICAL NO-UNDO 
    GET.
    SET (arg AS LOGICAL):
        ultraStatusBar1:Panels ["Notes"]:Checked = arg . 
    
        THIS-OBJECT:HasNotes = arg .
    END SET . 
    
    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisableNotes ():
        
        ASSIGN ultraStatusBar1:Panels ["Attachments"]:Enabled  = FALSE
               ultraStatusBar1:Panels ["CustomFields"]:Enabled = FALSE  
               ultraStatusBar1:Panels ["Notes"]:Enabled        = FALSE
               .
        
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EnableNotes ():

        ASSIGN ultraStatusBar1:Panels ["Attachments"]:Enabled  = TRUE
               ultraStatusBar1:Panels ["CustomFields"]:Enabled = TRUE  
               ultraStatusBar1:Panels ["Notes"]:Enabled        = TRUE
               .
        
    END METHOD .
    
    // ******************************************************
    // Begin Notes etc. implementation
    // ******************************************************
    
    
    
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the EmbeddedWindowTabFolderForm class                                                                      
        Notes:                                                                    
    ------------------------------------------------------------------------------*/    
    CONSTRUCTOR PUBLIC EmbeddedWindowTabFolderForm ():
        SUPER ().
        
        THIS-OBJECT:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1 . 
        
        ASSIGN THIS-OBJECT:WindowContainerColOffset = WinKitForms:DefaultWindowContainerColOffset
               THIS-OBJECT:WindowContainerRowOffset = WinKitForms:DefaultWindowContainerRowOffset .
        
        IF VALID-OBJECT(THIS-OBJECT:ToolbarsManager) THEN DO:
            THIS-OBJECT:ToolbarsManager:ToolClick:Subscribe (ToolClickHandler) .
            THIS-OBJECT:ToolbarsManager:BeforeToolDropdown:Subscribe (ToolDropDownHandler) .
        END.
               
        THIS-OBJECT:FormClosing:Subscribe(FormClosingHandler) .
        
        oTimer = NEW System.Windows.Forms.Timer () .
        oTimer:Interval = 1 . 
        oTimer:Enabled = FALSE .
        oTimer:Tick:Subscribe (TimerTickHandler) .
        
        oToolClickTimer = NEW System.Windows.Forms.Timer () .
        oToolClickTimer:Interval = 1 . 
        oToolClickTimer:Enabled = FALSE .
        oToolClickTimer:Tick:Subscribe (ToolClickTimerTickHandler) .
           
        THIS-OBJECT:ultraTabControl1:ActiveTabChanging:Subscribe (ActiveTabChangingHandler) .
        
        THIS-OBJECT:TabFolder = THIS-OBJECT:ultraTabControl1 . 
         
        oCustomizer = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.ITabFolderCustomizer} .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler for the ActiveTabChanging event handler                                                                     
        Notes:                                                        
        @param sender The reference to the object instance that raised the event
        @param e The ActiveTabChangingEventArgs instance with the data for this event                  
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ActiveTabChangingHandler (sender AS System.Object, 
                                                    e AS ActiveTabChangingEventArgs):
        
        DEFINE VARIABLE iPage AS INTEGER NO-UNDO.

        /* prevent recursion / infinitive loop */
        IF lSelectingTab THEN RETURN .

        ASSIGN iPage = INTEGER (e:Tab:Key) NO-ERROR . 
        
        IF iPage > 0  AND VALID-HANDLE (THIS-OBJECT:ProcedureHandle) THEN DO:
            IF ProcedureHelper:HasEntry (THIS-OBJECT:ProcedureHandle, 
                                         "select-page":U, 
                                         EntryTypeEnum:Procedure) THEN 
                RUN select-page IN (THIS-OBJECT:ProcedureHandle) (iPage) .
            ELSE
            IF ProcedureHelper:HasEntry (THIS-OBJECT:ProcedureHandle, 
                                         "selectPage":U, 
                                         EntryTypeEnum:Procedure) THEN
                RUN selectPage IN (THIS-OBJECT:ProcedureHandle) (iPage) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Assigns the Labels of the Tabfolder based on a Character List                                                                        
        Notes:                                                  
        @param pcLabels A delimited list of tab labels
        @param pcDelimiter The list delimited, like the | (pipe) for ADM2 folder                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AssignTabLabels (pcLabels AS CHARACTER,
                                        pcDelimiter AS CHARACTER):
        
        DEFINE VARIABLE i      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cLabel AS CHARACTER NO-UNDO.

        DO i = 1 TO NUM-ENTRIES (pcLabels, pcDelimiter):

            ASSIGN cLabel = ENTRY (i, pcLabels, pcDelimiter) .
            
            IF i <= THIS-OBJECT:TabFolder:Tabs:Count THEN 
                THIS-OBJECT:TabFolder:Tabs [i - 1]:Text = cLabel .
                
            IF THIS-OBJECT:ToolbarsManager:Ribbon:Tabs:Exists ("page_" + STRING (i)) THEN 
                THIS-OBJECT:ToolbarsManager:Ribbon:Tabs["page_" + STRING (i)]:Caption = cLabel .     

            IF THIS-OBJECT:ToolbarsManager:Ribbon:ContextualTabGroups:Exists ("page_" + STRING (i)) THEN 
                THIS-OBJECT:ToolbarsManager:Ribbon:ContextualTabGroups["page_" + STRING (i)]:Caption = cLabel .     
                 
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Closes the Form after unsubscribing it's own FormClosing event handle                                                                        
        Notes:   Invoked from src/winkit/closewindow.i after the ABL window WINDOW-CLOSE
                 event handler has not cancelled the closing of the WINDOW and Form                                                                           
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CloseFromCode ():
        
        THIS-OBJECT:FormClosing:Unsubscribe(FormClosingHandler) .
        
        THIS-OBJECT:Close () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Closes the dynamically created windows and WindowContainer objects                                                                       
        Notes:   Used by closewindow.i                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CloseTabs ():
        
        FOR EACH ttFolderPages:

            IF VALID-HANDLE (ttFolderPages.WindowHandle) THEN 
                DELETE OBJECT ttFolderPages.WindowHandle .
                
            IF VALID-OBJECT (ttFolderPages.WindowContainer) THEN     
                DELETE OBJECT ttFolderPages.WindowContainer .
        END.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a Tab on the TabFolder with the given Label and key                                                                       
        Notes:                                                              
        @param pcTabText The label to use for the new tab
        @param piTabKey The 1 based number for the tab to create          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateTab (pcTabText AS CHARACTER,
                                  piTabKey AS INTEGER):
        
        DEFINE VARIABLE oTab             AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO .
        DEFINE VARIABLE hWindow          AS HANDLE                                       NO-UNDO.
        DEFINE VARIABLE hFrame           AS HANDLE                                       NO-UNDO.
        DEFINE VARIABLE oWindowContainer AS Progress.Windows.WindowContainer             NO-UNDO.
        
        FIND ttFolderPages WHERE ttFolderPages.PageNum = piTabKey NO-ERROR .
    
        IF NOT AVAILABLE ttFolderPages THEN DO:
            CreateTabPageFrame (piTabKey) .
            
            FIND ttFolderPages WHERE ttFolderPages.PageNum = piTabKey NO-ERROR .
        END. 
        ELSE 
            RETURN . 

        ASSIGN hWindow = ttFolderPages.WindowHandle 
               hFrame  = ttFolderPages.FrameHandle 
               oTab    = CAST (ttFolderPages.TabPage, Infragistics.Win.UltraWinTabControl.UltraTab)
               oWindowContainer = CAST (ttFolderPages.WindowContainer, Progress.Windows.WindowContainer)
               .
               
        oTab:Text = pcTabText .  
        oTab:Key = STRING (piTabKey) .             
               
        ASSIGN hWindow:VISIBLE = TRUE . 
        
        VIEW hFrame IN WINDOW hWindow . 
/*        ASSIGN hFrame:VISIBLE = TRUE .*/

        RELEASE ttFolderPages .

        IF VALID-OBJECT (oCustomizer) THEN 
            oCustomizer:AssignTabProperties (THIS-OBJECT:ProcedureHandle,
                                             THIS-OBJECT,
                                             piTabKey,
                                             pcTabText,
                                             ultraTabControl1,
                                             oTab) .  

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates the Window and Frame used for folder pages
        Notes:   Creates ttFolderPages record                                                                     
        @param piPage The page number to create a window and frame for
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateTabPageFrame (piPage AS INTEGER):
        
        DEFINE VARIABLE oTab             AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO .
        DEFINE VARIABLE oWindowContainer AS Progress.Windows.WindowContainer             NO-UNDO.
        DEFINE VARIABLE hWindow          AS HANDLE                                       NO-UNDO.
        DEFINE VARIABLE hFrame           AS HANDLE                                       NO-UNDO.       
        
        CREATE WINDOW hWindow
            ASSIGN VIRTUAL-WIDTH  = THIS-OBJECT:EmbeddedWindow:WIDTH 
                   VIRTUAL-HEIGHT = THIS-OBJECT:EmbeddedWindow:HEIGHT 
                   WIDTH          = THIS-OBJECT:EmbeddedWindow:WIDTH 
                   HEIGHT         = THIS-OBJECT:EmbeddedWindow:HEIGHT
                   BGCOLOR        = WinKitForms:TabFolderBGColor
                   MAX-WIDTH      = 320 
                   MAX-HEIGHT     = 200
                   STATUS-AREA    = FALSE
                   MESSAGE-AREA   = FALSE 
                   NAME           = "Folderpage Window ":U + STRING (piPage) 
                   PRIVATE-DATA   = SUBSTITUTE ("WinKitTab&1&2&1&3&1&4":U,
                                                CHR(10),
                                                piPage,
                                                THIS-OBJECT:ProcedureHandle,
                                                THIS-OBJECT:EmbeddedWindow) .                   
    
        CREATE FRAME hFrame
            ASSIGN VIRTUAL-WIDTH  = hWindow:WIDTH 
                   VIRTUAL-HEIGHT = hWindow:HEIGHT
                   WIDTH          = hWindow:WIDTH
                   HEIGHT         = hWindow:HEIGHT
                   PARENT         = hWindow
                   BGCOLOR        = WinKitForms:TabFolderBGColor
                   BOX            = FALSE 
                   NAME           = "Folderpage Frame ":U + STRING (piPage). 
                    
        oTab = ultraTabControl1:Tabs:Add (GUID, STRING (piPage)) .
        
        oWindowContainer = NEW Progress.Windows.WindowContainer ().                
        oWindowContainer:Size = NEW Size ( hWindow:WIDTH-PIXELS, hWindow:HEIGHT-PIXELS ).
        oWindowContainer:EmbeddedWindow = hWindow.
        oWindowContainer:Dock = DockStyle:FILL .

        THIS-OBJECT:WindowContainers:Add (oWindowContainer) .
    
        CREATE ttFolderPages .
        ASSIGN ttFolderPages.PageNum         = piPage 
               ttFolderPages.WindowHandle    = hWindow 
               ttFolderPages.FrameHandle     = hFrame
               ttFolderPages.TabPage         = oTab 
               ttFolderPages.WindowContainer = oWindowContainer
               .

        RELEASE ttFolderPages .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates Tabs for a list of Tabs 
        Notes:                                                                        
        @param pcLabels A charcater delimited list of tab labels
        @param pcDelimiter The character used as the delimiter of the list
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateTabs (pcLabels AS CHARACTER,
                                   pcDelimiter AS CHARACTER):
        
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        
        DO i = 1 TO NUM-ENTRIES (pcLabels, pcDelimiter):
        
            THIS-OBJECT:CreateTab (ENTRY (i, pcLabels, pcDelimiter), i) .
        
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Creates Tabs for a list of Tabs 
        Notes:   Uses the comma as the default delimiter    
        @param pcLabels A comma-charcater delimited list of tab labels
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateTabs (pcLabels AS CHARACTER):

        THIS-OBJECT:CreateTabs (pcLabels, ",":U) .
    END.
    
    /*------------------------------------------------------------------------------
        Purpose: Deletes (removes) a Tab Page 
        Notes:   
        @param piPage The tab page to remove
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeleteTabPage (piPage AS INTEGER):
        
    DEFINE VARIABLE oTab             AS UltraTab NO-UNDO .
    DEFINE VARIABLE oWindowContainer AS Control  NO-UNDO . 

    FIND ttFolderPages WHERE ttFolderPages.PageNum = piPage NO-ERROR .

    IF AVAILABLE ttFolderPages THEN DO:
        ASSIGN oTab    = CAST (ttFolderPages.TabPage, Infragistics.Win.UltraWinTabControl.UltraTab) .
        
        IF VALID-OBJECT (ttFolderPages.WindowContainer) THEN DO:
        
            oWindowContainer = CAST (ttFolderPages.WindowContainer,
                                     Control) .
        
            oTab:TabControl:Controls:Remove (oWindowContainer) .            

            DELETE OBJECT oWindowContainer . 
        END.

        THIS-OBJECT:ultraTabControl1:Tabs:Remove (oTab) .
        
        GarbageCollectorHelper:DeleteObject (oTab) .
    END.
    
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Disables a folder Page                                                                     
        Notes:                          
        @param piPage The page number to disable (based on 1)                                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisableFolderPage (piPage AS INTEGER):
        
        DEFINE VARIABLE oTab             AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO .

        FIND ttFolderPages WHERE ttFolderPages.PageNum = piPage NO-ERROR .

        IF AVAILABLE ttFolderPages THEN DO:
            ASSIGN oTab    = CAST (ttFolderPages.TabPage, Infragistics.Win.UltraWinTabControl.UltraTab) .

            oTab:Enabled = FALSE .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a new instance of the Progress.Windows.WindowContainer 
                 instance and embeds the given window widget in that instance - 
                 but does not parent the WindowContainer instance in the Form 
                 (THIS-OBJECT). This is delayed till later.    
        Notes:   Required due to incombatibily issues with the Infragistics 
                 UltraTabbedMdiManager.                                                                       
        @param phWindow The handle of the Window to embed                                                                         
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID EmbedWindow (phWindow AS HANDLE):         
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        
        ASSIGN 
            iWindowStartHeight         = phWindow:HEIGHT-PIXELS
            iWindowStartWidth          = phWindow:WIDTH-PIXELS  
            THIS-OBJECT:EmbeddedWindow = phWindow . 
                 
        WinContainer = NEW Progress.Windows.WindowContainer( ).
        
        THIS-OBJECT:WindowContainers:Add (WinContainer) .
        
        THIS-OBJECT:ClientSize = NEW Size (phWindow:WIDTH-PIXELS + (THIS-OBJECT:ClientSize:Width - 
                                                                    0 //THIS-OBJECT:splitContainer1:Panel1:ClientSize:Width
                                                                    ), 
                                           phWindow:HEIGHT-PIXELS +  (THIS-OBJECT:ClientSize:Height - 
                                                                      0 // THIS-OBJECT:splitContainer1:Panel1:ClientSize:Height
                                                                      )).
        WinContainer:Size = NEW Size(phWindow:WIDTH-PIXELS, phWindow:HEIGHT-PIXELS).
        WinContainer:MinimumSize = NEW Size(phWindow:WIDTH-PIXELS, phWindow:HEIGHT-PIXELS).
        
        WinContainer:EmbeddedWindow = phWindow.       
        
        /* Mike Fechner, Consultingwerk Ltd. 23.09.2009
           Ability to set a logical offset for the location of the WindowContainer 
           within this .NET Form. Enables "hiding" of the empty space resulting
           that results from a hidden toolbar  */           
           
        RepositionWindowContainer () .   

        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        WinContainer:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Enables a folder Page                                                                     
        Notes:                                                                        
        @param piPage The page number to enable (based on 1)                                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EnableFolderPage (piPage AS INTEGER):
        
        DEFINE VARIABLE oTab             AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO .
        
        FIND ttFolderPages WHERE ttFolderPages.PageNum = piPage NO-ERROR .
    
        IF AVAILABLE ttFolderPages THEN DO:
            ASSIGN oTab    = CAST (ttFolderPages.TabPage, Infragistics.Win.UltraWinTabControl.UltraTab) .
            
            oTab:Enabled = TRUE . 
        END. 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Finalize the embedding of an ABL window widget by parenting 
                 the Progress.Windows.WindowContainer instance in the Form
                 (THIS-OBJECT). Sets the Form Text property and applies the 
                 window-resized event to the window widget.     
        Notes:   Required due to incombatibily issues with the Infragistics 
                 UltraTabbedMdiManager.                                                                       
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID FinalizeEmbedding ():
        DEFINE VARIABLE hWindow AS HANDLE NO-UNDO.
        
        ASSIGN hWindow = THIS-OBJECT:EmbeddedWindow 
            
               THIS-OBJECT:Finalized = TRUE .
        
        THIS-OBJECT:EmbeddedWindowTabFolderFormDesigner_Fill_Panel:SuspendLayout() .
        
        THIS-OBJECT:EmbeddedWindowTabFolderFormDesigner_Fill_Panel:Dock = System.Windows.Forms.DockStyle:None.
        
        THIS-OBJECT:EmbeddedWindowTabFolderFormDesigner_Fill_Panel:Size = NEW System.Drawing.Size (WinContainer:WIDTH + WinContainer:LEFT,
                                                                                          WinContainer:HEIGHT + WinContainer:Top ) . 
        
        WinContainer:Parent = THIS-OBJECT:ClientArea.

        WinContainer:Size = NEW System.Drawing.Size (
                MAX(THIS-OBJECT:EmbeddedWindowTabFolderFormDesigner_Fill_Panel:Width + deltaCol, 0),
                MAX(THIS-OBJECT:EmbeddedWindowTabFolderFormDesigner_Fill_Panel:Height + deltaRow, 0)).
        
        THIS-OBJECT:EmbeddedWindowTabFolderFormDesigner_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.

        THIS-OBJECT:Size = NEW System.Drawing.Size (
                             THIS-OBJECT:Width -
                             THIS-OBJECT:EmbeddedWindowTabFolderFormDesigner_Fill_Panel:Width + 
                             iWindowStartWidth - 
                             THIS-OBJECT:WindowContainerColOffset,
        
                             THIS-OBJECT:Height -
                             THIS-OBJECT:EmbeddedWindowTabFolderFormDesigner_Fill_Panel:Height + 
                             iWindowStartHeight - 
                             THIS-OBJECT:WindowContainerRowOffset) .
      

        THIS-OBJECT:EmbeddedWindowTabFolderFormDesigner_Fill_Panel:ResumeLayout(TRUE) .

        THIS-OBJECT:TEXT = hWindow:TITLE .
        
        APPLY "window-resized":U TO hWindow .            
        
    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Fianlizes the Embedding of a single Tab Page                                                                     
        Notes:                                               
        @param piPage The Tab Page Number                         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FinalizeTabPage (piPage AS INTEGER):
        
        DEFINE VARIABLE oTab             AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO . 
        DEFINE VARIABLE oWindowContainer AS Progress.Windows.WindowContainer             NO-UNDO . 
        
        DEFINE VARIABLE hFrame           AS HANDLE NO-UNDO.
        
        DEFINE VARIABLE iMinX AS INTEGER NO-UNDO.
        DEFINE VARIABLE iMaxX AS INTEGER NO-UNDO.
        DEFINE VARIABLE iMinY AS INTEGER NO-UNDO.
        DEFINE VARIABLE iMaxY AS INTEGER NO-UNDO.
        
        FIND ttFolderPages WHERE ttFolderPages.PageNum = piPage NO-ERROR .

        IF AVAILABLE ttFolderPages THEN DO:

            IF ttFolderPages.Finalized = TRUE THEN 
                RETURN . 
            
            ASSIGN ttFolderPages.Finalized = TRUE 
                   oTab                    = CAST (ttFolderPages.TabPage, 
                                                   Infragistics.Win.UltraWinTabControl.UltraTab)
                   oWindowContainer        = CAST (ttFolderPages.WindowContainer, 
                                                   Progress.Windows.WindowContainer).

            oTab:TabPage:Controls:Add (oWindowContainer) .
            oWindowContainer:Visible = TRUE .

            ASSIGN hFrame = ttFolderPages.FrameHandle:FIRST-CHILD:FIRST-CHILD .

            DO WHILE VALID-HANDLE (hFrame):

                IF iMinX = 0 THEN
                    iMinX = hFrame:X .
                IF iMinY = 0 THEN
                    iMinY = hFrame:Y .

                ASSIGN
                    iMaxX = MAX (iMaxX, hFrame:X + hFrame:WIDTH-PIXELS)
                    iMaxY = MAX (iMaxY, hFrame:Y + hFrame:HEIGHT-PIXELS)
                    
                    iMinX = MIN (iMinX, hFrame:X)
                    iMinY = MIN (iMinY, hFrame:Y)
                        .

                ASSIGN hFrame = hFrame:NEXT-SIBLING .
            END.

            ASSIGN hFrame = ttFolderPages.FrameHandle:FIRST-CHILD:FIRST-CHILD .

            DO WHILE VALID-HANDLE (hFrame):

                ASSIGN hFrame:X       = hFrame:X - iMinX
                       hFrame:Y       = hFrame:Y - iMinY
                       hFrame:BGCOLOR = WinKitForms:TabFolderBGColor.

                ASSIGN hFrame = hFrame:NEXT-SIBLING .
            END.

            {Consultingwerk/foreach.i System.Windows.Forms.Control oControl in oTab:TabPage:Controls}
                IF oControl <> oWindowContainer THEN DO:
                    ASSIGN oControl:Left = oControl:Left - iMinX
                           oControl:Top  = oControl:Top  - iMinY .
                    oControl:BringToFront () . 
                END.
            END.
        

            ASSIGN
                ttFolderPages.FrameHandle:WIDTH-PIXELS = iMaxX + 1 - iMinX
                ttFolderPages.FrameHandle:HEIGHT-PIXELS = iMaxY + 1 - iMinY
                ttFolderPages.FrameHandle:VIRTUAL-WIDTH-PIXELS = ttFolderPages.FrameHandle:WIDTH-PIXELS
                ttFolderPages.FrameHandle:VIRTUAL-HEIGHT-PIXELS = ttFolderPages.FrameHandle:HEIGHT-PIXELS
/*                ttFolderPages.WindowHandle:VIRTUAL-WIDTH-PIXELS = ttFolderPages.FrameHandle:WIDTH-PIXELS  */
/*                ttFolderPages.WindowHandle:VIRTUAL-HEIGHT-PIXELS = ttFolderPages.FrameHandle:HEIGHT-PIXELS*/
                .

        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detects Folder98 and other SmartFrames/Viewers                                                                       
        Notes:   Important for visibility and positioning of the UltraTabFoler
        @param plFolder98 OUTPUT True, when an folder98.w instance was found
        @param plOtherObject OUTPUT True, when another object was found                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FindPageZeroChilds (OUTPUT plFolder98 AS LOGICAL,
                                           OUTPUT plOtherObject AS LOGICAL):
        
        DEFINE VARIABLE cTargets     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cPageTargets AS CHARACTER NO-UNDO EXTENT 50 .
        DEFINE VARIABLE cObjectType  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
        DEFINE VARIABLE j            AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hProcedure   AS HANDLE    NO-UNDO.
        DEFINE VARIABLE lFound       AS LOGICAL   NO-UNDO.

        IF NOT Consultingwerk.Util.ProcedureHelper:HasEntry (THIS-OBJECT:ProcedureHandle, "GET-ATTRIBUTE":U, "PROCEDURE":U) THEN  
            RETURN . 
        
        ASSIGN plFolder98    = FALSE 
               plOtherObject = FALSE . 
        
        objectloop: DO i = 1 TO NUM-ENTRIES (cTargets):
            ASSIGN hProcedure = WIDGET-HANDLE (ENTRY (i, cTargets)) .
            
            RUN GET-ATTRIBUTE IN hProcedure ("TYPE":U) .
            
            ASSIGN cObjectType = RETURN-VALUE 
                   lFound      = FALSE .
            
            /* try to get object's page */
            pagesearch: DO j = 1 TO 50: 
                IF ListHelper:EntryIsInList (STRING (hProcedure), cPageTargets[j]) THEN DO:
                    lFound = TRUE . 
                    LEAVE pagesearch .
                END.                         
            END.
            
            IF NOT lFound THEN j = 0 .
            
            IF j > 0 THEN NEXT objectloop. 
            
            IF hProcedure:FILE-NAME = "adm/objects/folder98.w":U THEN     
                ASSIGN plFolder98 = TRUE . 
            ELSE
                IF cObjectType <> "SmartQuery":U AND cObjectType <> "SmartPanel":U AND 
                   hProcedure:FILE-NAME <> "objects/sframe/f-mntcpy.w":U AND 
                   hProcedure:FILE-NAME <> "adm/objects/status.w":U THEN 
                   
                   ASSIGN plOtherObject = TRUE . 
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler for the FormClosing event of the Form. When a window
                 is embedded, the FormClosing event will be cancelled and the 
                 WINDOW-CLOSE event to the window will be applied.                                                                        
        Notes:   The window is responsible for deleting the Form.  
                 {src/winkit/closewindow.i} can be used in the ON CLOSE OF THIS-
                 PROCEDURE trigger for that purpose                                                                       
        @param sender The reference to the object that raised the event
        @param e The FormClosingEventArgs with the data for this event                                                                                      
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID FormClosingHandler(sender AS System.Object, 
                                          e AS FormClosingEventArgs):

        DEFINE VARIABLE hWindow AS HANDLE NO-UNDO.
        
        ASSIGN hWindow = THIS-OBJECT:EmbeddedWindow .
    
        /* Mike Fechner, Consultingwerk Ltd. 22.02.2012 
           When the WINDOW Widget is in-SENSITIVE, cancel the FormClosing
           Event in any case */
        IF VALID-HANDLE (hWindow) AND hWindow:SENSITIVE = FALSE THEN DO:
           e:Cancel = TRUE .
           RETURN .
        END.
        
        IF Progress.Util.EnumHelper:AreEqual(e:CloseReason,
                                             CloseReason:UserClosing) THEN 
           IF VALID-HANDLE(hWindow) THEN DO:
               e:Cancel = TRUE .    
        
               oTimer:ENABLED = TRUE .                          

               /* Mike Fechner, Consultingwerk Ltd. 24.02.2012 - WinKit PoC
                  APPLY window-close moved to the Timer-Tick event handler,
                  so that the .NET Form has finished it's Close event handler, 
                  when the ABL WINDOW-CLOSE behaviour is started. */

/*                 APPLY "window-close":U TO hWindow .                             */
           END.                     

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the FormClosed event  
        Notes:   
        @param e The FormClosedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnFormClosed (e AS System.Windows.Forms.FormClosedEventArgs):

        IF VALID-OBJECT (oToolClickTimer) THEN DO:
            oToolClickTimer:Enabled = FALSE . 
            oToolClickTimer:Tick:Unsubscribe (ToolClickTimerTickHandler) .
            
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oToolClickTimer) .
        END.
        
        SUPER:OnFormClosed(INPUT e).

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Raises the SizeChanged event                                                                       
        Notes:   Don't forward sizing events when the window is minimized                                                                     
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnSizeChanged ( INPUT e AS System.EventArgs ):
        
        IF Progress.Util.EnumHelper:AreEqual (System.Windows.Forms.FormWindowState:Minimized,
                                              THIS-OBJECT:WindowState) THEN . 
        ELSE 
            SUPER:OnSizeChanged (INPUT e).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the HANDLE of the ABL Frame that was created as the default 
                 frame for a tab page                                                                      
        Notes:                                                                        
        @param piPage The 1 based index of the tab page
        @return The HANDLE of the ABL Frame that was created as the default 
                frame for a tab page 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetTabPageFrame (piPage AS INTEGER):
        
        FIND ttFolderPages WHERE ttFolderPages.PageNum = piPage NO-ERROR .
    
        IF AVAILABLE ttFolderPages THEN 
            RETURN ttFolderPages.FrameHandle .
        ELSE DO:
            CreateTabPageFrame (piPage) .     

            RETURN GetTabPageFrame (piPage) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the HANDLE of the ABL Window that was created for a tab page                                                                      
         Notes:                                                                        
        @param piPage The 1 based index of the tab page
        @return The HANDLE of the ABL Window that was created for a tab page 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetTabPageWindow (piPage AS INTEGER):
        
        FIND ttFolderPages WHERE ttFolderPages.PageNum = piPage NO-ERROR .
    
        IF AVAILABLE ttFolderPages THEN 
            RETURN ttFolderPages.WindowHandle .
        RETURN ? .
    
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns an indeterminate array of the HANDLEs of the embedded Windows
        Notes:   Returns a size 1 array with the EmbeddedWindow HANDLE
        @return The array of window HANDLE's
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE HANDLE EXTENT GetEmbeddedWindows ():

        DEFINE VARIABLE hWindows AS HANDLE  EXTENT NO-UNDO . 
        DEFINE VARIABLE iWindow  AS INTEGER        NO-UNDO INIT 1 .
        
        FOR EACH ttFolderPages:
            ASSIGN iWindow = iWindow + 1 . 
        END.
        
        ASSIGN EXTENT (hWindows) = iWindow 
               hWindows[1]       = THIS-OBJECT:EmbeddedWindow 
               iWindow           = 1 . 
        
        FOR EACH ttFolderPages:
            ASSIGN iWindow           = iWindow + 1 
                   hWindows[iWindow] = ttFolderPages.WindowHandle . 
        END.
       
        RETURN hWindows .         
    END.

    /*------------------------------------------------------------------------------
        Purpose: Returns the UltraTab Control that was created for a tab page                                                                      
        Notes:                                                                        
        @param piPage The 1 based index of the tab page
        @return The reference to the UltraTab Control that was created for a tab page
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Windows.Forms.Control GetTabPageControl (piPage AS INTEGER):
        
        FIND ttFolderPages WHERE ttFolderPages.PageNum = piPage NO-ERROR .
    
        IF AVAILABLE ttFolderPages THEN 
            RETURN CAST (ttFolderPages.TabPage, Infragistics.Win.UltraWinTabControl.UltraTab):TabPage .
        ELSE DO:
            CreateTabPageFrame (piPage) .     

            RETURN GetTabPageControl (piPage) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the WindowContainer Control that was created for a tab page                                                                      
        Notes:                                                                        
        @param piPage The 1 based index of the tab page
        @return The reference to the WindowContainer Control that was created for a tab page
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Progress.Windows.WindowContainer GetTabPageWindowContainer (piPage AS INTEGER):
        
        FIND ttFolderPages WHERE ttFolderPages.PageNum = piPage NO-ERROR .
    
        IF AVAILABLE ttFolderPages THEN 
            RETURN CAST (ttFolderPages.WindowContainer, Progress.Windows.WindowContainer) .
        ELSE DO:
            CreateTabPageFrame (piPage) .     

            RETURN GetTabPageWindowContainer (piPage) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the state of all overlay buttons in the IEmbeddedWindowForm                                                                      
        Notes: 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RefreshButtons ():
        
        DEFINE VARIABLE oWindowContainer AS System.Windows.Forms.Control . 
        
        Consultingwerk.Util.WidgetHelper:RefreshButtons (THIS-OBJECT:ClientArea) .
        
        FOR EACH ttFolderPages:
        
            IF NOT VALID-OBJECT (ttFolderPages.WindowContainer) THEN 
              NEXT .
            
            oWindowContainer = CAST (ttFolderPages.WindowContainer, 
                                     System.Windows.Forms.Control) .
          
            IF VALID-OBJECT (oWindowContainer) AND VALID-OBJECT (oWindowContainer:Parent) THEN 
            Consultingwerk.Util.WidgetHelper:RefreshButtons (oWindowContainer:Parent) .
        END.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adjusts the position of the WindowContainer based on the Col- and
                 Row-Offset                                                               
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID RepositionWindowContainer ():

        IF NOT VALID-OBJECT (WinContainer) THEN
            RETURN . 
        
        WinContainer:Location = NEW System.Drawing.Point (0 - deltaCol, 0 - deltaRow) . 
        WinContainer:Size = NEW System.Drawing.Size (WinContainer:Width + deltaCol, WinContainer:Height + deltaRow) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets the Splitter to fixed, disallow users to change the splitter 
                 distance 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetSplitterFixed ():
        
/*        THIS-OBJECT:splitContainer1:IsSplitterFixed = TRUE.*/
/*        THIS-OBJECT:splitContainer1:SplitterWidth = 1.     */
        
    END METHOD .

    /*------------------------------------------------------------------------------
       Purpose: Event Handler for the Tick Event of the Timer Component                                                                                                                                                                                                                                                                        
       Notes:                                                          
       @param sender The event sender
       @param e The .NET default System.EventArgs for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID TimerTickHandler (sender AS System.Object,
                                            e AS System.EventArgs):
                   
       DEFINE VARIABLE hWindow AS HANDLE NO-UNDO.
                   
        IF VALID-HANDLE (THIS-OBJECT:EmbeddedWindow) THEN DO:
            
            ASSIGN hWindow = THIS-OBJECT:EmbeddedWindow . 
            
            APPLY "WINDOW-CLOSE":U TO hWindow .
        END.
                   
        CATCH err AS Progress.Lang.Error :
            /* noop */         
        END CATCH.                                                      

        FINALLY:
            oTimer:Enabled = FALSE .                               
        END FINALLY.

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Selects a TabPage                                                                     
        Notes:                    
        @param piPage The tab page number                                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SelectTab (piPage AS INTEGER):
        
        DEFINE VARIABLE oTab             AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO .
        
        ASSIGN lSelectingTab = TRUE . 
        
        FIND ttFolderPages WHERE ttFolderPages.PageNum = piPage NO-ERROR .
    
        IF AVAILABLE ttFolderPages THEN DO:
            ASSIGN oTab    = CAST (ttFolderPages.TabPage, Infragistics.Win.UltraWinTabControl.UltraTab) .
            
            THIS-OBJECT:ultraTabControl1:SelectedTab = oTab .
        END. 
    
    
    
        {Consultingwerk/foreach.i ContextualTabGroup oTabGroup in this-object:ToolbarsManager:Ribbon:ContextualTabGroups}
            
            oTabGroup:Visible = (oTabGroup:Key = "page_" + STRING (piPage)) . 
        END.
        
        IF THIS-OBJECT:ToolbarsManager:Ribbon:ContextualTabGroups:Exists ("page_" + STRING (piPage)) THEN DO: 
            THIS-OBJECT:ToolbarsManager:Ribbon:SelectedTab = 
                THIS-OBJECT:ToolbarsManager:Ribbon:ContextualTabGroups["page_" + STRING (piPage)]:Tabs[0] .
            
            IF VALID-OBJECT (THIS-OBJECT:ToolbarsManager:MdiParentManager) AND 
                 THIS-OBJECT:ToolbarsManager:Ribbon:ContextualTabGroups:Exists ("page_" + STRING (piPage)) THEN  
                THIS-OBJECT:ToolbarsManager:MdiParentManager:Ribbon:SelectedTab = 
                    THIS-OBJECT:ToolbarsManager:Ribbon:ContextualTabGroups["page_" + STRING (piPage)]:Tabs[0]:AttachedParentTab .
        END.
        ELSE 
            IF VALID-OBJECT (THIS-OBJECT:ToolbarsManager:MdiParentManager) AND 
                 THIS-OBJECT:ToolbarsManager:Ribbon:Tabs:Count > 0 THEN  
                THIS-OBJECT:ToolbarsManager:MdiParentManager:Ribbon:SelectedTab = 
                    THIS-OBJECT:ToolbarsManager:Ribbon:Tabs[0]:AttachedParentTab .
        
    
        FINALLY:
            ASSIGN lSelectingTab = FALSE .      
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler for the ToolClick Event of the UltraToolbarsManager                                                                                                                                                                                                                                                                              
        Notes:   Applies CHOOSE to the base menu item
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event                                                          
    ------------------------------------------------------------------------------*/    
    METHOD PRIVATE VOID ToolClickHandler (sender AS System.Object,
                                          e AS ToolClickEventArgs):
                        
        DEFINE VARIABLE hMenuItem   AS HANDLE NO-UNDO.
        DEFINE VARIABLE hFrame      AS HANDLE NO-UNDO.
        DEFINE VARIABLE ReturnValue AS INTEGER NO-UNDO.
        
        THIS-OBJECT:UpdateControlsForEventHandler () .
        
        ASSIGN hMenuItem = WIDGET-HANDLE(STRING(THIS-OBJECT:ToolbarsManager:Tools[e:Tool:Key]:Tag)) .
        
        IF VALID-HANDLE(hMenuItem) THEN 
        CASE hMenuItem:TYPE:
            WHEN "MENU-ITEM":U THEN DO:
                CASE hMenuItem:SUBTYPE:
                    WHEN "NORMAL":U THEN DO:
                        IF TYPE-OF (e:Tool, StateButtonTool) 
                           AND CAN-QUERY (hMenuItem, "TOGGLE-BOX":U) 
                           AND hMenuItem:TOGGLE-BOX = TRUE 
                           AND NOT Consultingwerk.Util.UltratoolbarsHelper:RefreshingToolbar
                        THEN DO:
                            
                            hMenuItem:CHECKED = CAST (e:Tool, StateButtonTool):Checked .
                            
                            APPLY "VALUE-CHANGED":U TO hMenuItem .
                        END.
                        ELSE 
                        APPLY "CHOOSE":U TO hMenuItem .
                    END.
                    OTHERWISE 
                        MESSAGE SUBSTITUTE ("ToolClick event for MENU-ITEM of sub-type &1 not yet implemented."{&TRAN},
                                            hMenuItem:SUBTYPE) .
                END CASE .                          
            END .
            WHEN "BUTTON":U THEN DO:
                oToolClickTimer:Tag = BOX (STRING (hMenuItem)) .
                oToolClickTimer:Enabled = TRUE .             END.
            OTHERWISE MESSAGE SUBSTITUTE ("ToolClick event for MENU-ITEM of type &1 not yet implemented."{&TRAN},
                                          hMenuItem:TYPE) .             
        END CASE.  

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Tick event of the ToolClickTimer component 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ToolClickTimerTickHandler (sender AS System.Object,
                                                   e AS System.EventArgs):

        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
        
        oToolClickTimer:Enabled = FALSE . 

        ASSIGN hWidget = WIDGET-HANDLE (UNBOX (oToolClickTimer:Tag)) NO-ERROR . 
        
        oToolClickTimer:Tag = ? .
                                                         
        IF NOT VALID-HANDLE (hWidget) THEN 
            RETURN . 
                                                         
        APPLY "CHOOSE":U TO hWidget .
        
    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Handles the BeforeToolDropDown event of the UltraToolbarsManager
       Notes:   Delegates action to the MENU-DROP Event of the Progress SUB-MENU widget                                                                                                                                                                                                                                                                           
       @param sender The reference to the object that raised the event
       @param e The BeforeToolDropdownEventArgs with the data for this event                                                                                                                                                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ToolDropDownHandler (sender AS System.Object,
                                               e AS Infragistics.Win.UltraWinToolbars.BeforeToolDropdownEventArgs):
           
        DEFINE VARIABLE cHandle AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hMenu   AS HANDLE NO-UNDO.
           
        ASSIGN cHandle = UNBOX (e:Tool:SharedProps:RootTool:Tag) 
               hMenu   = WIDGET-HANDLE (cHandle) .
           
        IF VALID-HANDLE (hMenu) AND 
            hMenu:TYPE = Consultingwerk.WidgetTypeEnum:SubMenu AND
            hMenu:SENSITIVE THEN DO:
                  
            APPLY "MENU-DROP":U TO hMenu .
       END.
                   
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Update Form properties with Properties from the WindowWidget                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID UpdateWindowAttributes ():
        
        DEFINE VARIABLE oControl AS System.Windows.Forms.Control NO-UNDO .
        
        DEFINE VARIABLE hWidget  AS HANDLE                       NO-UNDO.
        DEFINE VARIABLE hWindow  AS HANDLE                       NO-UNDO.
        
        ASSIGN hWindow = THIS-OBJECT:EmbeddedWindow .
                
        IF VALID-HANDLE (hWindow) THEN DO:
            ASSIGN THIS-OBJECT:Text    = hWindow:TITLE 

                   THIS-OBJECT:ToolbarsManager:Enabled = hWindow:SENSITIVE 
                   THIS-OBJECT:TabFolder:Enabled = hWindow:SENSITIVE
                   
/*                   THIS-OBJECT:Enabled = hWindow:SENSITIVE*/
                   .  

            /* Mike Fechner, Consultingwerk Ltd. 22.02.2012 - WinKit PoC
               Controls that overlay ABL Widgets shall be Enabled, when both the Window 
               and the associated Widget are enabled */
            {Consultingwerk/foreach.i "'System.Collections.Generic.KeyValuePair<System.String,System.Windows.Forms.Control>'" oPair in this-object:OverlayControls}

                ASSIGN oControl = oPair:Value .

                IF TYPE-OF (oControl, Consultingwerk.WindowIntegrationKit.Controls.IOverlayControl) THEN 
                    ASSIGN hWidget = CAST (oControl, Consultingwerk.WindowIntegrationKit.Controls.IOverlayControl):WidgetHandle .
                ELSE 
                    NEXT .  
            
                oControl:Enabled = hWindow:SENSITIVE AND hWidget:SENSITIVE .
            END.

        END.

    END METHOD.
         
    /*------------------------------------------------------------------------------
        Purpose: Returns the parent control for Controls placed in this Form      
        Notes:                                                                           
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY ClientArea AS System.Windows.Forms.Control NO-UNDO 
    GET:
        IF VALID-OBJECT (THIS-OBJECT:ClientArea) THEN 
           RETURN THIS-OBJECT:ClientArea .
        ELSE  
            RETURN THIS-OBJECT:EmbeddedWindowTabFolderFormDesigner_Fill_Panel .     
    END GET.
    PROTECTED SET (arg AS System.Windows.Forms.Control):
        THIS-OBJECT:ClientArea = arg . 
    END SET . 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Splitter's distance
        Notes:   Should be set to match the position of the original ABL tabfolder                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SplitterDistance AS INTEGER NO-UNDO 
    GET:
/*        RETURN splitContainer1:SplitterDistance .*/
    END GET.
    SET(INPUT arg AS INTEGER):
/*        ASSIGN splitContainer1:SplitterDistance = arg .*/
    END SET.
     
    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the UltraTabControl used in this Form
        Notes:                                                                              
    ------------------------------------------------------------------------------*/ 
    DEFINE PUBLIC PROPERTY TabFolder AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Controls the distance of the TabFolder from the Top      
        Notes:                                                                           
    ------------------------------------------------------------------------------*/            
    DEFINE PUBLIC PROPERTY TabFolderTop AS INTEGER NO-UNDO 
    GET:
        RETURN ultraTabControl1:TOP .
    END GET. 
    SET (arg AS INTEGER):

        DEFINE VARIABLE iAdd AS INTEGER NO-UNDO.
        
        ASSIGN iAdd = ultraTabControl1:TOP - arg  
          
        ultraTabControl1:Top = arg
        ultraTabControl1:Height = THIS-OBJECT:HEIGHT - arg .  
        
        ultraTabControl1:WIDTH = THIS-OBJECT:Width . 
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Controls visibility of the TabFolder      
        Notes:                                                                           
    ------------------------------------------------------------------------------*/            
    DEFINE PUBLIC PROPERTY TabFolderVisible AS LOGICAL NO-UNDO 
    GET:
        RETURN THIS-OBJECT:ultraTabControl1:Visible . 
    END GET . 
    SET (arg AS LOGICAL):
        THIS-OBJECT:ultraTabControl1:Visible = arg . 
    END.  

    /*------------------------------------------------------------------------------
        Purpose: Gets and Sets the Col offset used when positioning the 
                 WindowContainer control in this .NET Form.      
        Notes:   Needs to be set before EmbedWindow is invoked                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY WindowContainerColOffset AS INTEGER NO-UNDO 
    GET():
        RETURN deltaCol . 
    END GET.
    SET(INPUT arg AS INTEGER):
        ASSIGN deltaCol = arg .
        
        RepositionWindowContainer () .
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and Sets the Row offset used when positioning the 
                 WindowContainer control in this .NET Form.      
        Notes:   Needs to be set before EmbedWindow is invoked                                                                        
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY WindowContainerRowOffset AS INTEGER NO-UNDO 
    GET():
        RETURN deltaRow .
    END GET.
    SET(INPUT arg AS INTEGER):
        DEFINE VARIABLE iDiff    AS INTEGER NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER NO-UNDO .
        DEFINE VARIABLE oControl AS System.Windows.Forms.Control NO-UNDO.
        
        ASSIGN iDiff = arg - deltaRow .
    
        IF iDiff <> 0 THEN DO:
            /* Mike Fechner, Consultingwerk Ltd. 09.07.2010
               Move up all rendered browsers by the diff */ 
    
            DO i = 0 TO THIS-OBJECT:EmbeddedWindowTabFolderFormDesigner_Fill_Panel:Controls:Count - 1:

                ASSIGN oControl = THIS-OBJECT:EmbeddedWindowTabFolderFormDesigner_Fill_Panel:Controls [i] .
    
                IF TYPE-OF (oControl, Consultingwerk.WindowIntegrationKit.Controls.RenderedBrowseControl) THEN DO:
                   oControl:Top = oControl:Top - iDiff .
                   
                   /* Mike Fechner, Consultingwerk Ltd. 09.07.2010
                      Possibly reposition */
                   IF CAST (oControl, Consultingwerk.WindowIntegrationKit.Controls.RenderedBrowseControl):AnchoredBottom THEN 
                       CAST (oControl, Consultingwerk.WindowIntegrationKit.Controls.RenderedBrowseControl):AnchorBottom () . 
                END.
            END.
        END .
        
        ASSIGN deltaRow = arg .
        
        RepositionWindowContainer () . 
    END SET.
        
END CLASS.
