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
    File        : ProcedureEditorForm
    Purpose     : Uses the WinKit to embed the ADE Procedure Editor into 
                  a .NET Form
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Dec 18 22:28:25 CET 2009
    Notes       : This class is only accessible on Windows
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (WinKit) NE 0 &THEN
USING Consultingwerk.Studio.ProcedureEditor.*     FROM PROPATH . 
USING Consultingwerk.WindowIntegrationKit.Forms.* FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .
USING Progress.Windows.*                          FROM ASSEMBLY .
USING System.Drawing.*                            FROM ASSEMBLY .
USING System.Windows.Forms.*                      FROM ASSEMBLY . 
&ENDIF

CLASS Consultingwerk.Studio.ProcedureEditor.ProcedureEditorForm 
&IF DEFINED (WinKit) NE 0 &THEN
    INHERITS ProcedureEditorBaseForm
    IMPLEMENTS IEmbeddedWindowForm
&ENDIF        
    : 
    
&IF DEFINED (WinKit) NE 0 &THEN
    DEFINE PRIVATE VARIABLE m_ProcedureEditorForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ProcedureEditorForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ProcedureEditorForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ProcedureEditorForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
   
    DEFINE VARIABLE WinContainer AS Progress.Windows.WindowContainer NO-UNDO . 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the main client are of the IEmbeddedWindowForm 
        Notes:   Read-Only                                                                         
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClientArea AS System.Windows.Forms.Control NO-UNDO 
    GET:
        RETURN THIS-OBJECT .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns logical value indicating if the current Editor requires 
                 to be saved
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY IsModified AS LOGICAL NO-UNDO 
	GET():
        DEFINE VARIABLE lModified AS LOGICAL   NO-UNDO .
        DEFINE VARIABLE hEditor   AS HANDLE    NO-UNDO .
        DEFINE VARIABLE hWindow   AS HANDLE    NO-UNDO .
        
        ASSIGN hWindow = THIS-OBJECT:EmbeddedWindow .
        
        IF NOT VALID-HANDLE (hWindow) THEN  
            RETURN FALSE . 
        
        RUN adecomm/_pwgeteh.p (hWindow , OUTPUT hEditor).        
		
		IF NOT VALID-HANDLE (hEditor) THEN 
		    RETURN FALSE .
	
		IF CAN-QUERY (hEditor, "MODIFIED":U) AND
		   CAN-QUERY (hEditor, "EMPTY":U) AND
           CAN-QUERY (hEditor, "NAME":U) THEN .
        ELSE 
            RETURN FALSE .
		
        RUN adecomm/_pwfmod.p (hEditor, OUTPUT lModified).
        
        RETURN lModified . 		
	END GET.
	 
    /*------------------------------------------------------------------------------
        Purpose: Returns logical value indicating if the current Editor is read-only
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY IsReadOnly AS LOGICAL NO-UNDO 
	GET:
        DEFINE VARIABLE lReadOnly AS LOGICAL   NO-UNDO .
        DEFINE VARIABLE hEditor   AS HANDLE    NO-UNDO .
        DEFINE VARIABLE hWindow   AS HANDLE    NO-UNDO .
        
        ASSIGN hWindow = THIS-OBJECT:EmbeddedWindow .
        
        IF NOT VALID-HANDLE (hWindow) THEN  
            RETURN FALSE . 
        
        RUN adecomm/_pwgeteh.p (hWindow , OUTPUT hEditor).        
        
        IF NOT VALID-HANDLE (hEditor) THEN 
            RETURN FALSE .
    
        IF CAN-QUERY (hEditor, "READ-ONLY":U) THEN 
            RETURN hEditor:READ-ONLY .
        ELSE 
            RETURN FALSE .
        
	END GET . 

    /*------------------------------------------------------------------------------
        Purpose: Gets and Sets the Col offset used when positioning the 
                 WindowContainer control in this .NET Form.      
        Notes:   Not supported by this class                                                                           
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY WindowContainerColOffset AS INTEGER NO-UNDO 
    GET:
        UNDO, THROW NEW Progress.Lang.AppError("Property getter for WindowContainerColOffset has not been implemented"{&TRAN}).
    END GET.
    SET (arg AS INTEGER):
        UNDO, THROW NEW Progress.Lang.AppError("Property setter for WindowContainerColOffset has not been implemented"{&TRAN}).        
    END SET.
     
    /*------------------------------------------------------------------------------
        Purpose: Gets and Sets the default Row offset used when positioning the 
                 WindowContainer control in this .NET Form.      
        Notes:   Not supported by this class                                                                           
    ------------------------------------------------------------------------------*/            
    DEFINE PUBLIC PROPERTY WindowContainerRowOffset AS INTEGER NO-UNDO 
    GET():
        UNDO, THROW NEW Progress.Lang.AppError("Property getter for WindowContainerRowOffset has not been implemented"{&TRAN}).
    END GET.
    SET(INPUT arg AS INTEGER):
        UNDO, THROW NEW Progress.Lang.AppError("Property setter for WindowContainerRowOffset has not been implemented"{&TRAN}).        
    END SET.
          
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ProcedureEditorForm class                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
     CONSTRUCTOR PUBLIC ProcedureEditorForm ():                
        SUPER().
        InitializeComponent().
        
        THIS-OBJECT:FormClosing:Subscribe(FormClosingHandler) .
        
        THIS-OBJECT:ToolbarsManager = ultraToolbarsManager1 .
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Checks the Syntax of the current source code 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID CheckSyntax ():
		
		RUN adecommx/_pwrun.p (THIS-OBJECT:EmbeddedWindow,
		                       "CHECK-SYNTAX":U) .

	END METHOD .

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
        Purpose: Creates a new instance of the Progress.Windows.WindowContainer 
                 instance and embeds the given window widget in that instance - 
                 but does not parent the WindowContainer instance in the Form 
                 (THIS-OBJECT). This is delayed till later.    
        Notes:   Required due to incombatibily issues with the Infragistics 
                 UltraTabbedMdiManager.
        @param phWindow The handle of the WINDOW widget to embed                                                                         
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID EmbedWindow (phWindow AS HANDLE):
        
        THIS-OBJECT:EmbeddedWindow = phWindow . 
        
        WinContainer = NEW Progress.Windows.WindowContainer( ).
        
        THIS-OBJECT:Size = NEW Size (phWindow:WIDTH-PIXELS, phWindow:HEIGHT-PIXELS).
        WinContainer:Size = NEW Size(phWindow:WIDTH-PIXELS, phWindow:HEIGHT-PIXELS).
        
        WinContainer:EmbeddedWindow = phWindow.
        WinContainer:Dock = System.Windows.Forms.DockStyle:Fill .

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
        
        WinContainer:Parent = THIS-OBJECT.
        WinContainer:Size = NEW System.Drawing.Size (THIS-OBJECT:Width,
                                                     THIS-OBJECT:Height).
        
        ASSIGN THIS-OBJECT:EmbeddedWindow:WIDTH-PIXELS  = WinContainer:WIDTH 
               THIS-OBJECT:EmbeddedWindow:HEIGHT-PIXELS = WinContainer:HEIGHT .

        THIS-OBJECT:UpdateWindowAttributes () .
        
        /* Mike Fechner, Consultingwerk Ltd. 18.12.2009
           APPLY won't work with THIS-OBJECT:EmbeddedWindow  */
        
        ASSIGN hWindow = THIS-OBJECT:EmbeddedWindow .

        APPLY "window-resized":U TO hWindow.            

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler for the FormClosing event of the Form. When a window
                 is embedded, the FormClosing event will be cancelled and the 
                 WINDOW-CLOSE event to the window will be applied.                                                                        
        Notes:   The window is responsible for deleting the Form.  
                 {src/adecommx/closewindow.i} can be used in the ON CLOSE OF THIS-
                 PROCEDURE trigger for that purpose
        @param sender The System.Object that raised the event
        @param e The FormClosingEventArgs with the data for this event                                                                       
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID FormClosingHandler (sender AS System.Object, 
                                           e AS FormClosingEventArgs):

        DEFINE VARIABLE hWindow AS HANDLE NO-UNDO.
        
        IF Progress.Util.EnumHelper:AreEqual(e:CloseReason,
                                             CloseReason:UserClosing) THEN 
            IF VALID-HANDLE(THIS-OBJECT:EmbeddedWindow) THEN DO:
                ASSIGN hWindow  = THIS-OBJECT:EmbeddedWindow 
                       e:Cancel = TRUE .
                
                APPLY "window-close":U TO hWindow .         
            END.        

            IF NOT VALID-HANDLE (hWindow) THEN 
                e:Cancel = FALSE . 

        RETURN.

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeComponent ():
        
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("ribbon1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Find":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("FindNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("FindPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup2":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Replace":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup3":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GotoLine":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Find":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("FindNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("FindPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Replace":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("GotoLine":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Studio.ProcedureEditor.ProcedureEditorForm":U).
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Progress.Windows.Form":U).
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:ApplicationMenuButtonImage = CAST(resources:GetObject("ultraToolbarsManager1.Ribbon.ApplicationMenuButtonImage":U), System.Drawing.Image).
        ribbonTab1:Caption = "Editor":U.
        ribbonGroup1:Caption = "Text Search":U.
        buttonTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool3:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool3:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool4:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar0[1] = buttonTool1.
        arrayvar0[2] = buttonTool3.
        arrayvar0[3] = buttonTool4.
        ribbonGroup1:Tools:AddRange(arrayvar0).
        ribbonGroup2:Caption = "Replace":U.
        buttonTool7:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar1[1] = buttonTool7.
        ribbonGroup2:Tools:AddRange(arrayvar1).
        ribbonGroup3:Caption = "Jump":U.
        buttonTool9:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar2[1] = buttonTool9.
        ribbonGroup3:Tools:AddRange(arrayvar2).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 3 NO-UNDO.
        arrayvar3[1] = ribbonGroup1.
        arrayvar3[2] = ribbonGroup2.
        arrayvar3[3] = ribbonGroup3.
        ribbonTab1:Groups:AddRange(arrayvar3).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar4[1] = ribbonTab1.
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar4).
        THIS-OBJECT:ultraToolbarsManager1:Ribbon:Visible = TRUE.
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        buttonTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance1.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
        buttonTool2:SharedPropsInternal:Caption = "Find":U.
        buttonTool2:SharedPropsInternal:Shortcut = System.Windows.Forms.Shortcut:CtrlF.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        buttonTool5:SharedPropsInternal:AppearancesLarge:Appearance = appearance3.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        buttonTool5:SharedPropsInternal:AppearancesSmall:Appearance = appearance4.
        buttonTool5:SharedPropsInternal:Caption = "Find Next":U.
        buttonTool5:SharedPropsInternal:Shortcut = System.Windows.Forms.Shortcut:F9.
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesLarge:Appearance = appearance5.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesSmall:Appearance = appearance6.
        buttonTool6:SharedPropsInternal:Caption = "Find Previous":U.
        buttonTool6:SharedPropsInternal:Shortcut = System.Windows.Forms.Shortcut:ShiftF9.
        appearance7:Image = CAST(resources:GetObject("appearance7.Image":U), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesLarge:Appearance = appearance7.
        appearance8:Image = CAST(resources:GetObject("appearance8.Image":U), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesSmall:Appearance = appearance8.
        buttonTool8:SharedPropsInternal:Caption = "Replace":U.
        buttonTool8:SharedPropsInternal:Enabled = FALSE.
        buttonTool8:SharedPropsInternal:Shortcut = System.Windows.Forms.Shortcut:CtrlR.
        appearance9:Image = CAST(resources:GetObject("appearance9.Image":U), System.Object).
        buttonTool10:SharedPropsInternal:AppearancesLarge:Appearance = appearance9.
        appearance10:Image = CAST(resources:GetObject("appearance10.Image":U), System.Object).
        buttonTool10:SharedPropsInternal:AppearancesSmall:Appearance = appearance10.
        buttonTool10:SharedPropsInternal:Caption = "Goto Line":U.
        buttonTool10:SharedPropsInternal:Shortcut = System.Windows.Forms.Shortcut:CtrlG.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 5 NO-UNDO.
        arrayvar5[1] = buttonTool2.
        arrayvar5[2] = buttonTool5.
        arrayvar5[3] = buttonTool6.
        arrayvar5[4] = buttonTool8.
        arrayvar5[5] = buttonTool10.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar5).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        /*  */
        /* m_ProcedureEditorForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 162).
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Left:Name = "m_ProcedureEditorForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(8, 247).
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ProcedureEditorForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(639, 162).
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Right:Name = "m_ProcedureEditorForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(8, 247).
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ProcedureEditorForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Top:Name = "m_ProcedureEditorForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(647, 162).
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_ProcedureEditorForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 409).
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Bottom:Name = "m_ProcedureEditorForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(647, 8).
        THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* ProcedureEditorForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(647, 417).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ProcedureEditorForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "ProcedureEditorForm":U.
        THIS-OBJECT:Text = "ProcedureEditorForm":U.
        THIS-OBJECT:WindowPositionRegistryKey = "ProcedureEditorForm":U.
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Inserts text in the Procedure Editor at the given line 
        Notes:   
        @param pcText The text to insert
        @param piLine The line number to insert the text 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID InsertText (pcText AS CHARACTER,
	                               piLine AS INTEGER):
		
        DEFINE VARIABLE hWindow AS HANDLE NO-UNDO .
        DEFINE VARIABLE hEditor AS HANDLE NO-UNDO .
        
        ASSIGN hWindow = THIS-OBJECT:EmbeddedWindow .
        
        RUN adecomm/_pwgeteh.p (hWindow , OUTPUT hEditor).        

        THIS-OBJECT:MoveCursor (piLine).

        hEditor:INSERT-STRING (pcText) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Moves editor cursor to a specified line number, column 1.
        Notes:   If entered line is greater than number of lines in buffer, then
                 move to EOF.
        @param piLine The line number to move the cursor to
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID MoveCursor (piLine AS INTEGER):
		
        DEFINE VARIABLE hWindow    AS HANDLE  NO-UNDO .
        DEFINE VARIABLE hEditor    AS HANDLE  NO-UNDO .

        DEFINE VARIABLE lMoveToEof AS LOGICAL NO-UNDO .

        ASSIGN hWindow = THIS-OBJECT:EmbeddedWindow .
        
        RUN adecomm/_pwgeteh.p (hWindow , OUTPUT hEditor).        

        IF hEditor:EMPTY THEN RETURN.
        
        /* If entered line is greater than number of lines in buffer, then
           move to EOF. */
        IF piLine > hEditor:NUM-LINES
            THEN ASSIGN lMoveToEof = hEditor:MOVE-TO-EOF().
        ELSE DO:
            /* If entered line 0, goto line 1. */
            IF ( piLine < 1 ) THEN ASSIGN piLine = 1.
    
            ASSIGN hEditor:CURSOR-LINE = piLine.
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the state of all overlay buttons in the IEmbeddedWindowForm                                                                      
        Notes:   Actually this method has no functionally, but is required for the 
                 implementation of the Interface IEmbeddedWindowForm                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RefreshButtons ():  
        
        /* NOOP */

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves the Editor contents
        Notes:   
        @return The name of the file  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER Save ():
        
        DEFINE VARIABLE hEditor   AS HANDLE    NO-UNDO .
        DEFINE VARIABLE hWindow   AS HANDLE    NO-UNDO .
        
        ASSIGN hWindow = THIS-OBJECT:EmbeddedWindow .
        
        RUN adecomm/_pwgeteh.p (hWindow , OUTPUT hEditor).        

        RUN adecomm/_pwsave.p (hEditor) .

        IF RETURN-VALUE = "ERROR":U THEN 
            RETURN ? . 

        RETURN hEditor:NAME .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Saves the Editor contents to a temporary file 
        Notes:   Useful for parsing with Proparse
        @return The name of the temporary file  
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER SaveToTemporaryFile ():
		
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO .
        DEFINE VARIABLE lModified AS LOGICAL   NO-UNDO .
        DEFINE VARIABLE hEditor   AS HANDLE    NO-UNDO .
        DEFINE VARIABLE hWindow   AS HANDLE    NO-UNDO .
        
        ASSIGN hWindow = THIS-OBJECT:EmbeddedWindow .
        
        RUN adecomm/_pwgeteh.p (hWindow , OUTPUT hEditor).        

        ASSIGN cFileName = SESSION:TEMP-DIRECTORY + Consultingwerk.Util.FileHelper:ShortFileName (hEditor:NAME) . 
               lModified = hEditor:MODIFIED . 

        hEditor:SAVE-FILE (cFileName) .

        ASSIGN hEditor:MODIFIED = lModified . 

		RETURN cFileName .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the ultraToolbarsManager1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraToolbarsManager1_ToolClick (sender AS System.Object, 
                                                         e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):
        
        CASE e:Tool:Key:
            WHEN "Find":U THEN
                RUN adecommx/_pwsrch.p ("FIND":U, THIS-OBJECT:EmbeddedWindow).
            WHEN "FindNext":U THEN
                RUN adecommx/_pwsrch.p ("FIND-NEXT":U, THIS-OBJECT:EmbeddedWindow).
            WHEN "FindPrev":U THEN
                RUN adecommx/_pwsrch.p ("FIND-PREV":U, THIS-OBJECT:EmbeddedWindow).
            WHEN "Replace":U THEN
                RUN adecommx/_pwsrch.p ("REPLACE":U, THIS-OBJECT:EmbeddedWindow).
            WHEN "GotoLine":U THEN  
                RUN adecommx/_pwsrch.p ("GOTO-LINE":U, THIS-OBJECT:EmbeddedWindow).
        END CASE.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Update Form properties with Properties from the WindowWidget                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID UpdateWindowAttributes ():
        
        IF VALID-HANDLE (THIS-OBJECT:EmbeddedWindow) THEN 
          THIS-OBJECT:Text = ENTRY (NUM-ENTRIES (REPLACE (THIS-OBJECT:EmbeddedWindow:Title, "/":U, "~\":U),
                                                 "~\":U),
                                    REPLACE (THIS-OBJECT:EmbeddedWindow:Title, "/":U, "~\":U),
                                    "~\":U) .   

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ProcedureEditorForm class                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DESTRUCTOR PUBLIC ProcedureEditorForm ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.
&ENDIF
END CLASS.
