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
    File        : DynamicLookupDesignerForm
    Purpose     : Designer for Dynamic Lookup Controls
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Oct 25 11:48:25 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                                   FROM PROPATH . 
USING Consultingwerk.Framework.RepositoryServices.*                FROM PROPATH . 
USING Consultingwerk.SmartComponents.Base.*                        FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.SmartWindowForm          FROM PROPATH .
USING Consultingwerk.SmartComponents.Framework.*                   FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*              FROM PROPATH .
USING Consultingwerk.SmartComponents.Tools.DynamicLookupDesigner.* FROM PROPATH .
USING Consultingwerk.Util.*                                        FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*                           FROM ASSEMBLY .
USING Progress.Lang.*                                              FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartComponents.Tools.DynamicLookupDesigner.DynamicLookupDesignerForm 
    INHERITS SmartWindowForm: 
    
    DEFINE PRIVATE VARIABLE btnOpenExisting AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE cbObjectType AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE gbLookup AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE gbLookupProperties AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1 AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.
    DEFINE PRIVATE VARIABLE DynamicLookupDesignerForm_Fill_Panel AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbNewObjectName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbObjectName AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE tbModule AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE tbLookupView AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbLookupTable AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbLookupBusinessEntity AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    DEFINE VARIABLE oRepository    AS IRepositoryService        NO-UNDO . 
    DEFINE VARIABLE oDesignLookup  AS SmartBusinessEntityLookup NO-UNDO .  
    DEFINE VARIABLE hDesignDataset AS HANDLE                    NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel6 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: The current state of the Lookup Designer                                                                         
        Notes:   Default   - waiting for either new or open 
                 New       - creating a new lookup
                 Opening   - waiting for choosing a lookup 
                 Editing   - editing an existing lookup
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupDesignerState AS LookupDesignerStateEnum NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DynamicLookupDesignerForm                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DynamicLookupDesignerForm ():
        
        SUPER().
        
        THIS-OBJECT:LookupDesignerState = LookupDesignerStateEnum:Default . 
        
        InitializeComponent().
        
        THIS-OBJECT:SetButtons () .
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnOpenExisting                                                                        
        Notes:                                                           
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event             
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnOpenExisting_Click (sender AS System.Object, 
                                               e AS System.EventArgs):
        
        DEFINE VARIABLE oSerializer AS ISmartBusinessEntityLookupSerializer NO-UNDO . 
        DEFINE VARIABLE hBuffer     AS HANDLE                               NO-UNDO.
        
        oSerializer = { Consultingwerk/get-service.i Consultingwerk.SmartComponents.Framework.ISmartBusinessEntityLookupSerializer
                                                     "NEW SmartBusinessEntityLookupSerializer ()"} . 

        oRepository:GetObjectByName (THIS-OBJECT:tbObjectName:Text,
                                     OUTPUT DATASET-HANDLE hDesignDataset) .
        
        THIS-OBJECT:CreateDesignLookup () .        
        
        ASSIGN hBuffer = hDesignDataset:GET-BUFFER-HANDLE (1) .
        
        oSerializer:Deserialize (hBuffer, oDesignLookup) . 
        
        THIS-OBJECT:DisplayLookupProperties () .
        
        THIS-OBJECT:tbDescription:Text = hBuffer::ObjectDescription .  
        THIS-OBJECT:tbModule:Text = hBuffer::ProductModuleCode .  
        
        THIS-OBJECT:LookupDesignerState = LookupDesignerStateEnum:Editing .

        THIS-OBJECT:SetButtons () .

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Cancels the current editing                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID Cancel ():

        IF VALID-HANDLE (hDesignDataset) THEN 
            DELETE OBJECT hDesignDataset .         

        ASSIGN hDesignDataset = ? . 

        IF VALID-OBJECT (oDesignLookup) THEN 
            DELETE OBJECT oDesignLookup .

        THIS-OBJECT:LookupDesignerState = LookupDesignerStateEnum:Default .

        THIS-OBJECT:SetButtons () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of the cbObjectType                                                                        
        Notes:                                                           
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event             
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID cbObjectType_TextChanged (sender AS System.Object, 
                                                 e AS System.EventArgs):
        
        THIS-OBJECT:SetLookupFilter () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a new design time lookup instance                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateDesignLookup ():
        
        hDesignDataset:GET-BUFFER-HANDLE (1):FIND-FIRST () .
        
        IF VALID-OBJECT (oDesignLookup) THEN 
            DELETE OBJECT oDesignLookup . 
            
        oDesignLookup = DYNAMIC-NEW (hDesignDataset:GET-BUFFER-HANDLE (1)::RenderingClassName) () .            

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Displays the properties of the current lookup instance                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DisplayLookupProperties ():
        
        THIS-OBJECT:tbLookupBusinessEntity:Text  = oDesignLookup:LookupEntityName .
        THIS-OBJECT:tbLookupTable:Text           = oDesignLookup:LookupEntityTable .
        THIS-OBJECT:tbLookupView:Text            = oDesignLookup:LookupEntityView . 

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
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Close").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("btnNew").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("btnOpen").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("BtnSave").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("btnCancel").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("start").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("action").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("btnNew").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("btnOpen").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("BtnSave").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("btnCancel").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("btnNew").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("btnOpen").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("BtnSave").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("btnCancel").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Close").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.SmartComponents.Tools.DynamicLookupDesigner.DynamicLookupDesignerForm").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE editorButton3 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton3 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE editorButton4 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton4 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:DynamicLookupDesignerForm_Fill_Panel = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:gbLookup = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:cbObjectType = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:tbObjectName = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:btnOpenExisting = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:tbModule = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbNewObjectName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:gbLookupProperties = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:ultraLabel6 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:tbLookupView = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbLookupTable = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbLookupBusinessEntity = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:DynamicLookupDesignerForm_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:gbLookup, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:gbLookup:SuspendLayout().
        CAST(THIS-OBJECT:cbObjectType, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbObjectName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbModule, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbNewObjectName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:gbLookupProperties, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:gbLookupProperties:SuspendLayout().
        CAST(THIS-OBJECT:tbDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbLookupView, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbLookupTable, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbLookupBusinessEntity, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm").
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar0[1] = buttonTool13.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:FooterToolbar:NonInheritedTools:AddRange(arrayvar0).
        buttonTool3:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar1[1] = buttonTool1.
        arrayvar1[2] = buttonTool2.
        arrayvar1[3] = buttonTool3.
        arrayvar1[4] = buttonTool12.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar1).
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenuButtonImage = CAST(resources:GetObject("smartToolbarController1.Ribbon.ApplicationMenuButtonImage"), System.Drawing.Image).
        ribbonTab1:Caption = "Start".
        ribbonGroup1:Caption = "Action".
        buttonTool9:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool10:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool11:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool4:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar2[1] = buttonTool9.
        arrayvar2[2] = buttonTool10.
        arrayvar2[3] = buttonTool11.
        arrayvar2[4] = buttonTool4.
        ribbonGroup1:Tools:AddRange(arrayvar2).
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 1 NO-UNDO.
        arrayvar3[1] = ribbonGroup1.
        ribbonTab1:Groups:AddRange(arrayvar3).
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar4[1] = ribbonTab1.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar4).
        THIS-OBJECT:smartToolbarController1:Ribbon:Visible = TRUE.
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        appearance9:Image = CAST(resources:GetObject("appearance9.Image"), System.Object).
        buttonTool5:SharedPropsInternal:AppearancesLarge:Appearance = appearance9.
        appearance10:Image = CAST(resources:GetObject("appearance10.Image"), System.Object).
        buttonTool5:SharedPropsInternal:AppearancesSmall:Appearance = appearance10.
        buttonTool5:SharedPropsInternal:Caption = "&New".
        appearance11:Image = CAST(resources:GetObject("appearance11.Image"), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesLarge:Appearance = appearance11.
        appearance12:Image = CAST(resources:GetObject("appearance12.Image"), System.Object).
        buttonTool6:SharedPropsInternal:AppearancesSmall:Appearance = appearance12.
        buttonTool6:SharedPropsInternal:Caption = "&Open".
        appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
        buttonTool7:SharedPropsInternal:AppearancesLarge:Appearance = appearance13.
        appearance14:Image = CAST(resources:GetObject("appearance14.Image"), System.Object).
        buttonTool7:SharedPropsInternal:AppearancesSmall:Appearance = appearance14.
        buttonTool7:SharedPropsInternal:Caption = "&Save".
        appearance15:Image = CAST(resources:GetObject("appearance15.Image"), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesLarge:Appearance = appearance15.
        appearance16:Image = CAST(resources:GetObject("appearance16.Image"), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesSmall:Appearance = appearance16.
        buttonTool8:SharedPropsInternal:Caption = "&Cancel".
        buttonTool14:SharedPropsInternal:Caption = "&Close".
        buttonTool14:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:TextOnlyAlways.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 5 NO-UNDO.
        arrayvar5[1] = buttonTool5.
        arrayvar5[2] = buttonTool6.
        arrayvar5[3] = buttonTool7.
        arrayvar5[4] = buttonTool8.
        arrayvar5[5] = buttonTool14.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar5).
        THIS-OBJECT:smartToolbarController1:ToolClick:Subscribe(THIS-OBJECT:smartToolbarController1_ToolClick).
        /*  */
        /* DynamicLookupDesignerForm_Fill_Panel */
        /*  */
        THIS-OBJECT:DynamicLookupDesignerForm_Fill_Panel:Controls:Add(THIS-OBJECT:gbLookup).
        THIS-OBJECT:DynamicLookupDesignerForm_Fill_Panel:Controls:Add(THIS-OBJECT:gbLookupProperties).
        THIS-OBJECT:DynamicLookupDesignerForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:DynamicLookupDesignerForm_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:DynamicLookupDesignerForm_Fill_Panel:Location = NEW System.Drawing.Point(9, 182).
        THIS-OBJECT:DynamicLookupDesignerForm_Fill_Panel:Name = "DynamicLookupDesignerForm_Fill_Panel".
        THIS-OBJECT:DynamicLookupDesignerForm_Fill_Panel:Size = NEW System.Drawing.Size(684, 339).
        THIS-OBJECT:DynamicLookupDesignerForm_Fill_Panel:TabIndex = 0.
        /*  */
        /* gbLookup */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:gbLookup:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:gbLookup:Controls:Add(THIS-OBJECT:cbObjectType).
        THIS-OBJECT:gbLookup:Controls:Add(THIS-OBJECT:tbObjectName).
        THIS-OBJECT:gbLookup:Controls:Add(THIS-OBJECT:btnOpenExisting).
        THIS-OBJECT:gbLookup:Controls:Add(THIS-OBJECT:tbModule).
        THIS-OBJECT:gbLookup:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:gbLookup:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:gbLookup:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:gbLookup:Controls:Add(THIS-OBJECT:tbNewObjectName).
        THIS-OBJECT:gbLookup:Location = NEW System.Drawing.Point(6, 6).
        THIS-OBJECT:gbLookup:Name = "gbLookup".
        THIS-OBJECT:gbLookup:Size = NEW System.Drawing.Size(672, 110).
        THIS-OBJECT:gbLookup:TabIndex = 0.
        THIS-OBJECT:gbLookup:Text = "Lookup Selection".
        /*  */
        /* cbObjectType */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbObjectType:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbObjectType:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:cbObjectType:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:cbObjectType:Location = NEW System.Drawing.Point(192, 19).
        THIS-OBJECT:cbObjectType:Name = "cbObjectType".
        THIS-OBJECT:cbObjectType:Size = NEW System.Drawing.Size(378, 24).
        THIS-OBJECT:cbObjectType:TabIndex = 1.
        THIS-OBJECT:cbObjectType:TextChanged:Subscribe(THIS-OBJECT:cbObjectType_TextChanged).
        /*  */
        /* tbObjectName */
        /*  */
        THIS-OBJECT:tbObjectName:AddLookupButton = FALSE.
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbObjectName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Key = "ShowLookupDialog".
        editorButton1:Text = "...".
        THIS-OBJECT:tbObjectName:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:tbObjectName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tbObjectName:Location = NEW System.Drawing.Point(192, 73).
        THIS-OBJECT:tbObjectName:LookupBrowserExcludeFields = "".
        THIS-OBJECT:tbObjectName:LookupBrowserFields = "object_filename,object_description,product_module_code,object_type_code".
        THIS-OBJECT:tbObjectName:LookupControls = "".
        THIS-OBJECT:tbObjectName:LookupDialogClassName = "".
        THIS-OBJECT:tbObjectName:LookupDialogFilterFields = "object_filename,product_module_code".
        THIS-OBJECT:tbObjectName:LookupDialogFilterOperators = "BEGINS,BEGINS".
        THIS-OBJECT:tbObjectName:LookupDialogQuerySort = "".
        THIS-OBJECT:tbObjectName:LookupDialogQueryString = "".
        THIS-OBJECT:tbObjectName:LookupDialogTitle = "Select Lookup Control".
        THIS-OBJECT:tbObjectName:LookupEntityName = "Consultingwerk.Framework.RepositoryServices.Dynamics.Entities.SmartObjectBusinessEntity".
        THIS-OBJECT:tbObjectName:LookupEntityTable = "eSmartObject".
        THIS-OBJECT:tbObjectName:LookupEntityView = "".
        THIS-OBJECT:tbObjectName:LookupFields = "".
        THIS-OBJECT:tbObjectName:LookupKeyField = "object_filename".
        THIS-OBJECT:tbObjectName:LookupKeyValue = "".
        THIS-OBJECT:tbObjectName:LookupKeyValueColumn = "".
        THIS-OBJECT:tbObjectName:LookupQuerySort = "".
        THIS-OBJECT:tbObjectName:LookupQueryString = "FOR EACH eSmartObject WHERE eSmartObject.object_filename BEGINS ~"&1~"".
        THIS-OBJECT:tbObjectName:LookupTimerDelay = 750.
        THIS-OBJECT:tbObjectName:Name = "tbObjectName".
        THIS-OBJECT:tbObjectName:Size = NEW System.Drawing.Size(378, 24).
        THIS-OBJECT:tbObjectName:TabIndex = 5.
        /*  */
        /* btnOpenExisting */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:btnOpenExisting:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image"), System.Object).
        appearance1:ImageHAlign = Infragistics.Win.HAlign:Center.
        appearance1:ImageVAlign = Infragistics.Win.VAlign:Top.
        appearance1:TextHAlignAsString = "Center".
        appearance1:TextVAlignAsString = "Bottom".
        THIS-OBJECT:btnOpenExisting:Appearance = appearance1.
        THIS-OBJECT:btnOpenExisting:ImageSize = NEW System.Drawing.Size(48, 48).
        THIS-OBJECT:btnOpenExisting:Location = NEW System.Drawing.Point(583, 19).
        THIS-OBJECT:btnOpenExisting:Name = "btnOpenExisting".
        THIS-OBJECT:btnOpenExisting:Size = NEW System.Drawing.Size(75, 75).
        THIS-OBJECT:btnOpenExisting:TabIndex = 6.
        THIS-OBJECT:btnOpenExisting:Text = "Open".
        THIS-OBJECT:btnOpenExisting:Click:Subscribe(THIS-OBJECT:btnOpenExisting_Click).
        /*  */
        /* tbModule */
        /*  */
        THIS-OBJECT:tbModule:AddLookupButton = FALSE.
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbModule:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton2:Key = "ShowLookupDialog".
        editorButton2:Text = "...".
        THIS-OBJECT:tbModule:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:tbModule:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tbModule:Location = NEW System.Drawing.Point(192, 46).
        THIS-OBJECT:tbModule:LookupBrowserExcludeFields = "".
        THIS-OBJECT:tbModule:LookupBrowserFields = "product_module_code,product_module_description,product_code,product_description".
        THIS-OBJECT:tbModule:LookupControls = "".
        THIS-OBJECT:tbModule:LookupDialogClassName = "".
        THIS-OBJECT:tbModule:LookupDialogFilterFields = "product_code,product_module_code".
        THIS-OBJECT:tbModule:LookupDialogFilterOperators = "BEGINS,BEGINS".
        THIS-OBJECT:tbModule:LookupDialogQuerySort = "".
        THIS-OBJECT:tbModule:LookupDialogQueryString = "".
        THIS-OBJECT:tbModule:LookupDialogTitle = "Lookup".
        THIS-OBJECT:tbModule:LookupEntityName = "Consultingwerk.Framework.RepositoryServices.Dynamics.ProductModuleBusinessEntity".
        THIS-OBJECT:tbModule:LookupEntityTable = "egsc_product_module".
        THIS-OBJECT:tbModule:LookupEntityView = "".
        THIS-OBJECT:tbModule:LookupFields = "".
        THIS-OBJECT:tbModule:LookupKeyField = "product_module_code".
        THIS-OBJECT:tbModule:LookupKeyValue = "".
        THIS-OBJECT:tbModule:LookupKeyValueColumn = "".
        THIS-OBJECT:tbModule:LookupQuerySort = "".
        THIS-OBJECT:tbModule:LookupQueryString = "for each egsc_product_module where egsc_product_module.product_module_code BEGINS ~"&1~"".
        THIS-OBJECT:tbModule:LookupTimerDelay = 750.
        THIS-OBJECT:tbModule:Name = "tbModule".
        THIS-OBJECT:tbModule:Size = NEW System.Drawing.Size(378, 24).
        THIS-OBJECT:tbModule:TabIndex = 3.
        THIS-OBJECT:tbModule:TextChanged:Subscribe(THIS-OBJECT:tbModule_TextChanged).
        /*  */
        /* ultraLabel3 */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel3:Appearance = appearance2.
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(8, 50).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3".
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel3:TabIndex = 2.
        THIS-OBJECT:ultraLabel3:Text = "Module:".
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel1:Appearance = appearance3.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(8, 23).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1".
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 0.
        THIS-OBJECT:ultraLabel1:Text = "Object Type:".
        /*  */
        /* ultraLabel2 */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel2:Appearance = appearance4.
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(8, 77).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2".
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 4.
        THIS-OBJECT:ultraLabel2:Text = "Object Name:".
        /*  */
        /* tbNewObjectName */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbNewObjectName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbNewObjectName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tbNewObjectName:Location = NEW System.Drawing.Point(192, 73).
        THIS-OBJECT:tbNewObjectName:Name = "tbNewObjectName".
        THIS-OBJECT:tbNewObjectName:Size = NEW System.Drawing.Size(378, 24).
        THIS-OBJECT:tbNewObjectName:TabIndex = 7.
        /*  */
        /* gbLookupProperties */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:gbLookupProperties:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:gbLookupProperties:Controls:Add(THIS-OBJECT:ultraLabel6).
        THIS-OBJECT:gbLookupProperties:Controls:Add(THIS-OBJECT:tbDescription).
        THIS-OBJECT:gbLookupProperties:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:gbLookupProperties:Controls:Add(THIS-OBJECT:tbLookupView).
        THIS-OBJECT:gbLookupProperties:Controls:Add(THIS-OBJECT:tbLookupTable).
        THIS-OBJECT:gbLookupProperties:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:gbLookupProperties:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:gbLookupProperties:Controls:Add(THIS-OBJECT:tbLookupBusinessEntity).
        THIS-OBJECT:gbLookupProperties:Location = NEW System.Drawing.Point(6, 122).
        THIS-OBJECT:gbLookupProperties:Name = "gbLookupProperties".
        THIS-OBJECT:gbLookupProperties:Size = NEW System.Drawing.Size(672, 211).
        THIS-OBJECT:gbLookupProperties:TabIndex = 1.
        THIS-OBJECT:gbLookupProperties:Text = "Lookup Properties".
        /*  */
        /* ultraLabel6 */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel6:Appearance = appearance5.
        THIS-OBJECT:ultraLabel6:Location = NEW System.Drawing.Point(8, 25).
        THIS-OBJECT:ultraLabel6:Name = "ultraLabel6".
        THIS-OBJECT:ultraLabel6:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel6:TabIndex = 0.
        THIS-OBJECT:ultraLabel6:Text = "Description".
        /*  */
        /* tbDescription */
        /*  */
        THIS-OBJECT:tbDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tbDescription:Location = NEW System.Drawing.Point(192, 21).
        THIS-OBJECT:tbDescription:Name = "tbDescription".
        THIS-OBJECT:tbDescription:Size = NEW System.Drawing.Size(468, 24).
        THIS-OBJECT:tbDescription:TabIndex = 1.
        /*  */
        /* ultraButton1 */
        /*  */
        THIS-OBJECT:ultraButton1:Anchor = System.Windows.Forms.AnchorStyles:Top.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image"), System.Object).
        appearance6:ImageHAlign = Infragistics.Win.HAlign:Center.
        appearance6:ImageVAlign = Infragistics.Win.VAlign:Top.
        appearance6:TextHAlignAsString = "Center".
        appearance6:TextVAlignAsString = "Bottom".
        THIS-OBJECT:ultraButton1:Appearance = appearance6.
        THIS-OBJECT:ultraButton1:ImageSize = NEW System.Drawing.Size(96, 96).
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(259, 102).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1".
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(155, 119).
        THIS-OBJECT:ultraButton1:TabIndex = 7.
        THIS-OBJECT:ultraButton1:Text = "Lookup Designer".
        THIS-OBJECT:ultraButton1:Click:Subscribe(THIS-OBJECT:ultraButton1_Click).
        /*  */
        /* tbLookupView */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbLookupView:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbLookupView:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tbLookupView:Location = NEW System.Drawing.Point(376, 75).
        THIS-OBJECT:tbLookupView:Name = "tbLookupView".
        THIS-OBJECT:tbLookupView:ReadOnly = TRUE.
        THIS-OBJECT:tbLookupView:Size = NEW System.Drawing.Size(282, 24).
        THIS-OBJECT:tbLookupView:TabIndex = 6.
        /*  */
        /* tbLookupTable */
        /*  */
        THIS-OBJECT:tbLookupTable:ButtonsRight:Add(editorButton3).
        THIS-OBJECT:tbLookupTable:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tbLookupTable:Location = NEW System.Drawing.Point(192, 75).
        THIS-OBJECT:tbLookupTable:Name = "tbLookupTable".
        THIS-OBJECT:tbLookupTable:ReadOnly = TRUE.
        THIS-OBJECT:tbLookupTable:Size = NEW System.Drawing.Size(178, 24).
        THIS-OBJECT:tbLookupTable:TabIndex = 5.
        THIS-OBJECT:tbLookupTable:EditorButtonClick:Subscribe(THIS-OBJECT:tbLookupTable_EditorButtonClick).
        THIS-OBJECT:tbLookupTable:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* ultraLabel5 */
        /*  */
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel5:Appearance = appearance7.
        THIS-OBJECT:ultraLabel5:Location = NEW System.Drawing.Point(8, 79).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5".
        THIS-OBJECT:ultraLabel5:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel5:TabIndex = 4.
        THIS-OBJECT:ultraLabel5:Text = "Tables:".
        /*  */
        /* ultraLabel4 */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel4:Appearance = appearance8.
        THIS-OBJECT:ultraLabel4:Location = NEW System.Drawing.Point(8, 52).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4".
        THIS-OBJECT:ultraLabel4:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel4:TabIndex = 2.
        THIS-OBJECT:ultraLabel4:Text = "Business Entity:".
        /*  */
        /* tbLookupBusinessEntity */
        /*  */
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar8 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbLookupBusinessEntity:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbLookupBusinessEntity:ButtonsRight:Add(editorButton4).
        THIS-OBJECT:tbLookupBusinessEntity:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tbLookupBusinessEntity:Location = NEW System.Drawing.Point(192, 48).
        THIS-OBJECT:tbLookupBusinessEntity:Name = "tbLookupBusinessEntity".
        THIS-OBJECT:tbLookupBusinessEntity:ReadOnly = TRUE.
        THIS-OBJECT:tbLookupBusinessEntity:Size = NEW System.Drawing.Size(466, 24).
        THIS-OBJECT:tbLookupBusinessEntity:TabIndex = 3.
        THIS-OBJECT:tbLookupBusinessEntity:EditorButtonClick:Subscribe(THIS-OBJECT:tbLookupBusinessEntity_EditorButtonClick).
        THIS-OBJECT:tbLookupBusinessEntity:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 9.
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 182).
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Left:Name = "m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Left".
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(9, 339).
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 9.
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(693, 182).
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Right:Name = "m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Right".
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(9, 339).
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Top:Name = "m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Top".
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(702, 182).
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 9.
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 521).
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Bottom:Name = "m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Bottom".
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(702, 9).
        THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* DynamicLookupDesignerForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(702, 530).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:DynamicLookupDesignerForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_DynamicLookupDesignerForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon"), System.Drawing.Icon).
        THIS-OBJECT:Name = "DynamicLookupDesignerForm".
        THIS-OBJECT:Text = "SmartLookup Designer".
        THIS-OBJECT:WindowPositionRegistryKey = "DynamicLookupDesignerForm".
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:DynamicLookupDesignerForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:gbLookup, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:gbLookup:ResumeLayout(FALSE).
        THIS-OBJECT:gbLookup:PerformLayout().
        CAST(THIS-OBJECT:cbObjectType, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbObjectName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbModule, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbNewObjectName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:gbLookupProperties, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:gbLookupProperties:ResumeLayout(FALSE).
        THIS-OBJECT:gbLookupProperties:PerformLayout().
        CAST(THIS-OBJECT:tbDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbLookupView, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbLookupTable, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbLookupBusinessEntity, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown handler of the Text boxes with an 
                 EditorButton (Lookup)
        Notes:   Maps F3 to EditorButtonClick
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID LookupKeyDownHandler (sender AS System.Object, 
                                              e AS System.Windows.Forms.KeyEventArgs):

        DEFINE VARIABLE cControlName AS CHARACTER NO-UNDO.
        
        IF e:Handled = FALSE AND Progress.Util.EnumHelper:AreEqual (e:KeyCode, SmartLookupSettings:ShortcutKey) 
            AND THIS-OBJECT:Enabled THEN DO:

            ASSIGN cControlName = CAST (sender, System.Windows.Forms.Control):Name .

            DYNAMIC-INVOKE (THIS-OBJECT, 
                            SUBSTITUTE ("&1_EditorButtonClick":U, cControlName), 
                            sender, 
                            NEW EditorButtonEventArgs (?, ?)) .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .                
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Creats a new Lookup                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID NewLookup ():

        oRepository:GetEmptyObjectStructure (THIS-OBJECT:cbObjectType:Text, 
                                             OUTPUT DATASET-HANDLE hDesignDataset) .

        hDesignDataset:GET-BUFFER-HANDLE (1):BUFFER-CREATE () .

        THIS-OBJECT:CreateDesignLookup () .

        tbDescription:Text = "":U . 

        THIS-OBJECT:DisplayLookupProperties () .

        THIS-OBJECT:LookupDesignerState = LookupDesignerStateEnum:New .

        THIS-OBJECT:SetButtons () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event                                                                         
        Notes:       
        @param e The System.EventArgs with the data for this event                                                                 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
        
        SUPER:OnLoad (e) .

&IF DEFINED (UseSimpleDynamicsRepositoryService) NE 0 &THEN                
        ASSIGN oRepository = {Consultingwerk/get-service.i Consultingwerk.Framework.RepositoryServices.IRepositoryService} . 
                                                          
        IF NOT VALID-OBJECT (oRepository) THEN DO: 
            oRepository = DYNAMIC-NEW "Consultingwerk.Framework.RepositoryServices.Dynamics.SimpleDynamicsRepositoryService" ()  .

            FrameworkSettings:ServiceContainer:AddService 
                             (Progress.Lang.Class:GetClass ("Consultingwerk.Framework.RepositoryServices.IRepositoryService":U),
                             oRepository) .       
        END.                                                             
&ELSE
        ASSIGN oRepository = {Consultingwerk/get-service.i Consultingwerk.Framework.RepositoryServices.IRepositoryService} .
&ENDIF            
          
        IF NOT VALID-OBJECT (oRepository) THEN 
            UNDO, THROW NEW AppError ("Unable to start this Form without Repository Service.", 0) .

        THIS-OBJECT:cbObjectType:ValueList = Consultingwerk.Util.ValueListHelper:FromList (oRepository:ClassChildren ("SmartBusinessEntityLookup")) .
        THIS-OBJECT:cbObjectType:SelectedIndex = 0 . 
        
        THIS-OBJECT:SetLookupFilter () .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Prepares opening an existing lookup                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID Open ():
        
        THIS-OBJECT:LookupDesignerState = LookupDesignerStateEnum:Opening .

        THIS-OBJECT:SetButtons() .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves the current lookup to the repository                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID Save ():

        DEFINE VARIABLE oSerializer AS ISmartBusinessEntityLookupSerializer NO-UNDO . 
        DEFINE VARIABLE hBuffer     AS HANDLE                               NO-UNDO.
        
        oSerializer = { Consultingwerk/get-service.i Consultingwerk.SmartComponents.Framework.ISmartBusinessEntityLookupSerializer
                                                     "NEW SmartBusinessEntityLookupSerializer ()"} . 
        
        ASSIGN hBuffer = hDesignDataset:GET-BUFFER-HANDLE (1) .
            
        hBuffer:FIND-FIRST () .

        IF THIS-OBJECT:LookupDesignerState = LookupDesignerStateEnum:New THEN
            ASSIGN hBuffer::ObjectName = tbNewObjectName:Text . 
        ELSE 
            ASSIGN hBuffer::ObjectName = tbObjectName:Text .                                
    
        ASSIGN hBuffer::ObjectDescription = tbDescription:Text 
               hBuffer::ObjectType        = cbObjectType:Text
               hBuffer::ProductModuleCode = tbModule:Text
               .
                
        oSerializer:Serialize (oDesignLookup, hBuffer) . 
        
        oRepository:StoreObject ((THIS-OBJECT:LookupDesignerState = LookupDesignerStateEnum:New), 
                                 DATASET-HANDLE hDesignDataset BY-REFERENCE) .
        
        THIS-OBJECT:LookupDesignerState = LookupDesignerStateEnum:Editing .                          

        THIS-OBJECT:SetButtons() .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets the Buttons according to the LookupDesignerState                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SetButtons ():
        
        CASE THIS-OBJECT:LookupDesignerState:
            
            WHEN LookupDesignerStateEnum:Default THEN DO:
                smartToolbarController1:EnableTools ("btnNew,btnOpen":U) .                
                smartToolbarController1:DisableTools ("btnSave,btnCancel":U) .
                
                THIS-OBJECT:btnOpenExisting:Enabled = FALSE . 
                
                THIS-OBJECT:gbLookup:Enabled = FALSE . 
                THIS-OBJECT:gbLookupProperties:Enabled = FALSE .
                
                THIS-OBJECT:tbNewObjectName:Visible = FALSE .
                THIS-OBJECT:tbObjectName:Visible = TRUE .  
            END.
            WHEN LookupDesignerStateEnum:New THEN DO:
                smartToolbarController1:DisableTools ("btnNew,btnOpen":U) .                
                smartToolbarController1:EnableTools ("btnSave,btnCancel":U) . 
                
                THIS-OBJECT:btnOpenExisting:Enabled = FALSE . 

                THIS-OBJECT:gbLookup:Enabled = TRUE . 
                THIS-OBJECT:gbLookupProperties:Enabled = TRUE . 

                THIS-OBJECT:tbNewObjectName:Visible = TRUE .
                THIS-OBJECT:tbObjectName:Visible = FALSE .  
            END.
            WHEN LookupDesignerStateEnum:Opening THEN DO:
                smartToolbarController1:DisableTools ("btnSave,btnNew,btnOpen":U) .                
                smartToolbarController1:EnableTools ("btnCancel":U) .

                THIS-OBJECT:btnOpenExisting:Enabled = TRUE . 
                
                THIS-OBJECT:gbLookup:Enabled = TRUE . 
                THIS-OBJECT:gbLookupProperties:Enabled = FALSE . 

                THIS-OBJECT:tbNewObjectName:Visible = FALSE .
                THIS-OBJECT:tbObjectName:Visible = TRUE .  
            END.
            WHEN LookupDesignerStateEnum:Editing THEN DO:
                smartToolbarController1:DisableTools ("btnNew,btnOpen":U) .                
                smartToolbarController1:EnableTools ("btnSave,btnCancel":U) .
                
                THIS-OBJECT:btnOpenExisting:Enabled = FALSE . 
                
                THIS-OBJECT:gbLookup:Enabled = FALSE . 
                THIS-OBJECT:gbLookupProperties:Enabled = TRUE . 

                THIS-OBJECT:tbNewObjectName:Visible = FALSE .
                THIS-OBJECT:tbObjectName:Visible = TRUE .  
            END.

        END CASE . 
              
    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Sets the filter for the lookup lookup 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SetLookupFilter ():
        
        DEFINE VARIABLE cQuery AS CHARACTER NO-UNDO.
        
        IF THIS-OBJECT:cbObjectType:Text > "":U THEN 
            ASSIGN cQuery = SUBSTITUTE (" eSmartObject.object_type_code = &1 ":U,
                                        QUOTER (THIS-OBJECT:cbObjectType:Text)) . 
        
        IF THIS-OBJECT:tbModule:Text > "":U THEN 
            ASSIGN cQuery = cQuery + 
                            (IF cQuery > "":U THEN " AND ":U ELSE "":U) + 
                            SUBSTITUTE (" eSmartObject.product_module_code = &1 ":U,
                                        QUOTER (THIS-OBJECT:tbModule:Text)) .  
        
        IF cQuery > "":U THEN 
            ASSIGN tbObjectName:LookupQueryString = "FOR EACH eSmartObject WHERE eSmartObject.object_filename BEGINS '&1' AND ":U + cQuery .
        ELSE 
            ASSIGN tbObjectName:LookupQueryString = "FOR EACH eSmartObject WHERE eSmartObject.object_filename BEGINS '&1'":U .

        IF cQuery > "":U THEN 
            ASSIGN tbObjectName:LookupDialogQueryString = "FOR EACH eSmartObject WHERE ":U + cQuery .
        ELSE 
            ASSIGN tbObjectName:LookupDialogQueryString = "":U . 
            
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the smartToolbarController1                                                                        
        Notes:                                                                       
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event   
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartToolbarController1_ToolClick (sender AS System.Object, 
                                                           e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):
        
        CASE e:Tool:Key:
            WHEN "btnNew":U THEN 
                THIS-OBJECT:NewLookup () . 
            WHEN "btnOpen":U THEN 
                THIS-OBJECT:Open () . 
            WHEN "BtnSave":U THEN 
                THIS-OBJECT:Save () . 
            WHEN "btnCancel":U THEN 
                THIS-OBJECT:Cancel () .
        END CASE .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the tbLookupBusinessEntity                                                                        
        Notes:                                                           
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event             
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbLookupBusinessEntity_EditorButtonClick (sender AS System.Object, 
                                                                  e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
        
        Consultingwerk.Framework.FrameworkSettings:AllowDesignFeatures = TRUE . 
        
        oDesignLookup:OnVerbClicked ("Select BusinessEntity":U) .
        
        THIS-OBJECT:DisplayLookupProperties () .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

        FINALLY:
            Consultingwerk.Framework.FrameworkSettings:AllowDesignFeatures = FALSE . 
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the tbLookupTable                                                                        
        Notes:                                                           
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event             
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbLookupTable_EditorButtonClick (sender AS System.Object, 
                                                         e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
        
        Consultingwerk.Framework.FrameworkSettings:AllowDesignFeatures = TRUE . 

        oDesignLookup:OnVerbClicked ("Select Tables":U) .
        
        THIS-OBJECT:DisplayLookupProperties () .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

        FINALLY:
            Consultingwerk.Framework.FrameworkSettings:AllowDesignFeatures = FALSE . 
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of the tbModule                                                                        
        Notes:                                                           
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event             
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbModule_TextChanged (sender AS System.Object, 
                                              e AS System.EventArgs ):
        
        THIS-OBJECT:SetLookupFilter () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton1                                                                        
        Notes:                                                        
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButton1_Click (sender AS System.Object, 
                                            e AS System.EventArgs):
        
        Consultingwerk.Framework.FrameworkSettings:AllowDesignFeatures = TRUE . 

        oDesignLookup:OnVerbClicked ("Lookup Designer":U) .
        
        THIS-OBJECT:DisplayLookupProperties () .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

        FINALLY:
            Consultingwerk.Framework.FrameworkSettings:AllowDesignFeatures = FALSE . 
        END FINALLY.
        

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the DynamicLookupDesignerForm class                                                       
        Notes:                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC DynamicLookupDesignerForm ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
