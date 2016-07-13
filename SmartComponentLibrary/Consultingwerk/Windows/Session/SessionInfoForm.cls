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
    File        : SessionInfoForm
    Purpose     : Display AVM and SmartComponent Library related session 
                  information for trouble shooting purposes
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jun 05 13:50:44 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                    FROM PROPATH . 
USING Consultingwerk.Assertion.*          FROM PROPATH .
USING Consultingwerk.Framework.*          FROM PROPATH . 
USING Consultingwerk.Forms.*              FROM PROPATH .
USING Consultingwerk.Util.*               FROM PROPATH . 
USING Consultingwerk.Windows.Session.*    FROM PROPATH .  
USING Consultingwerk.Windows.WebClient.*  FROM PROPATH .
USING Progress.Lang.*                     FROM PROPATH .
USING Infragistics.Win.UltraWinListView.* FROM ASSEMBLY . 

CLASS Consultingwerk.Windows.Session.SessionInfoForm 
    INHERITS BaseForm
    USE-WIDGET-POOL: 

    DEFINE PRIVATE VARIABLE button2 AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE fileLookupControl1 AS Consultingwerk.Windows.Session.FileLookupControl NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SessionInfoForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SessionInfoForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SessionInfoForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SessionInfoForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE button1 AS System.Windows.Forms.Button NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE lvDatabases AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE lvInternationalization AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE lvProcess AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE lvServers AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE lvServiceDefinitions AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE lvServices AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE lvSessionProperties AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE lvStartupParameter AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTile1 AS Infragistics.Win.Misc.UltraTile NO-UNDO.
    DEFINE PRIVATE VARIABLE lvPropath AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTile10 AS Infragistics.Win.Misc.UltraTile NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTile11 AS Infragistics.Win.Misc.UltraTile NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTile2 AS Infragistics.Win.Misc.UltraTile NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTile3 AS Infragistics.Win.Misc.UltraTile NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTile4 AS Infragistics.Win.Misc.UltraTile NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTile5 AS Infragistics.Win.Misc.UltraTile NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTile6 AS Infragistics.Win.Misc.UltraTile NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTile7 AS Infragistics.Win.Misc.UltraTile NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTile8 AS Infragistics.Win.Misc.UltraTile NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTile9 AS Infragistics.Win.Misc.UltraTile NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTilePanel1 AS Infragistics.Win.Misc.UltraTilePanel NO-UNDO.
    
    { Consultingwerk/Framework/ttServiceTable.i }  
    
    DEFINE VARIABLE oFullWindowDragManager1 AS Consultingwerk.Windows.Util.Forms.FullWindowDragManager NO-UNDO . 
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SessionInfoForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SessionInfoForm ():
		
        SUPER().
        InitializeComponent().
        
        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF
                        
        oFullWindowDragManager1 = NEW Consultingwerk.Windows.Util.Forms.FullWindowDragManager (THIS-OBJECT) .
                                
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the button2 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID button2_Click (sender AS System.Object, 
	                                   e AS System.EventArgs):

&IF DEFINED (SmartComponentLibrary) NE 0 &THEN
        DEFINE VARIABLE oForm     AS ProWcAppPropertiesForm NO-UNDO . 
        DEFINE VARIABLE cFileName AS CHARACTER              NO-UNDO .

        ASSIGN cFileName = StartupParameterHelper:GetStartupParameterValue ("prowcapp":U) .

        FileAssert:Exists (cFileName) .

        oForm = NEW ProWcAppPropertiesForm () .
        oForm:LoadFrom (FileHelper:FindFile (cFileName)) .     
    
        WAIT-FOR oForm:ShowDialog () .

        CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.
&ENDIF
	END METHOD .

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
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Session.SessionInfoForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewSubItemColumn1 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn NO-UNDO.
        ultraListViewSubItemColumn1 = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn("absolute":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewSubItemColumn2 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn NO-UNDO.
        ultraListViewSubItemColumn2 = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn("Parameters":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewSubItemColumn3 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn NO-UNDO.
        ultraListViewSubItemColumn3 = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn("Value":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewSubItemColumn4 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn NO-UNDO.
        ultraListViewSubItemColumn4 = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn("Value":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewSubItemColumn5 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn NO-UNDO.
        ultraListViewSubItemColumn5 = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn("Value":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewSubItemColumn6 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn NO-UNDO.
        ultraListViewSubItemColumn6 = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn("Value":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewSubItemColumn7 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn NO-UNDO.
        ultraListViewSubItemColumn7 = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn("Instance":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewSubItemColumn8 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn NO-UNDO.
        ultraListViewSubItemColumn8 = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn("Name":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewSubItemColumn9 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn NO-UNDO.
        ultraListViewSubItemColumn9 = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn("Absolute":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Windows Explorer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open File":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool1 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool1 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("Service Definitions":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open File":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Windows Explorer":U).
        THIS-OBJECT:button1 = NEW System.Windows.Forms.Button().
        THIS-OBJECT:ultraTilePanel1 = NEW Infragistics.Win.Misc.UltraTilePanel().
        THIS-OBJECT:ultraTile1 = NEW Infragistics.Win.Misc.UltraTile().
        THIS-OBJECT:lvPropath = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:ultraTile2 = NEW Infragistics.Win.Misc.UltraTile().
        THIS-OBJECT:lvDatabases = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:ultraTile3 = NEW Infragistics.Win.Misc.UltraTile().
        THIS-OBJECT:lvSessionProperties = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:ultraTile4 = NEW Infragistics.Win.Misc.UltraTile().
        THIS-OBJECT:lvStartupParameter = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:ultraTile5 = NEW Infragistics.Win.Misc.UltraTile().
        THIS-OBJECT:lvInternationalization = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:ultraTile6 = NEW Infragistics.Win.Misc.UltraTile().
        THIS-OBJECT:lvProcess = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:ultraTile7 = NEW Infragistics.Win.Misc.UltraTile().
        THIS-OBJECT:lvServices = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:ultraTile8 = NEW Infragistics.Win.Misc.UltraTile().
        THIS-OBJECT:lvServers = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:ultraTile9 = NEW Infragistics.Win.Misc.UltraTile().
        THIS-OBJECT:lvServiceDefinitions = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:ultraTile11 = NEW Infragistics.Win.Misc.UltraTile().
        THIS-OBJECT:fileLookupControl1 = NEW Consultingwerk.Windows.Session.FileLookupControl().
        THIS-OBJECT:ultraTile10 = NEW Infragistics.Win.Misc.UltraTile().
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:button2 = NEW System.Windows.Forms.Button().
        CAST(THIS-OBJECT:ultraTilePanel1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTilePanel1:SuspendLayout().
        THIS-OBJECT:ultraTile1:SuspendLayout().
        CAST(THIS-OBJECT:lvPropath, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTile2:SuspendLayout().
        CAST(THIS-OBJECT:lvDatabases, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTile3:SuspendLayout().
        CAST(THIS-OBJECT:lvSessionProperties, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTile4:SuspendLayout().
        CAST(THIS-OBJECT:lvStartupParameter, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTile5:SuspendLayout().
        CAST(THIS-OBJECT:lvInternationalization, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTile6:SuspendLayout().
        CAST(THIS-OBJECT:lvProcess, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTile7:SuspendLayout().
        CAST(THIS-OBJECT:lvServices, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTile8:SuspendLayout().
        CAST(THIS-OBJECT:lvServers, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTile9:SuspendLayout().
        CAST(THIS-OBJECT:lvServiceDefinitions, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTile11:SuspendLayout().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* button1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:button1, "button1":U).
        THIS-OBJECT:button1:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:button1:Name = "button1":U.
        THIS-OBJECT:button1:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:button1:UseVisualStyleBackColor = TRUE.
        /*  */
        /* ultraTilePanel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTilePanel1, "ultraTilePanel1":U).
        appearance1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:ultraTilePanel1:Appearance = appearance1.
        THIS-OBJECT:ultraTilePanel1:MaximumVisibleLargeTiles = 2.
        THIS-OBJECT:ultraTilePanel1:MinimumTileSize = NEW System.Drawing.Size(180, 10).
        THIS-OBJECT:ultraTilePanel1:Name = "ultraTilePanel1":U.
        THIS-OBJECT:ultraTilePanel1:NormalModeDimensions = NEW System.Drawing.Size(3, 3).
        THIS-OBJECT:ultraTilePanel1:Tiles:Add(THIS-OBJECT:ultraTile1).
        THIS-OBJECT:ultraTilePanel1:Tiles:Add(THIS-OBJECT:ultraTile2).
        THIS-OBJECT:ultraTilePanel1:Tiles:Add(THIS-OBJECT:ultraTile3).
        THIS-OBJECT:ultraTilePanel1:Tiles:Add(THIS-OBJECT:ultraTile4).
        THIS-OBJECT:ultraTilePanel1:Tiles:Add(THIS-OBJECT:ultraTile5).
        THIS-OBJECT:ultraTilePanel1:Tiles:Add(THIS-OBJECT:ultraTile6).
        THIS-OBJECT:ultraTilePanel1:Tiles:Add(THIS-OBJECT:ultraTile7).
        THIS-OBJECT:ultraTilePanel1:Tiles:Add(THIS-OBJECT:ultraTile8).
        THIS-OBJECT:ultraTilePanel1:Tiles:Add(THIS-OBJECT:ultraTile9).
        THIS-OBJECT:ultraTilePanel1:Tiles:Add(THIS-OBJECT:ultraTile11).
        /*  */
        /* ultraTile1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTile1, "ultraTile1":U).
        THIS-OBJECT:ultraTile1:Control = THIS-OBJECT:lvPropath.
        THIS-OBJECT:ultraTile1:Controls:Add(THIS-OBJECT:lvPropath).
        THIS-OBJECT:ultraTile1:IndexInLargeTileCollection = 0.
        THIS-OBJECT:ultraTile1:Name = "ultraTile1":U.
        THIS-OBJECT:ultraTile1:PositionInNormalMode = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraTile1:State = Infragistics.Win.Misc.TileState:Large.
        /*  */
        /* lvPropath */
        /*  */
        resources:ApplyResources(THIS-OBJECT:lvPropath, "lvPropath":U).
        THIS-OBJECT:lvPropath:MainColumn:AllowSorting = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:lvPropath:MainColumn:Key = "entry":U.
        THIS-OBJECT:lvPropath:MainColumn:Text = resources:GetString("lvPropath.MainColumn.Text":U).
        THIS-OBJECT:lvPropath:MainColumn:Width = 50.
        THIS-OBJECT:lvPropath:Name = "lvPropath":U.
        ultraListViewSubItemColumn1:AllowSorting = Infragistics.Win.DefaultableBoolean:False.
        ultraListViewSubItemColumn1:Key = "absolute":U.
        resources:ApplyResources(ultraListViewSubItemColumn1, "ultraListViewSubItemColumn1":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn EXTENT 1 NO-UNDO.
        arrayvar0[1] = ultraListViewSubItemColumn1.
        THIS-OBJECT:lvPropath:SubItemColumns:AddRange(arrayvar0).
        THIS-OBJECT:lvPropath:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Details.
        THIS-OBJECT:lvPropath:ViewSettingsDetails:AutoFitColumns = Infragistics.Win.UltraWinListView.AutoFitColumns:ResizeAllColumns.
        THIS-OBJECT:lvPropath:MouseDoubleClick:Subscribe(THIS-OBJECT:lvPropath_MouseDoubleClick).
        /*  */
        /* ultraTile2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTile2, "ultraTile2":U).
        THIS-OBJECT:ultraTile2:Control = THIS-OBJECT:lvDatabases.
        THIS-OBJECT:ultraTile2:Controls:Add(THIS-OBJECT:lvDatabases).
        THIS-OBJECT:ultraTile2:Name = "ultraTile2":U.
        THIS-OBJECT:ultraTile2:PositionInNormalMode = NEW System.Drawing.Point(1, 0).
        /*  */
        /* lvDatabases */
        /*  */
        resources:ApplyResources(THIS-OBJECT:lvDatabases, "lvDatabases":U).
        THIS-OBJECT:lvDatabases:MainColumn:Text = resources:GetString("lvDatabases.MainColumn.Text":U).
        THIS-OBJECT:lvDatabases:MainColumn:Width = 80.
        THIS-OBJECT:lvDatabases:Name = "lvDatabases":U.
        ultraListViewSubItemColumn2:Key = "Parameters":U.
        resources:ApplyResources(ultraListViewSubItemColumn2, "ultraListViewSubItemColumn2":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn EXTENT 1 NO-UNDO.
        arrayvar1[1] = ultraListViewSubItemColumn2.
        THIS-OBJECT:lvDatabases:SubItemColumns:AddRange(arrayvar1).
        THIS-OBJECT:lvDatabases:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Details.
        THIS-OBJECT:lvDatabases:ViewSettingsDetails:AutoFitColumns = Infragistics.Win.UltraWinListView.AutoFitColumns:ResizeAllColumns.
        /*  */
        /* ultraTile3 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTile3, "ultraTile3":U).
        THIS-OBJECT:ultraTile3:Control = THIS-OBJECT:lvSessionProperties.
        THIS-OBJECT:ultraTile3:Controls:Add(THIS-OBJECT:lvSessionProperties).
        THIS-OBJECT:ultraTile3:Name = "ultraTile3":U.
        THIS-OBJECT:ultraTile3:PositionInNormalMode = NEW System.Drawing.Point(2, 0).
        /*  */
        /* lvSessionProperties */
        /*  */
        resources:ApplyResources(THIS-OBJECT:lvSessionProperties, "lvSessionProperties":U).
        THIS-OBJECT:lvSessionProperties:MainColumn:Text = resources:GetString("lvSessionProperties.MainColumn.Text":U).
        THIS-OBJECT:lvSessionProperties:MainColumn:Width = 50.
        THIS-OBJECT:lvSessionProperties:Name = "lvSessionProperties":U.
        ultraListViewSubItemColumn3:Key = "Value":U.
        resources:ApplyResources(ultraListViewSubItemColumn3, "ultraListViewSubItemColumn3":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn EXTENT 1 NO-UNDO.
        arrayvar2[1] = ultraListViewSubItemColumn3.
        THIS-OBJECT:lvSessionProperties:SubItemColumns:AddRange(arrayvar2).
        THIS-OBJECT:lvSessionProperties:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Details.
        THIS-OBJECT:lvSessionProperties:ViewSettingsDetails:AutoFitColumns = Infragistics.Win.UltraWinListView.AutoFitColumns:ResizeAllColumns.
        /*  */
        /* ultraTile4 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTile4, "ultraTile4":U).
        THIS-OBJECT:ultraTile4:Control = THIS-OBJECT:lvStartupParameter.
        THIS-OBJECT:ultraTile4:Controls:Add(THIS-OBJECT:lvStartupParameter).
        THIS-OBJECT:ultraTile4:IndexInLargeTileCollection = 1.
        THIS-OBJECT:ultraTile4:Name = "ultraTile4":U.
        THIS-OBJECT:ultraTile4:PositionInNormalMode = NEW System.Drawing.Point(0, 1).
        THIS-OBJECT:ultraTile4:State = Infragistics.Win.Misc.TileState:Large.
        /*  */
        /* lvStartupParameter */
        /*  */
        resources:ApplyResources(THIS-OBJECT:lvStartupParameter, "lvStartupParameter":U).
        THIS-OBJECT:lvStartupParameter:MainColumn:Text = resources:GetString("lvStartupParameter.MainColumn.Text":U).
        THIS-OBJECT:lvStartupParameter:MainColumn:Width = 80.
        THIS-OBJECT:lvStartupParameter:Name = "lvStartupParameter":U.
        ultraListViewSubItemColumn4:Key = "Value":U.
        resources:ApplyResources(ultraListViewSubItemColumn4, "ultraListViewSubItemColumn4":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn EXTENT 1 NO-UNDO.
        arrayvar3[1] = ultraListViewSubItemColumn4.
        THIS-OBJECT:lvStartupParameter:SubItemColumns:AddRange(arrayvar3).
        THIS-OBJECT:lvStartupParameter:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Details.
        THIS-OBJECT:lvStartupParameter:ViewSettingsDetails:AutoFitColumns = Infragistics.Win.UltraWinListView.AutoFitColumns:ResizeAllColumns.
        /*  */
        /* ultraTile5 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTile5, "ultraTile5":U).
        THIS-OBJECT:ultraTile5:Control = THIS-OBJECT:lvInternationalization.
        THIS-OBJECT:ultraTile5:Controls:Add(THIS-OBJECT:lvInternationalization).
        THIS-OBJECT:ultraTile5:Name = "ultraTile5":U.
        THIS-OBJECT:ultraTile5:PositionInNormalMode = NEW System.Drawing.Point(1, 1).
        /*  */
        /* lvInternationalization */
        /*  */
        resources:ApplyResources(THIS-OBJECT:lvInternationalization, "lvInternationalization":U).
        THIS-OBJECT:lvInternationalization:MainColumn:Text = resources:GetString("lvInternationalization.MainColumn.Text":U).
        THIS-OBJECT:lvInternationalization:MainColumn:Width = 80.
        THIS-OBJECT:lvInternationalization:Name = "lvInternationalization":U.
        ultraListViewSubItemColumn5:Key = "Value":U.
        resources:ApplyResources(ultraListViewSubItemColumn5, "ultraListViewSubItemColumn5":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn EXTENT 1 NO-UNDO.
        arrayvar4[1] = ultraListViewSubItemColumn5.
        THIS-OBJECT:lvInternationalization:SubItemColumns:AddRange(arrayvar4).
        THIS-OBJECT:lvInternationalization:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Details.
        THIS-OBJECT:lvInternationalization:ViewSettingsDetails:AutoFitColumns = Infragistics.Win.UltraWinListView.AutoFitColumns:ResizeAllColumns.
        /*  */
        /* ultraTile6 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTile6, "ultraTile6":U).
        THIS-OBJECT:ultraTile6:Control = THIS-OBJECT:lvProcess.
        THIS-OBJECT:ultraTile6:Controls:Add(THIS-OBJECT:lvProcess).
        THIS-OBJECT:ultraTile6:Name = "ultraTile6":U.
        THIS-OBJECT:ultraTile6:PositionInNormalMode = NEW System.Drawing.Point(2, 1).
        /*  */
        /* lvProcess */
        /*  */
        resources:ApplyResources(THIS-OBJECT:lvProcess, "lvProcess":U).
        THIS-OBJECT:lvProcess:MainColumn:Text = resources:GetString("lvProcess.MainColumn.Text":U).
        THIS-OBJECT:lvProcess:MainColumn:Width = 80.
        THIS-OBJECT:lvProcess:Name = "lvProcess":U.
        ultraListViewSubItemColumn6:Key = "Value":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn EXTENT 1 NO-UNDO.
        arrayvar5[1] = ultraListViewSubItemColumn6.
        THIS-OBJECT:lvProcess:SubItemColumns:AddRange(arrayvar5).
        THIS-OBJECT:lvProcess:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Details.
        THIS-OBJECT:lvProcess:ViewSettingsDetails:AutoFitColumns = Infragistics.Win.UltraWinListView.AutoFitColumns:ResizeAllColumns.
        /*  */
        /* ultraTile7 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTile7, "ultraTile7":U).
        THIS-OBJECT:ultraTile7:Control = THIS-OBJECT:lvServices.
        THIS-OBJECT:ultraTile7:Controls:Add(THIS-OBJECT:lvServices).
        THIS-OBJECT:ultraTile7:Name = "ultraTile7":U.
        THIS-OBJECT:ultraTile7:PositionInNormalMode = NEW System.Drawing.Point(0, 2).
        /*  */
        /* lvServices */
        /*  */
        resources:ApplyResources(THIS-OBJECT:lvServices, "lvServices":U).
        THIS-OBJECT:lvServices:MainColumn:Text = resources:GetString("lvServices.MainColumn.Text":U).
        THIS-OBJECT:lvServices:MainColumn:Width = 80.
        THIS-OBJECT:lvServices:Name = "lvServices":U.
        ultraListViewSubItemColumn7:Key = "Instance":U.
        resources:ApplyResources(ultraListViewSubItemColumn7, "ultraListViewSubItemColumn7":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn EXTENT 1 NO-UNDO.
        arrayvar6[1] = ultraListViewSubItemColumn7.
        THIS-OBJECT:lvServices:SubItemColumns:AddRange(arrayvar6).
        THIS-OBJECT:lvServices:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Details.
        THIS-OBJECT:lvServices:ViewSettingsDetails:AutoFitColumns = Infragistics.Win.UltraWinListView.AutoFitColumns:ResizeAllColumns.
        /*  */
        /* ultraTile8 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTile8, "ultraTile8":U).
        THIS-OBJECT:ultraTile8:Control = THIS-OBJECT:lvServers.
        THIS-OBJECT:ultraTile8:Controls:Add(THIS-OBJECT:lvServers).
        THIS-OBJECT:ultraTile8:Name = "ultraTile8":U.
        /*  */
        /* lvServers */
        /*  */
        resources:ApplyResources(THIS-OBJECT:lvServers, "lvServers":U).
        THIS-OBJECT:lvServers:MainColumn:Text = resources:GetString("lvServers.MainColumn.Text":U).
        THIS-OBJECT:lvServers:MainColumn:Width = 80.
        THIS-OBJECT:lvServers:Name = "lvServers":U.
        ultraListViewSubItemColumn8:Key = "Name":U.
        resources:ApplyResources(ultraListViewSubItemColumn8, "ultraListViewSubItemColumn8":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn EXTENT 1 NO-UNDO.
        arrayvar7[1] = ultraListViewSubItemColumn8.
        THIS-OBJECT:lvServers:SubItemColumns:AddRange(arrayvar7).
        THIS-OBJECT:lvServers:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Details.
        THIS-OBJECT:lvServers:ViewSettingsDetails:AutoFitColumns = Infragistics.Win.UltraWinListView.AutoFitColumns:ResizeAllColumns.
        /*  */
        /* ultraTile9 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTile9, "ultraTile9":U).
        THIS-OBJECT:ultraTile9:Control = THIS-OBJECT:lvServiceDefinitions.
        THIS-OBJECT:ultraTile9:Controls:Add(THIS-OBJECT:lvServiceDefinitions).
        THIS-OBJECT:ultraTile9:Name = "ultraTile9":U.
        /*  */
        /* lvServiceDefinitions */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:SetContextMenuUltra(THIS-OBJECT:lvServiceDefinitions, "Service Definitions":U).
        resources:ApplyResources(THIS-OBJECT:lvServiceDefinitions, "lvServiceDefinitions":U).
        THIS-OBJECT:lvServiceDefinitions:MainColumn:Text = resources:GetString("lvServiceDefinitions.MainColumn.Text":U).
        THIS-OBJECT:lvServiceDefinitions:MainColumn:Width = 80.
        THIS-OBJECT:lvServiceDefinitions:Name = "lvServiceDefinitions":U.
        ultraListViewSubItemColumn9:Key = "Absolute":U.
        resources:ApplyResources(ultraListViewSubItemColumn9, "ultraListViewSubItemColumn9":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn EXTENT 1 NO-UNDO.
        arrayvar8[1] = ultraListViewSubItemColumn9.
        THIS-OBJECT:lvServiceDefinitions:SubItemColumns:AddRange(arrayvar8).
        THIS-OBJECT:lvServiceDefinitions:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Details.
        THIS-OBJECT:lvServiceDefinitions:ViewSettingsDetails:AutoFitColumns = Infragistics.Win.UltraWinListView.AutoFitColumns:ResizeAllColumns.
        /*  */
        /* ultraTile11 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTile11, "ultraTile11":U).
        THIS-OBJECT:ultraTile11:Control = THIS-OBJECT:fileLookupControl1.
        THIS-OBJECT:ultraTile11:Controls:Add(THIS-OBJECT:fileLookupControl1).
        THIS-OBJECT:ultraTile11:Name = "ultraTile11":U.
        /*  */
        /* fileLookupControl1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:fileLookupControl1, "fileLookupControl1":U).
        THIS-OBJECT:fileLookupControl1:Name = "fileLookupControl1":U.
        /*  */
        /* ultraTile10 */
        /*  */
        THIS-OBJECT:ultraTile10:Name = "ultraTile10":U.
        resources:ApplyResources(THIS-OBJECT:ultraTile10, "ultraTile10":U).
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.Forms.BaseForm":U).
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool1:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
        buttonTool1:SharedPropsInternal:Caption = resources:GetString("resource.Caption":U).
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        buttonTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance3.
        buttonTool2:SharedPropsInternal:Caption = resources:GetString("resource.Caption1":U).
        popupMenuTool1:SharedPropsInternal:Caption = resources:GetString("resource.Caption2":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar9[1] = buttonTool3.
        arrayvar9[2] = buttonTool4.
        popupMenuTool1:Tools:AddRange(arrayvar9).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar10[1] = buttonTool1.
        arrayvar10[2] = buttonTool2.
        arrayvar10[3] = popupMenuTool1.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar10).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        /*  */
        /* m_SessionInfoForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        resources:ApplyResources(THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Left, "m_SessionInfoForm_Toolbars_Dock_Area_Left":U).
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Left:Name = "m_SessionInfoForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_SessionInfoForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        resources:ApplyResources(THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Right, "m_SessionInfoForm_Toolbars_Dock_Area_Right":U).
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Right:Name = "m_SessionInfoForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_SessionInfoForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        resources:ApplyResources(THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Top, "m_SessionInfoForm_Toolbars_Dock_Area_Top":U).
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Top:Name = "m_SessionInfoForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_SessionInfoForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        resources:ApplyResources(THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Bottom, "m_SessionInfoForm_Toolbars_Dock_Area_Bottom":U).
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Bottom:Name = "m_SessionInfoForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* button2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:button2, "button2":U).
        THIS-OBJECT:button2:Name = "button2":U.
        THIS-OBJECT:button2:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:button2:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:button2:Click:Subscribe(THIS-OBJECT:button2_Click).
        /*  */
        /* SessionInfoForm */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:button2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTilePanel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:button1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SessionInfoForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Name = "SessionInfoForm":U.
        CAST(THIS-OBJECT:ultraTilePanel1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTilePanel1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTile1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:lvPropath, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTile2:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:lvDatabases, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTile3:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:lvSessionProperties, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTile4:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:lvStartupParameter, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTile5:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:lvInternationalization, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTile6:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:lvProcess, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTile7:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:lvServices, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTile8:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:lvServers, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTile9:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:lvServiceDefinitions, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTile11:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Event handler for the MouseDoubleClick event of the lvPropath
	    Notes:
	    @param sender The reference to the object that raised the event
	    @param e The MouseEventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID lvPropath_MouseDoubleClick (sender AS System.Object,
	                                                e AS System.Windows.Forms.MouseEventArgs):
		
		DEFINE VARIABLE cAbsolutePath AS CHARACTER NO-UNDO.
		
		ASSIGN cAbsolutePath = lvPropath:ActiveItem:SubItems["absolute":U]:Value .

        IF cAbsolutePath > "":U THEN 
            WindowsExplorerHelper:ShowFileInExplorer (cAbsolutePath) .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event 
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
    
        IF StartupParameterHelper:GetStartupParameterValue ("prowcapp":U) > "":U THEN 
            button2:Visible = TRUE .
        ELSE   
            button2:Visible = FALSE .        
        
        SUPER:OnLoad (e) .

    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event 
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):
		
		SUPER:OnShown (e) .
		
		Consultingwerk.Util.UserInterfaceHelper:ProcessEvents() . 

        THIS-OBJECT:PopulatePropath () .
        THIS-OBJECT:PopulateDatabases () .
        THIS-OBJECT:PopulateSession () .
        THIS-OBJECT:PopulateStartupParameters () .
        THIS-OBJECT:PopulateInternationalization () . 
        THIS-OBJECT:PopulateProcess () .
        THIS-OBJECT:PopulateServices () .
        THIS-OBJECT:PopulateServiceDefinitions () .
        THIS-OBJECT:PopulateServers () . 

        CATCH err AS Progress.Lang.Error :
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the Databases ListView 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PopulateDatabases ():

        DEFINE VARIABLE iEntry AS INTEGER              NO-UNDO .
        DEFINE VARIABLE oEntry AS UltraListViewItem    NO-UNDO . 
        DEFINE VARIABLE oImage AS System.Drawing.Image NO-UNDO . 

        ASSIGN oImage      = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\data.png":U) .

        DO iEntry = 1 TO NUM-DBS:

            oEntry = lvDatabases:Items:Add (STRING (iEntry), LDBNAME (iEntry)) .
            oEntry:Appearance:Image = oImage . 
            oEntry:SubItems["Parameters":U]:Value = DBPARAM (iEntry) .
        END.

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Populates the Internationalization ListView 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PopulateInternationalization ():
		
        DEFINE VARIABLE iEntry      AS INTEGER              NO-UNDO .
        DEFINE VARIABLE oEntry      AS UltraListViewItem    NO-UNDO . 
        DEFINE VARIABLE cProperties AS CHARACTER            NO-UNDO.
        DEFINE VARIABLE cProperty   AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE cValue      AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE hCall       AS HANDLE               NO-UNDO .
        DEFINE VARIABLE oImage      AS System.Drawing.Image NO-UNDO . 

        oEntry = lvInternationalization:Items:Add ("1":U, ".NET UI Culture":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\earth.png":U) . 
        oEntry:SubItems["Value":U]:Value = System.Threading.Thread:CurrentThread:CurrentUICulture:Name .

        oEntry = lvInternationalization:Items:Add ("2":U, ".NET Culture":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\earth.png":U) . 
        oEntry:SubItems["Value":U]:Value = System.Threading.Thread:CurrentThread:CurrentCulture:Name .

        oEntry = lvInternationalization:Items:Add ("3":U, ".NET Decimal Separator":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\symbol_hash.png":U) . 
        oEntry:SubItems["Value":U]:Value = SUBSTITUTE ("&1 (&2)":U,
                                                       System.Threading.Thread:CurrentThread:CurrentUICulture:NumberFormat:NumberDecimalSeparator,
                                                       ASC (System.Threading.Thread:CurrentThread:CurrentCulture:NumberFormat:NumberDecimalSeparator)) .

        oEntry = lvInternationalization:Items:Add ("4":U, ".NET 1000 Separator":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\symbol_hash.png":U) . 
        oEntry:SubItems["Value":U]:Value = SUBSTITUTE ("&1 (&2)":U,
                                                       System.Threading.Thread:CurrentThread:CurrentUICulture:NumberFormat:NumberDecimalSeparator,
                                                       ASC (System.Threading.Thread:CurrentThread:CurrentCulture:NumberFormat:NumberGroupSeparator)) .

        oEntry = lvInternationalization:Items:Add ("5":U, ".NET Short Date Pattern":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\calendar_31.png":U) . 
        oEntry:SubItems["Value":U]:Value = System.Threading.Thread:CurrentThread:CurrentUICulture:DateTimeFormat:ShortDatePattern .

        oEntry = lvInternationalization:Items:Add ("6":U, "ABL Decimal Separator":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\symbol_hash.png":U) . 
        oEntry:SubItems["Value":U]:Value = SUBSTITUTE ("&1 (&2)":U,
                                                       SESSION:NUMERIC-DECIMAL-POINT,
                                                       ASC (SESSION:NUMERIC-DECIMAL-POINT)) .

        oEntry = lvInternationalization:Items:Add ("7":U, "ABL 1000 Separator":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\symbol_hash.png":U) . 
        oEntry:SubItems["Value":U]:Value = SUBSTITUTE ("&1 (&2)":U,
                                                       SESSION:NUMERIC-SEPARATOR,
                                                       ASC (SESSION:NUMERIC-SEPARATOR)) .

        oEntry = lvInternationalization:Items:Add ("8":U, "ABL Date Format":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\calendar_31.png":U) . 
        oEntry:SubItems["Value":U]:Value = SESSION:DATE-FORMAT .

        /* and now repeat the CP session properties */
        ASSIGN cProperties = LIST-QUERY-ATTRS (SESSION:HANDLE)
               oImage      = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\font.png":U) .

        loop:
        DO iEntry = 1 TO NUM-ENTRIES (cProperties) ON ERROR UNDO, THROW:

            ASSIGN cProperty = ENTRY (iEntry, cProperties) .

            IF NOT cProperty BEGINS "CP":U THEN NEXT . 
    
            IF NOT CAN-QUERY (SESSION:HANDLE, cProperty) THEN 
                NEXT . 

            DO ON ERROR UNDO, THROW:

                CREATE CALL hCall.
                
                ASSIGN hCall:IN-HANDLE = SESSION:HANDLE 
                       hCall:CALL-TYPE = GET-ATTR-CALL-TYPE  
                       hCall:CALL-NAME = cProperty . 
            
                hCall:INVOKE () NO-ERROR . 

                IF ERROR-STATUS:NUM-MESSAGES > 0 THEN 
                    NEXT loop . 

                ASSIGN cValue = hCall:RETURN-VALUE .
                
                CATCH err AS Progress.Lang.Error:
                    NEXT loop .     
                END CATCH.
            END. 

            oEntry = lvInternationalization:Items:Add (STRING (iEntry), cProperty) .
            oEntry:Appearance:Image = oImage . 
            
            IF FILE-INFO:FULL-PATHNAME > "":U THEN 
                oEntry:SubItems["Value":U]:Value = BOX (cValue) .

            FINALLY:
                Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (hCall) .       
            END FINALLY.
        END.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the Process ListView 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PopulateProcess ():
		
        DEFINE VARIABLE oEntry      AS UltraListViewItem    NO-UNDO . 

        oEntry = lvProcess:Items:Add ("1":U, "PID":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\cpu.png":U) . 
        oEntry:SubItems["Value":U]:Value = System.Diagnostics.Process:GetCurrentProcess():Id .

        oEntry = lvProcess:Items:Add ("2":U, "Start Time":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\cpu.png":U) . 
        oEntry:SubItems["Value":U]:Value = STRING (System.Diagnostics.Process:GetCurrentProcess():StartTime, "99.99.9999 hh:mm:ss":U) .

        oEntry = lvProcess:Items:Add ("3":U, "Executable":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\window.png":U) . 
        oEntry:SubItems["Value":U]:Value = SessionHelper:ExecutablePath() .		

        oEntry = lvProcess:Items:Add ("4":U, "Command Line":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\window.png":U) . 
        oEntry:SubItems["Value":U]:Value = System.Environment:CommandLine .     
		
        oEntry = lvProcess:Items:Add ("5":U, "Working Directory":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\folder_document.png":U) . 
        oEntry:SubItems["Value":U]:Value = System.IO.Directory:GetCurrentDirectory() .     

        oEntry = lvProcess:Items:Add ("6":U, "Assemblies":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\cubes.png":U) . 
        oEntry:SubItems["Value":U]:Value = SessionHelper:AssembliesDirectory .     

        oEntry = lvProcess:Items:Add ("7":U, "Assemblies (full path)":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\cubes.png":U) . 
        oEntry:SubItems["Value":U]:Value = SessionHelper:AssembliesFullPath .     

        oEntry = lvProcess:Items:Add ("10":U, "OpenEdge Version":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\dlc.png":U) . 
        oEntry:SubItems["Value":U]:Value = SessionHelper:FullVersionString() .     

        oEntry = lvProcess:Items:Add ("11":U, "DLC":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\dlc.png":U) . 
        oEntry:SubItems["Value":U]:Value = SessionHelper:DLC .     

        oEntry = lvProcess:Items:Add ("12":U, "PROMSGS":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\earth.png":U) . 
        oEntry:SubItems["Value":U]:Value = SessionHelper:PROMSGS .     

        oEntry = lvProcess:Items:Add ("13":U, "Compiler available":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\dlc.png":U) . 
        oEntry:SubItems["Value":U]:Value = SessionHelper:AllowsCompile() .     

        oEntry = lvProcess:Items:Add ("14":U, "Framework Version":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\cubes.png":U) . 
        oEntry:SubItems["Value":U]:Value = (NEW FrameworkVersion()):GetVersionString () .     

        oEntry = lvProcess:Items:Add ("15":U, "32/64 bit Process":U) .
        oEntry:Appearance:Image = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\cpu.png":U) . 
        oEntry:SubItems["Value":U]:Value = SUBSTITUTE ("&1 bit":U, SessionHelper:ProcessBitness()) .     

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the Propath ListView 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PopulatePropath ():
		
        DEFINE VARIABLE cPropathOrig AS CHARACTER            NO-UNDO.
        DEFINE VARIABLE cPropath     AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE cEntry       AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE iEntry       AS INTEGER              NO-UNDO .
        DEFINE VARIABLE oEntry       AS UltraListViewItem    NO-UNDO . 
        DEFINE VARIABLE oImage1      AS System.Drawing.Image NO-UNDO . 
        DEFINE VARIABLE oImage2      AS System.Drawing.Image NO-UNDO . 
        DEFINE VARIABLE oImage3      AS System.Drawing.Image NO-UNDO . 

        ASSIGN oImage1      = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\folder_document.png":U) 
               oImage2      = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\books_blue.png":U) 
               oImage3      = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\error.png":U) 
        
               cPropathOrig = PROPATH 
               cPropath     = REPLACE (PROPATH, "/":U, "~\":U) .
               
        DO ON ERROR UNDO, THROW: 
            PROPATH      = ".":U .
            
            CATCH err AS Progress.Lang.SysError:
            	/* Ignore error 2311 */
            	IF err:GetMessageNum (1) <> 2311 THEN 
            	    UNDO, THROW err . 	
            END CATCH.
        END.
    
        DO iEntry = 1 TO NUM-ENTRIES (cPropath):

            ASSIGN cEntry              = ENTRY (iEntry, cPropath) 
                   FILE-INFO:FILE-NAME = cEntry .

            oEntry = lvPropath:Items:Add (STRING (iEntry), cEntry) .
            
            IF FILE-INFO:FILE-TYPE MATCHES "*D*":U THEN 
                oEntry:Appearance:Image = oImage1 .
            ELSE IF FILE-INFO:FILE-TYPE MATCHES "*F*":U THEN 
                oEntry:Appearance:Image = oImage2 .
            ELSE 
                oEntry:Appearance:Image = oImage3 .
            
            IF FILE-INFO:FULL-PATHNAME > "":U THEN 
                oEntry:SubItems["absolute":U]:Value = FILE-INFO:FULL-PATHNAME .
        END.

        FINALLY:
            PROPATH = cPropathOrig .    
            
            CATCH err AS Progress.Lang.SysError:
                /* Ignore error 2311 */
                IF err:GetMessageNum (1) <> 2311 THEN 
                    UNDO, THROW err .   
            END CATCH.
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the Servers List View  
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PopulateServers ():
		
        DEFINE VARIABLE hServer AS HANDLE               NO-UNDO .
        DEFINE VARIABLE oEntry  AS UltraListViewItem    NO-UNDO . 
        DEFINE VARIABLE oImage  AS System.Drawing.Image NO-UNDO . 

        ASSIGN oImage  = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\server.png":U) 
               hServer = SESSION:FIRST-SERVER .

        DO WHILE VALID-HANDLE (hServer) ON ERROR UNDO, THROW:

            oEntry = lvServers:Items:Add (STRING (hServer), hServer:SUBTYPE) .
            oEntry:Appearance:Image = oImage . 
            oEntry:SubItems["Name":U]:Value = hServer:NAME .
            
            FINALLY:
                IF VALID-HANDLE (hServer) THEN 
                    hServer = hServer:NEXT-SIBLING .		
            END FINALLY.
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the Service Definitions List View
        Notes:   The List View is populated by Querying the IServiceDefinitions 
                 implementation (ServiceContainer, which is in fact an extension
                 fo the CharacterList type) 
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PopulateServiceDefinitions ():
		
        DEFINE VARIABLE oImage       AS System.Drawing.Image NO-UNDO . 
        DEFINE VARIABLE oEntry       AS UltraListViewItem    NO-UNDO . 
        DEFINE VARIABLE oDefinitions AS IServiceDefinitions  NO-UNDO .

        oDefinitions = {Consultingwerk/get-service.i Consultingwerk.Framework.IServiceDefinitions} .

        IF NOT VALID-OBJECT (oDefinitions) THEN 
            RETURN . 

        ASSIGN oImage      = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\document_gear.png":U) .

        {Consultingwerk/foreachPrimitiveList.i Character cFileName in oDefinitions}

            FILE-INFO:FILE-NAME = cFileName . 
            
            oEntry = lvServiceDefinitions:Items:Add (cFileName, cFileName) .
            oEntry:Appearance:Image = oImage . 
            oEntry:SubItems ["Absolute":U]:Value = FILE-INFO:FULL-PATHNAME .

        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the Services List View 
        Notes:   Relies on the IServiceContainerDebugging interface which must be 
                 implemented by the active ServiceContainer. This interface allows 
                 to request a temp-table with all the registered services
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PopulateServices ():
		
        DEFINE VARIABLE oServiceContainer AS IServiceContainerDebugging NO-UNDO .
        DEFINE VARIABLE oEntry            AS UltraListViewItem          NO-UNDO . 
        DEFINE VARIABLE oImage            AS System.Drawing.Image       NO-UNDO . 

        IF NOT VALID-OBJECT (FrameworkSettings:ServiceContainer) THEN 
            RETURN . 
            
        IF NOT TYPE-OF (FrameworkSettings:ServiceContainer, IServiceContainerDebugging) THEN
            RETURN .  

        oServiceContainer = CAST (FrameworkSettings:ServiceContainer, IServiceContainerDebugging) .

        oServiceContainer:GetServiceTable (OUTPUT TABLE ttServiceTable) .

        ASSIGN oImage      = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\wrench.png":U) .

        FOR EACH ttServiceTable:
            oEntry = lvServices:Items:Add (ttServiceTable.ServiceType, ttServiceTable.ServiceType) .
            oEntry:Appearance:Image = oImage . 
            oEntry:SubItems ["Instance":U]:Value = ttServiceTable.ServiceInstance .
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the Session Properties ListView 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PopulateSession ():
		
        DEFINE VARIABLE cProperties AS CHARACTER         NO-UNDO.
        DEFINE VARIABLE cProperty   AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE iEntry      AS INTEGER           NO-UNDO .
        DEFINE VARIABLE oEntry      AS UltraListViewItem NO-UNDO . 
        DEFINE VARIABLE cValue      AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE hCall       AS HANDLE            NO-UNDO .
        DEFINE VARIABLE oImage      AS System.Drawing.Image NO-UNDO . 

        ASSIGN cProperties = LIST-QUERY-ATTRS (SESSION:HANDLE)
               oImage      = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\workstation.png":U) .

        loop:
        DO iEntry = 1 TO NUM-ENTRIES (cProperties) ON ERROR UNDO, THROW:

            ASSIGN cProperty = ENTRY (iEntry, cProperties) .

            IF NOT CAN-QUERY (SESSION:HANDLE, cProperty) THEN 
                NEXT . 

            DO ON ERROR UNDO, THROW:
                CREATE CALL hCall.
                
                ASSIGN hCall:IN-HANDLE = SESSION:HANDLE 
                       hCall:CALL-TYPE = GET-ATTR-CALL-TYPE  
                       hCall:CALL-NAME = cProperty . 
            
                hCall:INVOKE () NO-ERROR . 

                IF ERROR-STATUS:NUM-MESSAGES > 0 THEN 
                    NEXT loop . 

                ASSIGN cValue = hCall:RETURN-VALUE .
                
                CATCH err AS Progress.Lang.Error:
                	NEXT loop . 	
                END CATCH.
                
                FINALLY:
                    Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (hCall) . 		
                END FINALLY.
            END. 

            oEntry = lvSessionProperties:Items:Add (STRING (iEntry), cProperty) .
            oEntry:Appearance:Image = oImage . 
            
            IF FILE-INFO:FULL-PATHNAME > "":U THEN 
                oEntry:SubItems["Value":U]:Value = BOX (cValue) .
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the Startup Parameters ListView 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PopulateStartupParameters ():
		
        DEFINE VARIABLE cParameters AS CHARACTER            NO-UNDO EXTENT .
        DEFINE VARIABLE cParameter  AS CHARACTER            NO-UNDO.
        DEFINE VARIABLE cValue      AS CHARACTER            NO-UNDO.
        DEFINE VARIABLE iEntry      AS INTEGER              NO-UNDO .
        DEFINE VARIABLE oEntry      AS UltraListViewItem    NO-UNDO . 
        DEFINE VARIABLE oImage      AS System.Drawing.Image NO-UNDO . 

        ASSIGN cParameters = StartupParameterHelper:StartupParameterList
               oImage      = ImageHelper:Load ("Consultingwerk\Windows\Session\Images\gear.png":U) .

        DO iEntry = 1 TO EXTENT (cParameters):

            ASSIGN cParameter = cParameters [iEntry] .
            oEntry = lvStartupParameter:Items:Add (STRING (iEntry), cParameter) .
            oEntry:Appearance:Image = oImage . 
            oEntry:SubItems["Value":U]:Value = StartupParameterHelper:GetStartupParameterValue (cParameter) .
        END.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Opens the selected service definition file using the default application 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ServiceDefinitionsOpenFile ():
		
        DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.

        IF lvServiceDefinitions:SelectedItems:Count > 0 THEN DO:
            cFile = lvServiceDefinitions:SelectedItems[0]:SubItems["Absolute":U]:Text . 

            Consultingwerk.Windows.API.Win32:ShellExecute ("open":U, cFile) .            
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shows the selected service definition file in the Windows Explorer 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ServiceDefinitionsShowInExplorer ():
		
		DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.

        IF lvServiceDefinitions:SelectedItems:Count > 0 THEN DO:
            cFile = lvServiceDefinitions:SelectedItems[0]:SubItems["Absolute":U]:Text . 
            
            Consultingwerk.Util.WindowsExplorerHelper:ShowFileInExplorer (cFile) .
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Creates a new instance of the SessionInfoForm an Shows it as an Dialog 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID ShowSessionInformation ():
		
		DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO . 
		
		oForm = NEW SessionInfoForm () .
		
		WAIT-FOR oForm:ShowDialog () .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the UltraToolbarsManager 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraToolbarsManager1_ToolClick (sender AS System.Object, 
	                                                     e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):
		
		CASE e:Tool:Key:
		    
		    WHEN "Windows Explorer":U THEN 
		        THIS-OBJECT:ServiceDefinitionsShowInExplorer () . 
		    WHEN "Open File":U THEN 
		        THIS-OBJECT:ServiceDefinitionsOpenFile () . 
		    
		END CASE . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SessionInfoForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SessionInfoForm ():

        GarbageCollectorHelper:DeleteObject (oFullWindowDragManager1) .

    END DESTRUCTOR .

END CLASS.
