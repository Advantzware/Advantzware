/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartTabFolderForm
    Purpose     : Base Class for SmartTabFolderFrom based controls 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Apr 20 16:55:18 CEST 2015
    Notes       : SCL-782
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*                FROM PROPATH .
USING Consultingwerk.SmartComponents.BaseForms.TabFolder.* FROM PROPATH . 
USING Consultingwerk.SmartComponents.Interfaces.*          FROM PROPATH .
USING Consultingwerk.Util.*                                FROM PROPATH .
USING Infragistics.Win.Misc.*                              FROM ASSEMBLY .
USING Infragistics.Win.UltraWinTabControl.*                FROM ASSEMBLY .
USING Progress.Lang.*                                      FROM PROPATH .
USING System.Windows.Forms.*                               FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.BaseForms.TabFolder.SmartTabFolderForm 
    INHERITS SmartWindowForm: 
	
    DEFINE PRIVATE VARIABLE m_SmartTabFolderForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartTabFolderForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartTabFolderForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartTabFolderForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1 AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.
    DEFINE PRIVATE VARIABLE SmartTabFolderForm_Fill_Panel AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartTabFolderForm class 
        Notes:
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SmartTabFolderForm ():
		
        SUPER().

        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds an additional ISmartTabFolderPage based Control to an existing 
                 Tabfolder Page
        Notes:   The tab page with the given key must already exist
        @param poTabFolderPage The reference to the ISmartTabFolderPage to add to the tab page 
        @param pcPageKey  The key of the tab folder (see CreateTabPage) to add the control to
        @param pcTabKey  The key for the ISmartTabFolder Page 
        @param poDataSource  The reference to the ISmartDataSource to pass through to the ISmartTabFolderPage
        @param poTableIOSource The reference to the ISmartTableIOSource to pass through to the ISmartTabFolderPage
        @param poNavigationSource The reference to the ISmartNavigationSource to pass through to the ISmartTabFolderPage
        @return The reference to the added ISmartTabFolderPage (fluid coding style)
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ISmartTabFolderPage AddToTabPage (poTabFolderPage AS ISmartTabFolderPage,
                                                       pcPageKey AS CHARACTER,
                                                       pcTabKey AS CHARACTER,
                                                       poDataSource AS ISmartDataSource,
                                                       poTableIOSource AS ISmartTableIOSource,
                                                       poNavigationSource AS ISmartNavigationSource):
        
        DEFINE VARIABLE oControl AS System.Windows.Forms.Control NO-UNDO .
        DEFINE VARIABLE oTab     AS UltraTab                     NO-UNDO . 
        
        IF NOT THIS-OBJECT:ultraTabControl1:Tabs:Exists (pcPageKey) THEN 
            UNDO, THROW NEW AppError ("Invalid tab page key.", 0) .
        
        oTab = THIS-OBJECT:ultraTabControl1:Tabs [pcPageKey] .

        oControl = NEW UltraSplitter () . 
        oControl:Dock = DockStyle:Bottom . 
        
        oTab:TabPage:Controls:Add (oControl) .

        oControl = CAST (CAST (poTabFolderPage, Progress.Lang.Object), System.Windows.Forms.Control) .
        
        oControl:Dock = DockStyle:Bottom .
        oControl:Name = pcTabKey .
        
        oTab:TabPage:Controls:Add (oControl) .
        
        poTabFolderPage:InitializeSmartTabFolderPage (poDataSource, 
                                                      poTableIOSource, 
                                                      poNavigationSource) .
                
        RETURN poTabFolderPage .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds an additional ISmartTabFolderPage based Control to an existing 
                 Tabfolder Page
        Notes:   The tab page with the given key must already exist
                 Links to the default SmartToolbarController instance for TableIO and Navigation Links
        @param poTabFolderPage The reference to the ISmartTabFolderPage to add to the tab page 
        @param pcPageKey  The key of the tab folder (see CreateTabPage) to add the control to
        @param pcTabKey  The key for the ISmartTabFolder Page 
        @param poDataSource  The reference to the ISmartDataSource to pass through to the ISmartTabFolderPage
        @return The reference to the added ISmartTabFolderPage (fluid coding style)
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ISmartTabFolderPage AddToTabPage (poTabFolderPage AS ISmartTabFolderPage,
                                                       pcPageKey AS CHARACTER,
                                                       pcTabKey AS CHARACTER,
                                                       poDataSource AS ISmartDataSource):
        
        RETURN THIS-OBJECT:AddToTabPage (poTabFolderPage,
                                         pcPageKey,
                                         pcTabKey,
                                         poDataSource,
                                         THIS-OBJECT:smartToolbarController1,
                                         THIS-OBJECT:smartToolbarController1) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a new Tabfolder page and adds the provided ISmartTabFolderPage 
                 based Control 
        Notes:   
        @param poTabFolderPage The reference to the ISmartTabFolderPage to add to the tab page 
        @param pcTabLabel  The label to assign to the new Tabfolder page
        @param pcTabKey  The key used for the ISmartTabFolder Page and the Control 
        @param poDataSource  The reference to the ISmartDataSource to pass through to the ISmartTabFolderPage
        @param poTableIOSource The reference to the ISmartTableIOSource to pass through to the ISmartTabFolderPage
        @param poNavigationSource The reference to the ISmartNavigationSource to pass through to the ISmartTabFolderPage
        @return The reference to the added ISmartTabFolderPage (fluid coding style)
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED ISmartTabFolderPage CreateTabPage (poTabFolderPage AS ISmartTabFolderPage,
            	                                        pcTabLabel AS CHARACTER,
            	                                        pcTabKey AS CHARACTER,
            	                                        poDataSource AS ISmartDataSource,
            	                                        poTableIOSource AS ISmartTableIOSource,
            	                                        poNavigationSource AS ISmartNavigationSource):
		
        DEFINE VARIABLE oControl AS System.Windows.Forms.Control NO-UNDO .
        DEFINE VARIABLE oTab     AS UltraTab                     NO-UNDO . 
		
		oControl = CAST (CAST (poTabFolderPage, Progress.Lang.Object), System.Windows.Forms.Control) .
		
		oTab = THIS-OBJECT:ultraTabControl1:Tabs:Add (pcTabKey, pcTabLabel) .

        oControl:Dock = DockStyle:Fill .
        oControl:Name = pcTabKey .
        
        oTab:TabPage:Controls:Add (oControl) .
        
        poTabFolderPage:InitializeSmartTabFolderPage (poDataSource, 
                                                     poTableIOSource, 
                                                     poNavigationSource) .
        
        RETURN poTabFolderPage .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a new Tabfolder page and adds the provided ISmartTabFolderPage 
                 based Control 
        Notes:   Links to the default SmartToolbarController instance for TableIO and Navigation Links
        @param poTabFolderPage The reference to the ISmartTabFolderPage to add to the tab page 
        @param pcTabLabel  The label to assign to the new Tabfolder page
        @param pcTabKey  The key used for the ISmartTabFolder Page and the Control 
        @param poDataSource  The reference to the ISmartDataSource to pass through to the ISmartTabFolderPage
        @return The reference to the added ISmartTabFolderPage (fluid coding style)
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ISmartTabFolderPage CreateTabPage (poTabFolderPage AS ISmartTabFolderPage,
                                         pcTabLabel AS CHARACTER,
                                         pcTabKey AS CHARACTER,
                                         poDataSource AS ISmartDataSource):
        
        RETURN THIS-OBJECT:CreateTabPage (poTabFolderPage,
                                          pcTabLabel,
                                          pcTabKey,
                                          poDataSource,
                                          THIS-OBJECT:smartToolbarController1,
                                          THIS-OBJECT:smartToolbarController1) .

    END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Returns the reference to the ISmartTabFolderPage Control with the 
	             given key
	    Notes:
	    @param pcTabKey The key of the ISmartTabFolderPage to return
	    @return The reference to the ISmartTabFolderPage control or ? 
	------------------------------------------------------------------------------*/
	METHOD PUBLIC ISmartTabFolderPage GetSmartTabFolderPage (pcTabKey AS CHARACTER):
		
        {Consultingwerk/foreach.i UltraTab oTab in THIS-OBJECT:ultraTabControl1:Tabs}

            {Consultingwerk/foreach.i System.Windows.Forms.Control oControl in oTab:TabPage:Controls}
            
                IF oControl:Name = pcTabKey AND TYPE-OF (oControl, ISmartTabFolderPage) THEN 
                    RETURN CAST (oControl, ISmartTabFolderPage) . 
            END.
        END.

	    RETURN ? . 
		
	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initilaizes the visual design
        Notes:
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID InitializeComponent(  ):
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool48 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool48 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool49 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool49 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool50 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool50 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool51 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool51 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool52 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool52 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool53 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool53 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool54 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool54 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("start":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("navigation":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool55 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool55 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool56 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool56 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool57 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool57 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool58 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool58 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("maintenance":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool59 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool59 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool60 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool60 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool61 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool61 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool62 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool62 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool63 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool63 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool64 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool64 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("transaction":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool65 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool65 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool66 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool66 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool67 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool67 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool68 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool68 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool69 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool69 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("NavigationToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool70 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool70 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool71 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool71 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool72 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool72 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool73 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool73 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TableIOToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool74 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool74 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool75 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool75 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool76 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool76 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool77 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool77 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool78 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool78 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool79 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool79 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar3 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar3 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TransactionToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool80 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool80 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool81 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool81 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool82 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool82 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.SmartComponents.BaseForms.TabFolder.SmartTabFolderForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool83 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool83 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool84 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool84 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool85 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool85 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool86 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool86 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool87 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool87 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool88 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool88 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool89 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool89 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool90 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool90 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool91 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool91 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool92 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool92 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool93 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool93 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool94 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool94 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:SmartTabFolderForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SmartTabFolderForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:SmartTabFolderForm_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar0[1] = buttonTool48.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:FooterToolbar:NonInheritedTools:AddRange(arrayvar0).
        buttonTool52:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar1[1] = buttonTool49.
        arrayvar1[2] = buttonTool50.
        arrayvar1[3] = buttonTool51.
        arrayvar1[4] = buttonTool52.
        arrayvar1[5] = buttonTool53.
        arrayvar1[6] = buttonTool54.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar1).
        ribbonTab1:Caption = "Start":U.
        ribbonGroup1:Caption = "Navigation":U.
        buttonTool55:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool55:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool56:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool56:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool57:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool57:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool58:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool58:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar2[1] = buttonTool55.
        arrayvar2[2] = buttonTool56.
        arrayvar2[3] = buttonTool57.
        arrayvar2[4] = buttonTool58.
        ribbonGroup1:Tools:AddRange(arrayvar2).
        ribbonGroup2:Caption = "Maintenance":U.
        buttonTool59:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool59:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool60:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool60:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool61:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool61:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool62:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool63:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool64:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar3[1] = buttonTool59.
        arrayvar3[2] = buttonTool60.
        arrayvar3[3] = buttonTool61.
        arrayvar3[4] = buttonTool62.
        arrayvar3[5] = buttonTool63.
        arrayvar3[6] = buttonTool64.
        ribbonGroup2:Tools:AddRange(arrayvar3).
        ribbonGroup3:Caption = "Transaction":U.
        buttonTool65:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool66:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar4[1] = buttonTool65.
        arrayvar4[2] = buttonTool66.
        ribbonGroup3:Tools:AddRange(arrayvar4).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 3 NO-UNDO.
        arrayvar5[1] = ribbonGroup1.
        arrayvar5[2] = ribbonGroup2.
        arrayvar5[3] = ribbonGroup3.
        ribbonTab1:Groups:AddRange(arrayvar5).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar6[1] = ribbonTab1.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar6).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar7[1] = buttonTool67.
        arrayvar7[2] = buttonTool68.
        arrayvar7[3] = buttonTool69.
        THIS-OBJECT:smartToolbarController1:Ribbon:QuickAccessToolbar:NonInheritedTools:AddRange(arrayvar7).
        THIS-OBJECT:smartToolbarController1:Ribbon:Visible = TRUE.
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        THIS-OBJECT:smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        buttonTool70:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar8[1] = buttonTool70.
        arrayvar8[2] = buttonTool71.
        arrayvar8[3] = buttonTool72.
        arrayvar8[4] = buttonTool73.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar8).
        ultraToolbar1:Text = "Navigation Toolbar":U.
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
        buttonTool74:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar9[1] = buttonTool74.
        arrayvar9[2] = buttonTool75.
        arrayvar9[3] = buttonTool76.
        arrayvar9[4] = buttonTool77.
        arrayvar9[5] = buttonTool78.
        arrayvar9[6] = buttonTool79.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar9).
        ultraToolbar2:Text = "TableIO Toolbar":U.
        ultraToolbar3:DockedColumn = 1.
        ultraToolbar3:DockedRow = 1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar10[1] = buttonTool80.
        arrayvar10[2] = buttonTool81.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar10).
        ultraToolbar3:Text = "Transaction Toolbar":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 3 NO-UNDO.
        arrayvar11[1] = ultraToolbar1.
        arrayvar11[2] = ultraToolbar2.
        arrayvar11[3] = ultraToolbar3.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar11).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        buttonTool82:SharedPropsInternal:AppearancesSmall:Appearance = appearance1.
        buttonTool82:SharedPropsInternal:Caption = "First record":U.
        buttonTool82:SharedPropsInternal:CustomizerCaption = "First":U.
        buttonTool82:SharedPropsInternal:Enabled = FALSE.
        buttonTool82:SharedPropsInternal:ToolTipText = "First record":U.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool83:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
        buttonTool83:SharedPropsInternal:Caption = "Previous record":U.
        buttonTool83:SharedPropsInternal:CustomizerCaption = "Previous":U.
        buttonTool83:SharedPropsInternal:Enabled = FALSE.
        buttonTool83:SharedPropsInternal:ToolTipText = "Previous record":U.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        buttonTool84:SharedPropsInternal:AppearancesSmall:Appearance = appearance3.
        buttonTool84:SharedPropsInternal:Caption = "Next record":U.
        buttonTool84:SharedPropsInternal:CustomizerCaption = "Next":U.
        buttonTool84:SharedPropsInternal:Enabled = FALSE.
        buttonTool84:SharedPropsInternal:ToolTipText = "Next record":U.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        buttonTool85:SharedPropsInternal:AppearancesSmall:Appearance = appearance4.
        buttonTool85:SharedPropsInternal:Caption = "Last record":U.
        buttonTool85:SharedPropsInternal:CustomizerCaption = "Last":U.
        buttonTool85:SharedPropsInternal:Enabled = FALSE.
        buttonTool85:SharedPropsInternal:ToolTipText = "Last record":U.
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        buttonTool86:SharedPropsInternal:AppearancesSmall:Appearance = appearance5.
        buttonTool86:SharedPropsInternal:Caption = "Add record":U.
        buttonTool86:SharedPropsInternal:CustomizerCaption = "Add":U.
        buttonTool86:SharedPropsInternal:Enabled = FALSE.
        buttonTool86:SharedPropsInternal:ToolTipText = "Add record":U.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        buttonTool87:SharedPropsInternal:AppearancesSmall:Appearance = appearance6.
        buttonTool87:SharedPropsInternal:Caption = "Copy record":U.
        buttonTool87:SharedPropsInternal:CustomizerCaption = "Copy":U.
        buttonTool87:SharedPropsInternal:Enabled = FALSE.
        buttonTool87:SharedPropsInternal:ToolTipText = "Copy record":U.
        appearance7:Image = CAST(resources:GetObject("appearance7.Image":U), System.Object).
        buttonTool88:SharedPropsInternal:AppearancesSmall:Appearance = appearance7.
        buttonTool88:SharedPropsInternal:Caption = "Update record":U.
        buttonTool88:SharedPropsInternal:CustomizerCaption = "Update":U.
        buttonTool88:SharedPropsInternal:Enabled = FALSE.
        buttonTool88:SharedPropsInternal:ToolTipText = "Update record":U.
        appearance8:Image = CAST(resources:GetObject("appearance8.Image":U), System.Object).
        buttonTool89:SharedPropsInternal:AppearancesSmall:Appearance = appearance8.
        buttonTool89:SharedPropsInternal:Caption = "Save changes":U.
        buttonTool89:SharedPropsInternal:CustomizerCaption = "Save":U.
        buttonTool89:SharedPropsInternal:Enabled = FALSE.
        buttonTool89:SharedPropsInternal:ToolTipText = "Save changes":U.
        appearance9:Image = CAST(resources:GetObject("appearance9.Image":U), System.Object).
        buttonTool90:SharedPropsInternal:AppearancesSmall:Appearance = appearance9.
        buttonTool90:SharedPropsInternal:Caption = "Cancel update":U.
        buttonTool90:SharedPropsInternal:CustomizerCaption = "Cancel":U.
        buttonTool90:SharedPropsInternal:Enabled = FALSE.
        buttonTool90:SharedPropsInternal:ToolTipText = "Cancel update":U.
        appearance10:Image = CAST(resources:GetObject("appearance10.Image":U), System.Object).
        buttonTool91:SharedPropsInternal:AppearancesSmall:Appearance = appearance10.
        buttonTool91:SharedPropsInternal:Caption = "Delete record":U.
        buttonTool91:SharedPropsInternal:CustomizerCaption = "Delete":U.
        buttonTool91:SharedPropsInternal:Enabled = FALSE.
        buttonTool91:SharedPropsInternal:ToolTipText = "Delete record":U.
        appearance11:Image = CAST(resources:GetObject("appearance11.Image":U), System.Object).
        buttonTool92:SharedPropsInternal:AppearancesSmall:Appearance = appearance11.
        buttonTool92:SharedPropsInternal:Caption = "&Exit":U.
        buttonTool92:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance12:Image = CAST(resources:GetObject("appearance12.Image":U), System.Object).
        buttonTool93:SharedPropsInternal:AppearancesLarge:Appearance = appearance12.
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        buttonTool93:SharedPropsInternal:AppearancesSmall:Appearance = appearance13.
        buttonTool93:SharedPropsInternal:Caption = "Commit":U.
        buttonTool93:SharedPropsInternal:CustomizerCaption = "Commit":U.
        buttonTool93:SharedPropsInternal:Enabled = FALSE.
        buttonTool93:SharedPropsInternal:ToolTipText = "Commit transaction":U.
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        buttonTool94:SharedPropsInternal:AppearancesLarge:Appearance = appearance14.
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        buttonTool94:SharedPropsInternal:AppearancesSmall:Appearance = appearance15.
        buttonTool94:SharedPropsInternal:Caption = "Revert":U.
        buttonTool94:SharedPropsInternal:CustomizerCaption = "Revert":U.
        buttonTool94:SharedPropsInternal:Enabled = FALSE.
        buttonTool94:SharedPropsInternal:ToolTipText = "Revert transaction":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 13 NO-UNDO.
        arrayvar12[1] = buttonTool82.
        arrayvar12[2] = buttonTool83.
        arrayvar12[3] = buttonTool84.
        arrayvar12[4] = buttonTool85.
        arrayvar12[5] = buttonTool86.
        arrayvar12[6] = buttonTool87.
        arrayvar12[7] = buttonTool88.
        arrayvar12[8] = buttonTool89.
        arrayvar12[9] = buttonTool90.
        arrayvar12[10] = buttonTool91.
        arrayvar12[11] = buttonTool92.
        arrayvar12[12] = buttonTool93.
        arrayvar12[13] = buttonTool94.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar12).
        /*  */
        /* SmartTabFolderForm_Fill_Panel */
        /*  */
        /*  */
        /* SmartTabFolderForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:SmartTabFolderForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:SmartTabFolderForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:SmartTabFolderForm_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:SmartTabFolderForm_Fill_Panel:Location = NEW System.Drawing.Point(8, 151).
        THIS-OBJECT:SmartTabFolderForm_Fill_Panel:Name = "SmartTabFolderForm_Fill_Panel":U.
        THIS-OBJECT:SmartTabFolderForm_Fill_Panel:Size = NEW System.Drawing.Size(276, 107).
        THIS-OBJECT:SmartTabFolderForm_Fill_Panel:TabIndex = 0.
        /*  */
        /* ultraTabControl1 */
        /*  */
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(276, 107).
        THIS-OBJECT:ultraTabControl1:TabIndex = 5.
        THIS-OBJECT:ultraTabControl1:ViewStyle = Infragistics.Win.UltraWinTabControl.ViewStyle:Office2007.
        THIS-OBJECT:ultraTabControl1:SelectedTabChanged:Subscribe(THIS-OBJECT:ultraTabControl1_SelectedTabChanged).
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(1, 20).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(274, 86).
        /*  */
        /* m_SmartTabFolderForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 151).
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Left:Name = "m_SmartTabFolderForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(8, 107).
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartTabFolderForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(284, 151).
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Right:Name = "m_SmartTabFolderForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(8, 107).
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartTabFolderForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Top:Name = "m_SmartTabFolderForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(292, 151).
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartTabFolderForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 258).
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Bottom:Name = "m_SmartTabFolderForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(292, 8).
        THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* SmartTabFolderForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(292, 266).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:SmartTabFolderForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartTabFolderForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Name = "SmartTabFolderForm":U.
        THIS-OBJECT:Text = "SmartTabFolderForm":U.
        THIS-OBJECT:WindowPositionRegistryKey = "SmartTabFolderForm":U.
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:SmartTabFolderForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:SmartTabFolderForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Event handler for the SelectedTabChanged event of the ultraTabControl1 
	    Notes:
	    @param sender The reference to the object that raised the event
	    @param e The SelectedTabChangedEventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraTabControl1_SelectedTabChanged (sender AS System.Object, 
	                                                         e AS SelectedTabChangedEventArgs):
		
		DEFINE VARIABLE oFramesetPage AS ISmartTabFolderPage NO-UNDO . 
		
		oFramesetPage = THIS-OBJECT:GetSmartTabFolderPage (e:Tab:Key) .

        IF VALID-OBJECT (oFramesetPage) THEN 
            oFramesetPage:Activate() .

        CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartTabFolderForm class 
        Notes:
    ------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC SmartTabFolderForm ():

	END DESTRUCTOR.

END CLASS.
