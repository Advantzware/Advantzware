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
    File        : ManageClassPathForm
    Purpose     : Provides an user interface for maintain the .classpath file
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Apr 18 07:47:02 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.SmartWindowForm   FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*                 FROM PROPATH .
USING Progress.Lang.*                                       FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Tools.ManageClassPath.ManageClassPathForm INHERITS SmartWindowForm: 
    
    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE folderBrowserDialog1 AS System.Windows.Forms.FolderBrowserDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ManageClassPathForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ManageClassPathForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ManageClassPathForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_ManageClassPathForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE saveFileDialog1 AS System.Windows.Forms.SaveFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1 AS System.Windows.Forms.OpenFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE smartTempTableAdapter1 AS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1 AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.
    DEFINE PRIVATE VARIABLE ManageClassPathForm_Fill_Panel AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartUpdatableBrowser1 AS Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser NO-UNDO.

    {Consultingwerk/Util/TempTables/ttClassPath.i &ACCESS="PRIVATE"}
        
    DEFINE PRIVATE VARIABLE cBaseTitle AS CHARACTER NO-UNDO .         
        
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the file currently open.                                                                         
        Notes:                                                                            
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO INIT ?
    GET.
    PROTECTED SET (arg AS CHARACTER):
        
        THIS-OBJECT:FileName = arg .
                
        THIS-OBJECT:Text = cBaseTitle + " - ":U + arg .                  
                
    END SET.
     
    /*------------------------------------------------------------------------------
        Purpose: Default constructor of the ManageClassPathForm class                                                                         
        Notes:                                                                            
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ManageClassPathForm ():
        
        
        SUPER().
        InitializeComponent().
        
        ASSIGN cBaseTitle = THIS-OBJECT:Text .
        
        smartTempTableAdapter1:TempTableHandle = TEMP-TABLE ttClassPath:HANDLE . 
        smartTempTableAdapter1:RetrieveData () .
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Checks for the update state of the smartUpdatableBrowser                                                                         
        Notes:   Cancels when fields are just enabled. Throws an error when the user
                 is performing an update.                                                                         
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CheckForUpdate ():
        
        IF smartUpdatableBrowser1:SmartTableIOState = TableIOStateEnum:FieldsEnabled THEN 
            smartUpdatableBrowser1:CancelUpdate() .                         

        IF smartUpdatableBrowser1:SmartTableIOState = TableIOStateEnum:ModifyingData THEN 
            UNDO, THROW NEW AppError ("Unable to open or save file while still in update.", 0) . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeComponent(  ):
        
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool48 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool48 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool51 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool51 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool50 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool50 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SaveAs").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("start").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("fileGroup").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool42 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool42 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool43 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool43 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool44 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool44 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SaveAs").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("maintenance").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool15 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool15 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool16 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool16 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool17 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool17 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("navigation").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool20 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool20 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("NavigationToolbar").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool21 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool21 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool22 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool22 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool23 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool23 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool24 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool24 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TableIOToolbar").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool25 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool25 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool26 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool26 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool27 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool27 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool28 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool28 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool29 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool29 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool30 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool30 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool31 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool31 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool32 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool32 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool33 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool33 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool34 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool34 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool35 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool35 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool36 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool36 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool37 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool37 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
        appearance27 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool38 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool38 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
        appearance28 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool39 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool39 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
        appearance29 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool40 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool40 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
        appearance30 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool41 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool41 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
        appearance31 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool45 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool45 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Open").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance37 AS Infragistics.Win.Appearance NO-UNDO.
        appearance37 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance36 AS Infragistics.Win.Appearance NO-UNDO.
        appearance36 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool46 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool46 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Save").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
        appearance32 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
        appearance33 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE buttonTool47 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool47 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SaveAs").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
        appearance34 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance35 AS Infragistics.Win.Appearance NO-UNDO.
        appearance35 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ttClassPath", -1).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("Directory").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("Prefix").
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
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("ttClassPath").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.SmartComponents.Tools.ManageClassPath.ManageClassPathForm").
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:ManageClassPathForm_Fill_Panel = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:smartUpdatableBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:smartTempTableAdapter1 = NEW Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter(THIS-OBJECT:components).
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        THIS-OBJECT:saveFileDialog1 = NEW System.Windows.Forms.SaveFileDialog().
        THIS-OBJECT:folderBrowserDialog1 = NEW System.Windows.Forms.FolderBrowserDialog().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ManageClassPathForm_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm").
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar0[1] = buttonTool1.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:FooterToolbar:NonInheritedTools:AddRange(arrayvar0).
        buttonTool2:InstanceProps:IsFirstInGroup = TRUE.
        buttonTool5:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 9 NO-UNDO.
        arrayvar1[1] = buttonTool48.
        arrayvar1[2] = buttonTool51.
        arrayvar1[3] = buttonTool50.
        arrayvar1[4] = buttonTool2.
        arrayvar1[5] = buttonTool3.
        arrayvar1[6] = buttonTool4.
        arrayvar1[7] = buttonTool5.
        arrayvar1[8] = buttonTool6.
        arrayvar1[9] = buttonTool7.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar1).
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenuButtonImage = CAST(resources:GetObject("smartToolbarController1.Ribbon.ApplicationMenuButtonImage"), System.Drawing.Image).
        ribbonTab1:Caption = "Start".
        ribbonGroup1:Caption = "File".
        buttonTool42:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool43:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool44:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar2[1] = buttonTool42.
        arrayvar2[2] = buttonTool43.
        arrayvar2[3] = buttonTool44.
        ribbonGroup1:Tools:AddRange(arrayvar2).
        ribbonGroup2:Caption = "Maintenance".
        buttonTool12:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool12:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool13:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool13:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool14:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool14:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool15:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool16:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool17:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar3[1] = buttonTool12.
        arrayvar3[2] = buttonTool13.
        arrayvar3[3] = buttonTool14.
        arrayvar3[4] = buttonTool15.
        arrayvar3[5] = buttonTool16.
        arrayvar3[6] = buttonTool17.
        ribbonGroup2:Tools:AddRange(arrayvar3).
        ribbonGroup3:Caption = "Navigation".
        buttonTool8:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool8:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool9:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool9:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool10:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool10:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool11:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool11:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar4[1] = buttonTool8.
        arrayvar4[2] = buttonTool9.
        arrayvar4[3] = buttonTool10.
        arrayvar4[4] = buttonTool11.
        ribbonGroup3:Tools:AddRange(arrayvar4).
        ribbonGroup3:Visible = FALSE.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 3 NO-UNDO.
        arrayvar5[1] = ribbonGroup1.
        arrayvar5[2] = ribbonGroup2.
        arrayvar5[3] = ribbonGroup3.
        ribbonTab1:Groups:AddRange(arrayvar5).
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
        arrayvar6[1] = ribbonTab1.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar6).
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar7[1] = buttonTool18.
        arrayvar7[2] = buttonTool19.
        arrayvar7[3] = buttonTool20.
        THIS-OBJECT:smartToolbarController1:Ribbon:QuickAccessToolbar:NonInheritedTools:AddRange(arrayvar7).
        THIS-OBJECT:smartToolbarController1:Ribbon:Visible = TRUE.
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        buttonTool21:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar8[1] = buttonTool21.
        arrayvar8[2] = buttonTool22.
        arrayvar8[3] = buttonTool23.
        arrayvar8[4] = buttonTool24.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar8).
        ultraToolbar1:Text = "Navigation Toolbar".
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
        buttonTool25:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar9[1] = buttonTool25.
        arrayvar9[2] = buttonTool26.
        arrayvar9[3] = buttonTool27.
        arrayvar9[4] = buttonTool28.
        arrayvar9[5] = buttonTool29.
        arrayvar9[6] = buttonTool30.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar9).
        ultraToolbar2:Text = "TableIO Toolbar".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 2 NO-UNDO.
        arrayvar10[1] = ultraToolbar1.
        arrayvar10[2] = ultraToolbar2.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar10).
        appearance21:Image = CAST(resources:GetObject("appearance21.Image"), System.Object).
        buttonTool31:SharedPropsInternal:AppearancesSmall:Appearance = appearance21.
        buttonTool31:SharedPropsInternal:Caption = "First record".
        buttonTool31:SharedPropsInternal:CustomizerCaption = "First".
        buttonTool31:SharedPropsInternal:Enabled = FALSE.
        buttonTool31:SharedPropsInternal:ToolTipText = "First record".
        appearance22:Image = CAST(resources:GetObject("appearance22.Image"), System.Object).
        buttonTool32:SharedPropsInternal:AppearancesSmall:Appearance = appearance22.
        buttonTool32:SharedPropsInternal:Caption = "Previous record".
        buttonTool32:SharedPropsInternal:CustomizerCaption = "Previous".
        buttonTool32:SharedPropsInternal:Enabled = FALSE.
        buttonTool32:SharedPropsInternal:ToolTipText = "Previous record".
        appearance23:Image = CAST(resources:GetObject("appearance23.Image"), System.Object).
        buttonTool33:SharedPropsInternal:AppearancesSmall:Appearance = appearance23.
        buttonTool33:SharedPropsInternal:Caption = "Next record".
        buttonTool33:SharedPropsInternal:CustomizerCaption = "Next".
        buttonTool33:SharedPropsInternal:Enabled = FALSE.
        buttonTool33:SharedPropsInternal:ToolTipText = "Next record".
        appearance24:Image = CAST(resources:GetObject("appearance24.Image"), System.Object).
        buttonTool34:SharedPropsInternal:AppearancesSmall:Appearance = appearance24.
        buttonTool34:SharedPropsInternal:Caption = "Last record".
        buttonTool34:SharedPropsInternal:CustomizerCaption = "Last".
        buttonTool34:SharedPropsInternal:Enabled = FALSE.
        buttonTool34:SharedPropsInternal:ToolTipText = "Last record".
        appearance25:Image = CAST(resources:GetObject("appearance25.Image"), System.Object).
        buttonTool35:SharedPropsInternal:AppearancesSmall:Appearance = appearance25.
        buttonTool35:SharedPropsInternal:Caption = "Add record".
        buttonTool35:SharedPropsInternal:CustomizerCaption = "Add".
        buttonTool35:SharedPropsInternal:ToolTipText = "Add record".
        appearance26:Image = CAST(resources:GetObject("appearance26.Image"), System.Object).
        buttonTool36:SharedPropsInternal:AppearancesSmall:Appearance = appearance26.
        buttonTool36:SharedPropsInternal:Caption = "Copy record".
        buttonTool36:SharedPropsInternal:CustomizerCaption = "Copy".
        buttonTool36:SharedPropsInternal:ToolTipText = "Copy record".
        appearance27:Image = CAST(resources:GetObject("appearance27.Image"), System.Object).
        buttonTool37:SharedPropsInternal:AppearancesSmall:Appearance = appearance27.
        buttonTool37:SharedPropsInternal:Caption = "Update record".
        buttonTool37:SharedPropsInternal:CustomizerCaption = "Update".
        buttonTool37:SharedPropsInternal:ToolTipText = "Update record".
        appearance28:Image = CAST(resources:GetObject("appearance28.Image"), System.Object).
        buttonTool38:SharedPropsInternal:AppearancesSmall:Appearance = appearance28.
        buttonTool38:SharedPropsInternal:Caption = "Save changes".
        buttonTool38:SharedPropsInternal:CustomizerCaption = "Save".
        buttonTool38:SharedPropsInternal:Enabled = FALSE.
        buttonTool38:SharedPropsInternal:ToolTipText = "Save changes".
        appearance29:Image = CAST(resources:GetObject("appearance29.Image"), System.Object).
        buttonTool39:SharedPropsInternal:AppearancesSmall:Appearance = appearance29.
        buttonTool39:SharedPropsInternal:Caption = "Cancel update".
        buttonTool39:SharedPropsInternal:CustomizerCaption = "Cancel".
        buttonTool39:SharedPropsInternal:Enabled = FALSE.
        buttonTool39:SharedPropsInternal:ToolTipText = "Cancel update".
        appearance30:Image = CAST(resources:GetObject("appearance30.Image"), System.Object).
        buttonTool40:SharedPropsInternal:AppearancesSmall:Appearance = appearance30.
        buttonTool40:SharedPropsInternal:Caption = "Delete record".
        buttonTool40:SharedPropsInternal:CustomizerCaption = "Delete".
        buttonTool40:SharedPropsInternal:ToolTipText = "Delete record".
        appearance31:Image = CAST(resources:GetObject("appearance31.Image"), System.Object).
        buttonTool41:SharedPropsInternal:AppearancesSmall:Appearance = appearance31.
        buttonTool41:SharedPropsInternal:Caption = "&Exit".
        buttonTool41:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance37:Image = CAST(resources:GetObject("appearance37.Image"), System.Object).
        buttonTool45:SharedPropsInternal:AppearancesLarge:Appearance = appearance37.
        appearance36:Image = CAST(resources:GetObject("appearance36.Image"), System.Object).
        buttonTool45:SharedPropsInternal:AppearancesSmall:Appearance = appearance36.
        buttonTool45:SharedPropsInternal:Caption = "Open".
        appearance32:Image = CAST(resources:GetObject("appearance32.Image"), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesLarge:Appearance = appearance32.
        appearance33:Image = CAST(resources:GetObject("appearance33.Image"), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesSmall:Appearance = appearance33.
        buttonTool46:SharedPropsInternal:Caption = "Save".
        appearance34:Image = CAST(resources:GetObject("appearance34.Image"), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesLarge:Appearance = appearance34.
        appearance35:Image = CAST(resources:GetObject("appearance35.Image"), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesSmall:Appearance = appearance35.
        buttonTool47:SharedPropsInternal:Caption = "Save As".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 14 NO-UNDO.
        arrayvar11[1] = buttonTool31.
        arrayvar11[2] = buttonTool32.
        arrayvar11[3] = buttonTool33.
        arrayvar11[4] = buttonTool34.
        arrayvar11[5] = buttonTool35.
        arrayvar11[6] = buttonTool36.
        arrayvar11[7] = buttonTool37.
        arrayvar11[8] = buttonTool38.
        arrayvar11[9] = buttonTool39.
        arrayvar11[10] = buttonTool40.
        arrayvar11[11] = buttonTool41.
        arrayvar11[12] = buttonTool45.
        arrayvar11[13] = buttonTool46.
        arrayvar11[14] = buttonTool47.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar11).
        THIS-OBJECT:smartToolbarController1:ToolClick:Subscribe(THIS-OBJECT:smartToolbarController1_ToolClick).
        /*  */
        /* ManageClassPathForm_Fill_Panel */
        /*  */
        THIS-OBJECT:ManageClassPathForm_Fill_Panel:Controls:Add(THIS-OBJECT:smartUpdatableBrowser1).
        THIS-OBJECT:ManageClassPathForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:ManageClassPathForm_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ManageClassPathForm_Fill_Panel:Location = NEW System.Drawing.Point(8, 164).
        THIS-OBJECT:ManageClassPathForm_Fill_Panel:Name = "ManageClassPathForm_Fill_Panel".
        THIS-OBJECT:ManageClassPathForm_Fill_Panel:Size = NEW System.Drawing.Size(672, 315).
        THIS-OBJECT:ManageClassPathForm_Fill_Panel:TabIndex = 0.
        /*  */
        /* smartUpdatableBrowser1 */
        /*  */
        THIS-OBJECT:smartUpdatableBrowser1:DataSource = THIS-OBJECT:bindingSource1.
        THIS-OBJECT:smartUpdatableBrowser1:DeselectRowsOnActivate = TRUE.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Appearance = appearance1.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ResizeAllColumns.
        ultraGridColumn3:ButtonDisplayStyle = Infragistics.Win.UltraWinGrid.ButtonDisplayStyle:OnCellActivate.
        ultraGridColumn3:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn3:Header:VisiblePosition = 0.
        ultraGridColumn3:Style = Infragistics.Win.UltraWinGrid.ColumnStyle:EditButton.
        ultraGridColumn3:Width = 330.
        ultraGridColumn4:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn4:Header:VisiblePosition = 1.
        ultraGridColumn4:Width = 340.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar12 AS System.Object EXTENT 2 NO-UNDO.
        arrayvar12[1] = ultraGridColumn3.
        arrayvar12[2] = ultraGridColumn4.
        ultraGridBand1:Columns:AddRange(arrayvar12).
        ultraGridBand1:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:True.
        ultraGridBand1:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance2:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:Appearance = appearance2.
        appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance4:BackColor2 = System.Drawing.SystemColors:Control.
        appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:MaxRowScrollRegions = 1.
        appearance5:BackColor = System.Drawing.SystemColors:Window.
        appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveCellAppearance = appearance5.
        appearance6:BackColor = System.Drawing.SystemColors:Highlight.
        appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveRowAppearance = appearance6.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance7:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CardAreaAppearance = appearance7.
        appearance8:BorderColor = System.Drawing.Color:Silver.
        appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellAppearance = appearance8.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellPadding = 0.
        appearance9:BackColor = System.Drawing.SystemColors:Control.
        appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance9:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:GroupByRowAppearance = appearance9.
        appearance10:TextHAlignAsString = "Left".
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderAppearance = appearance10.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance11:BackColor = System.Drawing.SystemColors:Window.
        appearance11:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowAppearance = appearance11.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:smartUpdatableBrowser1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:smartUpdatableBrowser1:Font = NEW System.Drawing.Font("Microsoft Sans Serif", 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:smartUpdatableBrowser1:LinkDataSource = THIS-OBJECT:smartTempTableAdapter1.
        THIS-OBJECT:smartUpdatableBrowser1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartUpdatableBrowser1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:smartUpdatableBrowser1:Name = "smartUpdatableBrowser1".
        THIS-OBJECT:smartUpdatableBrowser1:SettingsKey = "def4bfbb-c59e-cd99-e011-8169ca52342c".
        THIS-OBJECT:smartUpdatableBrowser1:Size = NEW System.Drawing.Size(672, 315).
        THIS-OBJECT:smartUpdatableBrowser1:SmartTableIOState = "FieldsDisabled".
        THIS-OBJECT:smartUpdatableBrowser1:StoreVisibleGridColumns = TRUE.
        THIS-OBJECT:smartUpdatableBrowser1:TabIndex = 0.
        THIS-OBJECT:smartUpdatableBrowser1:Text = "smartUpdatableBrowser1".
        THIS-OBJECT:smartUpdatableBrowser1:VisibleGridColumns = "".
        THIS-OBJECT:smartUpdatableBrowser1:ClickCellButton:Subscribe(THIS-OBJECT:smartUpdatableBrowser1_ClickCellButton).
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource1:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar13 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar13 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc1:ChildTables = arrayvar13.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar14 AS Progress.Data.ColumnPropDesc EXTENT 2 NO-UNDO.
        arrayvar14[1] = NEW Progress.Data.ColumnPropDesc("Directory", "Directory", Progress.Data.DataType:CHARACTER).
        arrayvar14[2] = NEW Progress.Data.ColumnPropDesc("Prefix", "Prefix", Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar14.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        /*  */
        /* smartTempTableAdapter1 */
        /*  */
        THIS-OBJECT:smartTempTableAdapter1:AppServerPartition = "".
        THIS-OBJECT:smartTempTableAdapter1:BindingSourceExcludeFields = "".
        THIS-OBJECT:smartTempTableAdapter1:BindingSourceIncludeFields = "*".
        THIS-OBJECT:smartTempTableAdapter1:LinkDataSource = ?.
        THIS-OBJECT:smartTempTableAdapter1:LinkNavigationSource = ?.
        THIS-OBJECT:smartTempTableAdapter1:SmartDataSourceActive = TRUE.
        THIS-OBJECT:smartTempTableAdapter1:SmartDataSourceState = "Navigatable".
        /*  */
        /* m_ManageClassPathForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 164).
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Left:Name = "m_ManageClassPathForm_Toolbars_Dock_Area_Left".
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(8, 315).
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_ManageClassPathForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(680, 164).
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Right:Name = "m_ManageClassPathForm_Toolbars_Dock_Area_Right".
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(8, 315).
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_ManageClassPathForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Top:Name = "m_ManageClassPathForm_Toolbars_Dock_Area_Top".
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(688, 164).
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_ManageClassPathForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 479).
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Bottom:Name = "m_ManageClassPathForm_Toolbars_Dock_Area_Bottom".
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(688, 8).
        THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* openFileDialog1 */
        /*  */
        THIS-OBJECT:openFileDialog1:AddExtension = FALSE.
        THIS-OBJECT:openFileDialog1:FileName = ".classpath".
        THIS-OBJECT:openFileDialog1:Filter = "Classpath Files|.classpath|All Files|*.*".
        THIS-OBJECT:openFileDialog1:RestoreDirectory = TRUE.
        THIS-OBJECT:openFileDialog1:SupportMultiDottedExtensions = TRUE.
        THIS-OBJECT:openFileDialog1:Title = "Open .classpath".
        /*  */
        /* saveFileDialog1 */
        /*  */
        THIS-OBJECT:saveFileDialog1:FileName = ".classpath".
        THIS-OBJECT:saveFileDialog1:Filter = "Classpath Files|.classpath|All Files|*.*".
        THIS-OBJECT:saveFileDialog1:RestoreDirectory = TRUE.
        THIS-OBJECT:saveFileDialog1:SupportMultiDottedExtensions = TRUE.
        THIS-OBJECT:saveFileDialog1:Title = "Save .classpath ".
        /*  */
        /* folderBrowserDialog1 */
        /*  */
        THIS-OBJECT:folderBrowserDialog1:Description = "Select Directory".
        THIS-OBJECT:folderBrowserDialog1:RootFolder = System.Environment+SpecialFolder:MyComputer.
        /*  */
        /* ManageClassPathForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(688, 487).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ManageClassPathForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_ManageClassPathForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon"), System.Drawing.Icon).
        THIS-OBJECT:Name = "ManageClassPathForm".
        THIS-OBJECT:Text = ".classpath Maintenance".
        THIS-OBJECT:WindowPositionRegistryKey = "ManageClassPathForm".
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ManageClassPathForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Prompts for a filename and opens it.                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OpenFile ():
        
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .
        
        THIS-OBJECT:CheckForUpdate () .
        
        WAIT-FOR openFileDialog1:ShowDialog () SET oDialogResult .
        
        IF Progress.Util.EnumHelper:AreEqual (oDialogResult,
                                              System.Windows.Forms.DialogResult:OK) THEN .
        ELSE 
            RETURN .                                               
        
        TEMP-TABLE ttClassPath:READ-XML ("FILE", openFileDialog1:FileName, "EMPTY", ?, ?) .
    
        THIS-OBJECT:FileName = openFileDialog1:FileName .
        
        smartTempTableAdapter1:RetrieveData() .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves the current temp-table                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SaveFile ():
    
        CheckForUpdate () . 
                
        IF THIS-OBJECT:FileName > "":U THEN . 
        ELSE DO:
            THIS-OBJECT:SaveFileAs () .
            
            RETURN . 
        END.

        SaveToFile (THIS-OBJECT:FileName) .        

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves the current temp-table to a new file                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SaveFileAs ():
        
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO . 

        CheckForUpdate () . 
        
        WAIT-FOR saveFileDialog1:ShowDialog() SET oDialogResult . 
        
        IF Progress.Util.EnumHelper:AreEqual (oDialogResult, 
                                              System.Windows.Forms.DialogResult:OK) THEN .
        ELSE 
            RETURN .                                                   

        SaveToFile (saveFileDialog1:FileName) . 
        
        THIS-OBJECT:FileName = saveFileDialog1:FileName .         
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves to a file                                                                       
        Notes:                  
        @param pcFileName The file name to save the .classpath file to                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SaveToFile (pcFileName AS CHARACTER):
    
        TEMP-TABLE ttClassPath:WRITE-XML ("FILE":U, pcFileName, TRUE) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the smartToolbarController1                                                                        
        Notes:                                                                       
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event   
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartToolbarController1_ToolClick (sender AS System.Object,     
                                                           e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs ):
        
        CASE e:Tool:Key:
            WHEN "Open":U THEN 
                OpenFile () .
            WHEN "Save":U THEN 
                SaveFile () .
            WHEN "SaveAs":U THEN 
                SaveFileAs () .
         END CASE .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ClickCellButton event of the smartUpdatableBrowser1
                 instance                                                                          
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The CellEventArgs with the data for this event                                                             
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartUpdatableBrowser1_ClickCellButton (sender AS System.Object, 
                                                                e AS Infragistics.Win.UltraWinGrid.CellEventArgs):
        
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .
        
        CASE e:Cell:Column:Key:
            WHEN "Directory":U THEN DO:

                WAIT-FOR folderBrowserDialog1:ShowDialog () SET oDialogResult . 
                
                IF Progress.Util.EnumHelper:AreEqual (oDialogResult, 
                                                      System.Windows.Forms.DialogResult:Ok) THEN DO:
                                                          
                    e:Cell:Value = folderBrowserDialog1:SelectedPath .
                    e:Cell:SelectAll() .                                      
                END.                                                                                  
            END.    
        END CASE.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ManageClassPathForm class                                                                         
        Notes:                                                                            
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ManageClassPathForm ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
