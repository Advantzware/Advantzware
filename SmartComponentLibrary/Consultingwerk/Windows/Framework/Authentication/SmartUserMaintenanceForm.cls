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
    File        : SmartUserMaintenanceForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu Oct 11 21:12:49 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Enum.*                     FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.SmartWindowForm FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*               FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*     FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*         FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.*       FROM PROPATH .
USING Consultingwerk.Util.*                               FROM PROPATH .
USING Consultingwerk.Windows.Framework.Authentication.*   FROM PROPATH .  
USING Progress.Lang.*                                     FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Authentication.SmartUserMaintenanceForm 
    INHERITS SmartWindowForm
    IMPLEMENTS IDefaultActionSubscriber: 
    
    DEFINE PRIVATE VARIABLE ButtonUnAssignUserFromGroup AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ButtonAssignUserToGroup AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Top    AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea                        NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Right  AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea                        NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Left   AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea                        NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea                        NO-UNDO.
    DEFINE PRIVATE VARIABLE components                                           AS System.ComponentModel.IContainer                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE PossibleAssignementsTableAdapter AS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE PossibleAssignementsBindingSource AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE smartDataBrowser1                                    AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser                 NO-UNDO.
    DEFINE PRIVATE VARIABLE smartDataBrowser3 AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE smartDataBrowser2 AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE smartUserViewer1                                     AS Consultingwerk.Windows.Framework.Authentication.SmartUserViewer                NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1                              AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController           NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraSplitter1 AS Infragistics.Win.Misc.UltraSplitter NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel1 AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE UserBindingSource                                    AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE UserAdapter                                          AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter       NO-UNDO.
    DEFINE PRIVATE VARIABLE SmartUserMaintenanceForm_Fill_Panel                  AS Infragistics.Win.Misc.UltraPanel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE UserGroupBindingSource AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE UserGroupAdapter AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter NO-UNDO.

    DEFINE PRIVATE VARIABLE oUserGroupDatasetController           AS UserGroupDatasetController NO-UNDO.
    DEFINE PRIVATE VARIABLE lPendingChangesInUserGroupAssignement AS LOGICAL                    NO-UNDO INIT FALSE.

    CONSTRUCTOR PUBLIC SmartUserMaintenanceForm ():
        
        SUPER ().
        InitializeComponent ().
        
        /* Marko Rüterbories, Consultingwerk Ltd. 05.02.2013
           Register the DefaultActionSubscriber to the SmartDataBrowser controls
           to enable the user to assign or unassign a group to/from a user. */
        smartDataBrowser2:RegisterDefaultActionSubscriber (THIS-OBJECT).
        smartDataBrowser3:RegisterDefaultActionSubscriber (THIS-OBJECT).

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds an assignement to a Group for a User
        Notes:   
        @param sender Reference to the calling Object
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ButtonAssignUserToGroup_Click (sender AS System.Object, 
                                                       e      AS System.EventArgs):
        
        IF NOT VALID-OBJECT (oUserGroupDatasetController) THEN 
            UNDO, THROW NEW AppError ("No DatasteController registered for UserGroups!":U, 0).
    
        /* Marko Rüterbories, Consultingwerk Ltd. 05.10.2013
           Do not try to assign from an empty list. */
        IF PossibleAssignementsBindingSource:Count = 0 THEN RETURN.
        
        oUserGroupDatasetController:AssignUserToGroup (PossibleAssignementsTableAdapter:GetFieldValues("UserGroupGuid":U)).
        
        UserGroupAdapter:SubmitChanges ().
        
        UserGroupAdapter:QueryHandle:QUERY-OPEN ().
        UserGroupAdapter:BindingSource:RefreshAll ().
        
        PossibleAssignementsTableAdapter:RetrieveData ().

        /* Marko Rüterbories, Consultingwerk Ltd. 12.10.2013
           Refresh Assignment button sensitivity */
        SetAssignmentButtonState ().
        
        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
        FINALLY:
            lPendingChangesInUserGroupAssignement = TRUE.
        END FINALLY.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Removes the assignement of a User to a Group
        Notes:   
        @param sender Reference to the calling Object
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ButtonUnAssignUserFromGroup_Click (sender AS System.Object, 
                                                           e      AS System.EventArgs):
        
        IF NOT VALID-OBJECT (oUserGroupDatasetController) THEN 
            UNDO, THROW NEW AppError ("No DatasteController registered for UserGroups!":U, 0).
        
        /* Marko Rüterbories, Consultingwerk Ltd. 05.10.2013
           Do not try to assign from an empty list. */
        IF UserGroupBindingSource:Count = 0 THEN RETURN.

        oUserGroupDatasetController:UnAssignUserFromGroup ().
        
        UserGroupAdapter:SubmitChanges ().
        
        UserGroupAdapter:QueryHandle:QUERY-OPEN ().
        UserGroupAdapter:BindingSource:RefreshAll ().
        
        PossibleAssignementsTableAdapter:RetrieveData ().

        /* Marko Rüterbories, Consultingwerk Ltd. 12.10.2013
           Refresh Assignment button sensitivity */
        SetAssignmentButtonState ().
        
        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
        FINALLY:
            lPendingChangesInUserGroupAssignement = TRUE.
        END FINALLY.
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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Authentication.SmartUserMaintenanceForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE contextualTabGroup1 AS Infragistics.Win.UltraWinToolbars.ContextualTabGroup NO-UNDO.
        contextualTabGroup1 = NEW Infragistics.Win.UltraWinToolbars.ContextualTabGroup("GroupAssignement":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("start":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("navigation":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("maintenance":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool15 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool15 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool16 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool16 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool17 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool17 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonTab2 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
        ribbonTab2 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("GroupAssignement":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
        ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("Assignement":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool20 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool20 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool21 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool21 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool22 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool22 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("NavigationToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool23 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool23 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool24 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool24 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool25 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool25 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool26 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool26 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TableIOToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool27 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool27 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool28 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool28 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool29 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool29 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool30 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool30 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool31 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool31 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool32 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool32 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar3 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar3 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TransactionToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool33 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool33 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool34 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool34 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool35 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool35 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance41 AS Infragistics.Win.Appearance NO-UNDO.
        appearance41 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool36 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool36 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance42 AS Infragistics.Win.Appearance NO-UNDO.
        appearance42 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool37 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool37 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance43 AS Infragistics.Win.Appearance NO-UNDO.
        appearance43 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool38 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool38 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance44 AS Infragistics.Win.Appearance NO-UNDO.
        appearance44 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool39 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool39 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance45 AS Infragistics.Win.Appearance NO-UNDO.
        appearance45 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool40 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool40 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance46 AS Infragistics.Win.Appearance NO-UNDO.
        appearance46 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool41 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool41 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance47 AS Infragistics.Win.Appearance NO-UNDO.
        appearance47 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool42 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool42 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance48 AS Infragistics.Win.Appearance NO-UNDO.
        appearance48 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool43 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool43 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance49 AS Infragistics.Win.Appearance NO-UNDO.
        appearance49 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool44 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool44 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance50 AS Infragistics.Win.Appearance NO-UNDO.
        appearance50 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool45 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool45 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Exit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance51 AS Infragistics.Win.Appearance NO-UNDO.
        appearance51 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool46 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool46 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance52 AS Infragistics.Win.Appearance NO-UNDO.
        appearance52 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance53 AS Infragistics.Win.Appearance NO-UNDO.
        appearance53 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool47 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool47 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance54 AS Infragistics.Win.Appearance NO-UNDO.
        appearance54 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance55 AS Infragistics.Win.Appearance NO-UNDO.
        appearance55 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ePossibleAssignments":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn31 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn31 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UserGroupGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn32 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn32 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("GroupGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn33 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn33 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UserGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn34 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn34 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UserName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn35 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn35 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("GroupName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("ePossibleAssignments":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand2 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand2 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("eSmartUserGroup":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn26 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn26 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UserGroupGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn27 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn27 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("GroupGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn28 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn28 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UserGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn29 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn29 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UserName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn30 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn30 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("GroupName":U, -1, ?, 0, Infragistics.Win.UltraWinGrid.SortIndicator:Ascending, FALSE).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc2 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc2 = NEW Progress.Data.TableDesc("eSmartUserGroup":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
        appearance27 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
        appearance28 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
        appearance29 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand3 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand3 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("eSmartUser":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UserGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UserName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LanguageGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UserFullName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UserEmail":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn6 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn6 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UserPassword":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn17 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn17 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UserPasswordChangedDate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn18 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn18 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UserLastLogin":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn19 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn19 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LoginCompanyGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn20 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn20 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ManagerUserGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn7 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn7 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UserSecurityId":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn21 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn21 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LanguageIsoCode":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn22 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn22 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LanguageName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn23 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn23 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ManagerUserName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn24 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn24 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ManagerUserFullName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn25 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn25 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LoginCompanyName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
        appearance30 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
        appearance31 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
        appearance32 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
        appearance33 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
        appearance34 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance35 AS Infragistics.Win.Appearance NO-UNDO.
        appearance35 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance36 AS Infragistics.Win.Appearance NO-UNDO.
        appearance36 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance37 AS Infragistics.Win.Appearance NO-UNDO.
        appearance37 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance38 AS Infragistics.Win.Appearance NO-UNDO.
        appearance38 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance39 AS Infragistics.Win.Appearance NO-UNDO.
        appearance39 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance40 AS Infragistics.Win.Appearance NO-UNDO.
        appearance40 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc3 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc3 = NEW Progress.Data.TableDesc("eSmartUser":U).
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:smartUserViewer1 = NEW Consultingwerk.Windows.Framework.Authentication.SmartUserViewer().
        THIS-OBJECT:UserAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter(THIS-OBJECT:components).
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:ultraTabPageControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:smartDataBrowser3 = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:PossibleAssignementsBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:PossibleAssignementsTableAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter(THIS-OBJECT:components).
        THIS-OBJECT:smartDataBrowser2 = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:UserGroupBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:UserGroupAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter(THIS-OBJECT:components).
        THIS-OBJECT:ultraPanel1 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ButtonAssignUserToGroup = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ButtonUnAssignUserFromGroup = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:SmartUserMaintenanceForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:ultraSplitter1 = NEW Infragistics.Win.Misc.UltraSplitter().
        THIS-OBJECT:smartDataBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:UserBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        CAST(THIS-OBJECT:smartUserViewer1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:UserAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl2:SuspendLayout().
        CAST(THIS-OBJECT:smartDataBrowser3, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:PossibleAssignementsBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:PossibleAssignementsTableAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartDataBrowser2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:UserGroupBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:UserGroupAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraPanel1:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel1:SuspendLayout().
        THIS-OBJECT:SmartUserMaintenanceForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:SmartUserMaintenanceForm_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:UserBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartUserViewer1).
        resources:ApplyResources(THIS-OBJECT:ultraTabPageControl1, "ultraTabPageControl1":U).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        /*  */
        /* smartUserViewer1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartUserViewer1, "smartUserViewer1":U).
        THIS-OBJECT:smartUserViewer1:FocusControlOnAdd = ?.
        THIS-OBJECT:smartUserViewer1:FocusControlOnUpdate = ?.
        THIS-OBJECT:smartUserViewer1:LinkDataSource = THIS-OBJECT:UserAdapter.
        THIS-OBJECT:smartUserViewer1:LinkGroupAssignSource = ?.
        THIS-OBJECT:smartUserViewer1:LinkTableIOSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:smartUserViewer1:Name = "smartUserViewer1":U.
        /*  */
        /* UserAdapter */
        /*  */
        THIS-OBJECT:UserAdapter:EntityName = "Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U.
        THIS-OBJECT:UserAdapter:EntityTable = "eSmartUser":U.
        THIS-OBJECT:UserAdapter:LinkCommitSource = ?.
        THIS-OBJECT:UserAdapter:LinkDataSource = ?.
        THIS-OBJECT:UserAdapter:LinkNavigationSource = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar0[1] = buttonTool1.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:FooterToolbar:NonInheritedTools:AddRange(arrayvar0).
        buttonTool5:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar1[1] = buttonTool2.
        arrayvar1[2] = buttonTool3.
        arrayvar1[3] = buttonTool4.
        arrayvar1[4] = buttonTool5.
        arrayvar1[5] = buttonTool6.
        arrayvar1[6] = buttonTool7.
        THIS-OBJECT:smartToolbarController1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar1).
        resources:ApplyResources(contextualTabGroup1, "contextualTabGroup1":U).
        contextualTabGroup1:Key = "GroupAssignement":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ContextualTabGroup EXTENT 1 NO-UNDO.
        arrayvar2[1] = contextualTabGroup1.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedContextualTabGroups:AddRange(arrayvar2).
        resources:ApplyResources(ribbonTab1, "ribbonTab1":U).
        resources:ApplyResources(ribbonGroup1, "ribbonGroup1":U).
        buttonTool8:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool8:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool9:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool9:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool10:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool10:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool11:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool11:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar3[1] = buttonTool8.
        arrayvar3[2] = buttonTool9.
        arrayvar3[3] = buttonTool10.
        arrayvar3[4] = buttonTool11.
        ribbonGroup1:Tools:AddRange(arrayvar3).
        resources:ApplyResources(ribbonGroup2, "ribbonGroup2":U).
        buttonTool12:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool12:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool13:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool13:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool14:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool14:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool15:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool16:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        buttonTool17:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar4[1] = buttonTool12.
        arrayvar4[2] = buttonTool13.
        arrayvar4[3] = buttonTool14.
        arrayvar4[4] = buttonTool15.
        arrayvar4[5] = buttonTool16.
        arrayvar4[6] = buttonTool17.
        ribbonGroup2:Tools:AddRange(arrayvar4).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 2 NO-UNDO.
        arrayvar5[1] = ribbonGroup1.
        arrayvar5[2] = ribbonGroup2.
        ribbonTab1:Groups:AddRange(arrayvar5).
        resources:ApplyResources(ribbonTab2, "ribbonTab2":U).
        ribbonTab2:ContextualTabGroupKey = "GroupAssignement":U.
        resources:ApplyResources(ribbonGroup3, "ribbonGroup3":U).
        buttonTool18:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        buttonTool19:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar6[1] = buttonTool18.
        arrayvar6[2] = buttonTool19.
        ribbonGroup3:Tools:AddRange(arrayvar6).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 1 NO-UNDO.
        arrayvar7[1] = ribbonGroup3.
        ribbonTab2:Groups:AddRange(arrayvar7).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 2 NO-UNDO.
        arrayvar8[1] = ribbonTab1.
        arrayvar8[2] = ribbonTab2.
        THIS-OBJECT:smartToolbarController1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar8).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
        arrayvar9[1] = buttonTool20.
        arrayvar9[2] = buttonTool21.
        arrayvar9[3] = buttonTool22.
        THIS-OBJECT:smartToolbarController1:Ribbon:QuickAccessToolbar:NonInheritedTools:AddRange(arrayvar9).
        THIS-OBJECT:smartToolbarController1:Ribbon:Visible = TRUE.
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        THIS-OBJECT:smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        buttonTool23:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar10[1] = buttonTool23.
        arrayvar10[2] = buttonTool24.
        arrayvar10[3] = buttonTool25.
        arrayvar10[4] = buttonTool26.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar10).
        resources:ApplyResources(ultraToolbar1, "ultraToolbar1":U).
        ultraToolbar2:DockedColumn = 0.
        ultraToolbar2:DockedRow = 1.
        buttonTool27:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar11[1] = buttonTool27.
        arrayvar11[2] = buttonTool28.
        arrayvar11[3] = buttonTool29.
        arrayvar11[4] = buttonTool30.
        arrayvar11[5] = buttonTool31.
        arrayvar11[6] = buttonTool32.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar11).
        resources:ApplyResources(ultraToolbar2, "ultraToolbar2":U).
        ultraToolbar3:DockedColumn = 1.
        ultraToolbar3:DockedRow = 1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar12 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar12[1] = buttonTool33.
        arrayvar12[2] = buttonTool34.
        ultraToolbar3:NonInheritedTools:AddRange(arrayvar12).
        resources:ApplyResources(ultraToolbar3, "ultraToolbar3":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar13 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 3 NO-UNDO.
        arrayvar13[1] = ultraToolbar1.
        arrayvar13[2] = ultraToolbar2.
        arrayvar13[3] = ultraToolbar3.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar13).
        appearance41:Image = CAST(resources:GetObject("appearance41.Image":U), System.Object).
        buttonTool35:SharedPropsInternal:AppearancesSmall:Appearance = appearance41.
        buttonTool35:SharedPropsInternal:Caption = resources:GetString("resource.Caption4":U).
        buttonTool35:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption":U).
        buttonTool35:SharedPropsInternal:Enabled = FALSE.
        buttonTool35:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText":U).
        appearance42:Image = CAST(resources:GetObject("appearance42.Image":U), System.Object).
        buttonTool36:SharedPropsInternal:AppearancesSmall:Appearance = appearance42.
        buttonTool36:SharedPropsInternal:Caption = resources:GetString("resource.Caption5":U).
        buttonTool36:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption1":U).
        buttonTool36:SharedPropsInternal:Enabled = FALSE.
        buttonTool36:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText1":U).
        appearance43:Image = CAST(resources:GetObject("appearance43.Image":U), System.Object).
        buttonTool37:SharedPropsInternal:AppearancesSmall:Appearance = appearance43.
        buttonTool37:SharedPropsInternal:Caption = resources:GetString("resource.Caption6":U).
        buttonTool37:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption2":U).
        buttonTool37:SharedPropsInternal:Enabled = FALSE.
        buttonTool37:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText2":U).
        appearance44:Image = CAST(resources:GetObject("appearance44.Image":U), System.Object).
        buttonTool38:SharedPropsInternal:AppearancesSmall:Appearance = appearance44.
        buttonTool38:SharedPropsInternal:Caption = resources:GetString("resource.Caption7":U).
        buttonTool38:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption3":U).
        buttonTool38:SharedPropsInternal:Enabled = FALSE.
        buttonTool38:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText3":U).
        appearance45:Image = CAST(resources:GetObject("appearance45.Image":U), System.Object).
        buttonTool39:SharedPropsInternal:AppearancesSmall:Appearance = appearance45.
        buttonTool39:SharedPropsInternal:Caption = resources:GetString("resource.Caption8":U).
        buttonTool39:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption4":U).
        buttonTool39:SharedPropsInternal:Enabled = FALSE.
        buttonTool39:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText4":U).
        appearance46:Image = CAST(resources:GetObject("appearance46.Image":U), System.Object).
        buttonTool40:SharedPropsInternal:AppearancesSmall:Appearance = appearance46.
        buttonTool40:SharedPropsInternal:Caption = resources:GetString("resource.Caption9":U).
        buttonTool40:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption5":U).
        buttonTool40:SharedPropsInternal:Enabled = FALSE.
        buttonTool40:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText5":U).
        appearance47:Image = CAST(resources:GetObject("appearance47.Image":U), System.Object).
        buttonTool41:SharedPropsInternal:AppearancesSmall:Appearance = appearance47.
        buttonTool41:SharedPropsInternal:Caption = resources:GetString("resource.Caption10":U).
        buttonTool41:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption6":U).
        buttonTool41:SharedPropsInternal:Enabled = FALSE.
        buttonTool41:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText6":U).
        appearance48:Image = CAST(resources:GetObject("appearance48.Image":U), System.Object).
        buttonTool42:SharedPropsInternal:AppearancesSmall:Appearance = appearance48.
        buttonTool42:SharedPropsInternal:Caption = resources:GetString("resource.Caption11":U).
        buttonTool42:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption7":U).
        buttonTool42:SharedPropsInternal:Enabled = FALSE.
        buttonTool42:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText7":U).
        appearance49:Image = CAST(resources:GetObject("appearance49.Image":U), System.Object).
        buttonTool43:SharedPropsInternal:AppearancesSmall:Appearance = appearance49.
        buttonTool43:SharedPropsInternal:Caption = resources:GetString("resource.Caption12":U).
        buttonTool43:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption8":U).
        buttonTool43:SharedPropsInternal:Enabled = FALSE.
        buttonTool43:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText8":U).
        appearance50:Image = CAST(resources:GetObject("appearance50.Image":U), System.Object).
        buttonTool44:SharedPropsInternal:AppearancesSmall:Appearance = appearance50.
        buttonTool44:SharedPropsInternal:Caption = resources:GetString("resource.Caption13":U).
        buttonTool44:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption9":U).
        buttonTool44:SharedPropsInternal:Enabled = FALSE.
        buttonTool44:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText9":U).
        appearance51:Image = CAST(resources:GetObject("appearance51.Image":U), System.Object).
        buttonTool45:SharedPropsInternal:AppearancesSmall:Appearance = appearance51.
        buttonTool45:SharedPropsInternal:Caption = resources:GetString("resource.Caption14":U).
        buttonTool45:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance52:Image = CAST(resources:GetObject("appearance52.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesLarge:Appearance = appearance52.
        appearance53:Image = CAST(resources:GetObject("appearance53.Image":U), System.Object).
        buttonTool46:SharedPropsInternal:AppearancesSmall:Appearance = appearance53.
        buttonTool46:SharedPropsInternal:Caption = resources:GetString("resource.Caption15":U).
        buttonTool46:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption10":U).
        buttonTool46:SharedPropsInternal:Enabled = FALSE.
        buttonTool46:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText10":U).
        appearance54:Image = CAST(resources:GetObject("appearance54.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesLarge:Appearance = appearance54.
        appearance55:Image = CAST(resources:GetObject("appearance55.Image":U), System.Object).
        buttonTool47:SharedPropsInternal:AppearancesSmall:Appearance = appearance55.
        buttonTool47:SharedPropsInternal:Caption = resources:GetString("resource.Caption16":U).
        buttonTool47:SharedPropsInternal:CustomizerCaption = resources:GetString("resource.CustomizerCaption11":U).
        buttonTool47:SharedPropsInternal:Enabled = FALSE.
        buttonTool47:SharedPropsInternal:ToolTipText = resources:GetString("resource.ToolTipText11":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar14 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 13 NO-UNDO.
        arrayvar14[1] = buttonTool35.
        arrayvar14[2] = buttonTool36.
        arrayvar14[3] = buttonTool37.
        arrayvar14[4] = buttonTool38.
        arrayvar14[5] = buttonTool39.
        arrayvar14[6] = buttonTool40.
        arrayvar14[7] = buttonTool41.
        arrayvar14[8] = buttonTool42.
        arrayvar14[9] = buttonTool43.
        arrayvar14[10] = buttonTool44.
        arrayvar14[11] = buttonTool45.
        arrayvar14[12] = buttonTool46.
        arrayvar14[13] = buttonTool47.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar14).
        /*  */
        /* ultraTabPageControl2 */
        /*  */
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartDataBrowser3).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartDataBrowser2).
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:ultraPanel1).
        resources:ApplyResources(THIS-OBJECT:ultraTabPageControl2, "ultraTabPageControl2":U).
        THIS-OBJECT:ultraTabPageControl2:Name = "ultraTabPageControl2":U.
        /*  */
        /* smartDataBrowser3 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartDataBrowser3, "smartDataBrowser3":U).
        THIS-OBJECT:smartDataBrowser3:DataSource = THIS-OBJECT:PossibleAssignementsBindingSource.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Appearance = appearance1.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ExtendLastColumn.
        ultraGridColumn31:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn31:Header:VisiblePosition = 1.
        ultraGridColumn31:Hidden = TRUE.
        ultraGridColumn32:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn32:Header:VisiblePosition = 2.
        ultraGridColumn32:Hidden = TRUE.
        ultraGridColumn33:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn33:Header:VisiblePosition = 3.
        ultraGridColumn33:Hidden = TRUE.
        ultraGridColumn34:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn34:Header:Caption = resources:GetString("resource.Caption":U).
        ultraGridColumn34:Header:VisiblePosition = 0.
        ultraGridColumn34:Hidden = TRUE.
        ultraGridColumn35:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn35:Header:Caption = resources:GetString("resource.Caption1":U).
        ultraGridColumn35:Header:VisiblePosition = 4.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar15 AS System.Object EXTENT 5 NO-UNDO.
        arrayvar15[1] = ultraGridColumn31.
        arrayvar15[2] = ultraGridColumn32.
        arrayvar15[3] = ultraGridColumn33.
        arrayvar15[4] = ultraGridColumn34.
        arrayvar15[5] = ultraGridColumn35.
        ultraGridBand1:Columns:AddRange(arrayvar15).
        ultraGridBand1:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand1:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:True.
        appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance2:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:GroupByBox:Appearance = appearance2.
        appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance4:BackColor2 = System.Drawing.SystemColors:Control.
        appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:MaxRowScrollRegions = 1.
        appearance5:BackColor = System.Drawing.SystemColors:Window.
        appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:ActiveCellAppearance = appearance5.
        appearance6:BackColor = System.Drawing.SystemColors:Highlight.
        appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:ActiveRowAppearance = appearance6.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance7:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:CardAreaAppearance = appearance7.
        appearance8:BorderColor = System.Drawing.Color:Silver.
        appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:CellAppearance = appearance8.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:CellPadding = 0.
        appearance9:BackColor = System.Drawing.SystemColors:Control.
        appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance9:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:GroupByRowAppearance = appearance9.
        resources:ApplyResources(appearance10, "appearance10":U).
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:HeaderAppearance = appearance10.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance11:BackColor = System.Drawing.SystemColors:Window.
        appearance11:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:RowAppearance = appearance11.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartDataBrowser3:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:smartDataBrowser3:LinkDataSource = THIS-OBJECT:PossibleAssignementsTableAdapter.
        THIS-OBJECT:smartDataBrowser3:Name = "smartDataBrowser3":U.
        /*  */
        /* PossibleAssignementsBindingSource */
        /*  */
        THIS-OBJECT:PossibleAssignementsBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:PossibleAssignementsBindingSource:EntityName = "Consultingwerk.SmartFramework.Authorization.UserGroupBusinessEntity":U.
        THIS-OBJECT:PossibleAssignementsBindingSource:EntityTable = "ePossibleAssignments":U.
        THIS-OBJECT:PossibleAssignementsBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:PossibleAssignementsBindingSource:NoLOBs = FALSE.
        THIS-OBJECT:PossibleAssignementsBindingSource:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar16 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar16 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc1:ChildTables = arrayvar16.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar17 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar17[1] = NEW Progress.Data.ColumnPropDesc("UserGroupGuid":U, "UserGroupGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[2] = NEW Progress.Data.ColumnPropDesc("GroupGuid":U, "GroupGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[3] = NEW Progress.Data.ColumnPropDesc("UserGuid":U, "UserGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[4] = NEW Progress.Data.ColumnPropDesc("UserName":U, "UserName":U, Progress.Data.DataType:CHARACTER).
        arrayvar17[5] = NEW Progress.Data.ColumnPropDesc("GroupName":U, "GroupName":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar17.
        THIS-OBJECT:PossibleAssignementsBindingSource:TableSchema = tableDesc1.
        /*  */
        /* PossibleAssignementsTableAdapter */
        /*  */
        THIS-OBJECT:PossibleAssignementsTableAdapter:LinkDataSource = ?.
        THIS-OBJECT:PossibleAssignementsTableAdapter:LinkNavigationSource = ?.
        /*  */
        /* smartDataBrowser2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartDataBrowser2, "smartDataBrowser2":U).
        THIS-OBJECT:smartDataBrowser2:DataSource = THIS-OBJECT:UserGroupBindingSource.
        appearance13:BackColor = System.Drawing.SystemColors:Window.
        appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Appearance = appearance13.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ExtendLastColumn.
        ultraGridColumn26:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn26:Header:VisiblePosition = 1.
        ultraGridColumn26:Hidden = TRUE.
        ultraGridColumn27:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn27:Header:VisiblePosition = 2.
        ultraGridColumn27:Hidden = TRUE.
        ultraGridColumn28:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn28:Header:VisiblePosition = 3.
        ultraGridColumn28:Hidden = TRUE.
        ultraGridColumn29:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn29:Header:Caption = resources:GetString("resource.Caption2":U).
        ultraGridColumn29:Header:VisiblePosition = 0.
        ultraGridColumn29:Hidden = TRUE.
        ultraGridColumn30:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn30:Header:Caption = resources:GetString("resource.Caption3":U).
        ultraGridColumn30:Header:VisiblePosition = 4.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar18 AS System.Object EXTENT 5 NO-UNDO.
        arrayvar18[1] = ultraGridColumn26.
        arrayvar18[2] = ultraGridColumn27.
        arrayvar18[3] = ultraGridColumn28.
        arrayvar18[4] = ultraGridColumn29.
        arrayvar18[5] = ultraGridColumn30.
        ultraGridBand2:Columns:AddRange(arrayvar18).
        ultraGridBand2:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand2:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand2:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand2:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:BandsSerializer:Add(ultraGridBand2).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:True.
        appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance14:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:GroupByBox:Appearance = appearance14.
        appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance16:BackColor2 = System.Drawing.SystemColors:Control.
        appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:MaxRowScrollRegions = 1.
        appearance17:BackColor = System.Drawing.SystemColors:Window.
        appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:ActiveCellAppearance = appearance17.
        appearance18:BackColor = System.Drawing.SystemColors:Highlight.
        appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:ActiveRowAppearance = appearance18.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance19:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:CardAreaAppearance = appearance19.
        appearance20:BorderColor = System.Drawing.Color:Silver.
        appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:CellAppearance = appearance20.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:CellPadding = 0.
        appearance21:BackColor = System.Drawing.SystemColors:Control.
        appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance21:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:GroupByRowAppearance = appearance21.
        resources:ApplyResources(appearance22, "appearance22":U).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:HeaderAppearance = appearance22.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance23:BackColor = System.Drawing.SystemColors:Window.
        appearance23:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:RowAppearance = appearance23.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:smartDataBrowser2:FetchAllRecordsOnGroupBy = TRUE.
        THIS-OBJECT:smartDataBrowser2:LinkDataSource = THIS-OBJECT:UserGroupAdapter.
        THIS-OBJECT:smartDataBrowser2:Name = "smartDataBrowser2":U.
        /*  */
        /* UserGroupBindingSource */
        /*  */
        THIS-OBJECT:UserGroupBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:UserGroupBindingSource:EntityName = "Consultingwerk.SmartFramework.Authorization.UserGroupBusinessEntity":U.
        THIS-OBJECT:UserGroupBindingSource:EntityTable = "eSmartUserGroup":U.
        THIS-OBJECT:UserGroupBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:UserGroupBindingSource:NoLOBs = FALSE.
        THIS-OBJECT:UserGroupBindingSource:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar19 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar19 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc2:ChildTables = arrayvar19.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar20 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar20[1] = NEW Progress.Data.ColumnPropDesc("UserGroupGuid":U, "UserGroupGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar20[2] = NEW Progress.Data.ColumnPropDesc("GroupGuid":U, "GroupGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar20[3] = NEW Progress.Data.ColumnPropDesc("UserGuid":U, "UserGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar20[4] = NEW Progress.Data.ColumnPropDesc("UserName":U, "UserName":U, Progress.Data.DataType:CHARACTER).
        arrayvar20[5] = NEW Progress.Data.ColumnPropDesc("GroupName":U, "GroupName":U, Progress.Data.DataType:CHARACTER).
        tableDesc2:Columns = arrayvar20.
        THIS-OBJECT:UserGroupBindingSource:TableSchema = tableDesc2.
        /*  */
        /* UserGroupAdapter */
        /*  */
        THIS-OBJECT:UserGroupAdapter:AutoCommit = FALSE.
        THIS-OBJECT:UserGroupAdapter:DatasetControllerType = "Consultingwerk.SmartFramework.Authorization.UserGroupDatasetController":U.
        THIS-OBJECT:UserGroupAdapter:EntityName = "Consultingwerk.SmartFramework.Authorization.UserGroupBusinessEntity":U.
        THIS-OBJECT:UserGroupAdapter:EntityTable = "eSmartUserGroup":U.
        THIS-OBJECT:UserGroupAdapter:ForeignFields = "UserGuid,UserGuid":U.
        THIS-OBJECT:UserGroupAdapter:LinkCommitSource = THIS-OBJECT:smartToolbarController1.
        THIS-OBJECT:UserGroupAdapter:LinkDataSource = THIS-OBJECT:UserAdapter.
        THIS-OBJECT:UserGroupAdapter:LinkNavigationSource = ?.
        THIS-OBJECT:UserGroupAdapter:AfterRejectTransaction:Subscribe(THIS-OBJECT:UserGroupAdapter_AfterRejectTransaction).
        THIS-OBJECT:UserGroupAdapter:SmartTransactionStateChanged:Subscribe(THIS-OBJECT:UserGroupAdapter_SmartTransactionStateChanged).
        THIS-OBJECT:UserGroupAdapter:AfterRetrieveData:Subscribe(THIS-OBJECT:UserGroupAdapter_AfterRetrieveData).
        THIS-OBJECT:UserGroupAdapter:BeforeRetrieveData:Subscribe(THIS-OBJECT:UserGroupAdapter_BeforeRetrieveData).
        /*  */
        /* ultraPanel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraPanel1, "ultraPanel1":U).
        /*  */
        /* ultraPanel1.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ButtonAssignUserToGroup).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ButtonUnAssignUserFromGroup).
        THIS-OBJECT:ultraPanel1:Name = "ultraPanel1":U.
        /*  */
        /* ButtonAssignUserToGroup */
        /*  */
        appearance25:Image = CAST(resources:GetObject("appearance25.Image":U), System.Object).
        appearance25:ImageHAlign = Infragistics.Win.HAlign:Center.
        appearance25:ImageVAlign = Infragistics.Win.VAlign:Middle.
        THIS-OBJECT:ButtonAssignUserToGroup:Appearance = appearance25.
        resources:ApplyResources(THIS-OBJECT:ButtonAssignUserToGroup, "ButtonAssignUserToGroup":U).
        THIS-OBJECT:ButtonAssignUserToGroup:Name = "ButtonAssignUserToGroup":U.
        THIS-OBJECT:ButtonAssignUserToGroup:Click:Subscribe(THIS-OBJECT:ButtonAssignUserToGroup_Click).
        /*  */
        /* ButtonUnAssignUserFromGroup */
        /*  */
        appearance26:Image = CAST(resources:GetObject("appearance26.Image":U), System.Object).
        appearance26:ImageHAlign = Infragistics.Win.HAlign:Center.
        appearance26:ImageVAlign = Infragistics.Win.VAlign:Middle.
        THIS-OBJECT:ButtonUnAssignUserFromGroup:Appearance = appearance26.
        resources:ApplyResources(THIS-OBJECT:ButtonUnAssignUserFromGroup, "ButtonUnAssignUserFromGroup":U).
        THIS-OBJECT:ButtonUnAssignUserFromGroup:Name = "ButtonUnAssignUserFromGroup":U.
        THIS-OBJECT:ButtonUnAssignUserFromGroup:Click:Subscribe(THIS-OBJECT:ButtonUnAssignUserFromGroup_Click).
        /*  */
        /* SmartUserMaintenanceForm_Fill_Panel */
        /*  */
        /*  */
        /* SmartUserMaintenanceForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:SmartUserMaintenanceForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:SmartUserMaintenanceForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:ultraSplitter1).
        THIS-OBJECT:SmartUserMaintenanceForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:smartDataBrowser1).
        THIS-OBJECT:SmartUserMaintenanceForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        resources:ApplyResources(THIS-OBJECT:SmartUserMaintenanceForm_Fill_Panel, "SmartUserMaintenanceForm_Fill_Panel":U).
        THIS-OBJECT:SmartUserMaintenanceForm_Fill_Panel:Name = "SmartUserMaintenanceForm_Fill_Panel":U.
        /*  */
        /* ultraTabControl1 */
        /*  */
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl2).
        resources:ApplyResources(THIS-OBJECT:ultraTabControl1, "ultraTabControl1":U).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        appearance27:Image = CAST(resources:GetObject("appearance27.Image":U), System.Object).
        ultraTab1:Appearance = appearance27.
        ultraTab1:Key = "User":U.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        resources:ApplyResources(ultraTab1, "ultraTab1":U).
        ultraTab1:ForceApplyResources = "":U.
        appearance28:Image = CAST(resources:GetObject("appearance28.Image":U), System.Object).
        ultraTab2:Appearance = appearance28.
        ultraTab2:Key = "GroupAssignement":U.
        ultraTab2:TabPage = THIS-OBJECT:ultraTabPageControl2.
        resources:ApplyResources(ultraTab2, "ultraTab2":U).
        ultraTab2:ForceApplyResources = "":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar21 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 2 NO-UNDO.
        arrayvar21[1] = ultraTab1.
        arrayvar21[2] = ultraTab2.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar21).
        THIS-OBJECT:ultraTabControl1:ViewStyle = Infragistics.Win.UltraWinTabControl.ViewStyle:Office2007.
        THIS-OBJECT:ultraTabControl1:SelectedTabChanged:Subscribe(THIS-OBJECT:ultraTabControl1_SelectedTabChanged).
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTabSharedControlsPage1, "ultraTabSharedControlsPage1":U).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        /*  */
        /* ultraSplitter1 */
        /*  */
        THIS-OBJECT:ultraSplitter1:BackColor = System.Drawing.SystemColors:Control.
        resources:ApplyResources(THIS-OBJECT:ultraSplitter1, "ultraSplitter1":U).
        THIS-OBJECT:ultraSplitter1:Name = "ultraSplitter1":U.
        THIS-OBJECT:ultraSplitter1:RestoreExtent = 0.
        /*  */
        /* smartDataBrowser1 */
        /*  */
        THIS-OBJECT:smartDataBrowser1:DataSource = THIS-OBJECT:UserBindingSource.
        appearance29:BackColor = System.Drawing.SystemColors:Window.
        appearance29:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Appearance = appearance29.
        ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn1:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn1:Header:VisiblePosition = 0.
        ultraGridColumn1:Hidden = TRUE.
        ultraGridColumn2:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn2:Header, "ultraGridColumn2.Header":U).
        ultraGridColumn2:Header:VisiblePosition = 1.
        ultraGridColumn2:ForceApplyResources = "Header":U.
        ultraGridColumn3:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn3:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn3:Header:VisiblePosition = 3.
        ultraGridColumn3:Hidden = TRUE.
        ultraGridColumn4:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn4:Header, "ultraGridColumn4.Header":U).
        ultraGridColumn4:Header:VisiblePosition = 4.
        ultraGridColumn4:ForceApplyResources = "Header":U.
        ultraGridColumn5:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn5:Header, "ultraGridColumn5.Header":U).
        ultraGridColumn5:Header:VisiblePosition = 5.
        ultraGridColumn5:ForceApplyResources = "Header":U.
        ultraGridColumn6:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn6:Header, "ultraGridColumn6.Header":U).
        ultraGridColumn6:Header:VisiblePosition = 6.
        ultraGridColumn6:Hidden = TRUE.
        ultraGridColumn6:ForceApplyResources = "Header":U.
        ultraGridColumn17:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn17:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        resources:ApplyResources(ultraGridColumn17:Header, "ultraGridColumn17.Header":U).
        ultraGridColumn17:Header:VisiblePosition = 7.
        ultraGridColumn17:Hidden = TRUE.
        ultraGridColumn17:ForceApplyResources = "Header":U.
        ultraGridColumn18:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn18:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        resources:ApplyResources(ultraGridColumn18:Header, "ultraGridColumn18.Header":U).
        ultraGridColumn18:Header:VisiblePosition = 8.
        ultraGridColumn18:Hidden = TRUE.
        ultraGridColumn18:ForceApplyResources = "Header":U.
        ultraGridColumn19:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn19:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn19:Header:VisiblePosition = 9.
        ultraGridColumn19:Hidden = TRUE.
        ultraGridColumn20:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn20:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn20:Header:VisiblePosition = 10.
        ultraGridColumn20:Hidden = TRUE.
        ultraGridColumn7:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn7:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        resources:ApplyResources(ultraGridColumn7:Header, "ultraGridColumn7.Header":U).
        ultraGridColumn7:Header:VisiblePosition = 11.
        ultraGridColumn7:ForceApplyResources = "Header":U.
        ultraGridColumn21:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn21:Header:VisiblePosition = 12.
        ultraGridColumn21:Hidden = TRUE.
        ultraGridColumn22:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn22:Header:VisiblePosition = 13.
        ultraGridColumn23:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn23:Header:VisiblePosition = 15.
        ultraGridColumn23:Hidden = TRUE.
        ultraGridColumn24:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn24:Header, "ultraGridColumn24.Header":U).
        ultraGridColumn24:Header:VisiblePosition = 14.
        ultraGridColumn24:ForceApplyResources = "Header":U.
        ultraGridColumn25:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        resources:ApplyResources(ultraGridColumn25:Header, "ultraGridColumn25.Header":U).
        ultraGridColumn25:Header:VisiblePosition = 2.
        ultraGridColumn25:ForceApplyResources = "Header":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar22 AS System.Object EXTENT 16 NO-UNDO.
        arrayvar22[1] = ultraGridColumn1.
        arrayvar22[2] = ultraGridColumn2.
        arrayvar22[3] = ultraGridColumn3.
        arrayvar22[4] = ultraGridColumn4.
        arrayvar22[5] = ultraGridColumn5.
        arrayvar22[6] = ultraGridColumn6.
        arrayvar22[7] = ultraGridColumn17.
        arrayvar22[8] = ultraGridColumn18.
        arrayvar22[9] = ultraGridColumn19.
        arrayvar22[10] = ultraGridColumn20.
        arrayvar22[11] = ultraGridColumn7.
        arrayvar22[12] = ultraGridColumn21.
        arrayvar22[13] = ultraGridColumn22.
        arrayvar22[14] = ultraGridColumn23.
        arrayvar22[15] = ultraGridColumn24.
        arrayvar22[16] = ultraGridColumn25.
        ultraGridBand3:Columns:AddRange(arrayvar22).
        ultraGridBand3:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand3:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand3:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand3:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:BandsSerializer:Add(ultraGridBand3).
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance30:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance30:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance30:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:Appearance = appearance30.
        appearance31:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance31.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance32:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance32:BackColor2 = System.Drawing.SystemColors:Control.
        appearance32:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance32:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:PromptAppearance = appearance32.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:MaxRowScrollRegions = 1.
        appearance33:BackColor = System.Drawing.SystemColors:Window.
        appearance33:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:ActiveCellAppearance = appearance33.
        appearance34:BackColor = System.Drawing.SystemColors:Highlight.
        appearance34:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:ActiveRowAppearance = appearance34.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance35:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CardAreaAppearance = appearance35.
        appearance36:BorderColor = System.Drawing.Color:Silver.
        appearance36:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellAppearance = appearance36.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellPadding = 0.
        appearance37:BackColor = System.Drawing.SystemColors:Control.
        appearance37:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance37:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance37:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance37:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:GroupByRowAppearance = appearance37.
        resources:ApplyResources(appearance38, "appearance38":U).
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderAppearance = appearance38.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance39:BackColor = System.Drawing.SystemColors:Window.
        appearance39:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:RowAppearance = appearance39.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance40:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:TemplateAddRowAppearance = appearance40.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        resources:ApplyResources(THIS-OBJECT:smartDataBrowser1, "smartDataBrowser1":U).
        THIS-OBJECT:smartDataBrowser1:LinkDataSource = THIS-OBJECT:UserAdapter.
        THIS-OBJECT:smartDataBrowser1:Name = "smartDataBrowser1":U.
        THIS-OBJECT:smartDataBrowser1:BeforeSelectChange:Subscribe(THIS-OBJECT:smartDataBrowser1_BeforeSelectChange).
        /*  */
        /* UserBindingSource */
        /*  */
        THIS-OBJECT:UserBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:UserBindingSource:EntityName = "Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U.
        THIS-OBJECT:UserBindingSource:EntityTable = "eSmartUser":U.
        THIS-OBJECT:UserBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:UserBindingSource:NoLOBs = FALSE.
        tableDesc3:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar23 AS Progress.Data.ColumnPropDesc EXTENT 16 NO-UNDO.
        arrayvar23[1] = NEW Progress.Data.ColumnPropDesc("UserGuid":U, "UserGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar23[2] = NEW Progress.Data.ColumnPropDesc("UserName":U, "UserName":U, Progress.Data.DataType:CHARACTER).
        arrayvar23[3] = NEW Progress.Data.ColumnPropDesc("LanguageGuid":U, "LanguageGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar23[4] = NEW Progress.Data.ColumnPropDesc("UserFullName":U, "UserFullName":U, Progress.Data.DataType:CHARACTER).
        arrayvar23[5] = NEW Progress.Data.ColumnPropDesc("UserEmail":U, "UserEmail":U, Progress.Data.DataType:CHARACTER).
        arrayvar23[6] = NEW Progress.Data.ColumnPropDesc("UserPassword":U, "UserPassword":U, Progress.Data.DataType:CHARACTER).
        arrayvar23[7] = NEW Progress.Data.ColumnPropDesc("UserPasswordChangedDate":U, "UserPasswordChangedDate":U, Progress.Data.DataType:DATE).
        arrayvar23[8] = NEW Progress.Data.ColumnPropDesc("UserLastLogin":U, "UserLastLogin":U, Progress.Data.DataType:DATETIME_TZ).
        arrayvar23[9] = NEW Progress.Data.ColumnPropDesc("LoginCompanyGuid":U, "LoginCompanyGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar23[10] = NEW Progress.Data.ColumnPropDesc("ManagerUserGuid":U, "ManagerUserGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar23[11] = NEW Progress.Data.ColumnPropDesc("UserSecurityId":U, "User Security Id":U, Progress.Data.DataType:INTEGER).
        arrayvar23[12] = NEW Progress.Data.ColumnPropDesc("LanguageIsoCode":U, "Language ISO Code":U, Progress.Data.DataType:CHARACTER).
        arrayvar23[13] = NEW Progress.Data.ColumnPropDesc("LanguageName":U, "Language Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar23[14] = NEW Progress.Data.ColumnPropDesc("ManagerUserName":U, "Manager Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar23[15] = NEW Progress.Data.ColumnPropDesc("ManagerUserFullName":U, "Manager FullName":U, Progress.Data.DataType:CHARACTER).
        arrayvar23[16] = NEW Progress.Data.ColumnPropDesc("LoginCompanyName":U, "LoginCompanyName":U, Progress.Data.DataType:CHARACTER).
        tableDesc3:Columns = arrayvar23.
        THIS-OBJECT:UserBindingSource:TableSchema = tableDesc3.
        /*  */
        /* m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Left, "m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Left":U).
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Left:Name = "m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Right, "m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Right":U).
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Right:Name = "m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        resources:ApplyResources(THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Top, "m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Top":U).
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Top:Name = "m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
        resources:ApplyResources(THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Bottom, "m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Bottom":U).
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Bottom:Name = "m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* SmartUserMaintenanceForm */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:SmartUserMaintenanceForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartUserMaintenanceForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Name = "SmartUserMaintenanceForm":U.
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartUserViewer1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:UserAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl2:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartDataBrowser3, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:PossibleAssignementsBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:PossibleAssignementsTableAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartDataBrowser2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:UserGroupBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:UserGroupAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraPanel1:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ResumeLayout(FALSE).
        THIS-OBJECT:SmartUserMaintenanceForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:SmartUserMaintenanceForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:UserBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Override of the OnShown Method to first show the form and then 
                 initiate reading data.
        Notes:   
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnShown (e AS System.EventArgs):
        
        SUPER:OnShown (e).

        Consultingwerk.Util.UserInterfaceHelper:ProcessEvents() .

        UserAdapter:RetrieveData ().

        IF VALID-OBJECT (UserGroupAdapter:DatasetController) AND 
           TYPE-OF (UserGroupAdapter:DatasetController, Consultingwerk.SmartFramework.Authorization.UserGroupDatasetController) THEN 
            oUserGroupDatasetController = CAST (UserGroupAdapter:DatasetController, Consultingwerk.SmartFramework.Authorization.UserGroupDatasetController).
        
        PROCESS EVENTS.
        
    END METHOD .

	/*------------------------------------------------------------------------------
		Purpose: Set Assignment buttons en or disabled according to the availability
		         of data to be assigned
		Notes:   
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID SetAssignmentButtonState ():
		
		ASSIGN 
    		ButtonAssignUserToGroup:Enabled     = PossibleAssignementsBindingSource:Count > 0
            ButtonUnAssignUserFromGroup:Enabled = UserGroupBindingSource:Count > 0
            .
		
	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Save or reject pending changes of the UserGroupAdapter if required
        Notes:   
        @param sender The source of the event
        @param e An EventArgs that contains the data for the event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartDataBrowser1_BeforeSelectChange (sender AS System.Object, 
                                                              e      AS Infragistics.Win.UltraWinGrid.BeforeSelectChangeEventArgs):
        
        DEFINE VARIABLE oMessageProvider AS Consultingwerk.SmartFramework.IMessageProvider NO-UNDO.
        DEFINE VARIABLE oMessage         AS Consultingwerk.SmartFramework.Message          NO-UNDO.
        
        IF UserGroupAdapter:SmartTransactionState <> TransactionStateEnum:PendingChanges THEN RETURN.
            
        oMessageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IMessageProvider} .
        IF VALID-OBJECT (oMessageProvider) THEN 
        DO ON ERROR UNDO, THROW:
            oMessage = oMessageProvider:GetMessage ("SFR":U, 2) .
            IF MessageFormHelper:ShowMessage (oMessage) = DialogResultEnum:DialogResultYes THEN DO:
                UserGroupAdapter:CommitTransaction ().
            END.
            ELSE DO:
                UserGroupAdapter:RejectTransaction ().
            END.
            CATCH err AS Progress.Lang.Error :
                e:Cancel = TRUE.
                ErrorHelper:ShowErrorMessage (err) .    
            END CATCH.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Assign or unassign a Group from a User
        Notes:   
        @param sender The reference to the Instance of the SmartDataBrowser which has invoked the Method
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SmartDataBrowserDefaultActionHandler (sender AS SmartDataBrowser, 
                                                             e      AS System.EventArgs):

        CASE sender:
            WHEN smartDataBrowser2 THEN ButtonUnAssignUserFromGroup_Click (sender, e).
            WHEN smartDataBrowser3 THEN ButtonAssignUserToGroup_Click (sender, e).
        END CASE.        
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: When switching to the UserAssignement tab the contextual tab for
                 this purpose gets shown
        Notes:   
        @param sender Reference to the calling Object
        @param e An EventArgs that contains the SelectedTabChanged event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTabControl1_SelectedTabChanged (sender AS System.Object, 
                                                             e      AS Infragistics.Win.UltraWinTabControl.SelectedTabChangedEventArgs):
        
        IF e:Tab:Key = "GroupAssignement":U THEN DO:
            smartToolbarController1:Ribbon:ContextualTabGroups["GroupAssignement":U]:Visible = TRUE.
            smartToolbarController1:Ribbon:SelectedTab = smartToolbarController1:Ribbon:Tabs["GroupAssignement":U].
        END.
        ELSE 
            smartToolbarController1:Ribbon:ContextualTabGroups["GroupAssignement":U]:Visible = FALSE.
            
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Refresh the Assignment button sensitivity after rejecting changes                                                                    
        Notes:                                                                        
        @param sender Reference to the calling Object
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID UserGroupAdapter_AfterRejectTransaction (sender AS System.Object,
                                                                 e      AS System.EventArgs):
        
        /* Marko Rüterbories, Consultingwerk Ltd. 12.10.2013
           Refresh Assignment button sensitivity */
        SetAssignmentButtonState ().

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the PossibleAssignement Browser after new Data has been 
                 read by the UserGroupAdapter
        Notes:   
        @param sender Reference to the calling Object
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID UserGroupAdapter_AfterRetrieveData (sender AS System.Object, 
                                                            e      AS System.EventArgs):
        
        IF NOT VALID-HANDLE (PossibleAssignementsTableAdapter:BufferHandle) AND  
           VALID-HANDLE (UserGroupAdapter:DataSet) THEN 
            PossibleAssignementsTableAdapter:TempTableHandle = UserGroupAdapter:DataSet::ePossibleAssignments.
    
        PossibleAssignementsTableAdapter:RetrieveData ().
        
        /* Marko Rüterbories, Consultingwerk Ltd. 12.10.2013
           Refresh Assignment button sensitivity */
        SetAssignmentButtonState ().
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets then CustomerContext just before executing the RetrieveData
                 Method of the UserGroupAdapter
        Notes:   
        @param sender Reference to the calling Object
        @param e An EventArgs that contains the Cancelable event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID UserGroupAdapter_BeforeRetrieveData (sender AS System.Object, 
                                                             e      AS System.ComponentModel.CancelEventArgs):
        
        UserGroupAdapter:CustomContext = SUBSTITUTE ("LoginCompany=&1|GroupAssignment=yes":U,
                                                     UserAdapter:GetFieldValues("LoginCompanyGuid":U)).
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Makes sure that the PossibleAssignement Browser shows the correct
                 data
        Notes:   
        @param sender Reference to the calling Object
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID UserGroupAdapter_SmartTransactionStateChanged (sender AS System.Object,
                                                                       e      AS System.EventArgs):
        
        IF lPendingChangesInUserGroupAssignement AND 
           UserGroupAdapter:SmartTransactionState = TransactionStateEnum:NoPendingChanges THEN 
            PossibleAssignementsTableAdapter:RetrieveData ().

        /* Marko Rüterbories, Consultingwerk Ltd. 12.10.2013
           Refresh Assignment button sensitivity */
        SetAssignmentButtonState ().
        
        FINALLY:
            lPendingChangesInUserGroupAssignement = FALSE.
        END FINALLY.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartUserMaintenanceForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartUserMaintenanceForm ():

    END DESTRUCTOR .

END CLASS.
