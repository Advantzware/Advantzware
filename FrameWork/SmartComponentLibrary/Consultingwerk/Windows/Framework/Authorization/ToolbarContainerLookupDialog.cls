/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ToolbarContainerLookupDialog
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Apr 20 12:20:26 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.*                           FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Consultingwerk.Windows.Framework.Authorization.* FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*              FROM ASSEMBLY .
USING Infragistics.Win.UltraWinTree.*                  FROM ASSEMBLY .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Authorization.ToolbarContainerLookupDialog INHERITS BaseForm:


    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton2 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTree1 AS Infragistics.Win.UltraWinTree.UltraTree NO-UNDO.

    DEFINE VARIABLE oToolbars AS "System.Collections.Generic.Dictionary<System.String,UltraToolbarsManager>" NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the selected object
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SelectedObject AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the selected UltraToolbarsManager
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SelectedToolbarController AS UltraToolbarsManager NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the type of the ToolbarContainer
        Notes:   Form / TableIOTarget
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ToolbarContainerType AS ToolbarContainerTypeEnum NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ToolbarContainerLookupDialog class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ToolbarContainerLookupDialog ():

        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
        &ENDIF

        ASSIGN oToolbars = NEW "System.Collections.Generic.Dictionary<System.String,UltraToolbarsManager>" () .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds the TableIO Targets of a Form to the UltraTreeNode for the Form
        Notes:   Recursive method
        @param poControl The current parent control
        @param poFormNode The UltraTreeNode for the current Form
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AddTableIoTargets (poControl AS System.Windows.Forms.Control,
                                             poFormNode AS UltraTreeNode):

        DEFINE VARIABLE oTarget AS Consultingwerk.SmartComponents.Interfaces.ISmartTableIOTarget NO-UNDO .
         DEFINE VARIABLE cKey    AS CHARACTER                                                     NO-UNDO .
         DEFINE VARIABLE oNode   AS UltraTreeNode                                                 NO-UNDO .

        {Consultingwerk/foreach.i System.Windows.Forms.Control oControl in poControl:Controls}

            IF TYPE-OF (oControl, System.Windows.Forms.Form) THEN
                NEXT .

            IF TYPE-OF (oControl, Consultingwerk.SmartComponents.Interfaces.ISmartTableIOTarget) THEN DO:

                oTarget = CAST (oControl, Consultingwerk.SmartComponents.Interfaces.ISmartTableIOTarget) .

                IF oTarget:SecurityKey > "":U THEN
                    ASSIGN cKey = oTarget:SecurityKey .
                ELSE
&IF NOT PROVERSION EQ "10.2B":U &THEN
                    ASSIGN cKey = oTarget:GetClass():TypeName .
&ELSE
                    /* Mike Fechner, Consultingwerk Ltd. 14.03.2015
                       GetClass():TypeName known to crash the AVM sometimes on 10.2B,
                       so use GetType():FullName instead */
                    IF TYPE-OF (oTarget, System.Object) THEN
                        cKey = CAST (CAST (oTarget, Progress.Lang.Object), System.Object):GetType():FullName .
                    ELSE
                        cKey = "":U .
&ENDIF

                IF cKey > "":U AND NOT poFormNode:Nodes:Exists (cKey) THEN DO:
                    oNode = poFormNode:Nodes:Add (GUID, cKey) .
                    oNode:LeftImages:Add (ImageHelper:Load("Consultingwerk/Windows/Framework/Images/SmartFramework/edit_16.png":U)) .
                    oNode:Tag = "TableIOTarget":U .
                END.
            END.
            ELSE
                THIS-OBJECT:AddTableIoTargets (oControl, poFormNode) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Creates or locates an UltarTreeNode for the given Package
        Notes:
        @param pcPackage The name of the package to create a node for
        @return The UltraTreeNode that references the package
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED UltraTreeNode CreatePackageNode (pcPackage AS CHARACTER):

        DEFINE VARIABLE oNode  AS UltraTreeNode       NO-UNDO .
        DEFINE VARIABLE cEntry AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cKey   AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE i      AS INTEGER             NO-UNDO .

        DEFINE VARIABLE oNodes AS TreeNodesCollection NO-UNDO .

        IF pcPackage > "":U THEN .
        ELSE
            RETURN ? .

        oNodes = ultraTree1:Nodes .

        DO i = 1 TO NUM-ENTRIES (pcPackage, ".":U):

            ASSIGN cEntry = ENTRY (i, pcPackage, ".":U)
                   cKey   = TRIM (cKey + ".":U + cEntry, ".":U) .

            IF oNodes:Exists (cKey) THEN
                ASSIGN oNode  = oNodes[cKey]
                       oNodes = oNode:Nodes .
            ELSE DO:
                ASSIGN oNode  = oNodes:Add (cKey, cEntry)
                       oNodes = oNode:Nodes .

                oNode:LeftImages:Add (ImageHelper:Load("Consultingwerk/Windows/Framework/Images/SmartFramework/folder_16.png":U)) .
            END.
        END.

        RETURN oNode.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the Tree View Control
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FillTree ():

        DEFINE VARIABLE oObject  AS Progress.Lang.Object      NO-UNDO .
        DEFINE VARIABLE oForm    AS System.Windows.Forms.Form NO-UNDO .
        DEFINE VARIABLE oToolbar AS UltraToolbarsManager      NO-UNDO .

        DEFINE VARIABLE oPackageNode AS UltraTreeNode NO-UNDO .
        DEFINE VARIABLE oFormNode    AS UltraTreeNode NO-UNDO .

        ASSIGN oObject = SESSION:FIRST-OBJECT .

        DO WHILE VALID-OBJECT (oObject) ON ERROR UNDO, THROW:

            IF NOT TYPE-OF (oObject, System.Windows.Forms.Form) THEN
                NEXT .

            oForm = CAST (oObject, System.Windows.Forms.Form) .

            oToolbar = UltraToolbarsManager:FromForm (oForm) .

            IF NOT VALID-OBJECT (oToolbar) THEN
                NEXT .

            IF NOT TYPE-OF (oToolbar, Consultingwerk.SmartComponents.Implementation.SmartToolbarController) THEN
                NEXT .

            oPackageNode = THIS-OBJECT:CreatePackageNode (oForm:GetClass():Package) .

            IF VALID-OBJECT (oPackageNode) AND NOT oPackageNode:Nodes:Exists (oForm:GetClass():TypeName) THEN DO:
                oToolbars:Add (oForm:GetClass():TypeName, oToolbar) .

                oFormNode = oPackageNode:Nodes:Add (oForm:GetClass():TypeName, ClassHelper:ShortClassName (oForm:GetClass())) .
                oFormNode:Tag = "Form":U .

                oFormNode:LeftImages:Add (ImageHelper:Load("Consultingwerk/Windows/Framework/Images/SmartFramework/window_16.png":U)) .

                THIS-OBJECT:AddTableIoTargets (oForm, oFormNode) .
            END.

            FINALLY:
                oObject = oObject:NEXT-SIBLING .
            END FINALLY.
        END.

    END METHOD .

    /**
     * Purpose: Initializes the Visual Design
     * Notes:
     */
    @InitializeComponent.
    METHOD PRIVATE VOID InitializeComponent ():

        /* NOTE: The following method is automatically generated.

        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.

        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Authorization.ToolbarContainerLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE m_override1 AS Infragistics.Win.UltraWinTree.Override NO-UNDO.
        m_override1 = NEW Infragistics.Win.UltraWinTree.Override().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButton2 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraTree1 = NEW Infragistics.Win.UltraWinTree.UltraTree().
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraButton1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraButton1, "ultraButton1":U).
        THIS-OBJECT:ultraButton1:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Click:Subscribe(THIS-OBJECT:ultraButton1_Click).
        /*  */
        /* ultraButton2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraButton2, "ultraButton2":U).
        THIS-OBJECT:ultraButton2:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ultraButton2:Name = "ultraButton2":U.
        /*  */
        /* ultraTree1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTree1, "ultraTree1":U).
        THIS-OBJECT:ultraTree1:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:ultraTree1:DisplayStyle = Infragistics.Win.UltraWinTree.UltraTreeDisplayStyle:WindowsVista.
        THIS-OBJECT:ultraTree1:HideExpansionIndicators = Infragistics.Win.UltraWinTree.HideExpansionIndicators:OnMouseLeave.
        THIS-OBJECT:ultraTree1:HideSelection = FALSE.
        THIS-OBJECT:ultraTree1:Name = "ultraTree1":U.
        m_override1:HotTracking = Infragistics.Win.DefaultableBoolean:True.
        m_override1:SelectionType = Infragistics.Win.UltraWinTree.SelectType:Single.
        THIS-OBJECT:ultraTree1:Override = m_override1.
        THIS-OBJECT:ultraTree1:AfterSelect:Subscribe(THIS-OBJECT:ultraTree1_AfterSelect).
        /*  */
        /* ToolbarContainerLookupDialog */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTree1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:Name = "ToolbarContainerLookupDialog":U.
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        SUPER:OnShown (e) .

        PROCESS EVENTS .

        SESSION:SET-WAIT-STATE ("GENERAL":U) .

        THIS-OBJECT:FillTree () .

        SESSION:SET-WAIT-STATE ("":U) .

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event ultraButton1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButton1_Click (sender AS System.Object,
                                            e AS System.EventArgs):

        DEFINE VARIABLE cTag AS CHARACTER NO-UNDO.

        IF ultraTree1:SelectedNodes:Count = 0 THEN
            RETURN .

        ASSIGN cTag = UNBOX (ultraTree1:SelectedNodes[0]:Tag) .

        CASE cTag:
            WHEN "Form":U THEN DO:
                ASSIGN THIS-OBJECT:SelectedObject       = ultraTree1:SelectedNodes[0]:Key
                       THIS-OBJECT:ToolbarContainerType = ToolbarContainerTypeEnum:Form .

                IF oToolbars:ContainsKey (ultraTree1:SelectedNodes[0]:Key) THEN
                    ASSIGN THIS-OBJECT:SelectedToolbarController = oToolbars [ultraTree1:SelectedNodes[0]:Key] .
            END.
            WHEN "TableIOTarget":U THEN
                ASSIGN THIS-OBJECT:SelectedObject       = ultraTree1:SelectedNodes[0]:Text
                       THIS-OBJECT:ToolbarContainerType = ToolbarContainerTypeEnum:TableIOTarget .
        END CASE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterSelect evnet of the ultraTree1 instance
        Notes:
        @param sender The reference to the object that raised the event
        @param e The SelectEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTree1_AfterSelect (sender AS System.Object,
                                                e AS Infragistics.Win.UltraWinTree.SelectEventArgs):

        DEFINE VARIABLE cTag AS CHARACTER NO-UNDO.

        IF ultraTree1:SelectedNodes:Count = 0 THEN
            ultraButton1:Enabled = FALSE .
        ELSE DO:
            ASSIGN cTag = UNBOX (ultraTree1:SelectedNodes[0]:Tag) .

            IF cTag = "Form":U OR cTag = "TableIOTarget":U THEN
                ultraButton1:Enabled = TRUE.
            ELSE
                ultraButton1:Enabled = FALSE .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ToolbarContainerLookupDialog class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ToolbarContainerLookupDialog ():

        oToolbars:Clear() .

        GarbageCollectorHelper:DeleteObject (oToolbars) .
        GarbageCollectorHelper:ClearUltraTreeNodes (ultraTree1) .

    END DESTRUCTOR .

END CLASS.
