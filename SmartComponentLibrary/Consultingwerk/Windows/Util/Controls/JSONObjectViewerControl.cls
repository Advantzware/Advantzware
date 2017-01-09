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
    File        : JSONObjectViewerControl
    Purpose     : Reads a Json Construct and Visualizes the content in a
                  TreeView
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Tue Nov 29 18:28:35 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Util.*         FROM PROPATH.
USING Consultingwerk.Windows.Util.* FROM PROPATH.
USING Progress.Json.ObjectModel.*   FROM PROPATH.
USING Progress.Lang.*               FROM PROPATH.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Windows.Util.Controls.JSONObjectViewerControl
    INHERITS Progress.Windows.UserControl:

    DEFINE PRIVATE VARIABLE ultraTree1 AS Infragistics.Win.UltraWinTree.UltraTree NO-UNDO.

    DEFINE PRIVATE VARIABLE oUltraTreeColumnResizeHelper AS UltraTreeColumnResizeHelper NO-UNDO.
    DEFINE PRIVATE VARIABLE lHideSerializeType           AS LOGICAL                     NO-UNDO.

    /**
     * Purpose: Constructor for the BusinessEntityDesignerForm class
     * Notes:
     */
    CONSTRUCTOR PUBLIC JSONObjectViewerControl():

        InitializeComponent ().

        THIS-OBJECT:InitTreeHelper().

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /**
     * Purpose: Visializes an Json Construct in a TreeView
     * Notes:
     * @param poJsonObject The JsonConstruct to visualize
     */
    METHOD PUBLIC VOID DisplayJsonObject (poJsonObject AS JsonConstruct):

        THIS-OBJECT:DisplayJsonObject (poJsonObject,
                                       FALSE).

    END METHOD.

    /**
     * Purpose: Visializes an Json Construct in a TreeView
     * Notes:
     * @param poJsonObject The JsonObject to visualize
     * @param plHideSerializeType Switch to Show/Hide the Consultingwerk Serialize Attribute in Json objects
     */
    METHOD PUBLIC VOID DisplayJsonObject (poJsonObject        AS Progress.Json.ObjectModel.JsonConstruct,
                                          plHideSerializeType AS LOGICAL):

        DEFINE VARIABLE oNode AS Infragistics.Win.UltraWinTree.UltraTreeNode  NO-UNDO.

        lHideSerializeType = plHideSerializeType.

        GarbageCollectorHelper:ClearUltraTreeNodes (ultraTree1) .

        ASSIGN
            oNode = ultraTree1:Nodes:Add("Root":U).

        oNode:Cells ["PropertyLabel":U]:Value = BOX ("Attribute":U) .
        oNode:Cells ["PropertyValue":U]:Value = BOX ("Value":U) .

        IF TYPE-OF(poJsonObject, "Progress.Json.ObjectModel.JsonObject":U) THEN
            THIS-OBJECT:WalkthroughJSON (oNode, CAST(poJsonObject, JsonObject)).

        ELSE IF TYPE-OF(poJsonObject, "Progress.Json.ObjectModel.JsonArray":U) THEN
            THIS-OBJECT:WalkthroughJSON (oNode, CAST(poJsonObject, JsonArray)).

        oNode:ExpandAll().

    END METHOD.

    /**
     * Purpose: Initializes the TreeView and the UltraTreeColumnResizeHelper
     * Notes:
     */
    METHOD PRIVATE VOID InitTreeHelper():

        Consultingwerk.Util.GarbageCollectorHelper:ClearUltraTreeNodes (THIS-OBJECT:ultraTree1) .

        THIS-OBJECT:ultraTree1:Nodes:Clear () .

        /* Initialize resizing for NodeColumnSets */
        oUltraTreeColumnResizeHelper = NEW UltraTreeColumnResizeHelper (THIS-OBJECT:ultraTree1,
                                                                        175,
                                                                        "PropertyLabel":U).

    END METHOD.

    /**
     * Purpose: Initializes the Visual Design
     * Notes:
     */
    @InitializeComponent.
    METHOD PRIVATE VOID InitializeComponent(  ):

        /* NOTE: The following method is automatically generated.

        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.

        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeColumnSet1 AS Infragistics.Win.UltraWinTree.UltraTreeColumnSet NO-UNDO.
        ultraTreeColumnSet1 = NEW Infragistics.Win.UltraWinTree.UltraTreeColumnSet().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNodeColumn1 AS Infragistics.Win.UltraWinTree.UltraTreeNodeColumn NO-UNDO.
        ultraTreeNodeColumn1 = NEW Infragistics.Win.UltraWinTree.UltraTreeNodeColumn().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNodeColumn2 AS Infragistics.Win.UltraWinTree.UltraTreeNodeColumn NO-UNDO.
        ultraTreeNodeColumn2 = NEW Infragistics.Win.UltraWinTree.UltraTreeNodeColumn().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE m_override1 AS Infragistics.Win.UltraWinTree.Override NO-UNDO.
        m_override1 = NEW Infragistics.Win.UltraWinTree.Override().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode1 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode1 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode2 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode2 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode3 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode3 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode4 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode4 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode5 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode5 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode6 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode6 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        THIS-OBJECT:ultraTree1 = NEW Infragistics.Win.UltraWinTree.UltraTree().
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTree1 */
        /*  */
        THIS-OBJECT:ultraTree1:ColumnSettings:AllowCellEdit = Infragistics.Win.UltraWinTree.AllowCellEdit:ReadOnly.
        THIS-OBJECT:ultraTree1:ColumnSettings:AllowCellSizing = Infragistics.Win.UltraWinTree.LayoutSizing:Both.
        THIS-OBJECT:ultraTree1:ColumnSettings:AutoGenerateColumnSets = FALSE.
        THIS-OBJECT:ultraTree1:ColumnSettings:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:ultraTree1:ColumnSettings:BorderStyleColumnHeader = Infragistics.Win.UIElementBorderStyle:None.
        THIS-OBJECT:ultraTree1:ColumnSettings:ColumnAutoSizeMode = Infragistics.Win.UltraWinTree.ColumnAutoSizeMode:VisibleNodes.
        THIS-OBJECT:ultraTree1:ColumnSettings:ColumnHeaderWrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraTreeNodeColumn1:ButtonDisplayStyle = Infragistics.Win.UltraWinTree.ButtonDisplayStyle:Always.
        ultraTreeNodeColumn1:Key = "PropertyLabel":U.
        ultraTreeNodeColumn1:LayoutInfo:PreferredCellSize = NEW System.Drawing.Size(153, 18).
        ultraTreeNodeColumn1:LayoutInfo:PreferredLabelSize = NEW System.Drawing.Size(153, 0).
        ultraTreeNodeColumn1:LayoutInfo:SpanX = 2.
        ultraTreeNodeColumn1:LayoutInfo:SpanY = 2.
        ultraTreeNodeColumn2:ButtonDisplayStyle = Infragistics.Win.UltraWinTree.ButtonDisplayStyle:Always.
        ultraTreeNodeColumn2:Key = "PropertyValue":U.
        ultraTreeNodeColumn2:LayoutInfo:PreferredCellSize = NEW System.Drawing.Size(221, 18).
        ultraTreeNodeColumn2:LayoutInfo:PreferredLabelSize = NEW System.Drawing.Size(221, 0).
        ultraTreeNodeColumn2:LayoutInfo:SpanX = 2.
        ultraTreeNodeColumn2:LayoutInfo:SpanY = 2.
        ultraTreeColumnSet1:Columns:Add(ultraTreeNodeColumn1).
        ultraTreeColumnSet1:Columns:Add(ultraTreeNodeColumn2).
        ultraTreeColumnSet1:Key = "Level-0":U.
        ultraTreeColumnSet1:TipStyleCell = Infragistics.Win.UltraWinTree.TipStyleCell:Show.
        THIS-OBJECT:ultraTree1:ColumnSettings:ColumnSets:Add(ultraTreeColumnSet1).
        THIS-OBJECT:ultraTree1:ColumnSettings:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsVista.
        THIS-OBJECT:ultraTree1:ColumnSettings:LabelPosition = Infragistics.Win.UltraWinTree.NodeLayoutLabelPosition:None.
        THIS-OBJECT:ultraTree1:DisplayStyle = Infragistics.Win.UltraWinTree.UltraTreeDisplayStyle:WindowsVista.
        THIS-OBJECT:ultraTree1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraTree1:FullRowSelect = TRUE.
        THIS-OBJECT:ultraTree1:ImageTransparentColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:ultraTree1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraTree1:Name = "ultraTree1":U.
        THIS-OBJECT:ultraTree1:NodeConnectorColor = System.Drawing.SystemColors:ControlDark.
        m_override1:ColumnSetIndex = 0.
        THIS-OBJECT:ultraTree1:NodeLevelOverrides:Add(m_override1).
        ultraTreeNode2:Text = "":U.
        ultraTreeNode3:Text = "":U.
        ultraTreeNode4:Text = "":U.
        ultraTreeNode5:Text = "":U.
        ultraTreeNode6:Text = "":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTree.UltraTreeNode EXTENT 5 NO-UNDO.
        arrayvar0[1] = ultraTreeNode2.
        arrayvar0[2] = ultraTreeNode3.
        arrayvar0[3] = ultraTreeNode4.
        arrayvar0[4] = ultraTreeNode5.
        arrayvar0[5] = ultraTreeNode6.
        ultraTreeNode1:Nodes:AddRange(arrayvar0).
        ultraTreeNode1:Text = "":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinTree.UltraTreeNode EXTENT 1 NO-UNDO.
        arrayvar1[1] = ultraTreeNode1.
        THIS-OBJECT:ultraTree1:Nodes:AddRange(arrayvar1).
        THIS-OBJECT:ultraTree1:Size = NEW System.Drawing.Size(539, 432).
        THIS-OBJECT:ultraTree1:TabIndex = 0.
        THIS-OBJECT:ultraTree1:UseAppStyling = FALSE.
        THIS-OBJECT:ultraTree1:ViewStyle = Infragistics.Win.UltraWinTree.ViewStyle:Grid.
        /*  */
        /* JSONObjectViewerControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTree1).
        THIS-OBJECT:Name = "JSONObjectViewerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(539, 432).
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /**
     * Purpose: Reads an JsonArray and adds a TreeNode for each of it's elements
     * Notes:
     * @param poParentNode Object reference to the Parent TreeNode
     * @param poJsonArray  Object reference of the current JsonArray object
     */
    METHOD PRIVATE VOID WalkthroughJSON (poParentNode AS Infragistics.Win.UltraWinTree.UltraTreeNode,
                                         poJsonArray AS JsonArray):

        DEFINE VARIABLE iElementIteration AS INTEGER                                     NO-UNDO.
        DEFINE VARIABLE iDataType         AS INTEGER                                     NO-UNDO.
        DEFINE VARIABLE oNode             AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.

        DO iElementIteration = 1 TO poJsonArray:Length:

            IF VALID-OBJECT (poParentNode:Nodes:ParentNode) THEN
                oUltraTreeColumnResizeHelper:InitializeColumnResizing (poParentNode:Nodes:ParentNode:Level + 1).

            ASSIGN
                oNode     = poParentNode:Nodes:Add(GUID)
                oNode:Cells ["PropertyLabel":U]:Value = iElementIteration
                .

            iDataType = poJsonArray:GetType(iElementIteration).

            CASE iDataType:

                WHEN Progress.Json.ObjectModel.JsonDataType:ARRAY THEN DO:
                    ASSIGN oNode:Cells ["PropertyValue":U]:Value = "[ ]":U.
                    THIS-OBJECT:WalkthroughJSON (oNode, poJsonArray:GetJsonArray (iElementIteration)).
                END.

                WHEN Progress.Json.ObjectModel.JsonDataType:OBJECT THEN DO:
                    ASSIGN oNode:Cells ["PropertyValue":U]:Value = "~{ ~}":U.
                    THIS-OBJECT:WalkthroughJSON (oNode, poJsonArray:GetJsonObject(iElementIteration)).
                END.

                WHEN Progress.Json.ObjectModel.JsonDataType:BOOLEAN THEN
                    ASSIGN oNode:Cells ["PropertyValue":U]:Value =  poJsonArray:GetLogical (iElementIteration).

                WHEN Progress.Json.ObjectModel.JsonDataType:NULL THEN
                    ASSIGN oNode:Cells ["PropertyValue":U]:Value = "Null":U.

                WHEN Progress.Json.ObjectModel.JsonDataType:NUMBER THEN
                    ASSIGN oNode:Cells ["PropertyValue":U]:Value =  poJsonArray:GetDecimal (iElementIteration).

                WHEN Progress.Json.ObjectModel.JsonDataType:STRING THEN
                    ASSIGN oNode:Cells ["PropertyValue":U]:Value =  poJsonArray:GetCharacter (iElementIteration).
            END CASE.

        END.

    END METHOD.

    /**
     * Purpose: Reads an JsonObject and adds a TreeNode for each of it's elements
     * Notes:
     * @param poParentNode Object reference to the Parent TreeNode
     * @param poJsonObject Object reference of the current JsonObject object
     */
    METHOD PRIVATE VOID WalkthroughJSON (poParentNode AS Infragistics.Win.UltraWinTree.UltraTreeNode,
                                         poJsonObject AS JsonObject):

        DEFINE VARIABLE cNameArray     AS CHARACTER EXTENT NO-UNDO.
        DEFINE VARIABLE cName          AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iDataType      AS INT64     NO-UNDO.
        DEFINE VARIABLE iNameIteration AS INTEGER   NO-UNDO.

        DEFINE VARIABLE oNode AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.

        ASSIGN
            cNameArray = poJsonObject:GetNames ().

        nameIterationLoop:
        DO iNameIteration = 1 TO EXTENT (cNameArray):

            ASSIGN
                cName     = cNameArray[iNameIteration]
                iDataType = poJsonObject:GetType(cName)
                .

            IF lHideSerializeType AND cName = "SerializedType":U THEN
                NEXT nameIterationLoop .

            IF VALID-OBJECT (poParentNode:Nodes:ParentNode) THEN
                oUltraTreeColumnResizeHelper:InitializeColumnResizing (poParentNode:Nodes:ParentNode:Level + 1).

            ASSIGN
                oNode     = poParentNode:Nodes:Add(GUID)
                oNode:Cells ["PropertyLabel":U]:Value = cName
                .

            CASE iDataType:

                WHEN JsonDataType:ARRAY THEN DO:

                    ASSIGN oNode:Cells ["PropertyValue":U]:Value = "[ ]":U.

                    THIS-OBJECT:WalkthroughJSON (oNode,
                                                 poJsonObject:GetJsonArray (cName)).

                END.

                WHEN JsonDataType:OBJECT THEN DO:

                    ASSIGN oNode:Cells ["PropertyValue":U]:Value =  "~{ ~}":U.

                    THIS-OBJECT:WalkthroughJSON (oNode,
                                                 poJsonObject:GetJsonObject(cName)).

                END.

                WHEN JsonDataType:BOOLEAN THEN
                    ASSIGN oNode:Cells ["PropertyValue":U]:Value =  poJsonObject:GetLogical (cName).

                WHEN JsonDataType:NULL THEN
                    ASSIGN oNode:Cells ["PropertyValue":U]:Value = "Null":U.

                WHEN JsonDataType:NUMBER THEN
                    ASSIGN oNode:Cells ["PropertyValue":U]:Value =  poJsonObject:GetDecimal (cName).

                WHEN JsonDataType:STRING THEN
                    ASSIGN oNode:Cells ["PropertyValue":U]:Value =  poJsonObject:GetCharacter (cName).

            END CASE.
        END.

    END METHOD.

END CLASS.
