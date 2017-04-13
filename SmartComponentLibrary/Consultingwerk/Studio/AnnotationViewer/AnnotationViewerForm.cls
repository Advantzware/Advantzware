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
    File        : AnnotationViewerForm
    Purpose     : Form to select a Annotations file and visualization in a TreeView
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer
    Created     : Wed Nov 30 09:49:42 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Util.*               FROM PROPATH.
USING Progress.Lang.*                     FROM PROPATH.
USING Progress.Windows.*                  FROM PROPATH.
USING Progress.Json.ObjectModel.*         FROM PROPATH.
USING Consultingwerk.Forms.BaseForm       FROM PROPATH.
USING Infragistics.Win.UltraWinToolbars.* FROM ASSEMBLY.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Studio.AnnotationViewer.AnnotationViewerForm
    INHERITS BaseForm:

    DEFINE PRIVATE VARIABLE jsonObjectViewerControl1 AS Consultingwerk.Windows.Util.Controls.JSONObjectViewerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE m_AnnotationViewerForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_AnnotationViewerForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_AnnotationViewerForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_AnnotationViewerForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE AnnotationViewerForm_Fill_Panel AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1 AS System.Windows.Forms.OpenFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE smartToolbarController1 AS Consultingwerk.SmartComponents.Implementation.SmartToolbarController NO-UNDO.

    DEFINE VARIABLE cBaseTitle AS CHARACTER NO-UNDO INITIAL "Annotation File Viewer":U.
    DEFINE VARIABLE cFileName  AS CHARACTER NO-UNDO INITIAL ?.

    /**
     * Purpose: Constructor for the AnnotationViewerForm class
     * Notes:
     */
    CONSTRUCTOR PUBLIC AnnotationViewerForm (  ):

        SUPER().

        InitializeComponent().

        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the AnnotationViewerForm class
     * Notes:
     * @param pcFileName Filename of the file which is parsed into a JsonConstruct and visiualized
     */
    CONSTRUCTOR PUBLIC AnnotationViewerForm ( pcFileName AS CHARACTER ):

        THIS-OBJECT().

        IF pcFileName NE ? THEN
            THIS-OBJECT:ParseAndViewJSonFile(pcFileName,
                                             CAST (THIS-OBJECT:smartToolbarController1:Toolbars["JsonParserToolbar":U]:Tools["HideSerializeType":U],
                                                   StateButtonTool):Checked).

    END CONSTRUCTOR.

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
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("JsonParserToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("FileOpen":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool10 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool10 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("HideSerializeType":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE stateButtonTool11 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
        stateButtonTool11 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("HideSerializeType":U, "":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("FileOpen":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Studio.AnnotationViewer.AnnotationViewerForm":U).
        THIS-OBJECT:AnnotationViewerForm_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:jsonObjectViewerControl1 = NEW Consultingwerk.Windows.Util.Controls.JSONObjectViewerControl().
        THIS-OBJECT:smartToolbarController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartToolbarController(THIS-OBJECT:components).
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        THIS-OBJECT:AnnotationViewerForm_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:AnnotationViewerForm_Fill_Panel:SuspendLayout().
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* AnnotationViewerForm_Fill_Panel */
        /*  */
        /*  */
        /* AnnotationViewerForm_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:AnnotationViewerForm_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:jsonObjectViewerControl1).
        THIS-OBJECT:AnnotationViewerForm_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:AnnotationViewerForm_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:AnnotationViewerForm_Fill_Panel:Location = NEW System.Drawing.Point(0, 40).
        THIS-OBJECT:AnnotationViewerForm_Fill_Panel:Name = "AnnotationViewerForm_Fill_Panel":U.
        THIS-OBJECT:AnnotationViewerForm_Fill_Panel:Size = NEW System.Drawing.Size(674, 405).
        THIS-OBJECT:AnnotationViewerForm_Fill_Panel:TabIndex = 0.
        /*  */
        /* jsonObjectViewerControl1 */
        /*  */
        THIS-OBJECT:jsonObjectViewerControl1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:jsonObjectViewerControl1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:jsonObjectViewerControl1:Name = "jsonObjectViewerControl1":U.
        THIS-OBJECT:jsonObjectViewerControl1:Size = NEW System.Drawing.Size(674, 405).
        THIS-OBJECT:jsonObjectViewerControl1:TabIndex = 0.
        /*  */
        /* smartToolbarController1 */
        /*  */
        THIS-OBJECT:smartToolbarController1:DesignerFlags = 1.
        THIS-OBJECT:smartToolbarController1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:smartToolbarController1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.Forms.BaseForm":U).
        THIS-OBJECT:smartToolbarController1:ShowFullMenusDelay = 500.
        THIS-OBJECT:smartToolbarController1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        stateButtonTool10:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar0[1] = buttonTool11.
        arrayvar0[2] = stateButtonTool10.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar0).
        ultraToolbar1:Text = "JsonParserToolbar":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 1 NO-UNDO.
        arrayvar1[1] = ultraToolbar1.
        THIS-OBJECT:smartToolbarController1:Toolbars:AddRange(arrayvar1).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        stateButtonTool11:SharedPropsInternal:AppearancesLarge:Appearance = appearance1.
        stateButtonTool11:SharedPropsInternal:Caption = "Hide Serialize Type information":U.
        stateButtonTool11:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool12:SharedPropsInternal:AppearancesLarge:Appearance = appearance2.
        buttonTool12:SharedPropsInternal:Caption = "Open Json File":U.
        buttonTool12:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
        arrayvar2[1] = stateButtonTool11.
        arrayvar2[2] = buttonTool12.
        THIS-OBJECT:smartToolbarController1:Tools:AddRange(arrayvar2).
        THIS-OBJECT:smartToolbarController1:UseLargeImagesOnToolbar = TRUE.
        THIS-OBJECT:smartToolbarController1:ToolClick:Subscribe(THIS-OBJECT:ToolClickHandler).
        /*  */
        /* m_AnnotationViewerForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 40).
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Left:Name = "m_AnnotationViewerForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(0, 405).
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_AnnotationViewerForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(674, 40).
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Right:Name = "m_AnnotationViewerForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(0, 405).
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_AnnotationViewerForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Top:Name = "m_AnnotationViewerForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(674, 40).
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* m_AnnotationViewerForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 445).
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Bottom:Name = "m_AnnotationViewerForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(674, 0).
        THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:smartToolbarController1.
        /*  */
        /* openFileDialog1 */
        /*  */
        THIS-OBJECT:openFileDialog1:DefaultExt = "*.annotations":U.
        THIS-OBJECT:openFileDialog1:Filter = "Annotation-Files (*.annotations)|*.annotations|All Files (*.*)|*.*":U.
        THIS-OBJECT:openFileDialog1:InitialDirectory = ".":U.
        THIS-OBJECT:openFileDialog1:RestoreDirectory = TRUE.
        THIS-OBJECT:openFileDialog1:Title = "Select Annotation File":U.
        /*  */
        /* AnnotationViewerForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(674, 445).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:AnnotationViewerForm_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_AnnotationViewerForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "AnnotationViewerForm":U.
        THIS-OBJECT:Text = "Annotation File Viewer":U.
        THIS-OBJECT:WindowPositionRegistryKey = "AnnotationViewerForm":U.
        THIS-OBJECT:AnnotationViewerForm_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:AnnotationViewerForm_Fill_Panel:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartToolbarController1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /**
     * Purpose:
     * Notes:
     * @param pcFileName          Filename of the file which is parsed into a JsonConstruct
     * @param plHideSerializeType Flag to Show/Hide the Consultingwerk SerialzeType Attribute in the TreeViews
     */
    METHOD PUBLIC VOID ParseAndViewJSonFile (pcFileName          AS CHARACTER,
                                             plHideSerializeType AS LOGICAL):

        DEFINE VARIABLE oObjectModelParser  AS ObjectModelParser  NO-UNDO.
        DEFINE VARIABLE oJsonConstruct      AS JsonConstruct      NO-UNDO.

        oObjectModelParser = NEW ObjectModelParser().

        DO ON ERROR UNDO:

            oJsonConstruct = oObjectModelParser:ParseFile (pcFileName).

            THIS-OBJECT:jsonObjectViewerControl1:DisplayJsonObject (oJsonConstruct,
                                                                    plHideSerializeType).

            ASSIGN
                THIS-OBJECT:Text = THIS-OBJECT:WindowTitle (pcFileName)
                cFileName = pcFileName
                .

            CATCH ple AS Progress.Lang.Error :
                ErrorHelper:ShowErrorMessage (ple).
            END CATCH.

        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oObjectModelParser).
            GarbageCollectorHelper:DeleteObject (oJsonConstruct).
        END FINALLY.

    END METHOD.


    /**
     * Purpose: Shows the openFileDialog to let the user select a *.annotations file
     * Notes:
     * @return returns the selected file. Cancel the dialog returns a ?.
     */
    METHOD PRIVATE CHARACTER SelectAnnotationFile():

        DEFINE VARIABLE oResult AS System.Windows.Forms.DialogResult NO-UNDO .

        WAIT-FOR openFileDialog1:ShowDialog () SET oResult .

        IF NOT Progress.Util.EnumHelper:AreEqual (System.Windows.Forms.DialogResult:OK,
                                                  oResult) THEN
            RETURN ?.

        RETURN openFileDialog1:FileName.

    END METHOD.


    /**
     * Purpose: Event handler for the ToolClick event of the ToolBar
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The ToolClickEventArgs with the data for this event
     */
    METHOD PRIVATE VOID ToolClickHandler( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs ):

        CASE e:Tool:Key:

            WHEN "FileOpen":U THEN DO:
                cFileName = THIS-OBJECT:SelectAnnotationFile ().

                IF cFileName NE ? THEN
                    THIS-OBJECT:ParseAndViewJSonFile(cFileName,
                                                     CAST (THIS-OBJECT:smartToolbarController1:Toolbars["JsonParserToolbar":U]:Tools["HideSerializeType":U],
                                                           StateButtonTool):Checked).
            END.
            WHEN "HideSerializeType":U THEN DO:

                IF cFileName NE ? THEN
                    THIS-OBJECT:ParseAndViewJSonFile (cFileName,
                                                      CAST (e:Tool, StateButtonTool):Checked).

            END.

        END CASE.

    END METHOD.

    /**
     * Purpose: Returns the Title that should be shown in the title bar Business Entity Designer Form
     * Notes:
     * @param pcFileName The currently open file name or ? for "untitled"
     * @return The Title that should be shown in the title bar
     */
    METHOD PROTECTED CHARACTER WindowTitle (pcFileName AS CHARACTER):

        DEFINE VARIABLE cShortFileName AS CHARACTER NO-UNDO.

        IF pcFileName <> ? THEN DO:
            ASSIGN cShortFileName = FileHelper:ShortFileNameWithoutExtension (pcFileName).

            RETURN SUBSTITUTE ("&1 - &2 (&3)":U,
                               cShortFileName,
                               cBaseTitle,
                               pcFileName) .
        END.
        ELSE
            RETURN SUBSTITUTE ("<untitled> - &1":U, cBaseTitle) .

    END METHOD.

    DESTRUCTOR PUBLIC AnnotationViewerForm ( ):

    END DESTRUCTOR.

END CLASS.