/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : BusinessEntityGeneratorLogControl
    Purpose     : Displays log information for the Business Entity Generator
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 02 22:28:27 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.UI.* FROM PROPATH .
USING Consultingwerk.Framework.*                 FROM PROPATH .
USING Consultingwerk.Util.*                      FROM PROPATH .
USING Infragistics.Win.*                         FROM ASSEMBLY .
USING Infragistics.Win.UltraWinDock.*            FROM ASSEMBLY .
USING Progress.Lang.*                            FROM PROPATH .
USING Progress.Windows.UserControl               FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityGeneratorLogControl
    INHERITS UserControl:

    DEFINE PRIVATE VARIABLE m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE BusinessEntityGeneratorLogControl_Fill_Panel AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
    DEFINE PRIVATE VARIABLE textBox1 AS System.Windows.Forms.TextBox NO-UNDO.

    DEFINE VARIABLE oStatusManager AS IStatusManager      NO-UNDO .
    DEFINE VARIABLE oPane          AS DockableControlPane NO-UNDO .

    DEFINE VARIABLE lSeparatorPending  AS LOGICAL NO-UNDO INIT FALSE .

    CONSTRUCTOR PUBLIC BusinessEntityGeneratorLogControl ():

        SUPER().
        InitializeComponent().
        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).


        oStatusManager = {Consultingwerk/get-service.i Consultingwerk.Framework.IStatusManager
                                                       "NEW Consultingwerk.Framework.StatusManager()"} .

        oStatusManager:GenericStatusChanded:Subscribe (GenericStatusChandedHandler) .

        FrameworkSettings:ServiceContainer:AddService (THIS-OBJECT:GetClass(),
                                                       THIS-OBJECT) .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.


    /**
     * Purpose: Clears the Text in the Log view
     * Notes:
     */
    METHOD PUBLIC VOID ClearText():

        textBox1:Text = "":U .

        UserInterfaceHelper:ProcessEvents() .

    END METHOD.

    /**
     * Purpose: Displays the text in the log control
     * Notes:
     * @param pcText The text to display
     */
    METHOD PUBLIC VOID DisplayText (pcText AS CHARACTER):

        textBox1:AppendText (SUBSTITUTE ("[&1] &2~n":U,
                                         STRING (NOW, "99.99.9999 hh:mm:ss.sss":U),
                                         pcText)) .

        ASSIGN lSeparatorPending = TRUE .

        UserInterfaceHelper:ProcessEvents() .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Docks the Control in a Tab Group relatively to another .NET Control
        Notes:
        @param poDockManager The reference to the UltraDockManager
        @param poControl The reference to the control that we should dock the Release Notes Control next to it
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DockControl (poDockManager AS UltraDockManager,
                                    poControl AS System.Windows.Forms.Control):

        DEFINE VARIABLE oControls AS "System.Windows.Forms.Control[]" NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 26.03.2015
           Fix settings for new Dockable pane */
        oPane = poDockManager:PaneFromControl (THIS-OBJECT) .

        IF NOT VALID-OBJECT (oPane) THEN DO:
            oControls = {Consultingwerk/new-array.i System.Windows.Forms.Control 1} .
            oControls:SetValue (THIS-OBJECT, 0) .

            poDockManager:DockControls (oControls,
                                        DockedLocation:DockedBottom,
                                        ChildPaneStyle:TabGroup) .

            oPane = poDockManager:PaneFromControl (THIS-OBJECT) .

            IF VALID-OBJECT (oPane) THEN DO:
                oPane:Text = "Generator Log"{&TRAN} .
                oPane:Settings:Appearance:Image = ImageHelper:Load
                        ("Consultingwerk/BusinessEntityDesigner/UI/Images/document_gear_16.png":U) .
            END.
        END.
        ELSE DO:
                oPane:Text = "Generator Log"{&TRAN} .
                oPane:Settings:Appearance:Image = ImageHelper:Load
                        ("Consultingwerk/BusinessEntityDesigner/UI/Images/document_gear_16.png":U) .
        END.

        oPane:Settings:AllowPin = DefaultableBoolean:FALSE.
        poDockManager:PaneFromControl (poControl):Parent:ChildPaneStyle = ChildPaneStyle:TabGroup .
        oPane:RepositionToGroup (poDockManager:PaneFromControl (poControl):Parent) .

    END METHOD .

    /**
     * Purpose: Event handler for the GenericStatusChanded event of the IStatusManager
     * Notes:
     * @param sender The reference to the IStatusManager that raised the event
     * @param pcStatus The new status text
     */
    METHOD PROTECTED VOID GenericStatusChandedHandler (sender AS IStatusManager,
                                                       pcStatus AS CHARACTER):

        IF pcStatus > "":U THEN DO:
            textBox1:AppendText (SUBSTITUTE ("[&1] &2~n":U,
                                             STRING (NOW, "99.99.9999 hh:mm:ss.sss":U),
                                             pcStatus)) .

            IF VALID-OBJECT (oPane) THEN
                oPane:Activate() .

            ASSIGN lSeparatorPending = TRUE .

            UserInterfaceHelper:ProcessEvents() .
        END.

    END METHOD.

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
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("UltraToolbar1":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Clear":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Clear":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityGeneratorLogControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:BusinessEntityGeneratorLogControl_Fill_Panel = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:textBox1 = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:BusinessEntityGeneratorLogControl_Fill_Panel:ClientArea:SuspendLayout().
        THIS-OBJECT:BusinessEntityGeneratorLogControl_Fill_Panel:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar0[1] = buttonTool1.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar0).
        ultraToolbar1:Text = "UltraToolbar1":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 1 NO-UNDO.
        arrayvar1[1] = ultraToolbar1.
        THIS-OBJECT:ultraToolbarsManager1:Toolbars:AddRange(arrayvar1).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        buttonTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance1.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
        buttonTool2:SharedPropsInternal:Caption = "Clear":U.
        buttonTool2:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar2[1] = buttonTool2.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar2).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        /*  */
        /* BusinessEntityGeneratorLogControl_Fill_Panel */
        /*  */
        /*  */
        /* BusinessEntityGeneratorLogControl_Fill_Panel.ClientArea */
        /*  */
        THIS-OBJECT:BusinessEntityGeneratorLogControl_Fill_Panel:ClientArea:Controls:Add(THIS-OBJECT:textBox1).
        THIS-OBJECT:BusinessEntityGeneratorLogControl_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
        THIS-OBJECT:BusinessEntityGeneratorLogControl_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:BusinessEntityGeneratorLogControl_Fill_Panel:Location = NEW System.Drawing.Point(0, 25).
        THIS-OBJECT:BusinessEntityGeneratorLogControl_Fill_Panel:Name = "BusinessEntityGeneratorLogControl_Fill_Panel":U.
        THIS-OBJECT:BusinessEntityGeneratorLogControl_Fill_Panel:Size = NEW System.Drawing.Size(300, 275).
        THIS-OBJECT:BusinessEntityGeneratorLogControl_Fill_Panel:TabIndex = 0.
        /*  */
        /* textBox1 */
        /*  */
        THIS-OBJECT:textBox1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:textBox1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:textBox1:Font = NEW System.Drawing.Font("Courier New":U, Progress.Util.CastUtil:ToSingle(9), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:textBox1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:textBox1:Multiline = TRUE.
        THIS-OBJECT:textBox1:Name = "textBox1":U.
        THIS-OBJECT:textBox1:ReadOnly = TRUE.
        THIS-OBJECT:textBox1:ScrollBars = System.Windows.Forms.ScrollBars:Both.
        THIS-OBJECT:textBox1:Size = NEW System.Drawing.Size(300, 275).
        THIS-OBJECT:textBox1:TabIndex = 0.
        THIS-OBJECT:textBox1:WordWrap = FALSE.
        /*  */
        /* m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Left:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 25).
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Left:Name = "m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(0, 275).
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Right:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(300, 25).
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Right:Name = "m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(0, 275).
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Top:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Top:Name = "m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(300, 25).
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 300).
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Bottom:Name = "m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(300, 0).
        THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* BusinessEntityGeneratorLogControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:BusinessEntityGeneratorLogControl_Fill_Panel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_BusinessEntityGeneratorLogControl_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Name = "BusinessEntityGeneratorLogControl":U.
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:BusinessEntityGeneratorLogControl_Fill_Panel:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:BusinessEntityGeneratorLogControl_Fill_Panel:ClientArea:PerformLayout().
        THIS-OBJECT:BusinessEntityGeneratorLogControl_Fill_Panel:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /**
     * Purpose: Clears the Text in the Log view
     * Notes:
     */
    METHOD PUBLIC VOID InsertSeperator ():

        IF lSeparatorPending THEN
            textBox1:AppendText ("~n":U).

        ASSIGN lSeparatorPending = FALSE .

    END METHOD.

    /**
     * Purpose: Event handler for the click evnet of the ultraToolbarsManager1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The ToolClickEventArgs with the data for this event
     */
    METHOD PRIVATE VOID ultraToolbarsManager1_ToolClick (sender AS System.Object,
                                                         e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):

        textBox1:Text = "":U .

        ASSIGN lSeparatorPending = FALSE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /**
     * Purpose: Destructor for the BusinessEntityGeneratorLogControl class
     * Notes:
     */
    DESTRUCTOR PUBLIC BusinessEntityGeneratorLogControl ():

    END DESTRUCTOR .

END CLASS.
