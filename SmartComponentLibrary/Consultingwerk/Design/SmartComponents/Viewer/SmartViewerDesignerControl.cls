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
    File        : SmartViewerDesignerControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Nov 09 22:53:47 CET 2010
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Design.*                        /* FROM ASSEMBLY */ .
USING Consultingwerk.Design.SmartComponents.*        FROM PROPATH .
USING Consultingwerk.Design.SmartComponents.Viewer.* FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*          FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .
USING System.*                                       FROM ASSEMBLY .
USING System.Collections.*                           FROM ASSEMBLY .
USING System.Collections.Generic.*                   FROM ASSEMBLY .
USING System.ComponentModel.*                        FROM ASSEMBLY .
USING System.ComponentModel.Design.*                 FROM ASSEMBLY .
USING System.Data.*                                  FROM ASSEMBLY .
USING System.Drawing.*                               FROM ASSEMBLY .
USING System.Drawing.Design.*                        FROM ASSEMBLY .
USING System.Text.*                                  FROM ASSEMBLY .
USING System.Windows.Forms.*                         FROM ASSEMBLY .

CLASS Consultingwerk.Design.SmartComponents.Viewer.SmartViewerDesignerControl
    INHERITS SmartUserControl
    IMPLEMENTS Consultingwerk.Design.SmartComponents.IDesignerControl,
               System.ComponentModel.Design.Serialization.INameCreationService:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE VARIABLE oCurrentLicenseContext AS System.ComponentModel.LicenseContext NO-UNDO .

    DEFINE VARIABLE oDesignSurface AS System.ComponentModel.Design.DesignSurface NO-UNDO .
    DEFINE VARIABLE oDesignView AS System.Windows.Forms.Control NO-UNDO .
    DEFINE VARIABLE oDesignerHost AS System.ComponentModel.Design.IDesignerHost NO-UNDO .
    DEFINE VARIABLE oSelectionService AS System.ComponentModel.Design.ISelectionService NO-UNDO .
    DEFINE VARIABLE oMenuCommandService AS System.ComponentModel.Design.MenuCommandService NO-UNDO .

    DEFINE VARIABLE oTemplateControl AS Progress.Windows.UserControl NO-UNDO .

    DEFINE VARIABLE oPropertyGrid AS System.Windows.Forms.PropertyGrid NO-UNDO .
    DEFINE VARIABLE oPropertyGridForm AS Consultingwerk.Design.SmartComponents.PropertyGridForm NO-UNDO .
    DEFINE VARIABLE oBindingSourceFieldForm AS Consultingwerk.Design.SmartComponents.BindingSourceFieldsForm NO-UNDO .
    DEFINE VARIABLE oBindingSource AS Progress.Data.BindingSource NO-UNDO .

    DEFINE VARIABLE cAddingFieldName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAddingFieldLabel    AS CHARACTER NO-UNDO.

    DEFINE VARIABLE oNewLabel   AS System.Windows.Forms.Label   NO-UNDO .
    DEFINE VARIABLE oNewControl AS System.Windows.Forms.Control NO-UNDO  .
    DEFINE PRIVATE VARIABLE timer1 AS System.Windows.Forms.Timer NO-UNDO.

    {Consultingwerk/Framework/RepositoryServices/ttViewerDesign.i}

    /*------------------------------------------------------------------------------
        Purpose: Raised whenever a field is added to the Designer
        Notes:
        @param sender The reference to the object that raised the event
        @param e The FieldCreatedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT FieldCreated SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                     e AS FieldCreatedEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: The reference to the template BindingSource (outside this form)
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BindingSource AS Progress.Data.BindingSource NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Logical value indicating if the ToolboxForm should be created
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CreateToolboxForm AS LOGICAL INITIAL TRUE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Logical value indicating if the PropertySheet should be created
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CreatePropertySheet AS LOGICAL INITIAL TRUE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Designer Host
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DesignerHost AS System.ComponentModel.Design.IDesignerHost NO-UNDO
    GET():
        RETURN oDesignerHost .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the UltraToolbarsManager with the
                 designer tools
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DesignerToolbar AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Get and sets the refrence to the service container
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ServiceContainer AS System.ComponentModel.Design.IServiceContainer NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the source control
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SourceControl AS System.Windows.Forms.Control NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the dictionary of substituted controls on the designer
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SubstituteDesignerControls AS "System.Collections.Generic.Dictionary<Component,System.String>" NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the dictionary of substituted dynamic contorls
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SubstituteDynamicControls AS "System.Collections.Generic.Dictionary<Component,System.String>" NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartViewerDesignerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartViewerDesignerControl ():

        SUPER().
        InitializeComponent().

        SubstituteDesignerControls = NEW "System.Collections.Generic.Dictionary<Component,System.String>" () .
        SubstituteDynamicControls = NEW "System.Collections.Generic.Dictionary<Component,System.String>" () .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ComponentAdded event
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ComponentEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ComponentAddedHandler (sender AS System.Object,
                                              e AS System.ComponentModel.Design.ComponentEventArgs):

        DEFINE VARIABLE oLabel           AS System.Windows.Forms.Label   NO-UNDO .
        DEFINE VARIABLE oControl         AS System.Windows.Forms.Control NO-UNDO .
        DEFINE VARIABLE cLabelName       AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE cBindingProperty AS CHARACTER                    NO-UNDO.



        IF cAddingFieldName > "":U AND NOT TYPE-OF (e:Component, System.Windows.Forms.Label) THEN .
        ELSE
        IF NOT TYPE-OF (e:Component, System.Windows.Forms.Label) THEN DO:

            IF TYPE-OF (e:Component, System.Windows.Forms.Control) THEN DO:

                oControl = CAST (e:Component, System.Windows.Forms.Control) .

                IF oControl:Name = "":U THEN

                     ASSIGN oControl:Name = UniqueControlName (oControl:GetType():Name,
                                                               oTemplateControl) .
            END.

            RETURN .
        END.
        ELSE RETURN .

        oControl = CAST (e:Component, System.Windows.Forms.Control) .

        ASSIGN cBindingProperty = Consultingwerk.Design.DesignerFieldsHelper:DefaultBindingPropertyForType (oControl:GetType()) .

        oControl:DataBindings:Add (cBindingProperty, oBindingSource, cAddingFieldName, TRUE) .

        CAST (e:Component, System.Windows.Forms.Control):BackColor = System.Drawing.Color:Beige .

        ASSIGN cLabelName = UniqueControlName (cAddingFieldName + "_label":U,
                                               oTemplateControl) .

        oLabel = CAST (oDesignerHost:CreateComponent (Progress.Util.TypeHelper:GetType ("System.Windows.Forms.Label":U),
                                                      cLabelName),
                       System.Windows.Forms.Label) .
        oLabel:Text = cAddingFieldLabel + ":":U .
        oLabel:Parent = oTemplateControl .
        oLabel:AutoSize = TRUE .

        ASSIGN cAddingFieldLabel    = "":U
               cAddingFieldName     = "":U

               oNewLabel            = oLabel
               oNewControl          = oControl

               timer1:Interval      = 5
               timer1:Enabled       = TRUE .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates the Design time BindingSource instance
        Notes:
        @return The reference to the BindingSource instance
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED Progress.Data.BindingSource CreateDesignBindingSource ():

        DEFINE VARIABLE oColumns AS "Progress.Data.ColumnPropDesc[]":U NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER                            NO-UNDO .
        DEFINE VARIABLE oColumn  AS Progress.Data.ColumnPropDesc       NO-UNDO .

        DEFINE VARIABLE oReturn AS Progress.Data.BindingSource NO-UNDO .

        oReturn = CAST (oDesignerHost:CreateComponent(Progress.Util.TypeHelper:GetType("Progress.Data.BindingSource":U),
                                                      "bindingSource1":U),
                        Progress.Data.BindingSource) .

        ASSIGN oColumns = {Consultingwerk/new-array.i Progress.Data.ColumnPropDesc THIS-OBJECT:BindingSource:TableSchema:Columns:Length} .

        DO i = 0 TO oColumns:Length - 1:

            oColumn = CAST (THIS-OBJECT:BindingSource:TableSchema:Columns:GetValue(i), Progress.Data.ColumnPropDesc)  .

            oColumns:SetValue(NEW Progress.Data.ColumnPropDesc (oColumn:ColumnName,
                                                                oColumn:ColumnLabel,
                                                                oColumn:ColumnType), i) .
        END.

        oReturn:TableSchema = NEW Progress.Data.TableDesc ("Table":U, oColumns,
                                                           {Consultingwerk/new-array.i Progress.Data.TableDesc 0}).

        RETURN oReturn .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a new name that is unique to all components in the specified
                 container
        Notes:
        @param container The container where the new object is added
        @param dataType The data type of the object that receives the name
        @return The name for the new control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER CreateName (container AS System.ComponentModel.IContainer,
                                        dataType AS System.Type):

        RETURN UniqueControlName (cAddingFieldName, oTemplateControl) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deletes the selected controls
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DeleteControls ():

        DEFINE VARIABLE oCollection AS System.Collections.ICollection NO-UNDO .
        DEFINE VARIABLE oArray      AS System.Array                   NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER                        NO-UNDO .

        oCollection = oSelectionService:GetSelectedComponents() .

        oArray = {Consultingwerk/new-array.i System.Object oCollection:Count} .

        oCollection:CopyTo (oArray, 0) .

        DO i = 0 TO oArray:Length - 1:
            CAST(oArray:GetValue(i), System.Windows.Forms.Control):Parent = ? .
            CAST(oArray:GetValue(i), System.Windows.Forms.Control):Dispose () .

            DELETE OBJECT oArray:GetValue (i) .
/*            oDesignerHost:DestroyComponent (CAST(oArray:GetValue(i), System.ComponentModel.IComponent)) .*/
        END.

        FINALLY:
            oCollection = NEW "System.Collections.ObjectModel.Collection<System.ComponentModel.Component>" () .

            oSelectionService:SetSelectedComponents (oCollection) .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Disables a tool on the Designer Toolbar
        Notes:
        @param pcTool The name of the tool to disable
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DisableTool (pcTool AS CHARACTER):

        IF VALID-OBJECT (THIS-OBJECT:DesignerToolbar) AND THIS-OBJECT:DesignerToolbar:Tools:Exists (pcTool) THEN
            THIS-OBJECT:DesignerToolbar:Tools[pcTool]:SharedProps:Enabled = FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Duplicates the controls of the source control on the target control
        Notes:
        @param poSourceControls The reference to the source control container
        @param poTargetControls The reference to the target control container
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DuplicateControls (poSourceControls AS System.Windows.Forms.Control,
                                             poTargetControls AS System.Windows.Forms.Control):

        DEFINE VARIABLE oTargetControl     AS System.Windows.Forms.Control   NO-UNDO .
        DEFINE VARIABLE cType              AS CHARACTER                      NO-UNDO .

        IF NOT VALID-OBJECT (poSourceControls) THEN
            RETURN .

        {Consultingwerk/foreach.i System.Windows.Forms.Control oSourceControl in  poSourceControls:Controls}

            IF oSourceControl = THIS-OBJECT THEN
                NEXT .

            ASSIGN cType = SubstituteType (oSourceControl:GetType():FullName) .

            oTargetControl = CAST (oDesignerHost:CreateComponent (Progress.Util.TypeHelper:GetType(cType), oSourceControl:Name),
                                       System.Windows.Forms.Control)  .

            IF THIS-OBJECT:SubstituteDynamicControls:ContainsKey (oSourceControl) THEN DO:
                THIS-OBJECT:SubstituteDesignerControls:Add (oTargetControl,
                                                            THIS-OBJECT:SubstituteDynamicControls [oSourceControl]) .
            END .

            ASSIGN
                oTargetControl:TEXT     = oSourceControl:TEXT
                oTargetControl:Dock     = oSourceControl:Dock
                oTargetControl:Anchor   = oSourceControl:Anchor
                oTargetControl:SIZE     = oSourceControl:SIZE
                oTargetControl:Location = oSourceControl:Location
                oTargetControl:Parent   = poTargetControls
                .

            IF TYPE-OF (oSourceControl, System.Windows.Forms.TextBox) THEN
               CAST (oTargetControl, System.Windows.Forms.TextBox):Multiline = CAST (oSourceControl, System.Windows.Forms.TextBox):Multiline .
            IF TYPE-OF (oSourceControl, Infragistics.Win.UltraWinEditors.UltraTextEditor) THEN
               CAST (oTargetControl, System.Windows.Forms.TextBox):Multiline = CAST (oSourceControl, Infragistics.Win.UltraWinEditors.UltraTextEditor):Multiline .

            {Consultingwerk/foreach.i System.Windows.Forms.Binding oBinding in oSourceControl:DataBindings}

                oTargetControl:DataBindings:Add (oBinding:PropertyName,
                                                 oBindingSource,
                                                 oBinding:BindingMemberInfo:BindingField,
                                                 oBinding:FormattingEnabled) .
            END.

            IF oSourceControl:Controls:Count > 0 THEN
                DuplicateControls (oSourceControl,
                                   oTargetControl) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Enables a tool on the Designer Toolbar
        Notes:
        @param pcTool The name of the tool to enable
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID EnableTool (pcTool AS CHARACTER):

        IF VALID-OBJECT (THIS-OBJECT:DesignerToolbar) AND THIS-OBJECT:DesignerToolbar:Tools:Exists (pcTool) THEN
            THIS-OBJECT:DesignerToolbar:Tools[pcTool]:SharedProps:Enabled = TRUE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the FormClosing event of the parent Form
        Notes:
        @param sender The reference to the object that raised the event
        @param e The FormClosingEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FormClosingHandler (sender AS System.Object, e AS System.Windows.Forms.FormClosingEventArgs):

        DEFINE VARIABLE oDialogResult AS DialogResultEnum NO-UNDO .

        IF e:Cancel THEN
            RETURN .

        oDialogResult = Consultingwerk.Util.MessageFormHelper:AskQuestion
              (Consultingwerk.Design.SmartComponents.SmartComponentsDesignCustomizer:SmartViewerDesignerControl_FormClosingPrompt) .

        IF oDialogResult:Equals (DialogResultEnum:DialogResultYes) THEN .
        ELSE
            e:Cancel = TRUE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Control and the Designer environment
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Initialize ():

        THIS-OBJECT:FindForm ():FormClosing:Subscribe (FormClosingHandler) .

        InitializeDesignEnvironment () .

        oBindingSource = CreateDesignBindingSource () .

        IF VALID-OBJECT (THIS-OBJECT:SourceControl) THEN
            DuplicateControls (THIS-OBJECT:SourceControl, oTemplateControl) .

        IF VALID-OBJECT (THIS-OBJECT:DesignerToolbar) THEN DO:
            THIS-OBJECT:DesignerToolbar:ToolClick:Subscribe(ToolClickHandler) .

            DisableTool ("ViewerDesignerDeleteControl":U) .
            DisableTool ("ViewerDesignerCustomize":U) .
            EnableTool ("ViewerDesignerSave":U) .
            EnableTool ("ViewerDesignerCancel":U) .
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
        THIS-OBJECT:timer1 = NEW System.Windows.Forms.Timer(THIS-OBJECT:components).
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* timer1 */
        /*  */
        THIS-OBJECT:timer1:Tick:Subscribe(THIS-OBJECT:timer1_Tick).
        /*  */
        /* SmartViewerDesignerControl */
        /*  */
        THIS-OBJECT:BackColor = System.Drawing.Color:Gray.
        THIS-OBJECT:Name = "SmartViewerDesignerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(593, 338).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Designer environment
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeDesignEnvironment ():

        DEFINE VARIABLE oCollection       AS "System.Collections.ObjectModel.Collection<Component>" NO-UNDO .
        DEFINE VARIABLE oServiceContainer AS System.ComponentModel.Design.IServiceContainer         NO-UNDO .

        IF THIS-OBJECT:CreatePropertySheet THEN DO:
            oPropertyGridForm = NEW Consultingwerk.Design.SmartComponents.PropertyGridForm () .
            oPropertyGridForm:Show () .
            oPropertyGrid = oPropertyGridForm:PropertyGrid .
        END.

        IF THIS-OBJECT:CreateToolboxForm THEN DO:
            oBindingSourceFieldForm = NEW Consultingwerk.Design.SmartComponents.BindingSourceFieldsForm () .
            oBindingSourceFieldForm:BindingSource = THIS-OBJECT:BindingSource .
            oBindingSourceFieldForm:DesignerControl = THIS-OBJECT .
            oBindingSourceFieldForm:Show() .
        END.

        oCurrentLicenseContext = System.ComponentModel.LicenseManager:CurrentContext .

        System.ComponentModel.LicenseManager:CurrentContext = NEW Consultingwerk.Design.LicenseContext () .

        oDesignSurface = NEW System.ComponentModel.Design.DesignSurface (Progress.Util.TypeHelper:GetType("Progress.Windows.UserControl":U)).

        /* Get the service container */
        oServiceContainer = CAST (oDesignSurface:GetService(Progress.Util.TypeHelper:GetType ("System.ComponentModel.Design.IServiceContainer":U)),
                                  System.ComponentModel.Design.IServiceContainer).

        THIS-OBJECT:ServiceContainer = oServiceContainer .

        oDesignerHost = CAST (oDesignSurface:GetService(Progress.Util.TypeHelper:GetType("System.ComponentModel.Design.IDesignerHost":U)),
                             System.ComponentModel.Design.IDesignerHost) .

        IF VALID-OBJECT (oPropertyGrid) THEN
            oServiceContainer:AddService(Progress.Util.TypeHelper:GetType ("System.Windows.Forms.PropertyGrid":U),
                                         oPropertyGrid) .

        oSelectionService = CAST (oServiceContainer:GetService (Progress.Util.TypeHelper:GetType("System.ComponentModel.Design.ISelectionService":U)),
                                  System.ComponentModel.Design.ISelectionService) .
        oSelectionService:SelectionChanged:Subscribe (SelectionChangedHandler) .

        oServiceContainer:AddService(Progress.Util.TypeHelper:GetType("System.ComponentModel.Design.IDesignerEventService":U),
                                     NEW Consultingwerk.Design.DesignerEventService(oDesignerHost)) .

        IF VALID-OBJECT (oBindingSourceFieldForm) THEN DO:
            oServiceContainer:AddService(Progress.Util.TypeHelper:GetType("System.Drawing.Design.IToolboxService":U),
                                         oBindingSourceFieldForm) .
            oBindingSourceFieldForm:DesignerHost = oDesignerHost .
        END.

        oMenuCommandService = NEW System.ComponentModel.Design.MenuCommandService (oDesignSurface) .
        oServiceContainer:AddService (Progress.Util.TypeHelper:GetType("System.ComponentModel.Design.IMenuCommandService":U),
                                     oMenuCommandService) .

        CAST (oServiceContainer:GetService (Progress.Util.TypeHelper:GetType ("System.ComponentModel.Design.IComponentChangeService":U)),
              System.ComponentModel.Design.IComponentChangeService):ComponentAdded:Subscribe (ComponentAddedHandler) .

        oServiceContainer:AddService (Progress.Util.TypeHelper:GetType ("System.ComponentModel.Design.Serialization.INameCreationService":U),
                                      THIS-OBJECT) .

        oTemplateControl = CAST (oDesignerHost:RootComponent, Progress.Windows.UserControl) .

        /* Set the default properties of our base template form */
        oTemplateControl:Dock = System.Windows.Forms.DockStyle:Fill .

        oTemplateControl:BackColor = System.Drawing.Color:White.
        oTemplateControl:AutoScroll = TRUE .

        oDesignView = CAST (oDesignSurface:View, Control).
        oDesignView:Dock = DockStyle:Fill.

        THIS-OBJECT:Controls:Add (oDesignView).

        oTemplateControl:Size = NEW System.Drawing.Size (oDesignView:Size:Width - 50, oDesignView:Height - 50) .

        oCollection = NEW "System.Collections.ObjectModel.Collection<Component>" ().

        oSelectionService:SetSelectedComponents (oCollection) .

        oCollection:Add (oTemplateControl) .

        oSelectionService:SetSelectedComponents (oCollection) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Gets a value indicating whether the specified name is valid
        Notes:   Always returns True
        @param name The name to validate
        @return Logical value indicating of the name is valid
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsValidName (name AS CHARACTER):

        RETURN TRUE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the FieldCreated event
        Notes:
        @param e The FieldCreatedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnFieldCreated (e AS FieldCreatedEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "FieldCreated":U) .

        THIS-OBJECT:FieldCreated:PUBLISH (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Prepares adding a Control
        Notes:
        @param pcFieldName The name of the field to add
        @param pcFieldDataType The DataType of the field to add
        @param pcFieldLabel The Label of the field to add
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PrepareAddingControl (pcFieldName AS CHARACTER,
                                             pcFieldDataType AS CHARACTER,
                                             pcFieldLabel AS CHARACTER):

        ASSIGN
            cAddingFieldName     = pcFieldName
            cAddingFieldLabel    = pcFieldLabel .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SelectionChanged event
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SelectionChangedHandler (sender AS System.Object,
                                                   e AS System.EventArgs):

        DEFINE VARIABLE selectedComponents AS ICollection                            NO-UNDO .
        DEFINE VARIABLE oObjects           AS System.Object EXTENT                   NO-UNDO .
        DEFINE VARIABLE i                  AS INTEGER                                NO-UNDO .
        DEFINE VARIABLE lContainer         AS LOGICAL INITIAL FALSE                  NO-UNDO .

        IF VALID-OBJECT (oSelectionService) THEN DO:
            selectedComponents = oSelectionService:GetSelectedComponents()  .

            IF selectedComponents:Count > 0 THEN DO:

                EXTENT (oObjects)= selectedComponents:Count .

                ASSIGN i = 0 .

                {Consultingwerk/foreach.i System.Object oObject in selectedComponents}
                    ASSIGN i = i + 1 .

                    oObjects[i] = oObject .

                    IF oObject = oTemplateControl THEN
                        ASSIGN lContainer = TRUE .
                END.

                IF VALID-OBJECT (oPropertyGrid) THEN
                    oPropertyGrid:SelectedObjects = oObjects.
            END.
            ELSE
                IF VALID-OBJECT (oPropertyGrid) THEN
                oPropertyGrid:SelectedObjects = ? .
        END.

        IF lContainer OR selectedComponents:Count = 0 THEN
            DisableTool ("ViewerDesignerDeleteControl":U) .
        ELSE
            EnableTool ("ViewerDesignerDeleteControl":U) .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Serializes the Design to a Temp-Table
        Notes:
        @param ttViewerDesign OUTPUT Temp-Table with the controls and their properties
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Serialize (OUTPUT TABLE ttViewerDesign):

        EMPTY TEMP-TABLE ttViewerDesign .

        Serialize (oTemplateControl, "":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Serializes the Controls of a container control
        Notes:   Recursive method
        @param poParentControl The reference to the Parent Control
        @param pcParentInstanceId The Instance Id of the current container instance
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID Serialize (poParentControl AS System.Windows.Forms.Control,
                                     pcParentInstanceId AS CHARACTER):

        DEFINE VARIABLE oBindingEnumerator AS System.Collections.IEnumerator NO-UNDO .

        DEFINE VARIABLE oBinding AS System.Windows.Forms.Binding NO-UNDO .
        DEFINE VARIABLE cControlName AS CHARACTER NO-UNDO.

        {Consultingwerk/foreach.i System.Windows.Forms.Control oControl in poParentControl:Controls}

            ASSIGN cControlName = UniqueControlNameForSerialization (oControl) .

            CREATE ttViewerDesign .
            ASSIGN ttViewerDesign.ControlName         = cControlName
                   ttViewerDesign.ControlInstanceId   = GUID
                   ttViewerDesign.ContainerInstanceId = pcParentInstanceId
                   ttViewerDesign.ControlType         = oControl:GetType():FullName
                   ttViewerDesign.ControlText         = oControl:Text
                   ttViewerDesign.ControlSize         = SUBSTITUTE ("&1,&2":U, oControl:Width, oControl:Height)
                   ttViewerDesign.ControlLocation     = SUBSTITUTE ("&1,&2":U, oControl:Left, oControl:Top)
                   ttViewerDesign.ControlDock         = oControl:Dock:ToString()
                   ttViewerDesign.ControlAnchor       = oControl:Anchor:ToString()
                 .

            IF THIS-OBJECT:SubstituteDesignerControls:ContainsKey (oControl) THEN
                ttViewerDesign.DynamicControlName = THIS-OBJECT:SubstituteDesignerControls[oControl] .

            IF TYPE-OF (oControl, System.Windows.Forms.TextBox) THEN
                ttViewerDesign.ControlMultiline = CAST (oControl, System.Windows.Forms.TextBox):Multiline .

            oBindingEnumerator = oControl:DataBindings:GetEnumerator() .
            oBindingEnumerator:Reset() .

            DO WHILE oBindingEnumerator:MoveNext ():
                oBinding = CAST (oBindingEnumerator:Current, System.Windows.Forms.Binding) .

                ASSIGN ttViewerDesign.BindingProperties = ttViewerDesign.BindingProperties + ",":U + oBinding:PropertyName
                       ttViewerDesign.BindingFields     = ttViewerDesign.BindingFields + ",":U + oBinding:BindingMemberInfo:BindingField .
            END.

            ASSIGN ttViewerDesign.BindingProperties = TRIM(ttViewerDesign.BindingProperties, ",":U)
                   ttViewerDesign.BindingFields     = TRIM(ttViewerDesign.BindingFields, ",":U)
                   .

            IF oControl:Controls:Count > 0 THEN
                THIS-OBJECT:Serialize (oControl,
                                       ttViewerDesign.ControlInstanceId) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Shuts down the design time environment
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ShutDown ():

        DEFINE VARIABLE oCollection AS System.Collections.ICollection NO-UNDO .
        DEFINE VARIABLE oForm       AS System.Windows.Forms.Form      NO-UNDO .

        oCollection = NEW "System.Collections.ObjectModel.Collection<System.ComponentModel.Component>" ().

        oSelectionService:SetSelectedComponents (oCollection) .

        IF VALID-OBJECT (oPropertyGridForm) THEN DO:
            oPropertyGridForm:Close () .

            IF VALID-OBJECT (oPropertyGridForm) THEN
                DELETE OBJECT oPropertyGridForm .
        END.

        IF VALID-OBJECT (oBindingSourceFieldForm) THEN DO:
            oBindingSourceFieldForm:Close () .

            IF VALID-OBJECT (oBindingSourceFieldForm) THEN
                DELETE OBJECT oBindingSourceFieldForm .
        END.

        System.ComponentModel.LicenseManager:CurrentContext = oCurrentLicenseContext .

        DisableTool ("ViewerDesignerDeleteControl":U) .
        EnableTool ("ViewerDesignerCustomize":U) .
        DisableTool ("ViewerDesignerSave":U) .
        DisableTool ("ViewerDesignerCancel":U) .

        ASSIGN oForm = THIS-OBJECT:FindForm () .

        IF VALID-OBJECT (oForm) THEN
            oForm:FormClosing:Unsubscribe (FormClosingHandler) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Substitutes the runtime control types with the design time control
                 types
        Notes:
        @param pcSourceType The source control type
        @return The control type to be used during design time
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER SubstituteType (pcSourceType AS CHARACTER):

        IF pcSourceType BEGINS "System.Windows.Forms.":U THEN
            RETURN pcSourceType .

        CASE pcSourceType:
            WHEN "Infragistics.Win.UltraWinEditors.UltraTextEditor":U THEN
                RETURN "System.Windows.Forms.TextBox":U .
            WHEN "Infragistics.Win.UltraWinEditors.UltraCheckEditor":U THEN
                RETURN "System.Windows.Forms.CheckBox":U .
            WHEN "Infragistics.Win.Misc.UltraLabel":U THEN
                RETURN "System.Windows.Forms.Label":U .
            WHEN "Infragistics.Win.Misc.UltraGroupBox":U THEN
                RETURN "System.Windows.Forms.GroupBox":U .
            WHEN "Infragistics.Win.UltraWinTabControl.UltraTabControl":U THEN
                RETURN "System.Windows.Forms.TabControl":U .
            OTHERWISE
                RETURN "System.Windows.Forms.Label":U .

        END CASE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Tick event of the timer1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID timer1_Tick (sender AS System.Object, e AS System.EventArgs):

        DEFINE VARIABLE oCollection AS "System.Collections.ObjectModel.Collection<System.ComponentModel.Component>" NO-UNDO .

        THIS-OBJECT:OnFieldCreated (NEW FieldCreatedEventArgs (oNewControl, oNewLabel)) .

        IF VALID-OBJECT (oNewLabel) AND VALID-OBJECT (oNewControl) THEN DO:

            oNewLabel:Parent   = oNewControl:Parent .
            oNewLabel:Location = NEW System.Drawing.Point (MAX (oNewControl:Left - 100, 0), oNewControl:Top + 3) .

            oCollection = NEW "System.Collections.ObjectModel.Collection<System.ComponentModel.Component>" () .

            oCollection:Add (oNewControl) .
            oCollection:Add (oNewLabel) .

            oSelectionService:SetSelectedComponents (oCollection) .
        END.

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

        FINALLY:
            ASSIGN oNewLabel      = ?
                   oNewControl    = ?
                   timer1:Enabled = FALSE .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the UltraToolbarsManager
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ToolClickHandler (sender AS System.Object,
                                            e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):

        CASE e:Tool:Key:
            WHEN "ViewerDesignerDeleteControl":U THEN
                DeleteControls () .
            WHEN "ViewerDesignerCancel":U THEN
                CAST (THIS-OBJECT:SourceControl, IDynamicViewerControl):CloseDesigner () .
            WHEN "ViewerDesignerSave":U THEN
                CAST (THIS-OBJECT:SourceControl, IDynamicViewerControl):SaveDesigner (oTemplateControl) .

        END CASE .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Builds an unique name for a Control
        Notes:
        @param pcControlName The proposed name for the Control
        @param poParentControl The controls parent
        @return The unique name for the control
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER UniqueControlName (pcControlName AS CHARACTER,
                                                  poParentControl AS System.Windows.Forms.Control):

        DEFINE VARIABLE i     AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cName AS CHARACTER NO-UNDO.

        IF NOT poParentControl:Controls:ContainsKey (pcControlName) THEN
            RETURN pcControlName .

        REPEAT:
            ASSIGN i     = i + 1
                   cName = SUBSTITUTE ("&1&2":U,
                                       pcControlName,
                                       STRING (i)) .

            IF NOT poParentControl:Controls:ContainsKey (cName) THEN
                RETURN cName .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a unique control name for the serialization
        Notes:
        @param poControl The reference to the control
        @return The unique control name for the serialization
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER UniqueControlNameForSerialization (poControl AS System.Windows.Forms.Control):

        DEFINE BUFFER ttViewerDesign FOR ttViewerDesign .

        DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i     AS INTEGER   NO-UNDO.

        ASSIGN cName = poControl:Name .

        IF NOT CAN-FIND (ttViewerDesign WHERE ttViewerDesign.ControlName = cName) THEN
            RETURN cName .

        REPEAT:
            i = i + 1 .

            IF NOT CAN-FIND (ttViewerDesign WHERE ttViewerDesign.ControlName = SUBSTITUTE ("&1&2":U,
                                                                                           cName, i)) THEN
                RETURN SUBSTITUTE ("&1&2":U, cName, i) .

        END.

        RETURN cName.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Gets a value indicating whether the specified name is valid.
        Notes:   his method is similar to IsValidName, except that this method throws
                 an exception if the name is invalid. This allows implementers to
                 provide detailed information in the exception message.
        @param name The name for the new control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ValidateName (name AS CHARACTER):

        RETURN .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartViewerDesignerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartViewerDesignerControl ( ):

        THIS-OBJECT:ShutDown() .

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

        CATCH err AS Progress.Lang.Error :
            /* noop */
        END CATCH.

    END DESTRUCTOR.

END CLASS.