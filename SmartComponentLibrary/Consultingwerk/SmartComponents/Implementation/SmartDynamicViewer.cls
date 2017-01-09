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
    File        : SmartDynamicViewer
    Purpose     : A dynamic viewer that can be styled by end users at runtime
    Syntax      :
    Description : This viewer control can be connected to a SmartDataAdapter
                  and the user can dynamically drop fields from the
                  SmartDataAdapter's binding source schema to the design
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Nov 09 21:11:40 CET 2010
    Notes       : Due to license restrictions the user will drop Microsoft
                  System.Windows.Forms controls to the design canvas, not
                  Infragistics Controls - this would require an Infragistics
                  developers license by the end user
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                      FROM PROPATH .
USING Consultingwerk.Framework.RepositoryServices.*   FROM PROPATH .
USING Consultingwerk.SmartComponents.Framework.*      FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Infragistics.Win.UltraWinTabControl.*           FROM ASSEMBLY .
USING System.ComponentModel.*                         FROM ASSEMBLY .
USING System.Windows.Forms.*                          FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartComponents.Implementation.SmartDynamicViewer
    INHERITS Consultingwerk.SmartComponents.Base.SmartViewerControl
    IMPLEMENTS Consultingwerk.Design.SmartComponents.Viewer.IDynamicViewerControl:

    {Consultingwerk/Framework/RepositoryServices/ttViewerDesign.i &ACCESS=PROTECTED}

    DEFINE VARIABLE oDesigner AS Consultingwerk.Design.SmartComponents.Viewer.SmartViewerDesignerControl NO-UNDO .
    DEFINE VARIABLE oRepository    AS IRepositoryService        NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets a reference to the UltraToolbarsManager instance
                 used by the Consultingwerk.Design.SmartComponents.Viewer.SmartViewerDesignerControl
                 instance
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DesignerToolbar AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Contains the key used to store settings of this Viewer
        Notes:   The SettingsKey will be generated as a GUID in the constructor of
                 this class. Typically this GUID will be overridden by a GUID
                 that was generated at the time the Control was added to a Form in
                 the Visual Designer
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SettingsKey AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets a dictionary of controls that should be substituted
                 by dynamic controls
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SubstituteDynamicControls AS "System.Collections.Generic.Dictionary<Component,System.String>":U NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Controls if type-substitution (from MS WinForms to Infragistics)
                 will be used during rendering
        Notes:   Set to false during design time rendering of the Viewer
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SubstituteTypes AS LOGICAL INITIAL TRUE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Creates a new instance of the SmartDynamicViewer class
        Notes:   The SettingsKey will be generated as a GUID in the constructor of
                 this class. Typically this GUID will be overridden by a GUID
                 that was generated at the time the Control was added to a Form in
                 the Visual Designer
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartDynamicViewer ():
        SUPER ().

        /* Mike Fechner, Consultingwerk Ltd. 12.10.2010
           A key to uniquely idnetify this instance, typically overwritten by the
           containing Form's initializeComponents method.  */
        THIS-OBJECT:SettingsKey = GUID .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Overrides AttachBindingSource in the SmartViewerControl class
        Notes:   Attaches Event handles to the viewer fields
        @param poBindingSource The BindingSource (typically of a DataAdapter) used by this SmartDynamicViewer
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID AttachBindingSource (poBindingSource AS Progress.Data.BindingSource):

        IF NOT VALID-OBJECT (poBindingSource) THEN
            RETURN .

        SUPER:AttachBindingSource (INPUT poBindingSource).

        InitializeControls () .
        InitializeControlEventHandler () .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Closes the Visual Designer instance
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CloseDesigner ():

        IF VALID-OBJECT (oDesigner) THEN DO:
            oDesigner:ShutDown() .
            THIS-OBJECT:Controls:Remove (oDesigner) .
            DELETE OBJECT oDesigner .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Loads the stored settings using the LoadSettings method and
                 creates the Control instances using the InitializeControls method
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID EndInit ():

        LoadSettings () .
        THIS-OBJECT:InitializeControls () .

        SUPER:EndInit().

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates the Control instances for all fields in the
                 ttViewerDesign temp-table (read by the LoadSettings method)
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeControls ():

        DEFINE VARIABLE oBindingSource AS Progress.Data.BindingSource  NO-UNDO .

        IF NOT VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN
            RETURN .

        ASSIGN oBindingSource = THIS-OBJECT:SmartDataSource:BindingSource .

        THIS-OBJECT:Controls:Clear() .

        InitializeControls ("":U, oBindingSource, THIS-OBJECT) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates the Control instances for all fields in the
                 ttViewerDesign temp-table (read by the LoadSettings method)
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeControlsWithoutBindingSource ():

        DEFINE VARIABLE oBindingSource AS Progress.Data.BindingSource  NO-UNDO .

        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN
            ASSIGN oBindingSource = THIS-OBJECT:SmartDataSource:BindingSource .

        THIS-OBJECT:Controls:Clear() .

        InitializeControls ("":U, oBindingSource, THIS-OBJECT) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates the Control instances in a container
        Notes:   Recursive method to allow nested containers (e.g. tab folder, group box)
        @param pcContainerInstanceId The key of the Container to render contained controls
        @param poBindingSource The reference to the BindingSource component
        @param poParentControl The reference to the parent control (matching the pcContainerInstanceId)
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeControls (pcContainerInstanceId AS CHARACTER,
                                              poBindingSource AS Progress.Data.BindingSource,
                                              poParentControl AS System.Windows.Forms.Control):

        DEFINE VARIABLE oControl     AS System.Windows.Forms.Control         NO-UNDO .
        DEFINE VARIABLE oType        AS System.Type                          NO-UNDO .
        DEFINE VARIABLE cType        AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE oTypeArray   AS "System.Type[]":U                    NO-UNDO .
        DEFINE VARIABLE oObjectArray AS "System.Object[]":U                  NO-UNDO .
        DEFINE VARIABLE i            AS INTEGER                              NO-UNDO .
        DEFINE VARIABLE hDataset     AS HANDLE                               NO-UNDO.
        DEFINE VARIABLE oSerializer  AS ISmartBusinessEntityLookupSerializer NO-UNDO .
        DEFINE VARIABLE hBuffer      AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE oAppearance  AS Infragistics.Win.Appearance          NO-UNDO .

        DEFINE BUFFER ttViewerDesign FOR ttViewerDesign .

        ASSIGN oTypeArray     = System.Type:EmptyTypes
               oObjectArray   = {Consultingwerk/new-array.i System.Object 0} .

        FOR EACH ttViewerDesign WHERE ttViewerDesign.ContainerInstanceId = pcContainerInstanceId
                                BY INTEGER (ENTRY (1, ttViewerDesign.ControlLocation))
                                BY INTEGER (ENTRY (2, ttViewerDesign.ControlLocation)) ON ERROR UNDO, THROW:

            IF THIS-OBJECT:SubstituteTypes THEN
                ASSIGN cType = SubstituteType (ttViewerDesign.ControlType) .
            ELSE
                ASSIGN cType = ttViewerDesign.ControlType .

            ASSIGN oType = Progress.Util.TypeHelper:GetType (cType) .

            IF ttViewerDesign.DynamicControlName > "":U AND NOT VALID-OBJECT (THIS-OBJECT:SubstituteDynamicControls) THEN DO:
                IF NOT VALID-OBJECT (oRepository) THEN
&IF DEFINED (UseSimpleDynamicsRepositoryService) NE 0 &THEN
                    ASSIGN oRepository = {Consultingwerk/get-service.i Consultingwerk.Framework.RepositoryServices.IRepositoryService} .

                    IF NOT VALID-OBJECT (oRepository) THEN DO:
                        oRepository = DYNAMIC-NEW "Consultingwerk.Framework.RepositoryServices.Dynamics.SimpleDynamicsRepositoryService":U ()  .

                        FrameworkSettings:ServiceContainer:AddService
                                         (Progress.Lang.Class:GetClass ("Consultingwerk.Framework.RepositoryServices.IRepositoryService":U),
                                         oRepository) .
                    END.
&ELSE
                    ASSIGN oRepository = {Consultingwerk/get-service.i Consultingwerk.Framework.RepositoryServices.IRepositoryService} .
&ENDIF

                IF NOT VALID-OBJECT (oRepository) THEN
                    UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to render complex control &1 without Repository Service.":U,
                                                          ttViewerDesign.DynamicControlName),
                                              0) .

                ASSIGN oSerializer = { Consultingwerk/get-service.i Consultingwerk.SmartComponents.Framework.ISmartBusinessEntityLookupSerializer
                                                                    "NEW SmartBusinessEntityLookupSerializer ()"} .

                oRepository:GetObjectByName (ttViewerDesign.DynamicControlName,
                                             OUTPUT DATASET-HANDLE hDataset) .

                hDataset:GET-BUFFER-HANDLE (1):FIND-FIRST () .

                oControl = DYNAMIC-NEW (hDataset:GET-BUFFER-HANDLE (1)::RenderingClassName) () .


                ASSIGN hBuffer = hDataset:GET-BUFFER-HANDLE (1) .

                oSerializer:Deserialize (hBuffer, CAST (oControl, SmartBusinessEntityLookup)) .

                CAST (oControl, SmartBusinessEntityLookup):AddLookupEditorButton () .

                DELETE OBJECT hDataset .
                ASSIGN hDataset = ? .
            END.
            ELSE
                oControl = CAST (oType:GetConstructor(oTypeArray):Invoke (oObjectArray),
                                 System.Windows.Forms.Control) .

            IF VALID-OBJECT (THIS-OBJECT:SubstituteDynamicControls) AND
               ttViewerDesign.DynamicControlName > "":U THEN
                THIS-OBJECT:SubstituteDynamicControls:Add (oControl, ttViewerDesign.DynamicControlName) .

            IF ttViewerDesign.ControlMultiline THEN
                Consultingwerk.Util.ReflectionHelper:SetPropertyValue (oControl,
                                                                       "Multiline":U,
                                                                       BOX (TRUE)) .

            ASSIGN oControl:Name = ttViewerDesign.ControlName
                   oControl:Text = ttViewerDesign.ControlText
                   oControl:Size = NEW System.Drawing.Size (INTEGER (ENTRY (1, ttViewerDesign.ControlSize)),
                                                            INTEGER (ENTRY (2, ttViewerDesign.ControlSize)))
                   oControl:Location = NEW System.Drawing.Point (INTEGER (ENTRY (1, ttViewerDesign.ControlLocation)),
                                                                 INTEGER (ENTRY (2, ttViewerDesign.ControlLocation)))

                   oControl:Dock = CAST (System.Enum:Parse (Progress.Util.TypeHelper:GetType("System.Windows.Forms.DockStyle":U),
                                                            ttViewerDesign.ControlDock,
                                                            TRUE), System.Windows.Forms.DockStyle) .
                   oControl:Anchor = CAST (System.Enum:Parse (Progress.Util.TypeHelper:GetType ("System.Windows.Forms.AnchorStyles":U),
                                                              ttViewerDesign.ControlAnchor) , System.Windows.Forms.AnchorStyles)
                      .

            THIS-OBJECT:ProcessControlProperties (cType, oControl) .

            IF ttViewerDesign.HAlign > "":U AND ReflectionHelper:HasProperty (oControl, "Appearance":U) THEN DO:

                ASSIGN oAppearance = CAST (ReflectionHelper:GetPropertyValue (oControl, "Appearance":U),
                                           Infragistics.Win.Appearance) .

                oAppearance:TextHAlign = {Consultingwerk/parse-enum.i Infragistics.Win.HAlign ttViewerDesign.HAlign} .
            END.

            IF VALID-OBJECT (poBindingSource) THEN
                DO i = 1 TO NUM-ENTRIES (ttViewerDesign.BindingProperties) ON ERROR UNDO, THROW:
                    oControl:DataBindings:Add (ENTRY (i, ttViewerDesign.BindingProperties),
                                               poBindingSource,
                                               ENTRY (i, ttViewerDesign.BindingFields),
                                               TRUE,
                                               DataSourceUpdateMode:OnPropertyChanged) .
                END.

            poParentControl:Controls:Add (oControl) .

            /* Recursion of UltraTabControl need separate handling */
            IF NOT TYPE-OF (oControl, Infragistics.Win.UltraWinTabControl.UltraTabControl) THEN
                THIS-OBJECT:InitializeControls (ttViewerDesign.ControlInstanceId,
                                                poBindingSource,
                                                oControl) .
            ELSE
                InitializeTabControl (ttViewerDesign.ControlInstanceId,
                                      poBindingSource,
                                      oControl) .

        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes a UltraTabControl
        Notes:
        @param pcContainerInstanceId The key of the Container to render contained controls (the UltraTabControl)
        @param poBindingSource The reference to the BindingSource component
        @param poParentControl The reference to the parent control (matching the pcContainerInstanceId)
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeTabControl (pcContainerInstanceId AS CHARACTER,
                                                poBindingSource AS Progress.Data.BindingSource,
                                                poParentControl AS System.Windows.Forms.Control):

        DEFINE BUFFER ttViewerDesign FOR ttViewerDesign .

        DEFINE VARIABLE oTabControl AS UltraTabControl NO-UNDO .
        DEFINE VARIABLE oTab        AS UltraTab        NO-UNDO .

        oTabControl = CAST (poParentControl, UltraTabControl) .

        FOR EACH ttViewerDesign WHERE ttViewerDesign.ContainerInstanceId = pcContainerInstanceId
                                BY INTEGER (ENTRY (1, ttViewerDesign.ControlLocation))
                                BY INTEGER (ENTRY (2, ttViewerDesign.ControlLocation)):

            oTab = oTabControl:Tabs:Add (ttViewerDesign.ControlInstanceId,
                                  ttViewerDesign.ControlText) .

            THIS-OBJECT:InitializeControls (ttViewerDesign.ControlInstanceId,
                                            poBindingSource,
                                            oTab:TabPage) .
        END .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Overlays the SmartDynamicViewer with the Designer.
        Notes:   Begins the visual design process from the end user
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InvokeDesigner ():

        oDesigner = NEW Consultingwerk.Design.SmartComponents.Viewer.SmartViewerDesignerControl () .

        oDesigner:SourceControl   = THIS-OBJECT .
        oDesigner:BindingSource   = THIS-OBJECT:SmartDataSource:BindingSource .
        oDesigner:DesignerToolbar = THIS-OBJECT:DesignerToolbar .

        oDesigner:Dock = System.Windows.Forms.DockStyle:Fill .
        THIS-OBJECT:Controls:Add (oDesigner) .
        oDesigner:Show() .
        oDesigner:BringToFront() .

        oDesigner:Initialize() .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Loads the Layout of the dynamic viewer.
        Notes:   This method is intended to be overridden. By default an XML file
                 in the application root folder is used.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID LoadSettings ():

        DEFINE VARIABLE oViewerRepository AS IDynamicViewerRepository NO-UNDO .

        oViewerRepository = {Consultingwerk/get-service.i
                                Consultingwerk.Framework.RepositoryServices.IDynamicViewerRepository
                                "NEW DefaultDynamicViewerRepository ()"} .

        oViewerRepository:LoadSettings (THIS-OBJECT:SettingsKey,
                                        OUTPUT TABLE ttViewerDesign BY-REFERENCE) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Processes specific default property values based on the Type of the Control
        Notes:
        @param pcType The Type of the Control
        @param poControl The reference to the Control
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ProcessControlProperties (pcType AS CHARACTER,
                                                    poControl AS System.Windows.Forms.Control):

        CASE pcType:

            WHEN "Infragistics.Win.Misc.UltraLabel":U THEN DO:
                CAST (poControl, Infragistics.Win.Misc.UltraLabel):Appearance:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
            END .

        END CASE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves the current design using the SaveSettings method and closes
                 the Visual Designer
        Notes:
        @param poTemplateControl The reference to the Visual Designer root control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaveDesigner (poTemplateControl AS System.Windows.Forms.Control):

        oDesigner:Serialize (OUTPUT TABLE ttViewerDesign) .

        THIS-OBJECT:SaveSettings() .

        THIS-OBJECT:CloseDesigner() .

        THIS-OBJECT:InitializeControls () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves the Layout of the dynamic viewer.
        Notes:   This method is intended to be overridden. By default an XML file
                 in the application root folder is created.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaveSettings ():

        DEFINE VARIABLE oViewerRepository AS IDynamicViewerRepository NO-UNDO .

        oViewerRepository = {Consultingwerk/get-service.i
                                Consultingwerk.Framework.RepositoryServices.IDynamicViewerRepository
                                "NEW DefaultDynamicViewerRepository ()"} .

        oViewerRepository:SaveSettings (THIS-OBJECT:SettingsKey,
                                        TABLE ttViewerDesign BY-REFERENCE) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs generic type substitution (Microsoft to Infragistics equivalent)
        Notes:
        @param pcBaseType The Type name used at design time
        @return The Type name used at runtime
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER SubstituteType (pcBaseType AS CHARACTER):

        CASE pcBaseType:
            WHEN "System.Windows.Forms.TextBox":U THEN
                RETURN "Infragistics.Win.UltraWinEditors.UltraTextEditor":U .
            WHEN "System.Windows.Forms.CheckBox":U THEN
                RETURN "Infragistics.Win.UltraWinEditors.UltraCheckEditor":U .
            WHEN "System.Windows.Forms.Label":U THEN
                RETURN "Infragistics.Win.Misc.UltraLabel":U .
            WHEN "System.Windows.Forms.GroupBox":U THEN
                RETURN "Infragistics.Win.Misc.UltraGroupBox":U .
            WHEN "System.Windows.Forms.TabControl":U THEN
                RETURN "Infragistics.Win.UltraWinTabControl.UltraTabControl":U .

        END CASE .

        RETURN pcBaseType .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the SmartDynamicViewer class
        Notes:   Deletes the designer instance if still valid.
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartDynamicViewer ():

        GarbageCollectorHelper:DeleteObject (oDesigner) .

    END DESTRUCTOR.

END CLASS.
