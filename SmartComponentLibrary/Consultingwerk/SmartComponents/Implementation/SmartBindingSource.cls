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
    File        : SmartBindingSource
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Feb 25 08:23:25 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.*           FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Resources.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*   FROM PROPATH .
USING Consultingwerk.Util.*                      FROM PROPATH .
USING Progress.Data.*                            FROM ASSEMBLY .
USING Progress.Lang.*                            FROM ASSEMBLY .
USING System.ComponentModel.*                    FROM ASSEMBLY .
USING System.ComponentModel.Design.*             FROM ASSEMBLY .
USING System.Windows.Forms.*                     FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Implementation.SmartBindingSource
    INHERITS BindingSource
    IMPLEMENTS ICustomTypeDescriptor, IHasDesignerVerbs :

    {Consultingwerk/SmartComponents/Support/ttTablesAndFields.i}
    {Consultingwerk/SmartComponents/Support/dsMetaschema.i}

    /* Mike Fechner, Consultingwerk Ltd. 16.05.2010
       Core implementation of ICustomTypeDescriptor */
    {Consultingwerk/SmartComponents/Base/ICustomTypeDescriptor.i}
    {Consultingwerk/SmartComponents/Base/CreateVerbs.i}

    /*------------------------------------------------------------------------------
        Purpose: If Property is set a Dialog is Shown to Add fields from the
                 Metaschema to the Design Canvas.
        Notes:   The Value of this Property is not changed but the Dialog is started
                 when temporarily set to TRUE.
                 Workaround to be able to start a Dialog from Propertygrid.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AddFields AS LOGICAL NO-UNDO INIT FALSE
    GET():
        RETURN FALSE .
    END GET.
    SET(INPUT arg AS LOGICAL):
        IF NOT THIS-OBJECT:DesignTime THEN
            RETURN .

        IF arg = TRUE THEN .
        ELSE RETURN .

        DesignerAddFields().

    END SET.

    /*------------------------------------------------------------------------------
        Purpose: If Property is set the Metaschema is cleared.
        Notes:   The Value of this Property is not changed when temporarily set to
                 TRUE.
                 Workaround to be able to start Tasks from Propertygrid.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClearSchema AS LOGICAL NO-UNDO INIT FALSE
    GET():
        RETURN FALSE .
    END GET.
    SET(INPUT arg AS LOGICAL):
        IF NOT THIS-OBJECT:DesignTime THEN
            RETURN .

        IF arg = TRUE THEN .
        ELSE RETURN .

        DesignerClearSchema().

    END.

    /*------------------------------------------------------------------------------
        Purpose: A Dictionary of custom types
        Notes:   Allows simplyfied customization of the SmartBindingSource in case a
                 customer wants to create instances of derived control types. The
                 dictionary should contain the type names of the derived contorls.
                 Controls will need to extend the following Infragistics types:

                 INTEGER, DECIMAL:      Infragistics.Win.UltraWinEditors.UltraNumericEditor
                 LOGICAL:               Infragistics.Win.UltraWinEditors.UltraCheckEditor
                 DATE, DATETIME:        Infragistics.Win.UltraWinEditors.UltraDateTimeEditor
                 CHARACTeR (OTHERWISE): Infragistics.Win.UltraWinEditors.UltraTextEditor
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY CustomControlTypes AS "System.Collections.Generic.Dictionary<Progress.Data.DataType,System.String>" NO-UNDO
    GET.
    SET.

     /*------------------------------------------------------------------------------
        Purpose: If Property is set a Dialog is Shown where the user can choose a
                 Fields from a Databasetable to be automaticly build into the
                 SmartBindingSources Metaschema.
                 The Value of this Property is not changed but the Dialog is started
                 when temporarily set to TRUE.
        Notes:   Workaround to be able to start a Dialog from Propertygrid.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ImportSchema AS LOGICAL NO-UNDO INIT FALSE
    GET():
        RETURN FALSE.
    END GET.
    SET(INPUT arg AS LOGICAL):
        IF NOT THIS-OBJECT:DesignTime THEN
            RETURN .

        IF arg = TRUE THEN .
        ELSE RETURN .

        DesignerImportSchema().

    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Propery flags if the current instance of the SmartBindingSource
                 is running at design time or runtime.
        Notes:   Set during constructor using a check at the
                 System.ComponentModel.LicenseManager
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY DesignTime AS LOGICAL NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to generate Designer Verbs (actions for the
                 Visual Designer)
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY DesignerVerbs AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to mark properties (comma-delimited list) that
                 will be excluded from the .NET TypeDescriptor properties list
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY HiddenProperties AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to mark properties (comma-delimited list) that
                 should not show up in the Visual Designers Property Grid
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY NonBrowsableProperties AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartBindingSource class. Set's the
                 DesignTime property using the LicenseManager.
        Notes:   According to
                 http://dotnetfacts.blogspot.com/2009/01/identifying-run-time-and-design-mode.html
                 that is only possible during the constructor of the class.
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBindingSource ():
        SUPER ().

        THIS-OBJECT:DesignTime = Progress.Util.EnumHelper:AreEqual(System.ComponentModel.LicenseManager:UsageMode,
                                                                   System.ComponentModel.LicenseUsageMode:Designtime) .

        THIS-OBJECT:SetDesignerProperties () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartBindingSource class
        Notes:   Set's the DesignTime property using the LicenseManager. According to
                 http://dotnetfacts.blogspot.com/2009/01/identifying-run-time-and-design-mode.html
                 that is only possible during the constructor of the class.
        @param poContainer The IContainer to add the SmartBusinessEntityAdapter to.
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBindingSource (poContainer AS System.ComponentModel.IContainer):

        SUPER (INPUT poContainer).

        THIS-OBJECT:DesignTime = Progress.Util.EnumHelper:AreEqual(System.ComponentModel.LicenseManager:UsageMode,
                                                                   System.ComponentModel.LicenseUsageMode:Designtime) .

        THIS-OBJECT:SetDesignerProperties () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Create a Control for a field from the Design time binding source
        Notes:   Uses IDesignerHost:CreateComponent
        @param poType The DataType of the field
        @param pcControlName The name of the Control
        @param pcColumnName The name of the Column
        @param pcColumnLabel The label of the Column
        @return The reference to the Control that was created
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED System.Windows.Forms.Control CreateControlForField (poType AS Progress.Data.DataType,
                                                                         pcControlName AS CHARACTER,
                                                                         pcColumnName AS CHARACTER,
                                                                         pcColumnLabel AS CHARACTER):

        DEFINE VARIABLE oControl    AS System.Windows.Forms.Control NO-UNDO .
        DEFINE VARIABLE oCustomType AS System.Type                  NO-UNDO .
        DEFINE VARIABLE cCustomType AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE oType       AS System.Type                  NO-UNDO .
        DEFINE VARIABLE oObject     AS Progress.Lang.Object         NO-UNDO .

        IF VALID-OBJECT (THIS-OBJECT:CustomControlTypes) AND
           THIS-OBJECT:CustomControlTypes:ContainsKey (poType) THEN DO:

            ASSIGN cCustomType = THIS-OBJECT:CustomControlTypes [poType]
                   oCustomType = Progress.Util.TypeHelper:GetType (cCustomType) .

            /* Mike Fechner, Consultingwerk Ltd. 20.05.2011
               In case the .NET type is not yet valid (ABL inherited contorl) we have
               to create an instance of the custom control to force emitting the type
               to .NET - Then we will be able to get a reference to the .NET Type */
            IF NOT VALID-OBJECT (oCustomType) THEN DO:

                oObject = DYNAMIC-NEW (cCustomType) ().
                DELETE OBJECT oObject .

                oCustomType = Progress.Util.TypeHelper:GetType (cCustomType) .
            END.
        END.

        /* Second: Add the DataField to the Form. The type of the Control depends on the Datatype of the field
                   in the BindingSourceSchema */
        CASE poType:ToString() :
            WHEN "INTEGER":U OR
            WHEN "DECIMAL":U THEN DO:
                IF NOT VALID-OBJECT (oCustomType) THEN
                    ASSIGN oType = Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinEditors.UltraNumericEditor":U) .
                ELSE
                    ASSIGN oType = oCustomType .

                oControl = CAST(CAST(THIS-OBJECT:Container,
                                     IDesignerHost):CreateComponent (oType,
                                                                     pcControlName) ,
                                                                     Infragistics.Win.UltraWinEditors.UltraNumericEditor) .

                IF poType:ToString() = "DECIMAL":U THEN
                    ASSIGN
                        CAST(oControl, Infragistics.Win.UltraWinEditors.UltraNumericEditor):NumericType = Infragistics.Win.UltraWinEditors.NumericType:Double
                        CAST(oControl, Infragistics.Win.UltraWinEditors.UltraNumericEditor):TabNavigation = Infragistics.Win.UltraWinMaskedEdit.MaskedEditTabNavigation:NextControl.

&IF NOT PROVERSION EQ "10.2B" &THEN
                CAST(oControl, Infragistics.Win.UltraWinEditors.UltraNumericEditor):DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
&ENDIF

                oControl:DataBindings:Add("Value":U, THIS-OBJECT, pcColumnName) .
            END.
            WHEN "LOGICAL":U THEN DO:
                IF NOT VALID-OBJECT (oCustomType) THEN
                    ASSIGN oType = Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinEditors.UltraCheckEditor":U) .
                ELSE
                    ASSIGN oType = oCustomType .

                oControl = CAST(CAST(THIS-OBJECT:Container,
                                     IDesignerHost):CreateComponent (oType,
                                                                     pcControlName) ,
                                                                     Infragistics.Win.UltraWinEditors.UltraCheckEditor) .

                /* Mike Fechner, Consultingwerk Ltd. 16.09.2009
                   Bind logical fields to checked property (not value which is not available)  */
                /* Mike Fechner, Consultingwerk Ltd. 15.03.2010
                   Bind UltraCheckEditor to CheckedValue to allow ? values */
                oControl:DataBindings:Add("CheckedValue":U, THIS-OBJECT, pcColumnName) .
                oControl:Text = pcColumnLabel  .
            END.
            WHEN "DATE":U OR WHEN "DATETIME":U THEN DO:
                IF NOT VALID-OBJECT (oCustomType) THEN
                    ASSIGN oType = Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinEditors.UltraDateTimeEditor":U) .
                ELSE
                    ASSIGN oType = oCustomType .

                oControl = CAST(CAST(THIS-OBJECT:Container,
                                     IDesignerHost):CreateComponent (oType,
                                                                     pcControlName) ,
                                                                     Infragistics.Win.UltraWinEditors.UltraDateTimeEditor) .

                /* Set additional properties */
                CAST(oControl, Infragistics.Win.UltraWinEditors.UltraDateTimeEditor):AutoFillDate =
                    Infragistics.Win.UltraWinMaskedEdit.AutoFillDate:MonthAndYear .

&IF NOT PROVERSION EQ "10.2B" &THEN
                CAST(oControl, Infragistics.Win.UltraWinEditors.UltraDateTimeEditor):DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
&ENDIF

                oControl:DataBindings:Add("Value":U, THIS-OBJECT, pcColumnName) .
            END.
            OTHERWISE DO:
                IF NOT VALID-OBJECT (oCustomType) THEN
                    ASSIGN oType = Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinEditors.UltraTextEditor":U) .
                ELSE
                    ASSIGN oType = oCustomType .

                oControl = CAST(CAST(THIS-OBJECT:Container,
                                     IDesignerHost):CreateComponent (oType,
                                                                     pcControlName) ,
                                                                     Infragistics.Win.UltraWinEditors.UltraTextEditor) .

&IF NOT PROVERSION EQ "10.2B" &THEN
                CAST(oControl, Infragistics.Win.UltraWinEditors.UltraTextEditor):DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
&ENDIF

                oControl:DataBindings:Add("Value":U, THIS-OBJECT, pcColumnName) .
            END.
        END CASE.

        RETURN oControl .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Create a Control for the label of a field from the Design time
                 binding source
        Notes:   Uses IDesignerHost:CreateComponent
        @param poType The DataType of the Control
        @param pcControlName The name of the Control
        @param pcColumnName The name of the Column
        @param pcColumnLabel The label of the Column
        @return The reference to the Control that was created
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED System.Windows.Forms.Control CreateLabelForField (poType AS Progress.Data.DataType,
                                                                       pcControlName AS CHARACTER,
                                                                       pcColumnName AS CHARACTER,
                                                                       pcColumnLabel AS CHARACTER):

        DEFINE VARIABLE oControl AS Infragistics.Win.Misc.ControlBase NO-UNDO .

        /* First: Add a Label to the Form and set Text and TabIndex Properties */
        oControl = CAST(CAST(THIS-OBJECT:Container,
                             IDesignerHost):CreateComponent (
                    Progress.Util.TypeHelper:GetType ("Infragistics.Win.Misc.UltraLabel":U),
                    pcControlName + "_Label":U) ,
                    Infragistics.Win.Misc.ControlBase) .

        /* Mike Fechner, Consultingwerk Ltd. 02.06.2009
           Labels should receive a transparent label background */
        IF NOT VALID-OBJECT(oControl:Appearance) THEN
               oControl:Appearance = NEW Infragistics.Win.Appearance () .

        ASSIGN
            oControl:Appearance:BackColorAlpha = Infragistics.Win.Alpha:Transparent
            oControl:Text                      = "&":U + pcColumnLabel + ":":U .

        RETURN oControl .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event Handler method for Designer Verbs
        Notes:   This method is intended to be overridden
        @param pcVerbText The name (Label) of the Designer Verb that was invoked
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OnVerbClicked (pcVerbText AS CHARACTER):

        Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = FALSE .

        CASE pcVerbText:
            WHEN "Import Schema":U THEN
                DesignerImportSchema () .
            WHEN "Add Fields":U THEN
                DesignerAddFields () .
            WHEN "Clear Schema":U THEN
                DesignerClearSchema() .
        END CASE .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

        FINALLY:
            Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = TRUE .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Recursively walk through the BindingSource predefined schema
                 and add every choosen Field to the Design Canvas.
        Notes:   This Method is called from DesignerAddFields () after the Dialog
                 to chose fields had been shown and one ore more fields has been
                 selected.
        @param oTable The current TableDesc to be processed
        @param plPrefixWithTable Shall ControlNames be prefixed with the TableName
        @param cChoosenColumns List of ColumnNames selected in the AddFields Dialog
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID RecurseAddControls(INPUT oTable AS Progress.Data.TableDesc,
                                           INPUT plPrefixWithTable AS LOGICAL,
                                           INPUT cChoosenColumns AS CHARACTER):

        DEFINE VARIABLE oChildTableArray   AS Progress.Data.TableDesc EXTENT NO-UNDO.
        DEFINE VARIABLE i                  AS INTEGER NO-UNDO.

        /* First add the fields of the given table */
        AddControlsForChosenFields(oTable, plPrefixWithTable, cChoosenColumns).

        /* Then loop through the child tables. These will always be prefixed with the tablename */
        IF NOT VALID-OBJECT(oTable:ChildTables) THEN
            RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 20.02.2010
           Only perform recursion when there are ChildTables */
        IF VALID-OBJECT(oTable:ChildTables) AND oTable:ChildTables:Length > 0 THEN DO:

            oChildTableArray = oTable:ChildTables.

            DO i = 1 TO EXTENT (oChildTableArray):
                IF VALID-OBJECT(oChildTableArray[i]) THEN
                    RecurseAddControls (oChildTableArray[i], TRUE, cChoosenColumns) .
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Add controls (Label and DataControl) for each choosen field of the
                 currently processed table to the design canvas.
        Notes:   This Method is called by RecurseAddControls ().
                 First the name of the new Controls is build from the column name
                 and the SmartBindingSource Instance Name. Possibly the Name
                 occures more than once so it gets iterated from the second time on.
                 After this an UltraLabel and a Datatype specific EditorControl is
                 added to the Design Canvas.
        @param oTable The TableDesc to be processed
        @param plPrefixWithTable Shall ControlNames be prefixed with the TableName
        @param cChoosenColumns List of ColumnNames selected in the AddFields Dialog
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AddControlsForChosenFields (INPUT oTable AS Progress.Data.TableDesc,
                                                      INPUT plPrefixWithTable AS LOGICAL,
                                                      INPUT cChoosenColumns AS CHARACTER):

        DEFINE VARIABLE oContainer           AS Control                      NO-UNDO .
        DEFINE VARIABLE oControl             AS Control                      NO-UNDO .

        DEFINE VARIABLE cColumnName          AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE cControlName         AS CHARACTER                    NO-UNDO .

        DEFINE VARIABLE oColumnArray         AS Progress.Data.ColumnPropDesc NO-UNDO EXTENT .
        DEFINE VARIABLE iControlCount        AS INTEGER                      NO-UNDO .
        DEFINE VARIABLE iFields              AS INTEGER                      NO-UNDO .

        DEFINE VARIABLE iIterate             AS INTEGER                      NO-UNDO .
        DEFINE VARIABLE cOriginalControlName AS CHARACTER                    NO-UNDO .

        DEFINE VARIABLE cLabelString         AS CHARACTER                    NO-UNDO .

        oContainer = CAST (CAST(THIS-OBJECT:Container,
                                IDesignerHost):RootComponent, Control) .

        ASSIGN
            oColumnArray  = oTable:Columns
            iControlCount = ROUND(oContainer:Controls:Count / 2, 0)
            .

        DO iFields = 1 TO EXTENT(oColumnArray):

            ASSIGN cColumnName           = SUBSTITUTE("&1_&2":U,
                                                   oTable:Name,
                                                   oColumnArray[iFields]:ColumnName).

            IF ListHelper:EntryIsInList (cColumnName, cChoosenColumns) THEN DO:
                IF plPrefixWithTable THEN
                    ASSIGN
                        cControlName          = cColumnName
                        .
                ELSE
                    ASSIGN
                        cControlName          = oColumnArray[iFields]:ColumnName .
                        .
                /* Prevent ControlNames from causing problems i.e. array fields containing brackets [] FieldName[1]*/
                /* Mike Fechner, Consultingwerk Ltd. 10.02.2011
                   Bug 2587: Replace "." (DOT) in Control Name */
                ASSIGN cControlName = DesignerHelper:DesignerSafeName (cControlName) .

                /* Check wheather the new columnname already exists and has to be iterated.
                   Set the new columnname */
                ASSIGN
                    iIterate             = 1
                    cOriginalControlName = cControlName
                    .

                /* Check if ControlName already used */
                DO WHILE oContainer:Controls:ContainsKey (cControlName):
                    /* A control with the same name was found.
                       Iterate the original name of the field and test again. */
                    ASSIGN
                        cControlName = cOriginalControlName + STRING(iIterate)
                        iIterate     = iIterate + 1
                        .
                END.

                /* Mike Fechner, Consultingwerk Ltd. 09.01.2012
                   Default to Fieldname as the label for empty Labels */
                ASSIGN cLabelString = oColumnArray[iFields]:ColumnLabel .

                IF cLabelString > "":U THEN .
                ELSE ASSIGN cLabelString = ENTRY (NUM-ENTRIES (oColumnArray[iFields]:ColumnName, ":":U),
                                                  oColumnArray[iFields]:ColumnName,
                                                  ":":U) .

                oControl = CreateLabelForField (oColumnArray[iFields]:ColumnType,
                                                cControlName,
                                                oColumnArray[iFields]:ColumnName,
                                                cLabelString) .

                ASSIGN
                    oControl:Size     = NEW System.Drawing.Size (100, 20)
                    oControl:Location = NEW System.Drawing.Point (20 , 24 + (26 * iControlCount))
                    oControl:AutoSize = TRUE
                    oControl:TabIndex = oContainer:Controls:Count + 1
                    .

                oContainer:Controls:Add(oControl) .

                oControl = ?.

                oControl = CreateControlForField (oColumnArray[iFields]:ColumnType,
                                                  cControlName,
                                                  oColumnArray[iFields]:ColumnName,
                                                  oColumnArray[iFields]:ColumnLabel) .

                oControl:SIZE     = NEW System.Drawing.Size (150, 20) .
                oControl:Location = NEW System.Drawing.Point (130 , 20 + (26 * iControlCount)) .
                oControl:TabIndex = oContainer:Controls:Count + 1.

                oContainer:Controls:Add(oControl) .

                iControlCount = iControlCount + 1.
            END. /* IF oChooser:checkedListBox1:CheckedItems:Contains(oColumnArray[iFields]:ColumnName) THEN DO: */
        END. /* DO iFields = 1 TO EXTENT(oColumnArray): */

        CATCH e AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(e, SmartComponentLibraryCustomizer:SmartBindingSource_AddControlsForChosenFieldsCatchBlock_Title) .
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Add the selected Fields from the current schema of the
                 SmartBindingSource to the design canvas.
        Notes:   A Dialog is shown to select fields and if fields are selected
                 the Controls are added recursively.
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DesignerAddFields ():

        DEFINE VARIABLE cChoosenColumns AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cMessages       AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i               AS INTEGER   NO-UNDO .

        DEFINE VARIABLE lSure            AS LOGICAL  NO-UNDO INIT TRUE .

        /* Check for Design Time Mode */
        IF NOT TYPE-OF(CAST(THIS-OBJECT:Container,
                            IDesignerHost):RootComponent, Control) THEN
           RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 24.08.2009
           Bug 2013: Access to Dialoges in Consultingwerk.SmartComponents.Design.dll
           outsourced to .p file, this removes the runtime dependency on the design
           time assembly.
           .R File is launched, to prevent runtime compile */
        IF SEARCH ("Consultingwerk/SmartComponents/Support/SmartBindingSourceDesigner_ShowAddFieldsChooserForm.r":U) = ?
            THEN DO:

            MESSAGE "Unable to invoke Consultingwerk.SmartComponents.Design.dll" SKIP(1)
                    "Consultingwerk/SmartComponents/Support/SmartBindingSourceDesigner_ShowAddFieldsChooserForm.r":U
                    "could not be found!"
                VIEW-AS ALERT-BOX ERROR TITLE "SmartBindingSource" .

            RETURN .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 08.10.2015
           Prompt "Are you sure" when function is executed for a BindingSource that
           is on a Form (and not on a Viewer/UserControl) */
        IF TYPE-OF (CAST(THIS-OBJECT:Container,
                         IDesignerHost):RootComponent, Form) THEN DO:

            MESSAGE "Are you sure that you want to add Controls to the Form?"{&TRAN} SKIP
                    "In most situations Data Bound Controls are added to Viewers."{&TRAN}
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Add Fields"{&TRAN} UPDATE lSure .

            IF lSure THEN .
            ELSE
                RETURN .
        END.

        RUN Consultingwerk/SmartComponents/Support/SmartBindingSourceDesigner_ShowAddFieldsChooserForm.r
                (THIS-OBJECT,
                 OUTPUT cChoosenColumns) NO-ERROR .

        IF ERROR-STATUS:ERROR THEN DO:
            DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
                ASSIGN cMessages = cMessages +
                                   (IF i > 1 THEN CHR(13) ELSE "":U) +
                                   ERROR-STATUS:GET-MESSAGE (i) .
            END.

            MESSAGE "Unable to invoke Consultingwerk.SmartComponents.Design.dll" SKIP(1)
                    cMessages
                VIEW-AS ALERT-BOX ERROR TITLE "SmartBindingSource" .

            RETURN .
        END.

        /* If the User has chosen one or more Fields to add to the Form do ... */
        IF NUM-ENTRIES(cChoosenColumns) > 0 THEN
            RecurseAddControls(THIS-OBJECT:TableSchema, TRUE, cChoosenColumns).

        CATCH e AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessageBox (e, SmartComponentLibraryCustomizer:SmartBindingSource_DesignerAddFieldsCatchBlock_Title) .
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
            Purpose: Clear the currently present Schema of the SmartBindingSource.
                     This Method is called when the Property ClearSchema is set to
                     TRUE.
            Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DesignerClearSchema(  ):
        /* Check for Design Time Mode */
        IF NOT TYPE-OF(CAST(THIS-OBJECT:Container,
                            IDesignerHost):RootComponent, Control) THEN
           RETURN .

        /* Clear TableSchema */
        THIS-OBJECT:TableSchema = ? .

        CATCH e AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (e, SmartComponentLibraryCustomizer:SmartBindingSource_DesignerClearSchemaCatchBlock_Title) .
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
            Purpose: Import the schema of one or more tables selected from one or
                     more connected databases.
                     This Method is called when the Property ImportSchema is set to
                     TRUE.

                     A Dataset is filled with the Metaschema information and passed
                     to a .NET Dialog as XML information. After the user has
                     choosen one or more fields an Alias is created for the choosen
                     Database and a second Dataset is filled with the Schema
                     information of the choosen Table and Fields. This second
                     Dataset is then processed and every selected Field is added to
                     a Tabledescription variable witch is then set as the
                     TableSchema of the current SmartBindingSource Instance.

            Notes:   The Method may be overridden to import the schema from other
                     datasources
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DesignerImportSchema ():
        DEFINE VARIABLE lcDataset                       AS LONGCHAR                       NO-UNDO .
        DEFINE VARIABLE cChoosenSchema                  AS CHARACTER                      NO-UNDO .
        DEFINE VARIABLE cDatabaseName                   AS CHARACTER                      NO-UNDO .
        DEFINE VARIABLE cTableName                      AS CHARACTER                      NO-UNDO .
        DEFINE VARIABLE cMode                           AS CHARACTER                      NO-UNDO .
        DEFINE VARIABLE cExceptList                     AS CHARACTER                      NO-UNDO .
        DEFINE VARIABLE cMessages                       AS CHARACTER                      NO-UNDO .
        DEFINE VARIABLE i                               AS INTEGER                        NO-UNDO .
        DEFINE VARIABLE oTableDesc                      AS Progress.Data.TableDesc        NO-UNDO .
        DEFINE VARIABLE oSmartBindingSourceArrayFactory AS SmartBindingSourceArrayFactory NO-UNDO .

        /* Check for Design Time Mode */
        IF NOT TYPE-OF(CAST(THIS-OBJECT:Container,
                            IDesignerHost):RootComponent, Control) THEN
           RETURN .

        FillDatasetMetaschema ().

        DATASET dsMetaschema:WRITE-XML("LONGCHAR":U, lcDataset).


        /* Mike Fechner, Consultingwerk Ltd. 24.08.2009
           Bug 2013: Access to Dialoges in Consultingwerk.SmartComponents.Design.dll
           outsourced to .p file, this removes the runtime dependency on the design
           time assembly.
           .R File is launched, to prevent runtime compile */
        IF SEARCH ("Consultingwerk/SmartComponents/Support/SmartBindingSourceDesigner_ShowImportSchemaForm.r":U) = ?
            THEN DO:

            MESSAGE "Unable to invoke Consultingwerk.SmartComponents.Design.dll" SKIP(1)
                    "Consultingwerk/SmartComponents/Support/SmartBindingSourceDesigner_ShowImportSchemaForm.r":U
                    "could not be found!"
                VIEW-AS ALERT-BOX ERROR TITLE "SmartBindingSource" .

            RETURN .
        END.

        RUN Consultingwerk/SmartComponents/Support/SmartBindingSourceDesigner_ShowImportSchemaForm.r
                (lcDataset,
                 OUTPUT cChoosenSchema) NO-ERROR .

        IF ERROR-STATUS:ERROR THEN DO:
            DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
                ASSIGN cMessages = cMessages +
                                   (IF i > 1 THEN CHR(13) ELSE "":U) +
                                   ERROR-STATUS:GET-MESSAGE (i) .
            END.

            MESSAGE "Unable to invoke Consultingwerk.SmartComponents.Design.dll" SKIP(1)
                    cMessages
                VIEW-AS ALERT-BOX ERROR TITLE "SmartBindingSource" .

            RETURN .
        END.

        /* if only a Database and a Tablename are returned leave */
        IF NUM-ENTRIES (cChoosenSchema) < 2 THEN
            RETURN.

        ASSIGN
            cMode                    = ENTRY(1, cChoosenSchema)
            ENTRY(1, cChoosenSchema) = "":U
            cChoosenSchema           = TRIM(cChoosenSchema, ",":U)
            THIS-OBJECT:TableSchema  = ?
            .

        oSmartBindingSourceArrayFactory = NEW SmartBindingSourceArrayFactory ().

        oTableDesc = NEW TableDesc ("Schema":U).

        CASE cMode:
            WHEN "MultipleTables":U THEN DO:
                DO i = 1 TO NUM-ENTRIES(cChoosenSchema):
                    ASSIGN
                        cDatabaseName = ENTRY(1, ENTRY(i, cChoosenSchema), ".":U)
                        cTableName    = ENTRY(2, ENTRY(i, cChoosenSchema), ".":U)
                        .

                    CREATE ALIAS DICTDB FOR DATABASE VALUE (cDatabaseName) .

                    RUN Consultingwerk/SmartComponents/Support/getTablesAndFields.p (OUTPUT DATASET dsSchema).

                    oTableDesc:Columns = oSmartBindingSourceArrayFactory:FillColumns(oTableDesc,
                                                                                     cTableName,
                                                                                     DATASET dsSchema,
                                                                                     "":U,
                                                                                     cExceptList).

                END.
            END.
            WHEN "SingleTable":U THEN DO:
                ASSIGN
                    cDatabaseName = ENTRY(1, cChoosenSchema)
                    cTableName    = ENTRY(2, cChoosenSchema)
                    .
                ASSIGN
                    ENTRY(1, cChoosenSchema) = "":U
                    ENTRY(2, cChoosenSchema) = "":U
                    cChoosenSchema           = TRIM(cChoosenSchema, ",":U)
                    .

                CREATE ALIAS DICTDB FOR DATABASE VALUE(cDatabaseName) .

                RUN Consultingwerk/SmartComponents/Support/getTablesAndFields.p (OUTPUT DATASET dsSchema).

                oTableDesc:Columns = oSmartBindingSourceArrayFactory:FillColumns(oTableDesc,
                                                                                 cTableName,
                                                                                 DATASET dsSchema,
                                                                                 cChoosenSchema,
                                                                                 cExceptList).

            END.
        END CASE.

        ASSIGN THIS-OBJECT:TableSchema = oTableDesc.

        CATCH e AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (e, SmartComponentLibraryCustomizer:SmartBindingSource_DesignerImportSchemaCatchBlock_Title) .
        END CATCH.

        FINALLY:
            IF VALID-OBJECT(oSmartBindingSourceArrayFactory) THEN
                DELETE OBJECT oSmartBindingSourceArrayFactory.
        END FINALLY.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Fill the Dataset dsMetaschema with all connected database names
                 their tables and fields.
                 This Method is called by DesignerImportSchema () to get a list
                 of all Databases their Tables and Fields to be passed into a
                 .NET dialog as XML information.
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID FillDatasetMetaschema ():
        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DATASET dsMetaschema:EMPTY-DATASET ().

        /* write all Databases to DatasetMetaschema */
        DO i = 1 TO NUM-DBS:
            CREATE eDatabase.

            ASSIGN eDatabase.DatabaseName = LDBNAME(i).

            RELEASE eDatabase.
        END.

        /* write all Tables to DatasetMetaschema*/
        FOR EACH eDatabase NO-LOCK ON ERROR UNDO, THROW:
            CREATE ALIAS DICTDB FOR DATABASE VALUE(eDatabase.DatabaseName) .

            RUN Consultingwerk/SmartComponents/Support/getTablesAndFields.p (OUTPUT DATASET dsSchema).

            FOR EACH ttTables NO-LOCK ON ERROR UNDO, THROW:
                CREATE eFile.

                ASSIGN
                    eFile.DatabaseName = eDatabase.DatabaseName
                    eFile.FileName     = ttTables.TableName
                    .

                /* write all Fields to DatasetMetaschema*/
                FOR EACH ttFields WHERE ttFields.TableName = eFile.FileName NO-LOCK ON ERROR UNDO, THROW:
                    CREATE eField.

                    ASSIGN
                        eField.DatabaseName = eDatabase.DatabaseName
                        eField.FileName     = eFile.FileName
                        eField.FieldName    = ttFields.ColumnName
                        .

                    RELEASE eField.
                END.

                RELEASE eFile.
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets Properties to customize Visual Designer behaviour
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SetDesignerProperties ():

        THIS-OBJECT:DesignerVerbs = "Import Schema,Add Fields,Clear Schema":U .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose:
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartBindingSource ( ):

        IF VALID-OBJECT (oDesignerVerbHelper) THEN
            DELETE OBJECT oDesignerVerbHelper .

    END DESTRUCTOR.

 END CLASS.
