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
    File        : SmartBusinessEntityLookupDesign
    Purpose     :
    Syntax      :
    Description : Implements ISmartBusinessEntityDesign, intended to allow
                  reuse of the SmartLookupDesignerSupport code with
                  alternative implementations of the SmartBusinessEntityLookup
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Apr 01 13:29:32 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                          FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.SmartLookup     FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*     FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*         FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*            FROM PROPATH .
USING Consultingwerk.Util.*                               FROM PROPATH .
USING Progress.Lang.*                                     FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Base.SmartBusinessEntityLookupDesign
    INHERITS SmartLookup
    IMPLEMENTS ISmartBusinessEntityLookupDesign,
               Consultingwerk.SmartComponents.ILookupKeyValue
    ABSTRACT:

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartBusinessEntityLookupDesign class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityLookupDesign ():
        SUPER ().

        THIS-OBJECT:BindableProperties = "LookupKeyValue":U .

        IF THIS-OBJECT:DesignTime THEN
            THIS-OBJECT:SetDesignerProperties () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Raised when the LookupKeyValue property has changed
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT LookupKeyValueChanged DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the comma delimited list of additional fields that
                 will be retrieved by the lookup operations
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupAdditionalFields AS CHARACTER NO-UNDO INITIAL "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the SmartBusinessEntityLookup instance cascades
                 the PerformLookup activity to linked (child) lookups
        Notes:   See SCL-345 for details
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupCascade AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and Sets the Class Name of the Lookup Dialog Form
        Notes:   The class needs to implement IBusinessEntityLookupDialogForm and
                 inherits System.Windows.Forms.Form
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupDialogClassName AS CHARACTER NO-UNDO INITIAL "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and Sets the if the last column of the Lookup Dialog should
                 be expanded
        Notes:   Default is TRUE
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupDialogExpandLastColumn AS LOGICAL NO-UNDO INITIAL TRUE
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Fields that allow to Filter on the Lookup Browser Dialog
        Notes:   Comma-Delimited, first entry is default
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupDialogFilterFields AS CHARACTER NO-UNDO INITIAL "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Operators for the LookupDialogFilterFields
        Notes:   Comma-Delimited, first entry is default
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupDialogFilterOperators AS CHARACTER NO-UNDO INITIAL "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Option to open the Lookup Dialog Query when the dialog is opened
        Notes:   SCL-673
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupDialogOpenQuery AS LOGICAL NO-UNDO INITIAL TRUE
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: QuerySort used when opening the Lookup Browser Dialog
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupDialogQuerySort AS CHARACTER NO-UNDO INITIAL "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: QueryString used when opening the Lookup Browser Dialog
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupDialogQueryString AS CHARACTER NO-UNDO INITIAL "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Name of the BusinessEntity for the Lookup.
                 This Property is set by the Developer during DesignTime.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupEntityName AS CHARACTER NO-UNDO INITIAL "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: RootTable for this Instance of the BusinessEntityLookup.
                 This Property is set by the Developer during DesignTime.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupEntityTable AS CHARACTER NO-UNDO INITIAL "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: List of Tables to read with the EntityTable.
                 This Property is set by the Developer during DesignTime.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupEntityView AS CHARACTER NO-UNDO INITIAL "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the current KeyValue of the Lookup
        Notes:   Data-Bindable Property
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupKeyValue AS CHARACTER NO-UNDO INITIAL "":U
    GET.
    SET (arg AS CHARACTER):
        /* Mike Fechner, Consultingwerk Ltd. 24.03.2011
           Compare old and new value before raising OnLookupKeyValueChanged event */
        IF arg <> THIS-OBJECT:LookupKeyValue THEN DO:
            THIS-OBJECT:LookupKeyValue = arg .
            THIS-OBJECT:OnLookupKeyValueChanged (System.EventArgs:Empty) .
        END.
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the name of the Data Source column which is used for
                 retrieving the LookupKeyValue property
        Notes:   Data-Bindable Property
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupKeyValueColumn AS CHARACTER NO-UNDO INITIAL "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: QuerySort used in PerformLookup method
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupQuerySort AS CHARACTER NO-UNDO INITIAL "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: QueryString used in PerformLookup method
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupQueryString AS CHARACTER NO-UNDO INITIAL "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Exports a Lookup definition to a file
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ExportLookupDefinition ():

        DEFINE VARIABLE lOk AS LOGICAL NO-UNDO.

        SmartBusinessEntityLookupSupport:ExportLookupToFile
              (CAST (THIS-OBJECT, SmartBusinessEntityLookup),
               OUTPUT lOk) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to the Container component/control
        Notes:   We were having issues wirth adding the .NET property Container
                 to the Interface ISmartBusinessEntityLookup, hence the method
        @return The reference to the IContainer of this Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.ComponentModel.IContainer GetContainer ():

        RETURN THIS-OBJECT:Container .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Imports a Lookup Definition from a file
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ImportLookupDefinition ():

        DEFINE VARIABLE lOk AS LOGICAL NO-UNDO.

        SmartBusinessEntityLookupSupport:ImportLookupFromFile
              (CAST (THIS-OBJECT, SmartBusinessEntityLookup),
               OUTPUT lOk) .

        IF lOk THEN
            Consultingwerk.Util.DesignerHelper:RaiseComponentChanged (THIS-OBJECT,
                                                                      "":U,
                                                                      ?,
                                                                      ?) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the KeyValueChanged event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnLookupKeyValueChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:LookupKeyValueChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler method for Designer Verbs
        Notes:   This method is intended to be overridden
        @param pcVerbText The label of the verb the developer clicked on in the Visual Designer
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE VOID OnVerbClicked (pcVerbText AS CHARACTER):

        Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = FALSE .

        CASE pcVerbText:
            WHEN "Lookup Designer":U THEN
                ShowLookupDesigner () .
            WHEN "Select Tables":U THEN
                ShowTablePickerDialog () .
            WHEN "Select BusinessEntity":U THEN
                ShowBusinessEntityPickerDialog () .
            WHEN "Select Lookup Dialog":U THEN
                ShowLookupDialogPickerDialog () .
            WHEN "Export Lookup Definition":U THEN
                ExportLookupDefinition () .
            WHEN "Import Lookup Definition":U THEN
                ImportLookupDefinition () .
        END CASE .

        SUPER:OnVerbClicked (pcVerbText) .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

        FINALLY:
            Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = TRUE .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets Properties to customize Visual Designer behaviour
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SetDesignerProperties ():

        ASSIGN
            THIS-OBJECT:DesignerVerbs         = TRIM (THIS-OBJECT:DesignerVerbs + ",Lookup Designer,Select BusinessEntity,Select Lookup Dialog,Select Tables,Export Lookup Definition,Import Lookup Definition":U, ",":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Shows a dialog to select a Business Entity
        Notes:   Invoked by the "Select BusinessEntity" Designer Verb, only intended
                 for DesignTime
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ShowBusinessEntityPickerDialog ():

        DEFINE VARIABLE cEntityName AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cOldValue   AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hDataset    AS HANDLE    NO-UNDO .

        IF NOT THIS-OBJECT:DesignTime AND NOT FrameworkSettings:AllowDesignFeatures THEN
            RETURN .

        Consultingwerk.SmartComponents.Support.BusinessEntityDesignerSupport:SelectBusinessEntityName (OUTPUT cEntityName).

        IF cEntityName > "":U THEN DO:
            ASSIGN cOldValue                    = THIS-OBJECT:LookupEntityName
                   THIS-OBJECT:LookupEntityName = cEntityName .

            Consultingwerk.Util.DesignerHelper:RaiseComponentChanged (THIS-OBJECT,
                                                                      "LookupEntityName":U,
                                                                      BOX (cOldValue),
                                                                      BOX (cEntityName)) .

            IF THIS-OBJECT:LookupEntityTable = "":U THEN
            DO ON ERROR UNDO, THROW:
                Consultingwerk.Framework.FrameworkSettings:ServiceAdapter:FetchDataset ("":U,
                                                                                        cEntityName,
                                                                                        OUTPUT DATASET-HANDLE hDataset) .

                IF VALID-HANDLE (hDataset) AND hDataset:NUM-BUFFERS > 0 THEN DO:

                    THIS-OBJECT:LookupEntityTable = hDataset:GET-BUFFER-HANDLE (1):NAME .

                    Consultingwerk.Util.DesignerHelper:RaiseComponentChanged (THIS-OBJECT,
                                                                              "LookupEntityTable":U,
                                                                              BOX (cOldValue),
                                                                              BOX (cEntityName)) .
                END.

                CATCH err AS Progress.Lang.Error:
                    ErrorHelper:ShowErrorMessage (err) .
                END CATCH.

                FINALLY:
                    GarbageCollectorHelper:DeleteObject (hDataset) .
                END FINALLY.
            END.
        END.

        Consultingwerk.Util.DesignerHelper:RefreshPropertyGrid (THIS-OBJECT) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invokes ther Lookup Designer Dialog
        Notes:   Invoked by the "Lookup Designer" Designer Verb, only intended
                 for DesignTime
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ShowLookupDesigner ():

        IF NOT THIS-OBJECT:DesignTime AND NOT FrameworkSettings:AllowDesignFeatures THEN
            RETURN .

        SmartLookupDesignerSupport:ShowLookupDesignerSupport (THIS-OBJECT) .

        Consultingwerk.Util.DesignerHelper:RaiseComponentChanged (THIS-OBJECT,
                                                                  "LookupKeyField":U,
                                                                  ?,
                                                                  ?) .

        Consultingwerk.Util.DesignerHelper:RefreshPropertyGrid (THIS-OBJECT) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invokes the Lookup Dialog Picker Dialog and sets properties of the
                 SmartBusinessEntityLookup during Design Time
        Notes:   Invoked by the "Select Lookup Dialog" Designer Verb. Only intended for
                 DesignTime
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ShowLookupDialogPickerDialog ():

        DEFINE VARIABLE cClassName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cOldValue  AS CHARACTER NO-UNDO.

        IF NOT THIS-OBJECT:DesignTime THEN
            RETURN .

        SmartLookupDesignerSupport:SelectLookupDialogClassName (OUTPUT cClassName) .

        IF cClassName > "":U THEN DO:
            ASSIGN cOldValue                         = THIS-OBJECT:LookupDialogClassName
                   THIS-OBJECT:LookupDialogClassName = cClassName .

            Consultingwerk.Util.DesignerHelper:RaiseComponentChanged (THIS-OBJECT,
                                                                      "LookupDialogClassName":U,
                                                                      BOX (cOldValue),
                                                                      BOX (cClassName)) .
        END.

        Consultingwerk.Util.DesignerHelper:RefreshPropertyGrid (THIS-OBJECT) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invokes the Table Picker Dialog and sets properties of the
                 SmartBusinessEntityLookup during Design Time
        Notes:   Invoked by the "Select Tables" Designer Verb. Only intended for
                 DesignTime
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ShowTablePickerDialog ():

        IF NOT THIS-OBJECT:DesignTime AND NOT FrameworkSettings:AllowDesignFeatures THEN
            RETURN .

        SmartLookupDesignerSupport:ShowTablePickerDialog (THIS-OBJECT) .

        Consultingwerk.Util.DesignerHelper:RaiseComponentChanged (THIS-OBJECT,
                                                                  "LookupEntityTable":U,
                                                                  ?,
                                                                  ?) .

        Consultingwerk.Util.DesignerHelper:RefreshPropertyGrid (THIS-OBJECT) .

    END METHOD.

END CLASS.
