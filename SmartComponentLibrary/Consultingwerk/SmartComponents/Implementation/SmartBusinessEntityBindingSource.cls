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
    File        : SmartBusinessEntityBindingSource
    Purpose     : Provides Design Time support for OERA Business Entities
    Syntax      :
    Description : Similar OERA support with SmartBusinessEntityAdapter
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Apr 07 09:36:41 CEST 2009
    Notes       : Inherits Progress.Data.BindingSource, enhancements only
                  for development, at runtime identical behaviour with
                  Progress.Data.BindingSource, aka ProBindingSource
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                      FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Resources.*      FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*        FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Data.*                                 FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource
    INHERITS SmartBindingSource :

    DEFINE VARIABLE cBindTo AS CHARACTER INITIAL "QUERY":U NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Binding the BusinessEntityBindingSource to a Query or a Dataset.
        Notes:   This Property is set by the Developer during DesignTime.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BindTo AS CHARACTER NO-UNDO INIT "":U
    GET():
        RETURN cBindTo.
    END GET.
    SET(arg AS CHARACTER):
        CASE arg:
            WHEN "QUERY":U OR
            WHEN "DATASET":U THEN
                ASSIGN cBindTo = arg.
            OTHERWISE
                UNDO, THROW NEW AppError ("Illegal value for property BindTo, possible values QUERY / DATASET.":U, 0).
        END CASE .
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: A List in the same oder as EntityView which defines Join (YES or NO)
                 for each Table of pcEntityView.
        Notes:   This Property is set by the Developer during DesignTime.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityJoin AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Name of the BusinessEntity.
        Notes:   This Property is set by the Developer during DesignTime.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: RootTable for this Instance of the BusinessEntity.
        Notes:   This Property is set by the Developer during DesignTime.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityTable AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: List of Tables to read with the EntityTable.
        Notes:   This Property is set by the Developer during DesignTime.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityView AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityBindingSource class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityBindingSource ():
        SUPER ().

        THIS-OBJECT:SetDesignerProperties () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityBindingSource class
        Notes:
        @param poContainer The System.ComponentModel.IContainer used as the Container for this component
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityBindingSource (poContainer AS System.ComponentModel.IContainer):

        SUPER (INPUT poContainer).

        THIS-OBJECT:SetDesignerProperties () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
            Purpose: Import the schema of a BusinessEntity defined by the Properties
                     EntityName, EntityTable, EntityView and EntityJoin.
                     This Method is called when the Property ImportSchema is set to
                     TRUE.
            Notes:   The current TableSchema of the SmartBusinessEntityBindingSource
                     is cleared out and the new Schema out of the BusinessEntity is
                     set.
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DesignerImportSchema ():

        THIS-OBJECT:DesignerImportSchema (FALSE) .

    END METHOD .

    /*------------------------------------------------------------------------------
            Purpose: Import the schema of a BusinessEntity defined by the Properties
                     EntityName, EntityTable, EntityView and EntityJoin.
                     This Method is called when the Property ImportSchema is set to
                     TRUE.
            Notes:   The current TableSchema of the SmartBusinessEntityBindingSource
                     is cleared out and the new Schema out of the BusinessEntity is
                     set.
            @param plSilent When True don't show message on completion
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DesignerImportSchema (plSilent AS LOGICAL):

        DEFINE VARIABLE i             AS INTEGER INITIAL 1    NO-UNDO.
        DEFINE VARIABLE hDataHandle   AS HANDLE               NO-UNDO.
        DEFINE VARIABLE hBuffer       AS HANDLE               NO-UNDO.

        DEFINE VARIABLE lFirstRun     AS LOGICAL              NO-UNDO.

        DEFINE VARIABLE oRootTable    AS TableDesc            NO-UNDO.
        DEFINE VARIABLE oWorkTable    AS TableDesc            NO-UNDO.

        DEFINE VARIABLE oArrayFactory AS DatasetRelationParser NO-UNDO.

        DEFINE VARIABLE cExceptList   AS CHARACTER            NO-UNDO.

        /* Clear the currently existing schema from the BindingSource */
        THIS-OBJECT:TableSchema = ? .

        hDataHandle = THIS-OBJECT:GetDesignTimeDataHandle (THIS-OBJECT:EntityName,
                                                           THIS-OBJECT:EntityTable,
                                                           THIS-OBJECT:EntityView,
                                                           THIS-OBJECT:EntityJoin,
                                                           THIS-OBJECT:BindTo) .

        IF hDataHandle:NUM-BUFFERS = 0 THEN DO:
            MESSAGE SmartComponentLibraryCustomizer:SmartBusinessEntityBindingSource_DesignerImportSchemaNoValidSchema
                VIEW-AS ALERT-BOX ERROR TITLE SmartComponentLibraryCustomizer:SmartBusinessEntityBindingSource_DesignerImportSchema_Title.

            RETURN.
        END.

        CASE cBindTo:
            WHEN "DATASET":U THEN DO:
                IF hDataHandle:NUM-RELATIONS > 0 THEN
                    oRootTable = RecurseDatasetRelations (hDataHandle, hDataHandle:GET-BUFFER-HANDLE(1)).
            END.
            OTHERWISE DO:
                ASSIGN lFirstRun = TRUE .

                DO i = 1 TO hDataHandle:NUM-BUFFERS:
                    hBuffer = hDataHandle:GET-BUFFER-HANDLE(i).

                    /* Create TableDesc with name of first Query-Buffer */
                    oWorkTable = NEW TableDesc (hBuffer:NAME).

                    oArrayFactory = NEW DatasetRelationParser().

                    IF lFirstRun THEN DO:
                        oRootTable = oWorkTable.
                    END.

                    oRootTable:Columns = oArrayFactory:GetTableFieldArray (hBuffer,
                                                                           oRootTable,
                                                                           cExceptList,
                                                                           BindingSourceHelper:UseFullFieldNames (THIS-OBJECT)).

                    DELETE OBJECT oArrayFactory.

                    ASSIGN lFirstRun = FALSE.
                END.
            END.
        END CASE.

        THIS-OBJECT:TableSchema = oRootTable .

        IF NOT plSilent THEN DO:
            Consultingwerk.Util.DesignerHelper:RaiseComponentChanged (THIS-OBJECT,
                                                                      "TableSchema":U,
                                                                      ?,
                                                                      oRootTable) .

            Consultingwerk.Util.MessageFormHelper:ShowMessage
                        (SmartComponentLibraryCustomizer:SmartBusinessEntityBindingSource_DesignerImportSchemaNewSchemaImported +
                         System.Environment:NewLine +
                         ENTRY(1, THIS-OBJECT:ToString(), "[":U),
                         SmartComponentLibraryCustomizer:SmartBusinessEntityBindingSource_DesignerImportSchema_Title) .
        END.

        CATCH ple AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (ple, SmartComponentLibraryCustomizer:SmartBusinessEntityBindingSource_DesignerImportSchema_Title) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a DataHandle to a DesignTime Dataset. This Dataset is then
                 analysed and the Schema is set as the new TableSchema.
        Notes:   This Method is called by DesignerImportSchema ()
        @param pcEntityName The name of the BusinessEntity
        @param pcEntityTable The RootTable for this Instance of the BusinessEntity
        @param pcEntityView List of Tables to read with the EntityTable
        @param pcEntityJoin List in the same oder as pcEntityView which defines Join YES or NO for each Table of pcEntityView
        @param pcQueryOrDataset Working on a Query or the ProDataset
        @return The handle of the design time dataset
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED HANDLE GetDesignTimeDataHandle (pcEntityName AS CHARACTER,
                                                     pcEntityTable AS CHARACTER,
                                                     pcEntityView AS CHARACTER,
                                                     pcEntityJoin AS CHARACTER,
                                                     pcQueryOrDataset AS CHARACTER):

        DEFINE VARIABLE hDataHandle AS HANDLE NO-UNDO.

        DEFINE VARIABLE oServiceAdapter AS Consultingwerk.OERA.IServiceAdapter NO-UNDO .

        IF pcQueryOrDataset = "QUERY":U THEN DO:
            hDataHandle = Consultingwerk.SmartComponents.Support.SmartBusinessEntityQuerySupport:PrepareBindingQuery (pcEntityName,
                                                                    pcEntityTable,
                                                                    pcEntityView,
                                                                    pcEntityJoin) .
            hDataHandle:QUERY-OPEN () .

            RETURN hDataHandle .
        END.
        ELSE DO ON ERROR UNDO, THROW:
            oServiceAdapter = Consultingwerk.Framework.FrameworkSettings:ServiceAdapter .

            oServiceAdapter:FetchDataset ("":U, pcEntityName,
                                          OUTPUT DATASET-HANDLE hDataHandle BY-REFERENCE) .

            RETURN hDataHandle.

            FINALLY:
                IF BusinessEntityDesignerSupport:SearchBusinessEntititiesOnAppServer AND
                   BusinessEntityDesignerSupport:DisconnectAppServerAfterEachRequest THEN
                    FrameworkSettings:AppServerServiceManager:DisconnectService ("":U) .
            END FINALLY.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the EntityTable, EntityName, EntityView and EntityJoin
                 properties from a SmartBusinessEntityAdapter or SmartDatasetChildAdapter
        Notes:
        @param poAdapter The reference to the SmartBusinessEntityAdapter or SmartDatasetChildAdapter to copy settings form
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeFromAdapter (poAdapter AS SmartDatasetAdapter):

        ASSIGN THIS-OBJECT:EntityName  = poAdapter:EntityName
               THIS-OBJECT:EntityTable = poAdapter:EntityTable
               THIS-OBJECT:EntityView  = poAdapter:EntityView .

        IF TYPE-OF (poAdapter, SmartBusinessEntityAdapter) THEN
            ASSIGN THIS-OBJECT:EntityJoin = CAST (poAdapter, SmartBusinessEntityAdapter):EntityJoin .
        ELSE
            ASSIGN THIS-OBJECT:EntityJoin = TRIM (FILL ("yes,":U,
                                                        NUM-ENTRIES (THIS-OBJECT:EntityView)),
                                                  ",":U) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler method for Designer Verbs
        Notes:   This method is intended to be overridden
        @param pcVerbText The text indicating the verb that was clicked
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID OnVerbClicked (pcVerbText AS CHARACTER):

        IF NOT THIS-OBJECT:DesignTime THEN
            RETURN .

        CASE pcVerbText:
            WHEN "Select Tables":U THEN
                ShowTablePickerDialog() .
            WHEN "Select BusinessEntity":U THEN
                ShowBusinessEntityPickerDialog () .
            WHEN "Reload BusinessEntities":U THEN
                BusinessEntityDesignerSupport:RestartBusinessEntities () .
        END CASE .

        SUPER:OnVerbClicked (INPUT pcVerbText).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Analyse the Dataset recursively and add all Tables and Fields
                 together to a WorkTable representing the new TableSchema.
        Notes:   This Method is called by DesignerImportSchema ().
        @param phDataHandle The Handle of the Dataset which is processed
        @param phBuffer The TopBuffer of the Dataset
        @return The TableDesc with the selected schema
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED TableDesc RecurseDatasetRelations (phDataHandle AS HANDLE,
                                                        phBuffer AS HANDLE):

        DEFINE VARIABLE oWorkTable    AS TableDesc             NO-UNDO.

        DEFINE VARIABLE oArrayFactory AS DatasetRelationParser NO-UNDO.
        DEFINE VARIABLE cExceptList   AS CHARACTER             NO-UNDO.

        DEFINE VARIABLE i             AS INTEGER NO-UNDO.
        DEFINE VARIABLE hRelation     AS HANDLE  NO-UNDO.

        oWorkTable    = NEW TableDesc(phBuffer:NAME).
        oArrayFactory = NEW DatasetRelationParser().
        cExceptList   = "":U.

        oWorkTable:Columns = oArrayFactory:GetTableFieldArray(phBuffer, ?, cExceptList).

        DO i = 1 TO phDataHandle:NUM-RELATIONS:
            hRelation     = phDataHandle:GET-RELATION(i).
            IF hRelation:PARENT-BUFFER = phBuffer AND
               hRelation:ACTIVE        = TRUE AND
               hRelation:RECURSIVE     = FALSE THEN DO:

                /* Add ChildTable */
                oWorkTable:ChildTables = oArrayFactory:PackChildTable(oWorkTable,
                                                                      RecurseDatasetRelations(phDataHandle,
                                                                                              hRelation:CHILD-BUFFER)).
            END.
        END.

        DELETE OBJECT oArrayFactory.

        RETURN oWorkTable.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets properties for the Visual Designer customization
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SetDesignerProperties ():

        THIS-OBJECT:DesignerVerbs = "Reload BusinessEntities,Select BusinessEntity,Select Tables,":U + THIS-OBJECT:DesignerVerbs .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Shows a dialog to select a Business Entity
        Notes:   Invoked by the "Select BusinessEntity" Designer Verb, only intended
                 for DesignTime
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ShowBusinessEntityPickerDialog ():

        DEFINE VARIABLE cEntityName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cOldValue   AS CHARACTER NO-UNDO.

        IF NOT THIS-OBJECT:DesignTime THEN
            RETURN .

        Consultingwerk.SmartComponents.Support.BusinessEntityDesignerSupport:SelectBusinessEntityName (OUTPUT cEntityName).

        IF cEntityName > "":U THEN DO:
            ASSIGN cOldValue              = THIS-OBJECT:EntityName
                   THIS-OBJECT:EntityName = cEntityName .

            Consultingwerk.Util.DesignerHelper:RaiseComponentChanged (THIS-OBJECT,
                                                                      "EntityName":U,
                                                                      BOX (cOldValue),
                                                                      BOX (cEntityName)) .
        END.

        Consultingwerk.Util.DesignerHelper:RefreshPropertyGrid (THIS-OBJECT) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invokes the Table Picker Dialog and sets properties of the
                 SmartBusinessEntityAdapter during Design Time
        Notes:   Invoked by the "Select Tables" Designer Verb. Only intended for
                 DesignTime
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ShowTablePickerDialog ():

        DEFINE VARIABLE hDataset    AS HANDLE    NO-UNDO.
        DEFINE VARIABLE lcSchema    AS LONGCHAR  NO-UNDO.
        DEFINE VARIABLE cTables     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cEntityJoin AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cOldTables  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cOldJoins   AS CHARACTER NO-UNDO.

        DEFINE VARIABLE i           AS INTEGER   NO-UNDO.

        IF NOT THIS-OBJECT:DesignTime THEN
            RETURN .

        IF THIS-OBJECT:BindTo <> "QUERY":U THEN DO:

            MESSAGE "Unable to start table picker dialog when binding to a Dataset."
                VIEW-AS ALERT-BOX WARNING .

            RETURN.
        END.

        hDataset = THIS-OBJECT:GetDesignTimeDataHandle (THIS-OBJECT:EntityName,
                                                        "":U,
                                                        "":U,
                                                        "":U,
                                                        "Dataset":U) .

        IF NOT VALID-HANDLE (hDataset) THEN DO:
            MESSAGE "Unable to start table picker dialog without a valid Dataset."
                VIEW-AS ALERT-BOX WARNING .

            RETURN .
        END.

        ASSIGN cEntityJoin = THIS-OBJECT:EntityJoin .

        RUN Consultingwerk/SmartComponents/Support/SmartBusinessEntityAdapterDesigner_ShowBusinessEntityTblPic.p
                (hDataset,
                 TRIM (SUBSTITUTE ("&1,&2":U, THIS-OBJECT:EntityTable, THIS-OBJECT:EntityView), ",":U),
                 OUTPUT cTables,
                 INPUT-OUTPUT cEntityJoin) .

        IF cTables > "":U THEN DO:
            ASSIGN cOldTables              = THIS-OBJECT:EntityTable
                   cOldJoins               = THIS-OBJECT:EntityJoin
                   THIS-OBJECT:EntityTable = ENTRY (1, cTables)
                   THIS-OBJECT:EntityJoin  = cEntityJoin .

            IF NUM-ENTRIES (cTables) > 1 THEN DO:
                ASSIGN
                    ENTRY (1, cTables)     = "":U .
                    THIS-OBJECT:EntityView = TRIM (cTables, ",":U) .
            END.
            ELSE
                THIS-OBJECT:EntityView = "":U .

            Consultingwerk.Util.DesignerHelper:RaiseComponentChanged (THIS-OBJECT,
                                                                      "EntityTable":U,
                                                                      BOX (cOldTables),
                                                                      BOX (THIS-OBJECT:EntityTable)) .

            Consultingwerk.Util.DesignerHelper:RaiseComponentChanged (THIS-OBJECT,
                                                                      "EntityJoin":U,
                                                                      BOX (cOldJoins),
                                                                      BOX (THIS-OBJECT:EntityView)) .
        END.

        Consultingwerk.Util.DesignerHelper:RefreshPropertyGrid (THIS-OBJECT) .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
        END FINALLY.

    END METHOD.

END CLASS.
