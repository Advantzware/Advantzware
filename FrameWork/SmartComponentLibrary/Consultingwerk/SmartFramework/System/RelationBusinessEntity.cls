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
    File        : RelationBusinessEntity
    Purpose     : Business Entity for Relation
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.04.2013 21:42:53
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.*              FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*       FROM PROPATH .
USING Consultingwerk.SmartFramework.Tools.Import.* FROM PROPATH.
USING Consultingwerk.OERA.*                        FROM PROPATH .
USING Consultingwerk.Util.*                        FROM PROPATH .
USING Progress.Lang.*                              FROM PROPATH .

@BusinessEntityTable (name="eSmartRelation", MandatoryColumns="RelationName", DefaultSort="BY eSmartRelation.RelationName") .

CLASS Consultingwerk.SmartFramework.System.RelationBusinessEntity
    INHERITS SmartBusinessEntity:

    { Consultingwerk/SmartFramework/System/dsRelation.i }
    { Consultingwerk/SmartFramework/System/dsMissingRelations.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RelationBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RelationBusinessEntity ():
        SUPER (DATASET dsRelation:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.System.RelationDataAccess":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and
                 Update operations (i.e. population of aggregated values)
        Notes:   Invoked during FetchData () and SaveChanges ()
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook for high level data validation before Update
                 operations
        Notes:   Invoked during SaveChanges (). When the ERROR flag of the ProDataset
                 is set, the Update operation will be cancelled before writing back
                 the data to the database using the DataAccess object
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ValidateData ():

        /* Mike Fechner, Consultingwerk Ltd. 09.05.2016
           Ignore validation while importing data */
        IF GenericDataImporter:Importing THEN
            RETURN .

        FOR EACH eSmartRelation ON ERROR UNDO, THROW:

            Consultingwerk.OERA.Validate:IsInList (BUFFER eSmartRelation:HANDLE,
                                                   "ParentCardinality":U,
                                                   "1,01,01n,1n":U,
                                                   MessageFormatter:GetMessage ("VALMSG":U, 2, "Parent Cardinality":U, eSmartRelation.ParentCardinality)) .

            Consultingwerk.OERA.Validate:IsInList (BUFFER eSmartRelation:HANDLE,
                                                   "ChildCardinality":U,
                                                   "01n,1,01,1n":U,
                                                   MessageFormatter:GetMessage ("VALMSG":U, 2, "Child Cardinality":U, eSmartRelation.ChildCardinality)) .

        END .

    END METHOD.

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.System.GetMissingDatabaseRelationsParameter", datasetInput="false", datasetOutput="true") .
    /*------------------------------------------------------------------------------
        Purpose: Creates missing SmartRelation records
        Notes:
        @param dsMissingRelations INPUT-OUTPUT DATASET
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateMissingDatabaseRelations (INPUT-OUTPUT DATASET dsMissingRelations,
                                                       poParameter AS Consultingwerk.SmartFramework.System.GetMissingDatabaseRelationsParameter):

        DEFINE VARIABLE oParentTable AS TableDatasetModel NO-UNDO .
        DEFINE VARIABLE oChildTable  AS TableDatasetModel NO-UNDO .
        DEFINE VARIABLE cFieldPairs  AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE i            AS INTEGER           NO-UNDO .

        THIS-OBJECT:TrackingChanges = FALSE .
        DATASET dsRelation:EMPTY-DATASET () .
        THIS-OBJECT:TrackingChanges = TRUE .

        ASSIGN oParentTable = NEW TableDatasetModel ()
               oChildTable  = NEW TableDatasetModel ().

        relationLoop:
        FOR EACH ttRelation BREAK BY ttRelation.ParentTable
            ON ERROR UNDO, THROW:

            IF FIRST-OF (ttRelation.ParentTable) THEN DO:
                oParentTable:SmartTable:Filter:Reset() .
                oParentTable:SmartTable:Filter:DatabaseName:EQ (poParameter:DatabaseName) .
                oParentTable:SmartTable:Filter:TableName:EQ (ttRelation.ParentTable):Run () .
                oParentTable:SmartTable:GetFirst() .
            END.

            oChildTable:SmartTable:Filter:Reset() .
            oChildTable:SmartTable:Filter:DatabaseName:EQ (poParameter:DatabaseName) .
            oChildTable:SmartTable:Filter:TableName:EQ (ttRelation.ChildTable):Run () .
            oChildTable:SmartTable:GetFirst() .

            IF NOT oParentTable:SmartTable:Available OR NOT oChildTable:SmartTable:Available THEN
                NEXT relationLoop .

            ASSIGN cFieldPairs = "":U .

            DO i = 1 TO NUM-ENTRIES (ttRelation.IndexFields):
                ASSIGN cFieldPairs = cFieldPairs +
                                     ENTRY (i, ttRelation.IndexFields) + ",":U +
                                     ENTRY (i, ttRelation.IndexFields) + ",":U .
            END.

            CREATE eSmartRelation .
            ASSIGN eSmartRelation.RelationGuid          = ?
                   eSmartRelation.ParentTableGUID       = oParentTable:SmartTable:TableGUID
                   eSmartRelation.ChildTableGUID        = oChildTable:SmartTable:TableGUID
                   eSmartRelation.RelationName          = SUBSTITUTE ("&1 of &2":U, ttRelation.ParentTable, ttRelation.ChildTable)
                   eSmartRelation.RelationDescription   = "":U
                   eSmartRelation.RelationFieldPair     = TRIM (cFieldPairs, ",":U)
                   eSmartRelation.ParentRoleDescription = ttRelation.ParentTable
                   eSmartRelation.ChildRoleDescription  = ttRelation.ChildTable
                   eSmartRelation.ParentCardinality     = "01":U
                   eSmartRelation.ChildCardinality      = "01n":U
                   eSmartRelation.OnParentDelete        = "Restrict":U
                   eSmartRelation.DefaultParentRelation = FALSE
                   eSmartRelation.Inactive              = FALSE .
        END.

        THIS-OBJECT:SaveChanges () .

        DatasetHelper:ThrowDatasetErrors (DATASET dsRelation:HANDLE) .

        FINALLY:
            THIS-OBJECT:TrackingChanges = FALSE .
        END FINALLY.

    END METHOD .

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.System.GetMissingDatabaseRelationsParameter", datasetInput="false", datasetOutput="true") .
    /*------------------------------------------------------------------------------
        Purpose: Returns a list of missing Database Relations to the caller
        Notes:
        @param dsMissingRelations INPUT-OUTPUT DATASET
        @param poParameter The Parameter Object for this method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetMissingDatabaseRelations (INPUT-OUTPUT DATASET dsMissingRelations,
                                                    poParameter AS Consultingwerk.SmartFramework.System.GetMissingDatabaseRelationsParameter):

        DEFINE VARIABLE oDataAccess AS IRelationDataAccess NO-UNDO .

        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:InitializeDataAccessObject () .

        ASSIGN oDataAccess = CAST (THIS-OBJECT:DataAccessObject, IRelationdataAccess) .

        DATASET dsMissingRelations:EMPTY-DATASET () .

        oDataAccess:GetMissingDatabaseRelations (INPUT-OUTPUT DATASET dsMissingRelations BY-REFERENCE,
                                                 poParameter) .

    END METHOD .

END CLASS.
