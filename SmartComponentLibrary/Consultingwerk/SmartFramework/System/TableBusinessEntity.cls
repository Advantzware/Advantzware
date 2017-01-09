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
    File        : TableBusinessEntity
    Purpose     : Business Entity for Table
    Syntax      :
    Description :
    Author(s)   : Mike Fechner
    Created     : 27.11.2012 23:44:55
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.SmartFramework.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

@BusinessEntityTable (name="eSmartTable", MandatoryColumns="TableName", DefaultSort="BY eSmartTable.TableName") .

CLASS Consultingwerk.SmartFramework.System.TableBusinessEntity
    INHERITS SmartBusinessEntity:

    { Consultingwerk/SmartFramework/System/dsTable.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the TableBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TableBusinessEntity ():
        SUPER (DATASET dsTable:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.System.TableDataAccess":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of the connected database in the GetDatabaseNamesResult instance
        Notes:
        @param dsTable The dsTable dataset used by this business entity, this parameter is not used here
        @param poGetDatabaseNamesResult The GetDatabaseNamesResult instance to return the data in
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetDatabaseNames (INPUT-OUTPUT DATASET {&DATASET-NAME},
                                         poGetDatabaseNamesResult AS GetDatabaseNamesResult):

        DEFINE VARIABLE oDataAccess AS TableDataAccess NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:IsValid (poGetDatabaseNamesResult) .

        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:InitializeDataAccessObject () .

        ASSIGN oDataAccess = CAST (THIS-OBJECT:DataAccessObject, TableDataAccess) .

        oDataAccess:GetDatabaseNames (poGetDatabaseNamesResult) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Imports the tables from one database
        Notes:
        @param dsTable The dsTable dataset used by this business entity, this parameter is not used here
        @param poImportDatabaseTablesParameter The ImportDatabaseTablesParameter instance with the database name to process
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ImportDatabaseTables (INPUT-OUTPUT DATASET {&DATASET-NAME},
                                             poImportDatabaseTablesParameter AS ImportDatabaseTablesParameter):

        DEFINE VARIABLE oDataAccess AS TableDataAccess NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:IsValid (poImportDatabaseTablesParameter) .

        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:InitializeDataAccessObject () .

        ASSIGN oDataAccess = CAST (THIS-OBJECT:DataAccessObject, TableDataAccess) .

        oDataAccess:ImportDatabaseTables (poImportDatabaseTablesParameter) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and
                 Update operations (i.e. population of aggregated values)
        Notes:   Invoked during FetchData () and SaveChanges ()
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():

        FOR EACH eSmartTable:
            ASSIGN eSmartTable.QualifiedTableName = SUBSTITUTE ("&1.&2":U,
                                                                eSmartTable.DatabaseName,
                                                                eSmartTable.TableName) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook for high level data validation before Update
                 operations
        Notes:   Invoked during SaveChanges (). When the ERROR flag of the ProDataset
                 is set, the Update operation will be cancelled before writing back
                 the data to the database using the DataAccess object
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ValidateData ():

    END METHOD.

END CLASS.
