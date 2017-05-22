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
    File        : ModifiedStateDataAccess
    Purpose     : Data Access Component
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 31.12.2016 00:59:18
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*            FROM PROPATH .
USING Consultingwerk.SmartFramework.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.ModifiedStateBusinessEntity", type="DataAccess") .

CLASS Consultingwerk.SmartFramework.System.ModifiedStateDataAccess
    INHERITS SmartDataAccess
    USE-WIDGET-POOL:

    { Consultingwerk/SmartFramework/System/dsModifiedState.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    @DefineDataSourcesStart.
    DEFINE PRIVATE DATA-SOURCE src_SmartModifiedState FOR SmartModifiedState .
    @DefineDataSourcesEnd.

    @DefineDataSourceInformationStart.
    @DefineDataSourceInformationEnd.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ModifiedStateDataAccess class
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.
        @param phDataset The handle of the Business Entity dataset
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ModifiedStateDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).

        BindDataset (DATASET-HANDLE phDataset BIND) .

        /* Fields that are not assigned to the database record when a new record is created. */
        /* THIS-OBJECT:AssignSkipList = "<key fields>":U. */
        THIS-OBJECT:AssignSkipDictionary:Add ("eSmartModifiedState":U, "ModifiedStateGuid":U).

        /* Mike Fechner, Consultingwerk Ltd. 01.07.2014
           SCL-334: By default, prepare queries of joined data source
                    queries. This behavior is the new default, and should
                    only be set to FALSE, when customers are already manipulating
                    the data-source query in code (e.g. QUERY-PREPARE in a BEFORE-FILL
                    call back) */
        THIS-OBJECT:PrepareJoinedChildTableQueries = TRUE .

        /* Allows to force applying the Source Default Query to child tables
           even when no filter expression was provided by the consumer */
        /* THIS-OBJECT:ForceApplyQueryOnChildTables = FALSE . */

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsModifiedState:HANDLE, FALSE) .

        @AttachDataSourcesStart.
        THIS-OBJECT:AttachDataSource (BUFFER eSmartModifiedState:HANDLE,
                                      DATA-SOURCE src_SmartModifiedState:HANDLE, "
                                      ModifiedStateGuid,SmartModifiedState.ModifiedStateGuid,
                                      ModifiedStateTable,SmartModifiedState.ModifiedStateTable,
                                      ModifiedStateKeyFieldValues,SmartModifiedState.ModifiedStateKeyFieldValues,
                                      ModifiedStateTimeStamp,SmartModifiedState.ModifiedStateTimeStamp,
                                      ModifiedStateUser,SmartModifiedState.ModifiedStateUser":U) .
        @AttachDataSourcesEnd.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received
                 Dataset reference
        Notes:   Invoked from the class constructor
        @param dsModifiedState DATASET to bind to
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsModifiedState BIND):
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: TO-DO: Subscribe to ProDataset Event Handlers using SET-CALLBACK
                 as needed
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData, note that the callback methods need to be
                 PUBLIC and need to have the Dataset defined as an INPUT Parameter.
                 Errors thrown from or raised in the FILL call-back methods of the
                 DataAccess class (SET-CALLBACK) will stop the Dataset from getting
                 filled / filled further. However errors thrown from the call-back
                 methods are not thrown from the FILL() method further up into a
                 catch block or to the client.
                 See http://knowledgebase.progress.com/articles/Article/P188375
        Sample:  BUFFER eCustomer:SET-CALLBACK (Consultingwerk.DatasetBufferCallbackEnum:AfterFill,
                                                "eCustomerAfterFill":U) .
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():

    END METHOD.

    /**
     * Purpose: Delete all records
     * Notes:   Throws an exception, when records could not be deleted due to record
     *          locking conflicts
     */
    METHOD PUBLIC VOID DeleteAll():

        DEFINE BUFFER SmartModifiedState FOR SmartModifiedState .
        DEFINE QUERY qQry FOR SmartModifiedState .
        DEFINE VARIABLE roRowid AS ROWID NO-UNDO .

        OPEN QUERY qQry FOR EACH SmartModifiedState EXCLUSIVE-LOCK .
        GET FIRST qQry NO-LOCK .

        DO WHILE NOT QUERY qQry:QUERY-OFF-END ON ERROR UNDO, THROW:
            roRowid = ROWID (SmartModifiedState) .

            GET CURRENT qQry EXCLUSIVE-LOCK NO-WAIT .

            IF LOCKED SmartModifiedState THEN DO:
                UNDO, THROW NEW RecordLockedException ("Unable to delete modified state. The record is locked."{&TRAN},
                                                       0,
                                                       "SmartModifiedState":U,
                                                       ROWID (SmartModifiedState)) .
            END.

            DELETE SmartModifiedState .

            GET NEXT qQry NO-LOCK .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsModifiedState:HANDLE, FALSE) .

        @DetachDataSourcesStart.
        BUFFER eSmartModifiedState:DETACH-DATA-SOURCE () .
        @DetachDataSourcesEnd.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the database field name matching a temp-table field name
                 contained in a consumers query string (query string vs. temp-table
                 definition)
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of
                 DataAccess:FetchData () query preparation
                 TO-DO: Provide code for alternative mapping
        @param pcTable The temp table name to return the source (database table) field name for
        @param pcColumn The temp table column name to return the source (database table) field name for
        @return The database field name mapped to the passed in temp-table field
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceColumn (pcTable AS CHARACTER, pcColumn AS CHARACTER):

        DEFINE VARIABLE cMap  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iPos  AS INTEGER   NO-UNDO .

        @SourceColumnCaseBlock.
        CASE pcTable:
            WHEN "eSmartModifiedState":U THEN
                cMap = DATA-SOURCE src_SmartModifiedState:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .

        END CASE .

        ASSIGN iPos = LOOKUP(pcColumn, cMap) .

        IF iPos MODULO 2 = 1 THEN
            RETURN ENTRY(LOOKUP(pcColumn,cMap) + 1,cMap).
        ELSE
            RETURN "":U .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the base query string used to retrieve data for the temp
                 tables. This query string will be appended by the query provided by
                 the consumer (FetchDataRequest object).
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of
                 DataAccess:FetchData () query preparation
                 TO-DO: Provide code to return the DATA-SOURCE root query string
        @param pcTable The temp table name to return the source default (database) query string for
        @return The base query string used to retrieve data for the given temp-table
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceDefaultQuery (pcTable AS CHARACTER):

        @SourceDefaultQueryCaseBlock.
        CASE pcTable:
            WHEN "eSmartModifiedState":U THEN
                RETURN "FOR EACH SmartModifiedState INDEXED-REPOSITION":U.

        END CASE .

    END METHOD.

END CLASS.
