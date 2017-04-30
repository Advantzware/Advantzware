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
    File        : AttributeValueDataAccess
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 20.10.2015 20:22:29
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING OpenEdge.Lang.* FROM PROPATH.
USING Consultingwerk.*                                 FROM PROPATH .
USING Consultingwerk.SmartFramework.*                  FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .
USING Consultingwerk.OERA.*                            FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity", type="DataAccess") .

CLASS Consultingwerk.SmartFramework.Repository.Class.AttributeValueDataAccess
    INHERITS SmartDataAccess
    USE-WIDGET-POOL:

    { Consultingwerk/SmartFramework/Repository/Class/dsAttributeValue.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    @DefineDataSourcesStart.
    DEFINE PRIVATE DATA-SOURCE src_SmartAttribute FOR SmartAttribute .
    DEFINE PRIVATE DATA-SOURCE src_SmartAttributeValue FOR SmartAttributeValue .
    @DefineDataSourcesEnd.

    @DefineDataSourceInformationStart.
    @DefineDataSourceInformationEnd.

    DEFINE VARIABLE oBufferDataSource AS IBufferDataSource NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AttributeValueDataAccess class
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.
        @param phDataset The handle of the Business Entity dataset
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttributeValueDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).

        BindDataset (DATASET-HANDLE phDataset BIND) .

        /* Fields that are not assigned to the database record when a new record is created. */
        /* Mike Fechner, Consultingwerk Ltd. 24.06.2014
           Do not honor AssignSkipList when importing records during SmartDB Migration */
        IF Consultingwerk.SmartFramework.Tools.Import.GenericDataImporter:Importing = FALSE THEN
            THIS-OBJECT:AssignSkipList = "AttributeValueGuid":U.

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

        DEFINE VARIABLE cMapping AS CHARACTER NO-UNDO.

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsAttributeValue:HANDLE, FALSE) .

        @AttachDataSourcesStart.
        THIS-OBJECT:AttachDataSource (BUFFER eSmartAttribute:HANDLE,
                                      DATA-SOURCE src_SmartAttribute:HANDLE, "
                                      AttributeGuid,SmartAttribute.AttributeGuid,
                                      AttributeGroupGuid,SmartAttribute.AttributeGroupGuid,
                                      AttributeLabel,SmartAttribute.AttributeLabel,
                                      TechnicalName,SmartAttribute.TechnicalName,
                                      AttributeDesription,SmartAttribute.AttributeDesription,
                                      RuntimeOnly,SmartAttribute.RuntimeOnly,
                                      VirtualProperty,SmartAttribute.VirtualProperty,
                                      ConstantLevel,SmartAttribute.ConstantLevel,
                                      LookupType,SmartAttribute.LookupType,
                                      LookupValues,SmartAttribute.LookupValues,
                                      PropertyType,SmartAttribute.PropertyType,
                                      RepositoryType,SmartAttribute.RepositoryType,
                                      SetServiceType,SmartAttribute.SetServiceType,
                                      PropertyOrEvent,SmartAttribute.PropertyOrEvent":U) .
        THIS-OBJECT:AttachDataSource (BUFFER eSmartAttributeValue:HANDLE,
                                      DATA-SOURCE src_SmartAttributeValue:HANDLE, "
                                      AttributeValueGuid,SmartAttributeValue.AttributeValueGuid,
                                      ObjectTypeGuid,SmartAttributeValue.ObjectTypeGuid,
                                      ObjectMasterGuid,SmartAttributeValue.ObjectMasterGuid,
                                      ContainerObjectMasterGuid,SmartAttributeValue.ContainerObjectMasterGuid,
                                      ObjectInstanceGuid,SmartAttributeValue.ObjectInstanceGuid,
                                      ConstantValue,SmartAttributeValue.ConstantValue,
                                      AttributeLabel,SmartAttributeValue.AttributeLabel,
                                      CharacterValue,SmartAttributeValue.CharacterValue,
                                      IntegerValue,SmartAttributeValue.IntegerValue,
                                      Int64Value,SmartAttributeValue.Int64Value,
                                      DateValue,SmartAttributeValue.DateValue,
                                      DateTimeValue,SmartAttributeValue.DateTimeValue,
                                      DateTimeTzValue,SmartAttributeValue.DateTimeTzValue,
                                      DecimalValue,SmartAttributeValue.DecimalValue,
                                      LogicalValue,SmartAttributeValue.LogicalValue,
                                      RawValue,SmartAttributeValue.RawValue,
                                      AppliesAtRuntime,SmartAttributeValue.AppliesAtRuntime":U) .
        @AttachDataSourcesEnd.

        /* Replace Data-Source with dynamic one */

        IF VALID-OBJECT (oBufferDataSource) THEN DO:
            ASSIGN cMapping = BUFFER eSmartAttribute:DATA-SOURCE-COMPLETE-MAP .
            BUFFER eSmartAttribute:DETACH-DATA-SOURCE () .
            THIS-OBJECT:AttachDataSource(BUFFER eSmartAttribute:HANDLE, oBufferDataSource, cMapping) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received
                 Dataset reference
        Notes:   Invoked from the class constructor
        @param dsAttributeValue DATASET to bind to
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsAttributeValue BIND):
        /* NOOP */
    END METHOD.

    /**
     * Purpose: This procedure is called from saveChanges in the DA
     *          to do the actual database commit and any transaction-related
     *          validation logic for a buffer.
     * Notes:
     * @param phBuffer The handle of the buffer to commit
     */
    METHOD OVERRIDE PROTECTED VOID CommitChanges (phBuffer AS HANDLE):

        DEFINE VARIABLE hBeforeBuff  AS HANDLE    NO-UNDO .

        SUPER:CommitChanges(phBuffer) .

        IF phBuffer:BEFORE-BUFFER:ERROR THEN
            RETURN .

        ASSIGN hBeforeBuff = phBuffer:BEFORE-BUFFER .

        IF (hBeforeBuff:ROW-STATE = ROW-MODIFIED OR hBeforeBuff:ROW-STATE = ROW-CREATED) AND
            phBuffer:NAME = "eSmartAttributeValue":U THEN DO:

            phBuffer:FIND-BY-ROWID (hBeforeBuff:AFTER-ROWID) .

            IF phBuffer::ContainerObjectMasterGuid > "":U THEN
                THIS-OBJECT:TrackModifiedState ("SmartObjectMaster":U,
                                                phBuffer::ContainerObjectMasterGuid) .
        END.

    END METHOD .

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
                                                "eCustomerAfterRowFill":U) .
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsAttributeValue:HANDLE, FALSE) .

        @DetachDataSourcesStart.
        BUFFER eSmartAttribute:DETACH-DATA-SOURCE () .
        BUFFER eSmartAttributeValue:DETACH-DATA-SOURCE () .
        @DetachDataSourcesEnd.

    END METHOD.

    /**
     * Purpose: Fetches data from the database
     * Notes:
     * @param poFetchDataRequest The IFetchDataRequest instance with the arguments for this call
     */
    METHOD OVERRIDE PUBLIC VOID FetchData (poFetchDataRequest AS Consultingwerk.OERA.IFetchDataRequest):

        DEFINE VARIABLE oQueryParser     AS QueryParser      NO-UNDO .
        DEFINE VARIABLE oQueryExpression AS IQueryExpression NO-UNDO .
        DEFINE VARIABLE cQueryString     AS CHARACTER        NO-UNDO .

        /* Only try to filter by eSmartAttribute.AttributeGroupName */
        IF ENTRY (1, THIS-OBJECT:ExpandTables (poFetchDataRequest:Tables)) = "eSmartAttribute":U THEN DO:

            cQueryString = ENTRY (1, poFetchDataRequest:Queries, CHR(1)) .

            /* Parse QueryString of the request into a list of query predicates */
            oQueryParser = NEW QueryParser () .
            oQueryExpression = oQueryParser:ParseQueryString (cQueryString) .

            /* Filter by ItemNum ? */                                          /* In the client query, it's ways filtered relatively to the TT's */
            IF VALID-OBJECT (QueryExpression:FindQueryExpressionForBufferField (BufferHelper:ParseFieldName ("eSmartAttribute.AttributeGroupName":U),
                                                                                oQueryExpression))
               OR oQueryParser:ByClause MATCHES "*eSmartAttribute.AttributeGroupName*":U THEN

                ASSIGN oBufferDataSource = NEW BufferDataSource (NEW BufferSpec ("SmartAttribute":U     , QueryJoinEnum:InnerJoin),
                                                                 NEW BufferSpec ("SmartAttributeGroup":U, QueryJoinEnum:InnerJoin)) .
        END.

        SUPER:FetchData (poFetchDataRequest).

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oBufferDataSource) .
        END FINALLY.
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

        IF pcTable = "eSmartAttribute":U AND VALID-OBJECT (oBufferDataSource) THEN
            RETURN oBufferDataSource:SourceColumn(pcColumn) .

        @SourceColumnCaseBlock.
        CASE pcTable:
            WHEN "eSmartAttribute":U THEN
                cMap = DATA-SOURCE src_SmartAttribute:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .
            WHEN "eSmartAttributeValue":U THEN
                cMap = DATA-SOURCE src_SmartAttributeValue:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .

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

        IF pcTable = "eSmartAttribute":U AND VALID-OBJECT (oBufferDataSource) THEN
            RETURN oBufferDataSource:SourceDefaultQuery() .

        @SourceDefaultQueryCaseBlock.
        CASE pcTable:
            WHEN "eSmartAttribute":U THEN
                RETURN "FOR EACH SmartAttribute INDEXED-REPOSITION":U.
            WHEN "eSmartAttributeValue":U THEN
                RETURN "FOR EACH SmartAttributeValue INDEXED-REPOSITION":U.

        END CASE .

    END METHOD.

END CLASS.
