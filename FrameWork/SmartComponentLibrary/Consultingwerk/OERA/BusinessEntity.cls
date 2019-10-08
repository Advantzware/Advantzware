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
    File        : BusinessEntity
    Purpose     : Provides Access and Business Logik for a ProDataset
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Feb 24 22:22:37 CET 2010
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.*                               FROM PROPATH .
USING Consultingwerk.Assertion.*                     FROM PROPATH .
USING Consultingwerk.Exceptions.*                    FROM PROPATH .
USING Consultingwerk.OERA.*                          FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .
USING Consultingwerk.Util.*                          FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.OERA.BusinessEntity
    IMPLEMENTS IBusinessEntity, IHasContextID, IBusinessService ABSTRACT:

    /**
     * Purpose: A Context ID / Instance ID
     * Notes:   Used to differentiate multiple instances of the same Business Entity
     *          Class
     */
    DEFINE PUBLIC PROPERTY ContextID AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns the name of the data access class used by this business
     *          entity
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DataAccessName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns the reference to the data access object used by this
     *          business entity
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DataAccessObject AS Consultingwerk.OERA.IDataAccess NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns a reference to the dataset owned by this business entity
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Returns the reference to the current IFetchDataRequest instance
     * Notes:   Allows accessing the IFetchDataRequest instance during ProDataset
     *          event call back and during the AttachDataSources method
     */
    DEFINE PROTECTED PROPERTY FetchDataRequest AS IFetchDataRequest NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Returns the type of the current request (FetchData/SaveChanges)
     * Notes:   Can be used in AttachDataSource etc. to adjust data sources for
     *          optimized read or save operations
     */
    DEFINE PUBLIC PROPERTY RequestType AS Consultingwerk.OERA.Enum.RequestTypeEnum NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Gets the reference of the current parameter object passed to the
     *          SaveChanges method
     * Notes:
     */
    DEFINE PROTECTED PROPERTY SaveChangesParameter AS Progress.Lang.Object NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Turns on and off tracking changes for the business entity temp-tables
     * Notes:   When assigning a value to this property, the TRACKING-CHANGES
     *          attribute of all temp-tables will be set. When querying the
     *          property, the value of the TRACKING-CHANGES attribute of the first
     *          updatable temp-table will be returned. When there is not updatable
     *          temp-table, FALSE will be returned
     */
    DEFINE PUBLIC PROPERTY TrackingChanges AS LOGICAL NO-UNDO
    GET:
        RETURN DatasetHelper:GetTrackingChanges (THIS-OBJECT:DatasetHandle) .
    END GET.
    SET (arg AS LOGICAL):
        Assert:NotNull (arg) .

        DatasetHelper:SetTrackingChanges (THIS-OBJECT:DatasetHandle,
                                          arg) .
    END SET.

    /**
     * Purpose: Returns the current IValidateDataContext instance
     * Notes:   Only valid while the ValidateDataContext (Dataset, IValidateDataContext)
     *          method is executed, see SCL-1030
     */
    DEFINE PROTECTED PROPERTY ValidateDataContext AS IValidateDataContext NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Constructor for the SmartBusinessEntity class
     * Notes:
     * @param phDataset The handle of the Business Entity Dataset (passed from the actual Business Entity)
     */
    CONSTRUCTOR PUBLIC BusinessEntity (phDataset AS HANDLE):

        DEFINE VARIABLE cTypeName AS CHARACTER NO-UNDO.

        SUPER ().

        IF NOT VALID-HANDLE (phDataset) OR phDataset:TYPE <> "DATASET":U THEN
            UNDO, THROW NEW AppError ("Invalid Dataset reference in BusinessEntity constructor."{&TRAN}, 0) .

        THIS-OBJECT:DatasetHandle = phDataset .

        /* Mike Fechner, Consultingwerk Ltd. 27.02.2010
           Use Name pattern to build DataAccess type name */

        ASSIGN cTypeName = THIS-OBJECT:GetClass():TypeName .

        IF cTypeName MATCHES "*BusinessEntity":U THEN
            cTypeName = SUBSTRING (cTypeName, 1, LENGTH (cTypeName, "CHARACTER":U) - 14 /*LENGTH ("BusinessEntity":U)*/, "CHARACTER":U) + "DataAccess":U .

        ELSE IF cTypeName MATCHES "*Entity":U THEN
            cTypeName = SUBSTRING (cTypeName, 1, LENGTH (cTypeName, "CHARACTER":U) - 6 /*LENGTH ("Entity":U)*/, "CHARACTER":U) + "DataAccess":U .

        ELSE
            ASSIGN cTypeName = cTypeName + "DataAccess":U .

        THIS-OBJECT:DataAccessName = cTypeName .

    END CONSTRUCTOR.

    /**
     * Purpose: Returns a TRUE value if a record is found that meets the specified
     *          FIND criteria; otherwise it returns FALSE. CAN-FIND does not make
     *          the record available to the Business Entity or it's caller.
     * Notes:   Invokable method
     *          Result will be assigned to the Result property of the CanFindParameter
     *          instance
     * @param phDataset Not used, do not pass in a DATASET or DATASET-HANDLE
     * @param poParameter The CanFindParameter with the parameters for this call
     */
    METHOD PUBLIC VOID CanFind (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                poParameter AS CanFindParameter):

        ObjectAssert:IsValid (poParameter, "poParameter":U) .
        Assert:NotNullOrEmpty (poParameter:FindString, "poParameter:FindString":U) .

        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:InitializeDataAccessObject () .

        CAST (THIS-OBJECT:DataAccessObject, IDataAccessCanFind):CanFind (poParameter) .

        IF VALID-HANDLE (phDataset) AND phDataset:NUM-REFERENCES = 0 THEN
            DELETE OBJECT phDataset .

    END METHOD .

    /**
     * Purpose: Counts the number of results of the query
     * Notes:   Used by the Generic JSDO Service to support the Count feature
     *          Supports STOP-AFTER: When STOP-AFTER occurs, the StopAfterNumResults
     *          value of the parameter object is returns as the Results
     *          http://confluence.consultingwerkcloud.com/wiki/display/SCL/Generic+Service+Interface+for+JSDO+with+Kendo+UI+dialect#GenericServiceInterfaceforJSDOwithKendoUIdialect-ResourceCount
     * @param phDataset The dataset handle (not used)
     * @param poParameter The CharacterHolder to receive the list of TOP-NAV-BUFFERS
     */
    METHOD PUBLIC VOID CountResultRecords (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                           poParameter AS ICountRecordsRequest):

        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:InitializeDataAccessObject () .

        CAST (THIS-OBJECT:DataAccessObject, IDataAccessCountRecords):CountResultRecords (poParameter) .

        @SuppressUnusedWarnings.
        CATCH err AS StopConditionException:
            poParameter:NumResults = poParameter:StopAfterNumResults .
        END CATCH.

        FINALLY:
            IF VALID-HANDLE (phDataset) THEN
                DELETE OBJECT phDataset NO-ERROR .

            ErrorHelper:ResetErrorStatus() .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Unloads the DataAccess object
     * Notes:
     */
    METHOD PROTECTED VOID DestroyDataAccessObject ():

        IF VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            DELETE OBJECT (THIS-OBJECT:DataAccessObject) .

        THIS-OBJECT:DataAccessObject = ? .

    END METHOD.

    /**
     * Purpose: Populates records in the child table identified by the buffer handle
     *          of the child table
     * Notes:   Calls into a second instance of the Business Entity and performs a
     *          FetchDataRequest based on the current record in the parent buffer
     *          Performs no action, when the Child table record for the parent is
     *          already available
     * @param phChildBuffer The child buffer
     */
    METHOD PROTECTED VOID FetchChildTableRecords (phChildBuffer AS HANDLE):

        DEFINE VARIABLE cQuery         AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE oRequest       AS FetchDataRequest NO-UNDO .
        DEFINE VARIABLE cEntityKey     AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE hDataset       AS HANDLE           NO-UNDO .
        DEFINE VARIABLE hDatasetBuffer AS HANDLE           NO-UNDO .
        DEFINE VARIABLE hQuery         AS HANDLE           NO-UNDO .

        BufferAssert:IsDatasetBuffer (phChildBuffer) .

        ASSIGN cQuery     = DatasetHelper:ChildRelationQueryStringWithValues (phChildBuffer) .

        IF Consultingwerk.CharacterType:IsNullOrEmpty (cQuery) THEN
            RETURN .

        ASSIGN cQuery = SUBSTRING (cQuery, INDEX (cQuery, "WHERE":U), -1, "CHARACTER":U) .

        phChildBuffer:FIND-FIRST (cQuery) NO-ERROR .

        IF NOT phChildBuffer:AVAILABLE THEN DO ON ERROR UNDO, THROW:

            ASSIGN oRequest = NEW FetchDataRequest (phChildBuffer:NAME,
                                                    SUBSTITUTE ("for each &1 &2":U,
                                                                phChildBuffer:NAME,
                                                                cQuery))
                   cEntityKey = SUBSTITUTE ("&1&2&3":U,
                                            THIS-OBJECT:GetClass():TypeName,
                                            CHR(1),
                                            GUID) .

            ServiceInterface:FetchData (cEntityKey,
                                        oRequest,
                                        OUTPUT DATASET-HANDLE hDataset) .

            hDatasetBuffer = hDataset:GET-BUFFER-HANDLE (phChildBuffer:NAME) .

            hQuery = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (hDatasetBuffer) .

            DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:
                phChildBuffer:BUFFER-CREATE () .

                phChildBuffer:BUFFER-COPY (hDataset:GET-BUFFER-HANDLE (phChildBuffer:NAME)) .
                phChildBuffer:BUFFER-RELEASE () .

                hQuery:GET-NEXT () .
            END.

            FINALLY:
                IF VALID-HANDLE (hQuery) THEN
                    DELETE OBJECT hQuery .

                IF VALID-HANDLE (hDataset) THEN
                    DELETE OBJECT hDataset .

                ServiceManager:StopBusinessService (cEntityKey) .
            END FINALLY.
        END.

    END METHOD .

    /**
     * Purpose:  Fetch data from the Data Access object
     * Notes:
     * @param poFetchDataRequest The IFetchDataRequest object with the parameters for this call
     */
    METHOD PUBLIC VOID FetchData (poFetchDataRequest AS IFetchDataRequest):

        ASSIGN
            THIS-OBJECT:RequestType      = Consultingwerk.OERA.Enum.RequestTypeEnum:FetchData
            THIS-OBJECT:FetchDataRequest = poFetchDataRequest .

        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:InitializeDataAccessObject () .

        THIS-OBJECT:DataAccessObject:FetchData (poFetchDataRequest) .

        THIS-OBJECT:ReceiveData() .

        /* Mike Fechner, Consultingwerk Ltd. 09.01.2014
           Due to a 4GL issue with the combination of the QryInfo LOG-ENTRY-TYPE
           and queries executed on temp-tables, we are resetting error messages here:
           Issue logged with Progress Tech-Support as: Case: 00262882 / Bug number: PSC00286506
           LOG-ENTRY-TYPE QryInfo causes ERROR-STATUS on FOR EACH <temp-table>

           Update 09-2015: According to Progress tech support - this issue is fixed in
           OpenEdge 11.6 */
        IF ERROR-STATUS:ERROR AND ERROR-STATUS:GET-NUMBER (1) = 565 AND
           ERROR-STATUS:GET-MESSAGE(1) MATCHES "*_IndexStat*":U THEN DO:

            LogManager:WriteFormattedMessage ("*** Ignoring doubious error caused by code in ReceiveData:~n*** BusinessEntity: &1~n&2":U,
                                              THIS-OBJECT:GetClass():TypeName,
                                              ERROR-STATUS:GET-MESSAGE (1)) .

            /* reset error status */
            ERROR-STATUS:ERROR = FALSE NO-ERROR .
        END.

        FINALLY:
            ASSIGN
                THIS-OBJECT:RequestType      = ?
                THIS-OBJECT:FetchDataRequest = ? .
        END FINALLY.

    END METHOD.

    /**
     * Purpose:  Fetch data and/or definitions
     * Notes:
     * @param poFetchDataRequest The IFetchDataRequest object with the parameter for this call
     * @param phDataset OUTPUT Dataset with the resulting data
     */
    METHOD PUBLIC FINAL VOID FetchData (poFetchDataRequest AS IFetchDataRequest,
                                        OUTPUT DATASET-HANDLE phDataset):

        THIS-OBJECT:FetchData (poFetchDataRequest) .

        ASSIGN phDataset = THIS-OBJECT:DatasetHandle .

    END METHOD.

    /**
     * Purpose: Allows retrieve multiple DB records from a Business Entity based on
     *          a temp-table with key values
     * Notes:   Proxy method to calling the same method in the data access class
     *          SCL-647
     * @param phDataset INPUT-OUTPUT DATAEST-HANDLE, only used for OUTPUT of the resulting dataset
     * @param poParameter The IFetchDataByKeyTableParameter with the data for this call
     */
    METHOD PUBLIC VOID FetchDataByKeyTable (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                            poParameter AS IFetchDataByKeyTableParameter):

        IF VALID-HANDLE (phDataset) AND phDataset:DYNAMIC THEN
            DELETE OBJECT phDataset NO-ERROR .

        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:InitializeDataAccessObject () .

        THIS-OBJECT:DataAccessObject:FetchDataByKeyTable (poParameter) .
        THIS-OBJECT:ReceiveData() .

        ASSIGN phDataset = THIS-OBJECT:DatasetHandle .

    END METHOD.

    /**
     * Purpose: Creates and returns an empty dataset to the caller
     * Notes:   Useful for dynamic callers that need access to the dataset schema prior to calling FetchData
     * @param phDataset OUTPUT Dataset with the resulting data
     */
    METHOD PUBLIC VOID FetchDataset (OUTPUT DATASET-HANDLE phDataset):

        CREATE DATASET phDataset .

        phDataset:CREATE-LIKE (THIS-OBJECT:DatasetHandle) .

        DELETE OBJECT phDataset .

    END METHOD.

    /**
     * Purpose: Returns the Business Entity Descriptor for this Business Entity
     * Notes:   Invokable Method
     *          http://confluence.consultingwerkcloud.com/wiki/display/SCL/Business+Entity+Descriptor
     * @param phDataset The dataset of the Business Entity
     * @param poParameter The GetBusinessEntityDescriptorParameter with the data for this call
     */
    METHOD PUBLIC VOID GetBusinessEntityDescriptor (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                                    poParameter AS GetBusinessEntityDescriptorParameter):

        DEFINE VARIABLE oService    AS IBusinessEntityDescriptorService NO-UNDO .
        DEFINE VARIABLE oDescriptor AS IBusinessEntityDescriptor         NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poParameter """poParameter"":U"} .

        oService = {Consultingwerk/get-service.i Consultingwerk.OERA.BusinessEntityDescriptor.IBusinessEntityDescriptorService
                                                 "NEW BusinessEntityDescriptorService()"} .

        oDescriptor = oService:GetBusinessEntityDescriptor (THIS-OBJECT:GetClass():TypeName) .

        poParameter:Descriptor = oDescriptor.

        /* SCL-1664 add GetInitialValues method */
        IF NOT oDescriptor:InvokableMethods:ContainsName ("GetInitialValues":U) THEN
            oDescriptor:InvokableMethods:Add ("GetInitialValues":U,
                                              "Consultingwerk.OERA.GetInitialValuesParameter":U,
                                              FALSE,
                                              TRUE) .

        IF VALID-HANDLE (phDataset) AND phDataset:NUM-REFERENCES = 0 THEN
            DELETE OBJECT phDataset .

        IF poParameter:ReturnDataset THEN DO:
            CREATE DATASET phDataset .

            phDataset:CREATE-LIKE (THIS-OBJECT:DatasetHandle) .

            DELETE OBJECT phDataset .
        END.

    END METHOD .

    /**
     * Purpose: Returns a database with one record containing the initial values
     * Notes:   Can be overridden in Business Entity implementations to provide
     *          dynamic initial values. The base class implementation will return
     *          the buffer fields initial values. The parameter object's TableNames
     *          is a comma-delimited list. The parameter object's ForeignKeyFields
     *          property is a character array (one element per table) of comma delimited
     *          lists of the provided foreign field names. The foreing field values is a
     *          character array (one element per table) of CHR(1) delimited field
     *          values
     * @param phDataset Used to return the dataset
     * @param poParameter The GetInitialValuesParameter instance with the parameters for this call
     */
    METHOD PUBLIC VOID GetInitialValues (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                         poParameter AS GetInitialValuesParameter):

        DEFINE VARIABLE i           AS INTEGER   NO-UNDO .
        DEFINE VARIABLE hBuffer     AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cBufferName AS CHARACTER NO-UNDO .

        IF NOT VALID-HANDLE (phDataset) THEN
            phDataset = THIS-OBJECT:DatasetHandle .

        phDataset:EMPTY-DATASET () .

        ObjectAssert:IsValid (poParameter, "poParameter":U) .

        DO i = 1 TO NUM-ENTRIES (poParameter:TableNames):

            ASSIGN cBufferName = ENTRY (i, poParameter:TableNames) .

            DatasetAssert:HasBuffer (phDataset, cBufferName) .

            ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE (cBufferName) .

            hBuffer:BUFFER-CREATE () .
        END.

    END METHOD.

    /**
     * Purpose: Populates the provided CharacterHolder with a comma-delimited list
     *          of the names of the dataset TOP-NAV-BUFFER's
     * Notes:
     * @param phDataset The dataset handle (not used)
     * @param poParameter The CharacterHolder to receive the list of TOP-NAV-BUFFERS
     */
    METHOD PUBLIC VOID GetTopNavBuffers (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                         poParameter AS CharacterHolder):

        DEFINE VARIABLE i       AS INTEGER   NO-UNDO .
        DEFINE VARIABLE hBuffer AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cNames  AS CHARACTER NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poParameter """poParameter"":U"} .

        DO i = 1 TO THIS-OBJECT:DatasetHandle:NUM-TOP-BUFFERS:
            ASSIGN hBuffer = THIS-OBJECT:DatasetHandle:GET-TOP-BUFFER (i)
                   cNames  = cNames + hBuffer:NAME + ",":U .
        END.

        ASSIGN poParameter:Value = TRIM (cNames, ",":U) .

        FINALLY:
            IF VALID-HANDLE (phDataset) THEN
                DELETE OBJECT phDataset NO-ERROR .

            ErrorHelper:ResetErrorStatus() .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Initializes the DataAcess Object
     * Notes:   Uses the DataAccessName property
     */
    METHOD PROTECTED VOID InitializeDataAccessObject ():

        DEFINE VARIABLE oFactory AS IDataAccessFactory NO-UNDO .

        IF VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:DestroyDataAccessObject () .

        IF NOT VALID-HANDLE (THIS-OBJECT:DatasetHandle) THEN
            UNDO, THROW NEW AppError ("DatasetHandle needs to be set prior to calling InitializeDataAccessObject"{&TRAN}, 0) .

        oFactory = {Consultingwerk/get-service.i Consultingwerk.OERA.IDataAccessFactory
                                                 "NEW Consultingwerk.OERA.DataAccessFactory()"} .

        THIS-OBJECT:DataAccessObject = oFactory:GetDataAccessObject (THIS-OBJECT:DataAccessName,
                                                                     THIS-OBJECT:DatasetHandle).

    END METHOD.

    /**
     * Purpose: Populates a single ProDataset record from the partial record to the complete
     *          record
     * Notes:   Used during SavePartialChanges and ValidatePartialData before SaveChanges
     *          (for a single record) or ValidateData are called
     * @param phPartialBuffer The buffer of the partial dataset to use as the source
     * @return Logical value indicating if the operation was successful
     */
    METHOD PROTECTED LOGICAL PopulateFromPartialBuffer (phPartialBuffer AS HANDLE):

        DEFINE VARIABLE cFindString AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hBuffer     AS HANDLE    NO-UNDO .

        ASSIGN hBuffer     = THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (phPartialBuffer:NAME) .

        /* Don't process unmodified records */
        CASE phPartialBuffer:BEFORE-BUFFER:ROW-STATE:

            WHEN ROW-CREATED THEN DO:
                THIS-OBJECT:DatasetHandle:EMPTY-DATASET () .

                /* Start updating or deleting the current row */
                THIS-OBJECT:TrackingChanges = TRUE .

                hBuffer:BUFFER-CREATE () .
                BufferHelper:FindAfterBuffer (phPartialBuffer:BEFORE-BUFFER) .
                hBuffer:BUFFER-COPY (phPartialBuffer) .
            END. /* row-created */

            WHEN ROW-MODIFIED OR WHEN ROW-DELETED THEN DO:

                /* Find based on partial buffer's before values */
                ASSIGN cFindString = THIS-OBJECT:UniqueFindPredicateWithValueBuffer (hBuffer,
                                                                                     phPartialBuffer,
                                                                                     hBuffer:NAME) .

                Assert:NotNullOrEmpty (cFindString, "cFindString":U) .

                THIS-OBJECT:FetchData (NEW FetchDataRequest (hBuffer:NAME,
                                                             SUBSTITUTE ("for each &1 &2":U,
                                                                         hBuffer:NAME,
                                                                         cFindString),
                                                             0)) .
                hBuffer:FIND-FIRST () NO-ERROR .

                IF hBuffer:AVAILABLE = FALSE THEN DO:
                    DatasetHelper:AddErrorString (hBuffer, ERROR-STATUS:GET-MESSAGE (1)) .
                    ErrorHelper:ResetErrorStatus() .

                    RETURN FALSE .
                END.

                /* Update the before buffer */
                hBuffer:BUFFER-COPY (phPartialBuffer:BEFORE-BUFFER) .

                /* Start updating or deleting the current row */
                THIS-OBJECT:TrackingChanges = TRUE .

                IF phPartialBuffer:BEFORE-BUFFER:ROW-STATE = ROW-DELETED THEN

                    hBuffer:BUFFER-DELETE () .

                ELSE DO:
                    BufferHelper:FindAfterBuffer (phPartialBuffer:BEFORE-BUFFER) .
                    hBuffer:BUFFER-COPY (phPartialBuffer) .
                END. /* else do --> row-modified */

            END. /* row-modified or row-deleted */

            OTHERWISE /* ROW-UNMODIFIED */
                RETURN FALSE .

        END CASE .

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Populates a single ProDataset record from the partial record to the complete
     *          record
     * Notes:   Used during SavePartialChanges and ValidatePartialData before SaveChanges
     *          (for a single record) or ValidateData are called
     * @param phPartialBuffer The buffer of the partial dataset to use as the source
     */
    METHOD PROTECTED VOID PopulatePartialBuffer (phPartialBuffer AS HANDLE):

        DEFINE VARIABLE hBuffer     AS HANDLE    NO-UNDO .

        ASSIGN hBuffer = THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (phPartialBuffer:NAME) .

        hBuffer:BEFORE-BUFFER:FIND-FIRST () .

        IF THIS-OBJECT:DatasetHandle:ERROR OR hBuffer:BEFORE-BUFFER:ERROR OR hBuffer:BEFORE-BUFFER:ERROR-STRING > "":U THEN
            ASSIGN phPartialBuffer:ERROR-STRING  = hBuffer:BEFORE-BUFFER:ERROR-STRING
                   phPartialBuffer:ERROR         = TRUE
                   phPartialBuffer:DATASET:ERROR = TRUE .

        /* Update partial buffer */
        IF NOT hBuffer:BEFORE-BUFFER:ROW-STATE = ROW-DELETED THEN DO:
            BufferHelper:FindAfterBuffer (hBuffer:BEFORE-BUFFER) NO-ERROR .
            phPartialBuffer:BUFFER-COPY (hBuffer) .
        END.

    END METHOD .

    /**
     * Purpose: Provides a hook to modify data in the ProDataset after Read and
     *          Update operations (i.e. population of aggregated values)
     * Notes:   Invoked during FetchData () and SaveChanges ()
     *          Abstract method to be implemented by the actual Business Entity
     */
    METHOD PUBLIC ABSTRACT VOID ReceiveData ().

    /**
     * Purpose: Saves changes using the DataAccess object
     * Notes:   This method saves changes contained in the dataset currently present
     *          in the Business Entity
     */
    METHOD PUBLIC VOID SaveChanges ():

        THIS-OBJECT:RequestType = Consultingwerk.OERA.Enum.RequestTypeEnum:SaveChanges .

        THIS-OBJECT:ValidateData() .

        IF NOT THIS-OBJECT:DatasetHandle:ERROR THEN DO:

            IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
                THIS-OBJECT:InitializeDataAccessObject () .

            THIS-OBJECT:DataAccessObject:SaveChanges () .

            THIS-OBJECT:ReceiveData() .
        END.

        FINALLY:
            THIS-OBJECT:RequestType = ? .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Saves changes
     * Notes:   This method is called from consumers of the Business Entity and
     *          receives the dataset. It calls into the SaveChanges() method with no
     *          parameters after the internal dataset has been set
     *          This method is called by the ServiceInterface when no parameter object
     *          has been passed in
     * @param phDataset INPUT-OUTPUT Dataset with changes
     */
    METHOD PUBLIC FINAL VOID SaveChanges (INPUT-OUTPUT DATASET-HANDLE phDataset) .

        DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.

        ASSIGN hDataset                         = THIS-OBJECT:DatasetHandle
               THIS-OBJECT:SaveChangesParameter = ? .

        hDataset:COPY-DATASET (phDataset) .

        THIS-OBJECT:SaveChanges () .

        phDataset:COPY-DATASET (hDataset) .

        phDataset:ERROR = hDataset:ERROR  .

    END METHOD.

    /**
     * Purpose: Saves changes
     * Notes:   This method is called from consumers of the Business Entity and
     *          receives the dataset. It calls into the SaveChanges() method with no
     *          parameters after the internal dataset has been set
     *          This method is called by the ServiceInterface when a parameter object
     *          has been passed in. The parameter object will be accessible from within
     *          the business entity using the protected property SaveChangesParameter
     * @param phDataset INPUT-OUTPUT Dataset with changes
     * @param poParameter Optional Parameter object for the SaveChanges method
     */
    METHOD PUBLIC FINAL VOID SaveChanges (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                          poParameter AS Progress.Lang.Object) .

        DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.

        ASSIGN hDataset                         = THIS-OBJECT:DatasetHandle
               THIS-OBJECT:SaveChangesParameter = poParameter.

        hDataset:COPY-DATASET (phDataset) .

        THIS-OBJECT:SaveChanges () .

        phDataset:COPY-DATASET (hDataset) .

        phDataset:ERROR = hDataset:ERROR  .

        FINALLY:
            ASSIGN THIS-OBJECT:SaveChangesParameter = ? .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Saves partial changes to the Busiuness Entity Dataset
     * Notes:   Saves only those fields from the provided Dataset. Does fetch the
     *          complete dataset first, then applies changes from the provided
     *          dataset to the complete dataset and saves this using SaveChanges.
     *          Relies on ROW-STATE in the partial dataset as well! A seperate method
     *          may be added in the future, that only expects current values from the
     *          passed in ProDataset
     *          When multiple records are provided in the partial dataset, currently
     *          each record is processed alone
     *          For simplicity of the partial dataset, we do not expect this dataset to
     *          have indexes defined. However, we do expect that the partial dataset
     *          contains the columns of the primary unique key of the business entity
     *          dataset columns
     *          Flow of actions:
     *          - Fetch the whole Dataset Records
     *          - Manipulate the "before"
     *          - Manipulate the "after"
     *          - SaveChanges()
     *          - Transfer ERROR, ERROR-STRING attributes
     *          - Return updated partial records to the caller
     *          SCL-987
     * @param phDataset INPUT-OUTPUT Dataset with changes
     * @param poParameter Optional Parameter object for the SavePartialChanges method (BusinessEntity:SaveChangesParameter)
     */
    METHOD PUBLIC VOID SavePartialChanges (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                           poParameter AS Progress.Lang.Object):

        DEFINE VARIABLE iPartialBuffer AS INTEGER   NO-UNDO .
        DEFINE VARIABLE hPartialBuffer AS HANDLE    NO-UNDO .
        DEFINE VARIABLE hQuery         AS HANDLE    NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phDataset WidgetTypeEnum:Dataset} .

        DatasetHelper:SetTrackingChanges (phDataset, FALSE) .

        ASSIGN THIS-OBJECT:SaveChangesParameter = poParameter.

        /* Loop through the buffers of the partial dataset */
        DO iPartialBuffer = 1 TO phDataset:NUM-BUFFERS ON ERROR UNDO, THROW:

            ASSIGN hPartialBuffer = phDataset:GET-BUFFER-HANDLE (iPartialBuffer) .

            IF NOT VALID-HANDLE (hPartialBuffer:BEFORE-BUFFER) THEN
                NEXT .

            /* We need to ensure that the Business Entity Dataset has a buffer with this name */
            DatasetAssert:HasBuffer (THIS-OBJECT:DatasetHandle, hPartialBuffer:NAME) .

            /* Now loop through the records in the partial buffer's before table */
            hQuery = QueryHelper:CreatePreparedQuery (hPartialBuffer:BEFORE-BUFFER) .

            queryloop: DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:

                THIS-OBJECT:DatasetHandle:EMPTY-DATASET () .

                IF THIS-OBJECT:PopulateFromPartialBuffer (hPartialBuffer) = FALSE THEN DO:
                    hQuery:GET-NEXT () .
                    NEXT queryloop .
                END.

                THIS-OBJECT:SaveChanges () .

                THIS-OBJECT:PopulatePartialBuffer (hPartialBuffer) .

                hQuery:GET-NEXT () .
                NEXT queryloop .

                FINALLY:
                    THIS-OBJECT:TrackingChanges = FALSE .
                END FINALLY.
            END.

            FINALLY:
                GarbageCollectorHelper:DeleteObject (hQuery) .
            END FINALLY.
        END.

        FINALLY:
            ASSIGN THIS-OBJECT:SaveChangesParameter = ? .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Builds a unique find predicate for a prodataset buffer (based on an
     *          unique index).
     * Notes:   Based on BufferHelper:UniqueFindPredicateWithValueBuffer. Called from
     *          SavePartialChanges. Extracted from SavePartialChanges to allow injecting
     *          missing key field values (e.g. tenant related fields) by overriding
     *          this method.
     * @param phBuffer The Business Entity ProDataset member buffer
     * @param phPartialBuffer The buffer of the partial dataset, values will be used from it's before-buffer
     * @param pcBufferName The name of the buffer for the find string
     * @return The unique find predicate used to locate the ProDataset buffer for the partial update
     */
    METHOD PROTECTED CHARACTER UniqueFindPredicateWithValueBuffer (phBuffer AS HANDLE,
                                                                   phPartialBuffer AS HANDLE,
                                                                   pcBufferName AS CHARACTER):

        RETURN BufferHelper:UniqueFindPredicateWithValueBuffer (phBuffer,
                                                                phPartialBuffer:BEFORE-BUFFER,
                                                                ?,
                                                                pcBufferName) .

    END METHOD .

    /**
     * Purpose: Provides a hook for high level data validation before Update
     *          operations
     * Notes:   Invoked during SaveChanges (). When the ERROR flag of the ProDataset
     *          is set, the Update operation will be cancelled before writing back
     *          the data to the database using the DataAccess object
     *          Abstract method to be implemented by the actual Business Entity
     */
    METHOD PUBLIC ABSTRACT VOID ValidateData ().

    /**
     * Purpose: Provides a client callable method for data validation
     * Notes:   Invokes ValidateData(), but not linked to any SaveChanges invokation
     *          The poContext parameter is available during the method execution through
     *          the ValidateDataContext property
     *          See SCL-1030
     * @param phDataset The DATASET-HANDLE with the data to validate
     * @param poContext The IValidateDataContext instance with the context for this call
     */
    METHOD PUBLIC VOID ValidateData (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                     poContext AS IValidateDataContext):

        THIS-OBJECT:ValidateDataContext = poContext .

        HandleAssert:ValidHandle (phDataset, "phDataset":U) .

        THIS-OBJECT:DatasetHandle:EMPTY-DATASET () .

        THIS-OBJECT:DatasetHandle:COPY-DATASET (phDataset, FALSE, FALSE, FALSE, ?, FALSE) .

        THIS-OBJECT:ValidateData () .

        phDataset:EMPTY-DATASET () .

        phDataset:COPY-DATASET (THIS-OBJECT:DatasetHandle, FALSE, FALSE, FALSE, ?, FALSE) .

        phDataset:ERROR = THIS-OBJECT:DatasetHandle:ERROR .

        THIS-OBJECT:DatasetHandle:EMPTY-DATASET () .

        FINALLY:
            IF VALID-HANDLE (phDataset) AND phDataset:NUM-REFERENCES = 0 THEN
                DELETE OBJECT phDataset .

            THIS-OBJECT:ValidateDataContext = ?  .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Provides a client callable method for partial data validation
     * Notes:   Invokes ValidateData(), but not linked to any SaveChanges invokation
     *          The poContext parameter is available during the method execution through
     *          the ValidateDataContext property
     *          See SCL-1030, SCL-1034
     * @param phDataset The DATASET-HANDLE with the data to validate
     * @param poContext The IValidateDataContext instance with the context for this call
     */
    METHOD PUBLIC VOID ValidatePartialData (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                            poContext AS IValidateDataContext):

        DEFINE VARIABLE iPartialBuffer AS INTEGER   NO-UNDO .
        DEFINE VARIABLE hPartialBuffer AS HANDLE    NO-UNDO .
        DEFINE VARIABLE hQuery         AS HANDLE    NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phDataset WidgetTypeEnum:Dataset} .

        DatasetHelper:SetTrackingChanges (phDataset, FALSE) .

        THIS-OBJECT:ValidateDataContext = poContext .

        /* Loop through the buffers of the partial dataset */
        DO iPartialBuffer = 1 TO phDataset:NUM-BUFFERS ON ERROR UNDO, THROW:

            ASSIGN hPartialBuffer = phDataset:GET-BUFFER-HANDLE (iPartialBuffer) .

            IF NOT VALID-HANDLE (hPartialBuffer:BEFORE-BUFFER) THEN
                NEXT .

            /* We need to ensure that the Business Entity Dataset has a buffer with this name */
            DatasetAssert:HasBuffer (THIS-OBJECT:DatasetHandle, hPartialBuffer:NAME) .

            /* Now loop through the records in the partial buffer's before table */
            hQuery = QueryHelper:CreatePreparedQuery (hPartialBuffer:BEFORE-BUFFER) .

            queryloop: DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:

                THIS-OBJECT:DatasetHandle:EMPTY-DATASET () .

                IF THIS-OBJECT:PopulateFromPartialBuffer (hPartialBuffer) = FALSE THEN DO:
                    hQuery:GET-NEXT () .
                    NEXT queryloop .
                END.

                THIS-OBJECT:ValidateData () .

                THIS-OBJECT:PopulatePartialBuffer (hPartialBuffer) .

                hQuery:GET-NEXT () .
                NEXT queryloop .

                FINALLY:
                    THIS-OBJECT:TrackingChanges = FALSE .
                END FINALLY.
            END.

            FINALLY:
                GarbageCollectorHelper:DeleteObject (hQuery) .
            END FINALLY.
        END.

        FINALLY:
            THIS-OBJECT:ValidateDataContext = ?  .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Destructor for the BusinessEntity class
     * Notes:
     */
    DESTRUCTOR PUBLIC BusinessEntity ():

        IF VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:DestroyDataAccessObject () .

    END DESTRUCTOR.

END CLASS.
