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
    File        : DataAccess
    Purpose     : Abstract base class for OERA Data Access objects
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Feb 24 22:19:03 CET 2010
    Notes       : SCL-1480: Now supports the SaveRowChanges custom log entry
                  type to log details about "current row modified by another
                  user" issues
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                 FROM PROPATH .
USING Consultingwerk.Assertion.*       FROM PROPATH .
USING Consultingwerk.Exceptions.*      FROM PROPATH .
USING Consultingwerk.Framework.*       FROM PROPATH .
USING Consultingwerk.OERA.*            FROM PROPATH .
USING Consultingwerk.OERA.Enum.*       FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .
USING Consultingwerk.OERA.Query.*      FROM PROPATH .
USING Consultingwerk.OERA.Resources.*  FROM PROPATH .
&IF DEFINED (SmartFramework) NE 0 &THEN
USING Consultingwerk.SmartFramework.*  FROM PROPATH .
&ENDIF
USING Consultingwerk.Util.*            FROM PROPATH .
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.OERA.DataAccess
    IMPLEMENTS IDataAccess, IDSQueryStringSource, IDataAccessCountRecords,
               IDataAccessCanFind ABSTRACT:

    DEFINE VARIABLE oDataSourceQueryInfoProvider  AS IDataSourceQueryInfoProvider  NO-UNDO .
    DEFINE VARIABLE lInFetchDataByKeyTable        AS LOGICAL                       NO-UNDO INITIAL FALSE .
    DEFINE VARIABLE oFetchDataByKeyTableParameter AS IFetchDataByKeyTableParameter NO-UNDO .
    DEFINE VARIABLE cTempFieldPairs               AS CHARACTER                     NO-UNDO .

    &IF DEFINED (AblReflection) NE 0 &THEN
    DEFINE VARIABLE cTransactionMethodNames AS CHARACTER NO-UNDO.
    &ENDIF

    /**
     * Purpose: Event raised after the execution of the SAVE-ROW-CHANGES method
     *          for an individul record in the DataAccessObject
     * Notes:
     * @param sender The reference to the object that raised this event
     * @param e The SaveRowChangesEventArgs with the data for this event
     */
    DEFINE PUBLIC EVENT AfterSavingRowChanges SIGNATURE VOID
         (sender AS Progress.Lang.Object,
          e AS SaveRowChangesEventArgs).

    /**
     * Purpose: Raised at the end of SaveChanges - within the database transaction
     *          block
     * Notes:
     * @param sender The object that raised the AfterSaveChanges event
     * @param e The Consultingwerk.EventArgs with the data for the event
     */
    DEFINE PUBLIC EVENT AfterSaveChanges SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                         e AS Consultingwerk.EventArgs).

    /**
     * Purpose: Raised at the beginning of SaveChanges - within the database transaction
     *          block
     * Notes:   This is not a cancelable event - as we have already started the database
     *          transaction and really only errors should revert it at this point
     * @param sender The object that raised the BeforeSaveChanges event
     * @param e The Consultingwerk.EventArgs with the data for the event
     */
    DEFINE PUBLIC EVENT BeforeSaveChanges SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                          e AS Consultingwerk.EventArgs).

    /**
     * Purpose: Event raised before the execution of the SAVE-ROW-CHANGES method
     *          for an individul record in the DataAccessObject
     * Notes:
     * @param sender The reference to the object that raised this event
     * @param e The SaveRowChangesEventArgs with the data for this event
     */
    DEFINE PUBLIC EVENT BeforeSavingRowChanges SIGNATURE VOID
         (sender AS Progress.Lang.Object,
          e AS SaveRowChangesEventArgs).

    /**
     * Purpose: Raised to allow custom handling of AppError's (Application errors,
     *          e.g. from DB triggers) raised while performing the SaveRowChanges method
     * Notes:
     * @param sender The object that raised the HandleCommitError event
     * @param e The HandleCommitErrorEventArgs with the data for the event
     */
    DEFINE PUBLIC EVENT HandleCommitError SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                          e AS HandleCommitErrorEventArgs).

    /**
     * Purpose: Raised to allow custom handling of SysError's (Progress runtime errors)
     *          raised while performing the SaveRowChanges method
     * Notes:
     * @param sender The object that raised the HandleCommitError event
     * @param e The HandleCommitErrorEventArgs with the data for the event
     */
    DEFINE PUBLIC EVENT HandleCommitSysError SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                             e AS HandleCommitErrorEventArgs).

    /**
     * Purpose: Gets and sets if errors (negative return-value) during the ProDataset
     *          FILL() method should cause an error thrown by the DataAccess class
     * Notes:   The default behaviour is not to throw errors to be consistent with
     *          the previous behaviour and the default behaviour of ProDatasets,
     *          see: http://knowledgebase.progress.com/articles/Article/P188375
     */
    DEFINE PROTECTED PROPERTY ThrowErrorsOnFillFailed AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets the reference to a CharacterDictionary with an entry per buffer
     *          (after-buffer name) as the key and the the skip-list of the SAVE-ROW-CHANGES
     *          method used as the value
     * Notes:   Entries in this Dictionary have preference over the AssignSkipList character list
     *          skip-list
     *          An optional character expression that evaluates to a comma-separated
     *          list of field names for fields that should not be assigned after a
     *          new row is created (that is, fields to skip). For example, a key
     *          field or other fields assigned a value by a CREATE database trigger.
     */
    DEFINE PROTECTED PROPERTY AssignSkipDictionary AS Consultingwerk.Framework.Collections.CharacterDictionary NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Gets and sets the skip-list of the SAVE-ROW-CHANGES method used as
     *          the skip-list of the SAVE-ROW-CHANGES method used in CommitChanges
     *          method
     * Notes:   This property is depricated now. Please use the AssignSkipDictionary
     *          class now.
     */
    DEFINE PUBLIC PROPERTY AssignSkipList AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /**
     *  Purpose: Gets the custom log file entry used by the DataAccess class
     *  Notes:
     */
    DEFINE PUBLIC STATIC PROPERTY CUSTOM_LOG_ENTRY AS CHARACTER INITIAL "DataAccess":U NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets the reference to the Dataset handle used by this Data Access
     *          object
     * Notes:   The Data Access object will typically be working on the ProDataset
     *          instance of a Business Entity (owner of the ProDataset)
     */
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Returns if the DataSources are currently attached
     * Notes:
     */
    DEFINE PROTECTED PROPERTY DataSourcesAttached AS LOGICAL NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns the reference to the current IFetchDataRequest instance
     * Notes:   Allows accessing the IFetchDataRequest instance during ProDataset
     *          event call back and during the AttachDataSources method
     */
    DEFINE PROTECTED PROPERTY FetchDataRequest AS IFetchDataRequest NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Instructs the Query-Builder in method BuildQueryString to remove
     *          an eventually present OUTER-JOIN keyword from the source default
     *          query when filtering on a ceratin table.
     * Notes:   This allows to filter Customers by SalesRep fields in a Query:
     *          FOR EACH Customer, FIRST SalesRep OF Customer OUTER-JOIN
     *          The property is forwarded to the constructor of the DSQueryString
     *          class.
     *          See Bug 2609 for a detailled discussion on the new functionality
     */
    DEFINE PUBLIC PROPERTY FilterUsingInnerJoin AS LOGICAL NO-UNDO INITIAL FALSE
    GET.
    PROTECTED SET.

    /**
     * Purpose: Allows to force applying the Source Default Query to child tables
     *          even when no filter expression was provided by the consumer
     * Notes:   When set to TRUE the FetchData method will assume a default
     *          "FOR EACH <temp-table-name>" query from the client, rather than an
     *          empty query string
     *          Useful when the source default query is required to filter out records
     *          even when dependent on the parent table relation (e.g. when records
     *          with certain statusses are filterred out)
     */
    DEFINE PUBLIC PROPERTY ForceApplyQueryOnChildTables AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Allows to turn on and off the logging of details of the
     *          FetchData method execution
     * Notes:
     */
    DEFINE PUBLIC STATIC PROPERTY LogFetchDataDetails AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets if the Data Access class should prepare the query on
     *          joined child tables during FetchData
     * Notes:   SCL-334: DataAccess does not prepare queries on child tables with
     *          joined query data sources
     */
    DEFINE PROTECTED PROPERTY PrepareJoinedChildTableQueries AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns the current purpose of the SourceDefaultQuery callback
     * Notes:   Only valid while the SourceDefaultQuery method is executed, allows
     *          to return different/optimized queries for differnt usages of
     *          the data source query, see SCL-258
     */
    DEFINE PROTECTED PROPERTY QueryPurpose AS QueryPurposeEnum NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns the type of the current request (FetchData/SaveChanges)
     * Notes:   Can be used in AttachDataSource etc. to adjust data sources for
     *          optimized read or save operations
     */
    DEFINE PUBLIC PROPERTY RequestType AS RequestTypeEnum NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Constructor of the DataAccess objects
     * Notes:   Requires to pass a valid dataset handle
     * @param phDataset The handle of the Business Entity dataset
     */
    CONSTRUCTOR PUBLIC DataAccess (phDataset AS HANDLE):
        SUPER ().

        IF NOT VALID-HANDLE (phDataset) OR phDataset:TYPE <> "DATASET":U THEN
            UNDO, THROW NEW AppError ("Invalid Dataset reference in DataAccess constructor."{&TRAN}, 0) .

        THIS-OBJECT:DatasetHandle = phDataset .

        THIS-OBJECT:AssignSkipDictionary = NEW Consultingwerk.Framework.Collections.CharacterDictionary
                ("":U, CHR (1), "":U, CHR (1)) .

        ASSIGN oDataSourceQueryInfoProvider = {Consultingwerk/get-service.i Consultingwerk.OERA.IDataSourceQueryInfoProvider}
               THIS-OBJECT:QueryPurpose = QueryPurposeEnum:Invalid .

        &IF DEFINED (AblReflection) NE 0 &THEN
        ASSIGN cTransactionMethodNames = ListHelper:MergeLists (AblReflectionHelper:GetMethodNamesByParameterCount (THIS-OBJECT:GetClass(),
                                                                                                                    0,
                                                                                                                    Progress.Reflect.Flags:Protected OR Progress.Reflect.Flags:Instance),
                                                                AblReflectionHelper:GetMethodNamesByParameterCount (THIS-OBJECT:GetClass(),
                                                                                                                    0,
                                                                                                                    Progress.Reflect.Flags:Public OR Progress.Reflect.Flags:Instance)) .
        &ENDIF

    END CONSTRUCTOR.

    /**
     * Purpose: Invoked by DataAccess:CommitChanges within the database transaction
     *          to optionally assign key field values for new records
     * Notes:   Provides a hook to assign key fields for new records before the
     *          changes (create) are written to the database. The default implementation
     *          of this method does nothing. Override this method to implement specific
     *          code
     * @param phBeforeBuffer The handle of the before buffer
     */
    METHOD PROTECTED VOID AssignKeyFieldValues (phBeforeBuffer AS HANDLE):

        /* noop */

    END METHOD .

    /**
     * Purpose: Attaches a data-source object to a temp-table buffer in a ProDataSet
     *          object
     * Notes:   Filters out any white space character from the field lists to allow
     *          for human readable source code in the DataAccess classes
     *          AttachDataSources method
     * @param phDatasetBuffer The handle of the ProDataset Buffer
     * @param phDataSource The handle of the DATA-SOURCE object
     * @param pcPairsList pairs-list An optional character expression that evaluates to a comma-delimited list of field name pairs that specify a mapping between different field names in the data-source object buffer and the ProDataSet temp-table buffer
     * @return Logical return value of the ATTACH-DATA-SOURCE method
     */
    METHOD PROTECTED LOGICAL AttachDataSource (phDatasetBuffer AS HANDLE,
                                               phDataSource AS HANDLE,
                                               pcPairsList AS CHARACTER):

        RETURN THIS-OBJECT:AttachDataSource (phDatasetBuffer, phDataSource, pcPairsList, ?, ?) .

    END METHOD.

    /**
     * Purpose: Attaches a data-source object to a temp-table buffer in a ProDataSet
     *          object
     * Notes:   Filters out any white space character from the field lists to allow
     *          for human readable source code in the DataAccess classes
     *          AttachDataSources method
     * @param phDatasetBuffer The handle of the ProDataset Buffer
     * @param phDataSource The handle of the DATA-SOURCE object
     * @param pcPairsList pairs-list An optional character expression that evaluates to a comma-delimited list of field name pairs that specify a mapping between different field names in the data-source object buffer and the ProDataSet temp-table buffer
     * @param pcExceptList An optional character expression that evaluates to a comma-separated list of fields in the ProDataSet object buffer that will not be populated with data from the data source (that is, fields to exclude). Use this option when it is easier to specify fields to exclude rather than include. You can specify except-fields or include-fields, but not both
     * @return Logical return value of the ATTACH-DATA-SOURCE method
     */
    METHOD PROTECTED LOGICAL AttachDataSource (phDatasetBuffer AS HANDLE,
                                               phDataSource AS HANDLE,
                                               pcPairsList AS CHARACTER,
                                               pcExceptList AS CHARACTER):

        RETURN THIS-OBJECT:AttachDataSource (phDatasetBuffer, phDataSource, pcPairsList, pcExceptList, ?) .

    END METHOD.

    /**
     * Purpose: Attaches a data-source object to a temp-table buffer in a ProDataSet
     *          object
     * Notes:   Filters out any white space character from the field lists to allow
     *          for human readable source code in the DataAccess classes
     *          AttachDataSources method
     * @param phDatasetBuffer The handle of the ProDataset Buffer
     * @param phDataSource The handle of the DATA-SOURCE object
     * @param pcPairsList pairs-list An optional character expression that evaluates to a comma-delimited list of field name pairs that specify a mapping between different field names in the data-source object buffer and the ProDataSet temp-table buffer
     * @param pcExceptList An optional character expression that evaluates to a comma-separated list of fields in the ProDataSet object buffer that will not be populated with data from the data source (that is, fields to exclude). Use this option when it is easier to specify fields to exclude rather than include. You can specify except-fields or include-fields, but not both
     * @param pcIncludeFields An optional character expression that evaluates to a comma-separated list of fields to include in the ProDataSet object buffer, as an alternative to specifying fields to exclude in except-fields. Use this option when it is easier to specify fields to include rather than exclude. You can specify include-fields or except-fields, but not both. If you specify include-fields, you must set except-fields to the Unknown value (?)
     * @return Logical return value of the ATTACH-DATA-SOURCE method
     */
    METHOD PROTECTED LOGICAL AttachDataSource (phDatasetBuffer AS HANDLE,
                                               phDataSource AS HANDLE,
                                               pcPairsList AS CHARACTER,
                                               pcExceptList AS CHARACTER,
                                               pcIncludeFields AS CHARACTER):

        DEFINE VARIABLE cPairsList     AS CHARACTER NO-UNDO INITIAL ? .
        DEFINE VARIABLE cExceptList    AS CHARACTER NO-UNDO INITIAL ? .
        DEFINE VARIABLE cIncludeFields AS CHARACTER NO-UNDO INITIAL ? .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDatasetBuffer WidgetTypeEnum:Buffer} .
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDataSource WidgetTypeEnum:DataSource} .

        IF pcPairsList > "":U THEN
            ASSIGN cPairsList = REPLACE (REPLACE (REPLACE (REPLACE (pcPairsList, " ":U, "":U), CHR(9), "":U), CHR (10), "":U), CHR(13), "":U) .
        IF pcExceptList > "":U THEN
            ASSIGN cExceptList = REPLACE (REPLACE (REPLACE (REPLACE (pcExceptList, " ":U, "":U), CHR(9), "":U), CHR (10), "":U), CHR(13), "":U) .
        IF pcIncludeFields > "":U THEN
            ASSIGN cIncludeFields = REPLACE (REPLACE (REPLACE (REPLACE (pcIncludeFields, " ":U, "":U), CHR(9), "":U), CHR (10), "":U), CHR(13), "":U) .

        RETURN phDatasetBuffer:ATTACH-DATA-SOURCE (phDataSource, cPairsList, cExceptList, cIncludeFields) .

    END METHOD.

    /**
     * Purpose: Attaches a data-source object generated by an BufferDataSource
     *          instance to a temp-table buffer in a ProDataSet object
     * Notes:   Filters out any white space character from the field lists to allow
     *          for human readable source code in the DataAccess classes
     *          AttachDataSources method
     * @param phDatasetBuffer The handle of the ProDataset Buffer
     * @param poDataSource The reference to the the BufferDataSource instance that manages a data-source object handle
     * @return Logical return value of the ATTACH-DATA-SOURCE method
     */
    METHOD PROTECTED LOGICAL AttachDataSource (phDatasetBuffer AS HANDLE,
                                               poDataSource AS IBufferDataSource):

        HandleAssert:ValidHandle (phDatasetBuffer, "phDatasetBuffer":U) .
        ObjectAssert:IsValid (poDataSource, "poDataSource":U) .

        RETURN poDataSource:AttachDataSource (phDatasetBuffer) .

    END METHOD.

    /**
     * Purpose: Attaches a data-source object generated by an BufferDataSource
     *          instance to a temp-table buffer in a ProDataSet object
     * Notes:   Filters out any white space character from the field lists to allow
     *          for human readable source code in the DataAccess classes
     *          AttachDataSources method
     * @param phDatasetBuffer The handle of the ProDataset Buffer
     * @param poDataSource The reference to the the BufferDataSource instance that manages a data-source object handle
     * @param pcPairsList pairs-list An optional character expression that evaluates to a comma-delimited list of field name pairs that specify a mapping between different field names in the data-source object buffer and the ProDataSet temp-table buffer
     * @return Logical return value of the ATTACH-DATA-SOURCE method
     */
    METHOD PROTECTED LOGICAL AttachDataSource (phDatasetBuffer AS HANDLE,
                                               poDataSource AS IBufferDataSource,
                                               pcPairsList AS CHARACTER):

        HandleAssert:ValidHandle (phDatasetBuffer, "phDatasetBuffer":U) .
        ObjectAssert:IsValid (poDataSource, "poDataSource":U) .

        RETURN poDataSource:AttachDataSource (phDatasetBuffer, pcPairsList) .

    END METHOD.

    /**
     * Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers
     * Notes:   ABSTRACT method in Consultingwerk.OERA.DataAccess, overridden in
     *          actual DataAccess objects
     *          Invoked in FetchData and SaveChanges
     */
    METHOD PROTECTED ABSTRACT VOID AttachDataSources ().

    /**
     * Purpose: Returns the Query String to be used for the DATA-SOURCE of a ProDataset
     *          member buffer
     * Notes:   Wrapper to the access to the DSQueryString class to simplify overriding
     *          the query manipulation in a custom Data Access class or customer
     *          DataAccess base class override
     *          This method is used to provide the query string for non top-level buffers
     *          The returned Query String is based on the Query String that was provided
     *          (by the consumer) and the Source Default Query of the Data Access object
     * @param phBufferHandle The Handle of the ProDataset member buffer
     * @param poFetchDataRequest The FetchDataRequest object of the current request
     * @param pcQueryString The query string provided by the consumer
     * @return The Query String to be used for the data source of the ProDataset member buffer
     */
    METHOD PROTECTED CHARACTER BuildDataSourceQueryString (phBufferHandle AS HANDLE,
                                                           poFetchDataRequest AS IFetchDataRequest,
                                                           pcQueryString AS CHARACTER):

        DEFINE VARIABLE oQueryString AS IDSQueryString NO-UNDO .
        DEFINE VARIABLE hTempQuery   AS HANDLE         NO-UNDO.
        DEFINE VARIABLE iDSbuffer    AS INTEGER        NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i  phBufferHandle WidgetTypeEnum:Buffer} .

        oQueryString = NewDSQueryString (pcQueryString, phBufferHandle:NAME).

        CREATE QUERY hTempQuery.
        DO iDSbuffer = 1 TO phBufferHandle:DATA-SOURCE:NUM-SOURCE-BUFFERS:
            hTempQuery:ADD-BUFFER(phBufferHandle:DATA-SOURCE:GET-SOURCE-BUFFER (iDsbuffer)).
        END.

        RETURN oQueryString:BuildQueryString (hTempQuery).

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oQueryString) .
            GarbageCollectorHelper:DeleteObject (hTempQuery) .

            ASSIGN THIS-OBJECT:QueryPurpose = QueryPurposeEnum:Invalid .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Returns a TRUE value if a record is found that meets the specified
     *          FIND criteria; otherwise it returns FALSE. CAN-FIND does not make
     *          the record available to the Business Entity or it's caller.
     * Notes:   Result will be assigned to the Result property of the CanFindParameter
     *          instance
     * @param poParameter The CanFindParameter with the parameters for this call
     */
    METHOD PUBLIC VOID CanFind (poParameter AS CanFindParameter):

        DEFINE VARIABLE lAttached AS LOGICAL          NO-UNDO INITIAL FALSE .

        ASSIGN THIS-OBJECT:RequestType      = RequestTypeEnum:FetchData .

        IF NOT THIS-OBJECT:DataSourcesAttached THEN DO:
            ASSIGN lAttached = TRUE .

            THIS-OBJECT:AttachDataSources () .
        END.

        IF poParameter:TableName > "":U THEN .
        ELSE
            ASSIGN poParameter:TableName = THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (1):NAME .

        DO ON ERROR UNDO, THROW:

            THIS-OBJECT:FindRecord2 (poParameter:FindString,
                                     THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (poParameter:TableName),
                                     "":U,
                                     0,
                                     1) .

            poParameter:Result = TRUE .
            RETURN .

            @SuppressUnusedWarnings.
            CATCH err AS NotFoundException:
                poParameter:Result = FALSE .
                RETURN .
            END CATCH.
        END.

        FINALLY:
            /* Mike Fechner, Consultingwerk Ltd. 26.01.2011
               Bug 2861: DetachDataSources now moved to FINALLY block */
            IF THIS-OBJECT:DataSourcesAttached AND lAttached THEN DO:
                THIS-OBJECT:DetachDataSources () .
                THIS-OBJECT:DataSourcesAttached = FALSE .
            END.
            THIS-OBJECT:FetchDataRequest = ? .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: This method is called from SaveChanges in the DA
     *          to do the actual database commit and any transaction-related
     *          validation logic for a buffer.
     * Notes:   This is taken directly from the OERI.
     *       -  Note that in this sample that illustrates support for distributed
     *          business logic the DA saves the whole dataset in SaveChanges, so
     *          this is not currently called from the BE.
     *       -  The event names are the same as in the OERI. The word "Trans"
     *          is somewhat misleading as the DA need to support large transactions.
     *          One could consider using a name that indicates that this is a row
     *          event instead.
     * @param phBuffer The handle of the after buffer to commit
     */
    METHOD PROTECTED VOID CommitChanges (phBuffer AS HANDLE):

        DEFINE VARIABLE hDataSet     AS HANDLE                     NO-UNDO.
        DEFINE VARIABLE hBeforeBuff  AS HANDLE                     NO-UNDO.

        DEFINE VARIABLE hTable       AS HANDLE                     NO-UNDO.

        DEFINE VARIABLE oEventArgs   AS HandleCommitErrorEventArgs NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 02.07.2009
           Use the ORIGIN-HANDLE Table name for evaluating the table-name
           Required if changes-dataset contains name prefix (i.e. "ch_")
           When send form client */
        IF VALID-HANDLE(phBuffer:TABLE-HANDLE:ORIGIN-HANDLE) THEN
            ASSIGN hTable = phBuffer:TABLE-HANDLE:ORIGIN-HANDLE .
        ELSE
            ASSIGN hTable = phBuffer:TABLE-HANDLE .

        ASSIGN hBeforeBuff  = phBuffer:BEFORE-BUFFER
               hDataSet     = phBuffer:DATASET .

        /* This is a sub transaction, the actual transaction is in savechanges */
        DO TRANSACTION ON ERROR UNDO, THROW:
            /* Mike Fechner, Consultingwerk Ltd. 12.09.2011
               Raise BeforeSavingRowChanges  */
            OnBeforeSavingRowChanges (NEW SaveRowChangesEventArgs (hTable:NAME,
                                                                   phBuffer,
                                                                   hBeforeBuff,
                                                                   hBeforeBuff:ROW-STATE)) .

            IF NOT hBeforeBuff:ERROR THEN
                THIS-OBJECT:InvokeCommitChangesCallback(CommitCallbackTypeEnum:BeginTrans, hBeforeBuff) .

            IF NOT hBeforeBuff:ERROR THEN
            DO ON ERROR UNDO, THROW:

                /* Mike Fechner, Consultingwerk Ltd. 24.04.2012
                   Reset RETURN-VALUE before call to SAVE-ROW-CHANGES so
                   that we properly can catch errors from Triggers */
                RETURN-VALUE = "":U .

                DO ON ERROR UNDO, THROW:
                    /* Mike Fechner, Consultingwerk Ltd. 03.10.2015
                       Provides a hook to assign key fields for new records before the
                       changes (create) are written to the database */
                    IF hBeforeBuff:ROW-STATE = ROW-CREATED THEN
                        THIS-OBJECT:AssignKeyFieldValues (hBeforeBuff) .

                    /* Mike Fechner, Consultingwerk Ltd. 06.10.2011
                       Ability to override hBeforeBuff:SAVE-ROW-CHANGES by moving if to a separate method */
                    THIS-OBJECT:SaveRowChanges (hBeforeBuff) .

                    CATCH syserr AS Progress.Lang.SysError:
                        oEventArgs = NEW HandleCommitErrorEventArgs (syserr, hBeforeBuff) .

                        THIS-OBJECT:OnHandleCommitSysError (oEventArgs) .

                        IF VALID-OBJECT (oEventArgs) AND oEventArgs:Handled = TRUE THEN .
                        ELSE DO:
                            ASSIGN hBeforeBuff:ERROR = TRUE
                                   hDataSet:ERROR    = TRUE .

                            CASE syserr:GetMessageNum (1):
                                WHEN 132 THEN
                                     DatasetHelper:AddErrorString (hBeforeBuff,
                                                                   RecordAlreadyExistsException:FromSysError (syserr):GetMessage (1)) .

                                WHEN 11913 THEN
                                    DatasetHelper:AddErrorString (hBeforeBuff,
                                                                  Consultingwerk.OERA.Resources.OERACustomizer:DataAccess_RecordChangedByAnotherUser) .
                                WHEN 11914 THEN DO:
                                    IF RETURN-VALUE > "":U THEN
                                        DatasetHelper:AddErrorString (hBeforeBuff,
                                                                      RETURN-VALUE) .
                                    ELSE
                                        DatasetHelper:AddErrorString (hBeforeBuff,
                                                                      Consultingwerk.OERA.Resources.OERACustomizer:DataAccess_UnableToDelete) .
                                END.
                                WHEN 11915 THEN DO:
                                    IF RETURN-VALUE > "":U THEN
                                        DatasetHelper:AddErrorString (hBeforeBuff,
                                                                      RETURN-VALUE) .
                                    ELSE
                                        DatasetHelper:AddErrorString (hBeforeBuff,
                                                                      Consultingwerk.OERA.Resources.OERACustomizer:DataAccess_UnableToUpdate) .
                                END.
                                WHEN 12300 THEN
                                    THIS-OBJECT:HandleRecordLockedError (hBeforeBuff,
                                                                         syserr) .

                                OTHERWISE DO:
                                        DatasetHelper:AddErrorString (hBeforeBuff,
                                                                      ErrorHelper:FormattedErrorMessages (syserr)) .

                                        IF RETURN-VALUE > "":U THEN
                                            DatasetHelper:AddErrorString (hBeforeBuff,
                                                                          RETURN-VALUE) .

                                        IF FrameworkSettings:DebugMode AND syserr:CallStack > "":U THEN
                                            DatasetHelper:AddErrorString (hBeforeBuff,
                                                                          SUBSTITUTE ("~nBackend-Stacktrace:~n&1~n":U, STRING (ErrorHelper:StackTrace (syserr)))) .
                                END .
                            END CASE .
                        END.
                    END.

                    CATCH apperr AS Progress.Lang.AppError:
                        oEventArgs = NEW HandleCommitErrorEventArgs (apperr, hBeforeBuff) .

                        THIS-OBJECT:OnHandleCommitError (oEventArgs) .

                        IF VALID-OBJECT (oEventArgs) AND oEventArgs:Handled = TRUE THEN .
                        ELSE DO:
                            DatasetHelper:AddErrorString (hBeforeBuff,
                                                          ErrorHelper:FormattedErrorMessages (apperr)) .

                            IF FrameworkSettings:DebugMode AND apperr:CallStack > "":U THEN
                                DatasetHelper:AddErrorString (hBeforeBuff,
                                                              SUBSTITUTE ("~nBackend-Stacktrace:~n&1~n":U, STRING (ErrorHelper:StackTrace (apperr)))) .
                        END.
                    END CATCH.
                END.

                /* Mike Fechner, Consultingwerk Ltd. 12.09.2011
                   Raise AfterSavingRowChanges  */
                IF NOT hBeforeBuff:ERROR THEN
                    OnAfterSavingRowChanges (NEW SaveRowChangesEventArgs (hTable:NAME,
                                                                          phBuffer,
                                                                          hBeforeBuff,
                                                                          hBeforeBuff:ROW-STATE)) .

                IF NOT hBeforeBuff:ERROR THEN DO:
                    THIS-OBJECT:InvokeCommitChangesCallback(CommitCallbackTypeEnum:EndTrans, hBeforeBuff) .

                    /* Mike Fechner, Consultingwerk Ltd. 04.07.2009
                       Errors flagged from EndTrans procedure should undo the transaction */
                    IF hBeforeBuff:ERROR THEN
                        UNDO, RETURN .
                END.

            END. /* END DO IF no error after BeginTrans validation. */
        END.     /* END RowTrans TRANSACTION block */
    END METHOD.

    /**
     * Purpose: Counts the number of result records in the data source query
     * Notes:
     * @param poFetchDataRequest The ICountRecordsRequest that also implements IFetchDataRequest
     */
    METHOD PUBLIC VOID CountResultRecords (poFetchDataRequest AS ICountRecordsRequest):

        DEFINE VARIABLE hBuffer              AS HANDLE                 NO-UNDO .
        DEFINE VARIABLE cQuery               AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE hQuery               AS HANDLE                 NO-UNDO .

        DEFINE VARIABLE cTables              AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE cQueries             AS CHARACTER              NO-UNDO .

        DEFINE VARIABLE iStopAfter           AS INTEGER                NO-UNDO .

        ASSIGN THIS-OBJECT:RequestType      = RequestTypeEnum:FetchData.

        /* Mike Fechner, Consultingwerk Ltd. 20.01.2015
           Bug SCL-621: When the data-sources should (still) be attached here, that
                        indicates the the last invokation of this method here was
                        stopped by STOP or STOP-AFTER and might result in the
                        query-issues descriped in the ticket */
        IF THIS-OBJECT:DataSourcesAttached = TRUE THEN
            THIS-OBJECT:DetachDataSources() .

        THIS-OBJECT:AttachDataSources () .

        ASSIGN THIS-OBJECT:DataSourcesAttached = TRUE .

        ASSIGN
               cTables    = poFetchDataRequest:Tables
               cQueries   = poFetchDataRequest:Queries
               iStopAfter = poFetchDataRequest:StopAfter
               .

        /* Mike Fechner, Consultingwerk Ltd. 14.07.2013
           Support for * wild-cards in the FetchDataRequest:Tables parameter */
        IF INDEX (cTables, "*":U) > 0 THEN
            cTables = THIS-OBJECT:ExpandTables (cTables) .

        hBuffer = THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (ENTRY (1, cTables)) .

        /* query mapping with callback to sourcecolumn and source-default-query */
        cQuery  = ENTRY(1, cQueries, CHR(1)).

        /* Mike Fechner, Consultingwerk Ltd. 27.05.2014
           SCL-258 */
        ASSIGN THIS-OBJECT:QueryPurpose     = QueryPurposeEnum:RetrieveParentTable
               THIS-OBJECT:FetchDataRequest = CAST (poFetchDataRequest, IFetchDataRequest) .

        hQuery = THIS-OBJECT:InsertToDataSourceQuery (hBuffer,
                                                      CAST (poFetchDataRequest, IFetchDataRequest),
                                                      cQuery) .

        THIS-OBJECT:QueryPurpose = QueryPurposeEnum:Invalid .

        DO
        /* Mike Fechner, Consultingwerk Ltd. 01.05.2013
           Bug SCL-31: STOP-AFTER support
           Variable stop-after is not supported on OE10.2B */
        &IF PROVERSION NE "10.2B" &THEN
           STOP-AFTER iStopAfter
        &ENDIF
           ON STOP UNDO, RETURN ERROR NEW StopConditionException():

            IF NOT hQuery:IS-OPEN THEN
                hQuery:QUERY-OPEN () .

            hQuery:GET-LAST () .

            /* indexed-reposition? */
            IF hQuery:NUM-RESULTS = 1 THEN DO:
                ASSIGN cQueries                   = hQuery:PREPARE-STRING
                       ENTRY (1, cQueries, " ":U) = "PRESELECT":U .

                hQuery:QUERY-PREPARE (cQueries) .
                hQuery:QUERY-OPEN () .
            END.

            poFetchDataRequest:NumResults = hQuery:NUM-RESULTS .
        END.

        FINALLY:
            /* Mike Fechner, Consultingwerk Ltd. 07.07.2009
               Query won't be valid if primary table has no DATA-SOURCE */
            GarbageCollectorHelper:DeleteObject (hQuery) .

            ASSIGN THIS-OBJECT:RequestType      = ?
                   THIS-OBJECT:FetchDataRequest = ? .

            /* Mike Fechner, Consultingwerk Ltd. 26.01.2011
               Bug 2861: DetachDataSources now moved to FINALLY block */
            THIS-OBJECT:DetachDataSources () .
            THIS-OBJECT:DataSourcesAttached = FALSE .
            THIS-OBJECT:FetchDataRequest = ? .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Invokes the FILL method of a ProDataset member buffer
     * Notes:   Allows overriding this method in custom variants of the Data Access
     *          Errors thrown from or raised in the FILL call-back methods of the
     *          DataAccess class (SET-CALLBACK) will stop the Dataset from getting
     *          filled / filled further. However errors thrown from the call-back
     *          methods are not thrown from the FILL() method further up into a
     *          catch block or to the client.
     *          See http://knowledgebase.progress.com/articles/Article/P188375
     * @param phBuffer The handle of the proDataset
     * @param poFetchDataRequest The FetchDataRequest instance with the parameters for the current read
     * @return If the FILL is successful, this method returns TRUE. Otherwise, it returns FALSE.
     */
    METHOD PROTECTED LOGICAL DatasetBufferFill (phBuffer AS HANDLE,
                                                poFetchDataRequest AS IFetchDataRequest):

        DEFINE VARIABLE lResult      AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBuffer WidgetTypeEnum:Buffer} .

        IF NOT THIS-OBJECT:ThrowErrorsOnFillFailed THEN
            RETURN phBuffer:FILL () .

        ASSIGN cReturnValue = RETURN-VALUE .

        lResult = phBuffer:FILL () .

        IF lResult = FALSE THEN DO:
            IF RETURN-VALUE > "":U AND RETURN-VALUE <> cReturnValue THEN
                UNDO, THROW NEW DatasetBufferFillFailedException (RETURN-VALUE, 0) .

            UNDO, THROW NEW DatasetBufferFillFailedException ("Dataset Buffer Fill failed."{&TRAN}, 0) .

        END.

        RETURN TRUE .

    END METHOD.

    /**
     * Purpose: Invokes the ProDataset FILL method
     * Notes:   Allows overriding this method in custom variants of the Data Access
     *          Errors thrown from or raised in the FILL call-back methods of the
     *          DataAccess class (SET-CALLBACK) will stop the Dataset from getting
     *          filled / filled further. However errors thrown from the call-back
     *          methods are not thrown from the FILL() method further up into a
     *          catch block or to the client.
     *          See http://knowledgebase.progress.com/articles/Article/P188375
     * @param phDataset The handle of the ProDataset
     * @param poFetchDataRequest The FetchDataRequest instance with the parameters for the current read
     * @return If the FILL is successful, this method returns TRUE. Otherwise, it returns FALSE.
     */
    METHOD PROTECTED LOGICAL DatasetFill (phDataset AS HANDLE,
                                          poFetchDataRequest AS IFetchDataRequest):

        DEFINE VARIABLE lResult      AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDataset WidgetTypeEnum:Dataset} .

        IF NOT THIS-OBJECT:ThrowErrorsOnFillFailed THEN
            RETURN phDataset:FILL () .

        ASSIGN cReturnValue = RETURN-VALUE .

        lResult = phDataset:FILL () .

        IF lResult = FALSE THEN DO:
            IF RETURN-VALUE > "":U AND RETURN-VALUE <> cReturnValue THEN
                UNDO, THROW NEW DatasetFillFailedException (RETURN-VALUE, 0) .

            UNDO, THROW NEW DatasetFillFailedException ("Dataset Fill failed."{&TRAN}, 0) .
        END.

        RETURN TRUE .

    END METHOD.

    /**
     * Purpose: TO-DO: Subscribe to ProDataset Event Handlers using SET-CALLBACK
     *          as needed
     * Notes:   ABSTRACT method in Consultingwerk.OERA.DataAccess, overridden in
     *          actual DataAccess objects
     *          Invoked in FetchData
     */
    METHOD PROTECTED ABSTRACT VOID DefineReadEvents ().

    /**
     * Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers
     * Notes:   ABSTRACT method in Consultingwerk.OERA.DataAccess, overridden in
     *          actual DataAccess objects
     *          Invoked in FetchData and SaveChanges
     */
    METHOD PROTECTED ABSTRACT VOID DetachDataSources ().

    /**
     * Purpose: Expands the FetchDataRequest:Tables parameter, in case it contains
     *          the * wild-card character
     * Notes:
     * @param pcTables The initial Tables parameter
     * @return The tables parameter with the wild-cards expanded
     */
    METHOD PROTECTED CHARACTER ExpandTables (pcTables AS CHARACTER):

        DEFINE VARIABLE hDataset AS HANDLE    NO-UNDO .

        ASSIGN hDataset = THIS-OBJECT:DatasetHandle .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i hDataset WidgetTypeEnum:Dataset} .

        IF NOT VALID-HANDLE (hDataset) OR hDataset:TYPE <> "DATASET":U THEN
            UNDO, THROW NEW AppError ("Unable to call FetchData with no Dataset attached."{&TRAN}, 0) .

        RETURN ListHelper:ExpandListEntries (pcTables,
                                             DatasetHelper:BufferNames (hDataset)) .

    END METHOD .

    /**
     * Purpose: Allows retrieve multiple DB records from a Business Entity based on
     *          a temp-table with key values
     * Notes:
     * @param poParameter The IFetchDataByKeyTableParameter with the data for this call
     */
    METHOD PUBLIC VOID FetchDataByKeyTable (poParameter AS IFetchDataByKeyTableParameter):

        DEFINE VARIABLE oRequest      AS FetchDataRequest NO-UNDO .
        DEFINE VARIABLE hBuffer       AS HANDLE           NO-UNDO .
        DEFINE VARIABLE hSourceBuffer AS HANDLE           NO-UNDO EXTENT .
        DEFINE VARIABLE hQuery        AS HANDLE           NO-UNDO .
        DEFINE VARIABLE hDataSource   AS HANDLE           NO-UNDO .
        DEFINE VARIABLE i             AS INTEGER          NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poParameter """IFetchDataByKeyTableParameter"":U"} .
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i poParameter:KeyValueTableHandle WidgetTypeEnum:TempTable} .

        oRequest = NEW FetchDataRequest (poParameter:Tables) .

        ASSIGN THIS-OBJECT:RequestType      = RequestTypeEnum:FetchData
               THIS-OBJECT:FetchDataRequest = oRequest

               hBuffer = THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE
                                    (ENTRY (1, poParameter:Tables)).

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i hBuffer """Buffer Handle"":U"} .

        /* Mike Fechner, Consultingwerk Ltd. 20.01.2015
           Bug SCL-621: When the data-sources should (still) be attached here, that
                        indicates the the last invokation of this method here was
                        stopped by STOP or STOP-AFTER and might result in the
                        query-issues descriped in the ticket */
        IF THIS-OBJECT:DataSourcesAttached = TRUE THEN
            THIS-OBJECT:DetachDataSources() .

        THIS-OBJECT:AttachDataSources () .

        ASSIGN THIS-OBJECT:DataSourcesAttached = TRUE .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i hBuffer:DATA-SOURCE """hBuffer:DATA-SOURCE"":U"} .

        ASSIGN lInFetchDataByKeyTable        = TRUE
               oFetchDataByKeyTableParameter = poParameter
               cTempFieldPairs               = hBuffer:DATA-SOURCE-COMPLETE-MAP .

        CREATE QUERY hQuery .
        hQuery:ADD-BUFFER (poParameter:KeyValueTableHandle:DEFAULT-BUFFER-HANDLE) .

        EXTENT (hSourceBuffer) = hBuffer:DATA-SOURCE:NUM-SOURCE-BUFFERS .

        DO i = 1 TO hBuffer:DATA-SOURCE:NUM-SOURCE-BUFFERS:
            CREATE BUFFER hSourceBuffer[i] FOR TABLE hBuffer:DATA-SOURCE:GET-SOURCE-BUFFER (i) .

            hQuery:ADD-BUFFER (hSourceBuffer[i]) .
        END.

        CREATE DATA-SOURCE hDataSource .

        hDataSource:QUERY = hQuery .

        hBuffer:ATTACH-DATA-SOURCE (hDataSource, cTempFieldPairs) .

        THIS-OBJECT:FetchDataInternal (oRequest) .

        FINALLY:
            /* Mike Fechner, Consultingwerk Ltd. 26.01.2011
               Bug 2861: DetachDataSources now moved to FINALLY block */
            THIS-OBJECT:DetachDataSources () .
            THIS-OBJECT:DataSourcesAttached = FALSE .
            THIS-OBJECT:FetchDataRequest = ? .

            GarbageCollectorHelper:DeleteObject (hDataSource) .
            GarbageCollectorHelper:DeleteObject (hQuery) .

            DO i = 1 TO EXTENT (hSourceBuffer):
                GarbageCollectorHelper:DeleteObject (hSourceBuffer[i]) .
            END.

            ASSIGN lInFetchDataByKeyTable        = FALSE
                   oFetchDataByKeyTableParameter = ?
                   cTempFieldPairs               = "":U .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Fetch data from the data source
     * Notes:   poFetchDataRequest:Context property is used to control batching
     *          (forward/backwards) and the FindRowWhere/PreFetchOnFind
     *          The first entry by CHR(1) in the Context is CHR(2) delimited:
     *          1. Entry: RESTART-ROWID(s) - CHR(5) Delimited
     *          2. Entry: Find String
     *          3. Entry: PrefetchOnFind Value
     *          The poFetchDataRequest parameter may also be passed in as a
     *          FetchDataAccessRequest instance allowing further specific arguments
     *          to be passed that are only relevant to the DataAccess class
     * @param poFetchDataRequest The IFetchDataRequest object with the parameters for this call
     */
    METHOD PUBLIC VOID FetchData (poFetchDataRequest AS IFetchDataRequest):

        ASSIGN THIS-OBJECT:RequestType      = RequestTypeEnum:FetchData
               THIS-OBJECT:FetchDataRequest = poFetchDataRequest .

        /* Mike Fechner, Consultingwerk Ltd. 20.01.2015
           Bug SCL-621: When the data-sources should (still) be attached here, that
                        indicates the the last invokation of this method here was
                        stopped by STOP or STOP-AFTER and might result in the
                        query-issues descriped in the ticket */
        IF THIS-OBJECT:DataSourcesAttached = TRUE THEN
            THIS-OBJECT:DetachDataSources() .

        THIS-OBJECT:AttachDataSources () .

        ASSIGN THIS-OBJECT:DataSourcesAttached = TRUE .

        THIS-OBJECT:FetchDataInternal (poFetchDataRequest) .

        FINALLY:
            /* Mike Fechner, Consultingwerk Ltd. 26.01.2011
               Bug 2861: DetachDataSources now moved to FINALLY block */
            THIS-OBJECT:DetachDataSources () .
            THIS-OBJECT:DataSourcesAttached = FALSE .
            THIS-OBJECT:FetchDataRequest = ? .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Fetch data from the data source
     * Notes:   Internal version of the method. The attaching and de-attaching of
     *          the DataSources is no done in FetchData. The possible recursive call
     *          to fetch a complete LAST batch after FindRowWhere is calling into
     *          the method FetchDataInteral do not interfer with the attaching of
     *          the data-source's. SCL-621: This structure allows us to always deattach
     *          the data-sources when they should still be around (left over due to
     *          missing FINALLY-Block execution after STOP-Condition
     * @param poFetchDataRequest The IFetchDataRequest object with the parameters for this call
     */
    METHOD PRIVATE VOID FetchDataInternal (poFetchDataRequest AS IFetchDataRequest):

        DEFINE VARIABLE iBuffer              AS INTEGER                NO-UNDO .
        DEFINE VARIABLE iTable               AS INTEGER                NO-UNDO .
        DEFINE VARIABLE hBuffer              AS HANDLE                 NO-UNDO .
        DEFINE VARIABLE cQuery               AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE cSaveQuery           AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE hQuery               AS HANDLE                 NO-UNDO .
        DEFINE VARIABLE hDataset             AS HANDLE                 NO-UNDO .
        DEFINE VARIABLE hFillBuffer          AS HANDLE                 NO-UNDO .

        DEFINE VARIABLE cTables              AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE iBatchSize           AS INTEGER                NO-UNDO .
        DEFINE VARIABLE cContext             AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE cQueries             AS CHARACTER              NO-UNDO .

        DEFINE VARIABLE iMessage             AS INTEGER                NO-UNDO .
        DEFINE VARIABLE oAppError            AS Progress.Lang.AppError NO-UNDO .
        DEFINE VARIABLE oException           AS FetchDataException     NO-UNDO .
        DEFINE VARIABLE cDefault             AS CHARACTER              NO-UNDO .

        DEFINE VARIABLE cRequestContext      AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE roRestartRowids      AS IQueryRowIdentifier    NO-UNDO .
        DEFINE VARIABLE roNextRowids         AS IQueryRowIdentifier    NO-UNDO .
        DEFINE VARIABLE cFindString          AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE iPrefetchOnFind      AS INTEGER                NO-UNDO .
        DEFINE VARIABLE iQueryIndexInfo      AS INTEGER                NO-UNDO .
        DEFINE VARIABLE oFindRecordResult    AS FindRecordResult       NO-UNDO .

        DEFINE VARIABLE hLogBuffer           AS HANDLE                 NO-UNDO .

        DEFINE VARIABLE iNumResults          AS INTEGER                NO-UNDO .

        DEFINE VARIABLE oQueryInfo           AS IDataSourceQueryInfo   NO-UNDO .

        DEFINE VARIABLE hSaveQueries         AS HANDLE NO-UNDO EXTENT .

        ASSIGN hDataset = THIS-OBJECT:DatasetHandle

               cTables    = poFetchDataRequest:Tables
               iBatchSize = poFetchDataRequest:NumRecords
               cContext   = poFetchDataRequest:Context
               cQueries   = poFetchDataRequest:Queries

               roRestartRowids = THIS-OBJECT:NewQueryRowIdentifier ()

               .

        IF NOT VALID-HANDLE (hDataset) OR hDataset:TYPE <> "DATASET":U THEN
            UNDO, THROW NEW AppError ("Unable to call FetchData with no Dataset attached."{&TRAN}, 0) .

        /* Mike Fechner, Consultingwerk Ltd. 01.07.2015
           SCL-878 - we need to store the original data-source query handles */
        ASSIGN hSaveQueries = DatasetHelper:GetDataSourceQueryHandles (hDataset) .

        /* Mike Fechner, Consultingwerk Ltd. 14.07.2013
           Support for * wild-cards in the FetchDataRequest:Tables parameter */
        IF INDEX (cTables, "*":U) > 0 THEN
            cTables = THIS-OBJECT:ExpandTables (cTables) .

        DO iBuffer = 1 TO NUM-ENTRIES (cTables) ON ERROR UNDO, THROW:
            DatasetAssert:HasBuffer (hDataset, ENTRY (iBuffer, cTables)) .

            CATCH err AS Progress.Lang.Error:
                UNDO, THROW NEW InvalidTablesParameterValueException (err,
                                                                      "poFetchDataRequest:Tables":U,
                                                                      poFetchDataRequest:Tables,
                                                                      DatasetHelper:BufferNames(hDataset),
                                                                      ENTRY (iBuffer, cTables),
                                                                      THIS-OBJECT:GetClass():TypeName) .
            END CATCH.
        END.

        THIS-OBJECT:DefineReadEvents () .

        /* Mike Fechner, Consultingwerk Ltd. 01.10.2015
           SCL-1048: Optionally don't empty the Dataset before FILL */
        IF TYPE-OF (poFetchDataRequest, FetchDataAccessRequest) THEN DO:
            IF CAST (poFetchDataRequest, FetchDataAccessRequest):AppendFetchData = FALSE THEN
                THIS-OBJECT:DatasetHandle:EMPTY-DATASET () .
        END.
        ELSE
            /* Mike Fechner, Consultingwerk Ltd. 03.04.2009
               Only return current result set */
            THIS-OBJECT:DatasetHandle:EMPTY-DATASET () .

        DO iBuffer = 1 TO hDataset:NUM-BUFFERS:
            ASSIGN /* set handle and check if table is requested */
                hBuffer = hDataSet:GET-BUFFER-HANDLE(iBuffer)
                iTable  = LOOKUP(hBuffer:NAME, cTables).

            /* if table found (and valid datasource )*/
            IF iTable > 0 THEN
            DO:
                /* Mike Fechner, Consultingwerk Ltd. 07.10.2015
                   SCL-1057: Optionally use custom FILL-MODE */
                IF TYPE-OF (poFetchDataRequest, FetchDataAccessRequest) AND
                    CAST (poFetchDataRequest, FetchDataAccessRequest):FillMode > "":U THEN
                    hBuffer:FILL-MODE = CAST (poFetchDataRequest, FetchDataAccessRequest):FillMode .
                ELSE
                    hBuffer:FILL-MODE = "REPLACE":U.

                /* Mike Fechner, Consultingwerk Ltd. 23.07.2009
                   The first table might not have a DATA-SOURCE, it might implement
                   custom fill using a tables BEFORE-FILL event (before Change 10274)
                   this code was dependent on the DATA-SOURCE */
                IF iTable = 1 THEN
                    ASSIGN hFillBuffer = hBuffer .

                IF VALID-HANDLE(hBuffer:DATA-SOURCE) THEN
                DO:
                    /* Mike Fechner, Consultingwerk Ltd. 12.04.2013
                       SCL-48 - prepare query in case more than a single TOP-LEVEL-BUFFER
                       are requested */
                    IF NOT VALID-HANDLE (hBuffer:PARENT-RELATION) OR iTable = 1 THEN
                    DO:
                        /* set batchsize */
                        hBuffer:BATCH-SIZE = ABSOLUTE (iBatchSize).
                        /* if context set batch start */

                        IF ENTRY (1, cContext, CHR(1)) > "":U THEN DO:

                            /* Mike Fechner, Consultingwerk Ltd. 21.01.2011
                               The first entry - by CHR(1) of the Context is used for
                               Batching and repositioning.
                               1. Entry: RESTART-ROWID(s) - CHR(5) Delimited
                               2. Entry: Find String
                               3. Entry: PrefetchOnFind Value */
                            ASSIGN cRequestContext = ENTRY(1, cContext, CHR(1))
                                NO-ERROR .

                            roRestartRowids:ParseString (ENTRY (1, cRequestContext, CHR (2))) NO-ERROR .

                            IF roRestartRowids:IsBaseRowidSpecified () THEN
                                roRestartRowids:SetBufferRestartRowids (hBuffer) .

                            IF NUM-ENTRIES (cRequestContext, CHR (2)) > 1 THEN
                                ASSIGN cFindString = ENTRY (2, cRequestContext, CHR (2)) .
                            IF NUM-ENTRIES (cRequestContext, CHR (2)) > 2 THEN
                                ASSIGN iPrefetchOnFind = INTEGER (ENTRY (3, cRequestContext, CHR (2))) NO-ERROR  .
                        END.

                        /* Mike Fechner, Consultingwerk Ltd. 26.11.2010
                           Error handling */
                        DO ON ERROR UNDO, THROW:
                            /* query mapping with callback to sourcecolumn and source-default-query */
                            cQuery  = ENTRY(1, cQueries, CHR(1)).

                            /* Mike Fechner, Consultingwerk Ltd. 23.04.2011
                               DataAccess Query Logging */
                            IF DataAccess:LogFetchDataDetails THEN DO:
                                LogManager:WriteMessage (SUBSTITUTE ("Client-Query: &1"{&TRAN},
                                                                     cQuery),
                                                         DataAccess:CUSTOM_LOG_ENTRY) .

                                /* Mike Fechner, Consultingwerk Ltd. 27.05.2014
                                   SCL-258 */
                                THIS-OBJECT:QueryPurpose = QueryPurposeEnum:LogFetchDataDetails .

                                LogManager:WriteMessage (SUBSTITUTE ("Default-Query: &1"{&TRAN},
                                                                     THIS-OBJECT:SourceDefaultQuery (hBuffer:NAME)),
                                                         DataAccess:CUSTOM_LOG_ENTRY) .

                                THIS-OBJECT:QueryPurpose = QueryPurposeEnum:Invalid .
                            END.

                            /* Mike Fechner, Consultingwerk Ltd. 27.12.2013
                               Provide tracing information to caller */
                            IF VALID-OBJECT (oDataSourceQueryInfoProvider) THEN DO:
                                oQueryInfo = oDataSourceQueryInfoProvider:GetDataSourceQueryInfo (THIS-OBJECT:GetClass():TypeName,
                                                                                                  poFetchDataRequest) .

                                IF VALID-OBJECT (oQueryInfo) THEN
                                    ASSIGN oQueryInfo:ClientQuery        = cQuery

                                           /* Mike Fechner, Consultingwerk Ltd. 27.05.2014
                                              SCL-258 */
                                           THIS-OBJECT:QueryPurpose = QueryPurposeEnum:DataSourceQueryInfoProvider
                                           oQueryInfo:SourceDefaultQuery = THIS-OBJECT:SourceDefaultQuery (hBuffer:NAME)
                                           THIS-OBJECT:QueryPurpose = QueryPurposeEnum:Invalid .
                            END.

                            /* Mike Fechner, Consultingwerk Ltd. 27.05.2014
                               SCL-258 */
                            THIS-OBJECT:QueryPurpose = QueryPurposeEnum:RetrieveParentTable .

                            hQuery = THIS-OBJECT:InsertToDataSourceQuery (hBuffer,
                                                                          poFetchDataRequest,
                                                                          cQuery) .

                            hBuffer:DATA-SOURCE:QUERY = hQuery.

                            /* Mike Fechner, Consultingwerk Ltd. 03.12.2013
                               When doing the FetchLast call, we will now open the query
                               in reverse order and process the query forwards */
                            IF cFindString = "LAST":U THEN
                                QueryHelper:PrepareQueryInReverseOrder (hQuery) .

                            /* Mike Fechner, Consultingwerk Ltd. 23.04.2011
                               DataAccess Query Logging */
                            IF DataAccess:LogFetchDataDetails THEN DO:
                                LogManager:WriteMessage (SUBSTITUTE ("Prepare-Query: &1"{&TRAN},
                                                                     hQuery:PREPARE-STRING),
                                                         DataAccess:CUSTOM_LOG_ENTRY) .

                                DO iQueryIndexInfo = 1 TO hQuery:NUM-BUFFERS ON ERROR UNDO, THROW:
                                    /* Mike Fechner, Consultingwerk Ltd. 03.04.2012
                                       Bug 2820: Only query INDEX-INFORMATION for Progress DB's or Temp-Tables */
                                    ASSIGN hLogBuffer = hQuery:GET-BUFFER-HANDLE(iQueryIndexInfo) .

                                    IF (DBTYPE (hLogBuffer:DBNAME) = "PROGRESS":U OR DBTYPE (hLogBuffer:DBNAME) = ?) THEN
                                        LogManager:WriteMessage (SUBSTITUTE ("Index Information on Buffer &2: &1"{&TRAN},
                                                                             hQuery:INDEX-INFORMATION (iQueryIndexInfo),
                                                                             hLogBuffer:NAME),
                                                                 DataAccess:CUSTOM_LOG_ENTRY) .

                                    CATCH err AS Progress.Lang.Error:
                                        /* ignore errors above (Logging-Code should not throw errors) */
                                    END CATCH.
                                END.
                            END.

                            IF VALID-OBJECT (oQueryInfo) THEN
                                ASSIGN oQueryInfo:PrepareQuery = hQuery:PREPARE-STRING .

                            CATCH err AS Progress.Lang.Error :
                                /* Mike Fechner, Consultingwerk Ltd. 27.05.2014
                                   SCL-258 */
                                ASSIGN THIS-OBJECT:QueryPurpose = QueryPurposeEnum:CatchBlock .

                                DO ON ERROR UNDO, THROW:
                                    cDefault = THIS-OBJECT:SourceDefaultQuery (hBuffer:NAME) .

                                    @SuppressUnusedWarnings.
                                    CATCH innererr AS Progress.Lang.Error:
                                        /* ignore errors from here */
                                    END CATCH.
                                END.

                                ASSIGN THIS-OBJECT:QueryPurpose = QueryPurposeEnum:Invalid .

                                ASSIGN oException = NEW FetchDataException (err,
                                                                            err:GetMessage (1),
                                                                            err:GetMessageNum (1),
                                                                            cQuery,
                                                                            cDefault) .

                                DO iMessage = 2 TO err:NumMessages:
                                    oException:AddMessage (err:GetMessage(iMessage), 0) .
                                END.

                                oException:AddMessage (SUBSTITUTE ("Client-Query: &1"{&TRAN}, cQuery), 0) .
                                oException:AddMessage (SUBSTITUTE ("Default-Query: &1"{&TRAN}, cDefault), 0) .

                                UNDO, THROW oException .
                            END CATCH.
                        END. /* DO ON ERROR UNDO, THROW: */

                        /* Mike Fechner, Consultingwerk Ltd. 21.01.2011
                           Find row and adjust Restart Rowid */
                        IF cFindString > "":U AND cFindString <> "LAST":U THEN DO:
                            ASSIGN oFindRecordResult = THIS-OBJECT:FindRecord2 (cFindString, hBuffer, cQuery, iPrefetchOnFind, iBatchSize)
                                   roRestartRowids   = oFindRecordResult:RestartRowids .

                            IF roRestartRowids:IsBaseRowidSpecified () THEN
                                roRestartRowids:SetBufferRestartRowids (hBuffer) .
                        END.
                        ELSE IF TYPE-OF (poFetchDataRequest, IFetchDataBySkip) AND CAST (poFetchDataRequest, IFetchDataBySkip):Skip > 0 THEN DO:
                            ASSIGN roRestartRowids = THIS-OBJECT:SkipRecords (CAST (poFetchDataRequest, IFetchDataBySkip):Skip, hBuffer) .

                            IF roRestartRowids:IsBaseRowidSpecified () THEN
                                roRestartRowids:SetBufferRestartRowids (hBuffer) .
                        END.

                        IF iBatchSize < 0 THEN DO:
                            QueryHelper:PrepareQueryInReverseOrder (hQuery) .

                            IF roRestartRowids:IsBaseRowidSpecified() THEN
                                roRestartRowids:SetBufferRestartRowids (hBuffer) .
                        END. /* IF iBatchSize < 0 THEN DO: */
                    END. /* first table */
                    ELSE DO:
                        /* Mike Fechner, Consultingwerk Ltd. 04.06.2014
                           Always enter this code, block when the Data Access class enforces the ForceApplyQueryOnChildTables  */
                        IF NUM-ENTRIES(cQueries, CHR(1)) >= iTable OR THIS-OBJECT:ForceApplyQueryOnChildTables
                           OR (THIS-OBJECT:PrepareJoinedChildTableQueries = TRUE AND VALID-HANDLE (hBuffer:DATA-SOURCE) AND
                               hBuffer:DATA-SOURCE:NUM-SOURCE-BUFFERS > 1)

                        THEN DO:
                            /* Get Querystring for this buffer and ensure 4 entries - separated by space */
                            IF NUM-ENTRIES(cQueries, CHR(1)) >= iTable THEN
                                cQuery = ENTRY(iTable, cQueries,CHR(1)) + "   ":U.
                            ELSE
                                ASSIGN cQuery = "":U .

                            IF THIS-OBJECT:ForceApplyQueryOnChildTables = TRUE AND cQuery = "":U THEN
                                cQuery = SUBSTITUTE ("FOR EACH &1":U, hBuffer:NAME) .

                            IF cQuery > "":U OR
                              (THIS-OBJECT:PrepareJoinedChildTableQueries = TRUE AND VALID-HANDLE (hBuffer:DATA-SOURCE) AND
                               hBuffer:DATA-SOURCE:NUM-SOURCE-BUFFERS > 1)

                            THEN DO ON ERROR UNDO, THROW:
                                /* Mike Fechner, Consultingwerk Ltd. 27.05.2014
                                   SCL-258 */
                                THIS-OBJECT:QueryPurpose = QueryPurposeEnum:RetrieveChildTable .

                                ASSIGN cQuery = THIS-OBJECT:BuildDataSourceQueryString (hBuffer, poFetchDataRequest, cQuery) .

                                IF THIS-OBJECT:PrepareJoinedChildTableQueries = TRUE AND VALID-HANDLE (hBuffer:DATA-SOURCE) AND
                                   hBuffer:DATA-SOURCE:NUM-SOURCE-BUFFERS > 1 AND VALID-HANDLE (hBuffer:DATA-SOURCE:QUERY) AND hBuffer:DATA-SOURCE:QUERY:PREPARE-STRING = ?
                                THEN
                                    ASSIGN cSaveQuery = cQuery .
                                ELSE
                                    ASSIGN cSaveQuery = ? .

                                IF ENTRY(1, cQuery, " ":U) = "FOR":U THEN
                                    ASSIGN ENTRY (1, cQuery, " ":U) = "":U
                                           ENTRY (2, cQuery, " ":U) = "":U
                                           ENTRY (3, cQuery, " ":U) = "":U .

                                IF NUM-ENTRIES (cQuery, " ":U) > 3 AND ENTRY(4, cQuery, " ":U) = "WHERE":U THEN
                                    ASSIGN
                                        cQuery = SUBSTRING (cQuery, INDEX (cQuery, "WHERE":U) + 6, -1, "CHARACTER":U) .

                                cQuery = TRIM (cQuery) .
                            END. /* IF cQuery > "":U */

                            /* append cQuery to data-source fill-where-string */
                            IF cQuery > "":U THEN DO:
                                IF hBuffer:DATA-SOURCE:FILL-WHERE-STRING > "":U THEN
                                   hBuffer:DATA-SOURCE:FILL-WHERE-STRING = hBuffer:DATA-SOURCE:FILL-WHERE-STRING +
                                                                           (IF NOT cQuery BEGINS "BY":U THEN " AND ":U ELSE " ":U) + cQuery .
                                ELSE
                                    hBuffer:DATA-SOURCE:FILL-WHERE-STRING = "WHERE ":U + cQuery .
                            END.

                            IF cSaveQuery > "":U THEN DO:
                                cSaveQuery = QueryHelper:InsertExpression (SUBSTRING (cSaveQuery, 1, INDEX (cSaveQuery, ",":U) - 1, "CHARACTER":U),
                                                                           THIS-OBJECT:ParentRelationSourceQueryString(hBuffer),
                                                                           "AND":U) + SUBSTRING (cSaveQuery, INDEX (cSaveQuery, ",":U), -1, "CHARACTER":U) .

                                hBuffer:DATA-SOURCE:QUERY:QUERY-PREPARE (cSaveQuery).
                            END.

                        END. /* IF NUM-ENTRIES(cQueries, CHR(1)) >= iTable THEN DO: */
                    END. /* ELSE DO: */
                END. /* IF VALID-HANDLE(hBuffer:DATA-SOURCE) */
            END. /* IF iTable > 0 THEN */
            ELSE
                hBuffer:FILL-MODE = "NO-FILL":U.
        END. /* DO iBuffer = 1 TO hDataset:NUM-BUFFERS: */

        /* Mike Fechner, Consultingwerk Ltd. 23.04.2011
           DataAccess Logging */
        IF DataAccess:LogFetchDataDetails THEN DO:
            IF VALID-HANDLE (hFillBuffer:DATA-SOURCE) THEN
                LogManager:WriteMessage (SUBSTITUTE ("Restart-Rowid for Table &1: &2"{&TRAN},
                                                     hFillBuffer:NAME,
                                                     hFillBuffer:DATA-SOURCE:RESTART-ROWID),
                                         DataAccess:CUSTOM_LOG_ENTRY) .
            ELSE
                LogManager:WriteMessage (SUBSTITUTE ("Restart-Rowid for Table &1: &2"{&TRAN},
                                                     hFillBuffer:NAME,
                                                     "<no data source>"{&TRAN}),
                                         DataAccess:CUSTOM_LOG_ENTRY) .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 03.04.2009
           FILL based on child table or dataset */
        IF ENTRY(1, cTables) <> hDataset:GET-TOP-BUFFER(1):NAME THEN
            THIS-OBJECT:DatasetBufferFill (hFillBuffer, poFetchDataRequest) NO-ERROR .
        ELSE
            THIS-OBJECT:DatasetFill (hDataset, poFetchDataRequest) NO-ERROR .

/*        IF ERROR-STATUS:ERROR OR ERROR-STATUS:NUM-MESSAGES > 0 THEN*/
/*            MESSAGE ERROR-STATUS:GET-MESSAGE (1)                   */
/*                VIEW-AS ALERT-BOX.                                 */

        /* Mike Fechner, Consultingwerk Ltd. 01.10.2010
           Fix Bug 2416 */
        IF ERROR-STATUS:GET-NUMBER (1) = 91 OR ERROR-STATUS:GET-NUMBER (1) = 138 THEN
            ERROR-STATUS:ERROR = FALSE NO-ERROR .
        /* Mike Fechner, Consultingwerk Ltd. 25.02.2011
           Handling of remaining errors from FILL (temp-table with no data-source,
           temp-table with no unique index, ...) */
        ELSE IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
            ASSIGN oAppError = NEW Progress.Lang.AppError (ERROR-STATUS:GET-MESSAGE (1),
                                                           ERROR-STATUS:GET-NUMBER (1)) .

            DO iMessage = 2 TO ERROR-STATUS:NUM-MESSAGES:
                oAppError:AddMessage (ERROR-STATUS:GET-MESSAGE (iMessage),
                                      ERROR-STATUS:GET-NUMBER (iMessage)) .
            END.

            UNDO, THROW oAppError .
        END.

        /* Marko Rüterbories, Consultingwerk Ltd. 18.06.2009
           If PARENT-RELATION:REPOSITION of a Buffer which is requested is TRUE
           a dedicated FILL operation for this Buffer has to be performed. */
        DO iBuffer = 1 TO hDataset:NUM-BUFFERS:
            IF ListHelper:EntryIsInList (hDataset:GET-BUFFER-HANDLE (iBuffer):NAME, cTables)
               AND VALID-HANDLE (hDataset:GET-BUFFER-HANDLE (iBuffer):PARENT-RELATION) THEN

                IF hDataset:GET-BUFFER-HANDLE (iBuffer):PARENT-RELATION:REPOSITION THEN
                    THIS-OBJECT:DatasetBufferFill (hDataset:GET-BUFFER-HANDLE (iBuffer),
                                                   poFetchDataRequest) .
        END.

        /* set batch context for first table */
        hBuffer = hDataset:GET-BUFFER-HANDLE (ENTRY(1, cTables)) .

        /* Mike Fechner, Consultingwerk Ltd. 07.07.2009
           Support Tables with no DataSource  */
        /* Mike Fechner, Consultingwerk Ltd. 19.01.2011
           Check validity of hBuffer first */
        IF VALID-HANDLE (hBuffer) AND (NOT hBuffer:LAST-BATCH) AND VALID-HANDLE (hBuffer:DATA-SOURCE) THEN DO:
            roNextRowids = THIS-OBJECT:NewQueryRowIdentifier () .
            roNextRowids:GetDataSourceNextRowids (hBuffer:DATA-SOURCE) .

            ENTRY(1, cContext, CHR(1)) = roNextRowids:ToString() .

            GarbageCollectorHelper:DeleteObject (roNextRowids) .
        END.
        ELSE
            ENTRY(1, cContext, CHR(1)) = "":U.

        /* Mike Fechner, Consultingwerk Ltd. 19.11.2012
           Bug 2837: Verify that a FindRowWhere on the server does always return
           a full batch of records.  */
        IF cFindString > "":U AND cFindString <> "LAST":U AND
           VALID-HANDLE (hBuffer) AND iBatchSize > 0 AND
           VALID-HANDLE (hBuffer:DATA-SOURCE) AND hBuffer:LAST-BATCH THEN DO
               ON ERROR UNDO, THROW:

            ASSIGN iNumResults = BufferHelper:NumRecords (hBuffer) .

            IF iNumResults < iBatchSize THEN DO:
                ASSIGN cContext                            = poFetchDataRequest:Context
                       cRequestContext                     = ENTRY (1, cContext, CHR (1))
                       ENTRY (2, cRequestContext, CHR (2)) = "LAST":U
                       ENTRY (1, cContext, CHR (1))        = cRequestContext
                       poFetchDataRequest:Context          = cContext .

                THIS-OBJECT:FetchDataInternal (poFetchDataRequest) .
            END.

            FINALLY:
                DELETE OBJECT hQuery .
            END FINALLY.
        END.

        /* Mike Fechner, Consultingwerk Ltd. 23.04.2011
           DataAccess Logging */
        IF DataAccess:LogFetchDataDetails THEN DO:
            IF VALID-HANDLE (hFillBuffer:DATA-SOURCE) THEN DO:
                LogManager:WriteMessage (SUBSTITUTE ("Next-Rowid for Table &1: &2"{&TRAN},
                                                     hFillBuffer:NAME,
                                                     hFillBuffer:DATA-SOURCE:NEXT-ROWID),
                                         DataAccess:CUSTOM_LOG_ENTRY) .
                LogManager:WriteMessage (SUBSTITUTE ("Last-Batch for Table &1: &2"{&TRAN},
                                                     hFillBuffer:NAME,
                                                     hFillBuffer:LAST-BATCH),
                                         DataAccess:CUSTOM_LOG_ENTRY) .
            END.
            ELSE
                LogManager:WriteMessage (SUBSTITUTE ("Restart-Rowid for Table &1: &2"{&TRAN},
                                                     hFillBuffer:NAME,
                                                     "<no data source>"{&TRAN}),
                                         DataAccess:CUSTOM_LOG_ENTRY) .
        END.

        ASSIGN
            poFetchDataRequest:NextContext = cContext
            poFetchDataRequest:Context     = cContext .

        /* Set PrevContext, when Batching Backwards or after a FindRowWhere request */
        IF iBatchSize < 0 OR cFindString > "":U THEN DO:
            IF VALID-OBJECT (oFindRecordResult) AND oFindRecordResult:PreviousBatchAvailable = FALSE THEN
                poFetchDataRequest:PrevContext = "":U .
            ELSE
                poFetchDataRequest:PrevContext = roRestartRowids:ToString () .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 03.12.2013
           When doing the FetchLast call, we will now open the query
           in reverse order and process the query forwards - the NextContext
           in this case needs to be returned empty, and the PrevContext needs
           to activate batching */
        IF cFindString = "LAST":U THEN DO:
            ASSIGN poFetchDataRequest:PrevContext = poFetchDataRequest:NextContext
                   poFetchDataRequest:NextContext = "":U .
        END.
        ELSE IF iBatchSize < 0 THEN DO:
            DEFINE VARIABLE cTemp AS CHARACTER NO-UNDO.
            ASSIGN cTemp                          = poFetchDataRequest:PrevContext
                   poFetchDataRequest:PrevContext = poFetchDataRequest:NextContext
                   poFetchDataRequest:NextContext = cTemp .

        END.

        IF TYPE-OF (THIS-OBJECT, IProvidesSmartRecordInformation) THEN
            CAST (THIS-OBJECT, IProvidesSmartRecordInformation):AssignSmartRecordInformation () .

        FINALLY:
            /* Mike Fechner, Consultingwerk Ltd. 01.07.2015
               Restore queries of data-sources */
            DatasetHelper:SetDataSourceQueryHandles (hDataset, hSaveQueries) .

            /* Mike Fechner, Consultingwerk Ltd. 07.07.2009
               Query won't be valid if primary table has no DATA-SOURCE */
            GarbageCollectorHelper:DeleteObject (hQuery) .

            ASSIGN THIS-OBJECT:RequestType      = ?
                   THIS-OBJECT:FetchDataRequest = ? .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Finds a record in the data source query and returns the ROWID ARRAY
     *          (used as RESTART-ROWID in FetchData)
     * Notes:   This method is kept for backwards compatibility as the FindRecord2
     *          method is now the actual working method
     * @param pcFindString WHERE eCustomer.CustNum = 10
     * @param phBufferHandle The temp-table buffer handle
     * @param pcQueryString The current data-source query string
     * @param piPrefetch The number of records to prefetch
     * @param piBatchSize The number of records to return to the client (total incl. prefetch)
     * @return The ROWID array of the records - that starts the resulting batch
     */
    METHOD PROTECTED ROWID EXTENT FindRecord (pcFindString   AS CHARACTER,
                                              phBufferHandle AS HANDLE,
                                              pcQueryString  AS CHARACTER,
                                              piPrefetch     AS INTEGER,
                                              piBatchSize    AS INTEGER):

        DEFINE VARIABLE oResult AS FindRecordResult NO-UNDO .
        DEFINE VARIABLE oArray  AS ROWID     EXTENT NO-UNDO .

        oResult = FindRecord2 (pcFindString,
                               phBufferHandle,
                               pcQueryString,
                               piPrefetch,
                               piBatchSize) .

        IF VALID-OBJECT (oResult) AND TYPE-OF (oResult:RestartRowids,
                                               QueryRowIdentifier) THEN
            RETURN CAST (oResult:RestartRowids,
                         QueryRowIdentifier):ToArray () .
        ELSE
            RETURN oArray .

    END METHOD .

    /**
     * Purpose: Finds a record in the data source query and returns the ROWID ARRAY
     *          (used as RESTART-ROWID in FetchData)
     * Notes:
     * @param pcFindString WHERE eCustomer.CustNum = 10
     * @param phBufferHandle The temp-table buffer handle
     * @param pcQueryString The current data-source query string
     * @param piPrefetch The number of records to prefetch
     * @param piBatchSize The number of records to return to the client (total incl. prefetch)
     * @return The FindRecordResult with the array of rowids and the information if a previous batch is available
     */
    METHOD PROTECTED FindRecordResult FindRecord2 (pcFindString   AS CHARACTER,
                                                   phBufferHandle AS HANDLE,
                                                   pcQueryString  AS CHARACTER,
                                                   piPrefetch     AS INTEGER,
                                                   piBatchSize    AS INTEGER):

        DEFINE VARIABLE hQuery             AS HANDLE              NO-UNDO .
        DEFINE VARIABLE cSort              AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE iPos               AS INTEGER             NO-UNDO .
        DEFINE VARIABLE roRowids           AS IQueryRowIdentifier NO-UNDO .
        DEFINE VARIABLE lPreviousAvailable AS LOGICAL             NO-UNDO .

        DEFINE VARIABLE cQueryString       AS CHARACTER           NO-UNDO .

        roRowids = THIS-OBJECT:NewQueryRowIdentifier () .

        /* Mike Fechner, Consultingwerk Ltd. 05.05.2011
           The Indexed-reposition phrase may cause trouble when concatenating
           the query strings, it will be added by the QueryString class */
        ASSIGN pcQueryString = TRIM (pcQueryString) .

        IF pcQueryString > "":U AND
           ENTRY (NUM-ENTRIES (pcQueryString, " ":U), pcQueryString, " ":U) = "INDEXED-REPOSITION":U THEN DO:

            ENTRY (NUM-ENTRIES (pcQueryString, " ":U), pcQueryString, " ":U) = "":U .
        END.

        ASSIGN cQueryString = pcQueryString
               pcFindString = TRIM (pcFindString) .

        IF ENTRY (1, pcFindString, " ":U) = "WHERE":U THEN
            ENTRY (1, pcFindString, " ":U) = " ":U .

        ASSIGN iPos = INDEX (cQueryString, " BY ":U) .

        IF iPos > 0 THEN
            ASSIGN cSort         = SUBSTRING (cQueryString, iPos + 4, -1, "CHARACTER":U)
                   cQueryString = SUBSTRING (cQueryString, 1, iPos - 1, "CHARACTER":U) .

        IF cQueryString > "":U THEN
            ASSIGN cQueryString = SUBSTITUTE ("&1 AND &2":U,
                                              cQueryString,
                                               pcFindString) .
        ELSE
            ASSIGN cQueryString = SUBSTITUTE ("FOR EACH &1 WHERE &2":U,
                                               phBufferHandle:NAME,
                                               pcFindString) .

        IF cSort > "":U THEN
            ASSIGN cQueryString = SUBSTITUTE ("&1 BY &2":U, cQueryString, cSort) .

        /* Mike Fechner, Consultingwerk Ltd. 27.05.2014
           SCL-258 */
        THIS-OBJECT:QueryPurpose = QueryPurposeEnum:FindRecordQuery .

        hQuery = THIS-OBJECT:InsertToDataSourceQuery (phBufferHandle,
                                                      ?,
                                                      cQueryString) .

        hQuery:QUERY-OPEN () .

        hQuery:GET-FIRST () NO-ERROR .

        IF hQuery:GET-BUFFER-HANDLE (1):AVAILABLE AND piPrefetch > 0 THEN DO:
            roRowids:GetQueryCurrentRowids (hQuery) .

            DELETE OBJECT hQuery .

            /* Mike Fechner, Consultingwerk Ltd. 27.05.2014
               SCL-258 */
            THIS-OBJECT:QueryPurpose = QueryPurposeEnum:FindRecordRepositionBackwards .

            hQuery = THIS-OBJECT:InsertToDataSourceQuery (phBufferHandle,
                                                          ?,
                                                          pcQueryString) .

            hQuery:QUERY-OPEN () .

            roRowids:RepositionQuery (hQuery) .

            hQuery:GET-NEXT () .

            THIS-OBJECT:RepositionBackwards (hQuery, piPrefetch) .

            hQuery:GET-PREV () .

            ASSIGN lPreviousAvailable = hQuery:GET-BUFFER-HANDLE (1):AVAILABLE .

            hQuery:GET-NEXT () .

/*            MESSAGE "prefetch" SKIP (2) hQuery:PREPARE-STRING SKIP (2)*/
/*                    hQuery:GET-BUFFER-HANDLE (1)::CusTNum             */
/*                VIEW-AS ALERT-BOX.                                    */
        END.

        IF hQuery:GET-BUFFER-HANDLE (1):AVAILABLE THEN DO:
            roRowids:GetQueryCurrentRowids (hQuery) .

            RETURN NEW FindRecordResult (roRowids, lPreviousAvailable) .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 21.01.2011
           Raise ABL runtime error, when record not available */
        UNDO, THROW NEW NotFoundException ("No record is available."{&TRAN},
                                           0,
                                           pcFindString,
                                           phBufferHandle:NAME,
                                           pcQueryString,
                                           piPrefetch,
                                           piBatchSize,
                                           THIS-OBJECT:GetClass():TypeName) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .
        END FINALLY .

    END METHOD.

    /**
     * Purpose: Locates the matching record in the first source buffer of the given
     *          Dataset Buffer
     * Notes:
     * @param phBuffer The handle of a (after) ProDataset buffer
     * @param plExclusiveLock Logical value indicating if the record should be locked exclusively
     */
    METHOD PROTECTED VOID FindSourceBuffer (phBuffer AS HANDLE,
                                            plExclusiveLock AS LOGICAL):

        DEFINE VARIABLE i              AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iEntry         AS INTEGER   NO-UNDO .
        DEFINE VARIABLE hSourceBuffer  AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cFindString    AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cSourceField   AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cBufferField   AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cMap           AS CHARACTER NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phBuffer """Buffer Handle"":U"} .
        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phBuffer:DATA-SOURCE """Data-Source Handle"":U"} .

        ASSIGN hSourceBuffer = phBuffer:DATA-SOURCE:GET-SOURCE-BUFFER (1)
               cMap          = phBuffer:DATA-SOURCE-COMPLETE-MAP .

        IF phBuffer:DATA-SOURCE:KEYS (1) = "ROWID":U THEN DO:
              ASSIGN cSourceField = SUBSTITUTE ("ROWID(&1)":U, hSourceBuffer:NAME)

                     iEntry       = LOOKUP (cSourceField, cMap)

                     cBufferField = ENTRY (2, ENTRY (iEntry - 1, cMap), ".":U) .

                     cFindString = SUBSTITUTE ("WHERE &1 = TO-ROWID(&2)":U, cSourceField, QUOTER (phBuffer:BUFFER-FIELD (cBufferField):BUFFER-VALUE)) .
        END.
        ELSE DO i = 1 TO NUM-ENTRIES (phBuffer:DATA-SOURCE:KEYS (1)):

           ASSIGN cSourceField = SUBSTITUTE ("&1.&2":U,
                                             hSourceBuffer:NAME,
                                             ENTRY (i, phBuffer:DATA-SOURCE:KEYS (1))) .

           iEntry = LOOKUP (cSourceField, cMap) .

           IF iEntry = 0 OR iEntry MODULO 2 <> 0 THEN
               UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to determine target field for &1."{&TRAN}, cSourceField), 0) .

           ASSIGN cBufferField = ENTRY (2, ENTRY (iEntry - 1, cMap), ".":U) .

           ASSIGN cFindString = cFindString + (IF i = 1 THEN "WHERE ":U ELSE " AND ":U) +
                                SUBSTITUTE ("&1 = &2":U, cSourceField, QUOTER (phBuffer:BUFFER-FIELD (cBufferField):BUFFER-VALUE)) .
        END.

        IF plExclusiveLock THEN
            hSourceBuffer:FIND-UNIQUE (cFindString, EXCLUSIVE-LOCK) .
        ELSE
            hSourceBuffer:FIND-UNIQUE (cFindString, NO-LOCK) .

    END METHOD.

    /**
     * Purpose: Handles a record locked error invoked during the SAVE-ROW-CHANGES
     *          method
     * Notes:   Allows customization to the Progress default error message
     * @param phBeforeBuffer The handle to befure buffer
     * @param poSysError The orignal error raised
     */
    METHOD PROTECTED VOID HandleRecordLockedError (phBeforeBuffer AS HANDLE,
                                                   poSysError AS Progress.Lang.SysError):


        DEFINE VARIABLE hSourceBuffer  AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO .

        ASSIGN hSourceBuffer = phBeforeBuffer:AFTER-BUFFER:DATA-SOURCE:GET-SOURCE-BUFFER (1) .

        IF VALID-HANDLE (hSourceBuffer) THEN
            ASSIGN cMessage = SUBSTITUTE (OERACustomizer:DataAccess_RecordLocked,
                                          hSourceBuffer:TABLE) .
        ELSE
            ASSIGN cMessage = SUBSTITUTE (OERACustomizer:DataAccess_RecordLocked,
                                          "":U) .

        DatasetHelper:AddErrorString (phBeforeBuffer,
                                      cMessage) .

    END METHOD.

    /**
     * Purpose: Creates a new Query to be used as a DATA-SOURCE for a ProDataset
     *          member buffer
     * Notes:   Wrapper to the access to the DSQueryString class to simplify overriding
     *          the query manipulation in a custom Data Access class or customer
     *          DataAccess base class override
     *          This Query is prepared based on the Query String that was provided
     *          (by the consumer) and the Source Default Query of the Data Access object
     * @param phBufferHandle The Handle of the ProDataset member buffer
     * @param poFetchDataRequest The FetchDataRequest object of the current request
     * @param pcQueryString The query string provided by the consumer
     * @return The handle of the Query that was created
     */
    METHOD PROTECTED HANDLE InsertToDataSourceQuery (phBufferHandle AS HANDLE,
                                                     poFetchDataRequest AS IFetchDataRequest,
                                                     pcQueryString AS CHARACTER):

        DEFINE VARIABLE oQueryString       AS IDSQueryString NO-UNDO .
        DEFINE VARIABLE hQuery             AS HANDLE         NO-UNDO .
        DEFINE VARIABLE iDSbuffer          AS INTEGER        NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i  phBufferHandle WidgetTypeEnum:Buffer} .

        oQueryString = THIS-OBJECT:NewDSQueryString (pcQueryString, phBufferHandle:NAME).

        IF NOT VALID-HANDLE (phBufferHandle:DATA-SOURCE) THEN
            UNDO, THROW NEW Exception (SUBSTITUTE ("Unable to InsertToDataSourceQuery when buffer &1 is not attached to any DATA-SOURCE object handle."{&TRAN},
                                                   phBufferHandle:NAME),
                                       0) .

        CREATE QUERY hQuery.

        DO iDSbuffer = 1 TO phBufferHandle:DATA-SOURCE:NUM-SOURCE-BUFFERS:
            hQuery:ADD-BUFFER (phBufferHandle:DATA-SOURCE:GET-SOURCE-BUFFER (iDsbuffer)).
        END.

        oQueryString:InsertToQuery (hQuery).

        RETURN hQuery .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oQueryString) .

            ASSIGN THIS-OBJECT:QueryPurpose = QueryPurposeEnum:Invalid .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Invokes a CommitChangesCallback
     * Notes:   Provides a central location to handle error thrown from these
     *          callback methods. By default errors thrown will be thrown to the
     *          caller. By overriding this mehod customers can CATCH errors and add
     *          them to the ProDataset buffer ERROR-STRING etc.
     * @param poCommitCallbackType The CommitCallbackTypeEnum value describing the callback to be invoked
     * @param phBeforeBuffer The handle of the before-buffer (to determine the ROW-STATE)
     */
    METHOD PROTECTED VOID InvokeCommitChangesCallback (poCommitCallbackType AS CommitCallbackTypeEnum,
                                                       phBeforeBuffer AS HANDLE):

        DEFINE VARIABLE cLogicMethod AS CHARACTER                  NO-UNDO .
        DEFINE VARIABLE hBuffer      AS HANDLE                     NO-UNDO .
        DEFINE VARIABLE hTable       AS HANDLE                     NO-UNDO .

        ASSIGN hBuffer = phBeforeBuffer:AFTER-BUFFER .

        /* Mike Fechner, Consultingwerk Ltd. 02.07.2009
           Use the ORIGIN-HANDLE Table name for evaluating the table-name
           Required if changes-dataset contains name prefix (i.e. "ch_")
           When send form client */
        IF VALID-HANDLE(hBuffer:TABLE-HANDLE:ORIGIN-HANDLE) THEN
            ASSIGN hTable = hBuffer:TABLE-HANDLE:ORIGIN-HANDLE .
        ELSE
            ASSIGN hTable = hBuffer:TABLE-HANDLE .

        ASSIGN cLogicMethod = hTable:NAME
                         + IF phBeforeBuffer:ROW-STATE = ROW-DELETED THEN "Delete":U
                           ELSE IF phBeforeBuffer:ROW-STATE = ROW-CREATED THEN "Create":U
                           ELSE "Modify":U.

        /* Mike Fechner, Consultingwerk Ltd. 18.01.2016
           OpenEdge 11.6: Only invoke BeginTrans method, when a method with no parameter
                          exists*/
        &IF DEFINED (AblReflection) NE 0 &THEN
        IF LOOKUP (cLogicMethod + STRING (poCommitCallbackType), cTransactionMethodNames) > 0 THEN
        &ENDIF
        DYNAMIC-INVOKE (THIS-OBJECT, cLogicMethod + STRING (poCommitCallbackType)) .
        /* Mike Fechner, Consultingwerk Ltd. 07.03.2010
           Right now we can only dynamically invoke methods, but not
           Query if they are there. So ignore error 15312:
           Could not dynamically find method 'eOrderModifyEndTrans' in class
           'Consultingwerk.OERA.Sports2000.OrderDataAccess' with 0 matching
           parameter(s).  */

        /* Mike Fechner, Consultingwerk Ltd. 11.10.2015
           OpenEdge 11.6 now raises error 14457
           Could not locate method 'eSmartUserModifyBeginTrans' with matching
           signature in class 'Consultingwerk.SmartFramework.Authentication.UserDataAccess'. (14457) */
        CATCH err AS Progress.Lang.SysError:

            IF err:GetMessageNum(1) = 14457 OR err:GetMessageNum (1) = 15312 THEN .
            ELSE
                UNDO, THROW err.

        END CATCH.

    END METHOD.

    /**
     * Purpose: Logs details about "current row has been modified
     *          by another user to the current logile
     * Notes:   Invoked by the SaveRowChanges method when error 11913 occurs
     *          and the SaveRowChanges custom logentry type is activated
     * @param phBeforeBuffer The handle of the current before buffer (record)
     */
    METHOD PROTECTED VOID LogCurrentRowModifiedIssue (phBeforeBuffer AS HANDLE):

        DEFINE VARIABLE hDbBuffer AS HANDLE NO-UNDO.

        IF NOT LogManager:IsCustomLogEntryTypeActive ("SaveRowChanges":U) THEN
            RETURN .

        LogManager:WriteSeparator() .
        LogManager:WriteMessage ("Current BEFORE record:"{&TRAN}, "SaveRowChanges":U) .
        LogManager:WriteBuffer (phBeforeBuffer, FALSE) .
        LogManager:WriteSeparator() .

        CREATE BUFFER hDbBuffer FOR TABLE phBeforeBuffer:AFTER-BUFFER:DATA-SOURCE:GET-SOURCE-BUFFER (1) .

        BufferHelper:FindDatabaseBuffer (hDbBuffer,
                                         phBeforeBuffer,
                                         phBeforeBuffer:AFTER-BUFFER:DATA-SOURCE-COMPLETE-MAP) .

        LogManager:WriteMessage ("Current Database record:"{&TRAN}, "SaveRowChanges":U) .
        LogManager:WriteBuffer(hDbBuffer, FALSE) .
        LogManager:WriteSeparator() .

        FINALLY:
            IF VALID-HANDLE (hDbBuffer) THEN
                DELETE OBJECT hDbBuffer.
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Factory method for the DSQueryString objects
     * Notes:   It's not recommended that customers override this method, SCL-866
     *          Customers should rather override the two methods NewDSQueryString
     *          (pcQuery, pcBuffer, plFilterUsingInnerJoin) and NewDSQueryStringForKeyValueTable
     *          (pcQuery, pcBuffer, plFilterUsingInnerJoin, poFetchDataByKeyTableParameter,
     *          pcFieldPairs)
     * @param pcQuery The Query String for the DSQueryString instance
     * @param pcBuffer The Buffer Name for the DSQueryString instance
     * @return The new IDSQueryString instance
     */
    METHOD PROTECTED IDSQueryString NewDSQueryString (pcQuery AS CHARACTER,
                                                      pcBuffer AS CHARACTER):

        IF lInFetchDataByKeyTable THEN
            RETURN NewDSQueryStringForKeyValueTable (pcQuery,
                                                     pcBuffer,
                                                     THIS-OBJECT:FilterUsingInnerJoin,
                                                     oFetchDataByKeyTableParameter,
                                                     cTempFieldPairs) .
        ELSE
            RETURN NewDSQueryString (pcQuery,
                                     pcBuffer,
                                     THIS-OBJECT:FilterUsingInnerJoin) .

    END METHOD.

    /**
     * Purpose: Factory method for the DSQueryString objects
     * Notes:   Customers should override this method as a factory for custom IDSQueryString
     *          implementations
     * @param pcQuery The Query String for the DSQueryString instance
     * @param pcBuffer The Buffer Name for the DSQueryString instance
     * @param plFilterUsingInnerJoin Logical value indicating if the data source query should be using an inner join
     * @return The new IDSQueryString instance
     */
    METHOD PROTECTED IDSQueryString NewDSQueryString (pcQuery AS CHARACTER,
                                                      pcBuffer AS CHARACTER,
                                                      plFilterUsingInnerJoin AS LOGICAL):

        RETURN NEW DSQueryString (pcQuery,
                                  THIS-OBJECT,
                                  pcBuffer,
                                  plFilterUsingInnerJoin) .

    END METHOD .

    /**
     * Purpose: Factory method for the DSQueryStringForKeyValueTable objects
     * Notes:   Customers should override this method as a factory for custom
     *          DSQueryStringForKeyValueTable implementations
     * @param pcQuery The Query String for the DSQueryString instance
     * @param pcBuffer The Buffer Name for the DSQueryString instance
     * @param plFilterUsingInnerJoin Logical value indicating if the data source query should be using an inner join
     * @param poFetchDataByKeyTableParameter The IFetchDataByKeyTableParameter instance with the parameter of the FetchDataByKeyTable method
     * @param pcFieldPairs The originally attached field pairs
     * @return The new IDSQueryString instance
     */
    METHOD PROTECTED IDSQueryString NewDSQueryStringForKeyValueTable (pcQuery AS CHARACTER,
                                                                      pcBuffer AS CHARACTER,
                                                                      plFilterUsingInnerJoin AS LOGICAL,
                                                                      poFetchDataByKeyTableParameter AS IFetchDataByKeyTableParameter,
                                                                      pcFieldPairs AS CHARACTER):

            RETURN NEW DSQueryStringForKeyValueTable (pcQuery,
                                                      THIS-OBJECT,
                                                      pcBuffer,
                                                      plFilterUsingInnerJoin,
                                                      poFetchDataByKeyTableParameter,
                                                      pcFieldPairs) .

    END METHOD .

    /**
     * Purpose: Overridable factory method to create an instance of the IQueryRowIdentifier
     *          class
     * Notes:
     * @return The reference to the new instance of the IQueryRowIdentifier interface
     */
    METHOD PROTECTED IQueryRowIdentifier NewQueryRowIdentifier ():

        RETURN NEW QueryRowIdentifier () .

    END METHOD .

    /**
     * Purpose: Raises the AfterSaveChanges
     * Notes:
     * @param e The Consultingwerk.EventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnAfterSaveChanges (e AS Consultingwerk.EventArgs):

    /*    Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "AfterSaveChanges":U) . */

        IF NOT VALID-OBJECT (e) THEN
            e = Consultingwerk.EventArgs:Empty .

        THIS-OBJECT:AfterSaveChanges:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raises the AfterSavingRowChanges event
     * Notes:
     * @param e The SaveRowChangesEventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnAfterSavingRowChanges (e AS SaveRowChangesEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("Unable to raise the AfterSavingRowChanges event when there is no event argument"{&TRAN}, 0) .

        THIS-OBJECT:AfterSavingRowChanges:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the BeforeSaveChanges
     * Notes:
     * @param e The Consultingwerk.EventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnBeforeSaveChanges (e AS Consultingwerk.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = Consultingwerk.EventArgs:Empty .

        THIS-OBJECT:BeforeSaveChanges:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raises the BeforeSavingRowChanges event
     * Notes:
     * @param e The SaveRowChangesEventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnBeforeSavingRowChanges (e AS SaveRowChangesEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("Unable to raise the BeforeSavingRowChanges event when there is no event argument"{&TRAN}, 0) .

        THIS-OBJECT:BeforeSavingRowChanges:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the HandleCommitError
     * Notes:
     * @param e The HandleCommitErrorEventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnHandleCommitError (e AS HandleCommitErrorEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "HandleCommitError":U) .

        THIS-OBJECT:HandleCommitError:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raises the HandleCommitSysError
     * Notes:
     * @param e The HandleCommitErrorEventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnHandleCommitSysError (e AS HandleCommitErrorEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "HandleCommitSysError":U) .

        THIS-OBJECT:HandleCommitSysError:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Returns the query string matching the DATA-RELATION field pairs
     *          referencing the source fields on the parent table
     * Notes:
     * @param phBuffer The child buffer of the relation
     * @return The source query string matching the data-relation criteria
     */
    METHOD PUBLIC CHARACTER ParentRelationSourceQueryString (phBuffer AS HANDLE):

        DEFINE VARIABLE cQueryString    AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cSourceColumn   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hParentBuffer   AS HANDLE    NO-UNDO .
        DEFINE VARIABLE i               AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cFullColumnName AS CHARACTER NO-UNDO .

        ASSIGN hParentBuffer = DatasetHelper:ParentBuffer (phBuffer) .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i hParentBuffer """Parent Buffer Handle"":U"} .

        DO i = 1 TO NUM-ENTRIES(phBuffer:PARENT-RELATION:RELATION-FIELDS) BY 2:
            ASSIGN cFullColumnName = SUBSTITUTE ("&1.&2":U, phBuffer:NAME, ENTRY(i + 1, phBuffer:PARENT-RELATION:RELATION-FIELDS)) .


            ASSIGN cSourceColumn = THIS-OBJECT:SourceColumn (phBuffer:NAME,
                                                             cFullColumnName) .

            {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i cSourceColumn "SUBSTITUTE ('Source Column for &1':U, cFullColumnName)"} .

            ASSIGN cQueryString = cQueryString +
                                  (IF i > 1 THEN " AND ":U ELSE "":U) +
                                  SUBSTITUTE("&1 = &2.&3":U,
                                             cSourceColumn,
                                             hParentBuffer:NAME,
                                             ENTRY(i, phBuffer:PARENT-RELATION:RELATION-FIELDS)) .
        END.

        RETURN cQueryString .

    END METHOD .

    /**
     * Purpose: Reposition the given query backwards by the given number of records
     * Notes:   Overidable by child classes to implement custom behaviour
     * @param phQuery The query handle to reposition backwards
     * @param piNumRecords The number of records
     */
    METHOD PUBLIC VOID RepositionBackwards (phQuery AS HANDLE,
                                            piNumRecords AS INTEGER):

        phQuery:REPOSITION-BACKWARDS (piNumRecords).

    END METHOD.

    /**
     * Purpose: Evaluates the ROWID from which the ProDataset FILL operation
     *          needs to start on a backwards fill operation
     * Notes:   This method may be overridden in a DataAccess object class
     *          This method is no longer used for the backwards batching.
     *          It is kept for backwards compability of customers code.
     * @param phQuery The Query Handle to reposition backwards
     * @param proRowids The current ROWID Array
     * @param piBatchSize The number of records to reposition backwards
     * @return The array of starting ROWIDs for the next FILL operation
     */
    METHOD PROTECTED ROWID EXTENT RepositionBackwards (phQuery AS HANDLE,
                                                       proRowids AS ROWID EXTENT,
                                                       piBatchSize AS INTEGER):

        DEFINE VARIABLE roRepositionRowid AS ROWID EXTENT NO-UNDO .

        phQuery:QUERY-OPEN () .

        IF EXTENT (proRowids) > 0 THEN
            IF proRowids[1] <> ? THEN DO:
                QueryHelper:RepositionToRowidArray (phQuery, proRowids) .
                phQuery:REPOSITION-BACKWARDS (ABSOLUTE (piBatchSize)) .
            END.

        phQuery:GET-NEXT () .

        QueryHelper:GetCurrentRowids (phQuery, OUTPUT roRepositionRowid) .

        RETURN roRepositionRowid .

    END METHOD.

    /**
     * Purpose:     Called from saveChanges to process changes
     *              for a single DataSet temp-table buffer.
     * Notes:       This is similar to the OERI BE version, but has no pre and
     *              post transaction hooks because:
     *              - This method might very well be called inside a transaction.
     *              - The BE supports hooks outside of the transaction scope.
     * @param phBuffer The handle of the after buffer
     */
    METHOD PROTECTED VOID SaveBuffer (phBuffer AS HANDLE):

        DEFINE VARIABLE hBeforeBuff AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hBeforeQry  AS HANDLE    NO-UNDO.

        /* phBuffer is the after-table. Walk through the modified rows in
           the before-table, specifically to capture DELETEs, even though it
           is of course the after-table values that Progress writes to the db
           for CREATEs and MODIFYs. */

        hBeforeBuff = phBuffer:BEFORE-BUFFER.

        IF VALID-HANDLE(hBeforeBuff) THEN
        DO ON ERROR UNDO, THROW:
          CREATE QUERY hBeforeQry.
          hBeforeQry:SET-BUFFERS(hBeforeBuff).
          hBeforeQry:QUERY-PREPARE("FOR EACH ":U + hBeforeBuff:NAME).
          hBeforeQry:QUERY-OPEN().
          hBeforeQry:GET-FIRST().
          /* Process all the updates for the current table. */
          DO WHILE NOT hBeforeQry:QUERY-OFF-END ON ERROR UNDO, THROW:
              THIS-OBJECT:CommitChanges (phBuffer) .

              hBeforeQry:GET-NEXT().
          END. /* END DO WHILE NOT QUERY-OFF-END */
        END. /* END DO IF VALID-HANDLE */

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hBeforeQry) .
        END FINALLY.
    END METHOD.

    /**
     * Purpose:  Save changes to physical storage
     * Notes: This implementation of data access save traverses all records in order
     *        to:
     *        - Allow the DA to control transactions, which cannot be directly
     *          controlled in distributed BL. Transaction control is a concern of
     *          the BL, but should not be contolled with TRANSACTION keyword.
     *        - Offer a course grained save interface from distributed BL
     *      - The default operation here is just a transaction per record
     *      - This simple sample does not use child relations to traverse
     *        the buffers as the OERI BE example. (A property that controls
     *        update order would give the same effect).
     *      - Traversing of child data from a parent would make sense if done for
     *        one parent record at a time.
     */
    METHOD PUBLIC VOID SaveChanges ():

        DEFINE VARIABLE iBuffer AS INTEGER NO-UNDO.
        DEFINE VARIABLE hBuffer AS HANDLE  NO-UNDO.

        THIS-OBJECT:RequestType = RequestTypeEnum:SaveChanges .

        THIS-OBJECT:AttachDataSources () .
        THIS-OBJECT:DataSourcesAttached = TRUE .

        transBlock:
        DO TRANSACTION ON ERROR UNDO, THROW:
            OnBeforeSaveChanges (Consultingwerk.EventArgs:Empty) .

            DO iBuffer = 1 TO THIS-OBJECT:DatasetHandle:NUM-BUFFERS:

                hBuffer = THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (iBuffer).

                THIS-OBJECT:SaveBuffer (hBuffer) .

                /* Mike Fechner, Consultingwerk Ltd. 06.09.2011
                   When the Dataset is flagged as with an Error, we should revert
                   the transaction. */
                IF THIS-OBJECT:DatasetHandle:ERROR THEN
                    UNDO, LEAVE TransBlock .

            END.  /* DO iBuffer = 1 */

            OnAfterSaveChanges (Consultingwerk.EventArgs:Empty) .
        END.

        IF TYPE-OF (THIS-OBJECT, IProvidesSmartRecordInformation) THEN
            CAST (THIS-OBJECT, IProvidesSmartRecordInformation):AssignSmartRecordInformation () .

        FINALLY:
            THIS-OBJECT:DetachDataSources() .
            THIS-OBJECT:DataSourcesAttached = FALSE .

            THIS-OBJECT:RequestType = ? .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Overridable method that executes SAVE-ROW-CHANGES on the before buffer
     * Notes:   Called from CommitChanges
     * @param phBeforeBuffer The handle of the before buffer
     */
    METHOD PROTECTED VOID SaveRowChanges (phBeforeBuffer AS HANDLE):

        DEFINE VARIABLE cTableName AS CHARACTER NO-UNDO.

        ASSIGN cTableName = phBeforeBuffer:AFTER-BUFFER:NAME .

        DO ON ERROR UNDO, THROW
           ON STOP UNDO, RETURN ERROR NEW StopConditionException
            ("A stop condition has been raised while saving the changes."{&TRAN}, 0):
            /* Mike Fechner, Consultingwerk Ltd. 26.04.2011
               Skip-List for SAVE-ROW-CHANGES */
            IF THIS-OBJECT:AssignSkipDictionary:ContainsKey (cTableName) THEN
                phBeforeBuffer:SAVE-ROW-CHANGES (1, THIS-OBJECT:AssignSkipDictionary:GetValue (cTableName)) .
            ELSE
                IF THIS-OBJECT:AssignSkipList > "":U THEN
                    phBeforeBuffer:SAVE-ROW-CHANGES (1, THIS-OBJECT:AssignSkipList) .
            ELSE
                phBeforeBuffer:SAVE-ROW-CHANGES () .
        END.

        CATCH err AS Progress.Lang.SysError:

            IF err:GetMessageNum(1) = 11913 THEN
                THIS-OBJECT:LogCurrentRowModifiedIssue (phBeforeBuffer) .

            UNDO, THROW err .

        END CATCH.

    END METHOD.

    /**
     * Purpose: Sets's a ProDataset Buffer Callback-Method (BEFORE-FILL, AFTER-FILL, BEFORE-ROW-FILL, ...)
     * Notes:
     * @param phBuffer The ProDataset Buffer Handle to add the callback for
     * @param pcCallback The DatasetBufferEventEnum value to add the callback method for
     * @param pcMethod The name of the Method to register as a callback
     * @param poObject The Object that contains the callback method
     */
    METHOD PROTECTED VOID SetDatasetBufferCallback (phBuffer AS HANDLE,
                                                    pcCallback AS CHARACTER,
                                                    pcMethod AS CHARACTER,
                                                    poObject AS Progress.Lang.Object):

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBuffer ""BUFFER"":U} .

        IF NOT DatasetBufferCallbackEnum:IsValid (pcCallback) THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcCallback":U,
                                                            pcCallback,
                                                            "Consultingwerk.OERA.DataAccess":U) .

        phBuffer:SET-CALLBACK (pcCallback, pcMethod, poObject) .

    END METHOD.

    /**
     * Purpose: Sets's a ProDataset Buffer Callback-Method (BEFORE-FILL, AFTER-FILL, BEFORE-ROW-FILL, ...)
     * Notes:
     * @param phBuffer The ProDataset Buffer Handle to add the callback for
     * @param pcCallback The DatasetBufferEventEnum value to add the callback method for
     * @param pcMethod The name of the Method to register as a callback
     */
    METHOD PROTECTED VOID SetDatasetBufferCallback (phBuffer AS HANDLE,
                                                    pcCallback AS CHARACTER,
                                                    pcMethod AS CHARACTER):

        THIS-OBJECT:SetDatasetBufferCallback (phBuffer,
                                              pcCallback,
                                              pcMethod,
                                              THIS-OBJECT) .

    END METHOD.

    /**
     * Purpose: Sets's a ProDataset Callback-Method (BEFORE-FILL, AFTER-FILL)
     * Notes:
     * @param pcCallback The DatasetEventEnum value to add the callback method for
     * @param pcMethod The name of the Method to register as a callback
     * @param poObject The Object that contains the callback method
     */
    METHOD PROTECTED VOID SetDatasetCallback (pcCallback AS CHARACTER,
                                              pcMethod AS CHARACTER,
                                              poObject AS Progress.Lang.Object):

        IF NOT DatasetCallbackEnum:IsValid (pcCallback) THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcCallback":U,
                                                            pcCallback,
                                                            "Consultingwerk.OERA.DataAccess":U) .

        THIS-OBJECT:DatasetHandle:SET-CALLBACK (pcCallback, pcMethod, poObject) .

    END METHOD.

    /**
     * Purpose: Sets's a ProDataset Callback-Method (BEFORE-FILL, AFTER-FILL)
     * Notes:
     * @param pcCallback The DatasetEventEnum value to add the callback method for
     * @param pcMethod The name of the Method to register as a callback
     */
    METHOD PROTECTED VOID SetDatasetCallback (pcCallback AS CHARACTER,
                                              pcMethod AS CHARACTER):

        THIS-OBJECT:SetDatasetCallback (pcCallback,
                                        pcMethod,
                                        THIS-OBJECT) .

    END METHOD.

    /**
     * Purpose: Returns the Query Row Identifier (ROWID) to be used when navigating
     *          that DATA-SOURCE query based on the Skip argument of the FetchDataRequest
     * Notes:   Typically used when paging from a JSDO/Kendo UI client, SCL-819, SCL-822
     * @param piSkipRecords The number of records to skip from the beginning
     * @param phBuffer The temp-table buffer who's data-source should be navigated using Skip
     * @return The IQueryRowIdentifier identifying the restart rowid
     */
    METHOD PROTECTED IQueryRowIdentifier SkipRecords (piSkipRecords AS INTEGER,
                                                      phBuffer AS HANDLE):

        DEFINE VARIABLE hQuery   AS HANDLE              NO-UNDO.
        DEFINE VARIABLE roRowids AS IQueryRowIdentifier NO-UNDO .

        ASSIGN hQuery = phBuffer:DATA-SOURCE:QUERY .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i hQuery """Data Source Query"":U"} .

        hQuery:QUERY-OPEN () .
        hQuery:REPOSITION-TO-ROW (piSkipRecords + 1) .

        IF hQuery:QUERY-OFF-END THEN
            UNDO, THROW NEW QueryOffEndException ("Attempt to navigate behind the end of the query."{&TRAN},
                                                  0,
                                                  hQuery:PREPARE-STRING,
                                                  piSkipRecords) .

        hQuery:GET-NEXT () .

        roRowids = THIS-OBJECT:NewQueryRowIdentifier () .

        roRowids:GetQueryCurrentRowids (hQuery) .

        RETURN roRowids .

    END METHOD .

    /**
     * Purpose: Returns the database field name matching a temp-table field name
     *          contained in a consumers query string (query string vs. temp-table
     *          definition)
     * Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of
     *          DataAccess:FetchData () query preparation
     *          TO-DO: Provide code for alternative mapping
     * @param pcTable The temp table name to return the source (database table) field name for
     * @param pcColumn The temp table column name to return the source (database table) field name for
     * @return The database field name mapped to the passed in temp-table field
     */
    METHOD PUBLIC ABSTRACT CHARACTER SourceColumn (pcTable AS CHARACTER,
                                                   pcColumn AS CHARACTER).

    /**
     * Purpose: Returns the base query string used to retrieve data for the temp
     *          tables. This query string will be appended by the query provided by
     *          the consumer (FetchDataRequest object).
     * Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of
     *          DataAccess:FetchData () query preparation
     *          TO-DO: Provide code to return the DATA-SOURCE root query string
     * @param pcTable The temp table name to return the source default (database) query string for
     * @return The base query string used to retrieve data for the given temp-table
     */
    METHOD PUBLIC ABSTRACT CHARACTER SourceDefaultQuery (pcTable AS CHARACTER).

    /**
     * Purpose: Method allows enforcing specific indexes when populating child record
     *          of a data-relation. When the parent buffer and the child buffer are
     *          FILL'ed during the ProDataset:FILL() operation, we ensure the given index
     * Notes:   Allows to avoid situations where in a Order -> OrderLine data-relation
     *          when filtered by Orderline.ItemNum the itemnum index is choosen instead
     *          of the more efficient orderline index (both indexes provide one equality
     *          match, but itemnum is the alphabetically first index)
     *          This method should be called from the dataset's BEFORE-FILL call back
     *          Only processes the first buffer of the data-source query, as we assume,
     *          that the proper join criteria between the data-source query buffers
     *          is specified in the SourceDefaultQuery
     * @param phChildBuffer The handle of the child buffer
     * @param pcIndexName The index name to use
     */
    METHOD PROTECTED VOID SuggestDataSourceIndex (phChildBuffer AS HANDLE,
                                                  pcIndexName AS CHARACTER):

        DEFINE VARIABLE hParentBuffer AS HANDLE    NO-UNDO .
        DEFINE VARIABLE hQuery        AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cQueryString  AS CHARACTER NO-UNDO .

        BufferAssert:IsDatasetBuffer (phChildBuffer) .
        HandleAssert:ValidHandle (phChildBuffer:PARENT-RELATION:PARENT-BUFFER, "phChildBuffer:PARENT-RELATION:PARENT-BUFFER":U) .

        ASSIGN hParentBuffer = phChildBuffer:PARENT-RELATION:PARENT-BUFFER
               hQuery        = phChildBuffer:DATA-SOURCE:QUERY .

        IF hParentBuffer:FILL-MODE = "NO-FILL":U OR phChildBuffer:FILL-MODE = "NO-FILL":U THEN
            RETURN .

        IF hQuery:INDEX-INFORMATION (1) = pcIndexName THEN
            RETURN .

        ASSIGN cQueryString = QueryHelper:SetUseIndex (hQuery:PREPARE-STRING, 1, pcIndexName) .

        hQuery:QUERY-PREPARE (cQueryString) .

    END METHOD .

END CLASS.
