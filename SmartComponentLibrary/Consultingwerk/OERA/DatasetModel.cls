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
    File        : DatasetModel
    Purpose     : Model base class for a Dataset
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jun 15 01:42:38 CEST 2013
    Notes       : When using DatasetModel classes from within the actual
                  Business Entity itself, developers are supposed to set
                  the UseDedicatedBusinessEntity property to TRUE.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*           FROM PROPATH .
USING Consultingwerk.Assertion.* FROM PROPATH .
USING Consultingwerk.OERA.*      FROM PROPATH .
USING Consultingwerk.Util.*      FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.OERA.DatasetModel USE-WIDGET-POOL ABSTRACT:

    /**
     * Purpose: Gets/Sets the name of the AppServer Partition to be used
     *          by this DatasetModel
     * Notes:   Only relevant when running on the client side (no databases connected)
     */
    DEFINE PUBLIC PROPERTY AppServerPartition AS CHARACTER NO-UNDO INITIAL "":U
    GET:
        RETURN THIS-OBJECT:Performer:AppServerPartition .
    END GET .
    SET (arg AS CHARACTER):
        THIS-OBJECT:Performer:AppServerPartition = arg .
    END SET .

    /**
     * Purpose: Gets/Sets the BatchSize used when reading data from the Business
     *          Entity.
     * Notes:   Set to 0 to fetch all data and disable Batching
     */
    DEFINE PUBLIC PROPERTY BatchSize AS INTEGER INITIAL 100 NO-UNDO
    GET:
        RETURN THIS-OBJECT:Performer:BatchSize .
    END GET .
    SET (arg AS INTEGER):
        THIS-OBJECT:Performer:BatchSize = arg .
    END SET .

    /**
     * Purpose: Gets/Sets the reference to an optionally used Context Dataset
     * Notes:   The Context Dataset provides an simple way to exchange custom context
     *          between the client and the server (user interface to business logic)
     *          This property is only respected, when the Service Adapter is used for
     *          communicating with the Business Entity
     */
    DEFINE PUBLIC PROPERTY ContextDataset AS HANDLE NO-UNDO
    GET:
        RETURN THIS-OBJECT:Performer:ContextDataset .
    END GET .
    SET (arg AS HANDLE):
        THIS-OBJECT:Performer:ContextDataset = arg .
    END SET .

    /**
     * Purpose: Returns a reference to the primary Dataset owned by the DatasetController
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO
    GET.
    PRIVATE SET .

    /**
     * Purpose: Name of the BusinessEntity.
     * Notes:   This Property needs to be set by actual Dataset model implementation
     */
    DEFINE PUBLIC ABSTRACT PROPERTY EntityName AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Returns logical value indicating if the Dataset has changes
     * Notes:   Based on DatasetHelper:HasChanges
     */
    DEFINE PUBLIC PROPERTY HasChanges AS LOGICAL NO-UNDO
    GET:
        RETURN DatasetHelper:HasChanges (THIS-OBJECT:DatasetHandle) .
    END GET.

    /**
     * Purpose: Returns is a next batch of records is available
     * Notes:
     */
    DEFINE PUBLIC PROPERTY NextBatchAvailable AS LOGICAL INITIAL FALSE NO-UNDO
    GET:
        RETURN THIS-OBJECT:Performer:NextBatchAvailable .
    END GET .

    /**
     * Purpose: Returns the reference to the DatasetModelPerformer instance of this
     *          dataset model
     * Notes:   The DatasetModelPerformer performs the actual communication with the
     *          Business Entity
    */
    DEFINE PUBLIC PROPERTY Performer AS DatasetModelPerformer NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Gets and sets the STOP-AFTER value used when reading data from the
     *          Business Entity.
     * Notes:   Set to 0 to disable STOP-AFTER
     */
    DEFINE PUBLIC PROPERTY StopAfter AS INTEGER INITIAL 0 NO-UNDO
    GET:
        RETURN THIS-OBJECT:Performer:StopAfter .
    END GET.
    SET (arg AS INTEGER):
        ASSIGN THIS-OBJECT:Performer:StopAfter = arg .
    END SET .

    /**
     * Purpose: Returns the List of TableModels
     * Notes:
     */
    DEFINE PUBLIC PROPERTY TableModels AS ListTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Gets and sets if the DatasetModel is communicating with a dedicated
     *          Business Entity instance
     * Notes:   Required, when using the DatasetModel from within the actual Business
     *          Entity itself. As otherwise the read operations done through the
     *          DatasetModel instance to the Business Entity would clear the dataset
     *          within the Business Entity. Only applicable, when UseInterface =
     *          ServiceInterface
     */
    DEFINE PUBLIC PROPERTY UseDedicatedBusinessEntity AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:Performer:UseDedicatedBusinessEntity .
    END GET.
    SET (arg AS LOGICAL):
        ASSIGN THIS-OBJECT:Performer:UseDedicatedBusinessEntity = arg .
    END SET.

    /**
     *   Purpose: Gets and sets the Interface this DatasetModel uses for Business
     *            Entity Requests
     *   Notes:   Default depends on NUM-DBS > 0, then ServiceInterface else ServiceAdapter
     */
    DEFINE PUBLIC PROPERTY UseInterface AS UseInterfaceEnum NO-UNDO
    GET:
        RETURN THIS-OBJECT:Performer:UseInterface .
    END GET .
    SET (arg AS UseInterfaceEnum):
        ASSIGN THIS-OBJECT:Performer:UseInterface = arg .
    END.

    /**
     * Purpose: Gets and sets if the Dataset Model's ProDataset is TRACKING-CHANGES
     * Notes:
     */
    DEFINE PUBLIC PROPERTY TrackingChanges AS LOGICAL NO-UNDO
    GET:
        RETURN DatasetHelper:GetTrackingChanges (THIS-OBJECT:DatasetHandle) .
    END GET .
    SET (arg AS LOGICAL):
        DatasetHelper:SetTrackingChanges (THIS-OBJECT:DatasetHandle,
                                          arg) .
    END SET.

    /**
     * Purpose: Constructor for the DatasetModel class
     * Notes:
     * @param phDataset The handle of the ProDataset from the concrete model class
     */
    CONSTRUCTOR PUBLIC DatasetModel (phDataset AS HANDLE):
        SUPER ().

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDataset WidgetTypeEnum:Dataset} .

        ASSIGN THIS-OBJECT:Performer     = THIS-OBJECT:NewDatasetModelPerformer (phDataset,
                                                                                 THIS-OBJECT:EntityName)
               THIS-OBJECT:DatasetHandle = phDataset
               THIS-OBJECT:TableModels   = NEW ListTableModel () .

        IF NUM-DBS > 0 THEN
            THIS-OBJECT:UseInterface = UseInterfaceEnum:ServiceInterface .
        ELSE
            THIS-OBJECT:UseInterface = UseInterfaceEnum:ServiceAdapter .

    END CONSTRUCTOR.

    /**
     * Purpose: Returns the Dataset to the caller
     * Notes:
     * @param hDataset The Dataset to be returned to the caller
     */
    METHOD PUBLIC VOID GetDataset (OUTPUT DATASET-HANDLE hDataset BIND):

        ASSIGN hDataset = THIS-OBJECT:DatasetHandle .

    END METHOD .

    /**
     * Purpose: Overridable factory method for the DatasetModelPerformer instance
     *          used by the DatasetModel instance
     * Notes:   Allows customers, to return instances of custom DatasetModelPerformer
     *          classes
     * @param phDataset The handle of the ProDataset from the model class
     * @param pcEntityName The name of the Business Entity
     * @return The reference to the new DatasetModelPerformer instance
     */
    METHOD PROTECTED DatasetModelPerformer NewDatasetModelPerformer (phDataset AS HANDLE,
                                                                     pcEntityName AS CHARACTER):

        RETURN NEW DatasetModelPerformer (phDataset, pcEntityName) .

    END METHOD.

    /**
     * Purpose: Opens the queries of all table models
     * Notes:
     */
    METHOD PROTECTED VOID OpenTableModelQueries ():

        {Consultingwerk/foreachABL.i TableModel oTableModel in this-object:TableModels}

            IF NOT VALID-HANDLE (oTableModel:QueryHandle) THEN
                oTableModel:UseDatasetQuery () .

            oTableModel:QueryHandle:QUERY-OPEN () .
            oTableModel:QueryHandle:GET-FIRST () .
        END.

    END METHOD .

    /**
     * Purpose: Retrieves data from the backend
     * Notes:
     * @param pcTables The comma delimited list of the Table Names to retrieve
     * @param poQueryExpression The query expression to use for the first table
     */
    METHOD PUBLIC VOID RetrieveData (pcTables AS CHARACTER,
                                     poQueryExpression AS IQueryExpression):

        THIS-OBJECT:Performer:RetrieveData (pcTables, poQueryExpression) .

        THIS-OBJECT:OpenTableModelQueries () .

    END METHOD .

    /**
     * Purpose: Retrieves data from the backend
     * Notes:
     * @param pcTables The comma delimited list of the Table Names to retrieve
     * @param pcQueryString The query string to use for the first table
     * @return The reference to the DatasetModel instance itself
     */
    METHOD PUBLIC DatasetModel RetrieveData (pcTables AS CHARACTER,
                                             pcQueryString AS CHARACTER):

        DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cQueryBuffer AS CHARACTER NO-UNDO .

        ASSIGN cQueryBuffer = TRIM (ENTRY (1, pcTables)) .

        IF pcQueryString = "":U THEN
            ASSIGN cQueryString = SUBSTITUTE ("FOR EACH &1":U,
                                              cQueryBuffer) .
        ELSE IF pcQueryString BEGINS "WHERE ":U THEN
            ASSIGN cQueryString = SUBSTITUTE ("FOR EACH &1 &2":U,
                                              cQueryBuffer,
                                              pcQueryString) .
        ELSE IF pcQueryString BEGINS "FOR ":U THEN
            ASSIGN cQueryString = pcQueryString .
        ELSE
            ASSIGN cQueryString = SUBSTITUTE ("FOR EACH &1 WHERE &2":U,
                                              cQueryBuffer,
                                              pcQueryString) .

        THIS-OBJECT:Performer:RetrieveData (pcTables, cQueryString) .

        THIS-OBJECT:OpenTableModelQueries () .

        RETURN THIS-OBJECT .

    END METHOD .

    /**
     * Purpose: Retrieves data from the backend
     * Notes:
     * @param poNamedQuery The INamedQueryParameter that defines a named query
     * @return The reference to the DatasetModel instance itself
     */
    METHOD PUBLIC DatasetModel RetrieveData (poNamedQuery AS INamedQueryParameter):

        THIS-OBJECT:Performer:RetrieveData (poNamedQuery) .

        THIS-OBJECT:OpenTableModelQueries () .

        RETURN THIS-OBJECT .

    END METHOD .

    /**
     * Purpose: Retrieves data from the backend
     * Notes:
     * @param pcTables The comma delimited list of the Table Names to retrieve
     * @param poNamedQuery The INamedQueryParameter that defines a named query
     * @return The reference to the DatasetModel instance itself
     */
    METHOD PUBLIC DatasetModel RetrieveData (pcTables AS CHARACTER,
                                             poNamedQuery AS INamedQueryParameter):

        THIS-OBJECT:Performer:RetrieveData (pcTables, poNamedQuery) .

        THIS-OBJECT:OpenTableModelQueries () .

        RETURN THIS-OBJECT .

    END METHOD .

    /**
     * Purpose: Performs Save Changes request either using the Service Adapter or the
     *          Service Interface
     * Notes:
     * @return The reference to the DatasetModel instance itself
     */
    METHOD PUBLIC DatasetModel SaveChanges ():

        THIS-OBJECT:Performer:SaveChanges () .

        RETURN THIS-OBJECT .

    END METHOD .

    /**
     * Purpose: Ensures that all TableModels are using one of the ProDataset QUERY's
     * Notes:   Invokes UseDatasetQuery() in all TableModels
     * @return The reference to the DatasetModel instance itself
     */
    METHOD PUBLIC DatasetModel UseDatasetQueries ():

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        {Consultingwerk/foreachABL.i TableModel oTableModel in THIS-OBJECT:TableModels}
            oTableModel:UseDatasetQuery () .
        END.

        THIS-OBJECT:DatasetHandle:RELATIONS-ACTIVE = TRUE .

        /* Open all TOP-NAV-QUERY's for convenience */
        DO i =  1 TO THIS-OBJECT:DatasetHandle:NUM-TOP-BUFFERS:
            THIS-OBJECT:DatasetHandle:TOP-NAV-QUERY (i):QUERY-OPEN () .
        END.

        RETURN THIS-OBJECT .

    END METHOD .

    /**
     * Purpose: Destructor for the DatasetModel class
     * Notes:
     */
    DESTRUCTOR PUBLIC DatasetModel ():

        {Consultingwerk/foreachABL.i TableModel oTableModel in THIS-OBJECT:TableModels}
            GarbageCollectorHelper:DeleteObject (oTableModel) .
        END.

        GarbageCollectorHelper:DeleteObject (THIS-OBJECT:Performer) .

    END DESTRUCTOR .

END CLASS.
