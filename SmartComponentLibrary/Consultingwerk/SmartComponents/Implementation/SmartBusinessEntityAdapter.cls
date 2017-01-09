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
    File        : SmartBusinessEntityAdapter
    Purpose     : Provides navigation and update capabilities for tables
                  of a Business Entity ProDataset. Manages the Query and
                  ProBindingSource at runtime and provides design time
                  support using a custom TypeDescriptor
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories, Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Apr 06 10:34:27 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                                     FROM PROPATH .
USING Consultingwerk.Assertion.*                                           FROM PROPATH .
USING Consultingwerk.Exceptions.*                                          FROM PROPATH .
USING Consultingwerk.Framework.*                                           FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                               FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                                      FROM PROPATH.
USING Consultingwerk.SmartComponents.Base.*                                FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*                                FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*                      FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*                          FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.Design.IDesignCommitSource FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Resources.*                           FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*                             FROM PROPATH .
USING Consultingwerk.OERA.*                                                FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.*                       FROM PROPATH .
USING Consultingwerk.Util.*                                                FROM PROPATH .
USING Progress.Data.*                                                      FROM ASSEMBLY .
USING Progress.Lang.*                                                      FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter
    INHERITS SmartDatasetAdapter
    IMPLEMENTS ISupportsScrollup, ISmartCommitTarget, ISmartBusinessEntityAdapter,
               ISupportsFetchAll
    USE-WIDGET-POOL:

    DEFINE PRIVATE VARIABLE hDataset              AS HANDLE    NO-UNDO .
    DEFINE PRIVATE VARIABLE cQueryString          AS CHARACTER NO-UNDO .

    DEFINE PRIVATE VARIABLE lSaving               AS LOGICAL   NO-UNDO INIT FALSE .

    DEFINE PRIVATE VARIABLE cBindTo               AS CHARACTER INITIAL "QUERY":U NO-UNDO.

    DEFINE PRIVATE VARIABLE lRegistered           AS LOGICAL   NO-UNDO INIT FALSE .

    DEFINE PRIVATE VARIABLE lEndUpdateStateDSPending AS LOGICAL NO-UNDO INIT FALSE .

    /**
     * Purpose: Raised after an AutoCommit=TRUE transaction has been Commited
     * Notes:
     */
    DEFINE PUBLIC EVENT AfterAutoCommitTransaction DELEGATE System.EventHandler .

    /**
     * Purpose: Event fired whenever the SmartDataAdapter has received a new batch
     *          of data
     * Notes:   This event will be raised also with the initial batch of data (when
     *          the AfterReviewData event is raised as well)
     */
    DEFINE PUBLIC EVENT AfterBatchAvailable DELEGATE System.EventHandler.

    /**
     * Purpose: Raised before the SmartBusinessEntityAdapter gets the Changes of the ProDataset
     * Notes:   The AfterGetChanges Event allows manipulation of the Changes dataset
     *          on the client after it has been populated using the GET-CHANGES method
     * @param sender The reference to the object that raised the event
     * @param e The AfterGetChangesEventArgs with the data for this event
     */
    DEFINE PUBLIC EVENT AfterGetChanges SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                        e AS AfterGetChangesEventArgs).

    /**
     * Purpose: Raised when the AutoCommit property is changed
     * Notes:   Required for ISmartCommitTarget
     */
    DEFINE PUBLIC EVENT AutoCommitChanged DELEGATE System.EventHandler.

    /**
     * Purpose: Raised after an AutoCommit=FALSE transaction has been Commited
     * Notes:
     */
    DEFINE PUBLIC EVENT AfterCommitTransaction DELEGATE System.EventHandler .

    /**
     * Purpose: Raised after the processed changes from the backend are merged with
     *          the local data
     * Notes:
     * @param sender The object that raised the AfterMergeChanges event
     * @param e The MergeChangesEventArgs with the data for the event
     */
    DEFINE PUBLIC EVENT AfterMergeChanges SIGNATURE VOID (sender AS System.Object,
                                                          e AS MergeChangesEventArgs).

    /**
     * Purpose: Raised after a transaction has been Rejected
     * Notes:
     */
    DEFINE PUBLIC EVENT AfterRejectTransaction DELEGATE System.EventHandler .

    /**
     * Purpose: Raised before a transaction will be commited
     * Notes:
     */
    DEFINE PUBLIC EVENT BeforeCommitTransaction DELEGATE System.ComponentModel.CancelEventHandler .

    /**
     * Purpose: Raised before the SmartBusinessEntityAdapter gets the Changes of the ProDataset
     * Notes:   The BeforeGetChanges Event allows manipulation of the Data on the client
     *          prior to calling the GET-CHANGES method on the Dataset
     * @param sender The reference to the object that raised the event
     * @param e The BeforeGetChangesEventArgs with the data for this event
     */
    DEFINE PUBLIC EVENT BeforeGetChanges SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                         e AS BeforeGetChangesEventArgs).

    /**
     * Purpose: Raised before the processed changes from the backend are merged with
     *          the local data
     * Notes:
     * @param sender The object that raised the BeforeMergeChanges event
     * @param e The MergeChangesEventArgs with the data for the event
     */
    DEFINE PUBLIC EVENT BeforeMergeChanges SIGNATURE VOID (sender AS System.Object,
                                                           e AS MergeChangesEventArgs).

    /**
     * Purpose: Raised before a transaction will be rejected
     * Notes:
     */
    DEFINE PUBLIC EVENT BeforeRejectTransaction DELEGATE System.ComponentModel.CancelEventHandler .

    /**
     * Purpose: Raised after an AutoCommit=FALSE SmartBusinessEntityAdapter has submitted
     *          changes to the backend.
     * Notes:   Allows developers to influence the record to which the
     *          SmartBusinessEntityAdapter and linked SmartDataBrowsers should be located
     *          after the commit operation
     * @param sender The object that raised the FindCurrentRecordAfterCommitChanges event
     * @param e The BufferHandleEventArgs with the data for the event
     */
    DEFINE PUBLIC EVENT FindCurrentRecordAfterCommitChanges SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                            e AS BufferHandleEventArgs).

    /**
     * Purpose: Raised when CommitTransaction caused a validation error and the
     *          SmartBusinessEntityAdapter needs to locate the SmartDatasetChildAdapter
     *          to handle the issue
     * Notes:   Allows reopening a child-form that may be required because it contains
     *          the SmartDatasetChildAdapter
     * @param sender The object that raised the FindDataAdapterForCommitErrorHandling event
     * @param e The FindDataAdapterForCommitErrorHandlingEventArgs with the data for the event
     */
    DEFINE PUBLIC EVENT FindDataAdapterForCommitErrorHandling SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                              e AS FindDataAdapterForCommitErrorHandlingEventArgs).

    /**
     * Purpose: Raised when CommitTransaction caused a validation error and the
     *          SmartBusinessEntityAdapter needs to locate the updating SmartTableIOTarget
     *          to handle the issue
     * Notes:   Allows reopening a child-form that may be required because it contains
     *          the SmartDatasetChildAdapter
     * @param sender The object that raised the FindUpdatingDataTargetForCommitErrorHandling event
     * @param e The FindUpdatingDataTargetForCommitErrorHandlingEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT FindUpdatingTableIOTargetForCommitErrorHandling SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                                        e AS FindUpdatingTableIOTargetForCommitErrorHandlingEventArgs).

    /**
     * Purpose: Raised to allow subscribers to handle validations errors caused by
     *          a non AutoCommit transaction commit
     * Notes:   Set the e:Handled = TRUE to avoid error handling by the SmartBusinessEntityAdpater
     *          instance
     * @param sender The object that raised the HandleValidationError event
     * @param e The HandleValidationErrorEventArgs with the data for the event
     */
    DEFINE PUBLIC EVENT HandleValidationError SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                              e AS HandleValidationErrorEventArgs).

    /**
     * Purpose: Raised before an actual call to the Service Adapter is made to read
     *          data from the backend
     * Notes:   Allows modification of the FetchDataRequest object
     * @param sender The object that raised the NewFetchDataRequest event
     * @param e The NewFetchDataRequestEventArgs with the data for the event
     */
    DEFINE PUBLIC EVENT NewFetchDataRequest SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                            e AS NewFetchDataRequestEventArgs).

    /**
     * Purpose: Event fired whenever the SmartCommitSource property has
     *          changed
     * Notes:
     */
    DEFINE PUBLIC EVENT SmartCommitSourceChanged DELEGATE System.EventHandler.

    /**
     * Purpose: Event fired whenever the SmartTransactionState property has
     *          changed
     * Notes:
     */
    DEFINE PUBLIC EVENT SmartTransactionStateChanged DELEGATE System.EventHandler.

    /**
     * Purpose: Controls the AutoCommit functionality of the SmartBusinessEntityAdapter
     * Notes:
     */
    DEFINE PUBLIC PROPERTY AutoCommit AS LOGICAL NO-UNDO INIT TRUE
    GET.
    SET (arg AS LOGICAL):

        THIS-OBJECT:AutoCommit = arg .

        IF THIS-OBJECT:DesignTime THEN
            RETURN .

        OnAutoCommitChanged (System.EventArgs:Empty) .

        IF THIS-OBJECT:AutoCommit = TRUE THEN
            THIS-OBJECT:SmartTransactionState = TransactionStateEnum:NoManualCommit .
        ELSE
            THIS-OBJECT:SmartTransactionState = TransactionStateEnum:NoPendingChanges .

    END SET.

    /**
     * Purpose: Gets/Sets the BatchSize used when reading data from the Business
     *          Entity.
     * Notes:   Batching is used for the EntityTable, not the view tables!
     *          Set to 0 to fetch all data and disable Batching
     *          The default value for this property may be set using the DefaultBatchSize
     *          preprocessor variable in Consultingwerk/products.i
     */
    DEFINE PUBLIC PROPERTY BatchSize AS INTEGER INITIAL &IF DEFINED (DefaultBatchSize) NE 0 &THEN {&DefaultBatchSize} &ELSE 0 &ENDIF NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Binding the BusinessEntityBindingSource to a Query or a Dataset.
     *          This Property is set by the Developer during DesignTime.
     * Notes:
     */
    DEFINE PUBLIC OVERRIDE PROPERTY BindTo AS CHARACTER NO-UNDO INIT "QUERY":U
    GET():
        RETURN cBindTo.
    END GET.
    SET(arg AS CHARACTER):
        CASE arg:
            WHEN "QUERY":U OR
            WHEN "DATASET":U THEN
                ASSIGN cBindTo = arg.
            OTHERWISE
                UNDO, THROW NEW AppError ("Illegal value for property BindTo, possible values QUERY / DATASET."{&TRAN}, 0).
        END CASE .
    END SET.

    /**
     * Purpose: Gets/Sets the reference to an optionally used Context Dataset
     * Notes:   The Context Dataset provides an simple way to exchange custom context
     *          between the client and the server (user interface to business logic)
     */
    DEFINE PUBLIC PROPERTY ContextDataset AS HANDLE NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets/Sets the ContextID to recognize the instance of the
     *          Business Entity on the Backend
     * Notes:   Allows multiple instances of the Business Entities on the
     *          backend, see "Distinguishing between different Business Service
     *          instances using an instance key" at
     *          http://confluence.consultingwerkcloud.com/wiki/display/SCL/Business+Service+Names
     */
    DEFINE PUBLIC PROPERTY ContextID AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /**
     * Purpose: Gets/Sets the ContextString used for the communication with the
     *          Business Entity.
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ContextString AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /**
     * Purpose: Gets/Sets the CustomContext used for the communication with the
     *          Business Entity.
     * Notes:   While the ContextString is part of the Business Entity context (and
     *          thus mixing the Business Entity internal context with sort of custom
     *          context the property CustomContext is 100% reserved for the use
     *          in your own Business Entities / Data Access Objects
     */
    DEFINE PUBLIC PROPERTY CustomContext AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /**
     * Purpose: Returns the handle to the ProDataset instance managed by the
     *          SmartBusinessEntityAdapter
     * Notes:
     */
    DEFINE PUBLIC OVERRIDE PROPERTY DataSet AS HANDLE NO-UNDO
    GET():
        /* Mike Fechner, Consultingwerk Ltd. 03.12.2009
           Implementation moved to method to allow overriding in
           ticBusinessEntityAdapter class */
        IF NOT VALID-HANDLE (hDataset) THEN
            ASSIGN hDataset = GetEmptyDataset() .

        RETURN hDataset .
    END GET.
    PROTECTED SET (arg AS HANDLE):
        ASSIGN hDataset = arg .
    END.

    /**
     * Purpose: A Reference to the DatasetController that is is owning the Dataset
     *          on which this SmartBusinessEntityAdapter is operating.
     * Notes:   The property can only be set at long as this
     *          SmartBusinessEntityAdapter does not already have a valid dataset
     *          handle (DatasetHandle property)
     */
    DEFINE PUBLIC OVERRIDE PROPERTY DatasetController AS IDatasetController NO-UNDO
    GET.
    SET (arg AS IDatasetController):
        DEFINE VARIABLE lChanged AS LOGICAL NO-UNDO INIT FALSE .

       IF VALID-HANDLE (hDataset) THEN
           UNDO, THROW NEW AppError ("The DatasetController can only be set before the DatasetHandle is valid."{&TRAN}, 0) .

       IF VALID-OBJECT (arg) AND NOT VALID-HANDLE (arg:DatasetHandle) THEN
           UNDO, THROW NEW AppError ("Unable to use DatasetController without a valid DatasetHandle"{&TRAN}, 0) .

        ASSIGN lChanged = (THIS-OBJECT:DatasetController <> arg) .

       THIS-OBJECT:DatasetController = arg .

       IF VALID-OBJECT (THIS-OBJECT:DatasetController) THEN DO:
           THIS-OBJECT:Dataset = THIS-OBJECT:DatasetController:DatasetHandle .

           IF NOT lRegistered THEN DO:
               IF THIS-OBJECT:Initializing = FALSE THEN DO:
                   THIS-OBJECT:DatasetController:RegisterConsumer (THIS-OBJECT) .
                   ASSIGN lRegistered = TRUE .
               END.

               {Consultingwerk/foreachABL.i ISmartDataTarget oTarget in this-object:SmartDataTargets}

                   IF TYPE-OF (oTarget, SmartDatasetChildAdapter) THEN
                       THIS-OBJECT:DatasetController:RegisterConsumer (oTarget) .
               END.
           END.
       END.

        IF lChanged THEN
            OnDatasetControllerChanged (System.EventArgs:Empty) .
    END.

    /**
     * Purpose: The typename of the DatasetController that is is owning the Dataset
     *          on which this SmartBusinessEntityAdapter is operating.
     * Notes:   The property can only be set at long as this
     *          SmartBusinessEntityAdapter is not already connected to a
     *          DatasetController.
     */
    DEFINE PUBLIC PROPERTY DatasetControllerType AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET (arg AS CHARACTER):
        DEFINE VARIABLE oClass AS Progress.Lang.Class NO-UNDO .

        IF arg > "":U THEN DO:
            /* Test validity of TypeName */
            oClass = Progress.Lang.Class:GetClass (arg) .

            IF NOT oClass:IsA ("Consultingwerk.SmartComponents.Interfaces.IDatasetController":U) THEN
                UNDO, THROW NEW AppError ("DatasetControllerType needs to implement IDatasetController"{&TRAN}, 0).
        END.

        IF THIS-OBJECT:DesignTime THEN DO:
           THIS-OBJECT:DatasetControllerType = arg .
           RETURN .
        END.

        IF VALID-OBJECT (THIS-OBJECT:DatasetController) THEN
            UNDO, THROW NEW AppError ("The DatasetControllerType can only be set before the DatasetController is valid."{&TRAN}, 0) .

        ASSIGN THIS-OBJECT:DatasetControllerType = arg .

        /* Mike Fechner, Consultingwerk Ltd. 23.11.2012
           Instantiate the DatasetController at runtime here, so that it's
           launched before any other Control performs initialization during
           the EndInit calls */
        IF THIS-OBJECT:DatasetControllerType > "":U THEN
            LaunchDatasetController() .

        CATCH err AS Progress.Lang.SysError:
            /* Could not dynamically find class 'xyz'. (15287) */
            IF err:GetMessageNum(1) = 15287 THEN
                UNDO, THROW NEW AppError ("Invalid classname for DatasetControllerType"{&TRAN}, 0) .

             UNDO, THROW err .
        END CATCH.
    END SET .

    /**
     * Purpose: Controls if the SmartBusinessEntityAdapter instance empties the
     *          ProDataset at the beginning of a call to RetrieveData
     * Notes:   Set this to true to be able to aggregate data from multiple RetrieveData
     *          call (e.g. when the SmartBusinessEntityAdapter is used as the source
     *          for a tree view). In those cases it's expected that the BatchSize is
     *          set to 0. The use of a NavigationSource is also questionable in those
     *          cases, SCL-1099
     */
    DEFINE PUBLIC PROPERTY EmptyDatasetOnRetrieveData AS LOGICAL INITIAL TRUE NO-UNDO
    GET.
    SET.

    /**
     * Purpose: A List in the same order as EntityView which defines Join (YES or NO)
     *          for each Table of pcEntityView.
     *          This Property is set by the Developer during DesignTime.
     * Notes:
     */
    DEFINE PUBLIC OVERRIDE PROPERTY EntityJoin AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /**
     * Purpose: Name of the BusinessEntity.
     *          This Property is set by the Developer during DesignTime.
     * Notes:
     */
    DEFINE PUBLIC OVERRIDE PROPERTY EntityName AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET (arg AS CHARACTER):
        DEFINE VARIABLE hDataset  AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cPrevious AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lValid    AS LOGICAL   NO-UNDO.

        IF THIS-OBJECT:DesignTime AND
           NOT THIS-OBJECT:DesingerHostLoading AND
           NOT PROGRAM-NAME (2) MATCHES "*InitializeComponent*":U THEN
        DO ON ERROR UNDO, THROW:
            IF arg > "":U THEN DO ON ERROR UNDO, THROW:

                lValid = ValidateBusinessEntityName (arg) .

                IF NOT lValid THEN
                    UNDO, THROW NEW AppError (SUBSTITUTE ("The EntityName ~"&1~"entered is invalid."{&TRAN},
                                                          arg), 0) .

                ASSIGN cPrevious              = THIS-OBJECT:EntityName
                       /* need to set property so that Dataset can be returned */
                       THIS-OBJECT:EntityName = arg
                       hDataset = THIS-OBJECT:DataSet NO-ERROR .

                IF NOT VALID-HANDLE (hDataset) THEN DO:
                    THIS-OBJECT:EntityName = cPrevious .
                    UNDO, THROW NEW AppError (SUBSTITUTE ("Invalid Dataset for EntityName ~"&1~"."{&TRAN}, arg), 0) .
                END.

                FINALLY:
                    IF VALID-HANDLE (hDataset) THEN
                        DELETE OBJECT hDataset .
                END FINALLY.
            END.

            THIS-OBJECT:EntityName = arg .
        END.
        ELSE
            THIS-OBJECT:EntityName = arg .
    END SET .

    /**
     * Purpose: RootTable for this Instance of the BusinessEntity.
     *          This Property is set by the Developer during DesignTime.
     * Notes:
     */
    DEFINE PUBLIC OVERRIDE PROPERTY EntityTable AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET (arg AS CHARACTER):
        DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.
        DEFINE VARIABLE hBuffer  AS HANDLE NO-UNDO.

        IF THIS-OBJECT:DesignTime AND
           NOT THIS-OBJECT:DesingerHostLoading AND
           NOT PROGRAM-NAME (2) MATCHES "*InitializeComponent*":U AND
           arg > "":U THEN

        DO ON ERROR UNDO, THROW:
            IF THIS-OBJECT:EntityName > "":U THEN .
            ELSE
                UNDO, THROW NEW AppError ("Unable to set EntityTable before EntityName is set."{&TRAN}, 0) .

            ASSIGN hDataset = THIS-OBJECT:DataSet .

            IF VALID-HANDLE (hDataset) THEN DO:
                ASSIGN hBuffer = hDataset:GET-BUFFER-HANDLE (arg) NO-ERROR .

                IF NOT VALID-HANDLE (hBuffer) THEN
                    UNDO, THROW NEW AppError ("The entered table name is not valid."{&TRAN}, 0) .
            END.
            ELSE
                UNDO, THROW NEW AppError ("Unable to set EntityTable because Dataset is not valid."{&TRAN}, 0) .

            FINALLY:
                IF VALID-HANDLE (hDataset) THEN
                    DELETE OBJECT hDataset .
            END FINALLY.
        END.

        THIS-OBJECT:EntityTable = arg .
    END SET .

    /**
     * Purpose: List of Tables to read with the EntityTable.
     *          This Property is set by the Developer during DesignTime.
     * Notes:
     */
    DEFINE PUBLIC OVERRIDE PROPERTY EntityView AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET (arg AS CHARACTER):
        DEFINE VARIABLE hDataset AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hBuffer  AS HANDLE  NO-UNDO.
        DEFINE VARIABLE cBuffer  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i        AS INTEGER NO-UNDO.

        IF THIS-OBJECT:DesignTime AND
           NOT THIS-OBJECT:DesingerHostLoading AND
           NOT PROGRAM-NAME (2) MATCHES "*InitializeComponent*":U AND
           arg > "":U THEN

        DO ON ERROR UNDO, THROW:
            IF THIS-OBJECT:EntityName > "":U THEN .
            ELSE
                UNDO, THROW NEW AppError ("Unable to set EntityView before EntityName is set."{&TRAN}, 0) .

            ASSIGN hDataset = THIS-OBJECT:DataSet .

            IF VALID-HANDLE (hDataset) THEN DO i = 1 TO NUM-ENTRIES (arg):
                ASSIGN cBuffer = ENTRY (i, arg)
                       hBuffer = hDataset:GET-BUFFER-HANDLE (cBuffer) NO-ERROR .

                IF NOT VALID-HANDLE (hBuffer) THEN
                    UNDO, THROW NEW AppError (SUBSTITUTE ("The entered table &1 name is not valid."{&TRAN}, cBuffer) , 0) .
            END.
            ELSE
                UNDO, THROW NEW AppError ("Unable to set EntityView because Dataset is not valid."{&TRAN}, 0) .

            FINALLY:
                IF VALID-HANDLE (hDataset) THEN
                    DELETE OBJECT hDataset .
            END FINALLY.
        END.

        THIS-OBJECT:EntityView = arg .
    END SET .

    /**
     * Purpose: Controls if the FetchLast method will fetch all remaining records
     *          (behaviour until July 2011) or directly fetch the last batch using
     *          the current batch size and the ability to perform backwards batching
     *          for retrieving the records before the last batch
     * Notes:   Default is FALSE, which performs the old behaviour (until July 2011
     *          the only behaviour)
     */
    DEFINE PUBLIC PROPERTY FetchLastOnServer AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Specifies a record to be repositioned from the server (FIND-FIRST)
     * Notes:
     */
    DEFINE PROTECTED PROPERTY FindString AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /**
     * Purpose: Represents the SmartCommitSource property casted to a .NET Interface
     * Notes:   Implementation of Interface in Consultingwerk.SmartComponents.dll
     */
    DEFINE PUBLIC PROPERTY LinkCommitSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignCommitSource NO-UNDO
    GET:
        IF TYPE-OF (THIS-OBJECT:SmartCommitSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignCommitSource) THEN
            RETURN CAST (THIS-OBJECT:SmartCommitSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignCommitSource) .
    END GET .
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignCommitSource):
        IF NOT VALID-OBJECT (arg) OR TYPE-OF (arg, Consultingwerk.SmartComponents.Interfaces.ISmartCommitSource) THEN
            ASSIGN THIS-OBJECT:SmartCommitSource = CAST (arg, Consultingwerk.SmartComponents.Interfaces.ISmartCommitSource) .
    END.

    /**
     * Purpose: Gets and sets the reference to the Named Query Parameter instance
     *          used for the next invokation of the RetrieveData method
     * Notes:   Named queries are a concept where the actual query criteria is
     *          constructed within the Business Entity or the Data Access class.
     *          The consumer simply passes in a query name and a list of arguments
     *          (similar to Dictionary of <PrimitiveHolder> classes). The Business
     *          Entity or the Data Access class will convert this into the actual
     *          query parameters. This moves the responsibility of handling the
     *          complexity when building complex query strings into the Business
     *          Entity or alternatively into the Data Access class - allowing the
     *          back end developer to also decide on optimal use of indexes etc.
     *          http://confluence.consultingwerkcloud.com/wiki/display/SCL/Named+Queries
     */
    DEFINE PUBLIC PROPERTY NamedQuery AS Consultingwerk.OERA.INamedQueryParameter NO-UNDO
    GET.
    SET.

    /**
        Purpose: The context for the next Fetch next call
        Notes:
     */
    DEFINE PROTECTED PROPERTY NextContext AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /**
     * Purpose: Specifies the number of records the a FindRowWhere
     *          (FindRowWhereModeEnum FindOnServer) should read from the backend
     * Notes:   This is helpful to signal in a browser etc. that the user can
     *          scroll up - beyond the found row.
     */
    DEFINE PUBLIC PROPERTY PrefetchOnFindRowWhere AS INTEGER NO-UNDO INIT 10
    GET.
    SET.

    /**
     * Purpose: The context for the next Fetch prev call
     * Notes:
     */
    DEFINE PROTECTED PROPERTY PrevContext AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /**
     * Purpose: Provides access to the last query used by the SmartBusinessEntityAdapter
     *          to retrieve data from the backend
     * Notes:   Allows customers to use this information in a derived SmartBusinessEntityAdpater
     *          class, e.g. to count the records returned by the previous query
     */
    DEFINE PROTECTED PROPERTY PreviousFetchQueries AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Allows to show a prompt when RetrieveData is called while an update
     *          is pending in the BusinessEntityAdapter instance
     * Notes:   Typically it's not required to prompt if RetrieveData should be
     *          executed - as the default UI functions (Browser navigation, Browser
     *          sort, Smart Filter) that would cause RetrieveData to be called are
     *          sensitive to a pending update state. This property is to support
     *          customers that call RetrieveData from custom code
     */
    DEFINE PUBLIC PROPERTY PromptWhenRetrieveDataInUpdateState AS LOGICAL NO-UNDO INITIAL FALSE
    GET.
    SET.

    /**
     * Purpose: This is a base query string.
     * Notes:
     */
    DEFINE PUBLIC OVERRIDE PROPERTY QueryString AS CHARACTER NO-UNDO INIT "":U
    GET:
        RETURN cQueryString .
    END.
    SET (arg AS CHARACTER):
        THIS-OBJECT:SetQueryString (arg) .
    END SET .

    /**
     * Purpose: This property allows to subspend the call to RetrieveData in
     *          the setter of QuerySort (SetQuerySort method)
     * Notes:   Defaults to TRUE
     */
    DEFINE PUBLIC PROPERTY RetrieveDataOnChangeSortOrder AS LOGICAL NO-UNDO INIT TRUE
    GET.
    SET.

    /**
     * Purpose: Gets and sets the referecence to an optional parameter object passed
     *          to the Business Entity during SaveChanges/SubmitChanges
     * Notes:   Requires the ServiceAdapter to implement the IServiceAdapterWithSubmitDataParameter
     *          interface
     */
    DEFINE PUBLIC PROPERTY SaveChangesParameter AS ISerializable NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns the reference to the ServiceAdapter that is being used by this
     *          SmartBusinessEntityAdapter
     * Notes:   See InitializeServiceAdapter method
     */
    DEFINE PUBLIC PROPERTY ServiceAdapter AS IServiceAdapter NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: A reference to the SmartCommitSource of this SmartBusinessEntityAdapter
     * Notes:
     */
    DEFINE PUBLIC PROPERTY SmartCommitSource AS Consultingwerk.SmartComponents.Interfaces.ISmartCommitSource NO-UNDO
    GET.
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.ISmartCommitSource):

        IF NOT VALID-OBJECT (arg) AND VALID-OBJECT (THIS-OBJECT:SmartCommitSource) THEN
            THIS-OBJECT:SmartCommitSource:DeregisterSmartCommitTarget (THIS-OBJECT) .

        THIS-OBJECT:SmartCommitSource = arg .

        OnSmartCommitSourceChanged (System.EventArgs:Empty) .

        IF VALID-OBJECT (arg) THEN
            arg:RegisterSmartCommitTarget (THIS-OBJECT) .

    END SET.

    /**
     * Purpose: Returns the current Transaction State
     * Notes:
     */
    DEFINE PUBLIC PROPERTY SmartTransactionState AS TransactionStateEnum NO-UNDO
    GET.
    PRIVATE SET (arg AS TransactionStateEnum):

        IF THIS-OBJECT:AutoCommit = TRUE AND arg <> TransactionStateEnum:NoManualCommit THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Invalid TransactionState &1 when AutoCommit is true."{&TRAN}, arg:ToString()), 0) .
        ELSE IF THIS-OBJECT:AutoCommit = FALSE AND arg = TransactionStateEnum:NoManualCommit THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Invalid TransactionState &1 when AutoCommit is false."{&TRAN}, arg:ToString()), 0) .

        THIS-OBJECT:SmartTransactionState = arg .

        OnSmartTransactionStateChanged (System.EventArgs:Empty) .

    END SET.

    /**
     * Purpose: Value for StopAfter to be passed to the Service Interface
     * Notes:   Allows limiting the execution time of queries executed on the backend
     *          Not supported on OpenEdge 10.2B
     *          The default value for this property may be set using the DefaultStopAfter
     *          preprocessor variable in Consultingwerk/products.i
     */
    DEFINE PUBLIC PROPERTY StopAfter AS INTEGER INITIAL &IF DEFINED (DefaultStopAfter) NE 0 &THEN {&DefaultStopAfter} &ELSE 0 &ENDIF NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Attaches the Query/ProDataset to the BindingSource
     * Notes:
     */
    METHOD OVERRIDE PROTECTED VOID AttachQueryToBindingSource ():

        DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.

        ASSIGN hDataset = THIS-OBJECT:DataSet .

        IF THIS-OBJECT:BindTo = "QUERY":U THEN
            ASSIGN THIS-OBJECT:BindingSource:AutoSync = TRUE
                   THIS-OBJECT:BindingSource:Handle   = THIS-OBJECT:QueryHandle .
        ELSE
            ASSIGN THIS-OBJECT:BindingSource:AutoSync = TRUE
                   THIS-OBJECT:BindingSource:Handle   = hDataset .

    END METHOD.

    /**
     * Purpose: Empty the DATASET inside the DataAdapter and reopen Query.
     * Notes:
     */
    METHOD OVERRIDE PUBLIC VOID CloseQuery ():

        /* Mike Fechner, Consultingwerk Ltd. 21.07.2009
           SetTrackingChanges to FALSE before EMPTY-DATASET */
        SetTrackingChanges (FALSE) .

        IF VALID-HANDLE (hDataset) THEN
            hDataset:EMPTY-DATASET ()   .

        IF VALID-HANDLE (THIS-OBJECT:QueryHandle) THEN
            THIS-OBJECT:QueryHandle:QUERY-OPEN () .

        SUPER:CloseQuery () .

    END METHOD.

    /**
     * Purpose: Commits the transaction to the backend
     * Notes:   See http://jira.consultingwerkcloud.com/browse/SCL-407 for details
     *          on how errors are handled
     * @return Logical value indicating if the operation was successful
     */
    METHOD PUBLIC LOGICAL CommitTransaction ():

        DEFINE VARIABLE e                           AS System.ComponentModel.CancelEventArgs                    NO-UNDO .
        DEFINE VARIABLE roRowid                     AS ROWID EXTENT                                             NO-UNDO .
        DEFINE VARIABLE oErrorAdapter               AS SmartDatasetAdapter                                      NO-UNDO .
        DEFINE VARIABLE oParentAdapter              AS SmartDatasetAdapter                                      NO-UNDO .
        DEFINE VARIABLE oParentAdapters             AS Consultingwerk.Framework.Base.List                       NO-UNDO .
        DEFINE VARIABLE oUpdatingTarget             AS ISmartTableIOTarget                                      NO-UNDO .
        DEFINE VARIABLE oValidationEventArgs        AS HandleValidationErrorEventArgs                           NO-UNDO .
        DEFINE VARIABLE oRowids                     AS RowidDictionary                                          NO-UNDO .
        DEFINE VARIABLE hErrorBuffer                AS HANDLE                                                   NO-UNDO .
        DEFINE VARIABLE i                           AS INTEGER                                                  NO-UNDO .
        DEFINE VARIABLE hRepositionBuffer           AS HANDLE                                                   NO-UNDO .
        DEFINE VARIABLE cFindString                 AS CHARACTER                                                NO-UNDO .
        DEFINE VARIABLE lSuccessful                 AS LOGICAL                                                  NO-UNDO INIT ? .

        IF THIS-OBJECT:AutoCommit = TRUE THEN
            UNDO, THROW NEW AppError
                              (SmartComponentLibraryCustomizer:SmartBusinessEntityAdapter_CommitRejectWithAutoCommit,
                               0) .

        /* Mike Fechner, Consultingwerk Ltd. 22.05.2013
           Raise BeforeCommitTransaction event */
        e = NEW System.ComponentModel.CancelEventArgs () .

        THIS-OBJECT:OnBeforeCommitTransaction (e) .

        IF VALID-OBJECT (e) AND e:Cancel THEN
            RETURN FALSE .

        /* Save Pending updates in one of the SmartDataTargets */
        THIS-OBJECT:SaveInDataTargets () .

        IF THIS-OBJECT:SmartTransactionState <> TransactionStateEnum:PendingChanges THEN
            RETURN TRUE .

        IF NOT Consultingwerk.Util.DatasetHelper:HasChanges (THIS-OBJECT:Dataset) THEN
            RETURN TRUE .

        roRowid = DatasetHelper:GetDatasetRowids (THIS-OBJECT:DataSet) .

        /* Mike Fechner, Consultingwerk Ltd. 04.06.2014
           Moved DatasetHelper:RepositionDatasetBuffers to a finally block,
           so that we reposition every buffer also, when an error was thrown
           from the SubmitChangesToBackend (e.g. Validation messages)  */
        DO ON ERROR UNDO, THROW:
            DO ON ERROR UNDO, THROW:
                THIS-OBJECT:SubmitChangesToBackend() .

                ASSIGN lSuccessful = TRUE .

                FINALLY:
                    THIS-OBJECT:QueryHandle:QUERY-OPEN () .
                    THIS-OBJECT:BindingSource:RefreshAll () .
                END FINALLY.
            END.

            CATCH valerr AS ValidationException:

                ASSIGN lSuccessful          = FALSE
                       oValidationEventArgs = NEW HandleValidationErrorEventArgs (valerr) .

                THIS-OBJECT:OnHandleValidationError (oValidationEventArgs) .

                IF VALID-OBJECT (oValidationEventArgs) AND oValidationEventArgs:Handled THEN
                    RETURN FALSE .

                IF VALID-OBJECT (valerr:RecordIdentifier) AND
                   valerr:RecordIdentifier:BufferName > "":U AND
                   valerr:RecordIdentifier:Rowid <> ? THEN DO:

                   /* Locate the first linked adapter that deals with this table */
                   ASSIGN oErrorAdapter = THIS-OBJECT:FindCommitErrorDatasetChildAdapter (valerr:RecordIdentifier) .

                   IF VALID-OBJECT (oErrorAdapter) THEN DO:
                       DO ON ERROR UNDO, THROW:

                            /* Mike Fechner, Consultingwerk Ltd. 29.11.2015
                               SCL-1121: Need to reposition parent and grand parent Adapters as well
                                         to be able to reposition to a child record with errors */
                           IF oErrorAdapter <> THIS-OBJECT THEN DO:
                                hErrorBuffer = THIS-OBJECT:Dataset:GET-BUFFER-HANDLE (valerr:RecordIdentifier:BufferName) .

                                hErrorBuffer:FIND-BY-ROWID (valerr:RecordIdentifier:Rowid) .

                                oRowids = DatasetHelper:ParentRecordRowids (hErrorBuffer) .

                                oParentAdapters = NEW Consultingwerk.Framework.Base.List () .

                                oParentAdapter = CAST (oErrorAdapter:SmartDataSource, SmartDatasetAdapter) .

                                DO WHILE VALID-OBJECT (oParentAdapter):
                                    oParentAdapters:Add (oParentAdapter) .

                                    IF TYPE-OF (oParentAdapters, SmartBusinessEntityAdapter) THEN
                                        LEAVE .
                                    ELSE
                                        ASSIGN oParentAdapter = CAST (oParentAdapter:SmartDataSource, SmartDatasetAdapter) .
                                END.

                                DO i = oParentAdapters:Count TO 1 BY -1:
                                    oParentAdapter = CAST (oParentAdapters:GetItem (1), SmartDatasetAdapter) .

                                    IF oRowids:ContainsKey (oParentAdapter:EntityTable) THEN
                                        oParentAdapter:RepositionToRowid (oRowids:GetValue (oParentAdapter:EntityTable)) .
                                END.
                           END.

                           oErrorAdapter:RepositionToRowid (valerr:RecordIdentifier:Rowid) .

                           /* When we cannot reposition to the proper row,
                              just show the original error */
                           @SuppressUnusedWarnings.
                           CATCH repoerror AS Progress.Lang.Error:
                               ErrorHelper:ShowErrorMessage (valerr) .
                               RETURN FALSE .
                           END CATCH.
                       END.

                       roRowid = DatasetHelper:GetDatasetRowids (THIS-OBJECT:DataSet) .

                       ASSIGN oUpdatingTarget = THIS-OBJECT:FindLinkedUpdatingSmartDataTarget (oErrorAdapter, valerr:RecordIdentifier) .

                       IF VALID-OBJECT (oUpdatingTarget) THEN DO:
                           IF oUpdatingTarget:SmartTableIOState = TableIOStateEnum:FieldsDisabled THEN
                               oUpdatingTarget:EnableFields (EnableFieldsEnum:Update) .

                           oUpdatingTarget:HandleValidationError (valerr) .

                           RETURN FALSE .
                       END .
                   END.
                END.

                /* When not handled by an updating Data Target */
                ErrorHelper:ShowErrorMessage (valerr) .

                /* Leave here, don't change Transaction State */
                RETURN FALSE .

            END CATCH.

            FINALLY:
                IF lSuccessful = TRUE THEN DO:
                    /* Mike Fechner, Consultingwerk Ltd. 01.04.2016
                       SCL-1209 - allow developers to control which record should be
                       repositioned after successful commit by handling the
                       FindCurrentRecordAfterCommitChanges event */
                    /* Mike Fechner, Consultingwerk Ltd. 11.04.2016
                       Setting the SmartTransactionState to NoPendingChanges here is
                       required as the FindRowWhere (FindOnServerOnly) may otherwise
                       prompt if changes should be committed (again)  */
                    THIS-OBJECT:SmartTransactionState = TransactionStateEnum:NoPendingChanges .

                    ASSIGN hRepositionBuffer = THIS-OBJECT:DataSet:GET-BUFFER-HANDLE(THIS-OBJECT:EntityTable) .
                    hRepositionBuffer:FIND-BY-ROWID (roRowid[1]) .

                    THIS-OBJECT:OnFindCurrentRecordAfterCommitChanges (NEW BufferHandleEventArgs(hRepositionBuffer)) .

                    cFindString = BufferHelper:UniqueFindPredicate (hRepositionBuffer) .

                    THIS-OBJECT:QueryHandle:QUERY-OPEN () .

                    THIS-OBJECT:FindRowWhere (cFindString,
                                              FindRowWhereModeEnum:FindOnClient) .

                    IF hRepositionBuffer:ROWID <> roRowid[1] THEN
                        THIS-OBJECT:FindRowWhere (cFindString,
                                                  FindRowWhereModeEnum:FindOnServerOnly) .
                END.
                ELSE
                    THIS-OBJECT:RepositionToRowid (roRowid) .

                @SuppressUnusedWarnings.
                CATCH err AS Progress.Lang.Error:
                    THIS-OBJECT:BindingSource:Position = 0 .
                END CATCH.
            END FINALLY.
        END.

        THIS-OBJECT:SmartTransactionState = TransactionStateEnum:NoPendingChanges .

        THIS-OBJECT:OnAfterCommitTransaction (System.EventArgs:Empty) .

        IF lEndUpdateStateDSPending = TRUE AND
           VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND
           TYPE-OF (THIS-OBJECT:SmartDataSource, SmartDataAdapter) THEN DO:
            THIS-OBJECT:SmartDataSource:EndUpdateState (CAST (THIS-OBJECT:SmartDataSource, SmartDataAdapter):UpdatingSmartDataTarget) .
            ASSIGN lEndUpdateStateDSPending = FALSE .
        END.

        RETURN TRUE .

    END METHOD.

    /**
     * Purpose: Empties the ProDataset
     * Notes:   Only supported when EmptyDatasetOnRetrieveData = FALSE
     */
    METHOD PUBLIC VOID EmptyDataset ():

        IF THIS-OBJECT:EmptyDatasetOnRetrieveData <> FALSE THEN
            UNDO, THROW NEW NotSupportedException ("EmptyDataset":U,
                                                   THIS-OBJECT:GetClass():TypeName,
                                                   "EmptyDataset() is only supported when EmptyDatasetOnRetrieveData = FALSE"{&TRAN}) .

        THIS-OBJECT:DataSet:EMPTY-DATASET () .

    END METHOD .

    /**
     * Purpose: Executed at the end of the initialization of the
     *          SmartBusinessEntityAdapter to launch the DatasetController (if any).
     * Notes:
     */
    METHOD OVERRIDE PUBLIC VOID EndInit ():

        SUPER:EndInit().

        /* Mike Fechner, Consultingwerk Ltd. 10.08.2010
           Don't launch the DatasetController during DesignTime */
        IF THIS-OBJECT:DatasetControllerType > "":U AND NOT THIS-OBJECT:DesignTime

            /* Mike Fechner, Consultingwerk Ltd. 18.02.2011
               In EndInit the LaunchDatasetController should not be
               created, when there is already an instance. This could be
               the case, when a SmartBusinessEntityAdapter instance is
               used in a Form that's a base Form of another Form */
            AND NOT VALID-OBJECT (THIS-OBJECT:DatasetController)
            THEN
                LaunchDatasetController() .

       IF NOT lRegistered AND VALID-OBJECT (THIS-OBJECT:DatasetController) THEN DO:
           THIS-OBJECT:DatasetController:RegisterConsumer (THIS-OBJECT) .
           ASSIGN lRegistered = TRUE .
       END.

        /* Mike Fechner, Consultingwerk Ltd. 02.09.2011
           Assign SmartTransactionStateEnum property by re-assigning
           the AutoCommit property */
        IF NOT THIS-OBJECT:DesignTime THEN
            THIS-OBJECT:AutoCommit = THIS-OBJECT:AutoCommit .

    END METHOD.

    /**
     * Purpose: Begins a transaction when starting update
     * Notes:   Allows SmartDatasetChildAdapter to initiate TransactionState in a
     *          parent SmartBusinessEntityAdapter
     */
    METHOD PUBLIC OVERRIDE VOID BeginTransactionState ():

        IF THIS-OBJECT:AutoCommit = FALSE AND
           THIS-OBJECT:SmartTransactionState <> Consultingwerk.SmartComponents.Enum.TransactionStateEnum:PendingChanges THEN

            THIS-OBJECT:SmartTransactionState = Consultingwerk.SmartComponents.Enum.TransactionStateEnum:PendingChanges .

    END METHOD .

    /**
     * Purpose: The Message to for instance enable a Browser is send to all
     *          SmartDataTargets except the calling one.
     * Notes:   This Method is called from the SmartViewerControl derived Class when
     *          starting to Modify or Add data.
     *          Enforced by Interface ISmartDataSource.
     * @param poSmartDataTarget The reference to the ISmartDataTarget that ends the UpdateState
     */
    METHOD OVERRIDE PUBLIC VOID EndUpdateState (poSmartDataTarget AS Consultingwerk.SmartComponents.Interfaces.ISmartDataTarget):

        SUPER:EndUpdateState (poSmartDataTarget).

        IF THIS-OBJECT:AutoCommit = FALSE THEN
            THIS-OBJECT:VerifyTransactionPending () .

    END METHOD.

    /**
     * Purpose: Invokes EndUpdateState in the SmartDataSource of this SmartDataAdapter
     * Notes:   Suspends the call to EndUpdateState when there is a pending transaction
     * @param poSmartDataTarget The reference to the ISmartDataTarget that ends the UpdateState
     */
    METHOD PROTECTED OVERRIDE VOID EndUpdateStateInDataSource (poSmartDataTarget AS ISmartDataTarget):

        IF THIS-OBJECT:AutoCommit            = FALSE AND
           THIS-OBJECT:SmartTransactionState = TransactionStateEnum:PendingChanges THEN

            ASSIGN lEndUpdateStateDSPending = TRUE .
        ELSE
            SUPER:EndUpdateStateInDataSource (poSmartDataTarget) .

    END METHOD.

    /**
     * Purpose: Fetches all remaining data from the backend
     * Notes:
     * @return Logical value indicating if more rows have been fetched
     */
    METHOD PUBLIC LOGICAL FetchAll ():

        IF THIS-OBJECT:PrevContext > "":U OR
           THIS-OBJECT:NextContext > "":U THEN DO:

            THIS-OBJECT:FetchMoreRowsForwards (Consultingwerk.Const:MAX_INTEGER) .

            RETURN TRUE .
        END.

        RETURN FALSE .

    END METHOD .

    /**
     * Purpose: Overrides FetchFirst in SmartDataAdpater. Retrieves the first batch
     *          of records when backwards batching
     * Notes:   Method enforced by ISmartNavigationTarget
     * @return Logical value indicating if the operation was successful
     */
    METHOD OVERRIDE PUBLIC LOGICAL FetchFirst ():

        DEFINE VARIABLE oBrowser AS ISmartDataBrowser NO-UNDO .

        IF THIS-OBJECT:PrevContext > "":U THEN DO:
            RetrieveData () .

            oBrowser = THIS-OBJECT:FindLinkedSmartDataBrowser () .

            IF VALID-OBJECT (oBrowser) THEN
                oBrowser:ScrollToFirstRow () .

            RETURN TRUE .
        END.
        ELSE
            RETURN SUPER:FetchFirst() .

    END METHOD.

    /**
     * Purpose: Overrides FetchLast in SmartDataAdapter to retrieve full business
     *          entity result when batching.
     * Notes:   Method enforced by ISmartNavigationTarget
     * @return Logical value indicating if the operation was successful
     */
    METHOD OVERRIDE PUBLIC LOGICAL FetchLast ():

        DEFINE VARIABLE iSaveBatchSize AS INTEGER NO-UNDO.
        DEFINE VARIABLE lRet           AS LOGICAL NO-UNDO.

        ASSIGN iSaveBatchSize = THIS-OBJECT:BatchSize .

        /* Mike Fechner, Consultingwerk Ltd. 20.07.2011
           Do we need an alternative FetchLast method */
        IF THIS-OBJECT:BatchSize > 0 AND
           ENTRY(1, THIS-OBJECT:NextContext, CHR(1)) > "":U THEN DO:

            /* Mike Fechner, Consultingwerk Ltd. 20.07.2011
               Traditional method of FetchLast by temporarily setting
               BatchSize to zero and fetching all remaining records
               from the back end */
            IF THIS-OBJECT:FetchLastOnServer = FALSE THEN DO:
                THIS-OBJECT:BatchSize = 0 .

                lRet = SUPER:FetchLast().

                /* If we are not at the last row, this is probably because
                   additional batches have been fetched */
                IF THIS-OBJECT:RecordPosition <> RecordPositionEnum:LastRecord THEN DO:
                    lRet = SUPER:FetchLast().
                END.
            END.
            /* Mike Fechner, Consultingwerk Ltd. 20.07.2011
               Perform GET LAST in Data Access Layer */
            ELSE DO:
                THIS-OBJECT:FetchLastOnServer () .

                /* If we are not at the last row, this is probably because
                   additional batches have been fetched */
                IF THIS-OBJECT:RecordPosition <> RecordPositionEnum:LastRecord THEN DO:
                    lRet = SUPER:FetchLast().
                END.
            END.
        END.
        ELSE
            /* Mike Fechner, Consultingwerk Ltd. 20.07.2011
               no override required */
            lRet = SUPER:FetchLast () .

        RETURN lRet .

        FINALLY:
             THIS-OBJECT:BatchSize = iSaveBatchSize .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Fetch Last On Server, used by FetchLast when FetchLastOnServer = TRUE
     * Notes:
     */
    METHOD PUBLIC VOID FetchLastOnServer ():

        DEFINE VARIABLE oBrowser AS ISmartDataBrowser NO-UNDO .

        oBrowser = THIS-OBJECT:FindLinkedSmartDataBrowser () .

        THIS-OBJECT:FindString = "LAST":U .

        THIS-OBJECT:RetrieveData () .

        IF VALID-OBJECT (oBrowser) THEN
            oBrowser:ScrollToLastRow () .

        FINALLY:
            THIS-OBJECT:FindString = "":U .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Fetches additional rows forwards
     * Notes:   Useful to fill up the Dataset for data export operations (Excel Export)
     *          When we are actually backwards-batching we will start fetching the
     *          number of rows forward
     * @param piNumRows The number of rows to fetch forwards
     */
    METHOD PUBLIC VOID FetchMoreRowsForwards (piNumRows AS INTEGER):

        DEFINE VARIABLE iBatchSize AS INTEGER NO-UNDO.

        Assert:GreaterThanZero (piNumRows, "piNumRows":U) .

        ASSIGN iBatchSize = THIS-OBJECT:BatchSize .

        IF THIS-OBJECT:PrevContext > "":U THEN DO ON ERROR UNDO, THROW:

            ASSIGN THIS-OBJECT:BatchSize = piNumRows .

            THIS-OBJECT:RetrieveData () .

            RETURN .
        END.

        /* When we are not forward batching, we already have all rows on the client */
        IF THIS-OBJECT:NextContext > "":U THEN .
        ELSE RETURN .

        ASSIGN THIS-OBJECT:BatchSize = piNumRows .

        THIS-OBJECT:OffEndHandler (?, ?) .

        FINALLY:
            THIS-OBJECT:BatchSize = iBatchSize .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Overrides FetchPrev in SmartDataAdpater. Retrieves the previous batch
     *          of records when backwards batching
     * Notes:   Method enforced by ISmartNavigationTarget
     * @return Logical value indicating if the operation was successful
     */
    METHOD OVERRIDE PUBLIC LOGICAL FetchPrev ():

        DEFINE VARIABLE iNumResults       AS INTEGER   NO-UNDO.

        IF VALID-OBJECT (THIS-OBJECT:BindingSource) AND
           THIS-OBJECT:BindingSource:Position = 0 AND
           THIS-OBJECT:PrevContext > "":U THEN DO:

            ASSIGN iNumResults = THIS-OBJECT:BindingSource:Count .

            OffHome () .

            THIS-OBJECT:BindingSource:Position = THIS-OBJECT:BindingSource:Count - iNumResults - 1.

            RETURN TRUE .
        END.
        ELSE
            RETURN SUPER:FetchPrev().

    END METHOD.

    /**
     * Purpose: Locates the SmartDatasetAdapter to be used to handle a commit transaction
     *          validation error
     * Notes:
     * @param poRecordIdentifier The Record Identifier describing the record that caused the validation error
     * @return The updating SmartTableIOTarget used to handle the error (may be unknown)
     */
    METHOD SmartDatasetAdapter FindCommitErrorDatasetChildAdapter (poRecordIdentifier AS RecordIdentifier):

        DEFINE VARIABLE oErrorAdapter         AS SmartDatasetAdapter                            NO-UNDO .
        DEFINE VARIABLE oFindAdapterEventArgs AS FindDataAdapterForCommitErrorHandlingEventArgs NO-UNDO .

        ASSIGN oErrorAdapter         = THIS-OBJECT:FindLinkedDatasetAdapterForEntityTable (poRecordIdentifier:BufferName)
               oFindAdapterEventArgs = NEW FindDataAdapterForCommitErrorHandlingEventArgs (oErrorAdapter, poRecordIdentifier) .

        THIS-OBJECT:OnFindDataAdapterForCommitErrorHandling (oFindAdapterEventArgs) .

        IF VALID-OBJECT (oFindAdapterEventArgs) THEN
            ASSIGN oErrorAdapter = oFindAdapterEventArgs:Adapter .

        RETURN oErrorAdapter .

    END METHOD.

    /**
     * Purpose: Locates the SmartTableIOTarget to be used to handle a commit transaction
     *          validation error
     * Notes:
     * @param poAdapter The SmartDatasetAdapter that is used to handle the error
     * @param poRecordIdentifier The Record Identifier describing the record that caused the validation error
     * @return The updating SmartTableIOTarget used to handle the error (may be unknown)
     */
    METHOD ISmartTableIOTarget FindLinkedUpdatingSmartDataTarget (poAdapter AS SmartDatasetAdapter,
                                                                  poRecordIdentifier AS RecordIdentifier):

        DEFINE VARIABLE oUpdatingTarget             AS ISmartTableIOTarget                                      NO-UNDO .
        DEFINE VARIABLE oFindTableIOTargetEventArgs AS FindUpdatingTableIOTargetForCommitErrorHandlingEventArgs NO-UNDO .

        ASSIGN oUpdatingTarget             = poAdapter:FindLinkedUpdatingSmartDataTarget ()
               oFindTableIOTargetEventArgs = NEW FindUpdatingTableIOTargetForCommitErrorHandlingEventArgs (oUpdatingTarget, poRecordIdentifier) .

        THIS-OBJECT:OnFindUpdatingTableIOTargetForCommitErrorHandling (oFindTableIOTargetEventArgs) .

        IF VALID-OBJECT (oFindTableIOTargetEventArgs) THEN
            ASSIGN oUpdatingTarget = oFindTableIOTargetEventArgs:TableIOTarget .

        RETURN oUpdatingTarget .

    END METHOD.

    /**
     * Purpose: Finds a row in the EntityTable and locates the BindingSource
     *          to that row, optionally requesting the record on the server
     *          when not found on the client.
     * Notes:   Overloads FindRowWhere (pcFindString) in SmartDatasetAdapter
     *          The record is actually located using a dynamic query instead of a
     *          FIND so that sorting is also supported (typically sorting should
     *          be passed in with the same value as the QuerySort property of
     *          this SmartBusinessEntityAdapter instance
     *          When FindRowWhere (FindRowEhereModeEnum:FindOnServer) is called before
     *          any previous invokation of RetrieveData if will act like FindRowWhere
     *          (FindOnServerOnly).
     *          A FindRowWhere (FindOnClient) is called before any previous invokation
     *          of RetrieveData an error message is raised.
     * @param pcFindString The find criteria, optionally including a sort phrase (BY)
     * @param poFindMode The FindRowWhereModeEnum value indicating if the search should be done on the client first, client then server or server only
     * @return Logical value indicating if a record was found
     */
    METHOD PUBLIC LOGICAL FindRowWhere (pcFindString AS CHARACTER,
                                        poFindMode AS FindRowWhereModeEnum):

        DEFINE VARIABLE lResult    AS LOGICAL           NO-UNDO .
        DEFINE VARIABLE oBrowser   AS ISmartDataBrowser NO-UNDO .
        DEFINE VARIABLE lNextBatch AS LOGICAL           NO-UNDO INIT FALSE .
        DEFINE VARIABLE lPrevBatch AS LOGICAL           NO-UNDO INIT FALSE .

        /* Mike Fechner, Consultingwerk Ltd. 06.08.2014
           SCL-399: SmartBusinessEntityAdapter: When calling FindRowWhere (FindRowWhereModeEnum:FindOnServer)
                    before RetrieveData() act like FindRowWhereModeEnum:FindOnServerOnly */
        IF NOT VALID-HANDLE (THIS-OBJECT:QueryHandle) AND poFindMode = FindRowWhereModeEnum:FindOnServer THEN
            ASSIGN poFindMode = FindRowWhereModeEnum:FindOnServerOnly .

        IF poFindMode <> FindRowWhereModeEnum:FindOnServerOnly THEN DO:
            lResult = SUPER:FindRowWhere (INPUT pcFindString).

            IF lResult = TRUE THEN
                RETURN TRUE .

            IF poFindMode = FindRowWhereModeEnum:FindOnClient THEN
                RETURN FALSE .
        END.

        oBrowser = THIS-OBJECT:FindLinkedSmartDataBrowser () .

        /* Mike Fechner, Consultingwerk Ltd. 21.01.2011
           Record was not found on client, so store find string in protected
           property FindString so that RetrieveData can pass it to the back end. */
        THIS-OBJECT:FindString = pcFindString .

        THIS-OBJECT:RetrieveData () .

        /* Mike Fechner, Consultingwerk Ltd. 25.02.2011
           When prefetching rows, locate demanded record in result set */
        IF THIS-OBJECT:PrefetchOnFindRowWhere > 0 THEN DO:

            IF ENTRY (1, THIS-OBJECT:NextContext, CHR (1)) > "":U THEN
                ASSIGN lNextBatch = TRUE .

            IF ENTRY (1, THIS-OBJECT:PrevContext, CHR (1)) > "":U THEN
                ASSIGN lPrevBatch = TRUE .

            IF NOT lNextBatch AND VALID-OBJECT (oBrowser) THEN
                oBrowser:ScrollToLastRow () .
            ELSE IF NOT lPrevBatch AND VALID-OBJECT (oBrowser) THEN
                oBrowser:ScrollToFirstRow () .

            THIS-OBJECT:FindRowWhere (pcFindString) .

            /* Mike Fechner, Consultingwerk Ltd. 27.04.2015
               When the record  */
            IF VALID-OBJECT (oBrowser) THEN DO:

                IF lPrevBatch AND lNextBatch THEN
                    oBrowser:ScrollToCurrentRow (TRUE, TRUE) .
                ELSE
                    IF lNextBatch THEN
                        oBrowser:ScrollToCurrentRow (TRUE, FALSE) .
            END.
        END.

        RETURN (THIS-OBJECT:BindingSource:Count > 0) .

        FINALLY:
            THIS-OBJECT:FindString = "":U .
        END FINALLY.

    END METHOD.

    &IF DEFINED (NoStaticsInHybrids) EQ 0 &THEN
    /**
     * Purpose: Locates the SmartBusinessEntityAdapter instance that is working
     *          with the passed in IDatasetController
     * Notes:   This method may be removed using the NoStaticsInHybrids setting
     *          in Consultingwerk/products.i. This allows to avoid static members
     *          in hybrid classes SmartDataBrowser and SmartBusinessEntityAdapter
     *          to avoid issues with the combination of static and hybrids on 11.2
     *          and 11.3, OE defect OE00240707 http://knowledgebase.progress.com/articles/Article/000042229?popup=true
     * @param poDatasetController The IDatasetController instance that was launched by a SmartBusinessEntityAdapter
     * @return The SmartBusinessEntityAdapter that has launched the IDatasetController or the unknown value
     */
    METHOD PUBLIC STATIC SmartBusinessEntityAdapter FromDatasetController (poDatasetController AS IDatasetController):

        DEFINE VARIABLE oObject  AS Progress.Lang.Object       NO-UNDO .
        DEFINE VARIABLE oAdapter AS SmartBusinessEntityAdapter NO-UNDO .

        ASSIGN oObject = SESSION:FIRST-OBJECT .

        DO WHILE VALID-OBJECT (oObject):
            IF TYPE-OF (oObject, SmartBusinessEntityAdapter) THEN DO:
                ASSIGN oAdapter = CAST (oObject, SmartBusinessEntityAdapter) .

                IF VALID-OBJECT (oAdapter:DatasetController) AND oAdapter:DatasetController = poDatasetController
                    THEN RETURN oAdapter .
            END.

            ASSIGN oObject = oObject:NEXT-SIBLING .
        END.

        RETURN ? .

    END METHOD.
    &ENDIF

    /**
     * Purpose: Retrieves an empty dataset
     * Notes:   Used in the GET implementation of the DataSet property
     *          This dataset can be used to bind UI components to before calling
     *          RetrieveData
     * @return The handle of the empty ProDataset
     */
    METHOD PROTECTED HANDLE GetEmptyDataset ():

        DEFINE VARIABLE hDataset   AS HANDLE NO-UNDO.

        DEFINE VARIABLE oServiceAdapter AS Consultingwerk.OERA.IServiceAdapter NO-UNDO .
        DEFINE VARIABLE hContextDataset AS HANDLE                              NO-UNDO .

        IF NOT THIS-OBJECT:DesignTime THEN DO:
            {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i THIS-OBJECT:EntityName  "'EntityName':U"} .
            {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i THIS-OBJECT:EntityTable "'EntityTable':U"} .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 27.12.2009
           No action when the EntityName is not set */
        IF THIS-OBJECT:EntityName > "":U THEN .
        ELSE RETURN ? .

        oServiceAdapter = THIS-OBJECT:ServiceAdapter .

        IF VALID-HANDLE (THIS-OBJECT:ContextDataset) AND
           TYPE-OF (oServiceAdapter,
                    IServiceAdapterWithContextDataset) THEN DO:

            ASSIGN hContextDataset = THIS-OBJECT:ContextDataset .

            CAST(oServiceAdapter,
                 IServiceAdapterWithContextDataset):FetchDataset (THIS-OBJECT:AppServerPartition,
                                                                  THIS-OBJECT:EntityName,
                                                                  OUTPUT DATASET-HANDLE hDataset BY-REFERENCE,
                                                                  hContextDataset) .
        END.
        ELSE
        oServiceAdapter:FetchDataset (THIS-OBJECT:AppServerPartition,
                                      THIS-OBJECT:EntityName,
                                      OUTPUT DATASET-HANDLE hDataset BY-REFERENCE) .

        IF THIS-OBJECT:DesignTime AND
           BusinessEntityDesignerSupport:SearchBusinessEntititiesOnAppServer AND
           BusinessEntityDesignerSupport:DisconnectAppServerAfterEachRequest THEN
            FrameworkSettings:AppServerServiceManager:DisconnectService ("":U) .

        RETURN hDataset .

    END METHOD.

    /**
     * Purpose: Evaluates the current record position property value
     * Notes:   Overrides standard implementation in SmartDataAdapter to allow
     *          Backwards Batching
     * @return The current record position as a CHARACTER representing an RecordPositionEnum value
     */
    METHOD OVERRIDE PROTECTED CHARACTER GetRecordPosition ():

        IF VALID-OBJECT (THIS-OBJECT:BindingSource) AND
           THIS-OBJECT:BindingSource:Position = 0 AND
           THIS-OBJECT:BindingSource:Count > 1 AND
           THIS-OBJECT:PrevContext > "":U THEN

            RETURN RecordPositionEnum:NotFirstOrLast .

        RETURN SUPER:GetRecordPosition().

    END METHOD.

    /**
     * Purpose: Evaluates the status of the current RetrieveData operation
     * Notes:   NotBatching / BackwardsBatching / ForwardBatching
     * @return The RetrieveDataStatusEnum value that describes the current data status
     */
    METHOD PUBLIC RetrieveDataStatusEnum GetRetrieveDataStatus ():

        IF THIS-OBJECT:NextContext > "":U AND THIS-OBJECT:PrevContext > "":U THEN
            RETURN RetrieveDataStatusEnum:ForwardAndBackwardsBatching .

        ELSE IF THIS-OBJECT:NextContext > "":U THEN
            RETURN RetrieveDataStatusEnum:ForwardBatching .

        ELSE IF THIS-OBJECT:PrevContext > "":U THEN
            RETURN RetrieveDataStatusEnum:BackwardsBatching .

        ELSE RETURN RetrieveDataStatusEnum:NotBatching .

    END METHOD .

    /**
     * Purpose: Prompts if pending changes should be saved before retrieving new
     *          data
     * Notes:
     * @return Logical value indicating if we still have a pending transaction
     */
    METHOD PROTECTED LOGICAL HasPendingTransaction ():

        DEFINE VARIABLE oDialogResult AS DialogResultEnum NO-UNDO .

        IF THIS-OBJECT:SmartTransactionState = TransactionStateEnum:PendingChanges THEN DO:

            oDialogResult = MessageFormHelper:AskQuestion (SmartComponentLibraryCustomizer:SmartBusinessEntityAdapter_PromptCommitTransactionBeforeRetrieveData) .

            IF oDialogResult = DialogResultEnum:DialogResultYes THEN DO:
                THIS-OBJECT:CommitTransaction () .

                IF THIS-OBJECT:SmartTransactionState = TransactionStateEnum:NoPendingChanges THEN
                    RETURN FALSE .
            END.

            RETURN TRUE .
        END.

        RETURN FALSE .

    END METHOD.

    /**
     * Purpose: Prompts if pending update should be saved before retrieving new
     *          data, called from RetrieveData
     * Notes:   Only used when property PromptWhenRetrieveDataInUpdateState = TRUE
     * @return Logical value indicating if RetrieveData should be cancelled
     */
    METHOD PROTECTED LOGICAL HasPendingUpdate ():

        DEFINE VARIABLE oDialogResult AS DialogResultEnum NO-UNDO .

        IF THIS-OBJECT:SmartDataSourceState = DataSourceStateEnum:Updating THEN DO:

            IF VALID-OBJECT (THIS-OBJECT:UpdatingSmartDataTarget) THEN DO:

                /* Continue data retrieval and cancel pending update? */
                oDialogResult = MessageFormHelper:AskQuestion (SmartComponentLibraryCustomizer:SmartBusinessEntityAdapter_PromptRetrieveDataWithPendingUpdate ) .

                IF oDialogResult = DialogResultEnum:DialogResultYes THEN DO:

                    CAST (THIS-OBJECT:UpdatingSmartDataTarget, ISmartTableIOTarget):CancelUpdate() .

                    /* When still in update state, cancel RetrieveData as well*/
                    IF THIS-OBJECT:SmartDataSourceState = DataSourceStateEnum:Updating THEN
                        RETURN TRUE .
                    ELSE
                        RETURN FALSE .
                END.
                ELSE
                    /* user has chosen no */
                    RETURN TRUE .
            END.
            ELSE
                /* Can't handle Updating state with no Updating Data Target */
                RETURN TRUE .
        END.
            RETURN FALSE .

    END METHOD.

    /**
     * Purpose: Initializes the SmartBusinessEntityAdpater
     * Notes:   Useful to initalize the BindingSource and Query before calling
     *          RetrieveData. Defaults to InitializeAdpater(TRUE) to empty the
     *          dataset
     *          It's not recommended to call this method prior to the Form's OnLoad
     *          or OnShown event as the ProBindingSource seems to reset the Handle
     *          Property (Query or Dataset) that was assigned to early during
     *          initialization
     */
    METHOD PUBLIC OVERRIDE VOID InitializeAdapter ():

        THIS-OBJECT:InitializeAdapter (TRUE) .

    END METHOD.

    /**
     * Purpose: Initializes the SmartBusinessEntityAdpater
     * Notes:   Useful to initalize the BindingSource and Query before calling
     *          RetrieveData. When the dataset requieres to be emptied, we need need
     *          to turn off tracking-changes.
     *          It's not recommended to call this method prior to the Form's OnLoad
     *          or OnShown event as the ProBindingSource seems to reset the Handle
     *          Property (Query or Dataset) that was assigned to early during
     *          initialization
     * @param plEmptyDataset Logical flag to indicate if the dataset should be emptied or not.
     */
    METHOD PUBLIC OVERRIDE VOID InitializeAdapter (plEmptyDataset AS LOGICAL):

        DEFINE VARIABLE lBindingSourceCreated AS LOGICAL NO-UNDO INIT FALSE .

        IF NOT THIS-OBJECT:FirstTime THEN
            RETURN  .

        /* If no valid BindingSource exist a new one is created right here */
        IF NOT VALID-OBJECT(THIS-OBJECT:BindingSource) THEN DO:
            CreateBindingSource ().
            ASSIGN lBindingSourceCreated = TRUE .
        END.

        IF plEmptyDataset THEN DO:
            /* Mike Fechner, Consultingwerk Ltd. 07.07.2009
               TRACKING-CHANGES needs to be turned of during EMPTY-DATASET */
            THIS-OBJECT:SetTrackingChanges (FALSE) .

            hDataset:EMPTY-DATASET ()   .
        END.

        IF NOT lBindingSourceCreated THEN
            THIS-OBJECT:PrepareDataBinding () .

        ASSIGN THIS-OBJECT:FirstTime = FALSE .

        THIS-OBJECT:AttachQueryToBindingSource () .

    END METHOD.

    /**
     * Purpose: Initializes the ServiceAdapter property of the SmartBusinessEntityAdapter
     *          class
     * Notes:   Invoked by the constructor, defaults to FrameworkSettings:ServiceAdapter but
     *          allows for customization in a child class
     */
    METHOD PROTECTED VOID InitializeServiceAdapter ():

        THIS-OBJECT:ServiceAdapter = FrameworkSettings:ServiceAdapter .

    END METHOD .

    /**
     * Purpose: Invokes a method in the backend OERA business entity
     * Notes:
     * @param pcMethodName The method to be invoked in the Business Entity
     * @param phDataset INPUT-OUTPUT DATASET-HANDLE The handle of the dataset to be passed to the Business Entity method
     * @param poParameter The ISerializable parameter object to be passed to the Business Entity method
     */
    METHOD PUBLIC VOID InvokeMethod (pcMethodName AS CHARACTER,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset,
                                     poParameter AS ISerializable):

        DEFINE VARIABLE oServiceAdapter AS Consultingwerk.OERA.IServiceAdapter NO-UNDO .
        DEFINE VARIABLE hContextDataset AS HANDLE                              NO-UNDO .

        oServiceAdapter = THIS-OBJECT:ServiceAdapter .

        /* Mike Fechner, Consultingwerk Ltd. 11.09.2011
           Indicate WaitState */
        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
            FrameworkSettings:WaitStateManager:SetWaitState ("InvokeMethod ":U + pcMethodName, THIS-OBJECT) .

        IF VALID-HANDLE (THIS-OBJECT:ContextDataset) AND TYPE-OF (oServiceAdapter,
                                                                  IServiceAdapterWithContextDataset) THEN DO ON ERROR UNDO, THROW:
            ASSIGN hContextDataset = THIS-OBJECT:ContextDataset .

            CAST(oServiceAdapter,
                 IServiceAdapterWithContextDataset):InvokeMethod (THIS-OBJECT:AppServerPartition,
                                                                  THIS-OBJECT:EntityName,
                                                                  pcMethodName,
                                                                  INPUT-OUTPUT DATASET-HANDLE phDataset BY-REFERENCE,
                                                                  poParameter,
                                                                  hContextDataset) .
        END.
        ELSE
            oServiceAdapter:InvokeMethod (THIS-OBJECT:AppServerPartition,
                                          THIS-OBJECT:EntityName,
                                          pcMethodName,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset BY-REFERENCE,
                                          poParameter) .

        IF VALID-HANDLE (phDataset) THEN
            DELETE OBJECT phDataset .

        /* Ignore error:
           Cannot delete a BY-REFERENCE PARAMETER dataset or table in the called procedure. (12327) */
        CATCH err AS Progress.Lang.SysError:
            IF err:GetMessageNum (1) = 12327 THEN .
            ELSE UNDO, THROW err .
        END CATCH.

        FINALLY:
            IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
                FrameworkSettings:WaitStateManager:ClearWaitState ("InvokeMethod ":U + pcMethodName, THIS-OBJECT) .

        END FINALLY.

    END METHOD.

    /**
     * Purpose: Invokes the ParentPositionChanged event handler in all connected
     *          SmartDataAdapter instances
     * Notes:   SCL-402: Clears pending cPreviousFetchQueryString in linked
     *          SmartDatasetChildAdpater instances
     * @param sender The reference to the sender associated with the event
     * @param e The reference to the System.EventArgs with the data for the event
     */
    METHOD OVERRIDE PROTECTED VOID InvokeParentPositionChanged (sender AS System.Object,
                                                                e AS System.EventArgs):


        THIS-OBJECT:ClearPreviousQueryString (FALSE) .

        SUPER:InvokeParentPositionChanged (sender, e).

    END METHOD.

    /**
     * Purpose: Launches the DatatasetController
     * Notes:   Throws an error when the DatasetController is already running.
     */
    METHOD PUBLIC VOID LaunchDatasetController ():

        DEFINE VARIABLE oInstance AS IDatasetController NO-UNDO.

        IF VALID-OBJECT (THIS-OBJECT:DatasetController) THEN
            UNDO, THROW NEW AppError ("Unable to launch DatasetController instance when there is already an instance associated with this instance."{&TRAN}, 0) .

        oInstance = DYNAMIC-NEW (THIS-OBJECT:DatasetControllerType) () .

        THIS-OBJECT:DatasetController = oInstance .

    END METHOD.

    /**
     * Purpose: Invokes the MERGE-CHANGES after data was submitted to the backend
     * Notes:
     * @param phChangesDataset The handle of the dataset with the changes returned back from the backend
     * @param phDataset The handle of the dataset to merge the changes into
     */
    METHOD PROTECTED VOID MergeChanges (phChangesDataset AS HANDLE,
                                        phDataset AS HANDLE):

        DEFINE VARIABLE o AS MergeChangesEventArgs NO-UNDO .

        o = NEW MergeChangesEventArgs (phChangesDataset,
                                       phDataset) .

        THIS-OBJECT:OnBeforeMergeChanges (o) .

        phChangesDataset:MERGE-CHANGES (phDataset, TRUE) .

        o = NEW MergeChangesEventArgs (phChangesDataset,
                                       phDataset) .

        THIS-OBJECT:OnAfterMergeChanges (o) .

    END METHOD .

    /**
     * Purpose: Factory method for IFetchDataRequest objects
     * Notes:   Allows creation of alternative implementations of IFetchDataRequest
     *          Used by RetrieveData and OffEndHandler in preparation of calls to
     *          the ServiceAdapter
     * @param pcTables The tables to be requested
     * @param pcQueries The queries to be requested
     * @param pcRequests The requests parameter
     * @param piNumRecords The number of records to be requested
     * @param pcContext The context to be passed to the Business Entity
     * @return The new IFetchDataRequest instance
     */
    METHOD PUBLIC Consultingwerk.OERA.IFetchDataRequest NewFetchDataRequest (pcTables AS CHARACTER,
                                                                             pcQueries AS CHARACTER,
                                                                             pcRequests AS CHARACTER,
                                                                             piNumRecords AS INTEGER,
                                                                             pcContext AS CHARACTER):

        RETURN NEW FetchDataRequest (pcTables,
                                     pcQueries,
                                     pcRequests,
                                     piNumRecords,
                                     pcContext) .

    END METHOD.

    /**
     * Purpose: Event handler for the OffEnd event of the ProBindingSource object
     * Notes:   Overrides OffEndHandler in SmartDataAdapter base class
     * @param sender The reference to the object that raised the event
     * @param e The OffEndEventArgs with the data for the event
     */
    METHOD OVERRIDE PROTECTED VOID OffEndHandler (INPUT sender AS System.Object,
                                                  INPUT e AS Progress.Data.OffEndEventArgs ):

        DEFINE VARIABLE cEntity           AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cTables           AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cQueries          AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cJoins            AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iNumRecords       AS INTEGER   NO-UNDO.

        DEFINE VARIABLE iNumResults       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hNextBatch        AS HANDLE    NO-UNDO.

        DEFINE VARIABLE iPos              AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lWaitStateActive  AS LOGICAL   NO-UNDO.

        DEFINE VARIABLE hCountQuery       AS HANDLE    NO-UNDO.

        DEFINE VARIABLE oFetchDataRequest AS Consultingwerk.OERA.IFetchDataRequest NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 30.09.2009
           Don't fetch next batch when saving data */
        IF lSaving THEN
            RETURN .

        IF THIS-OBJECT:FetchQueryString > "":U THEN .
        ELSE THIS-OBJECT:FetchQueryString = THIS-OBJECT:QueryString .

        IF VALID-OBJECT (THIS-OBJECT:FetchFilterValues) AND NOT THIS-OBJECT:FetchFilterValues:IsEmpty THEN
            ASSIGN cQueries = THIS-OBJECT:MergeQueryStrings (THIS-OBJECT:FetchQueryString,
                                                             THIS-OBJECT:FetchFilterValues) .
        ELSE
            ASSIGN cQueries = THIS-OBJECT:FetchQueryString + FILL(CHR(1),
                                                                  NUM-ENTRIES(cTables) - NUM-ENTRIES(THIS-OBJECT:FetchQueryString, CHR(1))) .

        ASSIGN
            cEntity    = THIS-OBJECT:EntityName
            cTables    = THIS-OBJECT:EntityTable + (IF THIS-OBJECT:EntityView > "":U THEN ",":U ELSE "":U) + THIS-OBJECT:EntityView
            cJoins     = THIS-OBJECT:EntityJoin
            .

        /* Mike Fechner, Consultingwerk Ltd. 01.09.2010
           Entity ContextID (instance ID) will be appended to Entity name if set. */
        IF THIS-OBJECT:ContextID > "":U THEN
            ASSIGN cEntity = cEntity + CHR (1) + THIS-OBJECT:ContextID .

        /* Mike Fechner, Consultingwerk Ltd. 26.05.2009
           Support for Batching */
        IF THIS-OBJECT:BatchSize > 0 THEN
            ASSIGN iNumRecords = THIS-OBJECT:BatchSize .

        CREATE DATASET hNextBatch .
        hNextBatch:CREATE-LIKE (hDataset, "next_batch":U) .

        /* Mike Fechner, Consultingwerk Ltd. 13.06.2009
           Indicate WaitState */
        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
            FrameworkSettings:WaitStateManager:SetWaitState ("RetrieveData":U, THIS-OBJECT) .

        ASSIGN lWaitStateActive = TRUE .

        oFetchDataRequest = NewFetchDataRequest (cTables,
                                                 cQueries,
                                                 "FIRST":U,
                                                 iNumRecords,
                                                 THIS-OBJECT:NextContext) .

        ASSIGN
          oFetchDataRequest:Joins     = cJoins
          oFetchDataRequest:Positions = "ALL":U + FILL(CHR(1) + "ALL":U, NUM-ENTRIES(cTables) - 1)
          oFetchDataRequest:CustomContext = THIS-OBJECT:CustomContext
          .

        /* Mike Fechner, Consultingwerk Ltd. 26.05.2009
           Fetch next batch by sending previous context to backend */
        THIS-OBJECT:RetrieveData (THIS-OBJECT:AppServerPartition,
                                  cEntity,
                                  oFetchDataRequest,
                                  OUTPUT DATASET-HANDLE hNextBatch BY-REFERENCE) .

        ASSIGN THIS-OBJECT:NextContext = oFetchDataRequest:NextContext .

        /* Mike Fechner, Consultingwerk Ltd. 13.06.2009
           Clear WaitState */
        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
            FrameworkSettings:WaitStateManager:ClearWaitState ("RetrieveData":U, THIS-OBJECT) .

        ASSIGN lWaitStateActive = FALSE .

        /* Mike Fechner, Consultingwerk Ltd. 26.05.2009
           When THIS-OBJECT:NextContext returned from server, activate Batching */
        IF ENTRY(1, THIS-OBJECT:NextContext, CHR(1)) > "":U THEN
            THIS-OBJECT:BindingSource:Batching = TRUE .
        ELSE
            THIS-OBJECT:BindingSource:Batching = FALSE .

        /* Mike Fechner, Consultingwerk Ltd. 12.06.2009
           Bug 1839: Store updated ContextString */
        ASSIGN iPos = INDEX(THIS-OBJECT:NextContext, CHR(1)) .

        IF iPos > 0 THEN
            THIS-OBJECT:ContextString = SUBSTRING(THIS-OBJECT:NextContext, iPos + 1) .

        /* Mike Fechner, Consultingwerk Ltd. 30.09.2009
           Count records in target datasets TOP-NAV-BUFFER to prevent duplicates
           resulting from re-reading recently added rows with the next batch */
        CREATE QUERY hCountQuery .
        hCountQuery:SET-BUFFERS (hDataset:GET-BUFFER-HANDLE (THIS-OBJECT:EntityTable)) .
        hCountQuery:QUERY-PREPARE ("PRESELECT EACH ":U + hDataset:GET-BUFFER-HANDLE (THIS-OBJECT:EntityTable):NAME) .
        hCountQuery:QUERY-OPEN () .

        ASSIGN iNumResults = hCountQuery:NUM-RESULTS .

        hDataset:COPY-DATASET (hNextBatch, FALSE, TRUE) .

        THIS-OBJECT:QueryHandle:QUERY-OPEN() .

        /* Mike Fechner, Consultingwerk Ltd. 30.09.2009
           Count no. of Rows added based on num results of received dataset */
        hCountQuery:QUERY-OPEN () .

        /* Mike Fechner, Consultingwerk Ltd. 05.08.2011
           Make OffEndHandler also useable without an acutal OffEnd event */
        IF VALID-OBJECT (e) THEN
            e:RowsAdded = hCountQuery:NUM-RESULTS - iNumResults .

        OnAfterBatchAvailable (System.EventArgs:Empty) .

        FINALLY:
            IF VALID-HANDLE (hNextBatch) THEN
                DELETE OBJECT hNextBatch .

            IF VALID-HANDLE (hCountQuery) THEN
                DELETE OBJECT hCountQuery .

            IF lWaitStateActive AND VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
                FrameworkSettings:WaitStateManager:ClearWaitState ("RetrieveData":U, THIS-OBJECT) .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Fetches the Previous Batch of records
     * Notes:
     */
    METHOD PUBLIC VOID OffHome ():

        DEFINE VARIABLE cEntity           AS CHARACTER                            NO-UNDO.
        DEFINE VARIABLE cTables           AS CHARACTER                            NO-UNDO.
        DEFINE VARIABLE cQueries          AS CHARACTER                            NO-UNDO.
        DEFINE VARIABLE cJoins            AS CHARACTER                            NO-UNDO.
        DEFINE VARIABLE iNumRecords       AS INTEGER                              NO-UNDO.
        DEFINE VARIABLE oBrowser          AS ISmartDataBrowser                    NO-UNDO .

        DEFINE VARIABLE hPrevBatch        AS HANDLE                               NO-UNDO.

        DEFINE VARIABLE iPos              AS INTEGER                              NO-UNDO.
        DEFINE VARIABLE lWaitStateActive  AS LOGICAL                              NO-UNDO.

        DEFINE VARIABLE oFetchDataRequest AS Consultingwerk.OERA.IFetchDataRequest NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 30.09.2009
           Don't fetch next batch when saving data */
        IF lSaving THEN
            RETURN .

        IF THIS-OBJECT:PrevContext > "":U THEN .
        ELSE
            RETURN .

        IF THIS-OBJECT:FetchQueryString > "":U THEN .
        ELSE THIS-OBJECT:FetchQueryString = THIS-OBJECT:QueryString .

        IF VALID-OBJECT (THIS-OBJECT:FetchFilterValues) AND NOT THIS-OBJECT:FetchFilterValues:IsEmpty THEN
            ASSIGN cQueries = THIS-OBJECT:MergeQueryStrings (THIS-OBJECT:FetchQueryString,
                                                             THIS-OBJECT:FetchFilterValues) .
        ELSE
            cQueries = THIS-OBJECT:FetchQueryString + FILL(CHR(1),
                                                           NUM-ENTRIES(cTables) - NUM-ENTRIES(THIS-OBJECT:FetchQueryString, CHR(1))) .

        ASSIGN
            cEntity    = THIS-OBJECT:EntityName
            cTables    = THIS-OBJECT:EntityTable + (IF THIS-OBJECT:EntityView > "":U THEN ",":U ELSE "":U) + THIS-OBJECT:EntityView
            cJoins     = THIS-OBJECT:EntityJoin
            .

        /* Mike Fechner, Consultingwerk Ltd. 01.09.2010
           Entity ContextID (instance ID) will be appended to Entity name if set. */
        IF THIS-OBJECT:ContextID > "":U THEN
            ASSIGN cEntity = cEntity + CHR (1) + THIS-OBJECT:ContextID .

        oBrowser = THIS-OBJECT:FindLinkedSmartDataBrowser () .

        /* Mike Fechner, Consultingwerk Ltd. 26.05.2009
           Support for Batching */
        IF THIS-OBJECT:BatchSize > 0 THEN
            ASSIGN iNumRecords = THIS-OBJECT:BatchSize * -1.

        CREATE DATASET hPrevBatch .
        hPrevBatch:CREATE-LIKE (hDataset, "prev_batch":U) .

        /* Mike Fechner, Consultingwerk Ltd. 13.06.2009
           Indicate WaitState */
        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
            FrameworkSettings:WaitStateManager:SetWaitState ("RetrieveData":U, THIS-OBJECT) .

        ASSIGN lWaitStateActive = TRUE .

        oFetchDataRequest = NewFetchDataRequest (cTables,
                                                 cQueries,
                                                 "FIRST":U,
                                                 iNumRecords,
                                                 THIS-OBJECT:PrevContext) .

        ASSIGN
          oFetchDataRequest:Joins     = cJoins
          oFetchDataRequest:Positions = "ALL":U + FILL(CHR(1) + "ALL":U, NUM-ENTRIES(cTables) - 1)
          oFetchDataRequest:CustomContext = THIS-OBJECT:CustomContext
          .

        /* Mike Fechner, Consultingwerk Ltd. 31.01.2011
           Fetch pprevious batch by sending previous context to backend */
        THIS-OBJECT:RetrieveData (THIS-OBJECT:AppServerPartition,
                                  cEntity,
                                  oFetchDataRequest,
                                  OUTPUT DATASET-HANDLE hPrevBatch BY-REFERENCE) .

        ASSIGN THIS-OBJECT:PrevContext = oFetchDataRequest:PrevContext .

        /* Mike Fechner, Consultingwerk Ltd. 13.06.2009
           Clear WaitState */
        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
            FrameworkSettings:WaitStateManager:ClearWaitState ("RetrieveData":U, THIS-OBJECT) .

        ASSIGN lWaitStateActive = FALSE .

        /* Mike Fechner, Consultingwerk Ltd. 12.06.2009
           Bug 1839: Store updated ContextString */
        ASSIGN iPos = INDEX(THIS-OBJECT:PrevContext, CHR(1)) .

        IF iPos > 0 THEN
            THIS-OBJECT:ContextString = SUBSTRING(THIS-OBJECT:PrevContext, iPos + 1) .


        hDataset:COPY-DATASET (hPrevBatch, FALSE, TRUE) .

        THIS-OBJECT:QueryHandle:QUERY-OPEN() .

        THIS-OBJECT:BindingSource:RefreshAll () .

        IF VALID-OBJECT (oBrowser) THEN
            oBrowser:ScrollToCurrentRow (TRUE) .

        OnAfterBatchAvailable (System.EventArgs:Empty) .

        FINALLY:
            IF VALID-HANDLE (hPrevBatch) THEN
                DELETE OBJECT hPrevBatch .

            IF lWaitStateActive AND VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
                FrameworkSettings:WaitStateManager:ClearWaitState ("RetrieveData":U, THIS-OBJECT) .
        END FINALLY.


    END METHOD.

    /**
     * Purpose: Raises the AfterAutoCommitTransaction event
     * Notes:
     * @param e A System.EventArgs with the Event-Data
     */
    METHOD PROTECTED VOID OnAfterAutoCommitTransaction (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty.

        THIS-OBJECT:AfterAutoCommitTransaction:Publish (THIS-OBJECT, e).

    END METHOD .

    /**
     * Purpose: Raises the AfterBatchAvailable event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnAfterBatchAvailable (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
          ASSIGN e = System.EventArgs:Empty .

        THIS-OBJECT:AfterBatchAvailable:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the AfterCommitTransaction event
     * Notes:
     * @param e A System.EventArgs with the Event-Data
     */
    METHOD PROTECTED VOID OnAfterCommitTransaction (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty.

        THIS-OBJECT:AfterCommitTransaction:Publish (THIS-OBJECT, e).

    END METHOD .

    /**
     * Purpose: Raises the AfterGetChangesEvent
     * Notes:
     * @param e The AfterGetChangesEventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnAfterGetChanges (e AS AfterGetChangesEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "AfterGetChanges":U) .

        THIS-OBJECT:AfterGetChanges:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raises the AfterMergeChanges
     * Notes:
     * @param e The MergeChangesEventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnAfterMergeChanges (e AS MergeChangesEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "AfterMergeChanges":U) .

        THIS-OBJECT:AfterMergeChanges:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raises the AfterRejectTransaction event
     * Notes:
     * @param e A System.EventArgs with the Event-Data
     */
    METHOD PROTECTED VOID OnAfterRejectTransaction (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty.

        THIS-OBJECT:AfterRejectTransaction:Publish (THIS-OBJECT, e).

    END METHOD .

    /**
     * Purpose: Raises the AutoCommitChanged property
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnAutoCommitChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:AutoCommitChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the BeforeCommitTransaction event
     * Notes:
     * @param e The CancelEventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnBeforeCommitTransaction (e AS System.ComponentModel.CancelEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "BeforeCommitTransaction":U) .

        THIS-OBJECT:BeforeCommitTransaction:Publish (THIS-OBJECT, e).

    END METHOD .

    /**
     * Purpose: Raises the BeforeGetChangesEvent
     * Notes:
     * @param e The BeforeGetChangesEventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnBeforeGetChanges (e AS BeforeGetChangesEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "BeforeGetChanges":U) .

        THIS-OBJECT:BeforeGetChanges:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raises the BeforeMergeChanges event
     * Notes:
     * @param e The MergeChangesEventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnBeforeMergeChanges (e AS MergeChangesEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "BeforeMergeChanges":U) .

        THIS-OBJECT:BeforeMergeChanges:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raises the BeforeRejectTransaction event
     * Notes:
     * @param e The CancelEventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnBeforeRejectTransaction (e AS System.ComponentModel.CancelEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "BeforeRejectTransaction":U) .

        THIS-OBJECT:BeforeRejectTransaction:Publish (THIS-OBJECT, e).

    END METHOD .

    /**
     * Purpose: Raises the NewFetchDataRequest
     * Notes:
     * @param e The NewFetchDataRequestEventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnNewFetchDataRequest (e AS NewFetchDataRequestEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "NewFetchDataRequest":U) .

        THIS-OBJECT:NewFetchDataRequest:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raises the FindCurrentRecordAfterCommitChanges
     * Notes:
     * @param e The BufferHandleEventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnFindCurrentRecordAfterCommitChanges (e AS BufferHandleEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "FindCurrentRecordAfterCommitChanges":U) .

        THIS-OBJECT:FindCurrentRecordAfterCommitChanges:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raises the FindDataAdapterForCommitErrorHandling
     * Notes:
     * @param e The FindDataAdapterForCommitErrorHandlingEventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnFindDataAdapterForCommitErrorHandling (e AS FindDataAdapterForCommitErrorHandlingEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "FindDataAdapterForCommitErrorHandling":U) .

        THIS-OBJECT:FindDataAdapterForCommitErrorHandling:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raises the FindUpdatingDataTargetForCommitErrorHandling
     * Notes:
     * @param e The FindUpdatingDataTargetForCommitErrorHandlingEventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnFindUpdatingTableIOTargetForCommitErrorHandling (e AS FindUpdatingTableIOTargetForCommitErrorHandlingEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "FindUpdatingDataTargetForCommitErrorHandling":U) .

        THIS-OBJECT:FindUpdatingTableIOTargetForCommitErrorHandling:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raises the HandleValidationError
     * Notes:
     * @param e The HandleValidationErrorEventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnHandleValidationError (e AS HandleValidationErrorEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "HandleValidationError":U) .

        THIS-OBJECT:HandleValidationError:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raises the SmartCommitSourceChanged event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnSmartCommitSourceChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:SmartCommitSourceChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the SmartTransactionStateChanged event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnSmartTransactionStateChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:SmartTransactionStateChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Event Handler method for Designer Verbs
     * Notes:
     * @param pcVerbText The name (Label) of the Designer Verb that was invoked
     */
    METHOD PUBLIC OVERRIDE VOID OnVerbClicked (pcVerbText AS CHARACTER):

        IF NOT THIS-OBJECT:DesignTime THEN
            RETURN .

        CASE pcVerbText:
            WHEN "Select Tables":U THEN
                ShowTablePickerDialog() .
            WHEN "Select BusinessEntity":U THEN
                ShowBusinessEntityPickerDialog () .
            WHEN "Select DatasetController":U THEN
                ShowDatasetControllerDialog () .
            WHEN "Reload BusinessEntities":U THEN
                BusinessEntityDesignerSupport:RestartBusinessEntities () .
            WHEN "Create BindingSource":U THEN
                BusinessEntityDesignerSupport:CreateBindingSourceFromAdapter (THIS-OBJECT,
                                                                              THIS-OBJECT:GetDesignTimeDataSourceClassName()) .
        END CASE .

        SUPER:OnVerbClicked (pcVerbText) .

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.
    END METHOD .

    /**
     * Purpose: Method used to prepare the BindingSource:Handle object (QUERY or ProDataset).
     * Notes:   Implemented in the SmartBusinessEntityAdapter and SmartDatasetChildAdapter
     *          Called from RetrieveData or CreateRecord (when called before RetrieveData)
     */
    METHOD OVERRIDE PROTECTED VOID PrepareDataBinding ():

        /* Used to ignore duplicate field names from joined tables */
        DEFINE VARIABLE cIncludeList          AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cExceptList           AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.

        ASSIGN hDataset = THIS-OBJECT:DataSet .

        IF THIS-OBJECT:BindTo = "QUERY":U THEN DO:
            THIS-OBJECT:QueryHandle = SmartBusinessEntityQuerySupport:PrepareBindingQuery (hDataset,
                                                               THIS-OBJECT:EntityTable,
                                                               THIS-OBJECT:EntityView,
                                                               THIS-OBJECT:EntityJoin,
                                                               TRUE,
                                                               OUTPUT cExceptList) .

            IF cExceptList > "":U THEN
                IF NOT VALID-OBJECT(THIS-OBJECT:BindingSource:TableSchema) THEN
                    THIS-OBJECT:BindingSource:SetFields("*":U, cExceptList, "":U) .
        END.
        ELSE DO:
            IF NOT VALID-OBJECT (THIS-OBJECT:BindingSource:TableSchema) THEN DO:
                IF THIS-OBJECT:BindingSourceIncludeFields > "":U OR THIS-OBJECT:BindingSourceExcludeFields > "":U THEN DO:
                    IF THIS-OBJECT:BindingSourceExcludeFields > "":U THEN
                        ASSIGN cIncludeList = "*":U
                               cExceptList = TRIM(cExceptList + ",":U + THIS-OBJECT:BindingSourceExcludeFields, ",":U).
                    ELSE
                        cIncludeList = THIS-OBJECT:BindingSourceIncludeFields .

                    THIS-OBJECT:BindingSource:SetFields(cIncludeList, cExceptList, "":U) .
                END.
            END.

            ASSIGN THIS-OBJECT:QueryHandle = hDataset:TOP-NAV-QUERY (1) .
        END.

    END METHOD.

    /**
     * Purpose: Returns true if a previous batch is available.
     * Notes:
     * @return Logical value indicating if a previous batch is available
     */
    METHOD PUBLIC LOGICAL PreviousBatchAvailable ():

        IF THIS-OBJECT:PrevContext > "":U THEN
            RETURN TRUE .

        RETURN FALSE .

    END METHOD.

    /**
     * Purpose: Refetches the current row from the backend and merges it with the
     *          frontend dataset
     * Notes:
     */
    METHOD PUBLIC VOID RefetchCurrentRow ():

        THIS-OBJECT:RefetchCurrentRow ("":U) .

    END METHOD.

    /**
     * Purpose: Refetches the current row from the backend and merges it with the
     *          frontend dataset
     * Notes:   The pcViewTables parameter is evaluated as the Tables parameter of
     *          the FetchDataRequest class
     * @param pcViewTables The comma delimited list of tables to refetch in addition to the EntityTable
     */
    METHOD PUBLIC VOID RefetchCurrentRow (pcViewTables AS CHARACTER):

        DEFINE VARIABLE oRequest        AS FetchDataRequest NO-UNDO .
        DEFINE VARIABLE hDataset        AS HANDLE           NO-UNDO .
        DEFINE VARIABLE hBuffer         AS HANDLE           NO-UNDO .
        DEFINE VARIABLE cTables         AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE cFindString     AS CHARACTER        NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 04.07.2009
           GET-CHANGES and REJECT-CHANGES might have invalidated Query position */
        RepositionCurrentRow () .

        /* Update Dataset from BindingSource */
        /* Mike Fechner, Consultingwerk Ltd. 13.07.2009
           Only assign the fields of the EntityTable,
           Assign will assign all fields from all tables. */
        ASSIGN hBuffer = THIS-OBJECT:DATASET:GET-BUFFER-HANDLE (THIS-OBJECT:EntityTable) .

        IF NOT hBuffer:AVAILABLE THEN
            RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 26.11.2015
           Can't refresh modified row as we'd be loosing pending changes. */
        IF hBuffer:ROW-STATE <> ROW-UNMODIFIED THEN
            UNDO, THROW NEW UnableToRefreshCurrentRowException ("Unable to refresh current row due to modifications."{&TRAN},
                                                                0) .

        IF pcViewTables > "":U THEN
            ASSIGN cTables = SUBSTITUTE ("&1,&2":U,
                                         THIS-OBJECT:EntityTable,
                                         pcViewTables) .
        ELSE
            ASSIGN cTables = THIS-OBJECT:EntityTable .

        ASSIGN cFindString = BufferHelper:UniqueFindPredicate (hBuffer) .

        oRequest = NEW FetchDataRequest (cTables,
                                         SUBSTITUTE ("for each &1 &2":U,
                                                     THIS-OBJECT:EntityTable,
                                                     cFindString),
                                                     1) .

        THIS-OBJECT:RetrieveData (THIS-OBJECT:AppServerPartition,
                                  THIS-OBJECT:EntityName,
                                  oRequest,
                                  OUTPUT DATASET-HANDLE hDataset) .

        THIS-OBJECT:DataSet:COPY-DATASET (hDataset, FALSE, TRUE) .

        RepositionCurrentRow () .

        hBuffer:FIND-FIRST (cFindString) .

        THIS-OBJECT:BindingSource:Refresh () .

        IF pcViewTables > "":U THEN DO:
            {Consultingwerk/foreachABL.i ISmartDataTarget oTarget in THIS-OBJECT:SmartDataTargets}
                IF TYPE-OF (oTarget, SmartDatasetChildAdapter) THEN
                    CAST (oTarget, SmartDatasetChildAdapter):RetrieveData () .
            END.
        END.

        THIS-OBJECT:OnCurrentChanged (System.EventArgs:Empty) .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Reverts the changes of a pending transaction
     * Notes:
     */
    METHOD PUBLIC VOID RejectTransaction ():

        DEFINE VARIABLE e       AS System.ComponentModel.CancelEventArgs NO-UNDO .
        DEFINE VARIABLE roRowid AS ROWID                                 EXTENT NO-UNDO .

        IF THIS-OBJECT:AutoCommit = TRUE THEN
            UNDO, THROW NEW AppError
                              (SmartComponentLibraryCustomizer:SmartBusinessEntityAdapter_CommitRejectWithAutoCommit,
                               0) .

        /* Mike Fechner, Consultingwerk Ltd. 22.05.2013
           Raise BeforeCommitTransaction event */
        e = NEW System.ComponentModel.CancelEventArgs () .

        THIS-OBJECT:OnBeforeRejectTransaction (e) .

        IF VALID-OBJECT (e) AND e:Cancel THEN
            RETURN .

        roRowid = DatasetHelper:GetDatasetRowids (THIS-OBJECT:DataSet) .

        /* 1. Cancel current update (and check that it's cancelled) */
        /* Save Pending updates in one of the SmartDataTargets */
        THIS-OBJECT:CancelInDataTargets () .

        IF THIS-OBJECT:SmartTransactionState <> TransactionStateEnum:PendingChanges THEN
            RETURN .

        IF NOT Consultingwerk.Util.DatasetHelper:HasChanges (THIS-OBJECT:Dataset) THEN DO:
            THIS-OBJECT:SmartTransactionState = TransactionStateEnum:NoPendingChanges .

            RETURN .
         END.

        THIS-OBJECT:DataSet:REJECT-CHANGES () .

        THIS-OBJECT:SmartTransactionState = TransactionStateEnum:NoPendingChanges .

        THIS-OBJECT:RefreshAfterRejectTranscation (roRowid) .

        THIS-OBJECT:OnAfterRejectTransaction (System.EventArgs:Empty) .

        IF lEndUpdateStateDSPending = TRUE AND
           VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND
           TYPE-OF (THIS-OBJECT:SmartDataSource, SmartDataAdapter) THEN DO:
            THIS-OBJECT:SmartDataSource:EndUpdateState (CAST (THIS-OBJECT:SmartDataSource, SmartDataAdapter):UpdatingSmartDataTarget) .
            ASSIGN lEndUpdateStateDSPending = FALSE .
        END.

    END METHOD.

    /**
     * Purpose: Repositions to the updated record after update
     * Notes:
     * @param proRowid The rowid of the record to position to
     * @param plCreated Logical value indicating if the current record was created
     */
    METHOD PROTECTED OVERRIDE VOID RepositionAfterUpdateRow (proRowid AS ROWID,
                                                             plCreated AS LOGICAL):

        DEFINE VARIABLE hBuffer                      AS HANDLE                                    NO-UNDO .
        DEFINE VARIABLE hQueryBuffer                 AS HANDLE                                    NO-UNDO .
        DEFINE VARIABLE cFindString                  AS CHARACTER                                 NO-UNDO .
        DEFINE VARIABLE eRepositionEventArgs         AS RepositionAfterUpdateRowEventArgs         NO-UNDO .
        DEFINE VARIABLE eUnableToRepositionEventArgs AS UnableToRepositionAfterUpdateRowEventArgs NO-UNDO .

        IF THIS-OBJECT:AutoCommit = FALSE OR plCreated = FALSE OR THIS-OBJECT:BatchSize = 0 THEN
            SUPER:RepositionAfterUpdateRow (proRowid, plCreated) .
        /* SCL-1061: As the new row may be in a different batch than the current one,
           Use FindRowWhere (FindOnServerOnly) to locate the new row */
        ELSE DO ON ERROR UNDO, THROW:
            /* SCL-1513: Additional pre-execution event */
            eRepositionEventArgs = NEW RepositionAfterUpdateRowEventArgs (THIS-OBJECT:QueryHandle,
                                                                          proRowid,
                                                                          plCreated) .

            THIS-OBJECT:OnRepositionAfterUpdateRow (eRepositionEventArgs) .

            IF VALID-OBJECT (eRepositionEventArgs) AND eRepositionEventArgs:Handled = TRUE THEN
                RETURN .

            ASSIGN hQueryBuffer = THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE (1) .

            CREATE BUFFER hBuffer FOR TABLE hQueryBuffer .

            hBuffer:FIND-BY-ROWID (proRowid) .

            ASSIGN cFindString = BufferHelper:UniqueFindPredicate (hBuffer) .

            THIS-OBJECT:FindRowWhere (cFindString, FindRowWhereModeEnum:FindOnServerOnly) .

            CATCH err AS Progress.Lang.Error:
                /* Mike Fechner, Consultingwerk Ltd. 01.11.2014
                   SCL-500: Event raised to allow custom behavior when we are no longer able to
                            reposition to the updated record */
                eUnableToRepositionEventArgs = NEW UnableToRepositionAfterUpdateRowEventArgs (THIS-OBJECT:QueryHandle,
                                                                                              proRowid) .

                THIS-OBJECT:OnUnableToRepositionAfterUpdateRow (eUnableToRepositionEventArgs) .

                IF VALID-OBJECT (eUnableToRepositionEventArgs) AND eUnableToRepositionEventArgs:Handled = FALSE THEN
                    Consultingwerk.Util.MessageFormHelper:ShowMessage
                         (SUBSTITUTE ("Error during repositioning to updated record.&1&2"{&TRAN},
                                      Consultingwerk.Environment:NewLine,
                                      err:GetMessage (1)),
                          "Error during update row"{&TRAN},
                          SUBSTITUTE ("Current query string: &1&2Find-String: &3"{&TRAN},
                                      THIS-OBJECT:QueryString,
                                      Consultingwerk.Environment:NewLine,
                                      cFindString),
                          Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageError) .
            END CATCH.
       END.

       FINALLY:
           GarbageCollectorHelper:DeleteObject (hBuffer) .
       END FINALLY.

    END METHOD .

    /**
     * Purpose: Bundles calls to ServiceAdapter:RetrieveData
     * Notes:   Simplifies modifications on the ServiceAdapter / ServiceInterface
     *          as we can centrally add or modify parameters
     * @param pcPartition The AppServer partition to be used for this call
     * @param pcEntity The name of the Business Entity
     * @param poFetchDataRequest The IFetchDataRequest with the parameter for this call
     * @param phDataset OUTPUT DATASET-HANDLE The result set
     */
    METHOD PROTECTED VOID RetrieveData (pcPartition AS CHARACTER,
                                        pcEntity AS CHARACTER,
                                        poFetchDataRequest AS IFetchDataRequest,
                                        OUTPUT DATASET-HANDLE phDataSet):

        DEFINE VARIABLE oServiceAdapter AS Consultingwerk.OERA.IServiceAdapter NO-UNDO .
        DEFINE VARIABLE hContextDataset AS HANDLE                              NO-UNDO .

        IF VALID-OBJECT (poFetchDataRequest) AND THIS-OBJECT:StopAfter <> ? THEN
            poFetchDataRequest:StopAfter = THIS-OBJECT:StopAfter .

        IF VALID-OBJECT (poFetchDataRequest) THEN
            poFetchDataRequest:NamedQuery = THIS-OBJECT:NamedQuery .

        OnNewFetchDataRequest (NEW NewFetchDataRequestEventArgs (poFetchDataRequest)) .

        oServiceAdapter = THIS-OBJECT:ServiceAdapter .

        IF VALID-HANDLE (THIS-OBJECT:ContextDataset) AND
           TYPE-OF (oServiceAdapter,
                    IServiceAdapterWithContextDataset) THEN DO ON ERROR UNDO, THROW:

            ASSIGN hContextDataset = THIS-OBJECT:ContextDataset .

            CAST(oServiceAdapter,
                 IServiceAdapterWithContextDataset):RetrieveData (pcPartition,
                                                                  pcEntity,
                                                                  poFetchDataRequest,
                                                                  OUTPUT DATASET-HANDLE phDataset BY-REFERENCE,
                                                                  hContextDataset) .
        END.
        ELSE
            oServiceAdapter:RetrieveData (pcPartition,
                                          pcEntity,
                                          poFetchDataRequest,
                                          OUTPUT DATASET-HANDLE phDataset BY-REFERENCE) .

    END METHOD.

    /**
     * Purpose: Sets properties that affect the Visual Desginer
     * Notes:
     */
    METHOD PRIVATE VOID SetDesignerProperties ():

        THIS-OBJECT:DesignerVerbs = "Reload BusinessEntities,Select BusinessEntity,Select DatasetController,Select Tables,Create BindingSource":U .

    END METHOD.

    /**
     * Purpose: Overridable Setter for QuerySort property
     * Notes:
     * @param arg The new value for the QuerySort property
     */
    METHOD PROTECTED OVERRIDE VOID SetQuerySort (arg AS CHARACTER):

        DEFINE VARIABLE cQueryString     AS CHARACTER         NO-UNDO.
        DEFINE VARIABLE cNewQueryString  AS CHARACTER         NO-UNDO.
        DEFINE VARIABLE lPositionWasZero AS LOGICAL           NO-UNDO INIT FALSE .
        DEFINE VARIABLE iBatchSize       AS INTEGER           NO-UNDO .
        DEFINE VARIABLE oBrowser         AS ISmartDataBrowser NO-UNDO .

        IF arg > "":U THEN .
        ELSE RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 01.03.2011
           Do nothing, when actually the same sort order is set again */
        IF TRIM (arg) = TRIM (cQuerySort) AND THIS-OBJECT:FetchQueryString > "":U THEN
            RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 01.10.2013
           Bug 2836: Setting sort indicator on SmartDataBrowser */
        oBrowser = THIS-OBJECT:FindLinkedSmartDataBrowser () .

        IF VALID-OBJECT (oBrowser) THEN
            oBrowser:UpdateSortIndicator (arg) .

        ASSIGN iBatchSize = THIS-OBJECT:BatchSize .

        IF iBatchSize > 0 THEN DO:
            IF THIS-OBJECT:FetchQueryString > "":U THEN
                cQueryString = ENTRY(1, THIS-OBJECT:FetchQueryString, CHR(1)) .
            ELSE
                cQueryString = ENTRY(1, THIS-OBJECT:QueryString, CHR(1)) .

            IF INDEX (cQueryString, " BY ":U) > 0 THEN
                cQueryString = SUBSTRING (cQueryString, 1, INDEX (cQueryString, " BY ":U) - 1) .

            IF TRIM(cQueryString) = "":U THEN DO:
.                IF CharacterType:IsNullOrEmpty(THIS-OBJECT:EntityTable) THEN
                    UNDO, THROW NEW SmartBusinessEntityAdapterException ("Cannot set QueryString when EntityTable is not set."{&TRAN}, 0) .

                cQueryString = "FOR EACH ":U + THIS-OBJECT:EntityTable .
            END.

            ASSIGN cQuerySort   = arg
                   cQueryString = SUBSTITUTE ("&1 &2":U,
                                              cQueryString,
                                              cQuerySort) .

            IF THIS-OBJECT:FetchQueryString > "":U THEN
                ASSIGN cNewQueryString                   = THIS-OBJECT:FetchQueryString
                       ENTRY(1, cNewQueryString, CHR(1)) = cQueryString
                       THIS-OBJECT:FetchQueryString      = cNewQueryString .
            ELSE
                ASSIGN cNewQueryString                   = THIS-OBJECT:QueryString
                       ENTRY(1, cNewQueryString, CHR(1)) = cQueryString
                       THIS-OBJECT:FetchQueryString      = cNewQueryString .

            /* Mike Fechner, Consultingwerk Ltd. 01.03.2011
               When QuerySort is set in the PropertyGrid we shouldn't retrieve data yet */
            IF (NOT THIS-OBJECT:Initializing) AND THIS-OBJECT:RetrieveDataOnChangeSortOrder THEN
                THIS-OBJECT:RetrieveData () .

            RETURN .
        END.
        ELSE
            IF NOT VALID-OBJECT (THIS-OBJECT:BindingSource) OR THIS-OBJECT:BindingSource:Position = 0 THEN
                ASSIGN lPositionWasZero = TRUE .

        ASSIGN cQuerySort = arg .

        IF NOT VALID-HANDLE (THIS-OBJECT:QueryHandle) THEN
            RETURN .

        ASSIGN cQueryString = THIS-OBJECT:QueryHandle:PREPARE-STRING .

        IF INDEX (cQueryString, " BY ":U) > 0 THEN
            cQueryString = SUBSTRING (cQueryString, 1, INDEX (cQueryString, " BY ":U) - 1) .

        cQueryString = SUBSTITUTE ("&1 &2":U, cQueryString, cQuerySort) .

        THIS-OBJECT:QueryHandle:QUERY-CLOSE () .

        DO ON ERROR UNDO, THROW:
            THIS-OBJECT:QueryHandle:QUERY-PREPARE (cQueryString)  .

            CATCH querypreperr AS Progress.Lang.Error:
                UNDO, THROW Exception:FromErrorMessageAndError (SUBSTITUTE ("Error opening Query: &1"{&TRAN}, cQueryString),
                                                                0,
                                                                querypreperr) .
            END CATCH.
        END.

        /* Mike Fechner, Consultingwerk Ltd. 25.09.2009
           Catch and ignore Error 91, all other errors will be thrown for
           further handling.
           ** No  record is available. (91)
           ** Kein  Satz verfügbar. (91)
           Raised by Query-Open (probably from within the ProBindingSource).
           Errorstack shows IMPLICIT as line number */
        DO ON ERROR UNDO, THROW:
            THIS-OBJECT:QueryHandle:QUERY-OPEN () .

            CATCH sysex AS Progress.Lang.SysError :
                IF sysex:GetMessageNum (1) = 91 THEN .
                ELSE UNDO, THROW sysex .
            END CATCH.
        END .

        THIS-OBJECT:BindingSource:RefreshAll () .

        IF iBatchSize = 0 THEN
            /* Mike Fechner, Consultingwerk Ltd. 31.10.2010
               When BatchSize is 0 and the BindingSource Postion was before and
               after the re-sort 0, notify data-targets */
            IF VALID-OBJECT (THIS-OBJECT:BindingSource) AND
               (THIS-OBJECT:BindingSource:Position = 0 AND lPositionWasZero) THEN
                InvokeParentPositionChanged (THIS-OBJECT, System.EventArgs:Empty) .

    END METHOD.

    /**
     * Purpose: Overridable Setter for QueryString property
     * Notes:   Resets the FetchQueryString property to empty string, so that the
     *          RetrieveData method rebuilds the FetchQueryString by concatenating
     *          QueryString and QuerySort
     * @param arg The new value for the QueryString property
     */
    METHOD PROTECTED VOID SetQueryString (arg AS CHARACTER):

        ASSIGN cQueryString                 = arg
               THIS-OBJECT:FetchQueryString = "":U .

    END METHOD.

    /**
     * Purpose: Assigns the internal flag that data is being saved
     * Notes:   Enforced by ISupportsSubmitChanges
     * @param plSaving The new value for the flag that data is being saved
     */
    METHOD PUBLIC OVERRIDE VOID SetSavingData (plSaving AS LOGICAL):

        lSaving = plSaving .

    END METHOD.

    /**
     * Purpose: SetSmartDataSource sets the SmartDataSource of the current instance
     *          of the SmartBusinessEntityAdapter.
     *          This Method is called from the Property Setter of SmartDataSource in
     *          the Base Class.
     *
     *          Gets the ForeignFields from the hDataset which if not read before is
     *          fetched from the Backend. Afterwards the backward handshake with the
     *          SmartDataSource is completed and the Handler for the PositionChanged
     *          Event is subscribed to be able to recognize that the Parent Data has
     *          changed.
     * Notes:   You do not need to call SetSmartDataSource () in the overriding method.
     * @param arg The SmartDataSource for this instance
     */
    METHOD PROTECTED OVERRIDE VOID SetSmartDataSource (INPUT arg AS ISmartDataSource):

        DEFINE VARIABLE i          AS INTEGER                NO-UNDO.
        DEFINE VARIABLE err        AS Progress.Lang.AppError NO-UNDO .

        IF VALID-OBJECT(arg) THEN DO:
            /* Build ForeignFields */
            IF THIS-OBJECT:ForeignFields > "":U THEN .
            ELSE DO ON ERROR UNDO, THROW:
                /* If I haven't read my dataset before, I need to read the schema */
                IF NOT VALID-HANDLE(hDataset) THEN DO:
                    ASSIGN hDataset = GetEmptyDataset() .

                    IF NOT VALID-HANDLE (hDataset:GET-BUFFER-HANDLE(THIS-OBJECT:EntityTable):PARENT-RELATION) THEN DO:
                        ASSIGN err = NEW Progress.Lang.AppError ("Unable to build ForeignFields from DataSet"{&TRAN}, 0) .
                        err:AddMessage (SUBSTITUTE("The entity table &1 has no parent relation"{&TRAN}, THIS-OBJECT:EntityTable), 0)  .

                        /* Mike Fechner, Consultingwerk Ltd. 26.08.2010
                           Ignore errors in Visual Designer */
                        IF NOT THIS-OBJECT:DesignTime THEN
                        UNDO, THROW err.
                    END.

                    THIS-OBJECT:ForeignFields = hDataset:GET-BUFFER-HANDLE(THIS-OBJECT:EntityTable):PARENT-RELATION:RELATION-FIELDS .

                    DELETE OBJECT hDataset .
                    hDataset = ? .
                END.
                ELSE DO:
                    IF NOT VALID-HANDLE (hDataset:GET-BUFFER-HANDLE(THIS-OBJECT:EntityTable):PARENT-RELATION) THEN DO:
                        ASSIGN err = NEW Progress.Lang.AppError ("Unable to build ForeignFields from DataSet"{&TRAN}, 0) .
                        err:AddMessage (SUBSTITUTE("The entity table &1 has no parent relation"{&TRAN}, THIS-OBJECT:EntityTable), 0)  .

                        /* Mike Fechner, Consultingwerk Ltd. 26.08.2010
                           Ignore errors in Visual Designer */
                        IF NOT THIS-OBJECT:DesignTime THEN
                        UNDO, THROW err.
                    END.

                    THIS-OBJECT:ForeignFields = hDataset:GET-BUFFER-HANDLE(THIS-OBJECT:EntityTable):PARENT-RELATION:RELATION-FIELDS .
                END.

                CATCH e AS Progress.Lang.SysError:
                    ASSIGN err = NEW Progress.Lang.AppError ("Unable to build ForeignFields from DataSet"{&TRAN}, 0) .

                    DO i = 1 TO e:NumMessages:
                        err:AddMessage (e:GetMessage (i), e:GetMessageNum (i)) .
                    END.

                    /* Mike Fechner, Consultingwerk Ltd. 26.08.2010
                       Ignore errors in Visual Designer */
                    IF NOT THIS-OBJECT:DesignTime THEN
                    UNDO, THROW err.
                END CATCH.
            END.
        END.

        SUPER:SetSmartDataSource (arg) .

    END METHOD.

    /**
     * Purpose: Constructor for the SmartBusinessEntityAdapter class
     * Notes:
     */
    CONSTRUCTOR PUBLIC SmartBusinessEntityAdapter ():
        SUPER ().

        THIS-OBJECT:SetDesignerProperties () .
        THIS-OBJECT:InitializeServiceAdapter () .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the SmartBusinessEntityAdapter class
     * Notes:
     * @param poContainer The IContainer to add the SmartBusinessEntityAdapter to.
     */
    CONSTRUCTOR PUBLIC SmartBusinessEntityAdapter (poContainer AS System.ComponentModel.IContainer):

        SUPER (INPUT poContainer).

        THIS-OBJECT:SetDesignerProperties() .
        THIS-OBJECT:InitializeServiceAdapter () .

    END CONSTRUCTOR.

    /**
     * Purpose: Reads Data from a Datasource and links the DataHandle
     *          with a BindingSource inside the Instance of SmartBusinessEntityAdapter.
     *
     *          If there is no valid instance of a BindigSource a new one is created.
     *          After that a Dataset is cleared and then filled with the requested
     *          data from the Database. Now a Query is build or reopened and the
     *          Handle to this Query is assigned to the BindingSource:Handle.
     *
     *          Enforced by Interface ISmartDataSource
     * Notes:   Do NOT call SUPER:RetrieveData() in the overriding method.
     *          Remember to test if a BindingSource exists. If not call
     *          CreateBindingSource from the base class.
     * @return Logical value indicating the success of the method
     */
    METHOD PUBLIC OVERRIDE LOGICAL RetrieveData ():

        DEFINE VARIABLE cEntity               AS CHARACTER                             NO-UNDO.
        DEFINE VARIABLE cTables               AS CHARACTER                             NO-UNDO.
        DEFINE VARIABLE cQueries              AS CHARACTER                             NO-UNDO.
        DEFINE VARIABLE cJoins                AS CHARACTER                             NO-UNDO.
        DEFINE VARIABLE iNumRecords           AS INTEGER                               NO-UNDO.
        DEFINE VARIABLE hFindDataset          AS HANDLE                                NO-UNDO.

        DEFINE VARIABLE cInitialContext       AS CHARACTER                             NO-UNDO.
        DEFINE VARIABLE iPos                  AS INTEGER                               NO-UNDO.

        DEFINE VARIABLE lWaitStateActive      AS LOGICAL                               NO-UNDO.
        DEFINE VARIABLE cQueryString          AS CHARACTER                             NO-UNDO.
        DEFINE VARIABLE lPositionWasZero      AS LOGICAL                               NO-UNDO INIT FALSE .
        DEFINE VARIABLE lBindingSourceCreated AS LOGICAL                               NO-UNDO INIT FALSE .
        DEFINE VARIABLE lFirstTime            AS LOGICAL                               NO-UNDO .
        DEFINE VARIABLE eCancel               AS System.ComponentModel.CancelEventArgs NO-UNDO .
        DEFINE VARIABLE oFetchDataRequest     AS Consultingwerk.OERA.IFetchDataRequest NO-UNDO .

        DEFINE VARIABLE oListQueryExpressions AS ListQueryExpressionByTable            NO-UNDO .
        DEFINE VARIABLE iPositionBeforeFind   AS INTEGER                               NO-UNDO INIT ? .

        DEFINE VARIABLE cNextContext          AS CHARACTER                             NO-UNDO.
        DEFINE VARIABLE cPrevContext          AS CHARACTER                             NO-UNDO.

        DEFINE VARIABLE oBusinessEntityDescriptorClient AS IBusinessEntityDescriptorClient       NO-UNDO .
        DEFINE VARIABLE oBusinessEntityDescriptor       AS IBusinessEntityDescriptor             NO-UNDO .
        DEFINE VARIABLE oBrowser                        AS ISmartDataBrowser                     NO-UNDO .

        IF THIS-OBJECT:HasPendingTransaction () THEN
            RETURN FALSE .

        /* Mike Fechner, Consultingwerk Ltd. 31.05.2016
           SCL-1288: Prompt to continue RetrieveData when there are pending updates */
        IF THIS-OBJECT:PromptWhenRetrieveDataInUpdateState AND THIS-OBJECT:HasPendingUpdate () THEN
            RETURN FALSE .

        eCancel = NEW System.ComponentModel.CancelEventArgs () .

        OnBeforeRetrieveData (eCancel) .

        IF VALID-OBJECT (eCancel) AND eCancel:Cancel = TRUE THEN
            RETURN FALSE .

        THIS-OBJECT:PreviousPosition = THIS-OBJECT:BindingSource:Position NO-ERROR .

        IF THIS-OBJECT:DesignTime THEN
            RETURN FALSE .

        /* If no valid BindingSource exist a new one is created right here */
        IF NOT VALID-OBJECT(THIS-OBJECT:BindingSource) THEN DO:
            CreateBindingSource ().
            ASSIGN lBindingSourceCreated = TRUE .
        END.

        IF THIS-OBJECT:BindingSource:Position > 0 THEN
            lPositionWasZero = FALSE .
        ELSE
            lPositionWasZero = TRUE .

        /* Mike Fechner, Consultingwerk Ltd. 07.07.2009
           TRACKING-CHANGES needs to be turned of during EMPTY-DATASET */
        SetTrackingChanges (FALSE) .

        ASSIGN lFirstTime = THIS-OBJECT:FirstTime .

        /* Mike Fechner, Consultingwerk Ltd. 13.09.2016
           SCL-1415 - Default Sort support from Business Entity Descriptor */
        IF lFirstTime AND THIS-OBJECT:QuerySort = "":U THEN DO:
            oBusinessEntityDescriptorClient = {Consultingwerk/get-service.i Consultingwerk.OERA.BusinessEntityDescriptor.IBusinessEntityDescriptorClient
                                                                            "NEW BusinessEntityDescriptorClient ()"} .

            oBusinessEntityDescriptor = oBusinessEntityDescriptorClient:GetBusinessEntityDescriptor (THIS-OBJECT:EntityName) .

            IF VALID-OBJECT (oBusinessEntityDescriptor) AND
                oBusinessEntityDescriptor:Tables:ContainsName (THIS-OBJECT:EntityTable) THEN DO:
                    cQuerySort = oBusinessEntityDescriptor:Tables:GetItem (THIS-OBJECT:EntityTable):DefaultSort .

                    oBrowser = THIS-OBJECT:FindLinkedSmartDataBrowser () .

                    IF VALID-OBJECT (oBrowser) THEN
                        oBrowser:UpdateSortIndicator (cQuerySort) .
            END.
        END.

        /* Mike Fechner, Consultingwerk Ltd. 01.03.2011
           Don't EMPTY-DATASET, when FindRowWhere / FindOnServer because
           we may need the data in case of an error (record not available) */
        IF THIS-OBJECT:EmptyDatasetOnRetrieveData /* SCL-1099 */ AND
       VALID-HANDLE(hDataset) AND NOT THIS-OBJECT:FindString > "":U THEN
            hDataset:EMPTY-DATASET ()   .

        IF THIS-OBJECT:FetchQueryString > "":U THEN .
        ELSE DO:
            /* Mike Fechner, Consultingwerk Ltd. 08.02.2014
               May need to add parent filter to the Fetch Query String */
            IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND THIS-OBJECT:ForeignFields > "":U THEN DO ON ERROR UNDO, THROW:

                THIS-OBJECT:FetchQueryString = SUBSTITUTE("&1 &2":U,
                                                          THIS-OBJECT:EvaluateParentQuery (),
                                                          THIS-OBJECT:QuerySort).
                /* Mike Fechner, Consultingwerk Ltd. 14.07.2015
                   SCL-907 - when an error is thrown from EvaluateParentQuery (), we
                   should close our query */
                @SuppressUnusedWarnings.
                CATCH err AS EvaluateParentQueryException:
                    THIS-OBJECT:CloseQuery () .
                    RETURN FALSE .
                END CATCH.
            END.
            ELSE DO:
                IF THIS-OBJECT:QueryString = "":U AND THIS-OBJECT:QuerySort > "":U THEN
                    THIS-OBJECT:FetchQueryString = SUBSTITUTE ("FOR EACH &1 &2":U,
                                                               THIS-OBJECT:EntityTable,
                                                               THIS-OBJECT:QuerySort) .
                ELSE
                    THIS-OBJECT:FetchQueryString = SUBSTITUTE("&1 &2":U,
                                                              THIS-OBJECT:QueryString,
                                                              THIS-OBJECT:QuerySort).
            END.
        END.

        ASSIGN oListQueryExpressions         = GetNewFilterValues ()
               THIS-OBJECT:FetchFilterValues = oListQueryExpressions .

        IF VALID-OBJECT (oListQueryExpressions) AND NOT oListQueryExpressions:IsEmpty THEN
            ASSIGN cQueries = THIS-OBJECT:MergeQueryStrings (THIS-OBJECT:FetchQueryString,
                                                             oListQueryExpressions) .
        ELSE
            ASSIGN cQueries   = THIS-OBJECT:FetchQueryString +
                                FILL(CHR(1), NUM-ENTRIES(cTables) - NUM-ENTRIES(THIS-OBJECT:FetchQueryString, CHR(1))) .

        ASSIGN
            THIS-OBJECT:PreviousFetchQueries = cQueries
            cEntity                          = THIS-OBJECT:EntityName
            cTables                          = THIS-OBJECT:EntityTable + (IF THIS-OBJECT:EntityView > "":U THEN ",":U ELSE "":U) + THIS-OBJECT:EntityView
            cJoins                           = THIS-OBJECT:EntityJoin
            .

        /* Mike Fechner, Consultingwerk Ltd. 01.09.2010
           Entity ContextID (instance ID) will be appended to Entity name if set. */
        IF THIS-OBJECT:ContextID > "":U THEN
            ASSIGN cEntity = cEntity + CHR (1) + THIS-OBJECT:ContextID .

        /* Mike Fechner, Consultingwerk Ltd. 26.05.2009
           Support for Batching */
        IF THIS-OBJECT:BatchSize > 0 THEN
            ASSIGN iNumRecords = THIS-OBJECT:BatchSize .

        /* Mike Fechner, Consultingwerk Ltd. 12.06.2009
           Bug 1839: Add ContextString to THIS-OBJECT:NextContext */
        IF THIS-OBJECT:ContextString > "":U THEN
            ASSIGN cInitialContext = CHR(1) + THIS-OBJECT:ContextString .

        /* Mike Fechner, Consultingwerk Ltd. 21.01.2011
           Pass FindString to server, when set, no RESTART-ROWID,
           so just add the FindString on the second position */
        IF THIS-OBJECT:FindString > "":U THEN
            ASSIGN cInitialContext = CHR(2) + THIS-OBJECT:FindString +
                                     CHR(2) + SUBSTITUTE ("&1":U, THIS-OBJECT:PrefetchOnFindRowWhere) +
                                     cInitialContext .

        /* Mike Fechner, Consultingwerk Ltd. 13.06.2009
           Indicate WaitState */
        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
            FrameworkSettings:WaitStateManager:SetWaitState ("RetrieveData":U, THIS-OBJECT) .

        ASSIGN lWaitStateActive = TRUE .

        oFetchDataRequest = NewFetchDataRequest (cTables,
                                                 cQueries,
                                                 "FIRST":U,
                                                 iNumRecords,
                                                 cInitialContext) .

        ASSIGN
          oFetchDataRequest:Joins     = cJoins
          oFetchDataRequest:Positions = "ALL":U + FILL(CHR(1) + "ALL":U, NUM-ENTRIES(cTables) - 1)
          oFetchDataRequest:CustomContext = THIS-OBJECT:CustomContext
          .

        /* Mike Fechner, Consultingwerk Ltd. 01.03.2011
           When FindRowWhere / FindOnServer call RetrieveData
           into a blank dataset, so that the previous data
           is still available in case of an error */
        IF THIS-OBJECT:FindString > "":U OR
           THIS-OBJECT:EmptyDatasetOnRetrieveData = FALSE /* SCL-1099 */ THEN DO ON ERROR UNDO, THROW:

            ASSIGN iPositionBeforeFind = THIS-OBJECT:BindingSource:Position .

            THIS-OBJECT:RetrieveData (THIS-OBJECT:AppServerPartition,
                                      cEntity,
                                      oFetchDataRequest,
                                      OUTPUT DATASET-HANDLE hFindDataset BY-REFERENCE) .

            IF THIS-OBJECT:EmptyDatasetOnRetrieveData = TRUE /* SCL-1099 */ THEN
                ASSIGN iPositionBeforeFind = ?  .

            /* Mike Fechner, Consultingwerk Ltd. 01.03.2011
               When no error delete the dataset and copy data */
            hDataset = THIS-OBJECT:DATASET .

            IF THIS-OBJECT:EmptyDatasetOnRetrieveData = TRUE /* SCL-1099 */ THEN
                hDataset:EMPTY-DATASET () .

            hDataset:COPY-DATASET (hFindDataset, FALSE, TRUE) .

            FINALLY:
                IF VALID-HANDLE (hFindDataset) THEN
                    DELETE OBJECT hFindDataset .
            END FINALLY.
        END.
        ELSE
            /* Mike Fechner, Consultingwerk Ltd. 26.05.2009
               Fetch next batch by sending previous context to backend */
            THIS-OBJECT:RetrieveData (THIS-OBJECT:AppServerPartition,
                                      cEntity,
                                      oFetchDataRequest,
                                      OUTPUT DATASET-HANDLE hDataset BY-REFERENCE) .

        ASSIGN THIS-OBJECT:NextContext = oFetchDataRequest:NextContext
               THIS-OBJECT:PrevContext = oFetchDataRequest:PrevContext .

        /* Mike Fechner, Consultingwerk Ltd. 27.04.2015
           Stripp off pending find string from Next/PrevContext */
        ASSIGN cNextContext = THIS-OBJECT:NextContext
               cPrevContext = THIS-OBJECT:PrevContext

               ENTRY (1, cNextContext, CHR (1)) = ENTRY (1, ENTRY (1, THIS-OBJECT:NextContext, CHR (1)), CHR (2))
               ENTRY (1, cPrevContext, CHR (1)) = ENTRY (1, ENTRY (1, THIS-OBJECT:PrevContext, CHR (1)), CHR (2))

               THIS-OBJECT:NextContext = cNextContext
               THIS-OBJECT:PrevContext = cPrevContext
               .

        /* Mike Fechner, Consultingwerk Ltd. 13.06.2009
           Clear WaitState */
        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
            FrameworkSettings:WaitStateManager:ClearWaitState ("RetrieveData":U, THIS-OBJECT) .

        ASSIGN lWaitStateActive = FALSE .

        /* Mike Fechner, Consultingwerk Ltd. 12.09.2009
           When binding to the Dataset it's required that we've filled the TOP-NAV-QUERY's table */
        IF THIS-OBJECT:BindTo = "DATASET":U AND
            hDataset:TOP-NAV-QUERY(1):GET-BUFFER-HANDLE(1):TABLE <> THIS-OBJECT:EntityTable THEN DO:

            UNDO, THROW NEW AppError ("When binding to a ProDataset it's required to retrieve the first "{&TRAN} +
                                      "table from the TOP-NAV-QUERY as the EntityTable."{&TRAN}, 0) .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 04.07.2009
           Activate Tracking Changes on the Dataset */
        SetTrackingChanges (TRUE) .

        /* Mike Fechner, Consultingwerk Ltd. 26.05.2009
           When THIS-OBJECT:NextContext returned from server, activate Batching */
        IF ENTRY(1, THIS-OBJECT:NextContext, CHR(1)) > "":U THEN
            THIS-OBJECT:BindingSource:Batching = TRUE .
        ELSE
            THIS-OBJECT:BindingSource:Batching = FALSE .

        /* Mike Fechner, Consultingwerk Ltd. 12.06.2009
           Bug 1839: Store updated ContextString */
        ASSIGN iPos = INDEX(THIS-OBJECT:NextContext, CHR(1)) .

        IF iPos > 0 THEN
            THIS-OBJECT:ContextString = SUBSTRING(THIS-OBJECT:NextContext, iPos + 1) .

        IF THIS-OBJECT:FirstTime THEN DO:
            THIS-OBJECT:PrepareDataBinding () .

            /* Mike Fechner, Consultingwerk Ltd. 18.01.2013
               Bug 2845 setting FirstTime to FALSE does no longer happen in the FINALLY block
               so that in case of an error finally is not reset */
            ASSIGN THIS-OBJECT:FirstTime = FALSE .
        END.

        IF THIS-OBJECT:QuerySort > "":U THEN DO:
            ASSIGN cQueryString = THIS-OBJECT:QueryHandle:PREPARE-STRING .

            /* Mike Fechner, Consultingwerk Ltd. 02.02.2011
               Bug 2582: This manipulation of the QueryString is not 100% rock solid,
               but searching for _BY_ provides less false matches then just searching
               for BY. Ultimatively we need to parse the QueryString while respecting
               single and double quotes. */
            IF INDEX (cQueryString, " BY ":U) > 0 THEN
                cQueryString = SUBSTRING (cQueryString, 1, INDEX (cQueryString, " BY ":U) - 1) .

            /* Marko Rüterbories, Consultingwerk Ltd. 30.06.2010
               Bug 2357: Added a whitespace between cQueryString and cQuerySort to prevent queries from
               beeing composed like "PRESELECT EACH fooBY foo.field" */
            cQueryString = SUBSTITUTE ("&1 &2":U, cQueryString, cQuerySort) .

            DO ON ERROR UNDO, THROW:
                THIS-OBJECT:QueryHandle:QUERY-PREPARE (cQueryString)  .

                CATCH querypreperr AS Progress.Lang.Error:
                    UNDO, THROW Exception:FromErrorMessageAndError (SUBSTITUTE ("Error opening Query: &1"{&TRAN}, cQueryString),
                                                                    0,
                                                                    querypreperr) .
                END CATCH.
            END.
        END.
        ELSE IF INDEX (ENTRY(1, cQueries, CHR(1)), " BY ":U) > 0 THEN DO ON ERROR UNDO, THROW:
            /* Mike Fechner, Consultingwerk Ltd. 10.10.2009
               If QueryString contains BY phrase, then the Adapters query
               needs to be sorted as well. */
            ASSIGN cQueryString = THIS-OBJECT:QueryHandle:PREPARE-STRING .

            IF INDEX (cQueryString, " BY ":U) > 0 THEN
                cQueryString = SUBSTRING (cQueryString, 1, INDEX (cQueryString, " BY ":U) - 1) .

            cQueryString = cQueryString + " ":U + SUBSTRING (ENTRY(1, cQueries, CHR(1)), INDEX (ENTRY(1, cQueries, CHR(1)), " BY ":U)) .


            THIS-OBJECT:QueryHandle:QUERY-PREPARE (cQueryString)  .

            CATCH querypreperr AS Progress.Lang.Error:
                UNDO, THROW Exception:FromErrorMessageAndError (SUBSTITUTE ("Error preparing SmartBusinessEntityAdapter Query: &1"{&TRAN}, cQueryString),
                                                                0,
                                                                querypreperr) .
            END CATCH.
        END.

        /* Mike Fechner, Consultingwerk Ltd. 25.09.2009
           Catch and ignore Error 91, all other errors will be thrown for
           further handling.
           ** No  record is available. (91)
           ** Kein  Satz verfügbar. (91)
           Raised by Query-Open (probably from within the ProBindingSource).
           Errorstack shows IMPLICIT as line number */
        DO ON ERROR UNDO, THROW:
            THIS-OBJECT:QueryHandle:QUERY-OPEN () .

            CATCH sysex AS Progress.Lang.SysError :
                IF sysex:GetMessageNum (1) = 91 THEN .
                ELSE UNDO, THROW sysex .
            END CATCH.
        END .

        IF lFirstTime THEN
            THIS-OBJECT:AttachQueryToBindingSource () .

        THIS-OBJECT:BindingSource:RefreshAll () .

        /* Initialize the state of the NavigationButtons */
        IF VALID-OBJECT (oSmartNavigationSource) THEN
            OnSmartNavigationTargetPositionChanged(NEW SmartNavigationTargetPositionChangedEventArgs(THIS-OBJECT:RecordPosition)) .

        /* Tell subscribers, that we got (new) data */
        OnAfterRetrieveData (System.EventArgs:Empty) .
        OnAfterBatchAvailable (System.EventArgs:Empty) .

        /* Mike Fechner, Consultingwerk Ltd. 24.06.2010
           When BindingSource's postition has been zero and is now zero
           the PositionChanged event of the BindingSource has not been
           raised, so tell others */
        IF (THIS-OBJECT:BindingSource:Position = 0 AND lPositionWasZero OR lBindingSourceCreated) THEN
            InvokeParentPositionChanged (THIS-OBJECT, System.EventArgs:Empty) .

        IF THIS-OBJECT:BindingSource:Position = 0 AND lPositionWasZero THEN
            THIS-OBJECT:OnCurrentChanged (System.EventArgs:Empty) .

        RETURN TRUE .

        CATCH e AS Progress.Lang.Error:

            /* Mike Fechner, Consultingwerk Ltd. 27.04.2015
               Only reopen query when error not from find */

            IF iPositionBeforeFind = ? THEN DO:
                /* Mike Fechner, Consultingwerk Ltd. 12.02.2015
                   SCL-658 : Reopen query in case of error, so that the data shown in
                             a Grid matches the actual contents in the ProDataset */
                IF VALID-HANDLE (THIS-OBJECT:QueryHandle) THEN
                    THIS-OBJECT:QueryHandle:QUERY-OPEN () .

                /* Can only call RefreshAll, when BindingSource is already associated to
                   an ABL data source (Query, Buffer, ProDataset) */
                IF VALID-OBJECT (THIS-OBJECT:BindingSource) AND VALID-HANDLE (THIS-OBJECT:BindingSource:Handle) THEN
                    THIS-OBJECT:BindingSource:RefreshAll ().
            END .
            ELSE
                THIS-OBJECT:BindingSource:Position = iPositionBeforeFind .

            /* Variable re-throw */
            IF THIS-OBJECT:ThrowErrorsFromRetrieveData THEN
                UNDO, THROW e .

            IF StartupParameterHelper:IOEverywhere = 1 THEN
                ErrorHelper:ShowErrorMessage (e,
                                              SUBSTITUTE ("&1 &2":U,
                                                          SmartComponentLibraryCustomizer:SmartBusinessEntityAdapter_RetrieveDataError,
                                                          ErrorHelper:ErrorTitle (e))) .
            ELSE
                ErrorHelper:ShowErrorMessageBox (e,
                                                 SUBSTITUTE ("&1 &2":U,
                                                             SmartComponentLibraryCustomizer:SmartBusinessEntityAdapter_RetrieveDataError,
                                                             ErrorHelper:ErrorTitle (e))) .
        END CATCH.

        FINALLY:
            IF lWaitStateActive AND VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
                FrameworkSettings:WaitStateManager:ClearWaitState ("RetrieveData":U, THIS-OBJECT) .

            THIS-OBJECT:PreviousPosition = THIS-OBJECT:BindingSource:Position NO-ERROR .
        END FINALLY.
    END METHOD.

    /**
     * Purpose:
     * Notes:
     */
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

    /**
     * Purpose: Shows Dialog to select a DatasetController from within the
     *          OEA/PDSOE Visual Designer
     * Notes:   Stores the chosen class name in the DatasetControllerType property
     */
    METHOD PROTECTED VOID ShowDatasetControllerDialog ():

        DEFINE VARIABLE cClassName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cOldValue  AS CHARACTER NO-UNDO.

        IF NOT THIS-OBJECT:DesignTime THEN
            RETURN .

        Consultingwerk.SmartComponents.Support.BusinessEntityDesignerSupport:SelectDatasetControllerType (OUTPUT cClassName).

        IF cClassName > "":U THEN DO:
            ASSIGN cOldValue                         = THIS-OBJECT:DatasetControllerType
                   THIS-OBJECT:DatasetControllerType = cClassName .

            Consultingwerk.Util.DesignerHelper:RaiseComponentChanged (THIS-OBJECT,
                                                                      "DatasetControllerType":U,
                                                                      BOX (cOldValue),
                                                                      BOX (cClassName)) .
        END.

        Consultingwerk.Util.DesignerHelper:RefreshPropertyGrid (THIS-OBJECT) .

    END METHOD.

    /**
     * Purpose: Invokes the Table Picker Dialog and sets properties of the
     *          SmartBusinessEntityAdapter during Design Time
     * Notes:   Only intended for DesignTime
     */
    METHOD PROTECTED VOID ShowTablePickerDialog ():

        DEFINE VARIABLE hDataset    AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cTables     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cEntityJoin AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cOldTables  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cOldJoins   AS CHARACTER NO-UNDO.

        IF NOT THIS-OBJECT:DesignTime THEN
            RETURN .

        ASSIGN hDataset = THIS-OBJECT:Dataset .

        IF NOT VALID-HANDLE (hDataset) THEN DO:
            MESSAGE "Unable to start table picker dialog without a valid dataset."{&TRAN}
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

    /**
     * Purpose: Creates a dataset containing the modifications of the
     *          SmartBusinessEntityAdapter's dataset and submits the changes dataset
     *          to the service adapter (and then to the Business Entity).
     * Notes:   Used from UpdateRow after every modification from the UI.
     */
    METHOD PUBLIC OVERRIDE VOID SubmitChanges ():

        IF THIS-OBJECT:AutoCommit THEN DO:
            SubmitChangesToBackend () .

            THIS-OBJECT:OnAfterAutoCommitTransaction (System.EventArgs:Empty) .
        END.
        ELSE
            IF THIS-OBJECT:SmartTransactionState = TransactionStateEnum:NoPendingChanges THEN
                THIS-OBJECT:SmartTransactionState = TransactionStateEnum:PendingChanges .

    END METHOD .

    /**
     * Purpose: Creates a dataset containing the modifications of the
     *          SmartBusinessEntityAdapter's dataset and submits the changes dataset
     *          to the service adapter (and then to the Business Entity).
     * Notes:   Used from UpdateRow after every modification from the UI.
     */
    METHOD PROTECTED VOID SubmitChangesToBackend ():

        DEFINE VARIABLE cContext        AS CHARACTER NO-UNDO.

        DEFINE VARIABLE hChangesDataset AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cMessages       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cEntity         AS CHARACTER NO-UNDO.

        DEFINE VARIABLE oServiceAdapter AS Consultingwerk.OERA.IServiceAdapter NO-UNDO .
        DEFINE VARIABLE hContextDataset AS HANDLE                              NO-UNDO .

        DEFINE VARIABLE oThrowError     AS ValidationException                 NO-UNDO .
        DEFINE VARIABLE i               AS INTEGER                             NO-UNDO .

        DEFINE VARIABLE roRowids        AS ROWID EXTENT                        NO-UNDO .

        roRowids = DatasetHelper:GetDatasetRowids(hDataset) .

        /* Mike Fechner, Consultingwerk Ltd. 30.07.2012
           The BeforeGetChanges Event allows manipulation of the Data on the client
           prior to calling the GET-CHANGES method on the Dataset */
        THIS-OBJECT:OnBeforeGetChanges (NEW BeforeGetChangesEventArgs (THIS-OBJECT:DataSet)) .

        /* Create the changes dataset */
        /* Mike Fechner, Consultingwerk Ltd. 25.06.2010
           Starting 10.2B it is obsolete to use a prefix when
           creating the changes dataset. */
        CREATE DATASET hChangesDataset .
        hChangesDataset:CREATE-LIKE (hDataset) .
        hChangesDataset:GET-CHANGES (hDataset, TRUE) .

        /* Mike Fechner, Consultingwerk Ltd. 30.07.2012
           The AfterGetChanges Event allows manipulation of the Changes dataset
           on the client after it has been populated using the GET-CHANGES method */
        THIS-OBJECT:OnAfterGetChanges (NEW AfterGetChangesEventArgs (THIS-OBJECT:DataSet,
                                                                     hChangesDataset)) .

        SetTrackingChanges (FALSE) .

        ASSIGN cEntity = THIS-OBJECT:EntityName .

        /* Mike Fechner, Consultingwerk Ltd. 01.09.2010
           Entity ContextID (instance ID) will be appended to Entity name if set. */
        IF THIS-OBJECT:ContextID > "":U THEN
            ASSIGN cEntity = cEntity + CHR (1) + THIS-OBJECT:ContextID .

        oServiceAdapter = THIS-OBJECT:ServiceAdapter .

        /* Mike Fechner, Consultingwerk Ltd. 05.12.2010
           When an error is returned from the service adapter,
           Reject all non ROW-CREATED changes an re-throw error
           (to UI)  */
        DO ON ERROR UNDO, THROW:
            IF VALID-HANDLE (THIS-OBJECT:ContextDataset) AND TYPE-OF (oServiceAdapter,
                                                                      IServiceAdapterWithContextDataset) THEN DO ON ERROR UNDO, THROW:
                ASSIGN hContextDataset = THIS-OBJECT:ContextDataset .

                IF TYPE-OF (oServiceAdapter, IServiceAdapterWithSubmitDataParameter) THEN
                    CAST(oServiceAdapter,
                         IServiceAdapterWithSubmitDataParameter):SubmitData (THIS-OBJECT:AppServerPartition,
                                                                            cEntity,
                                                                            INPUT-OUTPUT DATASET-HANDLE hChangesDataset BY-REFERENCE,
                                                                            INPUT-OUTPUT cContext,
                                                                            THIS-OBJECT:SaveChangesParameter,
                                                                            hContextDataset) .
                ELSE
                    CAST(oServiceAdapter,
                         IServiceAdapterWithContextDataset):SubmitData (THIS-OBJECT:AppServerPartition,
                                                                        cEntity,
                                                                        INPUT-OUTPUT DATASET-HANDLE hChangesDataset BY-REFERENCE,
                                                                        INPUT-OUTPUT cContext,
                                                                        hContextDataset) .
            END.
            ELSE DO:
                IF TYPE-OF (oServiceAdapter, IServiceAdapterWithSubmitDataParameter) THEN
                    CAST(oServiceAdapter,
                         IServiceAdapterWithSubmitDataParameter):SubmitData (THIS-OBJECT:AppServerPartition,
                                                                             cEntity,
                                                                             INPUT-OUTPUT DATASET-HANDLE hChangesDataset BY-REFERENCE,
                                                                             INPUT-OUTPUT cContext,
                                                                             THIS-OBJECT:SaveChangesParameter) .
                ELSE
                    oServiceAdapter:SubmitData (THIS-OBJECT:AppServerPartition,
                                                cEntity,
                                                INPUT-OUTPUT DATASET-HANDLE hChangesDataset BY-REFERENCE,
                                                INPUT-OUTPUT cContext) .

            END.

            CATCH err AS Progress.Lang.Error :
                Consultingwerk.Util.DatasetHelper:RejectNonCreateChanges (hDataset) .

                UNDO, THROW err .
            END CATCH.
        END.

        ASSIGN cMessages = DatasetHelper:DatasetErrorStrings (hChangesDataset) .

        /* When we receive an error message, cancel the update, reject the changes */
        IF cMessages > "":U THEN DO:
            /* Mike Fechner, Consultingwerk Ltd. 06.09.2011
               Split messages of ADM2 style error messages */
            ASSIGN oThrowError                  = NEW ValidationException (ENTRY (1, cMessages, CHR(3)), 0)
                   oThrowError:RecordIdentifier = DatasetHelper:DatasetErrorRecordIdentifier (hChangesDataset) .

            DO i = 2 TO NUM-ENTRIES (cMessages, CHR(3)):
                oThrowError:AddMessage (ENTRY (i, cMessages, CHR(3)), 0) .
            END.

            /* Mike Fechner, Consultingwerk Ltd. 23.07.2010
               Do not reject records in ROW-CREATED state. That would delete the records
               for the UI */
            /* Mike Fechner, Consultingwerk Ltd. 06.09.2011
               Do only reject the changes when AutoCommit is set to TRUE (default).
               When errors occur during manual commit, then we should not rejec the changes
               as we need a chance to resubmit those records later. */
            IF THIS-OBJECT:AutoCommit = TRUE THEN
                Consultingwerk.Util.DatasetHelper:RejectNonCreateChanges (hDataset) .

            UNDO, THROW oThrowError .
        END.

        SetTrackingChanges (FALSE) .

        THIS-OBJECT:MergeChanges (hChangesDataset, hDataset) .

        DatasetHelper:CopyReadOnlyTables (hChangesDataset, hDataset) .

        hDataset:ACCEPT-CHANGES () .

        DatasetHelper:RepositionDatasetBuffers(hDataset, roRowids) .

        FINALLY:
            IF VALID-HANDLE (hChangesDataset) THEN
                DELETE OBJECT hChangesDataset .

            /* Mike Fechner, Consultingwerk Ltd. 01.09.2013
               JIRA SCL-116: Tracking changes needs to be re-enabled after
               it was disabled for the MERGE-CHANGES method */
            SetTrackingChanges (TRUE) .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Validates a BusinessEntityName agains the Backend.
     * Notes:   This method is intended to be overridden by different backend
     *          adapters (i.e. ticEnterprise)
     * @param pcEntityName The name of the Business Entity
     * @return Logical value indicating if the business entity name is valid
     */
    METHOD PROTECTED LOGICAL ValidateBusinessEntityName (pcEntityName AS CHARACTER):

        DEFINE VARIABLE oServiceAdapter   AS Consultingwerk.OERA.IServiceAdapter NO-UNDO .

        oServiceAdapter = THIS-OBJECT:ServiceAdapter .

        RETURN oServiceAdapter:ValidateEntityName (THIS-OBJECT:AppServerPartition,
                                                   pcEntityName) .

        FINALLY:
            IF THIS-OBJECT:DesignTime AND
               BusinessEntityDesignerSupport:SearchBusinessEntititiesOnAppServer AND
               BusinessEntityDesignerSupport:DisconnectAppServerAfterEachRequest THEN
                FrameworkSettings:AppServerServiceManager:DisconnectService ("":U) .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Verifies that after cancelling an update the Transaction State
     *          should be PendingChanges
     * Notes:   Tests if the ProDataset actually has changes or any of the
     *          SmartDatasetChildAdapters are ModifyingData
     */
    METHOD PROTECTED VOID VerifyTransactionPending ():

        IF NOT THIS-OBJECT:AutoCommit = FALSE THEN
            RETURN .

        IF DatasetHelper:HasChanges (THIS-OBJECT:DataSet) THEN
            RETURN .

        IF NOT THIS-OBJECT:IsUpdating (TRUE) THEN
            THIS-OBJECT:RejectTransaction () .

    END METHOD.

    /**
     * Purpose: Destructor for the SmartBusinessEntityAdapter class
     * Notes:   Clean up Query and ProDataset from memory.
     */
    DESTRUCTOR PUBLIC SmartBusinessEntityAdapter ( ):
        IF VALID-OBJECT (THIS-OBJECT:DatasetController) THEN
            THIS-OBJECT:DatasetController:DeregisterConsumer (THIS-OBJECT) .

        IF VALID-HANDLE (THIS-OBJECT:QueryHandle) AND THIS-OBJECT:BindTo = "Query":U THEN
            DELETE OBJECT THIS-OBJECT:QueryHandle .

        IF VALID-HANDLE (hDataset) THEN
            DELETE OBJECT hDataset NO-ERROR .
    END DESTRUCTOR.

END CLASS.
