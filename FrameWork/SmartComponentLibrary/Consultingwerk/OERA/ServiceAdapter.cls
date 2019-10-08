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
    File        : ServiceAdapter
    Purpose     : Default OERA ServiceAdapter implementation
    Syntax      : Automatically instantiated when accessing the
                  Consultingwerk.Framework.FrameworkSettings:ServiceAdapter
                  for the first time in a session
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jun 01 09:27:33 CEST 2010
    Notes       : The OERA ServiceAdapter is the only class of the
                  SmartComponent Library framework that actually communicates
                  with the Backend for Data-Retrieval.
                  Supports custom LogEntryType:CustomLogEntries by using
                  the Log entry type stored in the CUSTOM_LOG_ENTRY property
                  (Default is "ServiceAdapter")
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.*                    FROM PROPATH .
USING Consultingwerk.Assertion.*          FROM PROPATH .
USING Consultingwerk.Exceptions.*         FROM PROPATH .
USING Consultingwerk.Framework.*          FROM PROPATH .
USING Consultingwerk.Framework.Enum.*     FROM PROPATH .
USING Consultingwerk.Framework.Session.*  FROM PROPATH .
USING Consultingwerk.SmartFramework.*     FROM PROPATH .
USING Consultingwerk.OERA.*               FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH .
USING Progress.Lang.*                     FROM PROPATH .
USING Consultingwerk.Util.*               FROM PROPATH .

CLASS Consultingwerk.OERA.ServiceAdapter
    IMPLEMENTS IServiceAdapter,
               IServiceAdapterWithContextDataset,
               IServiceAdapterWithInvokeTask,
               IServiceAdapterWithInvokeTaskAsync,
               IServiceAdapterWithContextEvents,
               IServiceAdapterWithSubmitDataParameter:

    DEFINE VARIABLE cCachedPartitionName           AS CHARACTER                NO-UNDO INITIAL ? .
    DEFINE VARIABLE hCachedServerHandle            AS HANDLE                   NO-UNDO .
    DEFINE VARIABLE oCachedAppServerServiceManager AS IAppServerServiceManager NO-UNDO .

    /**
     * Purpose: Raised when an Async Task has completed
     * Notes:
     * @param sender The object that raised the AsyncTaskCompleted event
     * @param e The AsyncTaskCompletedEventArgs with the data for the event
     */
    DEFINE PUBLIC EVENT AsyncTaskCompleted SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                           e AS AsyncTaskCompletedEventArgs).

    /**
     * Purpose: Raises the AsyncTaskCompleted
     * Notes:   This method is public because it's invoked by the async request handler
     *          procedure
     * @param e The AsyncTaskCompletedEventArgs with the data for the event
     */
    METHOD PUBLIC VOID OnAsyncTaskCompleted (e AS AsyncTaskCompletedEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "AsyncTaskCompleted":U) .

        THIS-OBJECT:AsyncTaskCompleted:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raised after the ServiceAdapter has performed an Request to the AppServer
     * Notes:
     * @param sender The object that raised the CollectContextFromServer event
     * @param e The Consultingwerk.EventArgs with the data for the event
     */
    DEFINE PUBLIC EVENT CollectContextFromServer SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                 e AS Consultingwerk.EventArgs).

    /**
     * Purpose: Raised before the ServiceAdapter performs an Request to the AppServer
     * Notes:
     * @param sender The object that raised the PrepareContextForRequest event
     * @param e The Consultingwerk.EventArgs with the data for the event
     */
    DEFINE PUBLIC EVENT PrepareContextForServer SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                e AS Consultingwerk.EventArgs).

    /**
     * Purpose: Gets/Sets the name of the AppServer Partition to be used
     *          by this SmartDataAdapter instance
     * Notes:   The property is defined in the abstract SmartDataAdapter base
     *          class - even when some implementations may not require an AppServer
     *          connection
     */
    DEFINE PUBLIC PROPERTY AppServerPartition AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the behavior used when the AppServer connection is lost
     * Notes:   Quit Show error message and quit application
     *          QuitRetry Show error message and prompt to either reconnect or quit the application
     *          QuitRetryIgnore Show error message and prompt to either reconnect, quit or ignore the issue
     *          Ignore Ignore the issue (typically the call with fail)
     */
    DEFINE PUBLIC PROPERTY BehaviorOnDisconnect AS OptionsOnDisconnectEnum NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets if the Service Adapter is currently blocked
     * Notes:   True, when the Service Adapter shows a reconnect-dialog
     */
    DEFINE PUBLIC PROPERTY BlockedByReconnectDialog AS LOGICAL NO-UNDO INITIAL FALSE
    GET.
    PRIVATE SET .

    /**
     * Purpose: Gets the custom log file entry used by the ServiceAdapter class
     * Notes:
     */
    DEFINE PUBLIC PROPERTY CUSTOM_LOG_ENTRY AS CHARACTER INITIAL "ServiceAdapter":U NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Turns on Logging of ServiceAdapter calls
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LoggingActive AS LOGICAL NO-UNDO
    GET.
    SET.

    /**
     * Purpose: The path to the OERA Service Interface procedures
     * Notes:   This is usually the support folder of an OERA code base
     */
    DEFINE PUBLIC PROPERTY ServiceInterfacePath AS CHARACTER NO-UNDO INITIAL "{&OERASI}":U
    GET.
    SET.

    /**
     * Purpose: Constructor for the ServiceAdapter class
     * Notes:
     */
    CONSTRUCTOR PUBLIC ServiceAdapter():

        ASSIGN THIS-OBJECT:BehaviorOnDisconnect = OptionsOnDisconnectEnum:Ignore .

    END CONSTRUCTOR.

    /**
     * Purpose: Returns the handle to the AppServer partition associated with
     *          This SmartDataAdapter
     * Notes:   This variant of the method should be overridden in case customers
     *          need to determine the default partion name based on the business
     *          service accessed
     * @param pcPartition The name of the AppServer partition
     * @param pcBusinessServiceName The name of the Business Service
     * @return The handle of the AppServer partition of SESSION:HANDLE
     */
    METHOD PROTECTED HANDLE ConnectService (pcPartition AS CHARACTER,
                                            pcBusinessServiceName AS CHARACTER):

        RETURN THIS-OBJECT:ConnectService (pcPartition) .

    END METHOD .

    /**
     * Purpose: Returns the handle to the AppServer partition associated with
     *          This SmartDataAdapter
     * Notes:
     * @param pcPartition The name of the AppServer partition
     * @return The handle of the AppServer partition of SESSION:HANDLE
     */
    METHOD PROTECTED HANDLE ConnectService (pcPartition AS CHARACTER):

        IF VALID-HANDLE (hCachedServerHandle) AND
           cCachedPartitionName = pcPartition AND
           oCachedAppServerServiceManager = FrameworkSettings:AppServerServiceManager THEN DO:

            &IF DEFINED (DotNetAccessible) NE 0 &THEN
            IF hCachedServerHandle <> SESSION:HANDLE AND NOT hCachedServerHandle:CONNECTED() THEN DO:
                IF NOT THIS-OBJECT:HandleDisconnection (pcPartition, ?) THEN
                    System.Windows.Forms.Application:Exit () .
            END.
            &ENDIF

            RETURN hCachedServerHandle .
        END .

        IF FrameworkSettings:AppServerServiceManager:IsConnected (pcPartition) THEN DO:
            ASSIGN hCachedServerHandle            = FrameworkSettings:AppServerServiceManager:GetServiceHandle (pcPartition)
                   cCachedPartitionName           = pcPartition
                   oCachedAppServerServiceManager = FrameworkSettings:AppServerServiceManager .

            &IF DEFINED (DotNetAccessible) NE 0 &THEN
            IF hCachedServerHandle <> SESSION:HANDLE AND NOT hCachedServerHandle:CONNECTED() THEN DO:
                IF NOT THIS-OBJECT:HandleDisconnection (pcPartition, ?) THEN
                    System.Windows.Forms.Application:Exit () .
            END.
            &ENDIF

            RETURN hCachedServerHandle .
        END.
        ELSE DO:
            ASSIGN hCachedServerHandle            = FrameworkSettings:AppServerServiceManager:ConnectService (pcPartition)
                   cCachedPartitionName           = pcPartition
                   oCachedAppServerServiceManager = FrameworkSettings:AppServerServiceManager .

            &IF DEFINED (DotNetAccessible) NE 0 &THEN
            IF hCachedServerHandle <> SESSION:HANDLE AND NOT hCachedServerHandle:CONNECTED() THEN DO:
                IF NOT THIS-OBJECT:HandleDisconnection (pcPartition, ?) THEN
                    System.Windows.Forms.Application:Exit () .
            END.
            &ENDIF

            RETURN hCachedServerHandle .
        END.

    END METHOD.

    /**
     * Purpose: Returns an Empty Dataset (schema) to the client.
     * Notes:   The caller of this method is responsible to clean up the Dataset
     *          when no longer used! The called should always invoke this method
     *          with the dataset output parameter BY-REFERENCE!
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @param phDataset OUTPUT DATASET-HANDLE The empty dataset
     */
    METHOD PUBLIC VOID FetchDataset (pcPartition AS CHARACTER,
                                     pcEntityName AS CHARACTER,
                                     OUTPUT DATASET-HANDLE phDataset):

        DEFINE VARIABLE hAppServer      AS HANDLE NO-UNDO .
        DEFINE VARIABLE hContextDataset AS HANDLE NO-UNDO .

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcEntityName, "FetchDataset":U, "":U) .

        /* Mike Fechner, Consultingwerk Ltd. 09.07.2013
           Pass Context Dataset to Server */
        ASSIGN hContextDataset = SessionManager:ContextDataset .

        THIS-OBJECT:OnPrepareContextForServer (Consultingwerk.EventArgs:Empty) .

        hAppServer = THIS-OBJECT:ConnectService (pcPartition, pcEntityName) .

        /* Mike Fechner, Consultingwerk Ltd. 01.05.2013
           Bug SCL-57: Handling of STOP-Conditions from the back end */
        DO ON STOP UNDO, RETURN ERROR NEW StopConditionException
            (SUBSTITUTE ("A stop condition has been raised while waiting for the backend.~nBackend component: &1"{&TRAN},
                         pcEntityName),
             0):

            {Consultingwerk/OERA/callloop-begin.i}
            RUN VALUE(THIS-OBJECT:ServiceInterfacePath + "/proSIfetchDataset.p":U) ON hAppServer
                                            (pcEntityName,
                                             OUTPUT DATASET-HANDLE phDataset,
                                             INPUT-OUTPUT DATASET-HANDLE hContextDataset BY-REFERENCE) .
            {Consultingwerk/OERA/callloop-end.i}

            IF hAppServer = SESSION:HANDLE THEN
                ServiceManager:ProcessServiceLifeCycle() .
        END.

        FINALLY:
            {Consultingwerk/OERA/delete-dataset.i phDataset deleteerr}

            THIS-OBJECT:OnCollectContextFromServer (Consultingwerk.EventArgs:Empty) .

            IF VALID-HANDLE (hContextDataset) THEN
                DELETE OBJECT hContextDataset NO-ERROR .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Returns an Empty Dataset (schema) to the client.
     * Notes:   The caller of this method is responsible to clean up the Dataset
     *          when no longer used! The called should always invoke this method
     *          with the dataset output parameter BY-REFERENCE!
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @param phDataset OUTPUT DATASET-HANDLE The empty dataset
     * @param phContextDataset The handle of the context dataset for this call
     */
    METHOD PUBLIC VOID FetchDataset (pcPartition AS CHARACTER,
                                     pcEntityName AS CHARACTER,
                                     OUTPUT DATASET-HANDLE phDataset,
                                     phContextDataset AS HANDLE):

        DEFINE VARIABLE hAppServer      AS HANDLE NO-UNDO .

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcEntityName, "FetchDataset":U, "":U) .

        IF NOT VALID-HANDLE(phContextDataset)
            THEN ASSIGN phContextDataset = ?.

        THIS-OBJECT:OnPrepareContextForServer (Consultingwerk.EventArgs:Empty) .

        hAppServer = THIS-OBJECT:ConnectService (pcPartition, pcEntityName) .

        /* Mike Fechner, Consultingwerk Ltd. 01.05.2013
           Bug SCL-57: Handling of STOP-Conditions from the back end */
        DO ON STOP UNDO, RETURN ERROR NEW StopConditionException
            (SUBSTITUTE ("A stop condition has been raised while waiting for the backend.~nBackend component: &1"{&TRAN},
                         pcEntityName),
             0):

            {Consultingwerk/OERA/callloop-begin.i}
            RUN VALUE(THIS-OBJECT:ServiceInterfacePath + "/proSIfetchDataset.p":U) ON hAppServer
                                            (pcEntityName,
                                             OUTPUT DATASET-HANDLE phDataset,
                                             INPUT-OUTPUT DATASET-HANDLE phContextDataset BY-REFERENCE) .
            {Consultingwerk/OERA/callloop-end.i}

            IF hAppServer = SESSION:HANDLE THEN
                ServiceManager:ProcessServiceLifeCycle() .
        END.

        FINALLY:
            {Consultingwerk/OERA/delete-dataset.i phDataset deleteerr}

            THIS-OBJECT:OnCollectContextFromServer (Consultingwerk.EventArgs:Empty) .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Handles disconnect issues by attempting to reconnect to the AppServer
     * Notes:
     * @param pcPartition The AppServer partition
     * @param pcErrorMessage The original error message
     * @return Logicla value indicating if the connection could be re-established or the issue should be ignored
     */
    METHOD PROTECTED LOGICAL HandleDisconnection (pcPartition AS CHARACTER,
                                                  pcErrorMessage AS CHARACTER):

        &IF DEFINED (DotNetAccessible) NE 0 &THEN

        DEFINE VARIABLE cMessage  AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE oResponse AS DialogResultEnum NO-UNDO .

        ASSIGN THIS-OBJECT:BlockedByReconnectDialog  = TRUE .

        IF THIS-OBJECT:BehaviorOnDisconnect = OptionsOnDisconnectEnum:Ignore THEN
            RETURN TRUE . /* Issue will be handled by the caller */

        ELSE DO:
            reconnectloop: REPEAT ON ERROR UNDO, THROW:

                /* Can't retrieve localized message - as the AppServer is currently not
                 * available */
                ASSIGN cMessage = "The AppServer connection has been lost."{&TRAN} .

                CASE THIS-OBJECT:BehaviorOnDisconnect:
                    WHEN OptionsOnDisconnectEnum:QuitRetryIgnore THEN
                        oResponse = MessageFormHelper:ShowMessage (cMessage,
                                                                   "AppServer connection error"{&TRAN},
                                                                   pcErrorMessage,
                                                                   MessageFormImages:ImageError,
                                                                   "Reconnect"{&TRAN}, DialogResultEnum:DialogResultRetry,
                                                                   "Exit"{&TRAN},      DialogResultEnum:DialogResultCancel,
                                                                   "Ignore"{&TRAN},    DialogResultEnum:DialogResultIgnore) .
                    WHEN OptionsOnDisconnectEnum:QuitRetry THEN
                        oResponse = MessageFormHelper:ShowMessage (cMessage,
                                                                   "AppServer connection error"{&TRAN},
                                                                   pcErrorMessage,
                                                                   MessageFormImages:ImageError,
                                                                   "Reconnect"{&TRAN}, DialogResultEnum:DialogResultRetry,
                                                                   "Exit"{&TRAN},      DialogResultEnum:DialogResultCancel) .
                    WHEN OptionsOnDisconnectEnum:Quit THEN
                        oResponse = MessageFormHelper:ShowMessage (cMessage,
                                                                   "AppServer connection error"{&TRAN},
                                                                   pcErrorMessage,
                                                                   MessageFormImages:ImageError,
                                                                   "Exit"{&TRAN},      DialogResultEnum:DialogResultCancel) .
                END CASE .

                ASSIGN pcErrorMessage = ? .

                CASE oResponse:
                    WHEN DialogResultEnum:DialogResultRetry THEN DO:

                        /* First disconnect from the AppServer */
                        DO ON ERROR UNDO, THROW:
                            FrameworkSettings:AppServerServiceManager:DisconnectService(pcPartition) .

                            CATCH syserr AS Progress.Lang.SysError:
                                IF syserr:GetMessageNum(1) <> 5537 THEN /*Server is not connected. (5537)*/
                                   UNDO, THROW syserr .
                            END CATCH.
                        END.

                        /* Then attempt to reconnect */
                        DO ON ERROR UNDO, THROW:
                            hCachedServerHandle = FrameworkSettings:AppServerServiceManager:ConnectService(pcPartition) .

                            CATCH err AS Progress.Lang.Error:
                                ErrorHelper:ShowErrorMessage(err) .
                                NEXT reconnectloop .
                            END CATCH.
                        END .

                        RETURN TRUE .
                    END.
                    WHEN DialogResultEnum:DialogResultCancel THEN DO:
                        RETURN FALSE .
                    END.
                    WHEN DialogResultEnum:DialogResultIgnore THEN DO:
                        RETURN TRUE .
                    END.

                END CASE .
            END. /* reconnectloop */
        END .

        RETURN FALSE.

        &ELSE
        RETURN TRUE .
        &ENDIF

        FINALLY:
            ASSIGN THIS-OBJECT:BlockedByReconnectDialog = FALSE .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Invokes a custom method of the BusinessEntity.
     * Notes:   When the poParameter is expected to just return values, it must still
     *          be passed in to this method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset INPUT-OUTPUT DATASET-HANDLE The handle of the dataset of the business entity, optionally INPUT, optionally OUTPUT
     * @param poParameter The serializable parameter object to the method
     */
    METHOD PUBLIC VOID InvokeMethod (pcPartition AS CHARACTER,
                                     pcEntityName AS CHARACTER,
                                     pcMethodName AS CHARACTER,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset,
                                     poParameter AS ISerializable):

        DEFINE VARIABLE lcParameter     AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE hAppServer      AS HANDLE   NO-UNDO .
        DEFINE VARIABLE hContextDataset AS HANDLE   NO-UNDO .

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcEntityName, "InvokeMethod":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server */
        ASSIGN hContextDataset = SessionManager:ContextDataset .

        THIS-OBJECT:OnPrepareContextForServer (Consultingwerk.EventArgs:Empty) .

        hAppServer = THIS-OBJECT:ConnectService (pcPartition, pcEntityName) .

        IF VALID-OBJECT (poParameter) THEN
            ASSIGN lcParameter = poParameter:Serialize () .

        /* Mike Fechner, Consultingwerk Ltd. 01.05.2013
           Bug SCL-57: Handling of STOP-Conditions from the back end */
        DO ON STOP UNDO, RETURN ERROR NEW StopConditionException
            (SUBSTITUTE ("A stop condition has been raised while waiting for the backend.~nBackend component: &1"{&TRAN},
                         pcEntityName),
             0):

            {Consultingwerk/OERA/callloop-begin.i}
            RUN VALUE(THIS-OBJECT:ServiceInterfacePath + "/proSIinvokeMethod.p":U) ON hAppServer
                                            (pcEntityName,
                                             pcMethodName,
                                             INPUT-OUTPUT DATASET-HANDLE phDataset,
                                             INPUT-OUTPUT lcParameter,
                                             INPUT-OUTPUT DATASET-HANDLE hContextDataset BY-REFERENCE) .
            {Consultingwerk/OERA/callloop-end.i}

            IF hAppServer = SESSION:HANDLE THEN
                ServiceManager:ProcessServiceLifeCycle() .
        END.

        IF VALID-OBJECT (poParameter) AND lcParameter > "":U THEN
            poParameter:Deserialize (lcParameter) .
        ELSE IF lcParameter > "":U THEN
            poParameter = Consultingwerk.Serializable:DeserializeInstance (lcParameter) .

        IF VALID-HANDLE (phDataset) THEN
            DELETE OBJECT phDataset .

        /* Ignore error:
           Cannot delete a BY-REFERENCE PARAMETER dataset or table in the called procedure. (12327) */
        CATCH err AS Progress.Lang.SysError:
            IF err:GetMessageNum (1) = 12327 THEN .
            ELSE UNDO, THROW err .
        END CATCH.

        FINALLY:
            {Consultingwerk/OERA/delete-dataset.i phDataset deleteerr}

            THIS-OBJECT:OnCollectContextFromServer (Consultingwerk.EventArgs:Empty) .

            IF VALID-HANDLE (hContextDataset) THEN
                DELETE OBJECT hContextDataset NO-ERROR .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Invokes a custom method of the BusinessEntity.
     * Notes:   The phContextDataset parameter is intended to be passed by-reference
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset INPUT-OUTPUT DATASET-HANDLE The handle of the dataset of the business entity, optionally INPUT, optionally OUTPUT
     * @param poParameter The serializable parameter object to the method
     * @param phContextDataset The handle of the context dataset for this call
     */
    METHOD PUBLIC VOID InvokeMethod (pcPartition  AS CHARACTER,
                                     pcEntityName AS CHARACTER,
                                     pcMethodName AS CHARACTER,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset,
                                     poParameter AS ISerializable,
                                     phContextDataset AS HANDLE):

        DEFINE VARIABLE lcParameter     AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE hAppServer      AS HANDLE   NO-UNDO .

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcEntityName, "InvokeMethod":U, pcMethodName) .

        hAppServer = THIS-OBJECT:ConnectService (pcPartition, pcEntityName) .

        THIS-OBJECT:OnPrepareContextForServer (Consultingwerk.EventArgs:Empty) .

        IF VALID-OBJECT (poParameter) THEN
            ASSIGN lcParameter = poParameter:Serialize () .

        /* Mike Fechner, Consultingwerk Ltd. 01.05.2013
           Bug SCL-57: Handling of STOP-Conditions from the back end */
        DO ON STOP UNDO, RETURN ERROR NEW StopConditionException
            (SUBSTITUTE ("A stop condition has been raised while waiting for the backend.~nBackend component: &1"{&TRAN},
                         pcEntityName),
             0):

            {Consultingwerk/OERA/callloop-begin.i}
            RUN VALUE(THIS-OBJECT:ServiceInterfacePath + "/proSIinvokeMethod.p":U) ON hAppServer
                                            (pcEntityName,
                                             pcMethodName,
                                             INPUT-OUTPUT DATASET-HANDLE phDataset,
                                             INPUT-OUTPUT lcParameter,
                                             INPUT-OUTPUT DATASET-HANDLE phContextDataset BY-REFERENCE) .
            {Consultingwerk/OERA/callloop-end.i}

            IF hAppServer = SESSION:HANDLE THEN
                ServiceManager:ProcessServiceLifeCycle() .
        END.

        IF VALID-OBJECT (poParameter) AND lcParameter > "":U THEN
            poParameter:Deserialize (lcParameter) .
        ELSE IF lcParameter > "":U THEN
            poParameter = Consultingwerk.Serializable:DeserializeInstance (lcParameter) .

        THIS-OBJECT:OnCollectContextFromServer (Consultingwerk.EventArgs:Empty) .

        {Consultingwerk/OERA/delete-dataset.i phDataset deleteerr}

    END METHOD .

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method
     * @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The handle of the second dataset to be passed to the task method
     * @param phDataset3 INPUT-OUTPUT DATASET-HANDLE The handle of the third dataset to be passed to the task method
     * @param phDataset4 INPUT-OUTPUT DATASET-HANDLE The handle of the fourth dataset to be passed to the task method
     * @param phDataset5 INPUT-OUTPUT DATASET-HANDLE The handle of the fifth dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     * @param phContextDataset The handle of the context dataset for this call
     */
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER,
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset4,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset5,
                                   poParameter AS Consultingwerk.ISerializable,
                                   phContextDataset AS HANDLE):

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTask":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        IF NOT VALID-HANDLE (phContextDataset) THEN
            ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketask.i 5}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method
     * @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The handle of the second dataset to be passed to the task method
     * @param phDataset3 INPUT-OUTPUT DATASET-HANDLE The handle of the third dataset to be passed to the task method
     * @param phDataset4 INPUT-OUTPUT DATASET-HANDLE The handle of the fourth dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     * @param phContextDataset The handle of the context dataset for this call
     */
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER,
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset4,
                                   poParameter AS Consultingwerk.ISerializable,
                                   phContextDataset AS HANDLE):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5 AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTask":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        IF NOT VALID-HANDLE (phContextDataset) THEN
            ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketask.i 4}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method
     * @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The handle of the second dataset to be passed to the task method
     * @param phDataset3 INPUT-OUTPUT DATASET-HANDLE The handle of the third dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     * @param phContextDataset The handle of the context dataset for this call
     */
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER,
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                   poParameter AS Consultingwerk.ISerializable,
                                   phContextDataset AS HANDLE):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset4 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5 AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTask":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        IF NOT VALID-HANDLE (phContextDataset) THEN
            ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketask.i 3}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method
     * @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The handle of the second dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     * @param phContextDataset The handle of the context dataset for this call
     */
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER,
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                   poParameter AS Consultingwerk.ISerializable,
                                   phContextDataset AS HANDLE):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset3 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset4 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5 AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTask":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        IF NOT VALID-HANDLE (phContextDataset) THEN
            ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketask.i 2}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     * @param phContextDataset The handle of the context dataset for this call
     */
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER,
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                   poParameter AS Consultingwerk.ISerializable,
                                   phContextDataset AS HANDLE):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset2 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset3 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset4 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5 AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTask":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        IF NOT VALID-HANDLE (phContextDataset) THEN
            ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketask.i 1}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param poParameter The serializable parameter object to the method
     * @param phContextDataset The handle of the context dataset for this call
     */
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER,
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   poParameter AS Consultingwerk.ISerializable,
                                   phContextDataset AS HANDLE):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset1 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset2 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        @Table(name="phDataset4",table="").
        DEFINE VARIABLE phDataset3 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset4 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5 AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTask":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        IF NOT VALID-HANDLE (phContextDataset) THEN
            ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketask.i 0}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method
     * @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The handle of the second dataset to be passed to the task method
     * @param phDataset3 INPUT-OUTPUT DATASET-HANDLE The handle of the third dataset to be passed to the task method
     * @param phDataset4 INPUT-OUTPUT DATASET-HANDLE The handle of the fourth dataset to be passed to the task method
     * @param phDataset5 INPUT-OUTPUT DATASET-HANDLE The handle of the fifth dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     */
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER,
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset4,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset5,
                                   poParameter AS Consultingwerk.ISerializable):

        DEFINE VARIABLE phContextDataset AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTask":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketask.i 5}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method
     * @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The handle of the second dataset to be passed to the task method
     * @param phDataset3 INPUT-OUTPUT DATASET-HANDLE The handle of the third dataset to be passed to the task method
     * @param phDataset4 INPUT-OUTPUT DATASET-HANDLE The handle of the fourth dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     */
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER,
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset4,
                                   poParameter AS Consultingwerk.ISerializable):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5       AS HANDLE NO-UNDO.
        DEFINE VARIABLE phContextDataset AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTask":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketask.i 4}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method
     * @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The handle of the second dataset to be passed to the task method
     * @param phDataset3 INPUT-OUTPUT DATASET-HANDLE The handle of the third dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     */
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER,
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                   poParameter AS Consultingwerk.ISerializable):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset4       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5       AS HANDLE NO-UNDO.
        DEFINE VARIABLE phContextDataset AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTask":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketask.i 3}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method
     * @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The handle of the second dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     */
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER,
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                   poParameter AS Consultingwerk.ISerializable):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset3       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset4       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5       AS HANDLE NO-UNDO.
        DEFINE VARIABLE phContextDataset AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTask":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketask.i 2}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     */
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER,
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                   poParameter AS Consultingwerk.ISerializable):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset2       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset3       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset4       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5       AS HANDLE NO-UNDO.
        DEFINE VARIABLE phContextDataset AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTask":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketask.i 1}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param poParameter The serializable parameter object to the method
     */
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER,
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   poParameter AS Consultingwerk.ISerializable):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset1       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset2       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset3       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset4       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5       AS HANDLE NO-UNDO.
        DEFINE VARIABLE phContextDataset AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTask":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketask.i 0}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class asynchronously
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 The handle of the first dataset to be passed to the task method
     * @param phDataset2 The handle of the second dataset to be passed to the task method
     * @param phDataset3 The handle of the third dataset to be passed to the task method
     * @param phDataset4 The handle of the fourth dataset to be passed to the task method
     * @param phDataset5 The handle of the fifth dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     * @param phContextDataset The handle of the context dataset for this call
     * @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
     * @return The handle of the async AppServer request
     */
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          phDataset1 AS HANDLE,
                                          phDataset2 AS HANDLE,
                                          phDataset3 AS HANDLE,
                                          phDataset4 AS HANDLE,
                                          phDataset5 AS HANDLE,
                                          poParameter AS ISerializable,
                                          phContextDataset AS HANDLE,
                                          poContext AS Progress.Lang.Object):

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTaskAsync":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        IF NOT VALID-HANDLE (phContextDataset) THEN
            ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketaskasync.i 5}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class asynchronously
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 The handle of the first dataset to be passed to the task method
     * @param phDataset2 The handle of the second dataset to be passed to the task method
     * @param phDataset3 The handle of the third dataset to be passed to the task method
     * @param phDataset4 The handle of the fourth dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     * @param phContextDataset The handle of the context dataset for this call
     * @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
     * @return The handle of the async AppServer request
     */
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          phDataset1 AS HANDLE,
                                          phDataset2 AS HANDLE,
                                          phDataset3 AS HANDLE,
                                          phDataset4 AS HANDLE,
                                          poParameter AS ISerializable,
                                          phContextDataset AS HANDLE,
                                          poContext AS Progress.Lang.Object):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5 AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTaskAsync":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        IF NOT VALID-HANDLE (phContextDataset) THEN
            ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketaskasync.i 4}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class asynchronously
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 The handle of the first dataset to be passed to the task method
     * @param phDataset2 The handle of the second dataset to be passed to the task method
     * @param phDataset3 The handle of the third dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     * @param phContextDataset The handle of the context dataset for this call
     * @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
     * @return The handle of the async AppServer request
     */
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          phDataset1 AS HANDLE,
                                          phDataset2 AS HANDLE,
                                          phDataset3 AS HANDLE,
                                          poParameter AS ISerializable,
                                          phContextDataset AS HANDLE,
                                          poContext AS Progress.Lang.Object):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset4 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5 AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTaskAsync":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        IF NOT VALID-HANDLE (phContextDataset) THEN
            ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketaskasync.i 3}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class asynchronously
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 The handle of the first dataset to be passed to the task method
     * @param phDataset2 The handle of the second dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     * @param phContextDataset The handle of the context dataset for this call
     * @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
     * @return The handle of the async AppServer request
     */
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          phDataset1 AS HANDLE,
                                          phDataset2 AS HANDLE,
                                          poParameter AS ISerializable,
                                          phContextDataset AS HANDLE,
                                          poContext AS Progress.Lang.Object):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset3 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset4 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5 AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTaskAsync":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        IF NOT VALID-HANDLE (phContextDataset) THEN
            ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketaskasync.i 2}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 The handle of the first dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     * @param phContextDataset The handle of the context dataset for this call
     * @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
     * @return The handle of the async AppServer request
     */
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          phDataset1 AS HANDLE,
                                          poParameter AS ISerializable,
                                          phContextDataset AS HANDLE,
                                          poContext AS Progress.Lang.Object):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset2 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset3 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset4 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5 AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTaskAsync":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        IF NOT VALID-HANDLE (phContextDataset) THEN
            ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketaskasync.i 1}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param poParameter The serializable parameter object to the method
     * @param phContextDataset The handle of the context dataset for this call
     * @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
     * @return The handle of the async AppServer request
     */
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          poParameter AS ISerializable,
                                          phContextDataset AS HANDLE,
                                          poContext AS Progress.Lang.Object):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset1 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset2 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset3 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset4 AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5 AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTaskAsync":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        IF NOT VALID-HANDLE (phContextDataset) THEN
            ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketaskasync.i 0}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class asynchronously
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 The handle of the first dataset to be passed to the task method
     * @param phDataset2 The handle of the second dataset to be passed to the task method
     * @param phDataset3 The handle of the third dataset to be passed to the task method
     * @param phDataset4 The handle of the fourth dataset to be passed to the task method
     * @param phDataset5 The handle of the fifth dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     * @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
     * @return The handle of the async AppServer request
     */
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          phDataset1 AS HANDLE,
                                          phDataset2 AS HANDLE,
                                          phDataset3 AS HANDLE,
                                          phDataset4 AS HANDLE,
                                          phDataset5 AS HANDLE,
                                          poParameter AS ISerializable,
                                          poContext AS Progress.Lang.Object):

        DEFINE VARIABLE phContextDataset AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTaskAsync":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketaskasync.i 5}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class asynchronously
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 The handle of the first dataset to be passed to the task method
     * @param phDataset2 The handle of the second dataset to be passed to the task method
     * @param phDataset3 The handle of the third dataset to be passed to the task method
     * @param phDataset4 The handle of the fourth dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     * @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
     * @return The handle of the async AppServer request
     */
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          phDataset1 AS HANDLE,
                                          phDataset2 AS HANDLE,
                                          phDataset3 AS HANDLE,
                                          phDataset4 AS HANDLE,
                                          poParameter AS ISerializable,
                                          poContext AS Progress.Lang.Object):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5       AS HANDLE NO-UNDO.
        DEFINE VARIABLE phContextDataset AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTaskAsync":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketaskasync.i 4}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class asynchronously
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 The handle of the first dataset to be passed to the task method
     * @param phDataset2 The handle of the second dataset to be passed to the task method
     * @param phDataset3 The handle of the third dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     * @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
     * @return The handle of the async AppServer request
     */
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          phDataset1 AS HANDLE,
                                          phDataset2 AS HANDLE,
                                          phDataset3 AS HANDLE,
                                          poParameter AS ISerializable,
                                          poContext AS Progress.Lang.Object):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset4       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5       AS HANDLE NO-UNDO.
        DEFINE VARIABLE phContextDataset AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTaskAsync":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketaskasync.i 3}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class asynchronously
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 The handle of the first dataset to be passed to the task method
     * @param phDataset2 The handle of the second dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     * @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
     * @return The handle of the async AppServer request
     */
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          phDataset1 AS HANDLE,
                                          phDataset2 AS HANDLE,
                                          poParameter AS ISerializable,
                                          poContext AS Progress.Lang.Object):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset3       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset4       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5       AS HANDLE NO-UNDO.
        DEFINE VARIABLE phContextDataset AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTaskAsync":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketaskasync.i 2}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 The handle of the first dataset to be passed to the task method
     * @param poParameter The serializable parameter object to the method
     * @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
     * @return The handle of the async AppServer request
     */
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          phDataset1 AS HANDLE,
                                          poParameter AS ISerializable,
                                          poContext AS Progress.Lang.Object):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset2       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset3       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset4       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5       AS HANDLE NO-UNDO.
        DEFINE VARIABLE phContextDataset AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTaskAsync":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketaskasync.i 1}

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param poParameter The serializable parameter object to the method
     * @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
     * @return The handle of the async AppServer request
     */
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          poParameter AS ISerializable,
                                          poContext AS Progress.Lang.Object):

        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset1       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset2       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset3       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset4       AS HANDLE NO-UNDO.
        @SuppressUnusedWarnings.
        DEFINE VARIABLE phDataset5       AS HANDLE NO-UNDO.
        DEFINE VARIABLE phContextDataset AS HANDLE NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcTaskName, "InvokeTaskAsync":U, pcMethodName) .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server when no request specific one */
        ASSIGN phContextDataset = SessionManager:ContextDataset .

        {Consultingwerk/OERA/sainvoketaskasync.i 0}

    END METHOD.

    /**
     * Purpose: Raises the CollectContextFromServer
     * Notes:
     * @param e The Consultingwerk.EventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnCollectContextFromServer (e AS Consultingwerk.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = Consultingwerk.EventArgs:Empty .

        THIS-OBJECT:CollectContextFromServer:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raises the PrepareContextForServer
     * Notes:
     * @param e The Consultingwerk.EventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnPrepareContextForServer (e AS Consultingwerk.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = Consultingwerk.EventArgs:Empty .

        THIS-OBJECT:PrepareContextForServer:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Reads data from the Backend
     * Notes:   Request Parameters are send using the IFetchDataRequest object
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @param poFetchDataRequest The IFetchDataRequest object with the data for this request
     * @param phDataset OUTPUT DATASET-HANDLE The resulting dataset
     */
    METHOD PUBLIC VOID RetrieveData (pcPartition        AS CHARACTER,
                                     pcEntityName       AS CHARACTER,
                                     poFetchDataRequest AS IFetchDataRequest,
                                     OUTPUT DATASET-HANDLE phDataSet) .

        DEFINE VARIABLE hAppServer AS HANDLE NO-UNDO.

        /* Request variables */
        DEFINE VARIABLE cEntity         AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cTables         AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cQueries        AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cJoins          AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cPositions      AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cRequests       AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cBatchContext   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lFillBatch      AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cNumRecords     AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cContext        AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cPrevContext    AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cNextContext    AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE lcParameter     AS LONGCHAR  NO-UNDO EXTENT 1.
        DEFINE VARIABLE lcNamedQuery    AS LONGCHAR  NO-UNDO EXTENT 1.

        DEFINE VARIABLE hDataset        AS HANDLE    NO-UNDO EXTENT 10 .
        DEFINE VARIABLE hContextDataset AS HANDLE    NO-UNDO.

        ObjectAssert:IsValid (poFetchDataRequest, "poFetchDataRequest":U) .

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcEntityName, "RetrieveData":U, "") .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server */
        ASSIGN hContextDataset = SessionManager:ContextDataset .

        THIS-OBJECT:OnPrepareContextForServer (Consultingwerk.EventArgs:Empty) .

        ASSIGN
            hAppServer       = THIS-OBJECT:ConnectService (pcPartition, pcEntityName)
            cEntity[1]       = pcEntityName
            cTables[1]       = poFetchDataRequest:Tables
            cQueries[1]      = poFetchDataRequest:Queries
            cJoins[1]        = poFetchDataRequest:Joins
            cPositions[1]    = poFetchDataRequest:Positions
            cRequests[1]     = poFetchDataRequest:Requests
            cBatchContext    = SUBSTITUTE ("&1&2&3":U,
                                           poFetchDataRequest:Context,
                                           CHR (3),
                                           poFetchDataRequest:CustomContext)
            cNumRecords[1]   = STRING(poFetchDataRequest:NumRecords)
            .

        IF VALID-OBJECT (poFetchDataRequest:CustomParameter) THEN
            lcParameter[1] = poFetchDataRequest:CustomParameter:Serialize () .

        IF VALID-OBJECT (poFetchDataRequest:NamedQuery) THEN
            lcNamedQuery[1] = CAST (poFetchDataRequest:NamedQuery, ISerializable):Serialize () .

        IF poFetchDataRequest:CustomContext > "":U THEN
            ASSIGN cBatchContext    = SUBSTITUTE ("&1&2&3":U,
                                                  poFetchDataRequest:Context,
                                                  CHR (3),
                                                  poFetchDataRequest:CustomContext) .
        ELSE
            ASSIGN cBatchContext    = poFetchDataRequest:Context .

        /* Mike Fechner, Consultingwerk Ltd. 01.05.2013
           Bug SCL-57: Handling of STOP-Conditions from the back end */
        DO ON STOP UNDO, RETURN ERROR NEW StopConditionException
            (SUBSTITUTE ("A stop condition has been raised while waiting for the backend.~nBackend component: &1"{&TRAN},
                         pcEntityName),
             0):

            {Consultingwerk/OERA/callloop-begin.i}
            RUN VALUE(THIS-OBJECT:ServiceInterfacePath + "/proSIretrieve.p":U) ON hAppServer
                                         (cEntity,
                                          cTables,
                                          cQueries,
                                          cJoins,
                                          cPositions,
                                          cRequests,
                                          cBatchContext,
                                          lFillBatch,
                                          poFetchDataRequest:StopAfter,
                                          INPUT-OUTPUT cNumRecords,
                                          INPUT-OUTPUT lcParameter,
                                          lcNamedQuery,
                                          OUTPUT DATASET-HANDLE phDataset,
                                          OUTPUT DATASET-HANDLE hDataset[2],
                                          OUTPUT DATASET-HANDLE hDataset[3],
                                          OUTPUT DATASET-HANDLE hDataset[4],
                                          OUTPUT DATASET-HANDLE hDataset[5],
                                          OUTPUT DATASET-HANDLE hDataset[6],
                                          OUTPUT DATASET-HANDLE hDataset[7],
                                          OUTPUT DATASET-HANDLE hDataset[8],
                                          OUTPUT DATASET-HANDLE hDataset[9],
                                          OUTPUT DATASET-HANDLE hDataset[10],
                                          INPUT-OUTPUT cContext,
                                          OUTPUT cPrevContext,
                                          OUTPUT cNextContext,
                                          INPUT-OUTPUT DATASET-HANDLE hContextDataset BY-REFERENCE) .
            {Consultingwerk/OERA/callloop-end.i}

            IF hAppServer = SESSION:HANDLE THEN
                ServiceManager:ProcessServiceLifeCycle() .
        END.

        ASSIGN
            poFetchDataRequest:NextContext = cNextContext[1]
            poFetchDataRequest:PrevContext = cPrevContext[1]
            .

        IF VALID-OBJECT (poFetchDataRequest:CustomParameter) AND lcParameter[1] > "":U THEN
            poFetchDataRequest:CustomParameter:Deserialize (lcParameter[1]) .
        IF lcParameter[1] > "":U THEN
             poFetchDataRequest:CustomParameter = Consultingwerk.Serializable:DeserializeInstance (lcParameter[1]) .

        FINALLY:
            {Consultingwerk/OERA/delete-dataset.i phDataset deleteerr}

            THIS-OBJECT:OnCollectContextFromServer (Consultingwerk.EventArgs:Empty) .

            IF VALID-HANDLE (hContextDataset) THEN
                DELETE OBJECT hContextDataset NO-ERROR .

            /* Delete datasets (delayed until this procedure terminates */
            IF VALID-HANDLE(hDataset[2]) THEN
                DELETE OBJECT hDataset[2] NO-ERROR .

            IF VALID-HANDLE(hDataset[3]) THEN
                DELETE OBJECT hDataset[3] NO-ERROR .

            IF VALID-HANDLE(hDataset[4]) THEN
                DELETE OBJECT hDataset[4] NO-ERROR .

            IF VALID-HANDLE(hDataset[5]) THEN
                DELETE OBJECT hDataset[5] NO-ERROR .

            IF VALID-HANDLE(hDataset[6]) THEN
                DELETE OBJECT hDataset[6] NO-ERROR .

            IF VALID-HANDLE(hDataset[7]) THEN
                DELETE OBJECT hDataset[7] NO-ERROR .

            IF VALID-HANDLE(hDataset[8]) THEN
                DELETE OBJECT hDataset[8] NO-ERROR .

            IF VALID-HANDLE(hDataset[9]) THEN
                DELETE OBJECT hDataset[9] NO-ERROR .

            IF VALID-HANDLE(hDataset[10]) THEN
                DELETE OBJECT hDataset[10] NO-ERROR .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Reads data from the Backend
     * Notes:   Request Parameters are send using the IFetchDataRequest object
     *          The phContextDataset parameter is intended to be passed by-reference
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @param poFetchDataRequest The IFetchDataRequest object with the data for this request
     * @param phDataset OUTPUT DATASET-HANDLE The resulting dataset
     * @param phContextDataset The handle of the context dataset for this call
     */
    METHOD PUBLIC VOID RetrieveData (pcPartition        AS CHARACTER,
                                     pcEntityName       AS CHARACTER,
                                     poFetchDataRequest AS IFetchDataRequest,
                                     OUTPUT DATASET-HANDLE phDataSet,
                                     phContextDataset AS HANDLE):

        DEFINE VARIABLE hAppServer AS HANDLE NO-UNDO.

        /* Request variables */
        DEFINE VARIABLE cEntity         AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cTables         AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cQueries        AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cJoins          AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cPositions      AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cRequests       AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cBatchContext   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lFillBatch      AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cNumRecords     AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cContext        AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cPrevContext    AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE cNextContext    AS CHARACTER NO-UNDO EXTENT 1.
        DEFINE VARIABLE lcParameter     AS LONGCHAR  NO-UNDO EXTENT 1.
        DEFINE VARIABLE lcNamedQuery    AS LONGCHAR  NO-UNDO EXTENT 1.

        DEFINE VARIABLE hDataset        AS HANDLE    NO-UNDO EXTENT 10 .

        ObjectAssert:IsValid (poFetchDataRequest, "poFetchDataRequest":U) .

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcEntityName, "RetrieveData":U, "") .

        THIS-OBJECT:OnPrepareContextForServer (Consultingwerk.EventArgs:Empty) .

        ASSIGN
            hAppServer    = THIS-OBJECT:ConnectService (pcPartition, pcEntityName)
            cEntity[1]       = pcEntityName
            cTables[1]       = poFetchDataRequest:Tables
            cQueries[1]      = poFetchDataRequest:Queries
            cJoins[1]        = poFetchDataRequest:Joins
            cPositions[1]    = poFetchDataRequest:Positions
            cRequests[1]     = poFetchDataRequest:Requests
            cBatchContext    = SUBSTITUTE ("&1&2&3":U,
                                           poFetchDataRequest:Context,
                                           CHR (3),
                                           poFetchDataRequest:CustomContext)
            cNumRecords[1]   = STRING(poFetchDataRequest:NumRecords)
            .

        IF VALID-OBJECT (poFetchDataRequest:CustomParameter) THEN
            lcParameter[1] = poFetchDataRequest:CustomParameter:Serialize () .

        IF VALID-OBJECT (poFetchDataRequest:NamedQuery) THEN
            lcNamedQuery[1] = CAST (poFetchDataRequest:NamedQuery, ISerializable):Serialize () .

        IF poFetchDataRequest:CustomContext > "":U THEN
            ASSIGN cBatchContext    = SUBSTITUTE ("&1&2&3":U,
                                                  poFetchDataRequest:Context,
                                                  CHR (3),
                                                  poFetchDataRequest:CustomContext) .
        ELSE
            ASSIGN cBatchContext    = poFetchDataRequest:Context .

        /* Mike Fechner, Consultingwerk Ltd. 01.05.2013
           Bug SCL-57: Handling of STOP-Conditions from the back end */
        DO ON STOP UNDO, RETURN ERROR NEW StopConditionException
            (SUBSTITUTE ("A stop condition has been raised while waiting for the backend.~nBackend component: &1"{&TRAN},
                         pcEntityName),
             0):

            {Consultingwerk/OERA/callloop-begin.i}
            RUN VALUE(THIS-OBJECT:ServiceInterfacePath + "/proSIretrieve.p":U) ON hAppServer
                                         (cEntity,
                                          cTables,
                                          cQueries,
                                          cJoins,
                                          cPositions,
                                          cRequests,
                                          cBatchContext,
                                          lFillBatch,
                                          poFetchDataRequest:StopAfter,
                                          INPUT-OUTPUT cNumRecords,
                                          INPUT-OUTPUT lcParameter,
                                          lcNamedQuery,
                                          OUTPUT DATASET-HANDLE phDataset,
                                          OUTPUT DATASET-HANDLE hDataset[2],
                                          OUTPUT DATASET-HANDLE hDataset[3],
                                          OUTPUT DATASET-HANDLE hDataset[4],
                                          OUTPUT DATASET-HANDLE hDataset[5],
                                          OUTPUT DATASET-HANDLE hDataset[6],
                                          OUTPUT DATASET-HANDLE hDataset[7],
                                          OUTPUT DATASET-HANDLE hDataset[8],
                                          OUTPUT DATASET-HANDLE hDataset[9],
                                          OUTPUT DATASET-HANDLE hDataset[10],
                                          INPUT-OUTPUT cContext,
                                          OUTPUT cPrevContext,
                                          OUTPUT cNextContext,
                                          INPUT-OUTPUT DATASET-HANDLE phContextDataset BY-REFERENCE) .
            {Consultingwerk/OERA/callloop-end.i}

            IF hAppServer = SESSION:HANDLE THEN
                ServiceManager:ProcessServiceLifeCycle() .
        END.

        ASSIGN
            poFetchDataRequest:NextContext = cNextContext[1]
            poFetchDataRequest:PrevContext = cPrevContext[1]
            .

        IF VALID-OBJECT (poFetchDataRequest:CustomParameter) AND lcParameter[1] > "":U THEN
            poFetchDataRequest:CustomParameter:Deserialize (lcParameter[1]) .
        IF lcParameter[1] > "":U THEN
             poFetchDataRequest:CustomParameter = Consultingwerk.Serializable:DeserializeInstance (lcParameter[1]) .

        FINALLY:
            {Consultingwerk/OERA/delete-dataset.i phDataset deleteerr}

            THIS-OBJECT:OnCollectContextFromServer (Consultingwerk.EventArgs:Empty) .

            /* Delete datasets (delayed until this procedure terminates */
            IF VALID-HANDLE(hDataset[2]) THEN
                DELETE OBJECT hDataset[2] NO-ERROR .

            IF VALID-HANDLE(hDataset[3]) THEN
                DELETE OBJECT hDataset[3] NO-ERROR .

            IF VALID-HANDLE(hDataset[4]) THEN
                DELETE OBJECT hDataset[4] NO-ERROR .

            IF VALID-HANDLE(hDataset[5]) THEN
                DELETE OBJECT hDataset[5] NO-ERROR .

            IF VALID-HANDLE(hDataset[6]) THEN
                DELETE OBJECT hDataset[6] NO-ERROR .

            IF VALID-HANDLE(hDataset[7]) THEN
                DELETE OBJECT hDataset[7] NO-ERROR .

            IF VALID-HANDLE(hDataset[8]) THEN
                DELETE OBJECT hDataset[8] NO-ERROR .

            IF VALID-HANDLE(hDataset[9]) THEN
                DELETE OBJECT hDataset[9] NO-ERROR .

            IF VALID-HANDLE(hDataset[10]) THEN
                DELETE OBJECT hDataset[10] NO-ERROR .
        END FINALLY.

     END METHOD .

    /**
     * Purpose: Saves modified (Create, Update, Delete) data to a Business Entity
     * Notes:   It's recommended but not required to just pass a changes dataset
     *          The phContextDataset parameter is intended to be passed by-reference
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @param hChangesDataset INPUT-OUTPUT DATASET-HANDLE The dataset with the modifications to the submitted
     * @param pcContext INPUT-OUTPUT The context to be passed to the business entity
     * @param poParameter An optional serializable parameter object to be exchanged with the business entity
     * @param phContextDataset The handle of the context dataset for this call
     */
    METHOD PUBLIC VOID SubmitData (pcPartition    AS CHARACTER,
                                   pcEntityName   AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE hChangesDataset,
                                   INPUT-OUTPUT pcContext      AS CHARACTER,
                                   poParameter                 AS ISerializable,
                                   phContextDataset AS HANDLE):

        DEFINE VARIABLE hAppServer      AS HANDLE   NO-UNDO .
        DEFINE VARIABLE lcParameter     AS LONGCHAR NO-UNDO .

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcEntityName, "SubmitData":U, "") .

        THIS-OBJECT:OnPrepareContextForServer (Consultingwerk.EventArgs:Empty) .

        hAppServer = THIS-OBJECT:ConnectService (pcPartition, pcEntityName) .

        IF VALID-OBJECT (poParameter) THEN
            ASSIGN lcParameter = poParameter:Serialize () .

        /* Mike Fechner, Consultingwerk Ltd. 01.05.2013
           Bug SCL-57: Handling of STOP-Conditions from the back end */
        DO ON STOP UNDO, RETURN ERROR NEW StopConditionException
            (SUBSTITUTE ("A stop condition has been raised while waiting for the backend.~nBackend component: &1"{&TRAN},
                         pcEntityName),
             0):

            {Consultingwerk/OERA/callloop-begin.i}
            RUN VALUE(THIS-OBJECT:ServiceInterfacePath + "/proSIsubmit.p":U) ON hAppServer
                                      (pcEntityName,
                                       INPUT-OUTPUT DATASET-HANDLE hChangesDataset,
                                       INPUT-OUTPUT pcContext,
                                       INPUT-OUTPUT lcParameter,
                                       INPUT-OUTPUT DATASET-HANDLE phContextDataset BY-REFERENCE) .
            {Consultingwerk/OERA/callloop-end.i}

            IF hAppServer = SESSION:HANDLE THEN
                ServiceManager:ProcessServiceLifeCycle() .
        END.

        IF VALID-OBJECT (poParameter) AND lcParameter > "":U THEN
            poParameter:Deserialize (lcParameter) .

        FINALLY:
            {Consultingwerk/OERA/delete-dataset.i hChangesDataset}

            THIS-OBJECT:OnCollectContextFromServer (Consultingwerk.EventArgs:Empty) .
        END FINALLY .

    END METHOD .

    /**
     * Purpose: Saves modified (Create, Update, Delete) data to a Business Entity
     * Notes:   It's recommended but not required to just pass a changes dataset
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @param hChangesDataset INPUT-OUTPUT DATASET-HANDLE The dataset with the modifications to the submitted
     * @param pcContext INPUT-OUTPUT The context to be passed to the business entity
     * @param poParameter An optional serializable parameter object to be exchanged with the business entity
     */
    METHOD PUBLIC VOID SubmitData (pcPartition AS CHARACTER,
                                   pcEntityName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE hChangesDataset,
                                   INPUT-OUTPUT pcContext AS CHARACTER,
                                   poParameter AS ISerializable):

        DEFINE VARIABLE hAppServer      AS HANDLE   NO-UNDO .
        DEFINE VARIABLE lcParameter     AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE hContextDataset AS HANDLE   NO-UNDO .

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcEntityName, "SubmitData":U, "") .

        /* Mike Fechner, Consultingwerk Ltd. 16.01.2013
           Pass Context Dataset to Server */
        ASSIGN hContextDataset = SessionManager:ContextDataset .

        THIS-OBJECT:OnPrepareContextForServer (Consultingwerk.EventArgs:Empty) .

        hAppServer = THIS-OBJECT:ConnectService (pcPartition, pcEntityName) .

        IF VALID-OBJECT (poParameter) THEN
            ASSIGN lcParameter = poParameter:Serialize () .

        /* Mike Fechner, Consultingwerk Ltd. 01.05.2013
           Bug SCL-57: Handling of STOP-Conditions from the back end */
        DO ON STOP UNDO, RETURN ERROR NEW StopConditionException
            (SUBSTITUTE ("A stop condition has been raised while waiting for the backend.~nBackend component: &1"{&TRAN},
                         pcEntityName),
             0):

            {Consultingwerk/OERA/callloop-begin.i}
            RUN VALUE(THIS-OBJECT:ServiceInterfacePath + "/proSIsubmit.p":U) ON hAppServer
                                      (pcEntityName,
                                       INPUT-OUTPUT DATASET-HANDLE hChangesDataset,
                                       INPUT-OUTPUT pcContext,
                                       INPUT-OUTPUT lcParameter,
                                       INPUT-OUTPUT DATASET-HANDLE hContextDataset BY-REFERENCE) .
            {Consultingwerk/OERA/callloop-end.i}

            IF hAppServer = SESSION:HANDLE THEN
                ServiceManager:ProcessServiceLifeCycle() .
        END.

        IF VALID-OBJECT (poParameter) AND lcParameter > "":U THEN
            poParameter:Deserialize (lcParameter) .

        FINALLY:
            {Consultingwerk/OERA/delete-dataset.i hChangesDataset}

            THIS-OBJECT:OnCollectContextFromServer (Consultingwerk.EventArgs:Empty) .

            IF VALID-HANDLE (hContextDataset) THEN
                DELETE OBJECT hContextDataset NO-ERROR .
        END FINALLY    .

    END METHOD .

    /**
     * Purpose: Saves modified (Create, Update, Delete) data to a Business Entity
     * Notes:   It's recommended but not required to just pass a changes dataset
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @param hChangesDataset INPUT-OUTPUT DATASET-HANDLE The dataset with the modifications to the submitted
     * @param pcContext INPUT-OUTPUT The context to be passed to the business entity
     */
    METHOD PUBLIC VOID SubmitData (pcPartition AS CHARACTER,
                                   pcEntityName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE hChangesDataset,
                                   INPUT-OUTPUT pcContext AS CHARACTER):

        DEFINE VARIABLE oNullReference AS ISerializable NO-UNDO .

        THIS-OBJECT:SubmitData (pcPartition,
                                pcEntityName,
                                INPUT-OUTPUT DATASET-HANDLE hChangesDataset BY-REFERENCE,
                                INPUT-OUTPUT pcContext,
                                oNullReference) .

        FINALLY:
            {Consultingwerk/OERA/delete-dataset.i hChangesDataset}
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Saves modified (Create, Update, Delete) data to a Business Entity
     * Notes:   It's recommended but not required to just pass a changes dataset
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @param hChangesDataset INPUT-OUTPUT DATASET-HANDLE The dataset with the modifications to the submitted
     */
    METHOD PUBLIC VOID SubmitData (pcPartition AS CHARACTER,
                                   pcEntityName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE hChangesDataset):

        DEFINE VARIABLE oNullReference AS ISerializable NO-UNDO .
        DEFINE VARIABLE cContext       AS CHARACTER     NO-UNDO .

        THIS-OBJECT:SubmitData (pcPartition,
                                pcEntityName,
                                INPUT-OUTPUT DATASET-HANDLE hChangesDataset BY-REFERENCE,
                                INPUT-OUTPUT cContext,
                                oNullReference) .

        FINALLY:
            {Consultingwerk/OERA/delete-dataset.i hChangesDataset}
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Saves modified (Create, Update, Delete) data to a Business Entity
     * Notes:   It's recommended but not required to just pass a changes dataset
     *          The phContextDataset parameter is intended to be passed by-reference
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @param hChangesDataset INPUT-OUTPUT DATASET-HANDLE The dataset with the modifications to the submitted
     * @param pcContext INPUT-OUTPUT The context to be passed to the business entity
     * @param phContextDataset The handle of the context dataset for this call
     */
    METHOD PUBLIC VOID SubmitData (pcPartition    AS CHARACTER,
                                   pcEntityName   AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE hChangesDataset,
                                   INPUT-OUTPUT pcContext      AS CHARACTER,
                                   phContextDataset AS HANDLE):

        DEFINE VARIABLE oNullReference AS ISerializable NO-UNDO .

        THIS-OBJECT:SubmitData (pcPartition,
                                pcEntityName,
                                INPUT-OUTPUT DATASET-HANDLE hChangesDataset BY-REFERENCE,
                                INPUT-OUTPUT pcContext,
                                oNullReference,
                                phContextDataset) .

        FINALLY:
            {Consultingwerk/OERA/delete-dataset.i hChangesDataset}
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Saves modified (Create, Update, Delete) data to a Business Entity
     * Notes:   It's recommended but not required to just pass a changes dataset
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @param hChangesDataset INPUT-OUTPUT DATASET-HANDLE The dataset with the modifications to the submitted
     * @param poParameter An optional serializable parameter object to be exchanged with the business entity
     */
    METHOD PUBLIC VOID SubmitData (pcPartition AS CHARACTER,
                                   pcEntityName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE hChangesDataset,
                                   poParameter AS ISerializable):

        DEFINE VARIABLE cContext AS CHARACTER NO-UNDO.

        THIS-OBJECT:SubmitData (pcPartition,
                                pcEntityName,
                                INPUT-OUTPUT DATASET-HANDLE hChangesDataset BY-REFERENCE,
                                INPUT-OUTPUT cContext,
                                poParameter) .

    END METHOD.

    /**
     * Purpose: Saves modified (Create, Update, Delete) data to a Business Entity
     * Notes:   It's recommended but not required to just pass a changes dataset
     *          The phContextDataset parameter is intended to be passed by-reference
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @param hChangesDataset INPUT-OUTPUT DATASET-HANDLE The dataset with the modifications to the submitted
     * @param poParameter An optional serializable parameter object to be exchanged with the business entity
     * @param phContextDataset The handle of the context dataset for this call
     */
    METHOD PUBLIC VOID SubmitData (pcPartition    AS CHARACTER,
                                   pcEntityName   AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE hChangesDataset,
                                   poParameter                 AS ISerializable,
                                   phContextDataset AS HANDLE):

        DEFINE VARIABLE cContext AS CHARACTER NO-UNDO.

        THIS-OBJECT:SubmitData (pcPartition,
                                pcEntityName ,
                                INPUT-OUTPUT DATASET-HANDLE hChangesDataset BY-REFERENCE,
                                INPUT-OUTPUT cContext,
                                poParameter,
                                phContextDataset) .

    END METHOD .

    /**
     * Purpose: Validates a business entity or business task name
     * Notes:   Typically used by the extensions to the OpenEdge Architect Visual
     *          Designer
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @return Logical value indicating if the Business Entity name if valid
     */
    METHOD PUBLIC LOGICAL ValidateEntityName (pcPartition AS CHARACTER,
                                              pcEntityName AS CHARACTER):

        DEFINE VARIABLE hAppServer AS HANDLE  NO-UNDO.
        DEFINE VARIABLE lValid     AS LOGICAL NO-UNDO.

        IF THIS-OBJECT:LoggingActive THEN
            THIS-OBJECT:WriteLogMessage (pcPartition, pcEntityName, "SubmitData":U, "") .

        hAppServer = THIS-OBJECT:ConnectService (pcPartition, pcEntityName) .

        /* Mike Fechner, Consultingwerk Ltd. 01.05.2013
           Bug SCL-57: Handling of STOP-Conditions from the back end */
        DO ON STOP UNDO, RETURN ERROR NEW StopConditionException
            (SUBSTITUTE ("A stop condition has been raised while waiting for the backend.~nBackend component: &1"{&TRAN},
                         pcEntityName),
             0):

            RUN VALUE (THIS-OBJECT:ServiceInterfacePath + "/proSIvalidateEntityName.p":U) ON hAppServer
                                            (pcEntityName,
                                             OUTPUT lValid) .

            IF hAppServer = SESSION:HANDLE THEN
                ServiceManager:ProcessServiceLifeCycle() .
        END.

        RETURN lValid .

    END METHOD.

    /**
     * Purpose: Writes a message to the log file
     * Notes:
     * @param pcPartition The name of the AppServer partition
     * @param pcServiceName The name of the business entity or business task
     * @param pcMethodName The name of the method in the service adapter
     * @param pcComments Additional comments
     */
    METHOD PROTECTED VOID WriteLogMessage (pcPartition AS CHARACTER,
                                           pcServiceName AS CHARACTER,
                                           pcMethodName AS CHARACTER,
                                           pcComments AS CHARACTER):

        IF CharacterType:IsNullOrEmpty (pcPartition) THEN
            pcPartition = "<default>":U .

        IF THIS-OBJECT:LoggingActive THEN
            LogManager:WriteMessage (SUBSTITUTE ("[Partition: &1 - Service Object: &2 - Method Name: &3] &4":U,
                                                 pcPartition,
                                                 pcServiceName,
                                                 pcMethodName,
                                                 pcComments),
                                     THIS-OBJECT:CUSTOM_LOG_ENTRY) .

    END METHOD.

END CLASS.
