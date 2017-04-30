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
    File        : IServiceAdapter
    Purpose     : Interface allowing customized implementations of the
                  OERA ServiceAdapter (client side)
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jun 01 09:25:47 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

INTERFACE Consultingwerk.OERA.IServiceAdapter:

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
    DEFINE PUBLIC PROPERTY BlockedByReconnectDialog  AS LOGICAL NO-UNDO
    GET.

    /**
     * Purpose: Raised when an Async Task has completed
     * Notes:
     * @param sender The object that raised the AsyncTaskCompleted event
     * @param e The AsyncTaskCompletedEventArgs with the data for the event
     */
    DEFINE PUBLIC EVENT AsyncTaskCompleted SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                           e AS AsyncTaskCompletedEventArgs).

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
                                     OUTPUT DATASET-HANDLE phDataset) .

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
                                     phContextDataset AS HANDLE) .

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
                                     poParameter AS ISerializable) .

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
                                     phContextDataset AS HANDLE) .

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
                                   phContextDataset AS HANDLE) .

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
                                   phContextDataset AS HANDLE) .

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
                                   phContextDataset AS HANDLE) .

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
                                   phContextDataset AS HANDLE) .

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
                                   phContextDataset AS HANDLE) .

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
                                   phContextDataset AS HANDLE) .

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
                                   poParameter AS Consultingwerk.ISerializable) .

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
                                   poParameter AS Consultingwerk.ISerializable) .

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
                                   poParameter AS Consultingwerk.ISerializable) .

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
                                   poParameter AS Consultingwerk.ISerializable) .

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
                                   poParameter AS Consultingwerk.ISerializable) .

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
                                   poParameter AS Consultingwerk.ISerializable) .

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
                                          poContext AS Progress.Lang.Object) .

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
                                          poContext AS Progress.Lang.Object) .

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
                                          poContext AS Progress.Lang.Object) .

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
                                          poContext AS Progress.Lang.Object) .

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
                                          poContext AS Progress.Lang.Object) .

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
                                          poContext AS Progress.Lang.Object) .

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
                                          poContext AS Progress.Lang.Object) .

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
                                          poContext AS Progress.Lang.Object) .

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
                                          poContext AS Progress.Lang.Object) .

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
                                          poContext AS Progress.Lang.Object) .

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
                                          poContext AS Progress.Lang.Object) .

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
                                          poContext AS Progress.Lang.Object) .

    /**
     * Purpose: Reads data from the Backend
     * Notes:   Request Parameters are send using the IFetchDataRequest object
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @param poFetchDataRequest The IFetchDataRequest object with the data for this request
     * @param phDataSet OUTPUT DATASET-HANDLE The resulting dataset
     */
    METHOD PUBLIC VOID RetrieveData (pcPartition        AS CHARACTER,
                                     pcEntityName       AS CHARACTER,
                                     poFetchDataRequest AS IFetchDataRequest,
                                     OUTPUT DATASET-HANDLE phDataSet) .
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
                                     phContextDataset AS HANDLE) .

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
                                   phContextDataset AS HANDLE) .

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
                                   poParameter AS ISerializable) .


    /**
     * Purpose: Saves modified (Create, Update, Delete) data to a Business Entity
     * Notes:   It's recommended but not required to just pass a changes dataset
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @param hChangesDataset INPUT-OUTPUT DATASET-HANDLE The dataset with the modifications to the submitted
     * @param pcContext INPUT-OUTPUT The context to be passed to the business entity
     */
    METHOD PUBLIC VOID SubmitData (pcPartition            AS CHARACTER,
                                   pcEntityName           AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE hChangesDataset,
                                   INPUT-OUTPUT pcContext AS CHARACTER) .

    /**
     * Purpose: Saves modified (Create, Update, Delete) data to a Business Entity
     * Notes:   It's recommended but not required to just pass a changes dataset
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @param hChangesDataset INPUT-OUTPUT DATASET-HANDLE The dataset with the modifications to the submitted
     */
    METHOD PUBLIC VOID SubmitData (pcPartition            AS CHARACTER,
                                   pcEntityName           AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE hChangesDataset) .

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
                                   phContextDataset AS HANDLE) .

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
                                   poParameter AS ISerializable) .

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
                                   phContextDataset AS HANDLE) .

    /**
     * Purpose: Validates a business entity name
     * Notes:   Typically used by the extensions to the OpenEdge Architect Visual
     *          Designer
     * @param pcPartition The name of the AppServer partition for this call or "" for the default partition
     * @param pcEntityName The name of the business entity
     * @return Logical value indicating if the Business Entity name if valid
     */
    METHOD PUBLIC LOGICAL ValidateEntityName (pcPartition    AS CHARACTER,
                                              pcEntityName   AS CHARACTER) .

END INTERFACE.
