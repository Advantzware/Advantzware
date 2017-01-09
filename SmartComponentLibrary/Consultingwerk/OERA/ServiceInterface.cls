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
    File        : ServiceInterface
    Purpose     : The OERA ServiceInterface which is the suggested
                  method of accessing business logic
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 04 07:37:32 CET 2010
    Notes       : SCL-1483: Handling STOP conditions raised by the invoked
                  Business Entity / Business Service call by thrownig an
                  AppError. During the implementation, we have encountered
                  crashes of the AVM (exspecially when the FILL timed out),
                  so that we are considering this the safer approach
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                    FROM PROPATH .
USING Consultingwerk.Exceptions.*                         FROM PROPATH .
USING Consultingwerk.Framework.*                          FROM PROPATH .
USING Consultingwerk.Framework.Authorization.*            FROM PROPATH .
USING Consultingwerk.Framework.Authorization.Exceptions.* FROM PROPATH .
USING Consultingwerk.Framework.Server.*                   FROM PROPATH .
USING Consultingwerk.OERA.*                               FROM PROPATH .
USING Consultingwerk.OERA.Enum.*                          FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.*                    FROM PROPATH .
USING Consultingwerk.Util.*                               FROM PROPATH .
USING Progress.Lang.*                                     FROM PROPATH .

CLASS Consultingwerk.OERA.ServiceInterface:

    DEFINE STATIC VARIABLE oRequestAuthorizationProvider AS IRequestAuthorizationProvider NO-UNDO .

    /**
     * Purpose: Raised when the Service Interface begins handling a request
     * Notes:
     * @param e The Consultingwerk.EventArgs with the data for this event
     */
    DEFINE PUBLIC STATIC EVENT Activated SIGNATURE VOID (e AS Consultingwerk.EventArgs).

    /**
     * Purpose: Raised when the Service Interface begins handling a request
     * Notes:   This event is raised after the Activated event. This event
     *          may be used by application or framework components to initialize
     *          for a request after the session- or context management has
     *          initialized
     * @param e The Consultingwerk.EventArgs with the data for this event
     */
    DEFINE PUBLIC STATIC EVENT AfterActivated SIGNATURE VOID (e AS Consultingwerk.EventArgs).

    /**
     * Purpose: Raised when the Service Interface is finished handling a request
     * Notes:   This event is raised after the Deactivated event. This event
     *          may be used by application or framework components to reset
     *          after a request
     * @param e The Consultingwerk.EventArgs with the data for this event
     */
    DEFINE PUBLIC STATIC EVENT AfterDeactivated SIGNATURE VOID (e AS Consultingwerk.EventArgs).

    /**
     * Purpose: Raised when the Service Interface is finished handling a request
     * Notes:
     * @param e The Consultingwerk.EventArgs with the data for this event
     */
    DEFINE PUBLIC STATIC EVENT Deactivated SIGNATURE VOID (e AS Consultingwerk.EventArgs).

    /**
     * Purpose: Gets the custom log file entry used by the ServiceInterface class
     * Notes:
     */
    DEFINE PUBLIC STATIC PROPERTY CUSTOM_LOG_ENTRY AS CHARACTER INITIAL 'ServiceInterface':U NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Indicates if the ServiceInterface is currently performing a request
     * Notes:
     */
    DEFINE PUBLIC STATIC PROPERTY IsActive AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Get's and Set's the logging level of the ServiceInterface class
     * Notes:   Currently only None and Request are supported. Possible
     *          future values are: RequestDetail, Response, ResponseDetail
     */
    DEFINE PUBLIC STATIC PROPERTY LoggingLevel AS ServiceInterfaceLoggingLevelEnum NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Static constructor of the ServiceInterface class
     * Notes:
     */
    CONSTRUCTOR STATIC ServiceInterface ():

        EvaluateRequestAuthorizationProvider () .

        /* Default LoggingLevel is None. Should be changed by application code
           when required */
        ASSIGN
            ServiceInterface:LoggingLevel = ServiceInterfaceLoggingLevelEnum:None .

    END CONSTRUCTOR.

    /**
     * Purpose: Executed when we begin handling a request
     * Notes:   This method is PUBLIC to allow Pacfic WebHandlers and REST Service
     *          classes to activate the session prior to calling into the service
     *          interface. This is important to allow the RestServerSessionActivator
     *          (or similar activate handlers) to instantiate the Context Dataset
     *          (from the Context Dataset Store) prior to calling into the
     *          Service Interface
     */
    METHOD PUBLIC STATIC VOID Activate ():

        IF ServiceInterface:IsActive THEN
            RETURN .

        ServiceInterface:IsActive = TRUE .

        ServiceInterface:OnActivated (Consultingwerk.EventArgs:Empty) .

        ServiceInterface:OnAfterActivated (Consultingwerk.EventArgs:Empty) .

    END METHOD.

    /**
     * Purpose: Executed when we finish handling a request
     * Notes:   See notes of the Activate() method
     */
    METHOD PUBLIC STATIC VOID Deactivate ():

        IF NOT ServiceInterface:IsActive THEN
            RETURN .

        ServiceInterface:OnDeactivated (Consultingwerk.EventArgs:Empty) .

        ServiceInterface:OnAfterDeactivated (Consultingwerk.EventArgs:Empty) .

        ServiceInterface:IsActive = FALSE .

    END METHOD.

    /**
     * Purpose: (Re-)Evaluates the current IRequestAuthorizationProvider service
     * Notes:
     */
    METHOD PUBLIC STATIC VOID EvaluateRequestAuthorizationProvider ():

        ASSIGN oRequestAuthorizationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.Authorization.IRequestAuthorizationProvider} .

    END METHOD.

    /**
     * Purpose: Reads data from a Business Entity
     * Notes:   Request Parameters are send using the IFetchDataRequest object
     *          Server-side code calling directly into the Service Interface is _NOT_
     *          supposed to call this method "BY-REFERENCE" as this is the place where
     *          the caller receives a copy of the Business Entities ProDataset
     * @param pcEntityName The name of the Business Entity
     * @param poFetchDataRequest The IFetchDataRequest instance with the data for this call
     * @param phDataset OUTPUT DATASET-HANDLE The dataset with the resultset - do not pass BY-REFERENCE
     */
    METHOD PUBLIC STATIC VOID FetchData (pcEntityName AS CHARACTER,
                                         poFetchDataRequest AS IFetchDataRequest,
                                         OUTPUT DATASET-HANDLE phDataset):

        DEFINE VARIABLE oBusinessEntity AS IBusinessEntity NO-UNDO .
        DEFINE VARIABLE iStopAfter      AS INTEGER         NO-UNDO INITIAL 0 .
        DEFINE VARIABLE dtStart         AS DATETIME        NO-UNDO .

        { Consultingwerk/OERA/sibeginrequest.i }

        /* Mike Fechner, Consultingwerk Ltd. 14.04.2011
           Optional logging of ServiceInterface calls */
        IF NOT ServiceInterface:LoggingLevel:Equals (ServiceInterfaceLoggingLevelEnum:None) THEN
            ServiceInterfaceLogger:LogFetchDataRequest (pcEntityName, poFetchDataRequest) .

        IF ServiceInterface:LoggingLevel:Equals (ServiceInterfaceLoggingLevelEnum:Runtime) THEN
            ASSIGN dtStart = NOW .

        /* Mike Fechner, Consultingwerk Ltd. 24.11.2012
           Perform request validation */
        IF VALID-OBJECT (oRequestAuthorizationProvider) AND
           oRequestAuthorizationProvider:IsFetchDataRestricted (pcEntityName,
                                                                poFetchDataRequest) THEN
            UNDO, THROW NEW ServiceInterfaceMethodNotAuthorizedException (pcEntityName,
                                                                          "FetchData":U) .

        oBusinessEntity = GetBusinessEntity (pcEntityName) .

        IF VALID-OBJECT (poFetchDataRequest) THEN
            ASSIGN iStopAfter = poFetchDataRequest:StopAfter .

        DO ON ERROR UNDO, THROW
        /* Mike Fechner, Consultingwerk Ltd. 01.05.2013
           Bug SCL-31: STOP-AFTER support
           Variable stop-after is not supported on OE10.2B */
        &IF PROVERSION NE "10.2B" &THEN
           STOP-AFTER iStopAfter
        &ENDIF
            ON STOP UNDO, RETURN ERROR NEW AppError ("A stop condition has occurred."{&TRAN}, 0):

            /* SCL-1528: When a Business Entity may be killed, by the life cycle, we
             * can't receive the Dataset by-reference as that database might be voided
             * by the siendrequest.i calling into the ServiceManager:ProcessLifeCycle
             */
            IF ServiceManager:HasLifeCycleDefinition (pcEntityName) THEN
                oBusinessEntity:FetchData (poFetchDataRequest,
                                           OUTPUT DATASET-HANDLE phDataset) .
            ELSE
                oBusinessEntity:FetchData (poFetchDataRequest,
                                           OUTPUT DATASET-HANDLE phDataset BY-REFERENCE) .

            @SuppressUnusedWarnings.
            CATCH ignoreerr AS IgnoreAtServiceInterface:
                /* noop */
            END CATCH.
        END.

        FINALLY:
            IF ServiceInterface:LoggingLevel:Equals (ServiceInterfaceLoggingLevelEnum:Runtime) THEN
                ServiceInterfaceLogger:LogFetchDataRequestRuntime (pcEntityName, dtStart) .

            { Consultingwerk/OERA/delete-dataset.i phDataset }
            { Consultingwerk/OERA/siendrequest.i }
        END FINALLY.
    END METHOD.

    /**
     * Purpose: Returns an Empty Dataset (schema)
     * Notes:   The caller of this method is responsible to clean up the Dataset
     *          when no longer used! The caller should always invoke this method
     *          with the dataset output parameter BY-REFERENCE!
     * @param pcEntityName The name of the Business Entity
     * @param phDataset OUTPUT DATASET-HANDLE The empty dataset
     */
    METHOD PUBLIC STATIC VOID FetchDataset (pcEntityName AS CHARACTER,
                                            OUTPUT DATASET-HANDLE phDataset):

        DEFINE VARIABLE oBusinessEntity AS IBusinessEntity NO-UNDO .

        { Consultingwerk/OERA/sibeginrequest.i }

        /* Mike Fechner, Consultingwerk Ltd. 14.04.2011
           Optional logging of ServiceInterface calls */
        IF NOT ServiceInterface:LoggingLevel:Equals (ServiceInterfaceLoggingLevelEnum:None) THEN
            ServiceInterfaceLogger:LogFetchDatasetRequest (pcEntityName) .

        /* Mike Fechner, Consultingwerk Ltd. 24.11.2012
           Perform request validation */
        IF VALID-OBJECT (oRequestAuthorizationProvider) AND
           oRequestAuthorizationProvider:IsFetchDataRestricted (pcEntityName,
                                                                ?) THEN
            UNDO, THROW NEW ServiceInterfaceMethodNotAuthorizedException (pcEntityName,
                                                                          "FetchData":U) .

        oBusinessEntity = GetBusinessEntity (pcEntityName) .

        DO ON ERROR UNDO, THROW
           ON STOP UNDO, RETURN ERROR NEW AppError ("A stop condition has occurred."{&TRAN}, 0):

            oBusinessEntity:FetchDataset (OUTPUT DATASET-HANDLE phDataset) .

            @SuppressUnusedWarnings.
            CATCH ignoreerr AS IgnoreAtServiceInterface:
                /* noop */
            END CATCH.
        END.

        FINALLY:
            { Consultingwerk/OERA/delete-dataset.i phDataset }
            { Consultingwerk/OERA/siendrequest.i }
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Returns the reference to the Business Entity instance
     * Notes:   Facade to Consultingwerk.OERA.ServiceManager:GetBusinessEntity
     *          (pcEntityName)
     * @param pcEntityName The name of the Business Entity to return
     * @return The reference to the Business Entity
     */
    METHOD PUBLIC STATIC IBusinessEntity GetBusinessEntity (pcEntityName AS CHARACTER):

        DEFINE VARIABLE oBusinessService AS Progress.Lang.Object          NO-UNDO .
        DEFINE VARIABLE oException       AS InvokeBusinessEntityException NO-UNDO .
        DEFINE VARIABLE i                AS INTEGER                       NO-UNDO .

        DO ON STOP UNDO, RETURN ERROR NEW AppError ("A stop condition has occurred."{&TRAN}, 0):
            oBusinessService = ServiceManager:GetBusinessService (pcEntityName) .
        END.

        IF TYPE-OF (oBusinessService, IBusinessEntity) THEN
            RETURN CAST (oBusinessService, IBusinessEntity) .
        ELSE
            RETURN ? .

        /* Mike Fechner, Consultingwerk Ltd. 15.06.2015
           SCL-860: Create Exception to provide more context */
        CATCH err AS Progress.Lang.Error:
            oException = NEW InvokeBusinessEntityException (err,
                                                            SUBSTITUTE ("Unable to start Business Entity: &1"{&TRAN}, QUOTER (pcEntityName)),
                                                            0) .

            oException:BusinessService = pcEntityName .

            DO i = 1 TO err:NumMessages:
               oException:AddMessage (err:GetMessage (i),
                                      err:GetMessageNum (i)) .
            END.

            UNDO, THROW oException .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Invokes a custom method of the BusinessEntity.
     * Notes:
     * @param pcEntityName The name of the Business Entity class
     * @param pcMethodName The name of the method of the task to execute
     * @param phDataset INPUT-OUTPUT DATASET-HANDLE The dataset-handle parameter to the task method
     * @param poParameter The parameter object
     */
    METHOD PUBLIC STATIC VOID InvokeMethod (pcEntityName AS CHARACTER,
                                            pcMethodName AS CHARACTER,
                                            INPUT-OUTPUT DATASET-HANDLE phDataset,
                                            poParameter AS Progress.Lang.Object) .

        DEFINE VARIABLE oBusinessEntity AS IBusinessEntity NO-UNDO .
        DEFINE VARIABLE dtStart         AS DATETIME        NO-UNDO .

        { Consultingwerk/OERA/sibeginrequest.i }

        /* Mike Fechner, Consultingwerk Ltd. 14.04.2011
           Optional logging of ServiceInterface calls */
        IF NOT ServiceInterface:LoggingLevel:Equals (ServiceInterfaceLoggingLevelEnum:None) THEN
            ServiceInterfaceLogger:LogInvokeMethodRequest (pcEntityName, pcMethodName, phDataset, poParameter) .

        IF ServiceInterface:LoggingLevel:Equals (ServiceInterfaceLoggingLevelEnum:Runtime) THEN
            ASSIGN dtStart = NOW .

        /* Mike Fechner, Consultingwerk Ltd. 24.11.2012
           Perform request validation */
        IF VALID-OBJECT (oRequestAuthorizationProvider) AND
           oRequestAuthorizationProvider:IsInvokeMethodRestricted (pcEntityName,
                                                                   pcMethodName,
                                                                   phDataset,
                                                                   poParameter) THEN
            UNDO, THROW NEW ServiceInterfaceMethodNotAuthorizedException (pcEntityName,
                                                                          pcMethodName) .

        oBusinessEntity = GetBusinessEntity (pcEntityName) .

        DO ON ERROR UNDO, THROW
           ON STOP UNDO, RETURN ERROR NEW AppError ("A stop condition has occurred."{&TRAN}, 0):

            /* Mike Fechner, Consultingwerk Ltd. 16.01.2016
               SCL-1167 - workaournd ABL issue with DYNAMIC-INVOKE when the parameter object = ? */
            &IF DEFINED (AblReflection) NE 0 &THEN
            IF NOT VALID-OBJECT (poParameter) THEN DO:

                DEFINE VARIABLE oMethod        AS Progress.Reflect.Method NO-UNDO .
                DEFINE VARIABLE oParameterList AS ParameterList           NO-UNDO .

                oMethod = AblReflectionHelper:GetInvokableMethod (oBusinessEntity:GetClass(), pcMethodName, 1) .

                IF NOT VALID-OBJECT (oMethod) THEN
                    UNDO, THROW NEW MethodInvokationException (SUBSTITUTE ("Invalid method &1 in &2"{&TRAN},
                                                                           pcMethodName,
                                                                           pcEntityName),
                                                               0,
                                                               pcMethodName) .

                oParameterList = NEW ParameterList (2) .

                oParameterList:SetParameter (1, "DATASET-HANDLE":U,             IoModeEnum:InputOutput, phDataset) .
                oParameterList:SetParameter (2, "CLASS Progress.Lang.Object":U, IoModeEnum:Input,       ?) .

                oMethod:Invoke (oBusinessEntity, oParameterList).
            END.
            ELSE
            &ENDIF

            DYNAMIC-INVOKE (oBusinessEntity,
                            pcMethodName,
                            INPUT-OUTPUT DATASET-HANDLE phDataset,
                            poParameter) .

            @SuppressUnusedWarnings.
            CATCH ignoreerr AS IgnoreAtServiceInterface:
                /* noop */
            END CATCH.
        END.

        /* Mike Fechner, Consultingwerk Ltd. 05.05.2011
           Ensure that the Dataset handle get's deleted */
        FINALLY:
            IF ServiceInterface:LoggingLevel:Equals (ServiceInterfaceLoggingLevelEnum:Runtime) THEN
                ServiceInterfaceLogger:LogInvokeMethodRequestRuntime (pcEntityName, pcMethodName, dtStart) .

            { Consultingwerk/OERA/delete-dataset.i phDataset deleteerr}
            { Consultingwerk/OERA/siendrequest.i }
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Invokes a custom method of the BusinessEntity.
     * Notes:
     * @param pcEntityName The name of the Business Entity class
     * @param pcMethodName The name of the method of the task to execute
     * @param phDataset INPUT-OUTPUT DATASET-HANDLE The dataset-handle parameter to the task method
     * @param plcParameter The LONGCHAR representation of the serialized parameter object
     */
    METHOD PUBLIC STATIC VOID InvokeMethod (pcEntityName AS CHARACTER,
                                            pcMethodName AS CHARACTER,
                                            INPUT-OUTPUT DATASET-HANDLE phDataset,
                                            INPUT-OUTPUT plcParameter AS LONGCHAR) .

        DEFINE VARIABLE oParameter AS Consultingwerk.ISerializable NO-UNDO .

        { Consultingwerk/OERA/sibeginrequest.i }

        /* Mike Fechner, Consultingwerk Ltd. 14.04.2011
           Optional logging of ServiceInterface calls */
        IF NOT ServiceInterface:LoggingLevel:Equals (ServiceInterfaceLoggingLevelEnum:None) THEN
            ServiceInterfaceLogger:LogInvokeMethodRequest (pcEntityName, pcMethodName, phDataset, plcParameter) .

        IF plcParameter > "":U THEN
            oParameter = Consultingwerk.Serializable:DeserializeInstance (plcParameter) .

        ServiceInterface:InvokeMethod (pcEntityName,
                                       pcMethodName,
                                       INPUT-OUTPUT DATASET-HANDLE phDataset BY-REFERENCE,
                                       oParameter) .

        IF VALID-OBJECT (oParameter) THEN
            plcParameter = oParameter:Serialize () .

        /* Mike Fechner, Consultingwerk Ltd. 05.05.2011
           Ensure that the Dataset handle get's deleted */
        FINALLY:
            { Consultingwerk/OERA/delete-dataset.i phDataset deleteerr}
            { Consultingwerk/OERA/siendrequest.i }
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the method of the task to execute
     * @param piNumDatasets The number of datasets that were passed in from the consumer
     * @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The first dataset-handle parameter to the task method
     * @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The second dataset-handle parameter to the task method
     * @param phDataset3 INPUT-OUTPUT DATASET-HANDLE The third dataset-handle parameter to the task method
     * @param phDataset4 INPUT-OUTPUT DATASET-HANDLE The fourth dataset-handle parameter to the task method
     * @param phDataset5 INPUT-OUTPUT DATASET-HANDLE The fifth dataset-handle parameter to the task method
     * @param poParameter The parameter object
     */
    METHOD PUBLIC STATIC VOID InvokeTask (pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          piNumDatasets AS INTEGER,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset4,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset5,
                                          poParameter AS Progress.Lang.Object):

        DEFINE VARIABLE oTask      AS Consultingwerk.OERA.IBusinessService NO-UNDO .
        DEFINE VARIABLE oException AS InvokeBusinessTaskException          NO-UNDO .
        DEFINE VARIABLE i          AS INTEGER                              NO-UNDO .

        &IF DEFINED (AblReflection) NE 0 &THEN
        @SuppressUnusedWarnings.
        DEFINE VARIABLE oMethod        AS Progress.Reflect.Method NO-UNDO .
        @SuppressUnusedWarnings.
        DEFINE VARIABLE oParameterList AS ParameterList           NO-UNDO .
        &ENDIF

        { Consultingwerk/OERA/sibeginrequest.i }

        IF pcMethodName > "":U THEN .
        ELSE ASSIGN pcMethodName = "Invoke":U .

        /* Mike Fechner, Consultingwerk Ltd. 24.11.2012
           Perform request validation */
        IF VALID-OBJECT (oRequestAuthorizationProvider) AND
           oRequestAuthorizationProvider:IsInvokeTaskRestricted (pcTaskName,
                                                                 pcMethodName,
                                                                 piNumDatasets,
                                                                 phDataset1,
                                                                 phDataset2,
                                                                 phDataset3,
                                                                 phDataset4,
                                                                 phDataset5,
                                                                 poParameter) THEN
            UNDO, THROW NEW ServiceInterfaceMethodNotAuthorizedException (pcTaskName,
                                                                          pcMethodName) .

        DO ON ERROR UNDO, THROW:
            oTask = ServiceManager:GetBusinessService (pcTaskName) .

            /* Mike Fechner, Consultingwerk Ltd. 15.06.2015
               SCL-860: Create Exception to provide more context */
            CATCH err AS Progress.Lang.Error:
                oException = NEW InvokeBusinessTaskException (err,
                                                              SUBSTITUTE ("Unable to start Business Task: &1"{&TRAN}, QUOTER (pcTaskName)),
                                                              0) .
                oException:BusinessService = pcTaskName .

                DO i = 1 TO err:NumMessages:
                   oException:AddMessage (err:GetMessage (i),
                                          err:GetMessageNum (i)) .
                END.

                UNDO, THROW oException .
            END CATCH.
        END.

        DO ON ERROR UNDO, THROW
           ON STOP UNDO, RETURN ERROR NEW AppError ("A stop condition has occurred."{&TRAN}, 0):

            CASE piNumDatasets:
                WHEN 5 THEN
                    {Consultingwerk/OERA/dynamic-invoke-workaround.i phDataset1 phDataset2 phDataset3 phDataset4 phDataset5}
                    DYNAMIC-INVOKE (oTask,
                                    pcMethodName,
                                    INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                    INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                    INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                    INPUT-OUTPUT DATASET-HANDLE phDataset4,
                                    INPUT-OUTPUT DATASET-HANDLE phDataset5,
                                    poParameter) .
                WHEN 4 THEN
                    {Consultingwerk/OERA/dynamic-invoke-workaround.i phDataset1 phDataset2 phDataset3 phDataset4}
                    DYNAMIC-INVOKE (oTask,
                                    pcMethodName,
                                    INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                    INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                    INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                    INPUT-OUTPUT DATASET-HANDLE phDataset4,
                                    poParameter) .
                WHEN 3 THEN
                    {Consultingwerk/OERA/dynamic-invoke-workaround.i phDataset1 phDataset2 phDataset3}
                    DYNAMIC-INVOKE (oTask,
                                    pcMethodName,
                                    INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                    INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                    INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                    poParameter) .
                WHEN 2 THEN
                    {Consultingwerk/OERA/dynamic-invoke-workaround.i phDataset1 phDataset2}
                    DYNAMIC-INVOKE (oTask,
                                    pcMethodName,
                                    INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                    INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                    poParameter) .
                WHEN 1 THEN
                    {Consultingwerk/OERA/dynamic-invoke-workaround.i phDataset1}
                    DYNAMIC-INVOKE (oTask,
                                    pcMethodName,
                                    INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                    poParameter) .
                WHEN 0 THEN
                    {Consultingwerk/OERA/dynamic-invoke-workaround.i}
                    DYNAMIC-INVOKE (oTask,
                                    pcMethodName,
                                    poParameter) .
                OTHERWISE
                    UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to InvokeTask with &1 Datasets"{&TRAN}, piNumDatasets), 0) .

            END CASE  .

            @SuppressUnusedWarnings.
            CATCH ignoreerr AS IgnoreAtServiceInterface:
                /* noop */
            END CATCH.
        END.

        /* Mike Fechner, Consultingwerk Ltd. 05.05.2011
           Ensure that the Dataset handle get's deleted */
        FINALLY:
            { Consultingwerk/OERA/delete-dataset.i phDataset1 }
            { Consultingwerk/OERA/delete-dataset.i phDataset2 }
            { Consultingwerk/OERA/delete-dataset.i phDataset3 }
            { Consultingwerk/OERA/delete-dataset.i phDataset4 }
            { Consultingwerk/OERA/delete-dataset.i phDataset5 }
            { Consultingwerk/OERA/siendrequest.i }
        END FINALLY.
    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the method of the task to execute
     * @param piNumDatasets The number of datasets that were passed in from the consumer
     * @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The first dataset-handle parameter to the task method
     * @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The second dataset-handle parameter to the task method
     * @param phDataset3 INPUT-OUTPUT DATASET-HANDLE The third dataset-handle parameter to the task method
     * @param phDataset4 INPUT-OUTPUT DATASET-HANDLE The fourth dataset-handle parameter to the task method
     * @param phDataset5 INPUT-OUTPUT DATASET-HANDLE The fifth dataset-handle parameter to the task method
     * @param plcParameter The LONGCHAR representation of the serialized parameter object
     */
    METHOD PUBLIC STATIC VOID InvokeTask (pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          piNumDatasets AS INTEGER,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset4,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset5,
                                          INPUT-OUTPUT plcParameter AS LONGCHAR):

        DEFINE VARIABLE oParameter AS Consultingwerk.ISerializable NO-UNDO .

        { Consultingwerk/OERA/sibeginrequest.i }

        IF plcParameter > "":U THEN
            oParameter = Consultingwerk.Serializable:DeserializeInstance (plcParameter) .

        ServiceInterface:InvokeTask (pcTaskName,
                                     pcMethodName,
                                     piNumDatasets,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset1 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset2 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset3 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset4 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset5 BY-REFERENCE,
                                     oParameter) .

        /* Mike Fechner, Consultingwerk Ltd. 16.09.2014
           SCL-459 : ensure deserialization of oParameter */
        IF VALID-OBJECT (oParameter) THEN
            ASSIGN plcParameter = oParameter:Serialize() .

        /* Mike Fechner, Consultingwerk Ltd. 05.05.2011
           Ensure that the Dataset handle get's deleted */
        FINALLY:
            { Consultingwerk/OERA/delete-dataset.i phDataset1 }
            { Consultingwerk/OERA/delete-dataset.i phDataset2 }
            { Consultingwerk/OERA/delete-dataset.i phDataset3 }
            { Consultingwerk/OERA/delete-dataset.i phDataset4 }
            { Consultingwerk/OERA/delete-dataset.i phDataset5 }
            { Consultingwerk/OERA/siendrequest.i }
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method
     * @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The handle of the second dataset to be passed to the task method
     * @param phDataset3 INPUT-OUTPUT DATASET-HANDLE The handle of the third dataset to be passed to the task method
     * @param phDataset4 INPUT-OUTPUT DATASET-HANDLE The handle of the fourth dataset to be passed to the task method
     * @param phDataset5 INPUT-OUTPUT DATASET-HANDLE The handle of the fifth dataset to be passed to the task method
     * @param poParameter The parameter object to the method
     */
    METHOD PUBLIC STATIC VOID InvokeTask (pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset4,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset5,
                                          poParameter AS Progress.Lang.Object):

        ServiceInterface:InvokeTask (pcTaskName,
                                     pcMethodName,
                                     5,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset1 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset2 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset3 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset4 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset5 BY-REFERENCE,
                                     poParameter) .

        /* Mike Fechner, Consultingwerk Ltd. 20.02.2014
           Ensure that the Dataset handle get's deleted */
        FINALLY:
            { Consultingwerk/OERA/delete-dataset.i phDataset1 }
            { Consultingwerk/OERA/delete-dataset.i phDataset2 }
            { Consultingwerk/OERA/delete-dataset.i phDataset3 }
            { Consultingwerk/OERA/delete-dataset.i phDataset4 }
            { Consultingwerk/OERA/delete-dataset.i phDataset5 }
        END FINALLY.

    END METHOD .


    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method
     * @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The handle of the second dataset to be passed to the task method
     * @param phDataset3 INPUT-OUTPUT DATASET-HANDLE The handle of the third dataset to be passed to the task method
     * @param phDataset4 INPUT-OUTPUT DATASET-HANDLE The handle of the fourth dataset to be passed to the task method
     * @param poParameter The parameter object to the method
     */
    METHOD PUBLIC STATIC VOID InvokeTask (pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset4,
                                          poParameter AS Progress.Lang.Object):

        DEFINE VARIABLE hDataset5 AS HANDLE NO-UNDO.

        ServiceInterface:InvokeTask (pcTaskName,
                                     pcMethodName,
                                     4,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset1 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset2 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset3 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset4 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE hDataset5 BY-REFERENCE,
                                     poParameter) .

        /* Mike Fechner, Consultingwerk Ltd. 20.02.2014
           Ensure that the Dataset handle get's deleted */
        FINALLY:
            { Consultingwerk/OERA/delete-dataset.i phDataset1 }
            { Consultingwerk/OERA/delete-dataset.i phDataset2 }
            { Consultingwerk/OERA/delete-dataset.i phDataset3 }
            { Consultingwerk/OERA/delete-dataset.i phDataset4 }
            { Consultingwerk/OERA/delete-dataset.i hDataset5 }
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method
     * @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The handle of the second dataset to be passed to the task method
     * @param phDataset3 INPUT-OUTPUT DATASET-HANDLE The handle of the third dataset to be passed to the task method
     * @param poParameter The parameter object to the method
     */
    METHOD PUBLIC STATIC VOID InvokeTask (pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                          poParameter AS Progress.Lang.Object):

        DEFINE VARIABLE hDataset4 AS HANDLE NO-UNDO.
        DEFINE VARIABLE hDataset5 AS HANDLE NO-UNDO.

        ServiceInterface:InvokeTask (pcTaskName,
                                     pcMethodName,
                                     3,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset1 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset2 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset3 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE hDataset4 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE hDataset5 BY-REFERENCE,
                                     poParameter) .

        /* Mike Fechner, Consultingwerk Ltd. 20.02.2014
           Ensure that the Dataset handle get's deleted */
        FINALLY:
            { Consultingwerk/OERA/delete-dataset.i phDataset1 }
            { Consultingwerk/OERA/delete-dataset.i phDataset2 }
            { Consultingwerk/OERA/delete-dataset.i phDataset3 }
            { Consultingwerk/OERA/delete-dataset.i hDataset4 }
            { Consultingwerk/OERA/delete-dataset.i hDataset5 }
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method
     * @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The handle of the second dataset to be passed to the task method
     * @param poParameter The parameter object to the method
     */
    METHOD PUBLIC STATIC VOID InvokeTask (pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                          poParameter AS Progress.Lang.Object):

        DEFINE VARIABLE hDataset3 AS HANDLE NO-UNDO.
        DEFINE VARIABLE hDataset4 AS HANDLE NO-UNDO.
        DEFINE VARIABLE hDataset5 AS HANDLE NO-UNDO.

        ServiceInterface:InvokeTask (pcTaskName,
                                     pcMethodName,
                                     2,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset1 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset2 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE hDataset3 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE hDataset4 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE hDataset5 BY-REFERENCE,
                                     poParameter) .

        /* Mike Fechner, Consultingwerk Ltd. 20.02.2014
           Ensure that the Dataset handle get's deleted */
        FINALLY:
            { Consultingwerk/OERA/delete-dataset.i phDataset1 }
            { Consultingwerk/OERA/delete-dataset.i phDataset2 }
            { Consultingwerk/OERA/delete-dataset.i hDataset3 }
            { Consultingwerk/OERA/delete-dataset.i hDataset4 }
            { Consultingwerk/OERA/delete-dataset.i hDataset5 }
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method
     * @param poParameter The parameter object to the method
     */
    METHOD PUBLIC STATIC VOID InvokeTask (pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                          poParameter AS Progress.Lang.Object):

        DEFINE VARIABLE hDataset2 AS HANDLE NO-UNDO.
        DEFINE VARIABLE hDataset3 AS HANDLE NO-UNDO.
        DEFINE VARIABLE hDataset4 AS HANDLE NO-UNDO.
        DEFINE VARIABLE hDataset5 AS HANDLE NO-UNDO.

        ServiceInterface:InvokeTask (pcTaskName,
                                     pcMethodName,
                                     1,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset1 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE hDataset2 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE hDataset3 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE hDataset4 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE hDataset5 BY-REFERENCE,
                                     poParameter) .

        /* Mike Fechner, Consultingwerk Ltd. 20.02.2014
           Ensure that the Dataset handle get's deleted */
        FINALLY:
            { Consultingwerk/OERA/delete-dataset.i phDataset1 }
            { Consultingwerk/OERA/delete-dataset.i hDataset2 }
            { Consultingwerk/OERA/delete-dataset.i hDataset3 }
            { Consultingwerk/OERA/delete-dataset.i hDataset4 }
            { Consultingwerk/OERA/delete-dataset.i hDataset5 }
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Invokes a method in a Task class.
     * Notes:   When no MethodName is passed then "Invoke" will be executed as the
     *          default task method
     * @param pcTaskName The name of the task class
     * @param pcMethodName The name of the business entity method to invoke
     * @param poParameter The parameter object to the method
     */
    METHOD PUBLIC STATIC VOID InvokeTask (pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          poParameter AS Progress.Lang.Object):

        DEFINE VARIABLE hDataset1 AS HANDLE NO-UNDO.
        DEFINE VARIABLE hDataset2 AS HANDLE NO-UNDO.
        DEFINE VARIABLE hDataset3 AS HANDLE NO-UNDO.
        DEFINE VARIABLE hDataset4 AS HANDLE NO-UNDO.
        DEFINE VARIABLE hDataset5 AS HANDLE NO-UNDO.

        ServiceInterface:InvokeTask (pcTaskName,
                                     pcMethodName,
                                     0,
                                     INPUT-OUTPUT DATASET-HANDLE hDataset1 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE hDataset2 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE hDataset3 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE hDataset4 BY-REFERENCE,
                                     INPUT-OUTPUT DATASET-HANDLE hDataset5 BY-REFERENCE,
                                     poParameter) .

        /* Mike Fechner, Consultingwerk Ltd. 20.02.2014
           Ensure that the Dataset handle get's deleted */
        FINALLY:
            { Consultingwerk/OERA/delete-dataset.i hDataset1 }
            { Consultingwerk/OERA/delete-dataset.i hDataset2 }
            { Consultingwerk/OERA/delete-dataset.i hDataset3 }
            { Consultingwerk/OERA/delete-dataset.i hDataset4 }
            { Consultingwerk/OERA/delete-dataset.i hDataset5 }
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Raises the Activated event
     * Notes:
     * @param e The Consultingwerk.EventArgs with the data for this event
     */
    METHOD PROTECTED STATIC VOID OnActivated (e AS Consultingwerk.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = Consultingwerk.EventArgs:Empty .

        ServiceInterface:Activated:Publish (e) .

    END METHOD.

    /**
     * Purpose: Raises the AfterActivated event
     * Notes:
     * @param e The Consultingwerk.EventArgs with the data for this event
     */
    METHOD PROTECTED STATIC VOID OnAfterActivated (e AS Consultingwerk.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = Consultingwerk.EventArgs:Empty .

        ServiceInterface:AfterActivated:Publish (e) .

    END METHOD.

    /**
     * Purpose: Raises the AfterDeactivated event
     * Notes:
     * @param e The Consultingwerk.EventArgs with the data for this event
     */
    METHOD PROTECTED STATIC VOID OnAfterDeactivated (e AS Consultingwerk.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = Consultingwerk.EventArgs:Empty .

        ServiceInterface:AfterDeactivated:Publish (e) .

    END METHOD.

    /**
     * Purpose: Raises the Deactivated event
     * Notes:
     * @param e The Consultingwerk.EventArgs with the data for this event
     */
    METHOD PROTECTED STATIC VOID OnDeactivated (e AS Consultingwerk.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = Consultingwerk.EventArgs:Empty .

        ServiceInterface:Deactivated:Publish (e) .

    END METHOD.

    /**
     * Purpose: Saves modified (Create, Update, Delete) data to a Business Entity
     * Notes:   It's recommended but not required to just pass a changes dataset
     * @param pcEntityName The name of the Business Entity to save the modified data with
     * @param phDataset INPUT-OUTPUT DATASET-HANDLE The handle of the dataset with the modified values
     */
    METHOD PUBLIC STATIC VOID SaveChanges (pcEntityName AS CHARACTER,
                                           INPUT-OUTPUT DATASET-HANDLE phDataset):

        DEFINE VARIABLE oNullReference AS Progress.Lang.Object NO-UNDO .

        ServiceInterface:SaveChanges (pcEntityName,
                                      INPUT-OUTPUT DATASET-HANDLE phDataset BY-REFERENCE,
                                      oNullReference) .

        FINALLY:
            { Consultingwerk/OERA/delete-dataset.i phDataset }
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Saves modified (Create, Update, Delete) data to a Business Entity
     * Notes:   It's recommended but not required to just pass a changes dataset
     * @param pcEntityName The name of the Business Entity to save the modified data with
     * @param phDataset INPUT-OUTPUT DATASET-HANDLE The handle of the dataset with the modified values
     * @param poParameter The reference to the optional parameter object instance
     */
    METHOD PUBLIC STATIC VOID SaveChanges (pcEntityName AS CHARACTER,
                                           INPUT-OUTPUT DATASET-HANDLE phDataset,
                                           poParameter AS Progress.Lang.Object):

        DEFINE VARIABLE oBusinessEntity AS IBusinessEntity NO-UNDO .
        DEFINE VARIABLE dtStart         AS DATETIME        NO-UNDO .

        { Consultingwerk/OERA/sibeginrequest.i }

        /* Mike Fechner, Consultingwerk Ltd. 14.04.2011
           Optional logging of ServiceInterface calls */
        IF NOT ServiceInterface:LoggingLevel:Equals (ServiceInterfaceLoggingLevelEnum:None) THEN
            ServiceInterfaceLogger:LogSaveChangesRequest (pcEntityName, phDataset) .

        IF ServiceInterface:LoggingLevel:Equals (ServiceInterfaceLoggingLevelEnum:Runtime) THEN
            ASSIGN dtStart = NOW .

        /* Mike Fechner, Consultingwerk Ltd. 24.11.2012
           Perform request validation */
        IF VALID-OBJECT (oRequestAuthorizationProvider) AND
           oRequestAuthorizationProvider:IsSaveChangesRestricted (pcEntityName,
                                                                  phDataset) THEN
            UNDO, THROW NEW ServiceInterfaceMethodNotAuthorizedException (pcEntityName,
                                                                          "SaveChanges":U) .

        oBusinessEntity = GetBusinessEntity (pcEntityName) .


        DO ON ERROR UNDO, THROW
           ON STOP UNDO, RETURN ERROR NEW AppError ("A stop condition has occurred."{&TRAN}, 0):

            IF VALID-OBJECT (poParameter) THEN
                oBusinessEntity:SaveChanges (INPUT-OUTPUT DATASET-HANDLE phDataset BY-REFERENCE, poParameter) .
            ELSE
                oBusinessEntity:SaveChanges (INPUT-OUTPUT DATASET-HANDLE phDataset BY-REFERENCE) .

            @SuppressUnusedWarnings.
            CATCH ignoreerr AS IgnoreAtServiceInterface:
                /* noop */
            END CATCH.
        END.

        FINALLY:
            IF ServiceInterface:LoggingLevel:Equals (ServiceInterfaceLoggingLevelEnum:Runtime) THEN
                ServiceInterfaceLogger:LogSaveChangesRequestRuntime (pcEntityName, dtStart) .

            { Consultingwerk/OERA/delete-dataset.i phDataset }
            { Consultingwerk/OERA/siendrequest.i }
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Stops an Business Entity Instance
     * Notes:   Facade to Consultingwerk.OERA.ServiceManager:StopBusinessEntity
     *          (pcEntityName)
     * @param pcEntityName The name of the Business Entity to stop
     */
    METHOD PUBLIC STATIC VOID StopBusinessEntity (pcEntityName AS CHARACTER):

        ServiceManager:StopBusinessService (pcEntityName) .

    END METHOD.

    /**
     * Purpose: Validates a Business Entity or Business Task Name
     * Notes:
     * @param pcEntityName The name of the Business Entity to validate
     * @return Logical value indicating if the Business Entity name is valid
     */
    METHOD PUBLIC STATIC LOGICAL ValidateBusinessEntityName (pcEntityName AS CHARACTER):

        DEFINE VARIABLE oBusinessEntity AS IBusinessEntity NO-UNDO .
        DEFINE VARIABLE lResult         AS LOGICAL         NO-UNDO .

        { Consultingwerk/OERA/sibeginrequest.i }

        ASSIGN
            oBusinessEntity = Consultingwerk.OERA.ServiceInterface:GetBusinessEntity (pcEntityName)
            lResult         = VALID-OBJECT (oBusinessEntity) .

        RETURN lResult .

        @SuppressUnusedWarnings.
        CATCH err AS Progress.Lang.Error:
            ASSIGN
                lResult = FALSE .
        END CATCH.

        FINALLY:
            { Consultingwerk/OERA/siendrequest.i }
        END FINALLY.

    END METHOD.

END CLASS.
