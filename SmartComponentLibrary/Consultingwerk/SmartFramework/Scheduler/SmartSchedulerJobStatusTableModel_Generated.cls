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
    File        : SmartSchedulerJobStatusTableModel_Generated
    Purpose     : Auto Generated portion of the SmartSchedulerJobStatusTableModel class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 28/08/2016 19:49:51,086+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusBusinessEntity", type="TableModelGenerated") .

CLASS Consultingwerk.SmartFramework.Scheduler.SmartSchedulerJobStatusTableModel_Generated 
    INHERITS TableModel ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current SchedulerJobStatusGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SchedulerJobStatusGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("SchedulerJobStatusGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("SchedulerJobStatusGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the SchedulerJobStatusGuid field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER SchedulerJobStatusGuidWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("SchedulerJobStatusGuid":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current SchedulerJobGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SchedulerJobGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("SchedulerJobGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("SchedulerJobGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the SchedulerJobGuid field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER SchedulerJobGuidWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("SchedulerJobGuid":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current SchedulerJobPlanGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SchedulerJobPlanGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("SchedulerJobPlanGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("SchedulerJobPlanGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the SchedulerJobPlanGuid field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER SchedulerJobPlanGuidWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("SchedulerJobPlanGuid":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ScheduleDateTime value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ScheduleDateTime AS DATETIME-TZ NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetDatetimetzFieldValue ("ScheduleDateTime":U) .
    END GET . 
    PUBLIC SET (arg AS DATETIME-TZ):
        THIS-OBJECT:SetDatetimetzFieldValue ("ScheduleDateTime":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the ScheduleDateTime field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC DATETIME-TZ ScheduleDateTimeWhenAvailable (pDefaultValue AS DATETIME-TZ):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("ScheduleDateTime":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current JobName value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY JobName AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("JobName":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("JobName":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the JobName field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER JobNameWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("JobName":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current Recurring value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Recurring AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("Recurring":U) .
    END GET . 
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("Recurring":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the Recurring field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("Recurring":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current JobStatus value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY JobStatus AS Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusEnum NO-UNDO
    GET:
        RETURN CAST (THIS-OBJECT:GetCharacterEnumFieldValue ("JobStatus":U, "Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusEnum":U), Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusEnum) .
    END GET . 
    PUBLIC SET (arg AS Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusEnum):
        THIS-OBJECT:SetCharacterEnumFieldValue ("JobStatus":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the JobStatus field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusEnum JobStatusWhenAvailable (pDefaultValue AS Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusEnum):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN CAST(THIS-OBJECT:GetCharacterEnumFieldValue ("JobStatus":U, "Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusEnum":U), Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusEnum) .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current StatusText value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StatusText AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("StatusText":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("StatusText":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the StatusText field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER StatusTextWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("StatusText":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current Percentage value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Percentage AS INTEGER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetIntegerFieldValue ("Percentage":U) .
    END GET . 
    PUBLIC SET (arg AS INTEGER):
        THIS-OBJECT:SetIntegerFieldValue ("Percentage":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the Percentage field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC INTEGER PercentageWhenAvailable (pDefaultValue AS INTEGER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("Percentage":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current MaxExecutionDelay value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MaxExecutionDelay AS INTEGER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetIntegerFieldValue ("MaxExecutionDelay":U) .
    END GET . 
    PUBLIC SET (arg AS INTEGER):
        THIS-OBJECT:SetIntegerFieldValue ("MaxExecutionDelay":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the MaxExecutionDelay field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC INTEGER MaxExecutionDelayWhenAvailable (pDefaultValue AS INTEGER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("MaxExecutionDelay":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current Cancel value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Cancel AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("Cancel":U) .
    END GET . 
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("Cancel":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the Cancel field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL CancelWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("Cancel":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current StartDateTime value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StartDateTime AS DATETIME-TZ NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetDatetimetzFieldValue ("StartDateTime":U) .
    END GET . 
    PUBLIC SET (arg AS DATETIME-TZ):
        THIS-OBJECT:SetDatetimetzFieldValue ("StartDateTime":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the StartDateTime field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC DATETIME-TZ StartDateTimeWhenAvailable (pDefaultValue AS DATETIME-TZ):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("StartDateTime":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current FinishedDateTime value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FinishedDateTime AS DATETIME-TZ NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetDatetimetzFieldValue ("FinishedDateTime":U) .
    END GET . 
    PUBLIC SET (arg AS DATETIME-TZ):
        THIS-OBJECT:SetDatetimetzFieldValue ("FinishedDateTime":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the FinishedDateTime field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC DATETIME-TZ FinishedDateTimeWhenAvailable (pDefaultValue AS DATETIME-TZ):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("FinishedDateTime":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current JobParamter value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY JobParamter AS LONGCHAR NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLongcharFieldValue ("JobParamter":U) .
    END GET . 
    PUBLIC SET (arg AS LONGCHAR):
        THIS-OBJECT:SetLongcharFieldValue ("JobParamter":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the JobParamter field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR JobParamterWhenAvailable (pDefaultValue AS LONGCHAR):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("JobParamter":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ContextDataSet value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ContextDataSet AS LONGCHAR NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLongcharFieldValue ("ContextDataSet":U) .
    END GET . 
    PUBLIC SET (arg AS LONGCHAR):
        THIS-OBJECT:SetLongcharFieldValue ("ContextDataSet":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the ContextDataSet field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR ContextDataSetWhenAvailable (pDefaultValue AS LONGCHAR):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("ContextDataSet":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current UserGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UserGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("UserGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("UserGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the UserGuid field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER UserGuidWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("UserGuid":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current DataBaseConnections value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DataBaseConnections AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("DataBaseConnections":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("DataBaseConnections":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the DataBaseConnections field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER DataBaseConnectionsWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("DataBaseConnections":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current SchedulerJobName value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SchedulerJobName AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("SchedulerJobName":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("SchedulerJobName":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the SchedulerJobName field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER SchedulerJobNameWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("SchedulerJobName":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current Description value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Description AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("Description":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("Description":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the Description field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER DescriptionWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN 
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("Description":U):BUFFER-VALUE .
        ELSE 
            RETURN pDefaultValue .
    END METHOD .



    /*------------------------------------------------------------------------------
        Purpose: Returns the Filter Class for this TableModel
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Filter AS SmartSchedulerJobStatusTableModelFilter NO-UNDO 
    GET:
        DEFINE VARIABLE oFilter AS SmartSchedulerJobStatusTableModelFilter NO-UNDO.
        
        oFilter = NEW SmartSchedulerJobStatusTableModelFilter (THIS-OBJECT) .
        
        &IF PROVERSION BEGINS "10" &THEN
        ASSIGN THIS-OBJECT:SavedFilterReference = oFilter .
        &ENDIF
        
        RETURN oFilter .      
    END.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSchedulerJobStatusTableModel_Generated class
        Notes:   
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartSchedulerJobStatusTableModel_Generated (poDatasetModel AS DatasetModel, phBufferHandle AS HANDLE):
        
        SUPER (poDatasetModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSchedulerJobStatusTableModel_Generated class
        Notes:   Creates a view model
        @param poTableModel The reference to SmartSchedulerJobStatusTableModel on which this SmartSchedulerJobStatusTableModel servers as a ViewModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED SmartSchedulerJobStatusTableModel_Generated (poTableModel AS SmartSchedulerJobStatusTableModel, phBufferHandle AS HANDLE):
        
        SUPER (poTableModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartSchedulerJobStatusTableModel as a view model for the current record
       Notes:
       @return The SmartSchedulerJobStatusTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartSchedulerJobStatusTableModel CreateCurrentRecordView ():
        
        RETURN CAST (THIS-OBJECT:CreateCurrentRecordViewInternal (), 
                     SmartSchedulerJobStatusTableModel) . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartSchedulerJobStatusTableModel as a view model
       Notes:
       @return The SmartSchedulerJobStatusTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartSchedulerJobStatusTableModel CreateView ():
        
        RETURN CAST (THIS-OBJECT:CreateViewInternal (), 
                     SmartSchedulerJobStatusTableModel) . 
        
    END METHOD.
    
END CLASS.
