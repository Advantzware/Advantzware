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
    File        : SmartSchedulerJobPlanTableModel_Generated
    Purpose     : Auto Generated portion of the SmartSchedulerJobPlanTableModel class
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 30/05/2016 10:38:21,381+02:00
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanBusinessEntity", type="TableModelGenerated") .

CLASS Consultingwerk.SmartFramework.Scheduler.SmartSchedulerJobPlanTableModel_Generated
    INHERITS TableModel ABSTRACT:

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
        Purpose: Gets and sets the current MaxRuntime value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MaxRuntime AS INTEGER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetIntegerFieldValue ("MaxRuntime":U) .
    END GET .
    PUBLIC SET (arg AS INTEGER):
        THIS-OBJECT:SetIntegerFieldValue ("MaxRuntime":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the MaxRuntime field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC INTEGER MaxRuntimeWhenAvailable (pDefaultValue AS INTEGER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("MaxRuntime":U):BUFFER-VALUE .
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
        Purpose: Gets and sets the current PlanningType value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PlanningType AS Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanningTypeEnum NO-UNDO
    GET:
        RETURN CAST (THIS-OBJECT:GetCharacterEnumFieldValue ("PlanningType":U, "Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanningTypeEnum":U), Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanningTypeEnum) .
    END GET .
    PUBLIC SET (arg AS Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanningTypeEnum):
        THIS-OBJECT:SetCharacterEnumFieldValue ("PlanningType":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the PlanningType field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanningTypeEnum PlanningTypeWhenAvailable (pDefaultValue AS Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanningTypeEnum):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN CAST(THIS-OBJECT:GetCharacterEnumFieldValue ("PlanningType":U, "Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanningTypeEnum":U), Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanningTypeEnum) .
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
        Purpose: Gets and sets the current RecurringMonday value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringMonday AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringMonday":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringMonday":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringMonday field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringMondayWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringMonday":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringTuesday value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringTuesday AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringTuesday":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringTuesday":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringTuesday field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringTuesdayWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringTuesday":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringWednesday value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringWednesday AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringWednesday":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringWednesday":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringWednesday field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringWednesdayWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringWednesday":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringThursday value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringThursday AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringThursday":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringThursday":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringThursday field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringThursdayWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringThursday":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringFriday value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringFriday AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringFriday":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringFriday":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringFriday field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringFridayWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringFriday":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringSaturday value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringSaturday AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringSaturday":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringSaturday":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringSaturday field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringSaturdayWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringSaturday":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringSunday value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringSunday AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringSunday":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringSunday":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringSunday field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringSundayWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringSunday":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current WeekInterval value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY WeekInterval AS INTEGER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetIntegerFieldValue ("WeekInterval":U) .
    END GET .
    PUBLIC SET (arg AS INTEGER):
        THIS-OBJECT:SetIntegerFieldValue ("WeekInterval":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the WeekInterval field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC INTEGER WeekIntervalWhenAvailable (pDefaultValue AS INTEGER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("WeekInterval":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringJanuary value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringJanuary AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringJanuary":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringJanuary":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringJanuary field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringJanuaryWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringJanuary":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringFebruary value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringFebruary AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringFebruary":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringFebruary":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringFebruary field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringFebruaryWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringFebruary":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringMarch value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringMarch AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringMarch":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringMarch":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringMarch field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringMarchWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringMarch":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringApril value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringApril AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringApril":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringApril":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringApril field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringAprilWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringApril":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringMay value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringMay AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringMay":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringMay":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringMay field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringMayWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringMay":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringJune value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringJune AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringJune":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringJune":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringJune field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringJuneWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringJune":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringJuly value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringJuly AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringJuly":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringJuly":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringJuly field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringJulyWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringJuly":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringAugust value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringAugust AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringAugust":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringAugust":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringAugust field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringAugustWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringAugust":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringSeptember value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringSeptember AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringSeptember":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringSeptember":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringSeptember field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringSeptemberWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringSeptember":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringOctober value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringOctober AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringOctober":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringOctober":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringOctober field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringOctoberWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringOctober":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringNovember value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringNovember AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringNovember":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringNovember":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringNovember field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringNovemberWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringNovember":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringDecember value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringDecember AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("RecurringDecember":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("RecurringDecember":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringDecember field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurringDecemberWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringDecember":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringDaylist value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringDaylist AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("RecurringDaylist":U) .
    END GET .
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("RecurringDaylist":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringDaylist field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER RecurringDaylistWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringDaylist":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current RecurringExecutionTimes value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecurringExecutionTimes AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("RecurringExecutionTimes":U) .
    END GET .
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("RecurringExecutionTimes":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the RecurringExecutionTimes field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER RecurringExecutionTimesWhenAvailable (pDefaultValue AS CHARACTER):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("RecurringExecutionTimes":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ValidFrom value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ValidFrom AS DATETIME-TZ NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetDatetimetzFieldValue ("ValidFrom":U) .
    END GET .
    PUBLIC SET (arg AS DATETIME-TZ):
        THIS-OBJECT:SetDatetimetzFieldValue ("ValidFrom":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the ValidFrom field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC DATETIME-TZ ValidFromWhenAvailable (pDefaultValue AS DATETIME-TZ):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("ValidFrom":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current ValidTo value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ValidTo AS DATETIME-TZ NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetDatetimetzFieldValue ("ValidTo":U) .
    END GET .
    PUBLIC SET (arg AS DATETIME-TZ):
        THIS-OBJECT:SetDatetimetzFieldValue ("ValidTo":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the ValidTo field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC DATETIME-TZ ValidToWhenAvailable (pDefaultValue AS DATETIME-TZ):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("ValidTo":U):BUFFER-VALUE .
        ELSE
            RETURN pDefaultValue .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current Active value
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Active AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("Active":U) .
    END GET .
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("Active":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the Active field when a record is available
        Notes:   Returns the default value, when no record
        @param pDefaultValue The default value to return when no record is available
        @return The field value or the default value when no record is available
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ActiveWhenAvailable (pDefaultValue AS LOGICAL):
        IF THIS-OBJECT:BufferHandle:AVAILABLE THEN
            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD ("Active":U):BUFFER-VALUE .
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
        Purpose: Returns the Filter Class for this TableModel
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Filter AS SmartSchedulerJobPlanTableModelFilter NO-UNDO
    GET:
        DEFINE VARIABLE oFilter AS SmartSchedulerJobPlanTableModelFilter NO-UNDO.

        oFilter = NEW SmartSchedulerJobPlanTableModelFilter (THIS-OBJECT) .

        &IF PROVERSION BEGINS "10" &THEN
        ASSIGN THIS-OBJECT:SavedFilterReference = oFilter .
        &ENDIF

        RETURN oFilter .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSchedulerJobPlanTableModel_Generated class
        Notes:
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartSchedulerJobPlanTableModel_Generated (poDatasetModel AS DatasetModel, phBufferHandle AS HANDLE):

        SUPER (poDatasetModel, phBufferHandle).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSchedulerJobPlanTableModel_Generated class
        Notes:   Creates a view model
        @param poTableModel The reference to SmartSchedulerJobPlanTableModel on which this SmartSchedulerJobPlanTableModel servers as a ViewModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED SmartSchedulerJobPlanTableModel_Generated (poTableModel AS SmartSchedulerJobPlanTableModel, phBufferHandle AS HANDLE):

        SUPER (poTableModel, phBufferHandle).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartSchedulerJobPlanTableModel as a view model for the current record
       Notes:
       @return The SmartSchedulerJobPlanTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartSchedulerJobPlanTableModel CreateCurrentRecordView ():

        RETURN CAST (THIS-OBJECT:CreateCurrentRecordViewInternal (),
                     SmartSchedulerJobPlanTableModel) .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartSchedulerJobPlanTableModel as a view model
       Notes:
       @return The SmartSchedulerJobPlanTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartSchedulerJobPlanTableModel CreateView ():

        RETURN CAST (THIS-OBJECT:CreateViewInternal (),
                     SmartSchedulerJobPlanTableModel) .

    END METHOD.

END CLASS.
