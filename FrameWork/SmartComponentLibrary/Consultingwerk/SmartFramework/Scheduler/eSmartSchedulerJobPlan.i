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
    File        : eSmartSchedulerJobPlan.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 31.05.2016 12:59:50
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobPlanBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartSchedulerJobPlan{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartSchedulerJobPlanBefore{&SUFFIX} &ENDIF
    FIELD SchedulerJobPlanGuid AS CHARACTER FORMAT "x(36)":U LABEL "Job Status ID":T
    FIELD SchedulerJobGuid AS CHARACTER FORMAT "x(36)":U LABEL "Job Guid":T
    FIELD MaxRuntime AS INTEGER FORMAT "zzzz9":U INIT "0":U LABEL "MaxRuntime":T
    FIELD MaxExecutionDelay AS INTEGER FORMAT "zzzz9":U INIT "0":U LABEL "Max. Delay":T
    FIELD JobParamter AS CLOB FORMAT "x(8)":U INIT ? LABEL "JobParamter":T
    FIELD PlanningType AS CHARACTER FORMAT "x(15)":U LABEL "Plan Type":T
    FIELD ScheduleDateTime AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U INIT ? LABEL "Once Now/Scheduled":T
    FIELD RecurringMonday AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring Monday":T
    FIELD RecurringTuesday AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring Tuesday":T
    FIELD RecurringWednesday AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring Wendesday":T
    FIELD RecurringThursday AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring Thursday":T
    FIELD RecurringFriday AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring Friday":T
    FIELD RecurringSaturday AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring Saturday":T
    FIELD RecurringSunday AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring Sunday":T
    FIELD WeekInterval AS INTEGER FORMAT "z9":U INIT "1":U LABEL "Week Interval":T
    FIELD RecurringJanuary AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring January":T
    FIELD RecurringFebruary AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring February":T
    FIELD RecurringMarch AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring March":T
    FIELD RecurringApril AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring April":T
    FIELD RecurringMay AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring May":T
    FIELD RecurringJune AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring June":T
    FIELD RecurringJuly AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring July":T
    FIELD RecurringAugust AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring August":T
    FIELD RecurringSeptember AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring September":T
    FIELD RecurringOctober AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring October":T
    FIELD RecurringNovember AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring November":T
    FIELD RecurringDecember AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Recurring December":T
    FIELD RecurringDaylist AS CHARACTER FORMAT "x(30)":U LABEL "Recurring Daylist":T
    FIELD RecurringExecutionTimes AS CHARACTER FORMAT "x(30)":U LABEL "Recurring Execution Times":T
    FIELD ValidFrom AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U INIT ? LABEL "JobPlan valid from":T
    FIELD ValidTo AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U INIT ? LABEL "JobPlan valid to":T
    FIELD Active AS LOGICAL FORMAT "yes/no":U INIT "Yes":U LABEL "JobPlan is active":T
    FIELD ContextDataSet AS CLOB FORMAT "x(8)":U INIT ? LABEL "ContextDataSet":T
    FIELD UserGuid AS CHARACTER FORMAT "x(36)":U LABEL "UserGuid":T
    FIELD DataBaseConnections AS CHARACTER FORMAT "x(120)":U LABEL "DataBaseConnections":T
    FIELD ClientPrincipal AS RAW
    FIELD SchedulerJobName AS CHARACTER FORMAT "x(40)":U LABEL "JobName":T

    INDEX Job SchedulerJobGuid ASCENDING
    INDEX JobPlan AS UNIQUE PRIMARY SchedulerJobPlanGuid ASCENDING

    .

    