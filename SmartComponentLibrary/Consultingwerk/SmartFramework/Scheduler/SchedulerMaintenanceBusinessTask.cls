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
    File        : SchedulerMaintenanceBusinessTask
    Purpose     : Business Task with supporting methods for scheduler
                  maintenance
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 04 14:45:48 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                FROM PROPATH .
USING Consultingwerk.OERA.*                     FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Consultingwerk.Util.*                     FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartFramework.Scheduler.SchedulerMaintenanceBusinessTask
    IMPLEMENTS IBusinessService:

    /*------------------------------------------------------------------------------
        Purpose: Returns the method names of the provided Business Task
        Notes:
        @param poParameter The GetBusinessTaskMethodsParameter with the data for this request
    ----------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetBusinessTaskMethods (poParameter AS GetBusinessTaskMethodsParameter):

        ObjectAssert:IsValid(poParameter, "poParameter":U) .

&IF DEFINED (AblReflection) NE 0 &THEN
        DEFINE VARIABLE oClass       AS Progress.Lang.Class NO-UNDO .
        DEFINE VARIABLE cMethodNames AS CHARACTER           NO-UNDO.

        oClass = Progress.Lang.Class:GetClass (poParameter:BusinessTaskName) .

        IF NOT VALID-OBJECT (oClass) THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Invalid class name (&1)! &2Please select a valid class name before selecting the Method!"{&TRAN},
                                                  poParameter:BusinessTaskName, Consultingwerk.Environment:NewLine), 0).

        cMethodNames = AblReflectionHelper:GetMethodNamesByParameterCount (oClass,
                                                                           0,
                                                                           Progress.Reflect.Flags:Public OR Progress.Reflect.Flags:Instance).

        ASSIGN cMethodNames = ListHelper:Remove(cMethodNames, "Clone":U, ",":U)
               cMethodNames = ListHelper:Remove(cMethodNames, "GetClass":U, ",":U)
               cMethodNames = ListHelper:Remove(cMethodNames, "ToString":U, ",":U)
               .

        poParameter:MethodNames = cMethodNames .
&ENDIF

    END METHOD.

END CLASS.
