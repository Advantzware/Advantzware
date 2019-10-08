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
    File        : GetBusinessTaskMethodsParameter
    Purpose     : Parameter class for the GetBusinessTaskMethods method
                  of the SchedulerMaintenanceBusinessTask class
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 04 14:46:57 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Scheduler.GetBusinessTaskMethodsParameter
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i BusinessTaskName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i MethodNames CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetBusinessTaskMethodsParameter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetBusinessTaskMethodsParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetBusinessTaskMethodsParameter class
        Notes:
        @param pcBusinessTaskName The name of the Business Task to return the method names from
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetBusinessTaskMethodsParameter (pcBusinessTaskName AS CHARACTER):
        THIS-OBJECT ().
        THIS-OBJECT:BusinessTaskName = pcBusinessTaskName .

    END CONSTRUCTOR.

END CLASS.
