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
    File        : BusinessTaskSchedulerJobCommand
    Purpose     : Executes a Business Task from the Scheduler
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Thu May 26 12:13:39 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH.
USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.Exceptions.*               FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Scheduler.BusinessTaskSchedulerJobCommand
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    IMPLEMENTS ISchedulerJobCommand:

    /*------------------------------------------------------------------------------
        Purpose: Get/Sets the class name of the BusinessTask
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i BusinessTaskClassName  CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Get/Sets the method name
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i BusinessTaskMethodName CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Editor Control used by the Function maintenance
                 Viewer
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY EditorControl AS CHARACTER NO-UNDO INIT "Consultingwerk.Windows.Framework.Scheduler.BusinessTaskSchedulerJobCommandControl":U
    GET.

    /**
     * Purpose: Returns the image representing this Function Call Parameter
     * Notes:
     */
    DEFINE PUBLIC STATIC PROPERTY ImageKey AS CHARACTER INITIAL "Consultingwerk/Windows/Framework/Images/SchedulerJobCommand/objects.png":U NO-UNDO
    GET.

    /**
     * Purpose: Constructor for the InvokeStaticMethodCallParameter class
     * Notes:
     */
    CONSTRUCTOR PUBLIC BusinessTaskSchedulerJobCommand ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

    /**
     * Purpose: Starts the Scheduler Job Command
     * Notes:
     * @param poParameter The parmeter object for the job execution (from the SmartSchedulerJobStatus table)
     */
     METHOD PUBLIC VOID Start (poParameter AS Progress.Lang.Object):

        ServiceInterface:InvokeTask (THIS-OBJECT:BusinessTaskClassName,
                                     THIS-OBJECT:BusinessTaskMethodName,
                                     poParameter) .

    END METHOD .

END CLASS.
