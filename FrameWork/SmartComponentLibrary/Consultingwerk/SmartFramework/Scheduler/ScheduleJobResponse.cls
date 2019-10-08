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
    File        : ScheduleJobResponse
    Purpose     : Response object of the SchedulerService:ScheduleJob
                  method
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 13 09:49:01 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Scheduler.ScheduleJobResponse
    IMPLEMENTS IScheduleJobResponse
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i SchedulerJobGuid CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i SchedulerJobStatusGuid CHARACTER} .

    /**
     * Purpose: Constructor for the ScheduleJobResponse class
     * Notes:
     * @param pcSchedulerJobGuid The value for the SchedulerJobGuid property
     * @param pcSchedulerJobStatusGuid The value for the SchedulerJobStatusGuid property
     */
    CONSTRUCTOR PUBLIC ScheduleJobResponse (pcSchedulerJobGuid       AS CHARACTER,
                                            pcSchedulerJobStatusGuid AS CHARACTER):
        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:SchedulerJobGuid       = pcSchedulerJobGuid
               THIS-OBJECT:SchedulerJobStatusGuid = pcSchedulerJobStatusGuid .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the ScheduleJobResponse class
     * Notes:
     */
    CONSTRUCTOR PUBLIC ScheduleJobResponse ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR.

END CLASS.
