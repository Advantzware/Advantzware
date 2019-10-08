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
    File        : SetJobStatusParameter
    Purpose     : Parameter object for the SetJobStatus method of the
                  SchedulerJobStatusBusinessEntity
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 23 20:40:36 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Scheduler.SetJobStatusParameter
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
&IF PROVERSION NE "11.4" AND PROVERSION NE "11.5" &THEN
    {&SERIALIZABLE}
&ENDIF    
        :

    {Consultingwerk/JsonSerializableProperty.i SchedulerJobStatusGuid CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i JobStatus SchedulerJobStatusEnum} .

    /**
     * Purpose: Constructor for the SetJobStatusParameter class
     * Notes:
     * @param pcSchedulerJobStatusGuid The Guid of the JobStatus record to change the status of
     * @param poJobStatus The value for the JobStatus
     */
    CONSTRUCTOR PUBLIC SetJobStatusParameter(pcSchedulerJobStatusGuid AS CHARACTER,
                                             poJobStatus AS SchedulerJobStatusEnum):
        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:SchedulerJobStatusGuid = pcSchedulerJobStatusGuid
               THIS-OBJECT:JobStatus              = poJobStatus .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the SetJobStatusParameter class
     * Notes:
     */
    CONSTRUCTOR PUBLIC SetJobStatusParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR.

END CLASS.
