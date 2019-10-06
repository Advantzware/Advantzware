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
    File        : UpdateJobStatusParameter
    Purpose     : Parameter Object for the UpdateJobStatus method
                  of the SchedulerJobStatusBusinessEntity
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 24 00:00:22 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Scheduler.UpdateJobStatusParameter
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i SchedulerJobStatusGuid CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i StatusText CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i PercentFinished INTEGER} .

    /**
     * Purpose: Constructor for the UpdateJobStatusParameter class
     * Notes:
     * @param pcSchedulerJobStatusGuid The GUID of the SmartSchedulerJobStatus record
     * @param pcStatusText The text to update
     * @param piPercentFinished The percentage finished
     */
    CONSTRUCTOR PUBLIC UpdateJobStatusParameter (pcSchedulerJobStatusGuid AS CHARACTER,
                                                 pcStatusText AS CHARACTER,
                                                 piPercentFinished AS INTEGER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:SchedulerJobStatusGuid = pcSchedulerJobStatusGuid
               THIS-OBJECT:StatusText             = pcStatusText
               THIS-OBJECT:PercentFinished        = piPercentFinished .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the UpdateJobStatusParameter class
     * Notes:
     */
    CONSTRUCTOR PUBLIC UpdateJobStatusParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR.

END CLASS.
