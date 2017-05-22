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
    File        : GetNextAvailableJobStatusParameter
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 16 23:01:48 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Scheduler.GetNextAvailableJobStatusParameter
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i MaxWaitTime INTEGER} .
    {Consultingwerk/JsonSerializableProperty.i SchedulerJobStatusGuid CHARACTER} .

    /**
     * Purpose: Constructor for the GetNextAvailableJobStatusParameter class
     * Notes:
     * @param piMaxWaitTime The maximum number of seconds to attempt to lock the next available job
     */
    CONSTRUCTOR PUBLIC GetNextAvailableJobStatusParameter (piMaxWaitTime AS INTEGER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:MaxWaitTime = piMaxWaitTime .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the GetNextAvailableJobStatusParameter class
     * Notes:
     */
    CONSTRUCTOR PUBLIC GetNextAvailableJobStatusParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR.

END CLASS.
