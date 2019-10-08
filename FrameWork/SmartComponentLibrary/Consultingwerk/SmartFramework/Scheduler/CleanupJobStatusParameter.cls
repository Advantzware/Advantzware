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
    File        : CleanupJobStatusParameter
    Purpose     : ParameterObject for the CleanupJobStatus Task
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Mon Nov 28 22:49:18 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Scheduler.CleanupJobStatusParameter
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the value for DeleteFailedFrom
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i DeleteFailedFrom  INTEGER} .

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the value for DeleteFinishedFrom
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i DeleteFinishedFrom  INTEGER} .

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the value for DeleteCancelledFrom
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i DeleteCancelledFrom  INTEGER} .

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the value for DeleteRunningFrom
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i DeleteRunningFrom  INTEGER} .

    /**
     * Purpose: Constructor for the CleanupJobStatusParameter class
     * Notes:
     * @param piDeleteFailedFrom    Number of days from now on from where the records with JobStatus "Failed" get deleted
     * @param piDeleteFinishedFrom  Number of days from now on from where the records with JobStatus "Finished" get deleted
     * @param piDeleteCancelledFrom Number of days from now on from where the records with JobStatus "Cancelled" get deleted
     * @param piDeleteRunningFrom   Number of days from now on from where the records with JobStatus "Running" get deleted
     */
    CONSTRUCTOR PUBLIC CleanupJobStatusParameter (piDeleteFailedFrom    AS INTEGER,
                                                  piDeleteFinishedFrom  AS INTEGER,
                                                  piDeleteCancelledFrom AS INTEGER,
                                                  piDeleteRunningFrom   AS INTEGER):
        THIS-OBJECT ().

        ASSIGN
            THIS-OBJECT:DeleteFailedFrom    = piDeleteFailedFrom
            THIS-OBJECT:DeleteFinishedFrom  = piDeleteFinishedFrom
            THIS-OBJECT:DeleteCancelledFrom = piDeleteCancelledFrom
            THIS-OBJECT:DeleteRunningFrom   = piDeleteRunningFrom
            .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the CleanupJobStatusParameter class
     * Notes:
     */
    CONSTRUCTOR PUBLIC CleanupJobStatusParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR.

END CLASS.
