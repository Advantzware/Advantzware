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
    File        : CleanupSchedulerJobCommand
    Purpose     : Executes a CleanupProcedure from the Scheduler
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Mon Nov 28 19:05:27 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.Exceptions.*               FROM PROPATH .
USING Consultingwerk.OERA.*                     FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Consultingwerk.Util.*                     FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartFramework.Scheduler.CleanupSchedulerJobCommand
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    IMPLEMENTS ISchedulerJobCommand
    USE-WIDGET-POOL:

    
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

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Editor Control used by the Function maintenance
                 Viewer
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY EditorControl AS CHARACTER NO-UNDO INIT "Consultingwerk.Windows.Framework.Scheduler.CleanupSchedulerJobCommandControl":U
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the image representing this Function Call Parameter
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ImageKey AS CHARACTER INITIAL "Consultingwerk/Windows/Framework/Images/SchedulerJobCommand/sponge.png":U NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CleanupSchedulerJobCommand class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CleanupSchedulerJobCommand ():
        
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

    /**
     * Purpose: Starts the Scheduler Job Command
     * Notes:
     * @param poParameter The parmeter object for the job execution (from the SmartSchedulerJobStatus table)
     */
     METHOD PUBLIC VOID Start (poParameter AS Progress.Lang.Object):

        DEFINE VARIABLE oJobStatus AS SchedulerJobStatusDatasetModel NO-UNDO.
        
        oJobStatus = NEW SchedulerJobStatusDatasetModel () .

        oJobStatus:CleanupJobStatus (NEW CleanupJobStatusParameter (THIS-OBJECT:DeleteFailedFrom, 
                                                                    THIS-OBJECT:DeleteFinishedFrom,
                                                                    THIS-OBJECT:DeleteCancelledFrom,
                                                                    THIS-OBJECT:DeleteRunningFrom)) .

        FINALLY:
            
            GarbageCollectorHelper:DeleteObject (oJobStatus).
            
        END FINALLY.
                    
    END METHOD .

END CLASS.
