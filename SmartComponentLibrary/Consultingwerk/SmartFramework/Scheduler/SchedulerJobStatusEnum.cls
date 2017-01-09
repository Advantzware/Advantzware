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
    File        : SchedulerJobStatusEnum
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Wed May 25 20:21:49 CEST 2016
    Notes       : OnHold:    The Job execution is paused prior to it's execution
                  Scheduled: The Job is waiting for execution at the planned time
                  Running:   The Job is beeing executed
                  Finished:  The Job has finished with success
                  Failed:    The Job has failed with an error
                  TimedOut:  The Job execution has timed out
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (NativeEnums) NE 0 &THEN
&GLOBAL-DEFINE CLASS ENUM
&ELSE
&GLOBAL-DEFINE CLASS CLASS
&ENDIF

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.Framework.Collections.*    FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

{&CLASS} Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusEnum
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF
        :

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF

    {Consultingwerk/EnumMember.i OnHold     0 SchedulerJobStatusEnum}
    {Consultingwerk/EnumMember.i Scheduled  1 SchedulerJobStatusEnum}
    {Consultingwerk/EnumMember.i Running    2 SchedulerJobStatusEnum}
    {Consultingwerk/EnumMember.i Finished   3 SchedulerJobStatusEnum}
    {Consultingwerk/EnumMember.i Failed     4 SchedulerJobStatusEnum}
    {Consultingwerk/EnumMember.i TimedOut   5 SchedulerJobStatusEnum}
    {Consultingwerk/EnumMember.i Cancelled  6 SchedulerJobStatusEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF

&IF "{&CLASS}" EQ "CLASS" &THEN
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SchedulerJobStatusEnum
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE SchedulerJobStatusEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().

        ASSIGN THIS-OBJECT:Value = piValue
               THIS-OBJECT:Label = pcLabel .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterList with the names of the Enum members
        Notes:
        @return The CharacterList with the names of the Enum members
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CharacterList GetNames ():

        RETURN NEW CharacterList ('{&EnumMembers}':U).

    END METHOD .

    {Consultingwerk/EnumFromString.i Consultingwerk.SmartFramework.Scheduler.SchedulerJobStatusEnum}

&ENDIF

END {&CLASS} .

