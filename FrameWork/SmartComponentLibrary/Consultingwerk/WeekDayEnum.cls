/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : WeekDayEnum
    Purpose     : Enumeration of WeekDays
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Mar 20 07:21:56 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.WeekDayEnum INHERITS Enum:

    {Consultingwerk/EnumMember.i Monday 1 WeekDayEnum}
    {Consultingwerk/EnumMember.i Tuesday 2 WeekDayEnum}
    {Consultingwerk/EnumMember.i Wednesday 3 WeekDayEnum}
    {Consultingwerk/EnumMember.i Thursday 4 WeekDayEnum}
    {Consultingwerk/EnumMember.i Friday 5 WeekDayEnum}
    {Consultingwerk/EnumMember.i Saturday 6 WeekDayEnum}
    {Consultingwerk/EnumMember.i Sunday 7 WeekDayEnum}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the WeekDayEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE WeekDayEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().

        ASSIGN THIS-OBJECT:Value = piValue
               THIS-OBJECT:Label = pcLabel .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the WeekDayEnum member that matches the value of the ABL
                 weekday function
        Notes:
        @param piAblWeekDay The value of the ABL weekday function
        @return The WeekDay enum member matching the input value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC WeekDayEnum FromAblWeekDay (piAblWeekDay AS INTEGER):

        CASE piAblWeekDay:
            WHEN 1 THEN
                RETURN WeekDayEnum:Sunday .
            WHEN 2 THEN
                RETURN WeekDayEnum:Monday .
            WHEN 3 THEN
                RETURN WeekDayEnum:Tuesday .
            WHEN 4 THEN
                RETURN WeekDayEnum:Wednesday .
            WHEN 5 THEN
                RETURN WeekDayEnum:Thursday .
            WHEN 6 THEN
                RETURN WeekDayEnum:Friday .
            WHEN 7 THEN
                RETURN WeekDayEnum:Saturday .
        END CASE .

        RETURN ? .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the WeekDayEnum member that matches the value of the ISO
                 weekday number
        Notes:   Following the ISO 8601 norm used in Germany and most European countries
                 where the week starts with Monday
        @param piIsoWeekDay The number of the ISO weekday
        @return The WeekDay enum member matching the input value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC WeekDayEnum FromIsoWeekDay (piIsoWeekDay AS INTEGER):

        CASE piIsoWeekDay:
            WHEN 1 THEN
                RETURN WeekDayEnum:Monday .
            WHEN 2 THEN
                RETURN WeekDayEnum:Tuesday .
            WHEN 3 THEN
                RETURN WeekDayEnum:Wednesday .
            WHEN 4 THEN
                RETURN WeekDayEnum:Thursday .
            WHEN 5 THEN
                RETURN WeekDayEnum:Friday .
            WHEN 6 THEN
                RETURN WeekDayEnum:Saturday .
            WHEN 7 THEN
                RETURN WeekDayEnum:Sunday .
        END CASE .

        RETURN ? .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the WeekDayEnum member that matches the passed in character
                 value
        Notes:   Only the first one or two characters are used for comparison:
                 M, TU, W, TH, F, SA, SU
        @param pcWeekDay The character value to return the weekday for
        @return The WeekDay enum member matching the input value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC WeekDayEnum FromString (pcWeekDay AS CHARACTER):

        IF pcWeekDay BEGINS "M":U THEN
            RETURN WeekDayEnum:Monday .
        IF pcWeekDay BEGINS "TU":U THEN
            RETURN WeekDayEnum:Tuesday .
        IF pcWeekDay BEGINS "W":U THEN
            RETURN WeekDayEnum:Wednesday .
        IF pcWeekDay BEGINS "TH":U THEN
            RETURN WeekDayEnum:Thursday .
        IF pcWeekDay BEGINS "F":U THEN
            RETURN WeekDayEnum:Friday .
        IF pcWeekDay BEGINS "SA":U THEN
            RETURN WeekDayEnum:Saturday .
        IF pcWeekDay BEGINS "SU":U THEN
            RETURN WeekDayEnum:Sunday .

        RETURN ? .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterList with the names of the Enum members
        Notes:
        @return The CharacterList with the names of the Enum members
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CharacterList GetNames ():

        RETURN NEW CharacterList ('{&EnumMembers}':U).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Integer number of the day of the week matching the
                 current WeekDayEnum member
        Notes:   Using US week day norm, compatible to the ABL Weekday function
        @return The Integer number of the day of the week
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC INTEGER ToAblWeekDay ():

        DEFINE VARIABLE iDay AS INTEGER NO-UNDO.

        ASSIGN iDay = THIS-OBJECT:Value + 1 .

        IF iDay = 8 THEN
            RETURN 1 .
        ELSE
            RETURN iDay .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Integer number of the day of the week matching the
                 current WeekDayEnum member
        Notes:   Following the ISO 8601 norm used in Germany and most European countries
                 where the week starts with Monday
        @return The Integer number of the day of the week
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC INTEGER ToIsoWeekDay ():

        RETURN THIS-OBJECT:Value .

    END METHOD .

END CLASS.
