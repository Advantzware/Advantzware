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
    File        : ProVersionEnum
    Purpose     : An enumeration of CHARACTER Constants with valid Progress
                  Version numbers from 10.1A on (first progress release with
                  support for OO code)
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 14 18:19:25 CET 2012
    Notes       : Property names are prefixed with a "P" because a property
                  identifier cannot start with a digit
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.ProVersionEnum:

    DEFINE PUBLIC STATIC PROPERTY P101A AS CHARACTER NO-UNDO INITIAL "10.1A":U GET.
    DEFINE PUBLIC STATIC PROPERTY P101B AS CHARACTER NO-UNDO INITIAL "10.1B":U GET.
    DEFINE PUBLIC STATIC PROPERTY P101C AS CHARACTER NO-UNDO INITIAL "10.1C":U GET.
    DEFINE PUBLIC STATIC PROPERTY P102A AS CHARACTER NO-UNDO INITIAL "10.2A":U GET.
    DEFINE PUBLIC STATIC PROPERTY P102B AS CHARACTER NO-UNDO INITIAL "10.2B":U GET.
    DEFINE PUBLIC STATIC PROPERTY P110  AS CHARACTER NO-UNDO INITIAL "11.0":U  GET.
    DEFINE PUBLIC STATIC PROPERTY P111  AS CHARACTER NO-UNDO INITIAL "11.1":U  GET.
    DEFINE PUBLIC STATIC PROPERTY P112  AS CHARACTER NO-UNDO INITIAL "11.2":U  GET.
    DEFINE PUBLIC STATIC PROPERTY P113  AS CHARACTER NO-UNDO INITIAL "11.3":U  GET.
    DEFINE PUBLIC STATIC PROPERTY P114  AS CHARACTER NO-UNDO INITIAL "11.4":U  GET.
    DEFINE PUBLIC STATIC PROPERTY P115  AS CHARACTER NO-UNDO INITIAL "11.5":U  GET.
    DEFINE PUBLIC STATIC PROPERTY P116  AS CHARACTER NO-UNDO INITIAL "11.6":U  GET.
    DEFINE PUBLIC STATIC PROPERTY P117  AS CHARACTER NO-UNDO INITIAL "11.7":U  GET.

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE ProVersionEnum ():
        SUPER ().
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Converts a PROVERSION string to a decimal representation for simpler
                 comparision
        Notes:   See SCL-479 for details
        @param pcVersionString The version string as returned by PROVERSION
        @return The decimal representation of the OpenEdge Version Number
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC DECIMAL ToDecimal (pcVersionString AS CHARACTER):

        IF pcVersionString BEGINS "10.1A":U THEN
            RETURN 10.11 .
        IF pcVersionString BEGINS "10.1B":U THEN
            RETURN 10.12 .
        IF pcVersionString BEGINS "10.1C":U THEN
            RETURN 10.13 .
        IF pcVersionString BEGINS "10.2A":U THEN
            RETURN 10.21 .
        IF pcVersionString BEGINS "10.2B":U THEN
            RETURN 10.22 .
        IF pcVersionString BEGINS "11.0":U THEN
            RETURN 11.0 .
        IF pcVersionString BEGINS "11.1":U THEN
            RETURN 11.1 .
        IF pcVersionString BEGINS "11.2":U THEN
            RETURN 11.2 .
        IF pcVersionString BEGINS "11.3":U THEN
            RETURN 11.3 .
        IF pcVersionString BEGINS "11.4":U THEN
            RETURN 11.4 .
        IF pcVersionString BEGINS "11.5":U THEN
            RETURN 11.5 .
        IF pcVersionString BEGINS "11.6":U THEN
            RETURN 11.6 .
        IF pcVersionString BEGINS "11.7":U THEN
            RETURN 11.7 .

        RETURN ? .

    END METHOD.

END CLASS.
