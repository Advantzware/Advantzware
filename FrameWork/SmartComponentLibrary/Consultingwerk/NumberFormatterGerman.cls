/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : NumberFormatterGerman
    Purpose     : Supports formatting numbersas human readable English strings
    Syntax      :
    Description : Based on http://rosettacode.org/wiki/Number_names#C.23
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 18 21:13:51 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.NumberFormatterGerman
    IMPLEMENTS INumberFormatter:

    DEFINE VARIABLE incrementsOfOne AS CHARACTER EXTENT 20 NO-UNDO
        INITIAL ["null":U,     "ein":U,      "zwei":U,     "drei":U,     "vier":U,
                 "fünf":U,     "sechs":U,    "sieben":U,   "acht":U,     "neun":U,
                 "zehn":U,     "elf":U,      "zwölf":U,    "dreizehn":U, "vierzehn":U,
                 "fünfzehn":U, "sechzehn":U, "siebzehn":U, "achtzehn":U, "neunzehn":U] .

    DEFINE VARIABLE incrementsOfTen AS CHARACTER EXTENT 10 NO-UNDO
        INITIAL ["":U,        "":U,        "zwangig":U, "dreissig":U, "vierzig":U,
                 "fünfzig":U, "sechzig":U, "siebzig":U, "achzig":U,   "neunzig":U].

    DEFINE VARIABLE millionName  AS CHARACTER NO-UNDO INITIAL "Millionen":U .
    DEFINE VARIABLE thousandName AS CHARACTER NO-UNDO INITIAL "Tausend":U .
    DEFINE VARIABLE hundredName  AS CHARACTER NO-UNDO INITIAL "Hundert":U .
    DEFINE VARIABLE andName      AS CHARACTER NO-UNDO INITIAL "und":U .

    /*------------------------------------------------------------------------------
        Purpose: Rounds a number down to the closest lower integer number
        Notes:
        @param piNumber The number to round
        @return The rounded number
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED INTEGER RoundNumber (piNumber AS DECIMAL):

        RETURN INTEGER (ROUND (piNumber - .5, 0)) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Formats a number as a string
        Notes:
        @param piNumber The number to format
        @return The string representing the number in a human readable form
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER FormatNumber (piNumber AS INTEGER):

        DEFINE VARIABLE cNumber AS CHARACTER INITIAL "":U NO-UNDO .

        IF piNumber >= 1000000 THEN DO:
            ASSIGN cNumber = cNumber + THIS-OBJECT:ParseTriplet (RoundNumber (piNumber / 1000000)) + " ":U + millionName .
            piNumber = piNumber MODULO 1000000 .
            IF (piNumber = 0) THEN
                RETURN cNumber .
        END .

        IF piNumber >= 1000 THEN DO:
            IF cNumber > "":U THEN
                ASSIGN cNumber = cNumber + ", ":U .

            ASSIGN cNumber = cNumber + THIS-OBJECT:ParseTriplet (RoundNumber (piNumber / 1000)) + " ":U + thousandName .
            piNumber = piNumber MODULO 1000 .
            IF (piNumber = 0) THEN
                RETURN cNumber .
        END.

        IF cNumber > "":U THEN
            ASSIGN cNumber = cNumber + ", ":U .

        ASSIGN cNumber = cNumber + THIS-OBJECT:ParseTriplet (piNumber) .

        RETURN cNumber .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Formats a number triplet as a string
        Notes:
        @param piNumber The number to format
        @return The string representing the number in a human readable form
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER ParseTriplet (piNumber AS DECIMAL):

        DEFINE VARIABLE cHunderter AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cZehner    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cEiner     AS CHARACTER NO-UNDO.

        DEFINE VARIABLE cNumber AS CHARACTER NO-UNDO .

        IF piNumber >= 100 THEN DO:
            ASSIGN cHunderter = incrementsOfOne[RoundNumber (piNumber / 100) + 1] .

            piNumber = piNumber MODULO 100 .
        END.

        IF piNumber >= 20 THEN DO:
            ASSIGN cZehner = incrementsOfTen[RoundNumber (piNumber / 10) + 1] .

            piNumber = piNumber MODULO 10 .
        END.

        IF RoundNumber (piNumber) = 1 AND cZehner = "":U THEN
            ASSIGN cEiner = "eins":U .
        ELSE
            ASSIGN cEiner = incrementsOfOne[RoundNumber (piNumber) + 1] .

        IF cHunderter > "":U THEN
            ASSIGN cNumber = cHunderter + hundredName .

        IF cEiner > "":U THEN
            ASSIGN cNumber = cNumber + cEiner .

        IF cZehner > "":U THEN DO:
            IF cNumber > "":U THEN
                ASSIGN cNumber = cNumber + andName .

            ASSIGN cNumber = cNumber + cZehner .
        END.

        RETURN StringHelper:CamelCaseWord(cNumber) .

    END METHOD .

END CLASS.
