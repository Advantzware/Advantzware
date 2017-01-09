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
    File        : NumberFormatterEnglish
    Purpose     : Supports formatting numbers as human readable English strings  
    Syntax      : 
    Description : Based on http://rosettacode.org/wiki/Number_names#C.23
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 18 21:13:51 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH .  
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.NumberFormatterEnglish
    IMPLEMENTS INumberFormatter: 

    DEFINE VARIABLE incrementsOfOne AS CHARACTER EXTENT 20 NO-UNDO 
        INIT ["zero":U,    "one":U,     "two":U,       "three":U,    "four":U,
              "five":U,    "six":U,     "seven":U,     "eight":U,    "nine":U,
              "ten":U,     "eleven":U,  "twelve":U,    "thirteen":U, "fourteen":U,
              "fifteen":U, "sixteen":U, "seventeen":U, "eighteen":U, "nineteen":U] .

    DEFINE VARIABLE incrementsOfTen AS CHARACTER EXTENT 10 NO-UNDO
        INIT ["":U,      "":U,      "twenty":U,  "thirty":U, "fourty":U,
              "fifty":U, "sixty":U, "seventy":U, "eighty":U, "ninety":U].
    
    DEFINE VARIABLE millionName  AS CHARACTER NO-UNDO INIT "million":U .
    DEFINE VARIABLE thousandName AS CHARACTER NO-UNDO INIT "thousand":U .
    DEFINE VARIABLE hundredName  AS CHARACTER NO-UNDO INIT "hundred":U .
    DEFINE VARIABLE andName      AS CHARACTER NO-UNDO INIT "and":U .
 
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

        DEFINE VARIABLE cNumber AS CHARACTER INIT "":U .

        IF piNumber >= 1000000 THEN DO:
            ASSIGN cNumber = cNumber + THIS-OBJECT:ParseTriplet (RoundNumber (piNumber / 1000000)) + " ":U + millionName .
            piNumber = piNumber MODULO 1000000 .
            IF (piNumber = 0) THEN RETURN cNumber .
        END .

        IF piNumber >= 1000 THEN DO:
            IF cNumber > "":U THEN 
                ASSIGN cNumber = cNumber + ", ":U .
                
            ASSIGN cNumber = cNumber + THIS-OBJECT:ParseTriplet (RoundNumber (piNumber / 1000)) + " ":U + thousandName .
            piNumber = piNumber MODULO 1000 .
            IF (piNumber = 0) THEN RETURN cNumber .
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
        
        DEFINE VARIABLE cNumber AS CHARACTER NO-UNDO . 
    
        IF piNumber >= 100 THEN DO:
            ASSIGN cNumber = cNumber + incrementsOfOne[RoundNumber (piNumber / 100) + 1] + " ":U + hundredName .
            
            piNumber = piNumber MODULO 100 .
            
            IF piNumber = 0 THEN 
                RETURN cNumber . 
        END. 
            
        IF cNumber > "":U THEN 
            ASSIGN cNumber = cNumber + " ":U + andName + " ":U.
        
        IF piNumber >= 20 THEN DO:
            ASSIGN cNumber = cNumber + incrementsOfTen[RoundNumber (piNumber / 10) + 1] .
            
            piNumber = piNumber MODULO 10 .

            IF piNumber = 0 THEN 
                RETURN cNumber .
        END.
             
        IF cNumber > "":U THEN 
            cNumber = cNumber + " ":U . 

        cNumber = cNumber + incrementsOfOne[INTEGER (ROUND (piNumber, 0)) + 1] .
        
        RETURN cNumber . 
        
    END METHOD . 

END CLASS.
