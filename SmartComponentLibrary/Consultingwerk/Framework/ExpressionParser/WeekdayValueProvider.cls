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
    File        : WeekdayValueProvider
    Purpose     : Logical Value Provider that compares the provided character 
                  Value against a list of weekday names (Monday - Sunday)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jun 21 10:53:31 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Assertion.*                  FROM PROPATH . 
USING Consultingwerk.Framework.ExpressionParser.* FROM PROPATH .  
USING Progress.Lang.*                             FROM PROPATH .

CLASS Consultingwerk.Framework.ExpressionParser.WeekdayValueProvider 
    IMPLEMENTS ILogicalValueProvider: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the list of the names of the days
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY DayNames AS CHARACTER INITIAL 'Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday'{&TRAN} NO-UNDO 
	GET.
	SET (arg AS CHARACTER):
	    ListAssert:NumEntries (arg, 7) .
	    
	    ASSIGN THIS-OBJECT:DayNames = arg . 
    END. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference date
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ReferenceDate AS DATE NO-UNDO INIT TODAY 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the WeekdayValueProvider class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC WeekdayValueProvider ():
        SUPER ().
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the WeekdayValueProvider class
        Notes:   
        @param pdtReferenceDate The reference date 
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC WeekdayValueProvider (pdtReferenceDate AS DATE):
		SUPER ().
		
		ASSIGN THIS-OBJECT:ReferenceDate = pdtReferenceDate . 
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the WeekdayValueProvider class
        Notes:   
        @param pdtReferenceDate The reference date 
        @param pcDayNames The comma delimited list of day names
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC WeekdayValueProvider (pdtReferenceDate AS DATE,
                                             pcDayNames AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:ReferenceDate = pdtReferenceDate 
               THIS-OBJECT:DayNames      = pcDayNames . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the WeekdayValueProvider class
        Notes:   
        @param pcDayNames The comma delimited list of day names
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC WeekdayValueProvider (pcDayNames AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:DayNames      = pcDayNames . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Parses a CHARACTER Expression and returns the logical result
        Notes:   Returns true when the RefenceDate is on the given weekday
        @param pcExpression The Character expression to parse
        @return The logical result of the expressioln
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL GetLogicalValue (pcExpression AS CHARACTER):
		
		DEFINE VARIABLE iDay AS INTEGER NO-UNDO.

        ListAssert:EntryIsInList (pcExpression, THIS-OBJECT:DayNames) .
		
        ASSIGN iDay = WEEKDAY (THIS-OBJECT:ReferenceDate) - 1 .
        
        IF iDay = 0 THEN iDay = 7 .
        
        IF LOOKUP (pcExpression, THIS-OBJECT:DayNames) = iDay THEN 
            RETURN TRUE . 
        ELSE 
            RETURN FALSE . 
        
	END METHOD .

END CLASS.
