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
    File        : IntegerType
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jun 17 13:10:21 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH . 
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.IntegerType: 

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE IntegerType ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Converts a character value to an integer value without raising an
                 error when values do not fit into integer  
        Notes:   
        @param pcValue The Character value to convert
        @return The integer value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER ConvertFromString (pcValue AS CHARACTER):
        
        RETURN IntegerType:ConvertFromString (pcValue, ?).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Converts a character value to an integer value without raising an
                 error when values do not fit into integer  
        Notes:   
        @param pcValue The Character value to convert
        @param piDefault The default value to return when the character value does not return a valid integer  
        @return The integer value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER ConvertFromString (pcValue AS CHARACTER,
                                                    piDefault AS INTEGER):
        
        DEFINE VARIABLE iValue AS INTEGER NO-UNDO.

        ASSIGN iValue = INTEGER (pcValue) .
        
        IF iValue = ? AND piDefault <> ? THEN 
            RETURN piDefault .
        
        RETURN iValue .
        
        CATCH err AS Progress.Lang.Error :
            
            IF err:GetMessageNum (1) = 76 THEN 
                RETURN piDefault . 
            
            UNDO, THROW err .     
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Verifies if the passed in integer value is not unknown value
                 or 0
        Notes:   
        @param piInteger The integer value to compare
        @return True when the passed in integer value is not ? or 0 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsNotNullOrZero (piInteger AS INTEGER):
        
        RETURN piInteger > 0 OR piInteger < 0 .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Verifies if the passed in integer value is the unknown value
                 or 0
        Notes:   
        @param piInteger The integer value to compare
        @return True when the passed in character value is ? or 0 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsNullOrEmpty (piInteger AS INTEGER):
        
        RETURN piInteger = ? OR piInteger = 0 .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the numeric part from the begining of a character value 
        Notes:   
        @param pcString The source string
        @return The string that contains only the numbers from the beginning of the source string
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC CHARACTER NumericString (pcString AS CHARACTER):
		
        DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cChar   AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i       AS INTEGER   NO-UNDO .
		
		DO i = 1 TO LENGTH (pcString):

            ASSIGN cChar = SUBSTRING (pcString, i, 1) .

            IF ASC (cChar) >= 48 AND ASC (cChar) <= 57 THEN 
                ASSIGN cReturn = cReturn + cChar .
            ELSE    
                RETURN cReturn .
		END.

        RETURN cReturn . 

	END METHOD .

END CLASS.
