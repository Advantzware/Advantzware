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
    File        : NotEmptyInputValidator
    Purpose     : Performs input validation for CHARACTER, DATE, DECIMAL,
                  INTEGER and LOGICAL input prompt values for not empty
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Feb 18 00:10:20 CET 2012
    Notes       : Not empty is interpreted as not ? and not the data type
                  default valud (exception is LOGICAL)
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.* FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.Framework.NotEmptyInputValidator 
    IMPLEMENTS ICharacterInputValidator, 
               IDateInputValidator, 
               IDecimalInputValidator, 
               IIntegerInputValidator, 
               ILogicalInputValidator: 

    /*------------------------------------------------------------------------------
        Purpose: Validates a CHARACTER value                                                                      
        Notes:   Returns FALSE when the value is the empty string or ?                                                       
        @param pcValue The CHARACTER value to validate
        @return TRUE when the validation is passed, otherwise FALSE           
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ValidateCharacterValue (pcValue AS CHARACTER):
        
        IF pcValue > "":U THEN 
            RETURN TRUE . 
            
        RETURN FALSE .      
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Validates a DATE value                                                                       
        Notes:   Returns FALSE when the value is ? .                                                        
        @param pdtValue The DATE value to validate
        @return TRUE when the validation is passed, otherwise FALSE           
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ValidateDateValue (pdtValue AS DATE):
        
        IF pdtValue = ? THEN 
            RETURN FALSE . 
            
        RETURN TRUE . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Validates a DECIMAL value                                                                    
        Notes:   Returns TRUE when the value is not zero and not ?                                                       
        @param pdeValue The DECIMAL value to validate
        @return TRUE when the validation is passed, otherwise FALSE           
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ValidateDecimalValue (pdeValue AS DECIMAL):
        
        IF pdeValue <> ? AND pdeValue <> 0 THEN 
            RETURN TRUE . 
            
        RETURN FALSE . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Validates an INTEGER value                                                                       
        Notes:   Returns TRUE when the value is not zero and not ?                                                        
        @param piValue The INTEGER value to validate
        @return TRUE when the validation is passed, otherwise FALSE           
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ValidateIntegerValue (piValue AS INTEGER):
        
        IF piValue <> ? AND piValue <> 0 THEN 
            RETURN TRUE . 
            
        RETURN FALSE . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Validates an LOGICAL value                                                                       
        Notes:   Returns TRUE whne the value is not ?                                                     
        @param plValue The LOGICAL value to validate
        @return TRUE when the validation is passed, otherwise FALSE           
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ValidateLogicalValue (plValue AS LOGICAL ):
        
        IF plValue <> ? THEN 
            RETURN TRUE . 
           
        RETURN FALSE . 

    END METHOD.

END CLASS.
