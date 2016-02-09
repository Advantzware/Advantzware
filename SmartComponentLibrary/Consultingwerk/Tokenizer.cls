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
    File        : Tokenizer
    Purpose     : A class that supportes splitting a CHARACTER string into 
                  tokens that might be quoted or not
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Mar 16 14:40:25 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Collections.* FROM PROPATH . 
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.Tokenizer: 

    /*------------------------------------------------------------------------------
        Purpose: Splits a string into individual tokens                                                                      
        Notes:   When plKeepQuotes is passed as TRUE, then leading ( and trailing ) on 
                 tokens will be added to the CharacterList as seperate Tokens as it's expected
                 that this will simplify the interpretation of a Query String                                           
        @param pcString The string to split into tokens
        @param plKeepQuotes True to leave the quotes in the tokens, false to ommit quotes
        @param pcOtherDelimiters A single character string containing other characters that are used as additional token delimiters, e.g. ",="
        @return A CharacterList of individual tokens                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CharacterList Tokenize (pcString AS CHARACTER, 
                                          plKeepQuotes AS LOGICAL,
                                          pcOtherDelimiters AS CHARACTER):

        DEFINE VARIABLE oTokens    AS CharacterList NO-UNDO . 
        DEFINE VARIABLE cQuoteChar AS CHARACTER     NO-UNDO INIT "":U.
        DEFINE VARIABLE iPos       AS INTEGER       NO-UNDO.
        DEFINE VARIABLE cChar      AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE cToken     AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE lInQuote   AS LOGICAL       NO-UNDO INIT FALSE .
        
        oTokens = NEW CharacterList ("":U, CHR (1)) .
        
        pcString = TRIM (pcString) .
        
        IF pcString > "":U THEN . 
        ELSE RETURN oTokens .        
        
        DO iPos = 1 TO LENGTH (pcString):
            ASSIGN cChar = SUBSTRING (pcString, iPos, 1) .  

            IF cChar = "~"":U OR 
               cChar = "'":U THEN DO:
                IF cQuoteChar = "":U THEN DO:
                    ASSIGN cQuoteChar = cChar
                           lInQuote   = TRUE .
                           
                    IF plKeepQuotes THEN 
                        cToken = cToken + cChar . 
                END.
                ELSE IF cQuoteChar = cChar THEN DO:
                    ASSIGN lInQuote   = FALSE
                           cQuoteChar = "":U .

                    IF plKeepQuotes THEN 
                        cToken = cToken + cChar . 
                        
                    IF plKeepQuotes AND LENGTH (cToken) > 1 AND cToken BEGINS "(":U THEN DO: 
                        oTokens:Add ("(":U) .
                        oTokens:Add (SUBSTRING (cToken, 2)) .
                        cToken = "":U .
                    END . 
                    ELSE IF plKeepQuotes AND LENGTH (cToken) > 1 AND cToken MATCHES "*)":U THEN DO: 
                        oTokens:Add (SUBSTRING (cToken, 1, LENGTH (cToken) - 1)) .
                        oTokens:Add (")":U) .
                        cToken = "":U .
                    END.    
                    ELSE IF LENGTH (cToken) > 0 THEN DO:
                        oTokens:Add (cToken) .
                        cToken = "":U . 
                    END.
                END.
                ELSE 
                    ASSIGN cToken = cToken + cChar . 
            END. /* " or ' */
            ELSE IF cChar = " ":U 
                 OR cChar = CHR (9) 
                 OR cChar = CHR (10) 
                 OR cChar = CHR (13) 
                 OR (pcOtherDelimiters > "":U AND 
                     INDEX (pcOtherDelimiters, cChar) > 0) THEN DO:
                IF lInQuote = FALSE THEN DO:
                    IF plKeepQuotes AND LENGTH (cToken) > 1 AND cToken BEGINS "(":U THEN DO: 
                        oTokens:Add ("(":U) .
                        oTokens:Add (SUBSTRING (cToken, 2)) .
                        cToken = "":U .
                    END . 
                    ELSE IF plKeepQuotes AND LENGTH (cToken) > 1 AND cToken MATCHES "*)":U THEN DO: 
                        oTokens:Add (SUBSTRING (cToken, 1, LENGTH (cToken) - 1)) .
                        oTokens:Add (")":U) .
                        cToken = "":U .
                    END.
                    ELSE IF LENGTH (cToken) > 0 THEN DO:
                        oTokens:Add (cToken) . 
                        cToken = "":U .            
                    END.                             
                END.
                ELSE 
                    ASSIGN cToken = cToken + cChar .
            END.
            ELSE 
                ASSIGN cToken = cToken + cChar . 
        END.

        IF plKeepQuotes AND LENGTH (cToken) > 1 AND cToken BEGINS "(":U THEN DO: 
            oTokens:Add ("(":U) .
            oTokens:Add (SUBSTRING (cToken, 2)) .
        END . 
        ELSE IF plKeepQuotes AND  LENGTH (cToken) > 1 AND cToken MATCHES "*)":U THEN DO: 
            oTokens:Add (SUBSTRING (cToken, 1, LENGTH (cToken) - 1)) .
            oTokens:Add (")":U) .
        END.    
        ELSE IF LENGTH (cToken) > 0 THEN 
            oTokens:Add (cToken) .
            
        RETURN oTokens . 
    
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Splits a string into individual tokens                                                                      
        Notes:   When plKeepQuotes is passed as TRUE, then leading ( and trailing ) on 
                 tokens will be added to the CharacterList as seperate Tokens as it's expected
                 that this will simplify the interpretation of a Query String                                       
        @param pcString The string to split into tokens
        @param plKeepQuotes True to leave the quotes in the tokens, false to ommit quotes
        @return A CharacterList of individual tokens                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CharacterList Tokenize (pcString AS CHARACTER, 
                                          plKeepQuotes AS LOGICAL):
        
        RETURN THIS-OBJECT:Tokenize (pcString, 
                                     plKeepQuotes, 
                                     ?) .

    END.

    /*------------------------------------------------------------------------------
        Purpose: Splits a string into individual tokens                                                                   
        Notes:                                          
        @param pcString The string to split into tokens
        @return A CharacterList of individual tokens                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CharacterList Tokenize (pcString AS CHARACTER):
                                              
        RETURN THIS-OBJECT:Tokenize (pcString, FALSE) .      

    END .

END CLASS.
