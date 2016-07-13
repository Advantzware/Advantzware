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
    File        : Debug
    Purpose     : Writes messages to the debug log file
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 12 17:18:42 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.* FROM PROPATH .  
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.Debug: 

    DEFINE STREAM logoutput. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Debug class
        Notes:   Disallow instance creation   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PRIVATE Debug ():
		SUPER ().
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Writes a message to the debug log file
        Notes:   Requires the DEBUG Compile time option
        @param pcMessageGroup The message group to prefix the message with
        @param pcMessage The message to write to the debug log file
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Log (pcMessageGroup AS CHARACTER,
                                   pcMessage AS CHARACTER):

        &IF DEFINED (DEBUG) EQ 0 &THEN
        RETURN . 
        &ELSE
        Debug:Log (SUBSTITUTE ("[&1] &2":U,
                   pcMessageGroup,
                   pcMessage)) .
        &ENDIF         
    END METHOD . 
        
    /*------------------------------------------------------------------------------
        Purpose: Writes a message to the debug log file
        Notes:   Requires the DEBUG Compile time option
        @param pcMessage The message to write to the debug log file
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID Log (pcMessage AS CHARACTER):
		
		&IF DEFINED (DEBUG) EQ 0 &THEN
		RETURN . 
		&ELSE
        OUTPUT STREAM logoutput TO VALUE ({&DebugLogFile}) UNBUFFERED APPEND .		
		
		PUT STREAM logoutput UNFORMATTED SUBSTITUTE ("[&1] &2":U, 
            		                                 STRING (NOW),
            		                                 pcMessage) SKIP .
		
		FINALLY:
            OUTPUT STREAM logoutput CLOSE  .		
        END FINALLY.
		&ENDIF

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Writes empty lines to the debug log file  
        Notes:   
        @param piLines The number of lines to Skip
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID Skip (piLines AS INTEGER):
		
		DEFINE VARIABLE i AS INTEGER NO-UNDO.
		
        &IF DEFINED (DEBUG) EQ 0 &THEN
        RETURN . 
        &ELSE
        IF piLines > 0 THEN . 
        ELSE piLines = 1 . 
        
        OUTPUT STREAM logoutput TO VALUE ({&DebugLogFile}) UNBUFFERED APPEND .      
        
        DO i = 1 TO piLines:
        
            PUT STREAM logoutput UNFORMATTED " ":U SKIP (1) .
            
        END.
        
        FINALLY:
            OUTPUT STREAM logoutput CLOSE  .        
        END FINALLY.
        &ENDIF

	END METHOD .

END CLASS.
