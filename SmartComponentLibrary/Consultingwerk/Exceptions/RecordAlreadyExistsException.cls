/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : RecordAlreadyExistsException
    Purpose     : Exception thrown by DataAccess and SmartDatasetAdapter 
                  when an update record conflicts with another record. 
    Syntax      : 
    Description : Allows customization of ABL error 132
                  ** Salesrep already exists with Sales Rep "bbb". (132)
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Fri Nov 27 18:32:41 CET 2015
    Notes       : The static MessageTemplate property allows to set a custom
                  default message text (used by the FromSysError method).
                  &1 can be used for the table name, &2 can be used for the 
                  fields/values. The default message does not use the 
                  &1 place holder.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*  FROM PROPATH .
USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Consultingwerk.Util.*       FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Exceptions.RecordAlreadyExistsException 
    INHERITS Exception: 

    /*------------------------------------------------------------------------------
        Purpose: Customization of the error message for ABL error 132 raised
                 during SAVE-ROW-CHANGES
        Notes:   ** Salesrep already exists with Sales Rep "bbb". (132)
                 &1 for the Table name, &2 for the field values
                 This property allows to set the (localized) default text for the 
                 exception
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY MessageTemplate AS CHARACTER 
/*       INITIAL "&1 already exists with &2!":T*/
       INITIAL "A conflicting record already exists with &2!":T 
       NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the FieldValues Name of this Exception
        Notes:                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FieldValues AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Table Name of this Exception
        Notes:                          
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY TableName AS CHARACTER NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Exception class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RecordAlreadyExistsException (poInnerException AS Progress.Lang.Error, 
	                                                 pcErrorString AS CHARACTER):
		SUPER (poInnerException, pcErrorString).
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Exception class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RecordAlreadyExistsException (poInnerException AS Progress.Lang.Error, 
	                                                 pcErrorString AS CHARACTER, 
	                                                 piMessageNumber AS INTEGER):
		SUPER (poInnerException, pcErrorString, piMessageNumber).
		
	END CONSTRUCTOR.
		
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Exception class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
        @param pcTableName The value for the TableName property
        @param pcFieldValues The value for the FieldValues property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RecordAlreadyExistsException (poInnerException AS Progress.Lang.Error, 
                                                     pcErrorString AS CHARACTER, 
                                                     piMessageNumber AS INTEGER,
                                                     pcTableName AS CHARACTER, 
                                                     pcFieldValues AS CHARACTER):
                                                         
        SUPER (poInnerException, pcErrorString, piMessageNumber).
        
        ASSIGN THIS-OBJECT:TableName   = pcTableName
               THIS-OBJECT:FieldValues = pcFieldValues .  

    END CONSTRUCTOR.
		
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Exception class
        Notes:                          
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RecordAlreadyExistsException (poInnerException AS Progress.Lang.Error):
		SUPER (poInnerException).
		
	END CONSTRUCTOR.
		
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Exception class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RecordAlreadyExistsException (pcErrorString AS CHARACTER):
		SUPER (pcErrorString).
		
	END CONSTRUCTOR.
		
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Exception class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RecordAlreadyExistsException (pcErrorString AS CHARACTER, 
	                                                 piMessageNumber AS INTEGER):
		SUPER (pcErrorString, piMessageNumber).
		
	END CONSTRUCTOR.
		
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Exception class
        Notes:                          
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RecordAlreadyExistsException ():
		SUPER ().
		
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	    Purpose: Parses a SysError 132 and returns the RecordAlreadyExistsException 
	             with all detail 
	    Notes:
	    @param poError The SysError to parse
	    @return The RecordAlreadyExistsException 
	------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC RecordAlreadyExistsException FromSysError (poError AS SysError):
		
		DEFINE VARIABLE oException AS RecordAlreadyExistsException NO-UNDO .
        DEFINE VARIABLE oInfo      AS Error132Info                 NO-UNDO .

        Assert:Equals (132, poError:GetMessageNum (1)) .

        oInfo = ErrorHelper:ParseError132 (poError) .
        
        oException = NEW RecordAlreadyExistsException (poError, 
                                                       SUBSTITUTE (RecordAlreadyExistsException:MessageTemplate,
                                                                   oInfo:TableName,
                                                                   oInfo:FieldValues),
                                                       0,
                                                       oInfo:TableName,
                                                       oInfo:FieldValues) .

		RETURN oException .

	END METHOD.

END CLASS.
