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
    File        : SessionInfo
    Purpose     : Holder class for session information
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Oct 10 19:10:14 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SessionInfo 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF    
    {&SERIALIZABLE}: 

    {Consultingwerk/JsonSerializableProperty.i Properties Consultingwerk.ListNameValuePair}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BufferFieldName class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SessionInfo ():
        SUPER ().
        
        THIS-OBJECT:Properties = NEW ListNameValuePair () . 
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Agent startup parameters
        Notes:   Replaces -sectok and -P parameters with ********   
        @return The Agent startup parameters 
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED CHARACTER GetStartupParameters ():
		
		DEFINE VARIABLE cStartupParameters AS CHARACTER NO-UNDO .
		DEFINE VARIABLE cEntry             AS CHARACTER NO-UNDO CASE-SENSITIVE .
		DEFINE VARIABLE i                  AS INTEGER   NO-UNDO .

        ASSIGN cStartupParameters = SESSION:STARTUP-PARAMETERS .

        DO i = 1 TO NUM-ENTRIES (cStartupParameters):

            ASSIGN cEntry = ENTRY (i, cStartupParameters) .

            IF cEntry BEGINS "-sectok ":U OR cEntry BEGINS "-P ":U THEN DO:
                ENTRY (2, cEntry, " ":U) = FILL ("*":U, 8) . 

                ASSIGN ENTRY (i, cStartupParameters) = cEntry .
            END.
        END.

        RETURN cStartupParameters . 

	END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Initializes the SessionInfo instance with the current session values
	    Notes:
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Initialize ():
				
        THIS-OBJECT:PopulateAvm () .
        THIS-OBJECT:PopulateDatabases() .
		THIS-OBJECT:PopulateSession () .
		    
	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Populates AVM properties
	    Notes:
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PopulateAvm ():
		
        THIS-OBJECT:Properties:Add ("AVM:PROPATH":U,    PROPATH) .
        THIS-OBJECT:Properties:Add ("AVM:PROVERSION":U, PROVERSION) .
        THIS-OBJECT:Properties:Add ("AVM:STARTUP-PARAMETERS":U, GetStartupParameters()) .

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose: Populates the Database connection information
	 Notes:
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PopulateDatabases ():
		
		DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO NUM-DBS:
        
            THIS-OBJECT:Properties:Add (SUBSTITUTE ("DB:&1":U, LDBNAME (i)), 
                                        DBPARAM (i)) .
        END.

	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Populates Session Properties
	    Notes:
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PopulateSession ():
		
        DEFINE VARIABLE cProperties AS CHARACTER         NO-UNDO.
        DEFINE VARIABLE cProperty   AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE iEntry      AS INTEGER           NO-UNDO .
        DEFINE VARIABLE cValue      AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE hCall       AS HANDLE            NO-UNDO .

        ASSIGN cProperties = LIST-QUERY-ATTRS (SESSION:HANDLE) .

        loop:
        DO iEntry = 1 TO NUM-ENTRIES (cProperties) ON ERROR UNDO, THROW:

            ASSIGN cProperty = ENTRY (iEntry, cProperties) .

            IF NOT CAN-QUERY (SESSION:HANDLE, cProperty) THEN 
                NEXT . 

            DO ON ERROR UNDO, THROW:

                CREATE CALL hCall.
                
                ASSIGN hCall:IN-HANDLE = SESSION:HANDLE 
                       hCall:CALL-TYPE = GET-ATTR-CALL-TYPE  
                       hCall:CALL-NAME = cProperty . 
            
                hCall:INVOKE () NO-ERROR . 

                IF ERROR-STATUS:NUM-MESSAGES > 0 THEN 
                    NEXT loop . 

                ASSIGN cValue = hCall:RETURN-VALUE .
                
                CATCH err AS Progress.Lang.Error:
                    NEXT loop .     
                END CATCH.
            END. 

            THIS-OBJECT:Properties:Add (SUBSTITUTE ("SESSION:&1":U, cProperty), cValue) .

            FINALLY:
                Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (hCall) .       
            END FINALLY.
        END.

	END METHOD.

END CLASS.
