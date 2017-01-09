/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *    Bronco Oostermeyer / flusso b.v., parts of this class are       * 
 *    adopted from https://bitbucket.org/bfv/pug2014                  *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : RestServletRequestParser
    Purpose     : Parses a CHARACTER/LONGCHAR String with the servlet request 
                  info into a RestServletRequest< instance.
    instance.
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 18 17:02:20 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.RestAdapter.*      FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .
USING Progress.Json.ObjectModel.*            FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH.
USING Consultingwerk.Util.*                  FROM PROPATH.

CLASS Consultingwerk.OERA.RestAdapter.RestServletRequestParser: 

    /*------------------------------------------------------------------------------
        Purpose: Parses the QueryString of the REST servlet request into a CharacterDictionary 
        Notes:   Currently only processes & and = Does not (yet) convert url encoded 
                 characters
        @param poRequest The RestServletRequest to parse the QueryString from
        @return The CharacterDictionary with the query name/value pairs
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CharacterDictionary ParseQueryString (poRequest AS RestServletRequest):
		
		RETURN THIS-OBJECT:ParseQueryString (poRequest:QueryString).

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the given QueryString into a CharacterDictionary 
        Notes:   Currently only processes & and = Does not (yet) convert url encoded 
                 characters    
        @param pcQueryString The query string to parse
        @return The CharacterDictionary with the query name/value pairs
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CharacterDictionary ParseQueryString (pcQueryString AS CHARACTER):
        
        RETURN ListHelper:AlternatingListToDictionary (pcQueryString, "&":U, "=":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses a CHARACTER/LONGCHAR String with the servlet request 
                 info into a RestServletRequest instance. 
        Notes:   
        @param pcRequest The servlet request info as received as a mapped input parameter from the REST Adapter
        @return The RestServletRequest with the data for the request
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC RestServletRequest ParseServletRequest (pcRequest AS LONGCHAR):
		
        DEFINE VARIABLE oRootArray AS JsonArray          NO-UNDO.
        DEFINE VARIABLE oParser    AS ObjectModelParser  NO-UNDO.
        DEFINE VARIABLE i          AS INTEGER            NO-UNDO.
        DEFINE VARIABLE oRequest   AS RestServletRequest NO-UNDO.
        DEFINE VARIABLE oNode      AS JsonObject         NO-UNDO.
        DEFINE VARIABLE cValue     AS CHARACTER          NO-UNDO.
        DEFINE VARIABLE cName      AS CHARACTER          NO-UNDO.
        
        oParser = NEW ObjectModelParser().
        oRootArray = CAST(oParser:Parse (pcRequest), JsonArray).
        
        oRequest = NEW RestServletRequest ().
        
        DO i = 1 TO oRootArray:Length ON ERROR UNDO, THROW:
          
          oNode = CAST (oRootArray:GetJsonObject(i), JsonObject).
          
          cName = oNode:GetCharacter("theName":U).
          
          /* Mike Fechner, Consultingwerk Ltd. 18.01.2015
             In the original code the name "method" was mapped to "HttpMethod".
             As the ABL does not require this mapping, we are leaving the method
             name unchanged in our implementation. */
          
/*          IF (theName = "method") THEN*/
/*            theName = "HttpMethod".   */
          
          cValue = oNode:GetCharacter ("theValue":U) NO-ERROR.
          
          DYNAMIC-PROPERTY (oRequest, cName) = cValue.
        END.
        
        RETURN oRequest .
        
	END METHOD .

END CLASS.
