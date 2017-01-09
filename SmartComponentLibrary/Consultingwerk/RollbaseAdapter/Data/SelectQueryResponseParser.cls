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
    File        : SelectQueryResponseParser
    Purpose     : Parses the SelectQuery result form Rollbase
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 04 21:40:04 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.RollbaseAdapter.Data.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .
USING Progress.Json.ObjectModel.*           FROM PROPATH. 

CLASS Consultingwerk.RollbaseAdapter.Data.SelectQueryResponseParser: 

    /*------------------------------------------------------------------------------
        Purpose: Parses the Json Reponse of a SelectQuery 
        Notes:   Creates records in the passed in buffer
        @param plcResponse The response to the REST SelectQuery method
        @param phBuffer The buffer handle to create records into
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ParseJsonResponse (plcResponse AS LONGCHAR,
	                                      phBuffer AS HANDLE):
        DEFINE VARIABLE oArray   AS Progress.Json.ObjectModel.JsonArray             NO-UNDO . 
        DEFINE VARIABLE oParser  AS Progress.Json.ObjectModel.ObjectModelParser     NO-UNDO . 
        DEFINE VARIABLE oAdapter AS Consultingwerk.RollbaseAdapter.IRollbaseAdapter NO-UNDO .
        DEFINE VARIABLE oRow     AS Progress.Json.ObjectModel.JsonArray             NO-UNDO . 
        DEFINE VARIABLE iRow     AS INTEGER                                         NO-UNDO .
        DEFINE VARIABLE hColumn  AS HANDLE                                          NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER                                         NO-UNDO .
        DEFINE VARIABLE iIdx     AS INTEGER                                         NO-UNDO .

        ASSIGN oParser  = NEW Progress.Json.ObjectModel.ObjectModelParser ()  
               oAdapter = {Consultingwerk/get-service.i Consultingwerk.RollbaseAdapter.IRollbaseAdapter}
               oArray   = CAST (oParser:Parse (plcResponse), JsonArray) .
        
        DO iRow = 1 TO oArray:Length:
        
        oRow = oArray:GetJsonArray(iRow) . 
        
            phBuffer:BUFFER-CREATE () . 
    
            iIdx = 0.
            readColumns:
            DO i = 1 TO phBuffer:NUM-FIELDS ON ERROR UNDO, THROW:
    
                ASSIGN hColumn = phBuffer:BUFFER-FIELD (i) .

                /* Mike Liewehr, AKIOMA KG / 01.05.2014
                   Allow for providing field subsets instead of only all fields
                   corresponds to same check in RollbaseAdapter:SelectToTempTable */
                   
                IF hColumn:XML-NODE-TYPE = "HIDDEN":U THEN 
                    NEXT readColumns.
                
                ASSIGN iIdx = iIdx + 1.
                
                CASE hColumn:DATA-TYPE:
                    WHEN "CHARACTER":U THEN 
                        IF SESSION:CHARSET = "utf-8":U THEN 
                            hColumn:BUFFER-VALUE = oRow:GetCharacter (iIdx) .
                        ELSE 
                            hColumn:BUFFER-VALUE = CODEPAGE-CONVERT (oRow:GetCharacter(iIdx), SESSION:CHARSET, "utf-8":U) .
                    WHEN "INT64":U THEN 
                        hColumn:BUFFER-VALUE = oRow:GetInteger (iIdx) .
                    WHEN "INTEGER":U THEN 
                        hColumn:BUFFER-VALUE = oRow:GetInteger (iIdx) .
                    WHEN "DATE":U THEN 
                        hColumn:BUFFER-VALUE = oAdapter:DateValue(oRow:GetCharacter (iIdx)) .                    
                    WHEN "DATETIME":U THEN 
                        hColumn:BUFFER-VALUE = oAdapter:DateTimeValue(oRow:GetCharacter (iIdx)) .
                    WHEN "DATETIME-TZ":U THEN 
                        hColumn:BUFFER-VALUE = oAdapter:DateTimeValue(oRow:GetCharacter (iIdx)) .
                    WHEN "DECIMAL":U THEN 
                        hColumn:BUFFER-VALUE = oRow:GetDecimal (iIdx) .                    
                    WHEN "LOGICAL":U THEN 
                        hColumn:BUFFER-VALUE = oRow:GetLogical (iIdx) .                    
                    WHEN "LONGCHAR":U THEN 
                        hColumn:BUFFER-VALUE = oRow:GetLongchar (iIdx) .                    
                END CASE .
            END.
    
            phBuffer:BUFFER-RELEASE () . 
        END.

	END METHOD .

END CLASS.
