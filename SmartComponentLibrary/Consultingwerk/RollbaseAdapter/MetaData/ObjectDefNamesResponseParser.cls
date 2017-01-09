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
    File        : ObjectDefNamesResponseParser
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Sun Nov 03 14:29:37 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.RollbaseAdapter.*          FROM PROPATH . 
USING Consultingwerk.RollbaseAdapter.MetaData.* FROM PROPATH .
USING Consultingwerk.Util.*                     FROM PROPATH .   
USING Progress.Lang.*                           FROM PROPATH .
USING Progress.Json.ObjectModel.*               FROM PROPATH . 

CLASS Consultingwerk.RollbaseAdapter.MetaData.ObjectDefNamesResponseParser: 

    /*------------------------------------------------------------------------------
        Purpose: Parses the response of the ObjectDefNames request
        Notes:   
        @param plcResponse The longchar with the response data from the ObjectDefNames request
        @return The Character array with the Object Definition Names
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER EXTENT ParseResponse (plcResponse AS LONGCHAR):
        
        DEFINE VARIABLE cNames       AS CHARACTER         NO-UNDO EXTENT.
        DEFINE VARIABLE oObjectModel AS ObjectModelParser NO-UNDO . 
        DEFINE VARIABLE oArray       AS JsonArray         NO-UNDO . 
        DEFINE VARIABLE i            AS INTEGER           NO-UNDO .
    
        plcResponse = StringHelper:Unquote (plcResponse) .

        oObjectModel = NEW ObjectModelParser () .
        
        oArray = CAST (oObjectModel:Parse (plcResponse), JsonArray).
        
        EXTENT (cNames) = oArray:Length  . 
        
        DO i = 1 TO EXTENT (cNames):
            
            cNames[i] = oArray:GetCharacter (i) .
        END.
        
        RETURN cNames .

    END METHOD .

END CLASS.
