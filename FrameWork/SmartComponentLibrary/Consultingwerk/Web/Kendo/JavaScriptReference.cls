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
    File        : JavaScriptReference
    Purpose     : Base class for types providing plain JavaScript source code
    Syntax      : 
    Description : Provides post-serialization 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 01 00:17:12 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*           FROM PROPATH . 
USING Consultingwerk.Web.Kendo.* FROM PROPATH . 
USING Consultingwerk.Util.*      FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH . 

CLASS Consultingwerk.Web.Kendo.JavaScriptReference 
    INHERITS JsonSerializable: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the JavaScriptReference class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC JavaScriptReference ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Fixes a JavaScript reference in the JSON Document by inserting  
                 unquoted JavaScript source code 
        Notes:   The JSON object model quotes every string - which would not be 
                 accepted as JavaScript source code by the browser
        @param plcJson INPUT-OUTPUT The JSON string to correct
        @param pcPropertyName The name of the JSON property to fix
        @param pcPropertyValue The property value to insert
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FixJavaScriptReference (INPUT-OUTPUT plcJson AS LONGCHAR,
                                               pcPropertyName AS CHARACTER,
                                               pcPropertyValue AS CHARACTER):
        
        DEFINE VARIABLE iIndex  AS INTEGER NO-UNDO.
        DEFINE VARIABLE iIndex2 AS INTEGER NO-UNDO.

        iIndex = INDEX (plcJson, SUBSTITUTE ("~"&1~":", pcPropertyName)) .
        
        IF iIndex > 0 THEN DO:
            iIndex = INDEX (plcJson, "~{":U, iIndex) .
            
            iIndex2 = StringHelper:FindClosingBracket (plcJson, iIndex) .
            
            IF iIndex2 > iIndex THEN  
                SUBSTRING (plcJson, iIndex, iIndex2 - iIndex + 1) = pcPropertyValue .
        END.

    END METHOD.

END CLASS.
