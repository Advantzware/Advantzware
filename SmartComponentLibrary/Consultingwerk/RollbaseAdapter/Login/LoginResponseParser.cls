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
    File        : LoginResponseParser
    Purpose     : Parses the Login result from Rollbase
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 03 11:40:26 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                               FROM PROPATH .
USING Consultingwerk.Framework.Collections.*         FROM PROPATH .
USING Consultingwerk.RollbaseAdapter.LoginResponse.* FROM PROPATH .  
USING Consultingwerk.Util.*                          FROM PROPATH .  
USING Consultingwerk.Util.Support.*                  FROM PROPATH .  
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.RollbaseAdapter.Login.LoginResponseParser 
    IMPLEMENTS ISaxReader: 

    DEFINE VARIABLE hSaxHandler          AS HANDLE        NO-UNDO .
    DEFINE VARIABLE lcCharacterData      AS LONGCHAR      NO-UNDO . 
    DEFINE VARIABLE lExpectingCharacter  AS LOGICAL       NO-UNDO INIT FALSE .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Error Message of the Login Response 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ErrorMessage AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the SessionId of the Login Response 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SessionId AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LoginResponseParser class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LoginResponseParser ():
        SUPER ().
        
        FIX-CODEPAGE (lcCharacterData) = "utf-8":U . 
        
        RUN Consultingwerk/sax-reader-handler.p 
            PERSISTENT SET hSaxHandler (THIS-OBJECT) .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Parses the Login Reponse 
        Notes:   
        @param plcResponse The response of the REST Login method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ParseResponse (plcResponse AS LONGCHAR):
        
        DEFINE VARIABLE hSaxReader AS HANDLE NO-UNDO.
        
        CREATE SAX-READER hSaxReader . 
        hSaxReader:HANDLER = hSaxHandler . 

        hSaxReader:SET-INPUT-SOURCE ("LONGCHAR":U, plcResponse).
        
        hSaxReader:SAX-PARSE () .                
        
        FINALLY:
            IF VALID-HANDLE (hSaxReader) THEN 
                DELETE OBJECT hSaxReader . 
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invoked when the XML parser detects character data.                                                                      
        Notes:   The parser calls this method to report each chunk of character data. 
                 It might report contiguous character data in one chunk, or split it 
                 into several chunks. If validation is enabled, whitespace is reported 
                 by the IgnorableWhitespace callback                                                                      
        @param plcCharData A LONGCHAR that contains a chunk of character data.
        @param piNumChars The number of characters contained in the MEMPTR.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaxCharacters (plcCharData AS LONGCHAR, 
                                      piNumChars AS INTEGER):
        
        IF lExpectingCharacter THEN 
            ASSIGN lcCharacterData = lcCharacterData + plcCharData .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invoked when the XML parser detects the end of an XML document.                                                                     
        Notes:                                                                        
        @param pcNamespaceURI A character string indicating the namespace URI of the element. If namespace processing is not enabled or the element is not part of a namespace, the string is of length zero.
        @param pcLocalName A character string indicating the non-prefixed element name. If namespace processing is not enabled, the string is of length zero.
        @param pcName A character string indicating the actual name of the element in the XML source. If the name has a prefix, qName includes it, whether or not namespace processing is enabled.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaxEndElement (pcNamespaceURI AS CHARACTER, 
                                      pcLocalName AS CHARACTER, 
                                      pcName AS CHARACTER):
        
        CASE pcName:
            WHEN "sessionId":U THEN DO:
                ASSIGN THIS-OBJECT:SessionId = lcCharacterData
                       lExpectingCharacter   = FALSE .                 
            END.
            WHEN "err":U THEN DO:
                ASSIGN THIS-OBJECT:ErrorMessage = lcCharacterData
                       lExpectingCharacter      = FALSE .              
            END . 
        END CASE . 

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Invoked when the XML parser detects the beginning of an element.                                                                     
        Notes:      
        @param pcNamespaceURI A character string indicating the namespace URI of the element. If namespace processing is not enabled or the element is not part of a namespace, the string is of length zero.
        @param pcLocalName A character string indicating the non-prefixed element name. If namespace processing is not enabled, the string is of length zero.
        @param pcName A character string indicating the actual name of the element in the XML source. If the name has a prefix, qName includes it, whether or not namespace processing is enabled.
        @param phAttributes A handle to a SAX-attributes object, which provides access to all attributes specified for the element. If the element has no attributes, attributes is still a valid handle, and the NUM-ITEMS attribute is zero.      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaxStartElement (pcNamespaceURI AS CHARACTER, 
                                        pcLocalName AS CHARACTER, 
                                        pcName AS CHARACTER, 
                                        phAttributes AS HANDLE):
        
        CASE pcName:
            WHEN "sessionId":U OR when "err":U THEN DO:
                ASSIGN lExpectingCharacter = TRUE 
                       lcCharacterData     = "":U . 
            END.
        END CASE . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the LoginResponseParser class                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC LoginResponseParser ():

        IF VALID-HANDLE (hSaxHandler) THEN 
            DELETE OBJECT hSaxHandler . 

    END DESTRUCTOR.

END CLASS.
