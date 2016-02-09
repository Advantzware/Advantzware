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
    File        : ResxFileParser
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jan 01 18:47:16 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .  
USING Consultingwerk.Util.Support.*          FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.Util.Support.ResxFileParser IMPLEMENTS ISaxReader: 

    DEFINE VARIABLE hSaxHandler          AS HANDLE        NO-UNDO .
    DEFINE VARIABLE lcCharacterData      AS LONGCHAR      NO-UNDO . 
    DEFINE VARIABLE lExpectingCharacter  AS LOGICAL       NO-UNDO INIT FALSE .
    DEFINE VARIABLE oList                AS CharacterList NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ResxFileParser class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ResxFileParser ():
		SUPER ().
		
        RUN Consultingwerk/sax-reader-handler.p 
            PERSISTENT SET hSaxHandler (THIS-OBJECT) .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Parses a resx file and returns a CharacterList of Data Elements 
        Notes:   
        @param pcFileName The .resx file to parse
        @return The CharacterList of Data Elements in the resx file
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CharacterList ParseResxFile (pcFileName AS CHARACTER):
		
        DEFINE VARIABLE hSaxReader AS HANDLE NO-UNDO.

        Consultingwerk.Assertion.FileAssert:Exists (pcFileName) .
        
        CREATE SAX-READER hSaxReader . 
        hSaxReader:HANDLER = hSaxHandler . 

        hSaxReader:SET-INPUT-SOURCE ("FILE":U, pcFileName).

        oList = NEW CharacterList () .
        
        hSaxReader:SAX-PARSE () .                

        RETURN oList .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hSaxReader) .
            
            oList = ?  .
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
                                      piNumChars  AS INTEGER):
        
        /* noop */
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invoked when the XML parser detects the end of an XML document.                                                                     
        Notes:                                                                        
        @param pcNamespaceURI A character string indicating the namespace URI of the element. If namespace processing is not enabled or the element is not part of a namespace, the string is of length zero.
        @param pcLocalName A character string indicating the non-prefixed element name. If namespace processing is not enabled, the string is of length zero.
        @param pcName A character string indicating the actual name of the element in the XML source. If the name has a prefix, qName includes it, whether or not namespace processing is enabled.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaxEndElement (pcNamespaceURI AS CHARACTER,
                                      pcLocalName    AS CHARACTER, 
                                      pcName         AS CHARACTER):
		
        /* noop */

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
                                        pcLocalName    AS CHARACTER,
                                        pcName         AS CHARACTER,
                                        phAttributes   AS HANDLE):
        
        DEFINE VARIABLE cName AS CHARACTER NO-UNDO.

        IF pcName = "data":U THEN DO:
    
            ASSIGN cName = phAttributes:GET-VALUE-BY-QNAME ("name":U) .

            IF cName > "":U THEN 
                oList:Add (cName) .
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ResxFileParser class                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ResxFileParser ():

        IF VALID-HANDLE (hSaxHandler) THEN 
            DELETE OBJECT hSaxHandler . 

    END DESTRUCTOR.

END CLASS.
