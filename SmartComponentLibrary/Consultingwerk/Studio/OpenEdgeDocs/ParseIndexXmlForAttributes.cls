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
    File        : ParseIndexXmlForAttributes
    Purpose     : Parses the index.xml from the PDSOE docs for "attributes"
                  Builds a list of available widget attributes. The LIST-QUERY-ATTRS
                  function does return attributes and methods of a widget.
                  The ttWidgetAttributes created by this class can be used
                  to distinguish between attributes and methods 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 13 15:50:57 CEST 2015
    Notes       : Supports parsing c:\Progress\OpenEdge115_64\oeide\eclipse\plugins\com.openedge.pdt.langref.help_11.5.1.00\index.xml
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW . 

USING Consultingwerk.Studio.OpenEdgeDocs.* FROM PROPATH . 
USING Consultingwerk.Util.*                FROM PROPATH . 
USING Progress.Lang.*                      FROM PROPATH .

CLASS Consultingwerk.Studio.OpenEdgeDocs.ParseIndexXmlForAttributes
    IMPLEMENTS Consultingwerk.ISaxReader: 

    DEFINE TEMP-TABLE ttWidgetAttributes NO-UNDO 
        FIELD AttributeName AS CHARACTER
        INDEX AttributeName IS UNIQUE PRIMARY AttributeName . 
        
    DEFINE VARIABLE hSaxHandler AS HANDLE NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ParseIndexXmlForAttributes class
        Notes:   Starts parsing and writes output to ./widgetattributes.xml
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ParseIndexXmlForAttributes ():
        SUPER ().

        DEFINE VARIABLE hSaxReader AS HANDLE NO-UNDO.

        RUN Consultingwerk/sax-reader-handler.p 
            PERSISTENT SET hSaxHandler (THIS-OBJECT) .
                            
        CREATE SAX-READER hSaxReader . 
        hSaxReader:HANDLER = hSaxHandler . 

        hSaxReader:SET-INPUT-SOURCE ("FILE":U, "index.xml":U).
        
        hSaxReader:SAX-PARSE () .                
        
        TEMP-TABLE ttWidgetAttributes:WRITE-XML ("file":U, "widgetattributes.xml":U, TRUE, ?, ?) .
        
        FINALLY:
            IF VALID-HANDLE (hSaxReader) THEN 
                DELETE OBJECT hSaxReader . 
        END FINALLY.                    
                    
    END CONSTRUCTOR.

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

    END METHOD.

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

    END METHOD.

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
        
        DEFINE VARIABLE cKeyword AS CHARACTER NO-UNDO.
        
        IF pcName <> "entry":U THEN
            RETURN .
            
        ASSIGN cKeyword = phAttributes:GET-VALUE-BY-QNAME ("keyword":U) .
        
        IF NUM-ENTRIES (cKeyword, " ":U) = 2 AND 
            ENTRY (2, cKeyword, " ":U) = "attribute":U THEN DO:               
        
            IF NOT CAN-FIND (ttWidgetAttributes WHERE ttWidgetAttributes.AttributeName = ENTRY (1, cKeyword, " ":U)) THEN DO:
                CREATE ttWidgetAttributes.
                ASSIGN ttWidgetAttributes.AttributeName = ENTRY (1, cKeyword, " ":U) .
            END.
        END.
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ParseIndexXmlForAttributes ():
        
        GarbageCollectorHelper:DeleteObject (hSaxHandler) .
        
    END DESTRUCTOR.

END CLASS.
