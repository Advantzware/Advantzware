/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ReleaseNotesParser
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd
    Created     : Sat Oct 03 18:21:23 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                     FROM PROPATH .
USING Consultingwerk.Studio.ReleaseNotes.* FROM PROPATH .  
USING Consultingwerk.Util.*                FROM PROPATH .
USING Progress.Lang.*                      FROM PROPATH .

CLASS Consultingwerk.Studio.ReleaseNotes.ReleaseNotesParser 
    IMPLEMENTS ISaxReader: 

    {Consultingwerk/Studio/ReleaseNotes/ttFeed.i}

    DEFINE VARIABLE hSaxHandler          AS HANDLE        NO-UNDO .
    DEFINE VARIABLE lcCharacterData      AS LONGCHAR      NO-UNDO . 
    DEFINE VARIABLE lExpectingCharacter  AS LOGICAL       NO-UNDO INIT FALSE .
    DEFINE VARIABLE lInEntries           AS LOGICAL       NO-UNDO INIT FALSE .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ReleaseNotesParser class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ReleaseNotesParser ():
		SUPER ().
		
        RUN Consultingwerk/sax-reader-handler.p 
            PERSISTENT SET hSaxHandler (THIS-OBJECT) .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Parses the given Atom feed
        Notes:   
        @param pcSourceFile The source file to parse
        @param ttFeed OUTPUT TABLE The temp-table representing the feed
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ParseAtomFeed (pcSourceFile AS CHARACTER,
	                                  OUTPUT TABLE ttFeed):
		
        DEFINE VARIABLE hSaxReader AS HANDLE NO-UNDO.

        EMPTY TEMP-TABLE ttFeed . 

        Consultingwerk.Assertion.FileAssert:Exists (pcSourceFile) .
        
        CREATE SAX-READER hSaxReader . 
        hSaxReader:HANDLER = hSaxHandler . 

        hSaxReader:SET-INPUT-SOURCE ("FILE":U, pcSourceFile).

        hSaxReader:SAX-PARSE () .                

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hSaxReader) .
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
	METHOD PUBLIC VOID SaxCharacters (plcCharData AS LONGCHAR, piNumChars AS INTEGER):
		
        ASSIGN lcCharacterData = lcCharacterData + plcCharData .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invoked when the XML parser detects the end of an XML document.                                                                     
        Notes:                                                                        
        @param pcNamespaceURI A character string indicating the namespace URI of the element. If namespace processing is not enabled or the element is not part of a namespace, the string is of length zero.
        @param pcLocalName A character string indicating the non-prefixed element name. If namespace processing is not enabled, the string is of length zero.
        @param pcName A character string indicating the actual name of the element in the XML source. If the name has a prefix, qName includes it, whether or not namespace processing is enabled.
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SaxEndElement (pcNamespaceURI AS CHARACTER, pcLocalName AS CHARACTER, pcName AS CHARACTER):
		
		IF NOT lInEntries THEN 
		    RETURN  .
		
		CASE pcName:
		    WHEN "title":U THEN 
		        ASSIGN ttFeed.EntryTitle = TRIM (STRING (lcCharacterData)) .
            WHEN "published":U THEN 
                ASSIGN ttFeed.Published = TRIM (STRING (lcCharacterData)) .
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

        DEFINE VARIABLE cLink     AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cCategory AS CHARACTER NO-UNDO .

		ASSIGN lcCharacterData = "":U .

        CASE pcName:
            WHEN "entry":U THEN DO: 
                CREATE ttFeed .
                ASSIGN lInEntries = TRUE . 
            END.
            WHEN "link":U THEN DO:
                IF NOT lInEntries THEN 
                    RETURN . 
                    
            ASSIGN cLink = phAttributes:GET-VALUE-BY-QNAME ("href":U) .

            IF cLink> "":U THEN 
                ttFeed.EntryLink = cLink .                     
            END.
            WHEN "category":U THEN DO:
                IF NOT lInEntries THEN 
                    RETURN . 

                ASSIGN cCategory = phAttributes:GET-VALUE-BY-QNAME ("term":U) 
                
                       ttFeed.Categories = ttFeed.Categories + 
                                           (IF ttFeed.Categories > "":U THEN ",":U ELSE "":U) + 
                                           TRIM (STRING (cCategory)) . 
            END.
            
        END CASE . 
         
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ReleaseNotesParser class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ReleaseNotesParser ():

        IF VALID-HANDLE (hSaxHandler) THEN 
            DELETE OBJECT hSaxHandler . 

    END DESTRUCTOR .

END CLASS.
