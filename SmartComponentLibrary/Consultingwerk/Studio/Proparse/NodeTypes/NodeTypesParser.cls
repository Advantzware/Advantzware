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
    File        : NodeTypesParser
    Purpose     : Parser for the Java Source Code of the proparse com.joanju.proparse.NodeTypes
                  class
    Syntax      : 
    Description : Returns a temp-table of nodetypes with NodeType name, 
                  ABL keyword and min arrreviated chars 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 22 11:33:11 CEST 2015
    Notes       : https://github.com/consultingwerk/proparse/blob/master/src/com/joanju/proparse/NodeTypes.java
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                 FROM PROPATH .
USING Consultingwerk.Util.*                      FROM PROPATH .
USING Consultingwerk.Studio.Proparse.NodeTypes.* FROM PROPATH .  
USING Progress.Lang.*                            FROM PROPATH .

CLASS Consultingwerk.Studio.Proparse.NodeTypes.NodeTypesParser: 

    {Consultingwerk/Studio/Proparse/NodeTypes/ttNodeTypes.i}

    /*------------------------------------------------------------------------------
        Purpose: Parses the NodeTypes.java
        Notes:   
        @param pcSourceFile The path of the Java source code to parse
        @param ttNodeTypes OUTPUT The ouptut para
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ParseNodeTypesJava (pcSourceFile AS CHARACTER,
	                                       OUTPUT TABLE ttNodeTypes):

        DEFINE VARIABLE cFileName   AS CHARACTER NO-UNDO .		
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cLine       AS CHARACTER NO-UNDO .
                                    
        DEFINE VARIABLE cNodeType   AS CHARACTER NO-UNDO .
        DEFINE VARIABLE iMinAbbr    AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cAblKeyWord AS CHARACTER NO-UNDO .

        FileAssert:Exists (pcSourceFile) .

        ASSIGN cFileName = FileHelper:FindFile (pcSourceFile) .

        INPUT FROM VALUE (cFileName) .

        EMPTY TEMP-TABLE ttNodeTypes .

        REPEAT:
		    IMPORT UNFORMATTED cLine . 
		
		    IF TRIM (cLine) = "static ~{":U THEN 
		      LEAVE .  
		END.
		
		REPEAT:
            IMPORT UNFORMATTED cLine . 

            ASSIGN cLine = TRIM (cLine) .

            IF NOT cLine BEGINS ("add(":U) THEN 
                NEXT . 

            ASSIGN cLine = SUBSTRING (cLine, 5) 
                
                   cNodeType = TRIM (ENTRY (1, cLine))
                   iMinAbbr  = INTEGER (ENTRY (2, cLine))
                   cAblKeyWord = StringHelper:Unquote (TRIM (ENTRY (3, cLine))) . 
            
            IF NOT cAblKeyWord > "":U THEN 
                NEXT .  

            IF CAN-FIND (ttNodeTypes WHERE ttNodeTypes.NodeType = cNodeType) THEN 
                NEXT . 

            CREATE ttNodeTypes . 
            ASSIGN ttNodeTypes.NodeType       = cNodeType
                   ttNodeTypes.MinAbbreviated = iMinAbbr
                   ttNodeTypes.AblKeyWord     = cAblKeyWord .

        END.

	END METHOD .

END CLASS.
