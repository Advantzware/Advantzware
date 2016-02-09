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
    File        : FrameParser
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Nov 19 07:58:10 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                 FROM PROPATH .
USING Consultingwerk.Framework.Collections.*           FROM PROPATH .  
USING Consultingwerk.Studio.LegacyGuiMigration.*       FROM PROPATH .
USING Consultingwerk.Studio.LegacyGuiMigration.Frame.* FROM PROPATH .
USING Consultingwerk.Studio.Proparse.*                 FROM PROPATH .   
USING Consultingwerk.Util.*                            FROM PROPATH . 
USING org.prorefactor.refactor.*                       FROM ASSEMBLY.
USING org.prorefactor.treeparser.*                     FROM ASSEMBLY.
USING org.prorefactor.core.*                           FROM ASSEMBLY.
USING com.joanju.proparse.NodeTypes                    FROM ASSEMBLY .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.Studio.LegacyGuiMigration.Frame.FrameParser
    INHERITS LegacyGuiParser: 

    DEFINE VARIABLE iOrder            AS INTEGER                   NO-UNDO . 

    {Consultingwerk/Studio/LegacyGuiMigration/Frame/ttFrameWidget.i}
    {Consultingwerk/Studio/LegacyGuiMigration/Frame/ttWidget.i}
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FrameParser class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC FrameParser ():
		SUPER ().
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Recursively parses the given file for FORM statements with named
                 Frame references
        Notes:   Processes nested Block_Nodes and BLOCK_LABEL nodes
        @param poNode The JPNode to process
        @param poList The list to add the FORM names to
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID GetFormNames (poNode AS JPNode,
	                                    poList AS CharacterList):
		
        DEFINE VARIABLE oNode  AS JPNode    NO-UNDO .
        DEFINE VARIABLE oWith  AS JPNode    NO-UNDO . 
        DEFINE VARIABLE oFrame AS JPNode    NO-UNDO . 
        DEFINE VARIABLE oID    AS JPNode    NO-UNDO . 
        DEFINE VARIABLE cId    AS CHARACTER NO-UNDO .
        
        ASSIGN oNode = poNode:firstChild () . 
        
        DO WHILE VALID-OBJECT (oNode):
            
            IF NodeTypes:getTypeName(oNode:getType()) = "FORMAT":U THEN DO:
                
                oWith = ProparseHelper:FindChildNodeOfNodeType (oNode, "WITH":U) .
                
                IF VALID-OBJECT (oWith) THEN DO:
                    
                    oFrame = ProparseHelper:FindChildNodeOfNodeType (oWith, "FRAME":U) .
                    
                    IF VALID-OBJECT (oFrame) THEN DO:

                        oID = ProparseHelper:FindChildNodeOfNodeType (oFrame, "ID":U) .
                        
                        IF VALID-OBJECT (oID) THEN DO:
                            
                            ASSIGN cID = oID:getText() .
                            
                            IF cId > "":U AND NOT poList:ContainsValue (cId) THEN 
                                poList:Add (cID) . 
                        END.
                    END . 
                END .                                         
            END.
            ELSE DO:
                IF ProparseHelper:GetSubtypeName (oNode:getSubTypeIndex()) = "BlockNode":U OR
                   ListHelper:EntryIsInList (NodeTypes:getTypeName(oNode:getType()),
                                             cNestingNodeTypes) THEN 
                
                    THIS-OBJECT:GetFormNames (oNode, poList) .                                                
            END.

            oNode = oNode:nextSibling () . 
        END.		

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the given file for the given frame name in FORM statements 
        Notes:   
        @param pcFrameName The name of the frame to return the widgets for
        @param poNode The JPNode to parse
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID GetFormWidgets (pcFrameName AS CHARACTER, 
	                                      poNode AS JPNode):
		
        DEFINE VARIABLE oNode  AS JPNode NO-UNDO .
        DEFINE VARIABLE oWith  AS JPNode NO-UNDO . 
        DEFINE VARIABLE oFrame AS JPNode NO-UNDO . 
        DEFINE VARIABLE oID    AS JPNode NO-UNDO . 
        
        ASSIGN oNode = poNode:firstChild () . 
        
        DO WHILE VALID-OBJECT (oNode):
            
            IF NodeTypes:getTypeName(oNode:getType()) = "FORMAT":U THEN DO:
                
                oWith = ProparseHelper:FindChildNodeOfNodeType (oNode, "WITH":U) .
                
                IF VALID-OBJECT (oWith) THEN DO:
                    
                    oFrame = ProparseHelper:FindChildNodeOfNodeType (oWith, "FRAME":U) .
                    
                    IF VALID-OBJECT (oFrame) THEN DO:

                        oID = ProparseHelper:FindChildNodeOfNodeType (oFrame, "ID":U) .
                        
                        IF VALID-OBJECT (oID) AND oID:getText() = pcFrameName THEN
                            THIS-OBJECT:ProcessFrameWidgets (oNode) .
                              
                    END . 
                END .                                         
            END.
            ELSE DO:
                IF ProparseHelper:GetSubtypeName (oNode:getSubTypeIndex()) = "BlockNode":U OR
                   ListHelper:EntryIsInList (NodeTypes:getTypeName(oNode:getType()),
                                             cNestingNodeTypes) THEN 
                
                    THIS-OBJECT:GetFormWidgets (pcFrameName, oNode) .                                                
            END.

            oNode = oNode:nextSibling () . 
        END.        

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the given file and returns a list of FRAME names
        Notes:   
        @param pcFileName The name of the file to parse
        @return The CharacterList of Frame names
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CharacterList GetFrameNames (pcFileName AS CHARACTER):
		
        DEFINE VARIABLE oParseUnit   AS ParseUnit         NO-UNDO . 
        DEFINE VARIABLE oNodeWrapper AS JPNodesWrapper    NO-UNDO . 
        DEFINE VARIABLE oEnumerator  AS JPNodesEnumerator NO-UNDO . 
        DEFINE VARIABLE oList        AS CharacterList     NO-UNDO . 
        DEFINE VARIABLE cId          AS CHARACTER         NO-UNDO .
		
		oList = NEW CharacterList () .
		
	    THIS-OBJECT:Initialize () .

        THIS-OBJECT:PublishStatus ("Parsing source code.":U) . 
        
        oParseUnit = ProparseHelper:ParseFile (pcFileName) .
        
        oNodeWrapper = NEW JPNodesWrapper (oParseUnit:getTopNode()) . 
        oNodeWrapper:NodeTypeFilter = "DEFINE":U  .
   
        {Consultingwerk/foreachABL.i JPNode oNode in oNodeWrapper}

            IF ProparseHelper:HasChildNodeOfNodeType (oNode, "FRAME":U) THEN DO:
                cId = ProparseHelper:FindChildNodeOfNodeType (oNode, "ID":U):getText () .
                
                IF cId > "":U AND NOT oList:ContainsValue (cId) THEN 
                    oList:Add (cId) .
            END.
        END.        

        THIS-OBJECT:GetFormNames (oParseUnit:getTopNode(),
                                  oList) .

        RETURN oList . 

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oNodeWrapper) .
            GarbageCollectorHelper:DeleteObject (oParseUnit) .		
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the given file for the given frame name and returns a temp-table
                 containing all the widgets found in that frame
        Notes:   
        @param pcFileName The name of the file to parse
        @param pcFrameName The name of the frame to return the widgets for
        @param ttFrameWidget OUTPUT TABLE The temp-table with the frame widget information
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID GetFrameWidgets (pcFileName AS CHARACTER,
	                                    pcFrameName AS CHARACTER,
	                                    OUTPUT TABLE ttFrameWidget):
		
        DEFINE VARIABLE oParseUnit        AS ParseUnit                 NO-UNDO . 
        DEFINE VARIABLE oNodeWrapper      AS JPNodesWrapper            NO-UNDO . 
        DEFINE VARIABLE oFormatNode       AS JPNode                    NO-UNDO .  
        DEFINE VARIABLE oViewAsNode       AS JPNode                    NO-UNDO .  
        DEFINE VARIABLE oSizeNode         AS JPNode                    NO-UNDO .  
        DEFINE VARIABLE oEqual            AS JPNode                    NO-UNDO . 
        DEFINE VARIABLE oWidgetRef        AS JPNode                    NO-UNDO . 
        DEFINE VARIABLE oIn               AS JPNode                    NO-UNDO . 
        DEFINE VARIABLE oList             AS CharacterList             NO-UNDO . 
        DEFINE VARIABLE cId               AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE cFieldId          AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE cType             AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE oNumericFormat    AS NumericFormat             NO-UNDO . 
        DEFINE VARIABLE oBufferFieldName  AS BufferFieldName           NO-UNDO .
        DEFINE VARIABLE hBuffer           AS HANDLE                    NO-UNDO .
        DEFINE VARIABLE hField            AS HANDLE                    NO-UNDO .
        DEFINE VARIABLE oPostProcessor    AS IFrameParserPostProcessor NO-UNDO . 
        
        ASSIGN iOrder = 0 . 
        
        oNumericFormat = SessionHelper:GetNumericFormat() .
        SESSION:NUMERIC-FORMAT = "AMERICAN":U . 
        
        EMPTY TEMP-TABLE ttFrameWidget .
        EMPTY TEMP-TABLE ttWidget .
        
        oList = NEW CharacterList () .
        
        THIS-OBJECT:Initialize () .

        THIS-OBJECT:PublishStatus ("Parsing source code.":U) . 
        
        oParseUnit = ProparseHelper:ParseFile (pcFileName) .
        
        oNodeWrapper = NEW JPNodesWrapper (oParseUnit:getTopNode()) . 
        oNodeWrapper:NodeTypeFilter = "DEFINE":U  .
   
        {Consultingwerk/foreachABL.i JPNode oNode in oNodeWrapper}
            ASSIGN cType = oNode:firstChild():getText () .            

            IF NOT cType = "VARIABLE":U AND NOT cType = "BUTTON":U AND NOT cType = "RECTANGLE":U AND NOT cType = "BROWSE":U THEN 
                NEXT . 

            IF NOT ProparseHelper:HasChildNodeOfNodeType (oNode, "ID":U) THEN 
                NEXT .

            CREATE ttWidget .
            ASSIGN ttWidget.FieldName  = ProparseHelper:FindChildNodeOfNodeType (oNode, "ID":U):getText () 
                   ttWidget.WidgetType = cType.

            IF ProparseHelper:HasChildNodeOfNodeType (oNode, "AS":U) THEN 
                ttWidget.FieldDataType = ProparseHelper:FindChildNodeOfNodeType (oNode, "AS":U):firstChild ():getText () .   

            IF ProparseHelper:HasChildNodeOfNodeType (oNode, "LABEL":U) THEN 
                ttWidget.WidgetLabel = StringHelper:Unquote(ProparseHelper:FindChildNodeOfNodeType (oNode, "LABEL":U):firstChild ():getText ()) .   

            IF ProparseHelper:HasChildNodeOfNodeType (oNode, "INITIAL":U) THEN 
                ttWidget.WidgetValue = ProparseHelper:FindChildNodeOfNodeType (oNode, "INITIAL":U):firstChild ():getText () .

            IF ProparseHelper:HasChildNodeOfNodeType (oNode, "FORMAT":U) THEN 
                ttWidget.FieldFormat = StringHelper:Unquote(ProparseHelper:FindChildNodeOfNodeType (oNode, "FORMAT":U):firstChild ():getText ()) .   

            IF ProparseHelper:HasChildNodeOfNodeType (oNode, "SIZE":U) THEN DO: 
                oSizeNode = ProparseHelper:FindChildNodeOfNodeType (oNode, "SIZE":U):firstChild ()  .

                ttWidget.ViewAsWidth = DECIMAL (oSizeNode:getText ()) .
                
                oSizeNode = oSizeNode:nextSibling ():nextSibling () .
                
                ttWidget.ViewAsHeight = DECIMAL (oSizeNode:getText ()) .
            END.
            
            IF ProparseHelper:HasChildNodeOfNodeType (oNode, "VIEWAS":U) THEN DO:
                oViewAsNode = ProparseHelper:FindChildNodeOfNodeType (oNode, "VIEWAS":U):firstChild ()  .

                ttWidget.ViewAs = oViewAsNode:getText () .

                IF ProparseHelper:HasChildNodeOfNodeType (oViewAsNode, "SIZE":U) THEN DO:
                    oSizeNode = ProparseHelper:FindChildNodeOfNodeType (oViewAsNode, "SIZE":U):firstChild ()  .

                    ttWidget.ViewAsWidth = DECIMAL (oSizeNode:getText ()) .

                    oSizeNode = oSizeNode:nextSibling ():nextSibling () .

                    ttWidget.ViewAsHeight = DECIMAL (oSizeNode:getText ()) .
                END.
                    
                IF ProparseHelper:HasChildNodeOfNodeType (oViewAsNode, "RADIOBUTTONS":U) THEN 
                    ASSIGN ttWidget.RadioButtons = THIS-OBJECT:ParseRadioButtons (oViewAsNode) . 

                IF ProparseHelper:HasChildNodeOfNodeType (oViewAsNode, "LISTITEMS":U) THEN 
                    ASSIGN ttWidget.ListItems = THIS-OBJECT:ParseListItems (oViewAsNode) .
                        
                IF ProparseHelper:HasChildNodeOfNodeType (oViewAsNode, "LISTITEMPAIRS":U) THEN 
                    ASSIGN ttWidget.ListItemPairs = THIS-OBJECT:ParseListItemPairs (oViewAsNode) . 
            END.
        END.
            
        {Consultingwerk/foreachABL.i JPNode oNode in oNodeWrapper nodefine}

            IF ProparseHelper:HasChildNodeOfNodeType (oNode, "FRAME":U) THEN DO:
                cId = ProparseHelper:FindChildNodeOfNodeType (oNode, "ID":U):getText () .
                
                IF cId = pcFrameName THEN 
                    THIS-OBJECT:ProcessFrameWidgets (oNode) . 
            END.
        END.        

        THIS-OBJECT:GetFormWidgets (pcFrameName, oParseUnit:getTopNode()) .

        /* Mike Fechner, Consultingwerk Ltd. 16.08.2014
           SCL-396 - Parsing of ASSIGN PRIVATE-DATA Statements */
        oParseUnit = ProparseHelper:ParseFile (pcFileName) .
        
        oNodeWrapper = NEW JPNodesWrapper (oParseUnit:getTopNode()) . 
        oNodeWrapper:NodeTypeFilter = "ASSIGN":U  .
      
        {Consultingwerk/foreachABL.i JPNode oNode in oNodeWrapper nodefine}
            IF ProparseHelper:HasChildNodeOfNodeType (oNode, "EQUAL":U) THEN DO:
                
                oEqual = ProparseHelper:FindChildNodeOfNodeType (oNode, "EQUAL":U) .

                IF ProparseHelper:HasChildNodeOfNodeType (oEqual, "Widget_ref":U) AND 
                   ProparseHelper:HasChildNodeOfNodeType (oEqual, "QSTRING":U) THEN DO:

                    oWidgetRef = ProparseHelper:FindChildNodeOfNodeType (oEqual, "Widget_ref":U) .

                    IF ProparseHelper:HasChildNodeOfNodeType (oWidgetRef, "Field_ref":U) AND 
                       ProparseHelper:HasChildNodeOfNodeType (oWidgetRef, "ID":U) AND 
                       ProparseHelper:HasChildNodeOfNodeType (oWidgetRef, "IN":U) THEN DO:

                        IF ProparseHelper:FindChildNodeOfNodeType (oWidgetRef, "ID":U):getText () = "PRIVATE-DATA":U THEN DO:
                            
                            ASSIGN oIn = ProparseHelper:FindChildNodeOfNodeType (oWidgetRef, "IN":U) .
                            
                            IF ProparseHelper:HasChildNodeOfNodeType (oIn, "FRAME":U) AND 
                               ProparseHelper:HasChildNodeOfNodeType (oIn, "ID":U) THEN DO:
                            
                                cId = ProparseHelper:FindChildNodeOfNodeType (oIn, "ID":U):getText () .
                                
                                IF cId = pcFrameName THEN DO:

                                    FIND ttFrameWidget WHERE ttFrameWidget.FieldName = ProparseHelper:FindChildNodeOfNodeType (oWidgetRef, "Field_ref":U):firstChild():getText () 
                                        NO-ERROR . 
                                        
                                    IF AVAILABLE ttFrameWidget THEN 
                                        ASSIGN ttFrameWidget.PrivateData = StringHelper:Unquote(ProparseHelper:FindChildNodeOfNodeType (oEqual, "QSTRING":U):getText()) .
                                END .
                            END .
                        END .
                    END.
                END . 
            END .
        END . 

        /* Mike Fechner, Consultingwerk Ltd. 10.12.2014
           SCL-541: Allow to post-process parser result through customization class */
        oPostProcessor = {Consultingwerk/get-service.i Consultingwerk.Studio.LegacyGuiMigration.Frame.IFrameParserPostProcessor} .
        
        IF VALID-OBJECT (oPostProcessor) THEN 
            oPostProcessor:PostProcessFrameWidgets (pcFileName,
                                                    pcFrameName,
                                                    INPUT-OUTPUT TABLE ttFrameWidget BY-REFERENCE) .

        FINALLY:
            SessionHelper:SetNumericFormat (oNumericFormat) .
            
            GarbageCollectorHelper:DeleteObject (oNodeWrapper) .
            GarbageCollectorHelper:DeleteObject (oParseUnit) .      
            
            EMPTY TEMP-TABLE ttWidget .
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses a DEFINE FRAME or FORM statement node
        Notes:   
        @param poNode The Node to process
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ProcessFrameWidgets (poNode AS JPNode):
		
        DEFINE VARIABLE oFrameNodeWrapper AS JPNodesWrapper            NO-UNDO .
		
        DEFINE VARIABLE oCurrentNode      AS JPNode                    NO-UNDO .  
        DEFINE VARIABLE oFormatNode       AS JPNode                    NO-UNDO .  
        DEFINE VARIABLE oViewAsNode       AS JPNode                    NO-UNDO .  
        DEFINE VARIABLE oSizeNode         AS JPNode                    NO-UNDO .  
        DEFINE VARIABLE oEqual            AS JPNode                    NO-UNDO . 
        DEFINE VARIABLE oWidgetRef        AS JPNode                    NO-UNDO . 
        DEFINE VARIABLE oIn               AS JPNode                    NO-UNDO . 
        DEFINE VARIABLE oList             AS CharacterList             NO-UNDO . 
        DEFINE VARIABLE cId               AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE cFieldId          AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE cType             AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE oNumericFormat    AS NumericFormat             NO-UNDO . 
        DEFINE VARIABLE oBufferFieldName  AS BufferFieldName           NO-UNDO .
        DEFINE VARIABLE hBuffer           AS HANDLE                    NO-UNDO .
        DEFINE VARIABLE hField            AS HANDLE                    NO-UNDO .
		
        ASSIGN oFrameNodeWrapper = NEW JPNodesWrapper (poNode) 
               oFrameNodeWrapper:NodeTypeFilter = "Form_Item":U .
        
        {Consultingwerk/foreachABL.i JPNode oFieldNode in oFrameNodeWrapper}

            /* Ignore SKIP phrases */
            IF ProparseHelper:HasChildNodeOfNodeType (oFieldNode, "SKIP":U) THEN
                NEXT .  

            CREATE ttFrameWidget .
            ASSIGN iOrder              = iOrder + 1
                   ttFrameWidget.Order = iOrder . 

            IF ProparseHelper:HasChildNodeOfNodeType (oFieldNode, "field_ref":U) THEN DO:
                oCurrentNode = ProparseHelper:FindChildNodeOfNodeType (oFieldNode, "field_ref":U) . 
                
                IF ProparseHelper:HasChildNodeOfNodeType (oCurrentNode, "ID":U) THEN DO:
                    ASSIGN cFieldId = ProparseHelper:FindChildNodeOfNodeType (oCurrentNode, "ID":U):getText () .
                    
                    FIND ttWidget WHERE ttWidget.FieldName = cFieldId NO-ERROR . 
                    IF AVAILABLE ttWidget THEN 
                        BUFFER-COPY ttWidget TO ttFrameWidget .
                    
                    ASSIGN ttFrameWidget.FieldName = cFieldId .
                END .
            END.
            ELSE IF ProparseHelper:HasChildNodeOfNodeType (oFieldNode, "QSTRING":U) THEN DO:
                oCurrentNode = ProparseHelper:FindChildNodeOfNodeType (oFieldNode, "QSTRING":U) . 
            
                ASSIGN ttFrameWidget.WidgetType  = "LITERAL":U
                       ttFrameWidget.WidgetValue = oCurrentNode:getText () . 
            END.
            
            IF ProparseHelper:HasChildNodeOfNodeType (oFieldNode, "Format_phrase":U) THEN DO:
                oFormatNode = ProparseHelper:FindChildNodeOfNodeType (oFieldNode, "Format_phrase":U) .
            
                IF ProparseHelper:HasChildNodeOfNodeType (oFormatNode, "LABEL":U) THEN 
                    ttFrameWidget.WidgetLabel = StringHelper:Unquote(ProparseHelper:FindChildNodeOfNodeType (oFormatNode, "LABEL":U):firstChild ():getText ()) .   
    
                IF ProparseHelper:HasChildNodeOfNodeType (oFormatNode, "FORMAT":U) THEN 
                    ttFrameWidget.FieldFormat = StringHelper:Unquote(ProparseHelper:FindChildNodeOfNodeType (oFormatNode, "FORMAT":U):firstChild ():getText ()) .   
            
                /* VIEW-AS */
                IF ProparseHelper:HasChildNodeOfNodeType (oFormatNode, "VIEWAS":U) THEN DO:
                    oCurrentNode = ProparseHelper:FindChildNodeOfNodeType (oFormatNode, "VIEWAS":U)  .

                    oViewAsNode = oCurrentNode:firstChild () .

                    IF VALID-OBJECT (oViewAsNode) THEN DO:
                        ttFrameWidget.ViewAs = oViewAsNode:getText ()  .
                        
                        IF ProparseHelper:HasChildNodeOfNodeType (oViewAsNode, "SIZE":U) THEN DO: 
                            oSizeNode = ProparseHelper:FindChildNodeOfNodeType (oViewAsNode, "SIZE":U):firstChild ()  .

                            ttFrameWidget.ViewAsWidth = DECIMAL (oSizeNode:getText ()) .
                            
                            oSizeNode = oSizeNode:nextSibling ():nextSibling () .
                            
                            ttFrameWidget.ViewAsHeight = DECIMAL (oSizeNode:getText ()) .
                        END.
                            
                        IF ProparseHelper:HasChildNodeOfNodeType (oViewAsNode, "RADIOBUTTONS":U) THEN 
                            ASSIGN ttFrameWidget.RadioButtons = THIS-OBJECT:ParseRadioButtons (oViewAsNode) . 
        
                        IF ProparseHelper:HasChildNodeOfNodeType (oViewAsNode, "LISTITEMS":U) THEN 
                            ASSIGN ttFrameWidget.ListItems = THIS-OBJECT:ParseListItems (oViewAsNode) . 
                                
                        IF ProparseHelper:HasChildNodeOfNodeType (oViewAsNode, "LISTITEMPAIRS":U) THEN 
                            ASSIGN ttFrameWidget.ListItemPairs = THIS-OBJECT:ParseListItemPairs (oViewAsNode) . 
                        
                    END.
                END. 
            
                /* AT */
                IF ProparseHelper:HasChildNodeOfNodeType (oFormatNode, "AT":U) THEN DO:
                    oCurrentNode = ProparseHelper:FindChildNodeOfNodeType (oFormatNode, "AT":U)  .
                    
                    IF ProparseHelper:HasChildNodeOfNodeType (oCurrentNode, "ROW":U) THEN
                        ttFrameWidget.AtRow = DECIMAL(ProparseHelper:FindChildNodeOfNodeType (oCurrentNode, "ROW":U):firstChild():getText ()) NO-ERROR .
                    IF ProparseHelper:HasChildNodeOfNodeType (oCurrentNode, "COLUMN":U) THEN
                        ttFrameWidget.AtColumn = DECIMAL(ProparseHelper:FindChildNodeOfNodeType (oCurrentNode, "COLUMN":U):firstChild():getText ()) NO-ERROR .
                END.
                                
                IF ProparseHelper:HasChildNodeOfNodeType (oFormatNode, "NOLABELS":U) THEN 
                    ttFrameWidget.NoLabels = TRUE .             
            END.
            
            /* Mike Fechner, Consultingwerk Ltd. 02.12.2013
               Determine Data Dictionary Label, Format, Data-Type of Field */
            IF (ttFrameWidget.WidgetLabel = "":U OR ttFrameWidget.FieldFormat = "":U OR  ttFrameWidget.FieldDataType = "":U)  
                AND NUM-ENTRIES (ttFrameWidget.FieldName, ".":U) > 1 THEN DO ON ERROR UNDO, THROW:
                    
                oBufferFieldName = BufferHelper:ParseFieldName (ttFrameWidget.FieldName) . 

                hBuffer = BufferHelper:CreateBuffer (oBufferFieldName) .
                
                IF VALID-HANDLE (hBuffer) THEN DO:
                    IF BufferHelper:HasField (hBuffer, oBufferFieldName:FieldName) THEN DO: 
                        hField = hBuffer:BUFFER-FIELD (oBufferFieldName:FieldName) .
                    
                        IF ttFrameWidget.WidgetLabel = "":U THEN 
                            ASSIGN ttFrameWidget.WidgetLabel = hField:LABEL .
                            
                        IF ttFrameWidget.FieldFormat = "":U THEN 
                            ASSIGN ttFrameWidget.FieldFormat = hField:FORMAT .
                         
                        IF ttFrameWidget.FieldDataType = "":U THEN 
                            ASSIGN ttFrameWidget.FieldDataType = UPPER(hField:DATA-TYPE) .
                    END.
                END.

                CATCH err AS Progress.Lang.Error:
                    /* ignore errors */ 
                END CATCH.
                
                FINALLY:
                    GarbageCollectorHelper:DeleteObject (hBuffer) .     
                END FINALLY.
            END.
            
            RELEASE ttFrameWidget .
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the FrameParser class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC FrameParser ():

    END DESTRUCTOR .

END CLASS.
