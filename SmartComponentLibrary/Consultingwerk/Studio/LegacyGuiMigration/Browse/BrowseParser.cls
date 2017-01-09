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
    File        : BrowseParser
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 05 22:17:09 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                  FROM PROPATH .
USING Consultingwerk.Framework.Collections.*            FROM PROPATH .
USING Consultingwerk.Studio.LegacyGuiMigration.*        FROM PROPATH .
USING Consultingwerk.Studio.LegacyGuiMigration.Browse.* FROM PROPATH .
USING Consultingwerk.Studio.Proparse.*                  FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH . 
USING org.prorefactor.refactor.*                        FROM ASSEMBLY.
USING org.prorefactor.treeparser.*                      FROM ASSEMBLY.
USING org.prorefactor.core.*                            FROM ASSEMBLY.
USING com.joanju.proparse.NodeTypes                     FROM ASSEMBLY .
USING Progress.Lang.*                                   FROM PROPATH .
USING Consultingwerk.Assertion.* FROM PROPATH.

CLASS Consultingwerk.Studio.LegacyGuiMigration.Browse.BrowseParser
    INHERITS LegacyGuiParser: 

    DEFINE VARIABLE iOrder            AS INTEGER                   NO-UNDO . 
    
    {Consultingwerk/Studio/LegacyGuiMigration/Browse/ttBrowseColumn.i}
    {Consultingwerk/Studio/LegacyGuiMigration/Browse/ttWidget.i}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BrowseParser class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BrowseParser ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Extracts the Query String from the OPEN QUERY Statement
        Notes:
        @param pcOpenQueryStatement The full text of the OPEN QUERY Statement
        @param pcQueryName The name of the query
        @return The extraced query string
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER ExtractQueryString (pcOpenQueryStatement AS CHARACTER,
                                                   pcQueryName AS CHARACTER):
        
        /* Remove OPEN */
        ASSIGN pcOpenQueryStatement = SUBSTRING (LEFT-TRIM (pcOpenQueryStatement), 5) . 

        /* Remove QUERY */                        
        ASSIGN pcOpenQueryStatement = SUBSTRING (LEFT-TRIM (pcOpenQueryStatement), 6) . 
        
        /* Remove Query name */                      
        ASSIGN pcOpenQueryStatement = SUBSTRING (LEFT-TRIM (pcOpenQueryStatement), LENGTH (pcQueryName) + 1) . 

        ASSIGN pcOpenQueryStatement = RIGHT-TRIM (pcOpenQueryStatement) .
        
        IF StringHelper:Ends (pcOpenQueryStatement, ".":U) THEN 
            pcOpenQueryStatement = SUBSTRING (pcOpenQueryStatement, 1, LENGTH (pcOpenQueryStatement) - 1) . 
         
        RETURN TRIM (pcOpenQueryStatement) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Locates all OPEN QUERY Stamements in the given parse unit and adds 
                 them to the BrowseWidgetQueryInformation  
        Notes:
        @param poParseUnit The ParseUnit for the current source
        @param poQueryInfo The BrowseWidgetQueryInformation to add the query strings to, must contain QueryName   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FindOpenQueryStatements (poParseUnit AS ParseUnit,
                                                   poQueryInfo AS BrowseWidgetQueryInformation):
        
        ObjectAssert:IsValid (poParseUnit, "poParseUnit":U) .
        ObjectAssert:IsValid (poQueryInfo , "poQueryInfo":U) .
        Assert:NotNullOrEmpty (poQueryInfo:QueryName, "poQueryInfo:QueryName":U) .

        THIS-OBJECT:FindOpenQueryStatements (poParseUnit:getTopNode (),
                                             poQueryInfo) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Recursivly locates all OPEN QUERY Stamements in the given JPNode and 
                 adds them to the BrowseWidgetQueryInformation  
        Notes:
        @param poNode The JPNode to process
        @param poQueryInfo The BrowseWidgetQueryInformation to add the query strings to, must contain QueryName   
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID FindOpenQueryStatements (poNode AS JPNode,
                                                 poQueryInfo AS BrowseWidgetQueryInformation):
        
        DEFINE VARIABLE oChild       AS JPNode    NO-UNDO . 
        DEFINE VARIABLE oId          AS JPNode    NO-UNDO .  
        DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO .
        
        oChild = poNode:firstChild () . 
        
        DO WHILE VALID-OBJECT (oChild):        
        
            IF NodeTypes:getTypeName (oChild:getType()) = "OPEN":U THEN DO:

                oId = ProparseHelper:FindChildNodeOfNodeType (oChild, "ID":U) . 
                
                IF NOT VALID-OBJECT (oId) THEN DO:
                    oChild = oChild:nextSibling () .
                    NEXT .                     
                END.

                IF NOT oId:getText () = poQueryInfo:QueryName THEN DO:
                    oChild = oChild:nextSibling () .
                    NEXT .                     
                END.

                ASSIGN cQueryString = THIS-OBJECT:ExtractQueryString (oChild:toStringFulltext(),
                                                                      poQueryInfo:QueryName) . 

                IF NOT poQueryInfo:OpenQueryStatements:ContainsValue (cQueryString) THEN 
                    poQueryInfo:OpenQueryStatements:Add (cQueryString) .
            END . 
            
            ELSE IF VALID-OBJECT (oChild:firstChild ()) THEN
             
                THIS-OBJECT:FindOpenQueryStatements (oChild,
                                                     poQueryInfo) .
        
            oChild = oChild:nextSibling () . 
        END . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Parses the given file and returns a list of BROWSE names
        Notes:   
        @param pcFileName The name of the file to parse
        @return The CharacterList of Frame names
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CharacterList GetBrowseNames (pcFileName AS CHARACTER):
        
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

            IF ProparseHelper:HasChildNodeOfNodeType (oNode, "BROWSE":U) THEN DO:
                cId = ProparseHelper:FindChildNodeOfNodeType (oNode, "ID":U):getText () .
                
                IF cId > "":U AND NOT oList:ContainsValue (cId) THEN 
                    oList:Add (cId) .
            END.
        END.        

        RETURN oList . 

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oNodeWrapper) .
            GarbageCollectorHelper:DeleteObject (oParseUnit) .      
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the given file for the given browse name and returns a temp-table
                 containing all the columns defined in that browse
        Notes:   
        @param pcFileName The name of the file to parse
        @param pcBrowseName The name of the browse to return the columns for
        @param ttBrowseColumn OUTPUT TABLE The temp-table with the frame widget information
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetBrowseColumns (pcFileName AS CHARACTER,
                                         pcBrowseName AS CHARACTER,
                                         OUTPUT TABLE ttBrowseColumn):

        DEFINE VARIABLE oParseUnit        AS ParseUnit                  NO-UNDO .
        DEFINE VARIABLE oNodeWrapper      AS JPNodesWrapper             NO-UNDO .
        DEFINE VARIABLE oFormatNode       AS JPNode                     NO-UNDO .
        DEFINE VARIABLE oViewAsNode       AS JPNode                     NO-UNDO .
        DEFINE VARIABLE cId               AS CHARACTER                  NO-UNDO .
        DEFINE VARIABLE cType             AS CHARACTER                  NO-UNDO .
        DEFINE VARIABLE oNumericFormat    AS NumericFormat              NO-UNDO .
        DEFINE VARIABLE oPostProcessor    AS IBrowseParserPostProcessor NO-UNDO .

        ASSIGN iOrder = 0 .

        oNumericFormat = SessionHelper:GetNumericFormat() .
        SESSION:NUMERIC-FORMAT = "AMERICAN":U .

        EMPTY TEMP-TABLE ttBrowseColumn .
        EMPTY TEMP-TABLE ttWidget .

        THIS-OBJECT:Initialize () .

        THIS-OBJECT:PublishStatus ("Parsing source code.":U) .

        oParseUnit = ProparseHelper:ParseFile (pcFileName) .

        oNodeWrapper = NEW JPNodesWrapper (oParseUnit:getTopNode()) .
        oNodeWrapper:NodeTypeFilter = "DEFINE":U  .

        {Consultingwerk/foreachABL.i JPNode oNode in oNodeWrapper}
            ASSIGN cType = oNode:firstChild():getText () .

            IF NOT cType = "VARIABLE":U THEN
                NEXT .

            IF NOT ProparseHelper:HasChildNodeOfNodeType (oNode, "ID":U) THEN
                NEXT .

            CREATE ttWidget .
            ASSIGN ttWidget.FieldName  = ProparseHelper:FindChildNodeOfNodeType (oNode, "ID":U):getText () .

            IF ProparseHelper:HasChildNodeOfNodeType (oNode, "AS":U) THEN
                ttWidget.FieldDataType = ProparseHelper:FindChildNodeOfNodeType (oNode, "AS":U):firstChild ():getText () .

            IF ProparseHelper:HasChildNodeOfNodeType (oNode, "LABEL":U) THEN
                ttWidget.WidgetLabel = StringHelper:Unquote(ProparseHelper:FindChildNodeOfNodeType (oNode, "LABEL":U):firstChild ():getText ()) .

            IF ProparseHelper:HasChildNodeOfNodeType (oNode, "INITIAL":U) THEN
                ttWidget.WidgetValue = ProparseHelper:FindChildNodeOfNodeType (oNode, "INITIAL":U):firstChild ():getText () .

            IF ProparseHelper:HasChildNodeOfNodeType (oNode, "FORMAT":U) THEN
                ttWidget.FieldFormat = StringHelper:Unquote(ProparseHelper:FindChildNodeOfNodeType (oNode, "FORMAT":U):firstChild ():getText ()) .

            IF ProparseHelper:HasChildNodeOfNodeType (oNode, "VIEWAS":U) THEN DO:
                oViewAsNode = ProparseHelper:FindChildNodeOfNodeType (oNode, "VIEWAS":U):firstChild ()  .

                ttWidget.ViewAs = oViewAsNode:getText () .

                IF ProparseHelper:HasChildNodeOfNodeType (oViewAsNode, "RADIOBUTTONS":U) THEN
                    ASSIGN ttWidget.RadioButtons = THIS-OBJECT:ParseRadioButtons (oViewAsNode) .

                IF ProparseHelper:HasChildNodeOfNodeType (oViewAsNode, "LISTITEMS":U) THEN
                    ASSIGN ttWidget.ListItems = THIS-OBJECT:ParseListItems (oViewAsNode) .

                IF ProparseHelper:HasChildNodeOfNodeType (oViewAsNode, "LISTITEMPAIRS":U) THEN
                    ASSIGN ttWidget.ListItemPairs = THIS-OBJECT:ParseListItemPairs (oViewAsNode) .
            END.
        END.

        {Consultingwerk/foreachABL.i JPNode oNode in oNodeWrapper nodefine}

            IF ProparseHelper:HasChildNodeOfNodeType (oNode, "BROWSE":U) THEN DO:
                cId = ProparseHelper:FindChildNodeOfNodeType (oNode, "ID":U):getText () .
                
                IF cId = pcBrowseName THEN 
                    THIS-OBJECT:ProcessBrowseColumns (oNode) . 
            END.
        END.        

        /* Mike Fechner, Consultingwerk Ltd. 10.12.2014
           SCL-541: Allow to post-process parser result through customization class */
        oPostProcessor = {Consultingwerk/get-service.i Consultingwerk.Studio.LegacyGuiMigration.Browse.IBrowseParserPostProcessor} .

        IF VALID-OBJECT (oPostProcessor) THEN
            oPostProcessor:PostProcessBrowseColumn (pcFileName,
                                                    pcBrowseName,
                                                    INPUT-OUTPUT TABLE ttBrowseColumn BY-REFERENCE) .

        FINALLY:
            SessionHelper:SetNumericFormat (oNumericFormat) .

            GarbageCollectorHelper:DeleteObject (oNodeWrapper) .
            GarbageCollectorHelper:DeleteObject (oParseUnit) .

            EMPTY TEMP-TABLE ttWidget .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the given file for the given browse name and returns a the 
                 BrowseWidgetQueryInformation with the name of the browse query and
                 all OPEN QUERY Statements found in source code 
        Notes:   
        @param pcFileName The name of the file to parse
        @param pcBrowseName The name of the browse to return the columns for
        @return The BrowseWidgetQueryInformation with the name of the browse query and all OPEN QUERY Statements found in source code  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC BrowseWidgetQueryInformation GetBrowseQueryInformation (pcFileName AS CHARACTER,
                                                                          pcBrowseName AS CHARACTER):
        
        DEFINE VARIABLE oParseUnit        AS ParseUnit                    NO-UNDO .
        DEFINE VARIABLE oNodeWrapper      AS JPNodesWrapper               NO-UNDO .
        DEFINE VARIABLE oQueryNode        AS JPNode                       NO-UNDO .
        DEFINE VARIABLE cId               AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE cType             AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE oNumericFormat    AS NumericFormat                NO-UNDO .
        DEFINE VARIABLE oPostProcessor    AS IBrowseParserPostProcessor   NO-UNDO .
        DEFINE VARIABLE oBrowseInfo       AS BrowseWidgetQueryInformation NO-UNDO . 

        ASSIGN iOrder = 0 .

        oNumericFormat = SessionHelper:GetNumericFormat() .
        SESSION:NUMERIC-FORMAT = "AMERICAN":U .

        THIS-OBJECT:Initialize () .

        THIS-OBJECT:PublishStatus ("Parsing source code.":U) .

        oParseUnit = ProparseHelper:ParseFile (pcFileName) .

        oNodeWrapper = NEW JPNodesWrapper (oParseUnit:getTopNode()) .
        oNodeWrapper:NodeTypeFilter = "DEFINE":U  .

        {Consultingwerk/foreachABL.i JPNode oNode in oNodeWrapper}

            IF ProparseHelper:HasChildNodeOfNodeType (oNode, "BROWSE":U) THEN DO:
                cId = ProparseHelper:FindChildNodeOfNodeType (oNode, "ID":U):getText () .
                
                IF cId = pcBrowseName THEN DO: 
                    IF ProparseHelper:HasChildNodeOfNodeType (oNode, "QUERY":U) THEN DO:
                        
                        oQueryNode = ProparseHelper:FindChildNodeOfNodeType (oNode, "QUERY":U) .
                    
                        oBrowseInfo = NEW BrowseWidgetQueryInformation (ProparseHelper:FindChildNodeOfNodeType (oQueryNode, "ID":U):getText ()) .                
                    
                        THIS-OBJECT:FindOpenQueryStatements (oParseUnit, oBrowseInfo) .
                        
                        oBrowseInfo:BufferNames = THIS-OBJECT:GetQueryBufferNames (oParseUnit, oBrowseInfo:QueryName) . 
                        
                        RETURN oBrowseInfo . 
                    END .
                    
                    RETURN ? . 
                END. 
            END.
        END.        

        FINALLY:
            SessionHelper:SetNumericFormat (oNumericFormat) .

            GarbageCollectorHelper:DeleteObject (oNodeWrapper) .
            GarbageCollectorHelper:DeleteObject (oParseUnit) .

            EMPTY TEMP-TABLE ttWidget .
        END FINALLY.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Parses a DEFINE BROWSE Statement
        Notes:
        @param poNode The Node to process
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ProcessBrowseColumns (poNode AS JPNode):
        
        DEFINE VARIABLE oFrameNodeWrapper AS JPNodesWrapper            NO-UNDO .
        DEFINE VARIABLE oDisplayNode      AS JPNode                    NO-UNDO .  
        
        DEFINE VARIABLE oCurrentNode      AS JPNode                    NO-UNDO .  
        DEFINE VARIABLE oFormatNode       AS JPNode                    NO-UNDO .  
        DEFINE VARIABLE oViewAsNode       AS JPNode                    NO-UNDO .  
        DEFINE VARIABLE oWidth            AS JPNode                    NO-UNDO .  
        DEFINE VARIABLE oEqual            AS JPNode                    NO-UNDO . 
        DEFINE VARIABLE oWidgetRef        AS JPNode                    NO-UNDO . 
        DEFINE VARIABLE oLexatNode        AS JPNode                    NO-UNDO . 
        DEFINE VARIABLE oAtColumnNameNode AS JPNode                    NO-UNDO . 
        DEFINE VARIABLE oIn               AS JPNode                    NO-UNDO . 
        DEFINE VARIABLE oList             AS CharacterList             NO-UNDO . 
        DEFINE VARIABLE cId               AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE iExtent           AS INTEGER                   NO-UNDO .
        DEFINE VARIABLE cFieldId          AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE cType             AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE oNumericFormat    AS NumericFormat             NO-UNDO . 
        DEFINE VARIABLE oBufferFieldName  AS BufferFieldName           NO-UNDO .
        DEFINE VARIABLE hBuffer           AS HANDLE                    NO-UNDO .
        DEFINE VARIABLE hField            AS HANDLE                    NO-UNDO .
        
        ASSIGN oDisplayNode = ProparseHelper:FindChildNodeOfNodeType (poNode, "DISPLAY":U) .

        IF NOT VALID-OBJECT (oDisplayNode) THEN 
            RETURN . 
        
        ASSIGN oFrameNodeWrapper = NEW JPNodesWrapper (oDisplayNode) 
               oFrameNodeWrapper:NodeTypeFilter = "Form_Item":U .
        
        {Consultingwerk/foreachABL.i JPNode oFieldNode in oFrameNodeWrapper}

            /* Ignore SKIP phrases */
            IF ProparseHelper:HasChildNodeOfNodeType (oFieldNode, "SKIP":U) THEN
                NEXT .  

            CREATE ttBrowseColumn .
            ASSIGN iOrder               = iOrder + 1
                   ttBrowseColumn.Order = iOrder . 

            IF ProparseHelper:HasChildNodeOfNodeType (oFieldNode, "field_ref":U) THEN DO:
                oCurrentNode = ProparseHelper:FindChildNodeOfNodeType (oFieldNode, "field_ref":U) . 
                
                IF ProparseHelper:HasChildNodeOfNodeType (oCurrentNode, "ID":U) THEN DO:
                    ASSIGN cFieldId = ProparseHelper:FindChildNodeOfNodeType (oCurrentNode, "ID":U):getText () .
                    
                    FIND ttWidget WHERE ttWidget.FieldName = cFieldId NO-ERROR . 
                    IF AVAILABLE ttWidget THEN 
                        BUFFER-COPY ttWidget TO ttBrowseColumn .
                    
                    ASSIGN ttBrowseColumn.FieldName = cFieldId .
                END .
                
                IF ProparseHelper:HasChildNodeOfNodeType (oCurrentNode, "Array_subscript":U) THEN DO:
                    ASSIGN iExtent = INTEGER (ProparseHelper:FindChildNodeOfNodeType (ProparseHelper:FindChildNodeOfNodeType (oCurrentNode, "Array_subscript":U),
                                                                                      "number":U):getText ()) .
                    
                    ASSIGN ttBrowseColumn.FieldName = SUBSTITUTE ("&1[&2]":U, ttBrowseColumn.FieldName, iExtent) .
                END .
            END.
            /* Expression */
            ELSE 
                ttBrowseColumn.Expression = TRIM (oFieldNode:firstChild():toStringFulltext()) .  
            
/*            ELSE IF ProparseHelper:HasChildNodeOfNodeType (oFieldNode, "QSTRING":U) THEN DO:     */
/*                oCurrentNode = ProparseHelper:FindChildNodeOfNodeType (oFieldNode, "QSTRING":U) .*/
/*                                                                                                 */
/*                ASSIGN ttFrameWidget.WidgetType  = "LITERAL":U                                   */
/*                       ttFrameWidget.WidgetValue = oCurrentNode:getText () .                     */
/*            END.                                                                                 */
            
            IF ProparseHelper:HasChildNodeOfNodeType (oFieldNode, "Format_phrase":U) THEN DO:
                oFormatNode = ProparseHelper:FindChildNodeOfNodeType (oFieldNode, "Format_phrase":U) .
            
                IF ProparseHelper:HasChildNodeOfNodeType (oFormatNode, "LABEL":U) THEN 
                    ttBrowseColumn.WidgetLabel = StringHelper:Unquote(ProparseHelper:FindChildNodeOfNodeType (oFormatNode, "LABEL":U):firstChild ():getText ()) .   
    
                IF ProparseHelper:HasChildNodeOfNodeType (oFormatNode, "FORMAT":U) THEN 
                    ttBrowseColumn.FieldFormat = StringHelper:Unquote(ProparseHelper:FindChildNodeOfNodeType (oFormatNode, "FORMAT":U):firstChild ():getText ()) .   

                IF ProparseHelper:HasChildNodeOfNodeType (oFormatNode, "WIDTH":U) THEN 
                    ttBrowseColumn.FieldWidth = DECIMAL(StringHelper:Unquote(ProparseHelper:FindChildNodeOfNodeType (oFormatNode, "WIDTH":U):firstChild ():getText ())) .   

                IF ProparseHelper:HasChildNodeOfNodeType (oFormatNode, "LEXAT":U) THEN DO: 
                    oLexatNode = ProparseHelper:FindChildNodeOfNodeType (oFormatNode, "LEXAT":U)  . 

                    IF ProparseHelper:HasChildNodeOfNodeType (oLexatNode, "Field_ref":U) THEN DO:
                        oAtColumnNameNode = ProparseHelper:FindChildNodeOfNodeType (oLexatNode, "Field_ref":U)  . 

                        ttBrowseColumn.ColumnName = oAtColumnNameNode:firstChild ():getText ()  .

                        FIND ttWidget WHERE ttWidget.FieldName = ttBrowseColumn.ColumnName NO-ERROR . 

                        IF AVAILABLE ttWidget THEN DO: 
                            IF ttBrowseColumn.FieldDataType = "":U AND ttWidget.FieldDataType > "":U THEN ttBrowseColumn.FieldDataType = ttWidget.FieldDataType .    
                            IF ttBrowseColumn.FieldFormat   = "":U AND ttWidget.FieldFormat   > "":U THEN ttBrowseColumn.FieldFormat   = ttWidget.FieldFormat .    
                            IF ttBrowseColumn.WidgetLabel   = "":U AND ttWidget.WidgetLabel   > "":U THEN ttBrowseColumn.WidgetLabel   = ttWidget.WidgetLabel .    
                            IF ttBrowseColumn.ListItems     = "":U AND ttWidget.ListItems     > "":U THEN ttBrowseColumn.ListItems     = ttWidget.ListItems .    
                            IF ttBrowseColumn.ListItemPairs = "":U AND ttWidget.ListItemPairs > "":U THEN ttBrowseColumn.ListItemPairs = ttWidget.ListItemPairs .    
                        END.
                        ELSE 
                            ErrorHelper:ResetErrorStatus() .       
                    END.
                END.
            
                /* VIEW-AS */
                IF ProparseHelper:HasChildNodeOfNodeType (oFormatNode, "VIEWAS":U) THEN DO:
                    oCurrentNode = ProparseHelper:FindChildNodeOfNodeType (oFormatNode, "VIEWAS":U)  .

                    oViewAsNode = oCurrentNode:firstChild () .

                    IF VALID-OBJECT (oViewAsNode) THEN DO:
                        ttBrowseColumn.ViewAs = oViewAsNode:getText ()  .
                        
                        IF ProparseHelper:HasChildNodeOfNodeType (oViewAsNode, "LISTITEMS":U) THEN 
                            ASSIGN ttBrowseColumn.ListItems = THIS-OBJECT:ParseListItems (oViewAsNode) . 
                                
                        IF ProparseHelper:HasChildNodeOfNodeType (oViewAsNode, "LISTITEMPAIRS":U) THEN 
                            ASSIGN ttBrowseColumn.ListItemPairs = THIS-OBJECT:ParseListItemPairs (oViewAsNode) . 
                        
                    END.
                END. 
            END.
            
            /* Mike Fechner, Consultingwerk Ltd. 02.12.2013
               Determine Data Dictionary Label, Format, Data-Type of Field */
            IF (ttBrowseColumn.WidgetLabel = "":U OR ttBrowseColumn.FieldFormat = "":U OR  ttBrowseColumn.FieldDataType = "":U)  
                AND NUM-ENTRIES (ttBrowseColumn.FieldName, ".":U) > 1 THEN DO ON ERROR UNDO, THROW:
                    
                oBufferFieldName = BufferHelper:ParseFieldName (ttBrowseColumn.FieldName) . 

                hBuffer = BufferHelper:CreateBuffer (oBufferFieldName) .
                
                IF VALID-HANDLE (hBuffer) THEN DO:
                    IF BufferHelper:HasField (hBuffer, oBufferFieldName:FieldName) THEN DO: 
                        hField = hBuffer:BUFFER-FIELD (oBufferFieldName:FieldName) .
                    
                        IF ttBrowseColumn.WidgetLabel = "":U THEN 
                            ASSIGN ttBrowseColumn.WidgetLabel = hField:LABEL .
                            
                        IF ttBrowseColumn.FieldFormat = "":U THEN 
                            ASSIGN ttBrowseColumn.FieldFormat = hField:FORMAT .
                         
                        IF ttBrowseColumn.FieldDataType = "":U THEN 
                            ASSIGN ttBrowseColumn.FieldDataType = UPPER(hField:DATA-TYPE) .
                    END.
                END.

                CATCH err AS Progress.Lang.Error:
                    /* ignore errors */ 
                END CATCH.
                
                FINALLY:
                    GarbageCollectorHelper:DeleteObject (hBuffer) .     
                END FINALLY.
            END.
            
            RELEASE ttBrowseColumn .
        END.

    END METHOD.

END CLASS.
