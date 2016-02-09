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
    File        : CalcExpressionProcessor
    Purpose     : Allows manipulation of calculated field expressions
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 02 08:48:53 GMT+01:00 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING com.joanju.proparse.NodeTypes              FROM ASSEMBLY .
USING Consultingwerk.*                           FROM PROPATH . 
USING Consultingwerk.Studio.LegacyGuiMigration.* FROM PROPATH .  
USING Consultingwerk.Studio.Proparse.*           FROM PROPATH .
USING Consultingwerk.Studio.SmartRefactor.*      FROM PROPATH .
USING Consultingwerk.Util.*                      FROM PROPATH .
USING org.prorefactor.core.JPNode                FROM ASSEMBLY .
USING org.prorefactor.nodetypes.*                FROM ASSEMBLY . 
USING org.prorefactor.treeparser.*               FROM ASSEMBLY .
USING Progress.Lang.*                            FROM PROPATH .

CLASS Consultingwerk.Studio.LegacyGuiMigration.CalcExpressionProcessor: 

    DEFINE VARIABLE hBusinessEntityDataset AS HANDLE NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Returns the next available field order value for the given temp-table   
        Notes:   
        @param pcTempTableName The temp-table name of return the field order for
        @return The next availalbe field  order value for the given temp-table
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED INTEGER NextFieldOrder (pcTempTableName AS CHARACTER):
		
        hBusinessEntityDataset::eField:FIND-LAST (SUBSTITUTE ("where TempTableName = &1 use-index FieldOrder":U, 
                                                              QUOTER (pcTempTableName))) NO-ERROR . 
            
        IF hBusinessEntityDataset::eField THEN 
            RETURN hBusinessEntityDataset::eField::FieldOrder .
        ELSE 
            RETURN 1 . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Processes a calculated field expression 
        Notes:   Replaces database field references in the expression with the mapped
                 temp-tables
        @param pcExpression The original calculated field expression
        @param phBusinessEntityDataset The dataset with the Business Entity design
        @return The modified calculated field expression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ProcessCalculatedFieldExpression (pcExpression AS CHARACTER, 
                                                              phBusinessEntityDataset AS HANDLE):
    
        DEFINE VARIABLE oParseUnit   AS ParseUnit       NO-UNDO . 
        DEFINE VARIABLE cFileName    AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE cFileName2   AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE oRefactor    AS RefactorSession NO-UNDO . 
        DEFINE VARIABLE lcCode       AS LONGCHAR        NO-UNDO .
        
        ASSIGN cFileName              = FileHelper:GetTempFileName() 
               cFileName2             = FileHelper:GetTempFileName() 
               hBusinessEntityDataset = phBusinessEntityDataset .
        
        hBusinessEntityDataset::eBusinessEntity:FIND-FIRST () . 
        
        OUTPUT TO VALUE (cFileName) .
        PUT UNFORMATTED pcExpression SKIP . 
        OUTPUT CLOSE . 
        
        oParseUnit = ProparseHelper:ParseFile (cFileName) .
    
        oRefactor = NEW RefactorSession (oParseUnit) . 

        THIS-OBJECT:WalkAst (oParseUnit:getTopNode():firstChild(), 
                             oRefactor) .
    
        oRefactor:SAVE (cFileName2)  .
    
        COPY-LOB FROM FILE cFileName2 TO lcCode .
        
        RETURN STRING (lcCode) .
    
        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.      

        FINALLY:
            OS-DELETE VALUE (cFileName) .       
            OS-DELETE VALUE (cFileName2) .       
        END FINALLY.
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the temp-table which is mapping the given source 
                 (DB) table 
        Notes:   
        @param pcDbTableName The database tale name 
        @return The temp-table name that maps the 
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED CHARACTER TempTableForSourceTable (pcDbTableName AS CHARACTER):
		
		DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.

        hQuery = QueryHelper:CreatePreparedQuery (hBusinessEntityDataset::eTable) .
        
        DO WHILE NOT hQuery:QUERY-OFF-END:
            IF ListHelper:EntryIsInList (pcDbTableName, hBusinessEntityDataset::eTable::SourceTableNames) THEN 
                RETURN hBusinessEntityDataset::eTable::TempTableName.
            
            hQuery:GET-NEXT () . 
        END.

        RETURN ? . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Recursively walks the AST
        Notes:   Registers rewrites of field names in the RefactorSession
        @param poNode The current JPNode
        @param poRefactor The refactor session to register rewrites in
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID WalkAst (poNode AS JPNode,
	                               poRefactor AS RefactorSession):
		
		DEFINE VARIABLE oFieldRefNode AS FieldRefNode    NO-UNDO . 
        DEFINE VARIABLE oField        AS BufferFieldName NO-UNDO .
        DEFINE VARIABLE hFieldBuffer  AS HANDLE          NO-UNDO .
        DEFINE VARIABLE hBuffer       AS HANDLE          NO-UNDO .
        DEFINE VARIABLE cSourceField  AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE cTempTable    AS CHARACTER       NO-UNDO .

        hFieldBuffer = hBusinessEntityDataset:GET-BUFFER-HANDLE ("eField":U) . 
               
        CREATE BUFFER hFieldBuffer FOR TABLE hFieldBuffer .                
               
        DO WHILE VALID-OBJECT (poNode):
            
            IF TYPE-OF (poNode, FieldRefNode) THEN DO:
            
                ASSIGN oFieldRefNode = CAST (poNode, FieldRefNode) 
                       oField        = ProparseHelper:GetBufferFieldName (oFieldRefNode) .
                       
                IF oField:TableName > "":U THEN DO:                               
        
                    ASSIGN cSourceField = "":U . 
        
                    IF oField:Index > 0 THEN DO:
                        hFieldBuffer:FIND-FIRST (SUBSTITUTE ("where FieldSource = &1":U, QUOTER (oField:TableName + ".":U + oField:FieldName + "[":U + STRING (oField:Index) + "]":U))) NO-ERROR .
                        
                        IF hFieldBuffer:AVAILABLE THEN 
                            ASSIGN cSourceField = hFieldBuffer::FieldSource .   
                        ELSE DO:
                            hFieldBuffer:FIND-FIRST (SUBSTITUTE ("where FieldSource = &1":U, QUOTER (oField:TableName + ".":U + oField:FieldName))) NO-ERROR .
                            
                            IF hFieldBuffer:AVAILABLE THEN 
                                ASSIGN cSourceField = hFieldBuffer::FieldSource + "[":U + STRING (oField:Index) + "]":U.   
                        END.
                    END. 
                    ELSE DO:
                        hFieldBuffer:FIND-FIRST (SUBSTITUTE ("where FieldSource = &1":U, QUOTER (oField:TableName + ".":U + oField:FieldName))) NO-ERROR .
                        
                        IF hFieldBuffer:AVAILABLE THEN 
                            ASSIGN cSourceField = hFieldBuffer::FieldSource .   
                    END.

                    IF cSourceField = "":U THEN DO ON ERROR UNDO, THROW:
                        
                        ASSIGN hBuffer    = BufferHelper:CreateBuffer (oField) 
                               cTempTable = THIS-OBJECT:TempTableForSourceTable (oField:TableName).
                        
                        IF cTempTable <> "":U THEN DO: 
                        
                            hFieldBuffer:BUFFER-CREATE () .

                            ASSIGN hFieldBuffer::BusinessEntityName       = hBusinessEntityDataset::eBusinessEntity::BusinessEntityName 
                                   hFieldBuffer::TempTableName            = cTempTable
                                   hFieldBuffer::FieldName                = oField:FieldName
                                   hFieldBuffer::FieldOrder               = THIS-OBJECT:NextFieldOrder (cTempTable)
                                   hFieldBuffer::FieldDataType            = hBuffer:BUFFER-FIELD (oField:FieldName):DATA-TYPE
                                   hFieldBuffer::FieldInitial             = hBuffer:BUFFER-FIELD (oField:FieldName):DEFAULT-STRING
                                   hFieldBuffer::FieldInitialUnknownValue = (hBuffer:BUFFER-FIELD (oField:FieldName):DEFAULT-VALUE = ?)
                                   hFieldBuffer::FieldLabel               = hBuffer:BUFFER-FIELD (oField:FieldName):LABEL
                                   hFieldBuffer::FieldFormat              = hBuffer:BUFFER-FIELD (oField:FieldName):FORMAT
                                   hFieldBuffer::FieldSource              = oField:GetExpression() 
                                   hFieldBuffer::FieldExtent              = 0
                                   .
                                   
                             IF oField:Index > 0 THEN 
                                hFieldBuffer::FieldName = hFieldBuffer::FieldName + STRING (oField:INDEX, 
                                                                                            FormatHelper:IntegerFormatForMax (hBuffer:BUFFER-FIELD (oField:FieldName):EXTENT)) .
                                                                                            
                             ASSIGN cSourceField = SUBSTITUTE ("&1.&2":U, 
                                                               hFieldBuffer::TempTableName,
                                                               hFieldBuffer::FieldName) .                                                                                              
                        END.
                        
                        FINALLY:
                            GarbageCollectorHelper:DeleteObject (hBuffer) .		
                        END FINALLY.
                    END.             
    
                    IF cSourceField > "":U THEN DO:
                        poRefactor:ReplaceNodeContent (oFieldRefNode:getIdNode(), cSourceField) .
    
                        IF ProparseHelper:HasChildNodeOfNodeType (oFieldRefNode, "Array_subscript":U) THEN DO:
                            poRefactor:ReplaceNodeContent (ProparseHelper:GetChildNodeByPath(oFieldRefNode, "Array_subscript~\LEFTBRACE":U), "":U) .
                            poRefactor:ReplaceNodeContent (ProparseHelper:GetChildNodeByPath(oFieldRefNode, "Array_subscript~\NUMBER":U), "":U) .
                            poRefactor:ReplaceNodeContent (ProparseHelper:GetChildNodeByPath(oFieldRefNode, "Array_subscript~\RIGHTBRACE":U), "":U) .
                        END.
                    END.
                END.
            END.
    
            IF VALID-OBJECT (poNode:firstChild()) THEN 
                THIS-OBJECT:WalkAst (poNode:firstChild(), poRefactor).
        
            poNode = poNode:nextSibling () .
        END.
        
        FINALLY:
            DELETE OBJECT hFieldBuffer .		
        END FINALLY.

	END METHOD . 

END CLASS.
