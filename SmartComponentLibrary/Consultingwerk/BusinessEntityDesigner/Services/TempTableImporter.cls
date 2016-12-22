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
    File        : TempTableImporter
    Purpose     : Imports a temp-table into the Business Entity model
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 19 21:16:20 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING com.joanju.proparse.NodeTypes                    FROM ASSEMBLY .
USING Consultingwerk.*                                 FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.*          FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH .  
USING Consultingwerk.BusinessEntityDesigner.UI.*       FROM PROPATH .
USING Consultingwerk.Framework.Collections.*           FROM PROPATH . 
USING Consultingwerk.Studio.Proparse.*                 FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH . 
USING org.prorefactor.core.JPNode                      FROM ASSEMBLY .
USING org.prorefactor.treeparser.*                     FROM ASSEMBLY .
USING Progress.Lang.*                                  FROM PROPATH .
USING System.Windows.Forms.*                           FROM ASSEMBLY . 
USING Progress.Util.* FROM ASSEMBLY.

CLASS Consultingwerk.BusinessEntityDesigner.Services.TempTableImporter: 

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i }

    /* Temp-Tables used in RefreshTableFromFile */
    DEFINE TEMP-TABLE tt_eField NO-UNDO LIKE eField
        FIELD FieldPresentInProparse AS LOGICAL INIT FALSE . 
    DEFINE TEMP-TABLE tt_eIndex NO-UNDO LIKE eIndex . 

    DEFINE VARIABLE iPositionOffset  AS INTEGER       NO-UNDO .
    DEFINE VARIABLE oProcessedTables AS CharacterList NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Searches the ParseUnit for Temp-Table Definitions and returns a 
                 List of Temp-Table names
        Notes:   
        @param poParseUnit The proparse ParseUnit containing the source
        @return The CharacterList with the located file names 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CharacterList FindTempTables (poParseUnit AS ParseUnit):
        
        DEFINE VARIABLE oList       AS CharacterList      NO-UNDO . 
        DEFINE VARIABLE oNodeWalker AS TempTableAstWalker NO-UNDO . 
                
        oList = NEW CharacterList () .
        
        oNodeWalker = NEW TempTableAstWalker () .         
        oNodeWalker:TempTableAstNodeAvailable:Subscribe (FindTableTablesHandler) .    
        oNodeWalker:ProcessAst (poParseUnit, ?, oList) .
        
        RETURN oList .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TempTableAstNodeAvailable event of the 
                 TempTableAstWalker during the searching for temp-table names
        Notes:   
        @param sender The object that raised the event
        @param e The TempTabelAstNodeAvailableEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FindTableTablesHandler (sender AS Progress.Lang.Object,
                                                  e AS TempTableAstNodeAvailableEventArgs):
        
        e:List:Add (e:TableName) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the value of a FIELD attribute
        Notes:   
        @param poNode The JPNode of the FIELD
        @param pcAttributeName The Name of the Attribute (Node Type)
        @return The attribute value (CHARACTER)
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GetFieldAttributeValue (poNode AS JPNode,
                                                       pcAttributeName AS CHARACTER):
        
        DEFINE VARIABLE cValue         AS CHARACTER NO-UNDO .
        DEFINE VARIABLE oAttributeNode AS JPNode    NO-UNDO .
        DEFINE VARIABLE iIndex         AS INTEGER   NO-UNDO .
    
        oAttributeNode = ProparseHelper:FindChildNodeOfNodeType (poNode, pcAttributeName) .

        IF NOT VALID-OBJECT (oAttributeNode) OR NOT VALID-OBJECT (oAttributeNode:firstChild()) THEN 
            RETURN ? .

        ASSIGN cValue = TRIM(oAttributeNode:firstChild():getText ()) .

        IF cValue BEGINS "~"":U THEN DO:
            iIndex = R-INDEX (cValue, "~"":U) .
            
            IF iIndex > 2 THEN 
                cValue = SUBSTRING (cValue, 2, iIndex - 2) .
        END.
        ELSE IF cValue BEGINS "'":U THEN DO:
            iIndex = R-INDEX (cValue, "'":U) .
            
            IF iIndex > 2 THEN 
                cValue = SUBSTRING (cValue, 2, iIndex - 2) .            
        END.

        RETURN cValue.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Imports Temp-Tables from an ABL code block into the model of the 
                 Business Entity Dataset Controller
        Notes:   
        @param plcCodeBlock The code block to inport
        @param poController The BusinessEntityDatasetController instance
        @return The CharacterList of imported tables
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CharacterList ImportTablesFromCodeBlock (plcCodeBlock AS LONGCHAR,
                                                           poController AS BusinessEntityDatasetController):
        
        DEFINE VARIABLE oParseUnit       AS ParseUnit       NO-UNDO . 
        DEFINE VARIABLE oImportedTables  AS CharacterList   NO-UNDO . 
        
        ASSIGN iPositionOffset = 0 . 
        
        DO ON ERROR UNDO, THROW:
            ProparseHelper:SetProparseEnvironment() . 
            ProparseHelper:ExportDatabaseSchema () .
            ProparseHelper:Initialize() .

            oParseUnit = ProparseHelper:ParseString (plcCodeBlock) .

            CATCH err AS Progress.Lang.Error :
                UNDO, THROW NEW Consultingwerk.Exceptions.Exception (err, 
                                                                     "Unable to parse the code block."{&TRAN},
                                                                     0) .   
            END CATCH.

        END.

        ImportTablesFromParseUnit (oParseUnit, ?, poController, OUTPUT oImportedTables) .

        RETURN oImportedTables .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Imports Temp-Tables from an ABL source code file into the model of the 
                 Business Entity Dataset Controller
        Notes:   
        @param pcFile The filename to import
        @param poTempTableList The optional CharacterList that contains the list of Tables to import
        @param poController The BusinessEntityDatasetController instance
        @return The CharacterList of imported tables
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CharacterList ImportTablesFromFile (pcFile AS CHARACTER,
                                                      poTempTableList AS CharacterList,
                                                      poController AS BusinessEntityDatasetController):
        
        DEFINE VARIABLE oParseUnit       AS ParseUnit       NO-UNDO . 
        DEFINE VARIABLE oImportedTables  AS CharacterList   NO-UNDO . 
        
        ASSIGN iPositionOffset = 0 . 
        
        DO ON ERROR UNDO, THROW:
            ProparseHelper:SetProparseEnvironment() . 
            ProparseHelper:ExportDatabaseSchema () .
            ProparseHelper:Initialize() .

            oParseUnit = ProparseHelper:ParseFile (pcFile) .

            CATCH err AS Progress.Lang.Error :
                UNDO, THROW NEW Consultingwerk.Exceptions.Exception (err, 
                                                                     SUBSTITUTE ("Unable to parse the source file ~"&1~""{&TRAN}, 
                                                                                 pcFile),
                                                                     0) .   
            END CATCH.

        END.

        THIS-OBJECT:ImportTablesFromParseUnit (oParseUnit, poTempTableList, poController, OUTPUT oImportedTables) .

        RETURN oImportedTables .

    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Imports Temp-Tables from an ABL source code file into the model of the 
                 Business Entity Dataset Controller
        Notes:   
        @param pcFile The filename to import
        @param poController The BusinessEntityDatasetController instance
        @return The CharacterList of imported tables
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CharacterList ImportTablesFromFile (pcFile AS CHARACTER,
                                                      poController AS BusinessEntityDatasetController):
        
        RETURN THIS-OBJECT:ImportTablesFromFile (pcFile, ?, poController) . 

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Imports Temp-Tables from an ABL source code file into the model of the 
                 Business Entity Dataset Controller
        Notes:
        @param poParseUnit The reference to the ParseUnit
        @param poTempTableList The optional CharacterList that contains the list of Tables to import
        @param poController The BusinessEntityDatasetController instance
        @param poImportedTables OUTPUT The reference to the CharacterList of imported tables
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ImportTablesFromParseUnit (poParseUnit AS ParseUnit,
                                                  poTempTableList AS CharacterList,
                                                  poController AS BusinessEntityDatasetController,
                                                  OUTPUT poImportedTables AS CharacterList):
        
        DEFINE VARIABLE oTempTables      AS CharacterList   NO-UNDO . 
        DEFINE VARIABLE oTableImportForm AS TableImportForm NO-UNDO . 
        DEFINE VARIABLE oDialogResult    AS DialogResult    NO-UNDO .
        DEFINE VARIABLE cNewTableName    AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE cPrefix          AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE cPrefix2         AS CHARACTER       NO-UNDO .
        
        oTempTables = THIS-OBJECT:FindTempTables (poParseUnit) . 

        IF oTempTables:COUNT = 0 THEN 
            UNDO, THROW NEW AppError ("No temp-tables found in the input file."{&TRAN}, 0) .

        IF VALID-OBJECT (poTempTableList) THEN 
            oTempTables = poTempTableList .             
        ELSE DO:
            oTableImportForm = NEW TableImportForm () .
            oTableImportForm:SetTables (oTempTables) .
            
            oTableImportForm:Owner = poController:BusinessEntityViewerControl:FindForm () .
            
            WAIT-FOR oTableImportForm:ShowDialog () 
                SET oDialogResult . 
    
            {Consultingwerk/check-dialogresult-ok.i oDialogResult} .
    
            oTempTables = oTableImportForm:SelectedTables .
        END.
        
        DO ON ERROR UNDO, THROW:
            ASSIGN cPrefix  = poController:DatasetHandle::eBusinessEntity::DefaultTablePrefix
                   cPrefix2 = poController:DatasetHandle::eBusinessEntity::DefaultBeforeTablePrefix
                   poController:DatasetHandle::eBusinessEntity::DefaultTablePrefix       = "":U 
                   poController:DatasetHandle::eBusinessEntity::DefaultBeforeTablePrefix = "":U. 
    
            oProcessedTables = NEW CharacterList () .                        
            poImportedTables = oProcessedTables .
                        
            THIS-OBJECT:ProcessSelectedTables (oTempTables,
                                               poParseUnit, 
                                               poController) .
    
            FINALLY:
                poController:DatasetHandle::eBusinessEntity::DefaultTablePrefix       = cPrefix .    
                poController:DatasetHandle::eBusinessEntity::DefaultBeforeTablePrefix = cPrefix2.           
            END FINALLY.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Processes a Field Node
        Notes:   
        @param poNode The JPNode pointing to the FIELD definition
        @param pcTable The current temp-table name
        @param piIndex The Field index
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ProcessFieldNode (poNode AS JPNode,
                                            pcTable AS CHARACTER, 
                                            piIndex AS INTEGER):
        
        DEFINE VARIABLE oSubNode      AS JPNode          NO-UNDO . 
        DEFINE VARIABLE oFieldRefNode AS JPNode          NO-UNDO . 
        DEFINE VARIABLE oIdNode       AS JPNode          NO-UNDO . 
        DEFINE VARIABLE oArrayNode    AS JPNode          NO-UNDO . 
        DEFINE VARIABLE cValue        AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE lValue        AS LOGICAL         NO-UNDO .
        DEFINE VARIABLE oBufferField  AS BufferFieldName NO-UNDO .
        DEFINE VARIABLE hBuffer       AS HANDLE          NO-UNDO . 
        DEFINE VARIABLE hBufferField  AS HANDLE          NO-UNDO .
        DEFINE VARIABLE oInitialNode  AS JPNode          NO-UNDO . 

        DEFINE VARIABLE hLikeBuffer   AS HANDLE NO-UNDO.

        CREATE eField.
        ASSIGN eField.TempTableName = pcTable
               eField.FieldOrder    = piIndex .
        
        oSubNode = ProparseHelper:FindChildNodeOfNodeType (poNode, "ID":U) .
        
        IF VALID-OBJECT (oSubNode) THEN 
            eField.FieldName = oSubNode:getText () .

        /* LIKE Definition */
        oSubNode = ProparseHelper:FindChildNodeOfNodeType (poNode, "LIKE":U) .
        
        IF VALID-OBJECT (oSubNode) THEN DO:
            ASSIGN oFieldRefNode = ProparseHelper:FindChildNodeOfNodeType (oSubNode, "Field_ref":U) .
    
            IF VALID-OBJECT (oFieldRefNode) THEN DO:
                ASSIGN oIdNode = ProparseHelper:FindChildNodeOfNodeType (oFieldRefNode, "ID":U) .               
                
                IF VALID-OBJECT (oIdNode) THEN DO ON ERROR UNDO, THROW:

                    oBufferField = BufferHelper:ParseFieldName (oIdNode:getText ()) .

                    hBuffer = BufferHelper:BufferHandle (oBufferField) .

                    IF VALID-HANDLE (hBuffer) AND BufferHelper:HasField (hBuffer, oBufferField:FieldName) THEN DO:
                        ASSIGN hBufferField = hBuffer:BUFFER-FIELD (oBufferField:FieldName) 
                    
                               eField.FieldDataType = hBufferField:DATA-TYPE
                               eField.FieldInitial  = hBufferField:DEFAULT-VALUE
                               eField.FieldLabel    = hBufferField:LABEL 
                               eField.FieldFormat   = hBufferField:FORMAT  
                               eField.FieldExtent   = hBufferField:EXTENT      
                            .
                    END.

                    /* Mike Fechner, Consultingwerk Ltd. 28.09.2012
                       When the LIKE definition was LIKE an individual array element, don't 
                       set the EXTENT value */
                    ASSIGN oArrayNode = ProparseHelper:FindChildNodeOfNodeType (oFieldRefNode, "Array_subscript":U) .  

                    IF VALID-OBJECT (oArrayNode) THEN 
                        eField.FieldExtent = 0 . 

                    FINALLY:
                        GarbageCollectorHelper:DeleteObject (hBuffer) .        
                    END FINALLY.
                    
                END.
            END. 
        END.       
            
        oSubNode = ProparseHelper:FindChildNodeOfNodeType (poNode, "AS":U) .
        
        IF VALID-OBJECT (oSubNode) THEN DO:
            eField.FieldDataType = oSubNode:firstChild ():getText () .

            IF eField.FieldDataType = "PROGRESS.LANG.OBJECT":U THEN 
                eField.FieldDataType = "PROGRESS.LANG.OBJECT":U .
        END.

        /* Extent */
        ASSIGN cValue = THIS-OBJECT:GetFieldAttributeValue (poNode, "EXTENT":U) .        
        
        IF cValue > "":U THEN 
            eField.FieldExtent = INTEGER (cValue) NO-ERROR  . 
        
        /* Format */
        ASSIGN cValue = THIS-OBJECT:GetFieldAttributeValue (poNode, "FORMAT":U) .        
        
        IF cValue > "":U THEN 
            eField.FieldFormat = cValue . 
        
        /* Init */
        ASSIGN cValue = THIS-OBJECT:GetFieldAttributeValue (poNode, "INITIAL":U) .        
        
        /* Init ? */
        ASSIGN lValue       = FALSE 
               oInitialNode = ProparseHelper:FindChildNodeOfNodeType (poNode, "INITIAL":U) .
        
        IF VALID-OBJECT (oInitialNode) THEN 
            ASSIGN lValue = ProparseHelper:HasChildNodeOfNodeType (oInitialNode, "UNKNOWNVALUE":U) . 

        IF lValue = TRUE THEN DO:
            ASSIGN eField.FieldInitialUnknownValue = TRUE 
                   eField.FieldInitial             = "":U . 
        END. 
        ELSE IF cValue > "":U THEN 
                eField.FieldInitial = cValue . 
        
        /* Label */
        ASSIGN cValue = THIS-OBJECT:GetFieldAttributeValue (poNode, "LABEL":U) .        
        
        IF cValue > "":U THEN 
            eField.FieldLabel = cValue . 

        /* Label */
        ASSIGN cValue = THIS-OBJECT:GetFieldAttributeValue (poNode, "LABEL":U) .        
        
        IF cValue > "":U THEN 
            eField.FieldLabel = cValue . 

        /* SerializeName */
        ASSIGN cValue = THIS-OBJECT:GetFieldAttributeValue (poNode, "SERIALIZENAME":U) .        
        
        IF cValue > "":U THEN 
            eField.FieldSerializeName = cValue . 

        /* XML Data Type */
        ASSIGN cValue = THIS-OBJECT:GetFieldAttributeValue (poNode, "XMLDATATYPE":U) .        
        
        IF cValue > "":U THEN 
            eField.FieldXmlDataType = cValue . 

        /* XML Node Type */
        ASSIGN cValue = THIS-OBJECT:GetFieldAttributeValue (poNode, "XMLNODETYPE":U) .        
        
        IF cValue > "":U THEN 
            eField.FieldXmlNodeType = cValue . 

        /* XML Node Name */
        ASSIGN cValue = THIS-OBJECT:GetFieldAttributeValue (poNode, "XMLNODENAME":U) .        
        
        IF cValue > "":U THEN 
            eField.FieldXmlNodeName = cValue . 

        /* Serialize Hidden */    
        IF VALID-OBJECT (ProparseHelper:FindChildNodeOfNodeType (poNode, "SERIALIZEHIDDEN":U)) THEN 
            eField.FieldSerializeHidden = TRUE . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Processes an Index Node
        Notes:   
        @param poNode The JPNode pointing to the INDEX definition
        @param pcTable The current temp-table name
        @param piIndex The Index index (sequence)
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ProcessIndexNode (poNode AS JPNode,
                                            pcTable AS CHARACTER, 
                                            piIndex AS INTEGER):
        
        DEFINE VARIABLE oSubNode AS JPNode    NO-UNDO . 
        DEFINE VARIABLE cValue   AS CHARACTER NO-UNDO.

        CREATE eIndex.
        ASSIGN eIndex.TempTableName = pcTable
               eIndex.IndexOrder    = piIndex .
               
        oSubNode = ProparseHelper:FindChildNodeOfNodeType (poNode, "ID":U) .

        IF VALID-OBJECT (oSubNode) THEN
            eIndex.IndexName = oSubNode:getText () .

        IF VALID-OBJECT (ProparseHelper:FindChildNodeOfNodeType (poNode, "UNIQUE":U)) THEN
            eIndex.IndexUnique = TRUE .

        IF VALID-OBJECT (ProparseHelper:FindChildNodeOfNodeType (poNode, "PRIMARY":U)) THEN
            eIndex.IndexPrimary = TRUE .

        IF VALID-OBJECT (ProparseHelper:FindChildNodeOfNodeType (poNode, "WORDINDEX":U)) THEN
            eIndex.IndexWordIndex = TRUE .


        oSubNode = oSubNode:nextSibling () .

        DO WHILE VALID-OBJECT (oSubNode):
            IF NodeTypes:getTypeName (oSubNode:getType()) = "ID":U THEN DO:

                eIndex.FieldNames = eIndex.FieldNames + ",":U + oSubNode:getText () .

                IF VALID-OBJECT (oSubNode:nextSibling()) AND NodeTypes:getTypeName (oSubNode:nextSibling():getType()) = "DESCENDING":U THEN
                    eIndex.FieldSort = eIndex.FieldSort + ",DESCENDING":U .
                ELSE
                    eIndex.FieldSort = eIndex.FieldSort + ",ASCENDING":U .
            END.

            oSubNode = oSubNode:nextSibling () .
        END.

        ASSIGN eIndex.FieldNames = TRIM (eIndex.FieldNames, ",":U)
               eIndex.FieldSort  = TRIM (eIndex.FieldSort, ",":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Processes the selected Tables from the AST and creates them in the 
                 Business Entity Dataset Controller
        Notes:   
        @param poTempTables The ChracterList with the selected temp-tables
        @param poParseUnit The reference to the Proparse AST
        @param poController The reference to the BusinessEntityDatasetController
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ProcessSelectedTables (poTempTables AS CharacterList,
                                                 poParseUnit AS ParseUnit,
                                                 poController AS BusinessEntityDatasetController):
        
        DEFINE VARIABLE oNodeWalker AS TempTableAstWalker NO-UNDO . 
                        
        oNodeWalker = NEW TempTableAstWalker () .       
        oNodeWalker:TempTableAstNodeAvailable:Subscribe (ProcessSelectedTablesHandler) .  
        oNodeWalker:ProcessAst (poParseUnit, poController, poTempTables) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TempTableAstNodeAvailable event of the 
                 TempTableAstWalker during the processing of selected temp-table names
        Notes:   
        @param sender The object that raised the event
        @param e The TempTabelAstNodeAvailableEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ProcessSelectedTablesHandler (sender AS Progress.Lang.Object,
                                                        e AS TempTableAstNodeAvailableEventArgs):

        DEFINE VARIABLE oNode  AS JPNode  NO-UNDO .
        DEFINE VARIABLE iField AS INTEGER NO-UNDO.
        DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

        IF NOT e:List:ContainsValue (e:TableName) THEN 
            RETURN . 
            
        DATASET dsBusinessEntity:EMPTY-DATASET () .

        CREATE eTable.
        ASSIGN eTable.TempTableName = e:TableName .

        oNode = e:AstNode:firstChild () .

        DO WHILE VALID-OBJECT (oNode):
            CASE NodeTypes:getTypeName (oNode:getType()):
                WHEN "FIELD":U THEN DO:
                    iField = iField + 1 .
                    ProcessFieldNode (oNode, e:TableName, iField) . 
                END.
                WHEN "INDEX":U THEN DO:
                    iIndex = iIndex + 1.
                    ProcessIndexNode (oNode, e:TableName, iIndex) .
                END.
                
            END CASE . 
            
            oNode = oNode:nextSibling () .
        END.

        ASSIGN iPositionOffset = iPositionOffset + 1 . 

        oProcessedTables:Add (e:Controller:CreateTempTable (DATASET dsBusinessEntity:HANDLE,  
                                                            e:Controller:Model,
                                                            iPositionOffset * 50, 
                                                            iPositionOffset * 50,
                                                            FALSE,
                                                            FALSE)) .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Refreshes a Temp-Table from the include file 
        Notes:   
        @param pcTableName The temp-table name to refresh
        @param dsBusinessEntity The dataset with the current business entity model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RefreshTableFromFile (pcTableName AS CHARACTER,
                                             DATASET dsBusinessEntity):
        
        DEFINE VARIABLE cSourceFolder   AS CHARACTER                 NO-UNDO INIT ".":U .
        DEFINE VARIABLE cFileName       AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE oParseUnit      AS ParseUnit                 NO-UNDO . 
        DEFINE VARIABLE oTableNode      AS JPNode                    NO-UNDO . 
        DEFINE VARIABLE oNode           AS JPNode                    NO-UNDO . 
        DEFINE VARIABLE oID             AS JPNode                    NO-UNDO . 
        DEFINE VARIABLE iField          AS INTEGER                   NO-UNDO .
        DEFINE VARIABLE iIndex          AS INTEGER                   NO-UNDO .
        DEFINE VARIABLE oRemovedFields  AS CharacterList             NO-UNDO .  
        DEFINE VARIABLE oNewFieldDialog AS NewFieldInIncludeFileForm NO-UNDO .  
        DEFINE VARIABLE oDialogResult   AS DialogResult              NO-UNDO . 

        IF BusinessEntityDesignerSettings:IncludeFilesBaseFolder > "":U THEN DO:
            FILE-INFO:FILE-NAME = BusinessEntityDesignerSettings:IncludeFilesBaseFolder .
            
            IF FILE-INFO:FULL-PATHNAME > "":U AND FILE-INFO:FILE-TYPE MATCHES "*D*":U THEN 
                ASSIGN cSourceFolder = FILE-INFO:FULL-PATHNAME + "~\":U. 
            ELSE 
                UNDO, THROW NEW AppError (SUBSTITUTE ("Source folder '&1' is not available."{&TRAN}, 
                                                      BusinessEntityDesignerSettings:SourceCodeBaseFolder),
                                          0) . 
        END.        
        
        FIND FIRST eBusinessEntity . 
        FIND eTable WHERE eTable.TempTableName = pcTableName . 
        
        IF eBusinessEntity.DistinctTempTableIncludeFiles THEN 
            ASSIGN cFileName = SUBSTITUTE ("&1\&2\&3.i":U,
                                           cSourceFolder,
                                           eTable.TempTablePath,
                                           eTable.TempTableName) .
        ELSE 
            ASSIGN cFileName = SUBSTITUTE ("&1\&2\&3.i":U,
                                           cSourceFolder,
                                           eBusinessEntity.DatasetPath,
                                           eBusinessEntity.DatasetName) .

        FILE-INFO:FILE-NAME = cFileName .
        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN . 
        ELSE 
            UNDO, THROW NEW AppError (SUBSTITUTE ("The include file ~"&1~" cannot be found."{&TRAN},
                                                  cFileName),
                                      0) .
        
        /* Parse the include file */
        ProparseHelper:SetProparseEnvironment() . 
        ProparseHelper:ExportDatabaseSchema () .
        ProparseHelper:Initialize() .

        oParseUnit = ProparseHelper:ParseFile (FILE-INFO:FULL-PATHNAME) .
        
        oTableNode = oParseUnit:getTopNode():firstChild () .

        /* Mike Fechner, Consultingwerk Ltd. 10.07.2015
           Locate the Temp-Table JPNode in the include file */
        tempTableNodeLoop: 
        DO WHILE VALID-OBJECT (oTableNode):
            IF NodeTypes:getTypeName (oTableNode:getType()) = "DEFINE":U AND   
               VALID-OBJECT (ProparseHelper:FindChildNodeOfNodeType (oTableNode, "TEMPTABLE":U)) THEN DO: 
        
                oID = ProparseHelper:FindChildNodeOfNodeType (oTableNode, "ID":U) .
        
                IF VALID-OBJECT (oID) AND oID:getText () = pcTableName THEN
                    LEAVE tempTableNodeLoop .  
            END. 

            oTableNode = oTableNode:nextSibling () . 
        END .   

        IF NOT VALID-OBJECT (oTableNode) THEN 
            UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to refresh temp-table. The include file ~"&1~" does not contain a temp-table definition for ~"&2~"."{&TRAN},
                                                  cFileName,
                                                  pcTableName), 0) . 

        EMPTY TEMP-TABLE tt_eField . 
        EMPTY TEMP-TABLE tt_eIndex . 

        FOR EACH eField WHERE eField.BusinessEntityName = eTable.BusinessEntityName
                          AND eField.TempTableName      = eTable.TempTableName:
            
            CREATE tt_eField . 
            BUFFER-COPY eField TO tt_eField .
            DELETE eField . 
        END.

        FOR EACH eIndex WHERE eIndex.BusinessEntityName = eTable.BusinessEntityName
                          AND eIndex.TempTableName      = eTable.TempTableName:

            CREATE tt_eIndex .
            BUFFER-COPY eIndex TO tt_eIndex .                               
            DELETE eIndex . 
        END.

        /* verify which tt_eField record is still available in the proparse result*/
        ASSIGN oNode          = oTableNode:firstChild () 
               oRemovedFields = NEW CharacterList () . 

        DO WHILE VALID-OBJECT (oNode):
            IF NodeTypes:getTypeName (oNode:getType()) = "FIELD":U THEN DO:
                
                oID = ProparseHelper:FindChildNodeOfNodeType (oNode, "ID":U) .
        
                IF VALID-OBJECT (oID) THEN DO:
                    FIND tt_eField WHERE tt_eField.FieldName = oID:getText () NO-ERROR . 

                    IF AVAILABLE  (tt_eField) THEN
                        ASSIGN tt_eField.FieldPresentInProparse = TRUE .  
                END.                
            END.
            
            oNode = oNode:nextSibling () .
        END.

        FOR EACH tt_eField WHERE FieldPresentInProparse = FALSE ON ERROR UNDO, THROW:
            oRemovedFields:Add (tt_eField.FieldName) .
        END . 

        /* Loop through the temp-table definition in proparse and update the fields */
        ASSIGN oNode = oTableNode:firstChild () . 

        DO WHILE VALID-OBJECT (oNode):
            CASE NodeTypes:getTypeName (oNode:getType()):
                WHEN "FIELD":U THEN DO:
                    iField = iField + 1 .
                    ProcessFieldNode (oNode, pcTableName, iField) . 

                    FIND eField WHERE eField.BusinessEntityName = "":U
                                  AND eField.TempTableName      = pcTableName
                                  AND eField.FieldOrder         = iField .

                    ASSIGN eField.BusinessEntityName = eTable.BusinessEntityName .

                    FIND tt_eField WHERE tt_eField.BusinessEntityName = eTable.BusinessEntityName 
                                     AND tt_eField.TempTableName      = pcTableName
                                     AND tt_eField.FieldName          = eField.FieldName NO-ERROR .

                                     
                    IF AVAILABLE tt_eField THEN DO:
                        ASSIGN eField.FieldSource         = tt_eField.FieldSource 
                               eField.FieldDescription    = tt_eField.FieldDescription 
                               eField.FieldGetAccess      = tt_eField.FieldGetAccess 
                               eField.FieldSetAccess      = tt_eField.FieldSetAccess 
                               eField.FieldCalcExpression = tt_eField.FieldCalcExpression 
                               eField.FieldCaseSensitive  = tt_eField.FieldCaseSensitive                                
                               .  
                               
                        RELEASE eField .           
                    END.
                    ELSE DO:
                        IF oRemovedFields:Count > 0 THEN DO:
              
                            oNewFieldDialog = NEW NewFieldInIncludeFileForm () . 
                             
                            oNewFieldDialog:NewFieldName = SUBSTITUTE ("&1.&2":U, pcTableName, eField.FieldName).
                            oNewFieldDialog:RemovedFields = oRemovedFields . 
              
                            WAIT-FOR oNewFieldDialog:ShowDialog () SET oDialogResult . 
                        
                            IF EnumHelper:AreEqual (oDialogResult, DialogResult:Cancel) THEN 
                                UNDO, THROW NEW AppError ("Cancelled on users request."{&TRAN}, 0) .
                        
                            IF oNewFieldDialog:RenamedFieldName > "":U THEN DO:
                                FIND tt_eField WHERE tt_eField.BusinessEntityName = eTable.BusinessEntityName 
                                                 AND tt_eField.TempTableName      = pcTableName
                                                 AND tt_eField.FieldName          = oNewFieldDialog:RenamedFieldName NO-ERROR .
            
                                                 
                                IF AVAILABLE tt_eField THEN DO:
                                    ASSIGN eField.FieldSource         = tt_eField.FieldSource 
                                           eField.FieldDescription    = tt_eField.FieldDescription 
                                           eField.FieldGetAccess      = tt_eField.FieldGetAccess 
                                           eField.FieldSetAccess      = tt_eField.FieldSetAccess 
                                           eField.FieldCalcExpression = tt_eField.FieldCalcExpression 
                                           eField.FieldCaseSensitive  = tt_eField.FieldCaseSensitive                                
                                           .  
                                           
                                    RELEASE eField .           
                                END.
                                
                                oRemovedFields:Remove(oRemovedFields:IndexOf (oNewFieldDialog:RenamedFieldName)) . 
                            END.
                        
                        END.
                    END.              
                END.
                WHEN "INDEX":U THEN DO:
                    iIndex = iIndex + 1.
                    ProcessIndexNode (oNode, pcTableName, iIndex) .

                    FIND eIndex WHERE eIndex.BusinessEntityName = "":U
                                  AND eIndex.TempTableName      = pcTableName
                                  AND eIndex.IndexOrder         = iIndex .

                    ASSIGN eIndex.BusinessEntityName = eTable.BusinessEntityName .

                    IF AVAILABLE eIndex THEN 
                        RELEASE eIndex . 
                END.
                
            END CASE . 
            
            oNode = oNode:nextSibling () .
        END.

        CATCH err AS Progress.Lang.Error:
            IF AVAILABLE eField THEN 
                DELETE eField . 
                
            IF AVAILABLE eIndex THEN 
                DELETE eIndex . 
            
            /* Reset eField, eIndex in case of error */
            FOR EACH eField WHERE eField.BusinessEntityName = eTable.BusinessEntityName
                              AND eField.TempTableName      = eTable.TempTableName:
                
                DELETE eField . 
            END.
    
            FOR EACH eIndex WHERE eIndex.BusinessEntityName = eTable.BusinessEntityName
                              AND eIndex.TempTableName      = eTable.TempTableName:
    
                DELETE eIndex . 
            END.

            FOR EACH tt_eField:
                CREATE eField.
                BUFFER-COPY tt_eField TO eField .
                RELEASE eIndex . 
            END.
            
            FOR EACH tt_eIndex:
                CREATE eIndex.
                BUFFER-COPY tt_eIndex TO eIndex . 
                RELEASE eIndex . 
            END.            
            
            UNDO, THROW err . 
        END CATCH.

        FINALLY:
            EMPTY TEMP-TABLE tt_eField . 
            EMPTY TEMP-TABLE tt_eIndex . 
        END FINALLY.

    END METHOD .

END CLASS.
