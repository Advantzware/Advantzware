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
    File        : SmartDataObjectImporterPlugin
    Purpose     : Business Entity Designer Plugin that supports to import an 
                  ADM2 SmartDataObject into the Business Entity. 
    Syntax      : 
    Description : Creates an "Import SDO" button to the Ribbon of the 
                  Business Entity Designer. Using this Button developers
                  can import an SDO onto the Design Canvas. The import process
                  imports the fields of the SDO and the Data Source information
                  (query, source tables). 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 21 20:38:49 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                 FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.*          FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.*  FROM PROPATH .  
USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.UI.*       FROM PROPATH . 
USING Consultingwerk.Framework.Collections.*           FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                  FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH . 
USING Consultingwerk.Windows.Util.Forms.*              FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*              FROM ASSEMBLY  . 
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.SmartDataObjectImporterPlugin 
    IMPLEMENTS IBusinessEntityDesignerPlugin: 

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the BusinessEntityDatasetController 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Controller AS BusinessEntityDatasetController NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Parent Form 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Form AS System.Windows.Forms.Form NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartDataObjectImporterPlugin class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartDataObjectImporterPlugin ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Searches the source code for a preprocessor definition 
        Notes:   
        @param pcPreprocessorName The preprocessor variable to search for 
        @param plcSourceCode The whole source code
        @return The value of the preprocessor name 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GetPreprocessorDefinition (pcPreprocessorName AS CHARACTER,
                                                          plcSourceCode AS LONGCHAR):
        
        DEFINE VARIABLE iCount   AS INTEGER   NO-UNDO.
        DEFINE VARIABLE i        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iTilde   AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cLine    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cPattern AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cResult  AS CHARACTER NO-UNDO.
        
        ASSIGN iCount   = NUM-ENTRIES (plcSourceCode, CHR (10))
               cPattern = "&Scoped-define ":U + pcPreprocessorName + " ":U .

        DO i = 1 TO iCount:
        
            ASSIGN cLine = STRING (TRIM (TRIM (ENTRY (i, plcSourceCode, CHR (10)), CHR(13)), " ":U)) .
              
            IF cLine BEGINS cPattern THEN DO:

                ASSIGN iTilde = 0 . 
                
                DO WHILE ASC (STRING (SUBSTRING (cLine, LENGTH (cLine), 1))) = 126 /* tilde */:

                    ASSIGN iTilde  = iTilde + 1
                           cResult = cResult + TRIM (cLine, CHR (126))   
                           cLine   = STRING (TRIM (TRIM (ENTRY (i + iTilde, plcSourceCode, CHR (10)), CHR(13)), " ":U)) .
                END.
                
                ASSIGN cResult = SUBSTRING (cResult + cLine , LENGTH (cPattern) + 1) .
                
                RETURN cResult .
            END.                
        END.

        RETURN ? .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shows the Wizzard to import an ADM2 SmartDataObject
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ImportSDO ():
        
        DEFINE VARIABLE oOpenFileDialog AS System.Windows.Forms.OpenFileDialog NO-UNDO .
        DEFINE VARIABLE oDialogResult   AS System.Windows.Forms.DialogResult   NO-UNDO . 
        
        oOpenFileDialog = NEW System.Windows.Forms.OpenFileDialog () .
        oOpenFileDialog:AddExtension = TRUE . 
        oOpenFileDialog:CheckFileExists = TRUE . 
        oOpenFileDialog:CheckPathExists = TRUE . 
        oOpenFileDialog:DefaultExt = "w":U . 
        oOpenFileDialog:DereferenceLinks = TRUE . 
        oOpenFileDialog:Filter = "AppBuilder source files (*.w)|*.w|All files (*.*)|*.*"{&TRAN} .
        oOpenFileDialog:Multiselect = FALSE . 
        oOpenFileDialog:RestoreDirectory = TRUE . 
        oOpenFileDialog:Title = "Import ADM2 SmartDataObject into model"{&TRAN} .         
        
        WAIT-FOR oOpenFileDialog:ShowDialog () SET oDialogResult .
         
        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .
        
        THIS-OBJECT:ImportSDO (oOpenFileDialog:FileName) . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Imports an ADM2 SmartDataObject into the model
        Notes:   
        @param pcSDO The name of the SmartDataObject to import
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ImportSDO (pcSDO AS CHARACTER):
        
        DEFINE VARIABLE lcSourceCode  AS LONGCHAR          NO-UNDO .
        DEFINE VARIABLE oImporter     AS TempTableImporter NO-UNDO . 
        DEFINE VARIABLE oTempTables   AS CharacterList     NO-UNDO . 
        DEFINE VARIABLE oImported     AS CharacterList     NO-UNDO .
        DEFINE VARIABLE cQueryName    AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE cTables       AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE cQueryString  AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE oDialogResult AS DialogResultEnum  NO-UNDO . 

        oTempTables = NEW CharacterList () .
        oTempTables:Add ("RowObject":U) .

        DO ON ERROR UNDO, THROW:
            oImporter = NEW TempTableImporter () .
            
            oImported = oImporter:ImportTablesFromFile (pcSDO,
                                                        oTempTables,
                                                        THIS-OBJECT:Controller) .

            FINALLY:
                GarbageCollectorHelper:DeleteObject (oImporter) .        
            END FINALLY.
        END.
        
        IF oImported:Count > 0 THEN .
        ELSE 
            RETURN .   

        COPY-LOB FROM FILE pcSDO TO lcSourceCode . 

        ASSIGN cQueryName   = THIS-OBJECT:GetPreprocessorDefinition ("QUERY-NAME":U, lcSourceCode) 
               cTables      = THIS-OBJECT:GetPreprocessorDefinition (SUBSTITUTE ("TABLES-IN-QUERY-&1":U,
                                                                                 cQueryName), lcSourceCode)
               cQueryString = THIS-OBJECT:GetPreprocessorDefinition (SUBSTITUTE ("QUERY-STRING-&1":U,
                                                                                 cQueryName), lcSourceCode)
               .
        
        FIND eTable WHERE eTable.TempTableName = oImported:GetValue (1) .  
        
        ASSIGN eTable.SourceTableNames   = REPLACE (cTables, " ":U, ",":U)
               eTable.SourceBufferNames  = REPLACE (cTables, " ":U, ",":U)
               eTable.SourceDefaultQuery = cQueryString 
               eTable.TempTablePurpose   = SUBSTITUTE ("Imported from &1"{&TRAN}, pcSDO). 
        
        THIS-OBJECT:ParseFieldSpecs (eTable.TempTableName,
                                     cQueryName,
                                     lcSourceCode) .
                                             
        oDialogResult = MessageFormHelper:AskQuestion ("Remove SDO supporting fields (RowNum, RowIdent, etc.)?"{&TRAN},
                                                       "Smart Data Object Importer":U) .

        IF oDialogResult = DialogResultEnum:DialogResultYes THEN 
            THIS-OBJECT:RemoveSdoSupportFields (eTable.TempTableName) .

        IF VALID-OBJECT (THIS-OBJECT:Controller) THEN 
            THIS-OBJECT:Controller:Refresh() .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the _QUERY-BLOCK for the field details
        Notes:   
        @param pcTableName The name of the temp-table
        @param pcQueryName The name of the Query 
        @param plcSourceCode The source code block
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ParseFieldSpecs (pcTableName AS CHARACTER,
                                           pcQueryName AS CHARACTER, 
                                           plcSourceCode AS LONGCHAR):
        
        DEFINE VARIABLE lcCodeBlock      AS LONGCHAR        NO-UNDO . 
        DEFINE VARIABLE iPos             AS INTEGER         NO-UNDO .
        DEFINE VARIABLE iPos2            AS INTEGER         NO-UNDO .
        DEFINE VARIABLE cFieldMapping    AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE cFieldProperties AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE i                AS INTEGER         NO-UNDO .
        DEFINE VARIABLE cLine            AS CHARACTER       NO-UNDO .

        DEFINE VARIABLE oTokenizer       AS Tokenizer       NO-UNDO . 
        DEFINE VARIABLE oCharacterList   AS CharacterList   NO-UNDO .
        DEFINE VARIABLE oBufferFieldName AS BufferFieldName NO-UNDO . 

        oTokenizer = NEW Tokenizer () .

        ASSIGN iPos  = INDEX (plcSourceCode, 
                              SUBSTITUTE ("&&ANALYZE-SUSPEND _QUERY-BLOCK QUERY &1":U, pcQueryName))
               iPos2 = INDEX (plcSourceCode, 
                              "&ANALYZE-RESUME":U, iPos) .

        ASSIGN lcCodeBlock = SUBSTRING (plcSourceCode, iPos, iPos2 - iPos) .
                      
        fieldspecloop: DO i = 1 TO NUM-ENTRIES (lcCodeBlock, CHR (10)):
            ASSIGN cLine = TRIM (ENTRY (i, lcCodeBlock, CHR (10)), CHR (13)) .             
            
            IF cLine BEGINS "     _FldNameList":U THEN DO:
                ASSIGN cFieldMapping    = TRIM (SUBSTRING (cLine, INDEX (cLine, ">":U) + 1)) 
                       cFieldProperties = TRIM (ENTRY (i + 1, lcCodeBlock, CHR (10)), CHR (13)) 
                       
                       oCharacterList   = oTokenizer:Tokenize (cFieldProperties) .
                
                FIND eField WHERE eField.TempTableName = pcTableName 
                              AND eField.FieldName     = oCharacterList:GetValue (2) NO-ERROR .  
                
                IF NOT AVAILABLE eField THEN DO:
                    MessageFormHelper:ShowMessage (SUBSTITUTE ("Unable to parse field specification:~n&1"{&TRAN},
                                                               cLine),
                                                   "Smart Data Object Importer":U,
                                                   MessageFormImages:ImageWarning) . 
                    
                    NEXT fieldspecloop . 
                END. 
                
                IF cFieldMapping = "~"_<CALC>~"":U THEN 
                    ASSIGN eField.FieldCalcExpression = oCharacterList:GetValue (1) .
                ELSE DO:
                    ASSIGN oBufferFieldName = BufferHelper:ParseFieldName (cFieldMapping) .

                    IF oBufferFieldName:TableName > "":U THEN DO:
                        IF oBufferFieldName:Index > 0 THEN 
                            ASSIGN eField.FieldSource = SUBSTITUTE ("&1.&2[&3]":U,
                                                                    oBufferFieldName:TableName,
                                                                    oBufferFieldName:FieldName,
                                                                    oBufferFieldName:Index) .
                        ELSE 
                            ASSIGN eField.FieldSource = SUBSTITUTE ("&1.&2":U,
                                                                    oBufferFieldName:TableName,
                                                                    oBufferFieldName:FieldName) .
                    END.
                END.  
            END.
        END.                      

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oTokenizer) .
            GarbageCollectorHelper:DeleteObject (oCharacterList) .
        END FINALLY.

    END METHOD .

	/*------------------------------------------------------------------------------
	   Purpose: Removes the SDO support fields and indexes from the design
	   Notes:
	   @param pcTableName The name of the imported temp-table
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID RemoveSdoSupportFields (pcTableName AS CHARACTER):
		
		DEFINE VARIABLE cFields  AS CHARACTER NO-UNDO INIT "RowNum,RowIdent,RowMod,RowIdentIdx,RowUserProp":U.
        DEFINE VARIABLE cIndexes AS CHARACTER NO-UNDO INIT "RowNum,RowMod,RowIdentIdx":U.

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DEFINE BUFFER b_eField FOR eField . 
        DEFINE BUFFER b_eIndex FOR eIndex . 

        DO i = 1 TO NUM-ENTRIES (cFields):
            FIND b_eField WHERE b_eField.TempTableName = pcTableName 
                            AND b_eField.FieldName     = ENTRY (i, cFields) NO-ERROR . 
                            
            IF AVAILABLE b_eField THEN 
                DELETE b_eField . 
        END.
    
        DO i = 1 TO NUM-ENTRIES (cIndexes):
            FIND b_eIndex WHERE b_eIndex.TempTableName = pcTableName 
                            AND b_eIndex.IndexName     = ENTRY (i, cIndexes) NO-ERROR . 
                            
            IF AVAILABLE b_eIndex THEN 
                DELETE b_eIndex . 
        END.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Shuts down the plugin                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Shutdown ():

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the plugin                                                                      
        Notes:                      
        @param dsBusinessEntity Business Entity with the Design dataset, called with the BIND keyword
        @param poController The BusinessEntityDatasetController
        @param poForm The main Form of the BusinessEntityDesigner
        @param poToolbar The reference to the UltraToolbarsManager used by the BusinessEntityDesigner                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Startup (DATASET FOR dsBusinessEntity BIND, 
                                poController AS BusinessEntityDatasetController, 
                                poForm AS BusinessEntityDesignerForm, 
                                poToolbar AS UltraToolbarsManager):
        
        DEFINE VARIABLE oGenerarteViewer AS ButtonTool  NO-UNDO .
        DEFINE VARIABLE oGroup           AS RibbonGroup NO-UNDO .
        
        THIS-OBJECT:Form = poForm . 
        THIS-OBJECT:Controller = poController . 
        
        oGenerarteViewer = NEW ButtonTool ("SmartDataObjectImporter_Plugin_ImportSDO":U) .
        oGenerarteViewer:SharedProps:Caption = "Import SDO"{&TRAN} .
        oGenerarteViewer:SharedProps:ToolTipText = "Import ADM2 SmartDataObject"{&TRAN} .
        oGenerarteViewer:SharedProps:DisplayStyle = ToolDisplayStyle:ImageAndText .
        poToolbar:Tools:Add (oGenerarteViewer) .        

        FILE-INFO:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/text_binary_into_16.png":U .        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            ASSIGN oGenerarteViewer:SharedProps:AppearancesSmall:Appearance:Image = System.Drawing.Image:FromFile 
                        (FILE-INFO:FULL-PATHNAME)  .        

        FILE-INFO:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/text_binary_into_32.png":U .        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            ASSIGN oGenerarteViewer:SharedProps:AppearancesLarge:Appearance:Image = System.Drawing.Image:FromFile 
                        (FILE-INFO:FULL-PATHNAME)  .        
        
        oGroup = poToolbar:Ribbon:Tabs [0]:Groups ["Design":U] .
  
        oGroup:Tools:AddTool ("SmartDataObjectImporter_Plugin_ImportSDO":U):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .
        oGroup:Tools["SmartDataObjectImporter_Plugin_ImportSDO":U]:InstanceProps:IsFirstInGroup = TRUE . 
  
        poToolbar:ToolClick:Subscribe (ToolClickHandler) .
  
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Handles the ToolClick event of the Business Entity Designer's 
                 UltraToolbarsManager                                                                         
        Notes:      
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ToolClickHandler (sender AS System.Object,
                                            e AS ToolClickEventArgs):
        
        CASE e:Tool:Key:
            WHEN "SmartDataObjectImporter_Plugin_ImportSDO":U THEN
                ImportSDO () .
        END.
        
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
    END METHOD.

END CLASS.
