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
    File        : BusinessEntityDesignerSettings
    Purpose     : Maintains the settings (Options Dialog) of the 
                  Business Entity Designer
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 17 22:37:44 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.* FROM PROPATH .
USING Consultingwerk.Exceptions.*             FROM PROPATH.
USING Consultingwerk.Util.*                   FROM PROPATH.
USING Consultingwerk.Windows.Util.Forms.*     FROM PROPATH .
USING Progress.Lang.*                         FROM ASSEMBLY .

CLASS Consultingwerk.BusinessEntityDesigner.BusinessEntityDesignerSettings: 

    { Consultingwerk/BusinessEntityDesigner/eBusinessEntityDesignerSettings.i &ACCESS="PRIVATE STATIC" }

    /*------------------------------------------------------------------------------
        Purpose: Raised then the settings have changed                                                                     
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC EVENT SettingsChanged SIGNATURE VOID 
       (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs).

    DEFINE PRIVATE STATIC VARIABLE lChangingSettings AS LOGICAL NO-UNDO .
    DEFINE PRIVATE STATIC VARIABLE lChangedSettings  AS LOGICAL NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: The names of the plugin classes used by the business entity designer,
                 Seperated with the pipe character (|).                                                                     
        Notes:   Class should implement 
                 Consultingwerk.BusinessEntityDesigner.IBusinessEntityDesignerPlugin   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY BusinessEntityDesignerPlugins AS CHARACTER NO-UNDO
    GET:
        RETURN eBusinessEntityDesignerSettings.BusinessEntityDesignerPlugins .
    END GET . 
    SET (arg AS CHARACTER):
        eBusinessEntityDesignerSettings.BusinessEntityDesignerPlugins = arg .
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
    END SET. 

    /*------------------------------------------------------------------------------
        Purpose: The name of the class used to generate the actual source code                                                                     
        Notes:   Class should implement 
                 Consultingwerk.BusinessEntityDesigner.Generator.IBusinessEntityGenerator                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY BusinessEntityGenerator AS CHARACTER NO-UNDO
    GET:
        RETURN eBusinessEntityDesignerSettings.BusinessEntityGenerator .
    END GET . 
    SET (arg AS CHARACTER):
        eBusinessEntityDesignerSettings.BusinessEntityGenerator = arg .
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
    END SET. 

    /*------------------------------------------------------------------------------
        Purpose: Controls of the Business Entity Generator is supposed to compile
                 the DatasetController class
        Notes:   Useful in situation with mixed OpenEdge Releases (OE10 backend, OE11
                 GUI) 
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC STATIC PROPERTY CompileDatasetController AS LOGICAL NO-UNDO 
	GET:
        RETURN eBusinessEntityDesignerSettings.CompileDatasetController .
    END GET .
    SET (arg AS LOGICAL):
        eBusinessEntityDesignerSettings.CompileDatasetController = arg .

        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
	END SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets a custom services file to be used by the Business Entity 
                 Designer
        Notes:   Custom services will be registered in the default ServiceContainer 
                 and allow customization as an alternative to plugins 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY CustomServices AS CHARACTER NO-UNDO 
    GET:
        RETURN eBusinessEntityDesignerSettings.CustomServices .
    END GET .
    SET (arg AS CHARACTER):
        eBusinessEntityDesignerSettings.CustomServices = arg .

        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: The name of the class used to generate temp table field names for 
                 fields of db tables dragged to the design canvas                                                                     
        Notes:   Class should implement 
                 Consultingwerk.BusinessEntityDesigner.Services.IFieldNameGeneratorService                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY FieldNameGeneratorService AS CHARACTER NO-UNDO 
    GET:
        RETURN eBusinessEntityDesignerSettings.FieldNameGeneratorService .
    END GET .
    SET (arg AS CHARACTER):
        eBusinessEntityDesignerSettings.FieldNameGeneratorService = arg .

        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
    END SET. 

    /*------------------------------------------------------------------------------
        Purpose: The name of the class used to generate data-relation field names for 
                 new data-relations dragged to the design canvas                                                                     
        Notes:   Class should implement 
                 Consultingwerk.BusinessEntityDesigner.Services.IDataRelationFieldsService                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY DataRelationFieldsService AS CHARACTER NO-UNDO 
    GET:
        RETURN eBusinessEntityDesignerSettings.DataRelationFieldsService .
    END GET .
    SET (arg AS CHARACTER):
        eBusinessEntityDesignerSettings.DataRelationFieldsService = arg .

        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
    END SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if Keywords generated by the code generators should 
                 get generated in upper or lower case
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC STATIC PROPERTY KeywordCasing AS CHARACTER NO-UNDO 
    GET:
        RETURN eBusinessEntityDesignerSettings.KeywordCasing .
    END GET . 
    SET (arg AS CHARACTER):
        eBusinessEntityDesignerSettings.KeywordCasing = arg .
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
    END SET. 

    /*------------------------------------------------------------------------------
        Purpose: Open generated files in OpenEdge IDE after generating
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY OpenFilesInIDE AS LOGICAL NO-UNDO 
    GET:
        RETURN eBusinessEntityDesignerSettings.OpenFilesInIDE .
    END GET .
    SET (arg AS LOGICAL):
        eBusinessEntityDesignerSettings.OpenFilesInIDE = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
    END SET . 

    /*------------------------------------------------------------------------------
        Purpose: Prompt for Save Changes before generating code
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC STATIC PROPERTY PromptForSaveBeforeGenerate AS LOGICAL NO-UNDO 
    GET:
        RETURN eBusinessEntityDesignerSettings.PromptForSaveBeforeGenerate .
    END GET .
    SET (arg AS LOGICAL):
        eBusinessEntityDesignerSettings.PromptForSaveBeforeGenerate = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
    END SET . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Settings File that settings were loaded from 
        Notes:   May return "<no settings file>" in a fresh environment
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC STATIC PROPERTY SettingsFile AS CHARACTER NO-UNDO INIT "<no settings file>":U  
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns if a settings file was found when starting the Business 
                 Entity Designerthe 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY SettingsFileLoaded AS LOGICAL NO-UNDO INIT FALSE   
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Split Array Fields into seperate fields when dragging database 
                 tables on the design canvas
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY SplitArrayFields AS LOGICAL NO-UNDO 
    GET:
        RETURN eBusinessEntityDesignerSettings.SplitArrayFields .
    END GET . 
    SET (arg AS LOGICAL):
        IF arg <> eBusinessEntityDesignerSettings.SplitArrayFields THEN 
            lChangedSettings = TRUE .  
        
        eBusinessEntityDesignerSettings.SplitArrayFields = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
    END SET. 
    

    /*------------------------------------------------------------------------------
        Purpose: The folder containing the template files used by the code 
                 generator                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY TemplateFolder AS CHARACTER NO-UNDO
    GET:
        RETURN eBusinessEntityDesignerSettings.TemplateFolder .
    END GET . 
    SET (arg AS CHARACTER):
        eBusinessEntityDesignerSettings.TemplateFolder = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
    END SET. 
    
    
    /*------------------------------------------------------------------------------
        Purpose: The r-code directory                                                                      
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY RCodeBaseFolder AS CHARACTER NO-UNDO
    GET:
        RETURN eBusinessEntityDesignerSettings.RCodeBaseFolder .
    END GET . 
    SET (arg AS CHARACTER):
        eBusinessEntityDesignerSettings.RCodeBaseFolder = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.        
    END SET. 

    /*------------------------------------------------------------------------------
        Purpose: The r-code directory for GUI objects                                                                     
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY RCodeBaseFolderGui AS CHARACTER NO-UNDO
    GET:
        IF eBusinessEntityDesignerSettings.RCodeBaseFolderGui > "":U THEN 
            RETURN eBusinessEntityDesignerSettings.RCodeBaseFolderGui .
        ELSE 
            RETURN eBusinessEntityDesignerSettings.RCodeBaseFolder .
    END GET . 
    SET (arg AS CHARACTER):
        eBusinessEntityDesignerSettings.RCodeBaseFolderGui = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.        
    END SET. 
        
    /*------------------------------------------------------------------------------
        Purpose: The sourcecode directory                                                                      
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY SourceCodeBaseFolder AS CHARACTER NO-UNDO
    GET:
        RETURN eBusinessEntityDesignerSettings.SourceCodeBaseFolder .
    END GET . 
    SET (arg AS CHARACTER):
        eBusinessEntityDesignerSettings.SourceCodeBaseFolder = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.        
    END SET. 

    /*------------------------------------------------------------------------------
        Purpose: The sourcecode directory for GUI objects                                                            
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY SourceCodeBaseFolderGui AS CHARACTER NO-UNDO
    GET:
        IF eBusinessEntityDesignerSettings.SourceCodeBaseFolderGui > "":U THEN 
            RETURN eBusinessEntityDesignerSettings.SourceCodeBaseFolderGui .
        ELSE 
            RETURN eBusinessEntityDesignerSettings.SourceCodeBaseFolder .
    END GET . 
    SET (arg AS CHARACTER):
        eBusinessEntityDesignerSettings.SourceCodeBaseFolderGui = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.        
    END SET. 

    /*------------------------------------------------------------------------------
        Purpose: Value for the GenerateDatasetController flag for new Business Entities                                                       
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY GenerateDatasetController AS LOGICAL NO-UNDO
    GET:
        RETURN eBusinessEntityDesignerSettings.GenerateDatasetController .
    END GET . 
    SET (arg AS LOGICAL):
        eBusinessEntityDesignerSettings.GenerateDatasetController = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.        
    END SET.               
              
    /*------------------------------------------------------------------------------
        Purpose: The sourcecode directory for include files                                                       
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY IncludeFilesBaseFolder AS CHARACTER NO-UNDO
    GET:
        IF eBusinessEntityDesignerSettings.IncludeFilesBaseFolder > "":U THEN 
            RETURN eBusinessEntityDesignerSettings.IncludeFilesBaseFolder .
        ELSE 
            RETURN eBusinessEntityDesignerSettings.SourceCodeBaseFolder .
    END GET . 
    SET (arg AS CHARACTER):
        eBusinessEntityDesignerSettings.IncludeFilesBaseFolder = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.        
    END SET. 
                
    /*------------------------------------------------------------------------------
        Purpose: The sourcecode directory for include files                                                       
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LauncherCommandLine AS CHARACTER NO-UNDO
    GET:
        RETURN eBusinessEntityDesignerSettings.LauncherCommandLine .
    END GET . 
    SET (arg AS CHARACTER):
        eBusinessEntityDesignerSettings.LauncherCommandLine = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.        
    END SET. 
                
    /*------------------------------------------------------------------------------
        Purpose: The tablename generator                                                                      
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY TableNameGeneratorService AS CHARACTER NO-UNDO
    GET:
        RETURN eBusinessEntityDesignerSettings.TableNameGeneratorService .
    END GET . 
    SET (arg AS CHARACTER):
        eBusinessEntityDesignerSettings.TableNameGeneratorService = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.        
    END SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: The name pattern used by the business entity designer when 
                 generating names for DATA-RELATION objects, i.e. "&1&2Relation"                                                                      
        Notes:   Use &1 for the source table name and &2 for the target table name                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY RelationNameSubstitute AS CHARACTER NO-UNDO
    GET:
        RETURN eBusinessEntityDesignerSettings.RelationNameSubstitute .
    END GET . 
    SET (arg AS CHARACTER):
        eBusinessEntityDesignerSettings.RelationNameSubstitute = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.        
    END SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Option to replace hyphens in database table names with proper
                 CamelCase when dragging a database table onto the design cansas
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ReplaceHyphenInTableName AS LOGICAL NO-UNDO
    GET:
        RETURN eBusinessEntityDesignerSettings.ReplaceHyphenInTableName .
    END GET . 
    SET (arg AS LOGICAL):
        IF arg <> eBusinessEntityDesignerSettings.ReplaceHyphenInTableName THEN 
            lChangedSettings = TRUE .  
        
        eBusinessEntityDesignerSettings.ReplaceHyphenInTableName = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
    END SET. 

    /*------------------------------------------------------------------------------
        Purpose: Option to replace underscores in database table names with proper
                 CamelCase when dragging a database table onto the design cansas
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ReplaceUnderScoreInTableName AS LOGICAL NO-UNDO
    GET:
        RETURN eBusinessEntityDesignerSettings.ReplaceUnderScoreInTableName .
    END GET . 
    SET (arg AS LOGICAL):
        IF arg <> eBusinessEntityDesignerSettings.ReplaceUnderScoreInTableName THEN 
            lChangedSettings = TRUE .  
        
        eBusinessEntityDesignerSettings.ReplaceUnderScoreInTableName = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
    END SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: The default prefix used for temp-table names generated for 
                 db tables dragged to the design canvas                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY DefaultTablePrefix AS CHARACTER NO-UNDO
    GET:
        RETURN eBusinessEntityDesignerSettings.DefaultTablePrefix .
    END GET . 
    SET (arg AS CHARACTER):
        IF arg <> eBusinessEntityDesignerSettings.DefaultTablePrefix THEN 
            lChangedSettings = TRUE .  
        
        eBusinessEntityDesignerSettings.DefaultTablePrefix = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
    END SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: The default suffix used for temp-table names generated for 
                 db tables dragged to the design canvas                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY DefaultTableSuffix AS CHARACTER NO-UNDO
    GET:
        RETURN eBusinessEntityDesignerSettings.DefaultTableSuffix .
    END GET . 
    SET (arg AS CHARACTER):
        IF arg <> eBusinessEntityDesignerSettings.DefaultTableSuffix THEN 
            lChangedSettings = TRUE .  
        
        eBusinessEntityDesignerSettings.DefaultTableSuffix = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
        
        SaveSettings() .
    END SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: The default prefix used for before table names generated for 
                 db tables dragged to the design canvas                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY DefaultBeforeTablePrefix AS CHARACTER NO-UNDO
    GET:
        RETURN eBusinessEntityDesignerSettings.DefaultBeforeTablePrefix .
    END GET . 
    SET (arg AS CHARACTER):
        IF arg <> eBusinessEntityDesignerSettings.DefaultBeforeTablePrefix THEN 
            lChangedSettings = TRUE .  
        
        eBusinessEntityDesignerSettings.DefaultBeforeTablePrefix = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
    END SET.                 

    /*------------------------------------------------------------------------------
        Purpose: The default suffix used for before table names generated for 
                 db tables dragged to the design canvas                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY DefaultBeforeTableSuffix AS CHARACTER NO-UNDO
    GET:
        RETURN eBusinessEntityDesignerSettings.DefaultBeforeTableSuffix .
    END GET . 
    SET (arg AS CHARACTER):
        IF arg <> eBusinessEntityDesignerSettings.DefaultBeforeTableSuffix THEN 
            lChangedSettings = TRUE .  
        
        eBusinessEntityDesignerSettings.DefaultBeforeTableSuffix = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
    END SET.                 

    /*------------------------------------------------------------------------------
        Purpose: The default for the DistinctTempTableIncludeFiles property of 
                 business entities (generate seperate include files per temp-table)
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY DistinctTempTableIncludeFiles AS LOGICAL NO-UNDO
    GET:
        RETURN eBusinessEntityDesignerSettings.DistinctTempTableIncludeFiles .
    END GET . 
    SET (arg AS LOGICAL):
        IF arg <> eBusinessEntityDesignerSettings.DistinctTempTableIncludeFiles THEN 
            lChangedSettings = TRUE .  
        
        eBusinessEntityDesignerSettings.DistinctTempTableIncludeFiles = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
    END SET. 

    /*------------------------------------------------------------------------------
        Purpose: Use a local LRU .bedgm file history - one per .BusinessEntityDesignerSettings
        Notes:   LRU will be stored in the Windows registry under a local path                                                                        
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC STATIC PROPERTY UseLocalFileHistory AS LOGICAL NO-UNDO 
    GET:
        RETURN eBusinessEntityDesignerSettings.UseLocalFileHistory .
    END GET . 
    SET (arg AS LOGICAL):
        IF arg <> eBusinessEntityDesignerSettings.UseLocalFileHistory THEN 
            lChangedSettings = TRUE .  
        
        eBusinessEntityDesignerSettings.UseLocalFileHistory = arg . 
        
        IF lChangedSettings AND NOT lChangingSettings THEN DO:
            lChangedSettings = FALSE .
            OnSettingsChanged (Consultingwerk.EventArgs:Empty) .
        END.
    END SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BusinessEntityDesignerSettings class                                                                         
        Notes:   Loads settings from the file .BusinessEntityDesignerSettings.xml                                                                         
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR STATIC BusinessEntityDesignerSettings ():

        FILE-INFO:FILE-NAME = ".BusinessEntityDesignerSettings.xml":U .

        IF FILE-INFO:FULL-PATHNAME <> ? THEN DO: 
            TEMP-TABLE eBusinessEntityDesignerSettings:READ-XML ("FILE":U, 
                                                                 FILE-INFO:FULL-PATHNAME,
                                                                 "EMPTY":U, ?, ?) .
            
            ASSIGN BusinessEntityDesignerSettings:SettingsFile       = FILE-INFO:FULL-PATHNAME .
                   BusinessEntityDesignerSettings:SettingsFileLoaded = TRUE .                                                         
        END.                                                                 
                                                                 
        FIND FIRST eBusinessEntityDesignerSettings NO-ERROR .
        
        IF NOT AVAILABLE eBusinessEntityDesignerSettings THEN DO:
            CREATE eBusinessEntityDesignerSettings .             

        /* Set defaults */
        ASSIGN
            BusinessEntityDesignerSettings:BusinessEntityGenerator   = "Consultingwerk.BusinessEntityDesigner.Generator.BusinessEntityGenerator":U
            BusinessEntityDesignerSettings:FieldNameGeneratorService = "Consultingwerk.BusinessEntityDesigner.Services.FieldNameGeneratorService":U
            BusinessEntityDesignerSettings:DataRelationFieldsService = "Consultingwerk.BusinessEntityDesigner.Services.DataRelationFieldsService":U
            BusinessEntityDesignerSettings:TemplateFolder            = "Consultingwerk\BusinessEntityDesigner\Generator\Templates":U
            BusinessEntityDesignerSettings:RelationNameSubstitute    = "&1&2Relation":U
            BusinessEntityDesignerSettings:DefaultTablePrefix        = "e":U
            BusinessEntityDesignerSettings:DefaultTableSuffix        = "":U
            BusinessEntityDesignerSettings:DefaultBeforeTablePrefix  = "e":U
            BusinessEntityDesignerSettings:DefaultBeforeTableSuffix  = "Before":U
                .
        END .         
        
        ERROR-STATUS:ERROR = FALSE NO-ERROR . 
        
    &IF PROVERSION EQ "11.0" &THEN
        CATCH err AS Progress.Lang.Error:
        	Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        	UNDO, THROW err . 	
        END CATCH.            
    &ENDIF
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Used to flag that a series of settings is being saved                                                                        
        Notes:   Used by the options dialog when setting properties of this class
                 to prevent saving of settings after each single property setter                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID BeginChangeSettings ():
        
        ASSIGN lChangingSettings = TRUE 
               lChangedSettings  = FALSE . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Used to flag that a series of settings has been saved                                                                        
        Notes:   Used by the options dialog when setting properties of this class
                 to prevent saving of settings after each single property setter                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID EndChangeSettings ():
        
        IF lChangingSettings AND lChangedSettings THEN 
            BusinessEntityDesignerSettings:OnSettingsChanged (Consultingwerk.EventArgs:Empty) .

        ASSIGN lChangingSettings = FALSE 
               lChangedSettings  = FALSE . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the SettingsChanged event                                                                        
        Notes:
        @param e The Consultingwerk.EventArgs with the data for this event                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID OnSettingsChanged (e AS Consultingwerk.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = Consultingwerk.EventArgs:Empty . 

        BusinessEntityDesignerSettings:SettingsChanged:Publish (?, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves settings to the file .BusinessEntityDesignerSettings.xml                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SaveSettings ():
        
        DEFINE VARIABLE oException AS ExceptionWithTitle NO-UNDO . 
        DEFINE VARIABLE cFileName  AS CHARACTER          NO-UNDO .
        
        FILE-INFO:FILE-NAME = ".BusinessEntityDesignerSettings.xml":U .
        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            cFileName = FILE-INFO:FULL-PATHNAME .
        ELSE 
            cFileName = ".BusinessEntityDesignerSettings.xml":U . 
        
        IF FILE-INFO:FULL-PATHNAME > "":U AND NOT FILE-INFO:FILE-TYPE MATCHES "*W*":U THEN  
            Consultingwerk.Util.MessageFormHelper:ShowMessage ("Unable to store application preferences."{&TRAN},
                                                               "Business Entity Designer"{&TRAN},
                                                               "The file .BusinessEntityDesignerSettings.xml is write protected."{&TRAN},
                                                               MessageFormImages:ImageWarning) . 
        ELSE DO ON ERROR UNDO, THROW: 
        
            TEMP-TABLE eBusinessEntityDesignerSettings:WRITE-XML ("FILE":U, 
                                                                  cFileName,
                                                                  TRUE) .
                                                                  
            CATCH err AS Progress.Lang.Error:
            		
                oException = NEW ExceptionWithTitle (err,
                                                     "Unable to store application preferences.~nThe file .BusinessEntityDesignerSettings.xml may be write protected or hidden."{&TRAN},
                                                     0,
                                                     "Business Entity Designer"{&TRAN}) .
        
                ErrorHelper:ShowErrorMessage (oException) .                            		
            		
            END CATCH.                                                                  
        END.                                                                  
                                                                  
        FIND FIRST eBusinessEntityDesignerSettings . 

    END METHOD.

END CLASS.
