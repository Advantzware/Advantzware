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
    File        : WebClientPackageHelper
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 30 08:38:45 CET 2012
    Notes       : A sample Json Configuration file may look like this:
                    {
                      "TargetPath": "..\\WebClient",
                      "CompileFiles": "*.cls,*.w,*.p",
                      "CompileLogFile": "..\\compile.log",
                      "CopyFiles": "*.xml,appsrvtt.d,*.resx,*.dll,*.png,*.gif,*.bmp,*.jpg",
                      "InfragisticsAssembliesFrom": "%DLC%\\bin\\infragistics\\winforms",
                      "InfragisticsAssembliesTo": "..\\WebClient\\Assemblies",
                      "AlternativeConfigDirectory": "..\\WebClientConfig"
                    }
                  This class is designed for OpenEdge 11 and up
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW . 

{Consultingwerk/products.i} 

USING Consultingwerk.Exceptions.*         FROM PROPATH . 
USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH . 
USING Progress.Lang.*                     FROM PROPATH .

CLASS Consultingwerk.Studio.WebClientPackageHelper:

     /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE WebClientPackageHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Creates a WebClient Deployment based on a configuration file (Json)                                                                         
        Notes:   The Json File needs to match a JsonObject with the following properties:
                 TargetPath, CompileFiles, CompileLogFile, CopyFiles, InfragisticsAssembliesFrom
        @param pcConfigFile The name of the Json configuration file   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID CreateDeployment (pcConfigFile AS CHARACTER):
        
        IF PROVERSION > "11":U THEN . 
        ELSE 
            UNDO, THROW NEW NotSupportedException ("CreateDeployment":U,
                                                   "Consultingwerk.Studio.WebClientPackageHelper":U) .
                                                   
        &IF PROVERSION GE "11" &THEN
        DEFINE VARIABLE oJsonObject AS Progress.Json.ObjectModel.JsonObject        NO-UNDO .
        DEFINE VARIABLE oJsonParser AS Progress.Json.ObjectModel.ObjectModelParser NO-UNDO . 

        oJsonParser = NEW Progress.Json.ObjectModel.ObjectModelParser () .
                
        oJsonObject = CAST (oJsonParser:ParseFile (pcConfigFile),
                            Progress.Json.ObjectModel.JsonObject) . 

        CreateDeployment (oJsonObject) .        
        &ENDIF                                                   

    END METHOD.

    &IF PROVERSION GE "11" &THEN
    /*------------------------------------------------------------------------------
        Purpose: Creates a WebClient Deployment based on configuration data in a JsonObject                                                                      
        Notes:   The JsonObject needs to have the following properties: TargetPath, 
                 CompileFiles, CompileLogFile, CopyFiles, InfragisticsAssembliesFrom
        @param poConfigJsonObject The JsonObject with the configuration data   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID CreateDeployment (poConfigJsonObject AS Progress.Json.ObjectModel.JsonObject):
        
        DEFINE VARIABLE cTargetPath        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cCompileFiles      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cCompileLogFile    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cCopyFiles         AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cInfragisticsFrom  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cInfragisticsTo    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cAlternativeConfig AS CHARACTER NO-UNDO.

        ASSIGN cTargetPath        = poConfigJsonObject:GetCharacter ("TargetPath":U)
               cCompileFiles      = poConfigJsonObject:GetCharacter ("CompileFiles":U)
               cCompileLogFile    = poConfigJsonObject:GetCharacter ("CompileLogFile":U)
               cCopyFiles         = poConfigJsonObject:GetCharacter ("CopyFiles":U)
               cInfragisticsFrom  = poConfigJsonObject:GetCharacter ("InfragisticsAssembliesFrom":U)
               cInfragisticsTo    = poConfigJsonObject:GetCharacter ("InfragisticsAssembliesTo":U)
               cAlternativeConfig = poConfigJsonObject:GetCharacter ("AlternativeConfigDirectory":U)
                .

        OS-DELETE VALUE (cCompileLogFile) . 

        IF NOT Consultingwerk.Studio.CompilerHelper:CompileFiles (".":U,
                                                                  cCompileFiles,
                                                                  cTargetPath,
                                                                  cCompileLogFile,
                                                                  TRUE) THEN 
            
            Consultingwerk.Util.MessageFormHelper:ShowMessage (SUBSTITUTE ("Compilation finished with errors. Please check the logfile:&1&2"{&TRAN},
                                                                           Consultingwerk.Environment:NewLine,
                                                                           cCompileLogFile),
                                                               "WebClient Package Helper"{&TRAN},
                                                               MessageFormImages:ImageError) .

        Consultingwerk.Util.FileHelper:CopyFiles (".":U,
                                                  cCopyFiles,
                                                  cTargetPath) .        

        IF cInfragisticsFrom > "":U AND cInfragisticsTo > "":U THEN 
            Consultingwerk.Util.FileHelper:CopyFiles (REPLACE (cInfragisticsFrom, 
                                                               "%DLC%":U,
                                                               Consultingwerk.Util.SessionHelper:DLC),
                                                      "*.dll":U,
                                                      cInfragisticsTo) .        

        Consultingwerk.Util.FileHelper:CopyFiles (cAlternativeConfig,
                                                  "*.*":U,
                                                  cTargetPath) .        

        Consultingwerk.Util.MessageFormHelper:ShowMessage ("WebClient Deployment finished."{&TRAN},
                                                           "WebClient Package Helper"{&TRAN}) . 

    END METHOD . 
    &ENDIF
&ENDIF        
END CLASS.
