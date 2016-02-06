/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : OpenAccessSdkGenerator
    Purpose     : Generator for OpenAccess SDK interface procedures and 
                  JSON catalog file
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 03 23:21:28 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                                    FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.*                       FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.*             FROM PROPATH.
USING Consultingwerk.BusinessEntityDesigner.Plugins.OpenAccessSdk.* FROM PROPATH .  
USING Consultingwerk.OERA.*                                         FROM PROPATH .
USING Consultingwerk.OERA.OpenAccess.*                              FROM PROPATH .
USING Consultingwerk.Util.*                                         FROM PROPATH . 
USING Progress.Lang.*                                               FROM PROPATH .
USING Progress.Json.ObjectModel.*                                   FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.OpenAccessSdk.OpenAccessSdkGenerator: 

    /*------------------------------------------------------------------------------
        Purpose: Generates the OpenAccess SDK interface procedure and JSON catalog file
        Notes:   
        @param pcBusinessEntityName The name of the Business Entity
        @param pcEntityTable The primary table
        @param pcEntityView The list of secondary tables 
        @param pcInterfaceName The name of the service interface to generate
        @param pcRootFolder The root folder of the AppServer code base
        @param pcRelativeFolder The name of the relative folder that will contain the service interface procedure
        @param pcCatalogFolder The name of the folder to generate the catalog into
        @param piNumRecords The batch size to retrieve from the Business Entity
        @param piStopAfter The stop-after value
        @param plOuterJoin Use outer-join when joining the Business Entity Tables
        @param phDataset The handle of the ProDataset of the Business Entity
        @param pcDatasetIncludeFile The name and path of the ProDataset include file 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Generate (pcBusinessEntityName AS CHARACTER, 
 	                             pcEntityTable AS CHARACTER,
 	                             pcEntityView AS CHARACTER, 
 	                             pcInterfaceName AS CHARACTER,
	                             pcRootFolder AS CHARACTER,
                                 pcRelativeFolder AS CHARACTER,
                                 pcCatalogFolder AS CHARACTER,
                                 piNumRecords AS INTEGER, 
                                 piStopAfter AS INTEGER,
                                 plOuterJoin AS LOGICAL, 
                                 phDataset AS HANDLE,
                                 pcDatasetIncludeFile AS CHARACTER):
		
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcBusinessEntityName "'Business Entity Name':U"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcEntityTable "'Entity Table':U"} .
		{Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcInterfaceName "'Interface Name':U"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcRootFolder "'Root Folder':U"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcRelativeFolder "'Relative Folder':U"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcCatalogFolder "'Catalog Folder':U"} .
		
        THIS-OBJECT:GenerateCatalogFile (pcInterfaceName,
                                         pcRelativeFolder,
                                         pcCatalogFolder,
                                         pcEntityTable,
                                         pcEntityView,
                                         phDataset) .

        THIS-OBJECT:GenerateInterfaceProcedure (pcBusinessEntityName, 
                                                pcEntityTable,
                                                pcEntityView, 
                                                pcInterfaceName,
                                                pcRootFolder,
                                                pcRelativeFolder,
                                                piNumRecords, 
                                                piStopAfter,
                                                plOuterJoin, 
                                                pcDatasetIncludeFile) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the JSON Catalog file for the DataDirect OpenAccess SDK
        Notes:   
        @param pcInterfaceName The name of the service interface to generate
        @param pcRelativeFolder The name of the relative folder that will contain the service interface procedure
        @param pcCatalogFolder The name of the folder to generate the catalog into
        @param pcEntityTable The primary table
        @param pcEntityView The list of secondary tables 
        @param phDataset The handle of the ProDataset of the Business Entity
        @return The path of to the generated catalog file
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED CHARACTER GenerateCatalogFile (pcInterfaceName AS CHARACTER,
    	                                            pcRelativeFolder AS CHARACTER,
                                                    pcCatalogFolder AS CHARACTER,
                                                    pcEntityTable AS CHARACTER,
                                                    pcEntityView AS CHARACTER,
                                                    phDataset AS HANDLE):
		
        DEFINE VARIABLE oTableFactory       AS ResultsetTempTableFactory NO-UNDO . 
        DEFINE VARIABLE hTable              AS HANDLE                    NO-UNDO .
        DEFINE VARIABLE oSchemaGenerator    AS TempTableSchemaExporter   NO-UNDO .
        DEFINE VARIABLE oSchema             AS JsonObject                NO-UNDO . 
        DEFINE VARIABLE lcSchema            AS LONGCHAR                  NO-UNDO . 
		
        DEFINE VARIABLE cTemplateFile       AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE lcCode              AS LONGCHAR                  NO-UNDO .

        DEFINE VARIABLE cPath               AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE cFileName           AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs        NO-UNDO . 
    
        oTableFactory = NEW ResultsetTempTableFactory () . 
    
        hTable = oTableFactory:BuildResultsetTempTable (phDataset,
                                                        pcEntityTable,
                                                        pcEntityView) . 

        ASSIGN pcRelativeFolder = REPLACE (pcRelativeFolder, "~\":U, "/":U)
               cPath            = REPLACE (pcRelativeFolder, "/":U, "~\/":U)

               cTemplateFile    = TemplateHelper:Find ("OpenAccessSdk/catalog_json.template":U) .

        oSchemaGenerator = NEW TempTableSchemaExporter () . 
        
        oSchema = oSchemaGenerator:TempTableSchemaToJsonObject (hTable:DEFAULT-BUFFER-HANDLE) .
        
        oSchema:Write (INPUT-OUTPUT lcSchema, TRUE)  .

        COPY-LOB FROM FILE cTemplateFile TO lcCode . 

        ASSIGN lcCode = SUBSTITUTE (lcCode, 
                                    pcInterfaceName, 
                                    cPath, 
                                    lcSchema) 
               cFileName = SUBSTITUTE ("&1\&2.json":U,
                                       pcCatalogFolder,
                                       pcInterfaceName).        
        
        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("OpenAccess Catalog File":U, cFileName) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN 
                RETURN cFileName .
                
            IF oWriteFileEventArgs:NewFileName > "":U THEN 
                cFileName = oWriteFileEventArgs:NewFileName . 
        END.
        
        COPY-LOB FROM lcCode TO FILE cFileName . 

        RETURN cFileName . 

        FINALLY:
            IF VALID-HANDLE (hTable) THEN 
                DELETE OBJECT hTable . 		
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the Interface Procedure
        Notes:   
        @param pcBusinessEntityName The name of the Business Entity
        @param pcEntityTable The primary table
        @param pcEntityView The list of secondary tables 
        @param pcInterfaceName The name of the service interface to generate
        @param pcRootFolder The root folder of the AppServer code base
        @param pcRelativeFolder The name of the relative folder that will contain the service interface procedure
        @param piNumRecords The batch size to retrieve from the Business Entity
        @param piStopAfter The stop-after value
        @param plOuterJoin Use outer-join when joining the Business Entity Tables
        @param pcDatasetIncludeFile The name and path of the ProDataset include file
        @return The path of to the generated interface procedure
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED CHARACTER GenerateInterfaceProcedure (pcBusinessEntityName AS CHARACTER, 
                                                           pcEntityTable AS CHARACTER,
                                                           pcEntityView AS CHARACTER, 
                                                           pcInterfaceName AS CHARACTER,
                                                           pcRootFolder AS CHARACTER,
                                                           pcRelativeFolder AS CHARACTER,
                                                           piNumRecords AS INTEGER, 
                                                           piStopAfter AS INTEGER,
                                                           plOuterJoin AS LOGICAL, 
                                                           pcDatasetIncludeFile AS CHARACTER):

        DEFINE VARIABLE cTemplateFile       AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE lcCode              AS LONGCHAR           NO-UNDO . 
        DEFINE VARIABLE cFileName           AS CHARACTER          NO-UNDO . 
        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO . 
		
        ASSIGN cTemplateFile    = TemplateHelper:Find ("OpenAccessSdk/interface_procedure.template":U) .

        COPY-LOB FROM FILE cTemplateFile TO lcCode . 

        ASSIGN lcCode = REPLACE (lcCode, "@INTERFACE_NAME@":U,       pcInterfaceName)
               lcCode = REPLACE (lcCode, "@BUSINESS_ENTITY_NAME@":U, pcBusinessEntityName)
               lcCode = REPLACE (lcCode, "@ENTITY_TABLE@":U,         pcEntityTable)
               lcCode = REPLACE (lcCode, "@ENTITY_VIEW@":U,          pcEntityView)
               lcCode = REPLACE (lcCode, "@DATASET_INCLUDE_FILE@":U, pcDatasetIncludeFile)
               lcCode = REPLACE (lcCode, "@BATCH_SIZE@":U,           STRING (piNumRecords))
               lcCode = REPLACE (lcCode, "@STOP_AFTER@":U,           STRING (piStopAfter))
               lcCode = REPLACE (lcCode, "@OUTER_JOIN@":U,           STRING (plOuterJoin))
            
               lcCode = REPLACE (lcCode, "@DATE@":U, STRING (NOW, "99.99.9999 HH:MM:SS":U))                                       
               lcCode = REPLACE (lcCode, "@USER@":U, BusinessEntityGenerator:GetUserName())
            
            
               cFileName = SUBSTITUTE ("&1\&2\&3.p":U,
                                       pcRootFolder,
                                       pcRelativeFolder,
                                       pcInterfaceName).        
        
        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("OpenAccess Catalog File":U, cFileName) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN 
                RETURN cFileName .
                
            IF oWriteFileEventArgs:NewFileName > "":U THEN 
                cFileName = oWriteFileEventArgs:NewFileName . 
        END.
        
        COPY-LOB FROM lcCode TO FILE cFileName . 

        RETURN cFileName . 

	END METHOD .

END CLASS.
