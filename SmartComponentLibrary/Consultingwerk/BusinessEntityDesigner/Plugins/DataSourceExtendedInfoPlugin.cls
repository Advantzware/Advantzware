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
    File        : DataSourceExtendedInfoPlugin
    Purpose     : Business Entity Designer Plugin that creates code which 
                  provides runtime-queryable (using DYNAMIC-PROPERTY) info 
                  about data-source buffers and field mappings.
    Syntax      : 
    Description : Plugin manipulates the generated Data Access class after
                  code has been generated or regenerated. 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Feb 09 16:36:38 CET 2015
    Notes       : Requires annotations in the DataAccess Class and Templates:
                  @DefineDataSourceInformationStart.
                  @DefineDataSourceInformationEnd.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*           FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.* FROM PROPATH.
USING Consultingwerk.Studio.*                           FROM PROPATH.
USING Consultingwerk.Util.*                             FROM PROPATH.
USING Consultingwerk.Windows.Util.Forms.*               FROM PROPATH.
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.DataSourceExtendedInfoPlugin 
    IMPLEMENTS IBusinessEntityDesignerPlugin: 

    {Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY=REFERENCE-ONLY}

    DEFINE VARIABLE cOutputFolder AS CHARACTER NO-UNDO.
    
    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterGenerateSource and AfterReGenerateSource 
                 event of the BusinessEntityDatasetController                                                                        
        Notes:       
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event                                                                 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AfterGenerateSourceHandler (sender AS Progress.Lang.Object,
                                                      e AS Consultingwerk.EventArgs):
    
        ASSIGN cOutputFolder = THIS-OBJECT:FindOutputFolder () .
    
        THIS-OBJECT:UpdateDataAccessClass () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the output folder (source code folder for business logic) 
        Notes:   
        @return The output folder 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER FindOutputFolder ():
        
        DEFINE VARIABLE cSourceFolder AS CHARACTER NO-UNDO.

        IF BusinessEntityDesignerSettings:SourceCodeBaseFolder > "":U THEN DO:
            FILE-INFO:FILE-NAME = BusinessEntityDesignerSettings:SourceCodeBaseFolder .
            
            IF FILE-INFO:FULL-PATHNAME > "":U AND FILE-INFO:FILE-TYPE MATCHES "*D*":U THEN 
                ASSIGN cSourceFolder = FILE-INFO:FULL-PATHNAME + "~\":U. 
            ELSE 
                UNDO, THROW NEW AppError (SUBSTITUTE ("Source folder '&1' is not available."{&TRAN}, 
                                                      BusinessEntityDesignerSettings:SourceCodeBaseFolder),
                                          0) . 
        END.
        ELSE 
            ASSIGN cSourceFolder = "":U . 

        RETURN cSourceFolder . 

    END METHOD . 


    /*------------------------------------------------------------------------------
        Purpose: Locates the lines a start and end annotation are contained in a 
                 source code unit  
        Notes:   
        @param pcFileName The file name to parse
        @param pcStartAnnotation The name of the start annotation
        @param pcEndAnnotation The name of the end annotation
        @param pcAnnotationFileName OUTPUT The file name containing the annotations
        @param piStart The line number of the starting annotation
        @param piEnd The line number of the ending annotation
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GetAnnotationToFrom (pcFileName AS CHARACTER,    
                                               pcStartAnnotation AS CHARACTER,
                                               pcEndAnnotation AS CHARACTER,
                                               OUTPUT pcAnnotationFileName AS CHARACTER,
                                               OUTPUT piStart AS INTEGER,
                                               OUTPUT piEnd AS INTEGER):
        
        DEFINE VARIABLE cFile             AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE oParser           AS AnnotationParser NO-UNDO . 
        DEFINE VARIABLE oListAnnotation   AS ListAnnotation   NO-UNDO . 
        DEFINE VARIABLE oAnnotation       AS Annotation       NO-UNDO . 
        
        DEFINE VARIABLE iFrom             AS INTEGER          NO-UNDO .
        DEFINE VARIABLE iTo               AS INTEGER          NO-UNDO .
        
        ASSIGN oParser         = NEW AnnotationParser (pcFileName) 
               oListAnnotation = oParser:FindAnnotations (pcStartAnnotation) .
    
        IF oListAnnotation:Count > 0 THEN 
            ASSIGN cFile = oListAnnotation:GetItem (1):FileName
                   iFrom = oListAnnotation:GetItem (1):Line .
        ELSE DO:
            MessageFormHelper:ShowMessage ("Unable to update the DataSource Information!"{&TRAN},
                                           "Business Entity Generator"{&TRAN},
                                           SUBSTITUTE ("Unable to find the &1 annotation."{&TRAN}, 
                                                       pcStartAnnotation),
                                           MessageFormImages:ImageWarning) .
            RETURN .                                                        
        END. 

        ASSIGN oListAnnotation = oParser:FindAnnotations (pcEndAnnotation) .
    
        IF oListAnnotation:Count > 0 THEN DO:
            ASSIGN iTo   = oListAnnotation:GetItem (1):Line .
            
            IF cFile <> oListAnnotation:GetItem (1):FileName THEN DO:
                MessageFormHelper:ShowMessage ("Unable to update the DataSource Information!"{&TRAN},
                                               "Business Entity Generator"{&TRAN},
                                               SUBSTITUTE ("The &1 and &2 annotations are not contained in the same file."{&TRAN}, 
                                                           pcStartAnnotation,
                                                           pcEndAnnotation),
                                               MessageFormImages:ImageWarning) .
                
                RETURN . 
            END. 
        END.                   
        ELSE DO:
            MessageFormHelper:ShowMessage ("Unable to update the DataSource Information!"{&TRAN},
                                           "Business Entity Generator"{&TRAN},
                                           SUBSTITUTE ("Unable to find the &1 annotation."{&TRAN},
                                                       pcEndAnnotation),
                                           MessageFormImages:ImageWarning) .
            RETURN .                                                        
        END. 
                    
        IF iFrom >= iTo THEN DO:
            MessageFormHelper:ShowMessage ("Unable to update the DataSource Information!"{&TRAN},
                                           "Business Entity Generator"{&TRAN},
                                           SUBSTITUTE ("The &1 and &2 annotations are not in the right order."{&TRAN},
                                                       pcStartAnnotation,
                                                       pcEndAnnotation),
                                           MessageFormImages:ImageWarning) .
            RETURN . 
        END.   

        ASSIGN pcAnnotationFileName = cFile 
               piStart              = iFrom
               piEnd                = iTo .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the field mapping pairs for the named temp-table                                                                        
        Notes:                         
        @param pcTempTableName The name of the temp-table to return the field mappings for
        @param piSpaces The number of SPACE Characters (CHR(32)) to prefix each entry with
        @return The list of the field mappings                                               
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GetFieldMapping (pcTempTableName AS CHARACTER,
                                                piSpaces AS INTEGER):
        
        DEFINE BUFFER b_eField FOR eField . 
        
        DEFINE VARIABLE cMapping AS CHARACTER NO-UNDO.
        
        FOR EACH b_eField WHERE b_eField.TempTableName = pctempTableName
                            AND b_eField.FieldSource   > "":U  
                             BY b_eField.FieldOrder:

            IF piSpaces > 0 THEN 
                ASSIGN cMapping = cMapping + FILL (" ":U, piSpaces) +
                                             SUBSTITUTE ("&1,&2,":U,
                                                         b_eField.FieldName,
                                                         b_eField.FieldSource) + "~n":U.            
            ELSE 
                ASSIGN cMapping = cMapping + SUBSTITUTE ("&1,&2,":U,
                                                         b_eField.FieldName,
                                                         b_eField.FieldSource) .            
        END.                                      

        IF piSpaces > 0 THEN 
            ASSIGN cMapping = TRIM (cMapping, "~n":U) .

        RETURN TRIM (cMapping, ",":U) .

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
	                            poController AS Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityDatasetController, 
	                            poForm AS Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityDesignerForm, 
	                            poToolbar AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager):
		

        poController:AfterGenerateSource:Subscribe (AfterGenerateSourceHandler) .
        poController:AfterRegenerateSource:Subscribe (AfterGenerateSourceHandler) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the Data Access class 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID UpdateDataAccessClass ():
		
		DEFINE BUFFER bEntity FOR eBusinessEntity . 
        DEFINE BUFFER bTable  FOR eTable . 

        DEFINE VARIABLE iStart                 AS INTEGER            NO-UNDO .
        DEFINE VARIABLE iEnd                   AS INTEGER            NO-UNDO .
        DEFINE VARIABLE lcSource               AS LONGCHAR           NO-UNDO .
        DEFINE VARIABLE lcDefineDataSourceInfo AS LONGCHAR           NO-UNDO . 
        DEFINE VARIABLE i                      AS INTEGER            NO-UNDO .
		
        DEFINE VARIABLE cFilename              AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE lFirst                 AS LOGICAL            NO-UNDO INIT TRUE .
		
        DEFINE VARIABLE oWriteFileEventArgs    AS WriteFileEventArgs NO-UNDO . 
		
		FIND FIRST bEntity.
		
		FILE-INFO:FILE-NAME = SUBSTITUTE ("&1/&2/&3.cls":U, 
		                                  cOutputFolder, 
		                                  REPLACE (bEntity.DataAccessPackage, ".":U, "/":U),
		                                  bEntity.DataAccessName) . 
		
		IF FILE-INFO:FULL-PATHNAME = ? THEN 
		    RETURN . 
		    
        THIS-OBJECT:GetAnnotationToFrom (FILE-INFO:FULL-PATHNAME,    
                                         "@DefineDataSourceInformationStart":U,
                                         "@DefineDataSourceInformationEnd":U,
                                         OUTPUT cFileName,
                                         OUTPUT iStart,
                                         OUTPUT iEnd) . 
		
		IF iStart > 0 AND iEnd > 0 THEN DO:

            COPY-LOB FROM FILE cFileName TO lcSource . 

            DO i = iStart + 1 TO iEnd - 1:
                lcSource = Consultingwerk.Util.ListHelper:RemoveEntry (lcSource,
                                                                       iStart + 1,
                                                                       CHR (10)) . 
            END.
    
            FOR EACH bTable OF bEntity:  
                IF NOT lFirst THEN 
                    lcDefineDataSourceInfo = lcDefineDataSourceInfo + CHR(10) .

                lcDefineDataSourceInfo = lcDefineDataSourceInfo +   
                                         SUBSTITUTE ("    DEFINE PROTECTED PROPERTY SourceTables_&1 AS CHARACTER NO-UNDO INIT ~"&2~":U GET .&3":U,
                                                     bTable.TempTableName,
                                                     bTable.SourceTableNames,
                                                     CHR(10)) +
                                         SUBSTITUTE ("    DEFINE PROTECTED PROPERTY SourceBufferNames_&1 AS CHARACTER NO-UNDO INIT ~"&2~":U GET .&3":U,
                                                     bTable.TempTableName,
                                                     bTable.SourceBufferNames,
                                                     CHR(10)) +
                                         SUBSTITUTE ("    DEFINE PROTECTED PROPERTY SourceFieldMapping_&1 AS CHARACTER NO-UNDO INIT ~"&2~":U GET .&3":U,
                                                     bTable.TempTableName,
                                                     THIS-OBJECT:GetFieldMapping (bTable.TempTableName, 0),
                                                     CHR(10)) 
                                                     .                                                                                         
                ASSIGN lFirst = FALSE .                                                         
            END.
    
            lcSource = Consultingwerk.Util.ListHelper:InsertEntry (lcSource,
                                                                   iStart + 1,
                                                                   TRIM(STRING (lcDefineDataSourceInfo), CHR(10)),
                                                                   CHR (10)) .
    
            lcSource = StringHelper:FixLineEnding (lcSource) .
            
            oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("Data Access Object":U, cFileName) .
    
            IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
                IF oWriteFileEventArgs:Cancel THEN 
                    RETURN .
                    
                IF oWriteFileEventArgs:NewFileName > "":U THEN 
                    cFileName = oWriteFileEventArgs:NewFileName . 
            END.
            
            COPY-LOB FROM lcSource TO FILE cFileName . 
		END.
		
	END METHOD .

END CLASS.
