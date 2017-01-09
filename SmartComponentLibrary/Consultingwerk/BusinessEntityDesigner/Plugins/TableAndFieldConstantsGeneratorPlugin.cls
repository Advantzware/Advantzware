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
    File        : TableAndFieldConstantsGeneratorPlugin
    Purpose     : Business Entity Designer Plugin that supports the 
                  generation of include files with Constants for ProDataset 
                  Buffer and Field Names
    Syntax      : 
    Description : Creates an include file with the name <dataset name>_Constants.i
                  This include contains GLOBAL-DEFINE compile time variables
                  that can be used in dynamic code to avoid typos when building 
                  dynamic queries. Generates the include file whenever the 
                  Business Entity is generated of regenerated. 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 21 22:26:40 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                  FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.*           FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.*  FROM PROPATH . 
USING Consultingwerk.Exceptions.*                       FROM PROPATH . 
USING Consultingwerk.Util.*                             FROM PROPATH . 
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.TableAndFieldConstantsGeneratorPlugin 
    IMPLEMENTS IBusinessEntityDesignerPlugin: 

    {Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY"}

    &scoped-define DatasetConstantsIncludeTemplate datasetconstants.template
    
    DEFINE VARIABLE cOutputFolder AS CHARACTER NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterGenerateSource event of the 
                 BusinessEntityDatasetController                                                                        
        Notes:       
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event                                                                 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AfterGenerateSourceHandler (sender AS Progress.Lang.Object,
                                                      e AS Consultingwerk.EventArgs):
        
        /* Mike Fechner, Consultingwerk Ltd. 02.04.2012
           To-Do: Check Directory exists */
           
        /* Mike Fechner, Consultingwerk Ltd. 02.04.2012
           To-Do: Check Write protected */
    
        ASSIGN cOutputFolder = THIS-OBJECT:FindOutputFolder () .
    
        THIS-OBJECT:GenerateConstantsInclude () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Verifies the path to a template file                                                                       
        Notes:   Throws an error, when the template file cannot be found                                   
        @param pcFileName The relative template file name
        @return The absolute path to the template file                                 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER CheckTemplatePath (pcFileName AS CHARACTER):
        
        FILE-INFO:FILE-NAME = pcFileName .
        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            RETURN FILE-INFO:FULL-PATHNAME . 
            
        UNDO, THROW NEW AppError (SUBSTITUTE ("The template file ~"&1~" cannot be found."{&TRAN},
                                              pcFileName), 0) .    
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the output folder (source code folder for business logic) 
        Notes:   
        @return The output folder 
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED CHARACTER FindOutputFolder ():
		
        DEFINE VARIABLE cSourceFolder AS CHARACTER NO-UNDO.

        IF BusinessEntityDesignerSettings:IncludeFilesBaseFolder > "":U THEN DO:
            FILE-INFO:FILE-NAME = BusinessEntityDesignerSettings:IncludeFilesBaseFolder .
            
            IF FILE-INFO:FULL-PATHNAME > "":U AND FILE-INFO:FILE-TYPE MATCHES "*D*":U THEN 
                ASSIGN cSourceFolder = FILE-INFO:FULL-PATHNAME + "~\":U. 
            ELSE 
                UNDO, THROW NEW AppError (SUBSTITUTE ("Source folder '&1' is not available."{&TRAN}, 
                                                      BusinessEntityDesignerSettings:IncludeFilesBaseFolder),
                                          0) . 
        END.
        ELSE 
            ASSIGN cSourceFolder = "":U . 

        RETURN cSourceFolder . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the Model Classes                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GenerateConstantsInclude ():

        DEFINE VARIABLE cTemplate           AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE lcSource            AS LONGCHAR           NO-UNDO . 
        DEFINE VARIABLE lcConstants         AS LONGCHAR           NO-UNDO . 
        DEFINE VARIABLE cOutputFile         AS CHARACTER          NO-UNDO . 

        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO . 
        
        DEFINE BUFFER b_eBusinessEntity           FOR eBusinessEntity .
        DEFINE BUFFER b_eBusinessEntityProperties FOR eBusinessEntityProperties .
        DEFINE BUFFER b_eTable                    FOR eTable .
        DEFINE BUFFER b_eDataRelation             FOR eDataRelation .
        DEFINE BUFFER b_eField                    FOR eField .
        DEFINE BUFFER b_eIndex                    FOR eIndex . 
        
        FIND FIRST b_eBusinessEntity . 

        ASSIGN 
            cTemplate = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "\{&DatasetConstantsIncludeTemplate}":U)
                .  

        FOR EACH b_eTable ON ERROR UNDO, THROW:
            
            ASSIGN lcConstants = lcConstants + SUBSTITUTE ("~n&&GLOBAL-DEFINE &1 ~"&1~"~n":U,
                                                           b_eTable.TempTableName) . 
            
            FOR EACH b_eField WHERE b_eField.TempTableName  = b_eTable.TempTableName
                              BY b_eField.FieldOrder:

                ASSIGN lcConstants = lcConstants + SUBSTITUTE ("&&GLOBAL-DEFINE &1.&2 ~"&1.&2~"~n":U,
                                                               b_eTable.TempTableName,
                                                               b_eField.FieldName) . 
            END . 
        END . 

        COPY-LOB FROM FILE cTemplate TO lcSource .

        ASSIGN lcSource = SUBSTITUTE (lcSource, 
                                      b_eBusinessEntity.DatasetName,
                                      BusinessEntityGenerator:GetUserName(),
                                      STRING (NOW, "99.99.9999 HH:MM:SS":U),
                                      lcConstants) . 

        ASSIGN cOutputFile = TRIM(SUBSTITUTE ("&3&1/&2_Constants.i":U, 
                                              b_eBusinessEntity.DatasetPath, 
                                              b_eBusinessEntity.DatasetName,
                                              cOutputFolder), "/":U) . 

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("TempTable and Field Contants Include":U, cOutputFile) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN 
                RETURN .
                
            IF oWriteFileEventArgs:NewFileName > "":U THEN 
                cOutputFile = oWriteFileEventArgs:NewFileName . 
        END.

        COPY-LOB FROM lcSource TO FILE cOutputFile . 
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the Constants include file
        Notes:   This routine is intended to be used from outside of the Business Entity 
                 designer (automatic source code generation, unit testing)     
        @param dsBusinessEntity Business Entity with the Design dataset, called with the BY-REFERENCE keyword
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID GenerateConstantsInclude (DATASET FOR dsBusinessEntity):
		
		THIS-OBJECT:GenerateConstantsInclude () .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Method called when the Business Entity Designer will get closed                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Shutdown ():

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Method called when the Business Entity Designer is starting and 
                 has loaded the plugin.                                                                      
        Notes:   Passes references to some key components of the Designer to 
                 the plugin, use this to initialize the plugin, subscribe to events
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

    END METHOD.

END CLASS.
