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
    File        : RestInterfaceGenerator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jun 11 13:15:47 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.*                         FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.Generator.*               FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.Generator.RestInterface.* FROM PROPATH .  
USING Consultingwerk.Util.*                                           FROM PROPATH . 
USING Progress.Lang.*                                                 FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.RestInterface.RestInterfaceGenerator: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the AppServer Module
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AppServerModule AS CHARACTER NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Business Entity Name 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BusinessEntityName AS CHARACTER NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Business Entity Package 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BusinessEntityPackage AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name the ProDataset
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the path of the dataset include file
        Notes:   This is the path reference relative to the Eclipse workspace virtual
                 file system
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetPath AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Description
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Description AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the table used for the Read method
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FindTable AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Path for the Dataset Include File
        Notes:   This is the path reference used in the source code for referencing 
                 the include file 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IncludeFilePath AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Service Interface Name 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ServiceInterfaceName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Service Interface Package Name 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ServiceInterfacePackage AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the output folder 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SourceFolder AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the URI of the Interface  
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY URI AS CHARACTER NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Generates the source code for the service interface class 
        Notes:   
        @return The name of the generated class file 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER Generate ():

        DEFINE VARIABLE lcSource            AS LONGCHAR           NO-UNDO . 
        DEFINE VARIABLE cOutputFile         AS CHARACTER          NO-UNDO.
        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO . 
	
        FILE-INFO:FILE-NAME = BusinessEntityDesignerSettings:TemplateFolder + "/OeMobile/restinterface.template":U .

        COPY-LOB FROM FILE FILE-INFO:FULL-PATHNAME TO lcSource . 

        ASSIGN lcSource = SUBSTITUTE (lcSource, 
                                      THIS-OBJECT:ServiceInterfaceName,
                                      THIS-OBJECT:ServiceInterfacePackage,
                                      SUBSTITUTE ("&1.&2":U, THIS-OBJECT:BusinessEntityPackage, THIS-OBJECT:BusinessEntityName),
                                      THIS-OBJECT:Description,
                                      STRING (NOW, "99.99.9999 HH:MM:SS":U),
                                      THIS-OBJECT:AppServerModule,
                                      THIS-OBJECT:URI,
                                      THIS-OBJECT:DatasetName,
                                      THIS-OBJECT:DatasetPath) .
                                      
        ASSIGN lcSource = REPLACE (lcSource, "@FINDTABLE@":U, THIS-OBJECT:FindTable)
               lcSource = REPLACE (lcSource, "@INCLUDEFILEPATH@":U, THIS-OBJECT:IncludeFilePath)
               lcSource = REPLACE (lcSource, "@USER@":U, BusinessEntityGenerator:GetUserName ()) 
               lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, BusinessEntityGenerator:GenerateFileAnnotation(THIS-OBJECT:BusinessEntityPackage,
                                                                                                                  THIS-OBJECT:BusinessEntityName,
                                                                                                                  "RestInterface":U)) .

        ASSIGN cOutputFile = FileHelper:CombinePath (THIS-OBJECT:SourceFolder,
                                                     THIS-OBJECT:AppServerModule,
                                                     REPLACE (THIS-OBJECT:ServiceInterfacePackage, ".":U, "/":U),
                                                     SUBSTITUTE ("&1.cls":U, THIS-OBJECT:ServiceInterfaceName)) . 

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("REST Interface":U, cOutputFile) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN 
                RETURN cOutputFile .
                
            IF oWriteFileEventArgs:NewFileName > "":U THEN 
                cOutputFile = oWriteFileEventArgs:NewFileName . 
        END.

        COPY-LOB FROM lcSource TO FILE cOutputFile .
        
        RETURN cOutputFile .  

	END METHOD .    

END CLASS.
