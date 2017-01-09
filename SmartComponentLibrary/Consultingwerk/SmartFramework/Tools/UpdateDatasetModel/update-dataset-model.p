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
    File        : update-dataset-model.p
    Purpose     : Regenerates the Dataset Model source code for all 
                  Business Entities of the SmartFramework 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Mar 06 11:25:08 CET 2015
    Notes       : Due to dependency on the Business Entity Designer, this 
                  file is not suited for Linux builds 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.* FROM PROPATH.

{Consultingwerk/products.i}

&IF DEFINED (SmartComponentLibrary) NE 0 &THEN

DEFINE VARIABLE oGenerator AS Consultingwerk.BusinessEntityDesigner.Plugins.ModelClassGeneratorPlugin NO-UNDO .

{Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i}
{Consultingwerk/Util/TempTables/ttFileNames.i}

/* ***************************  Main Block  *************************** */

FileHelper:GetFileList ("Consultingwerk/SmartFramework":U,
                        "*.bedgm":U,
                        OUTPUT TABLE ttFileNames) .

DEFAULT-WINDOW:WIDTH = 220 . 

oGenerator = NEW Consultingwerk.BusinessEntityDesigner.Plugins.ModelClassGeneratorPlugin () . 
oGenerator:Startup (DATASET dsBusinessEntity BIND, ?, ?, ?) .

FOR EACH ttFileNames 
    ON ERROR UNDO, THROW:
        
    DATASET dsBusinessEntity:EMPTY-DATASET () .
    DATASET dsBusinessEntity:READ-XML ("FILE":U, ttFileNames.FileName, ?, ?, ?) .
    
    FIND FIRST eBusinessEntity .
    
    DISPL ttFileNames.FileName                  FORMAT "x(80)":U COLUMN-LABEL "File":U
          eBusinessEntity.BusinessEntityPackage FORMAT "x(50)":U COLUMN-LABEL "Package":U
          eBusinessEntity.BusinessEntityName    FORMAT "x(50)":U COLUMN-LABEL "Entity":U
          WITH WIDTH 210 .
    
    
    oGenerator:GenerateModelClasses (DATASET dsBusinessEntity) .
    
END.    

CATCH err AS Progress.Lang.Error :
	ErrorHelper:ShowErrorMessage (err) . 	
END CATCH.

&ENDIF