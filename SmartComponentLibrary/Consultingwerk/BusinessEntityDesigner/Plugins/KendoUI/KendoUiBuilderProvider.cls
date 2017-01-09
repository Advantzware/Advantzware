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
    File        : KendoUiBuilderProvider
    Purpose     : Service classes which can provide the Grid Builder class 
                  names and the list of available Templates to the KendoUiGridWizard
    Syntax      : 
    Description : Default IKendoUiBuilderProvider implementation
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 04 09:22:15 CEST 2015
    Notes       : Returns a fixed list of Grid Builder names and the templates
                  found in 
                  - Consultingwerk/BusinessEntityDesigner/Generator/Templates/Web/KendoUiGrid
                  - {TemplateDirectory}/Web/KendoUiGrid
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                        FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.*                 FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.KendoUI.* FROM PROPATH .  
USING Consultingwerk.Util.*                                   FROM PROPATH .
USING Progress.Lang.*                                         FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.KendoUI.KendoUiBuilderProvider 
    IMPLEMENTS IKendoUiBuilderProvider: 

    {Consultingwerk/Util/TempTables/ttFileNames.i}

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of available Grid Builders 
        Notes:   
        @return The ListNameValuePair with the available Grid Builder classes
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ListNameValuePair GetAvailableGridBuilders ():
        
        DEFINE VARIABLE oList AS ListNameValuePair NO-UNDO .
        
        oList = NEW ListNameValuePair () . 
        
        oList:Add ("Consultingwerk.BusinessEntityDesigner.Generator.KendoUI.UpdatableGridBuilder":U, "Updatable":U) . 
        oList:Add ("Consultingwerk.BusinessEntityDesigner.Generator.KendoUI.ReadOnlyGridBuilder":U, "Read-Only":U) . 

        RETURN oList . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of the available Page Template Names 
        Notes:   
        @return The ListNameValuePair with the available Page Template Names
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Consultingwerk.ListNameValuePair GetAvailableTemplates ():
        
        EMPTY TEMP-TABLE ttFileNames . 
        
        IF REPLACE (BusinessEntityDesignerSettings:TemplateFolder, "~\":U, "/":U) <> 
          "Consultingwerk/BusinessEntityDesigner/Generator/Templates":U AND 
          FileHelper:Exists (BusinessEntityDesignerSettings:TemplateFolder + "/Web/KendoUiGrid":U) THEN 
          
            FileHelper:GetFileList (BusinessEntityDesignerSettings:TemplateFolder + "/Web/KendoUiGrid":U,
                                    "*.template":U,
                                    OUTPUT TABLE ttFileNames) .
            
        FileHelper:GetFileList ("Consultingwerk/BusinessEntityDesigner/Generator/Templates/Web/KendoUiGrid":U,
                                "*.template":U,
                                OUTPUT TABLE ttFileNames APPEND) .
        
        DEFINE VARIABLE oList AS ListNameValuePair NO-UNDO .
        
        oList = NEW ListNameValuePair () . 
    
        FOR EACH ttFileNames:
            oList:Add (ttFileNames.FileName,
                       FileHelper:ShortFileName (ttFileNames.FileName)) .
        END.    
        
        RETURN oList  .

    END METHOD .

END CLASS.
