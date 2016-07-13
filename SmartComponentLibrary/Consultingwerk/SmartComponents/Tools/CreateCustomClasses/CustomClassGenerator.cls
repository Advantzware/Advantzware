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
    File        : CustomClassGenerator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Aug 07 21:19:22 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.* FROM PROPATH . 
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Tools.CreateCustomClasses.CustomClassGenerator: 
    
    DEFINE PUBLIC PROPERTY OERA_CLASSES AS CHARACTER INIT
            "BusinessEntity,DataAccess,ServiceAdapter":U    
    GET.
    PRIVATE SET.

    DEFINE PUBLIC PROPERTY SMARTCOMPONENT_CLASSES AS CHARACTER INIT
            "BusinessEntityAdapter,BusinessEntityBindingSource,BusinessEntityLookup,ComboEditor,~
DataBrowser,DataCombo,DataObjectAdapter,DataObjectBindingSource,DataObjectLookup,DatasetChildAdapter,~
DynamicBrowser,DynamicUpdatableBrowser,NavigationPanel,PanelController,TempTableAdapter,~
ToolbarController,UpdatableBrowser,UserControl,ViewerControl,WindowForm":U
    GET.
    PRIVATE SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CustomClassGenerator class                                                                       
        Notes:
        @param pcBaseFolder The base folder to generate the files in
        @param pcOERAPackage The package for the custom OERA classes
        @param pcOERAPrefix The prefix for the custom OERA classes
        @param pcSmartComponentsPackage The package for the custom SmartComponent Library classes
        @param pcSmartComponentsPrefix The prefix for the custom SmartComponent Library classes
        @param pcAuthor The author name for the classes comments
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CustomClassGenerator (pcBaseFolder             AS CHARACTER, 
                                             pcOERAPackage            AS CHARACTER, 
                                             pcOERAPrefix             AS CHARACTER, 
                                             pcSmartComponentsPackage AS CHARACTER, 
                                             pcSmartComponentsPrefix  AS CHARACTER, 
                                             pcAuthor                 AS CHARACTER):
                                                 
        SUPER ().
        
        DEFINE VARIABLE i          AS INTEGER  NO-UNDO.
        DEFINE VARIABLE lcTemplate AS LONGCHAR NO-UNDO . 

        DO i = 1 TO NUM-ENTRIES (OERA_CLASSES) ON ERROR UNDO, THROW:
            
            COPY-LOB FROM FILE FileHelper:FindFile (SUBSTITUTE ("&1\Sample&2.template",
                                                    "Consultingwerk/SmartComponents/Tools/CreateCustomClasses/Templates/OERA",
                                                    ENTRY (i, OERA_CLASSES))) 
                     TO lcTemplate . 

            lcTemplate = SUBSTITUTE (lcTemplate, 
                                     pcOERAPackage,
                                     pcOERAPrefix,
                                     pcAuthor) . 

            COPY-LOB FROM lcTemplate 
                     TO FILE SUBSTITUTE ("&1/&2/&3&4.cls":U,
                                         pcBaseFolder,
                                         REPLACE (pcOERAPackage, ".":U, "/":U),
                                         pcOERAPrefix,
                                         ENTRY (i, OERA_CLASSES)) . 
            
        END.               
            
        DO i = 1 TO NUM-ENTRIES (SMARTCOMPONENT_CLASSES) ON ERROR UNDO, THROW:
            
            COPY-LOB FROM FILE FileHelper:FindFile (SUBSTITUTE ("&1\Sample&2.template",
                                                                "Consultingwerk/SmartComponents/Tools/CreateCustomClasses/Templates/SmartComponents",
                                                                ENTRY (i, SMARTCOMPONENT_CLASSES)))  
                     TO lcTemplate . 

            lcTemplate = SUBSTITUTE (lcTemplate, 
                                     pcSmartComponentsPackage,
                                     pcSmartComponentsPrefix,
                                     pcAuthor) . 

            COPY-LOB FROM lcTemplate 
                     TO FILE SUBSTITUTE ("&1/&2/&3&4.cls":U,
                                         pcBaseFolder,
                                         REPLACE (pcSmartComponentsPackage, ".":U, "/":U),
                                         pcSmartComponentsPrefix,
                                         ENTRY (i, SMARTCOMPONENT_CLASSES)) . 
        END.               
            
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.                
        
    END CONSTRUCTOR.

END CLASS.
