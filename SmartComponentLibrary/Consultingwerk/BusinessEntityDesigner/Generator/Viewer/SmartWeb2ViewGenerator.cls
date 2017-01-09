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
    File        : SmartWeb2ViewGenerator
    Purpose     : Generator for SmartViewerControl Classes 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 17 10:05:14 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*                    FROM PROPATH .      
USING Consultingwerk.BusinessEntityDesigner.Generator.*          FROM PROPATH .       
USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.*   FROM PROPATH .  
USING Consultingwerk.BusinessEntityDesigner.Generator.Web2View.* FROM PROPATH.
USING Consultingwerk.Framework.Enum.*                            FROM PROPATH . 
USING Progress.Lang.*                                            FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Viewer.SmartWeb2ViewGenerator 
    IMPLEMENTS ISmartViewerGenerator: 

    DEFINE TEMP-TABLE ttMethods NO-UNDO 
        FIELD Name AS CHARACTER 
        FIELD Code AS CLOB 
        INDEX Name Name . 

    DEFINE TEMP-TABLE ttObjectNames NO-UNDO 
        FIELD ObjectName AS CHARACTER 
        INDEX ObjectName IS UNIQUE PRIMARY ObjectName. 

    DEFINE VARIABLE cSourceFolder AS CHARACTER NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of Columns in the BindingSource Schema
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BindingSourceColumns AS "System.Collections.Generic.List<Progress.Data.ColumnPropDesc>":U NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Class Name for the generated SmartViewerControl
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClassName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of IControlGenerators used by this SmartViewerGenerator
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ControlGenerators AS ListIControlGenerator NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the generated User Control should contain a 
                 Binding Source or not
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CreateBindingSource AS LOGICAL INITIAL TRUE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityJoin property of the SmartBusinessEntityBindingSource
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityJoin AS CHARACTER NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityName property of the SmartBusinessEntityBindingSource
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityTable property of the SmartBusinessEntityBindingSource
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityTable AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityView property of the SmartBusinessEntityBindingSource
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityView AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the output file name 
        Notes:   Optionally. Some Generators may not use the Viewer package and Class 
                 Name for generation.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OutputFileName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Class Name for the generated SmartViewerControl
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PackageName AS CHARACTER NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartWeb2ViewGenerator class
        Notes:   
        @param pcOutputFileName The output file name  
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartWeb2ViewGenerator (pcOutputFileName AS CHARACTER):
        SUPER ().
        
        THIS-OBJECT:BindingSourceColumns = NEW "System.Collections.Generic.List<Progress.Data.ColumnPropDesc>":U ().
        THIS-OBJECT:ControlGenerators = NEW ListIControlGenerator () .
        
        THIS-OBJECT:OutputFileName = pcOutputFileName . 
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Creates a method in the container control
        Notes:
        @param pcMethodName The name of the method to create
        @param pcCode The source code of the method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateMethod (pcMethodName AS CHARACTER,
                                     pcCode AS LONGCHAR):
    
        CREATE ttMethods . 
        ASSIGN ttMethods.Name = pcMethodName .
        
        COPY-LOB FROM pcCode TO ttMethods.Code .
    
    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Generates the source code for the SmartViewerControl 
        Notes:   
        @return The file name of the generated source 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER Generate ():
        
        DEFINE VARIABLE lcFields  AS LONGCHAR  NO-UNDO . 
        DEFINE VARIABLE lcCode    AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE cTemplate AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cOptions  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cFileNAme AS CHARACTER NO-UNDO .
        
        cTemplate = TemplateHelper:Find ("Web/Views/web2view.template":U) .

        COPY-LOB FROM FILE cTemplate TO lcCode . 
        
        {Consultingwerk/foreachABL.i IControlGenerator oControl in this-object:ControlGenerators}

            IF TYPE-OF (oControl, ISupportsWeb2ViewGeneration) THEN 
                lcFields = lcFields + CAST (oControl, ISupportsWeb2ViewGeneration):GenerateHtml () + "~n":U. 

        END.            

        ASSIGN lcCode = SUBSTITUTE (lcCode, lcFields) .            
        
        COPY-LOB FROM lcCode TO FILE THIS-OBJECT:OutputFileName .
        
        RUN Consultingwerk/BusinessEntityDesigner/Generator/Web2View/gene4gl.p (THIS-OBJECT:OutputFileName,
                                                                                INPUT-OUTPUT cOptions,
                                                                                INPUT-OUTPUT cFileName) .
                                
        IF cFileName > "":U THEN DO:
            COMPILE VALUE (cFileName) SAVE . 
            OS-DELETE VALUE (cFileName) .
        END .                                                     
        
        RETURN THIS-OBJECT:OutputFileName .  
 
    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Generates a temporary object name (e.g. appearance1) based on a 
                 provoded name (e.g. appearance)
        Notes:   
        @param pcObjectName The name to be used for a temporary object
        @return The name to be used for a temporary object
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GenerateObjectName (pcObjectName AS CHARACTER):
        
        DEFINE VARIABLE iCounter AS INTEGER   NO-UNDO INIT 0 .
        DEFINE VARIABLE cName    AS CHARACTER NO-UNDO.
        
        REPEAT:
            ASSIGN iCounter = iCounter + 1 
                   cName    = SUBSTITUTE ("&1&2":U,
                                          pcObjectName,
                                          iCounter).
                                          
            IF NOT CAN-FIND (ttObjectNames WHERE ttObjectNames.ObjectName = cName) THEN DO:
                CREATE ttObjectNames .
                ASSIGN ttObjectNames.ObjectName = cName .
                RELEASE ttObjectNames .
                
                RETURN cName .
            END.
        END .

    END METHOD .
    
END CLASS.
