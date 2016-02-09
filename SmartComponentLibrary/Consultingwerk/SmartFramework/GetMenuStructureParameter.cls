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
    File        : GetMenuStructureParameter
    Purpose     : Parameter object for the GetMenuStructure method
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Dec 07 17:53:03 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                     FROM PROPATH .
USING Consultingwerk.SmartFramework.*      FROM PROPATH .  
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .  
USING Progress.Lang.*                      FROM PROPATH .

CLASS Consultingwerk.SmartFramework.GetMenuStructureParameter
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF    
    IMPLEMENTS IGetMenuStructureParameter:

    /*------------------------------------------------------------------------------
        Purpose: Returns if the Root Menu Entry should also te retrieved 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i IncludeRootMenuEntry LOGICAL} .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Number of Levels deep the Menu is requested 
        Notes:   Use 0 to request all menu data
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i NumberOfLevels INTEGER} .
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the Root Node Key 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i RootMenuKey CHARACTER} .
    
    /*------------------------------------------------------------------------------
        Purpose: Returns if the authorization checks should be skipps 
        Notes:   Allows to retrieve full menu structure to clients that perform 
                 authentication only after the first menu item has been selected 
                 etc.
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i SkipAuthorizationCheck LOGICAL "INIT FALSE"} .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetMenuStructureParameter class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC GetMenuStructureParameter ():
		SUPER ().
		
		THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetMenuStructureParameter class
        Notes:   
        @param pcRootMenuKey The value for the RootMenuKey property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetMenuStructureParameter (pcRootMenuKey AS CHARACTER):
        THIS-OBJECT ().
        
        THIS-OBJECT:RootMenuKey = pcRootMenuKey .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetMenuStructureParameter class
        Notes:   
        @param pcRootMenuKey The value for the RootMenuKey property
        @param piNumberOfLevels The value for the NumberOfLevels property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetMenuStructureParameter (pcRootMenuKey AS CHARACTER,
                                                  piNumberOfLevels AS INTEGER):
        THIS-OBJECT (pcRootMenuKey).
        
        THIS-OBJECT:NumberOfLevels = piNumberOfLevels .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetMenuStructureParameter class
        Notes:   
        @param pcRootMenuKey The value for the RootMenuKey property
        @param piNumberOfLevels The value for the NumberOfLevels property
        @param plIncludeRootMenuEntry The value for the IncludeRootMenuEntry propetry
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetMenuStructureParameter (pcRootMenuKey AS CHARACTER,
                                                  piNumberOfLevels AS INTEGER,
                                                  plIncludeRootMenuEntry AS LOGICAL):
        THIS-OBJECT (pcRootMenuKey, piNumberOfLevels).
        
        THIS-OBJECT:IncludeRootMenuEntry = plIncludeRootMenuEntry .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetMenuStructureParameter class
        Notes:   
        @param pcRootMenuKey The value for the RootMenuKey property
        @param piNumberOfLevels The value for the NumberOfLevels property
        @param plIncludeRootMenuEntry The value for the IncludeRootMenuEntry property
        @param plSkipAuthorizationCheck The value of the SkipAuthorizationCheck
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetMenuStructureParameter (pcRootMenuKey AS CHARACTER,
                                                  piNumberOfLevels AS INTEGER,
                                                  plIncludeRootMenuEntry AS LOGICAL,
                                                  plSkipAuthorizationCheck AS LOGICAL):
                                                      
        THIS-OBJECT (pcRootMenuKey, piNumberOfLevels, plIncludeRootMenuEntry).
        
        THIS-OBJECT:SkipAuthorizationCheck = plSkipAuthorizationCheck .
        
    END CONSTRUCTOR.
        
END CLASS.
