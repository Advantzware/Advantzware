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
    File        : InvokeStaticMethodCallParameter
    Purpose     : Function call parameter implementation that can invoke
                  a static method of a class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Oct 12 01:47:10 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                FROM PROPATH . 
USING Consultingwerk.SmartFramework.* FROM PROPATH .  
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.SmartFramework.InvokeStaticMethodCallParameter 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF    
    IMPLEMENTS IFunctionCallParameter: 

    {Consultingwerk/JsonSerializableProperty.i CharacterParameterValue CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ClassName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i MethodName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i UseCharacterParameterValue LOGICAL} .

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Editor Control used by the Function maintenance
                 Viewer 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY EditorControl AS CHARACTER NO-UNDO INIT "Consultingwerk.Windows.Framework.Menu.InvokeStaticMethodCallParameterControl":U 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the image representing this Function Call Parameter
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ImageKey AS CHARACTER INITIAL "Consultingwerk/Windows/Framework/Images/FunctionCallParameter/gear.png":U NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InvokeStaticMethodCallParameter class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvokeStaticMethodCallParameter ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Invokes/Starts the function associated with the IFunctionCallParameter 
                 object
        Notes:   
        @param plForceNewInstance Logical value indicating if a new Instance should be started (True/False/?)
        @param poContext An optional reference to a context (could be the Control that launches the function) 
        @return Unknown value, this method does not return an object reference
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Progress.Lang.Object Invoke (plForceNewInstance AS LOGICAL,
                                               poContext AS Progress.Lang.Object):
        
        IF THIS-OBJECT:UseCharacterParameterValue = TRUE THEN 
            DYNAMIC-INVOKE (THIS-OBJECT:ClassName, THIS-OBJECT:MethodName, THIS-OBJECT:CharacterParameterValue) .
        ELSE 
            DYNAMIC-INVOKE (THIS-OBJECT:ClassName, THIS-OBJECT:MethodName) .
        
        RETURN ? .
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns true when the function is currently active (runninng) 
        Notes:   
        @return True when the function is currently active (runninng)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsActive ():
        
        RETURN FALSE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: ReActivates the function 
        Notes:   
        @return True when the function was successfully reactivated
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ReActivate ():
        
        RETURN FALSE .

    END METHOD .

END CLASS.
