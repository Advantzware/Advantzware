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
    File        : InvokeServiceMethodCallParameter
    Purpose     : Function call parameter implementation that can invoke
                  a method from a serivce in the default service container                  
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Nov 02 22:12:14 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Framework.*      FROM PROPATH . 
USING Consultingwerk.SmartFramework.* FROM PROPATH .
USING Consultingwerk.Util.*           FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF    
    IMPLEMENTS IFunctionCallParameter: 

    {Consultingwerk/JsonSerializableProperty.i CharacterParameterValue CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ClassName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i InterfaceName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i MethodName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i UseCharacterParameterValue LOGICAL} .
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Editor Control used by the Function maintenance
                 Viewer 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC STATIC PROPERTY EditorControl AS CHARACTER NO-UNDO INIT "Consultingwerk.Windows.Framework.Menu.InvokeServiceMethodCallParameterControl":U 
	GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the image representing this Function Call Parameter
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC STATIC PROPERTY ImageKey AS CHARACTER INITIAL "Consultingwerk/Windows/Framework/Images/FunctionCallParameter/gears.png":U NO-UNDO 
	GET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InvokeStaticMethodCallParameter class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvokeServiceMethodCallParameter ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Invokes/Starts the function associated with the IFunctionCallParameter 
                 object
        Notes:   
        @param plForceNewInstance Logical value indicating if a new Instance should be started (True/False/?)
        @param poContext An optional reference to a context (could be the Control that launches the function) 
        @return The reference to the Form that has been launched
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Progress.Lang.Object Invoke (plForceNewInstance AS LOGICAL,
                                               poContext AS Progress.Lang.Object):

        DEFINE VARIABLE oService AS Progress.Lang.Object NO-UNDO . 

        oService = FrameworkSettings:ServiceContainer:GetService 
                        (Progress.Lang.Class:GetClass (THIS-OBJECT:InterfaceName)) .

        IF NOT VALID-OBJECT (oService) THEN DO:    
            oService = DYNAMIC-NEW (THIS-OBJECT:ClassName) () .
            
            FrameworkSettings:ServiceContainer:AddService (Progress.Lang.Class:GetClass (THIS-OBJECT:InterfaceName),
                                                           oService) .
        END.

        IF THIS-OBJECT:UseCharacterParameterValue THEN 
            DYNAMIC-INVOKE (oService,
                            THIS-OBJECT:MethodName,
                            THIS-OBJECT:CharacterParameterValue) .
        ELSE 
            DYNAMIC-INVOKE (oService,
                            THIS-OBJECT:MethodName) . 

        RETURN oService .        
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns true when the function is currently active (runninng) 
        Notes:   
        @return True when the function is currently active (runninng)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsActive ():
        
        RETURN VALID-OBJECT (FrameworkSettings:ServiceContainer:GetService 
                        (Progress.Lang.Class:GetClass (THIS-OBJECT:InterfaceName))) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: ReActivates the function 
        Notes:   
        @return True when the function was successfully reactivated
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ReActivate ():
        
        THIS-OBJECT:Invoke (FALSE, ?) .
        
        RETURN TRUE . 
 
    END METHOD.

END CLASS.