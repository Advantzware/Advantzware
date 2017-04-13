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
    File        : RunProcedureCallParameter
    Purpose     : Function call parameter implementation that can launch 
                  a GUI for .NET form 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Nov 02 18:07:52 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                   FROM PROPATH .
USING Consultingwerk.Framework.*         FROM PROPATH . 
USING Consultingwerk.SmartFramework.*    FROM PROPATH .
USING Consultingwerk.Util.*              FROM PROPATH .
USING Consultingwerk.Windows.Framework.* FROM PROPATH .  
USING Progress.Lang.*                    FROM PROPATH .

CLASS Consultingwerk.SmartFramework.RunProcedureCallParameter 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF    
    IMPLEMENTS IFunctionCallParameter: 
    
    {Consultingwerk/JsonSerializableProperty.i AllowMultiple LOGICAL} .
    {Consultingwerk/JsonSerializableProperty.i CharacterParameterValue CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i InitializeInternalProcedure CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i InitializeInternalProcedureParameter CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ProcedureName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ReactivateInternalProcedure CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ReactivateInternalProcedureParameter CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i RunPersistent LOGICAL} .
    {Consultingwerk/JsonSerializableProperty.i UseCharacterParameterValue LOGICAL} .
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Editor Control used by the Function maintenance
                 Viewer 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY EditorControl AS CHARACTER NO-UNDO INIT "Consultingwerk.Windows.Framework.Menu.RunProcedureCallParameterControl":U 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the image representing this Function Call Parameter
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ImageKey AS CHARACTER INITIAL "Consultingwerk/Windows/Framework/Images/FunctionCallParameter/media_play_green.png":U NO-UNDO 
    GET.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InvokeStaticMethodCallParameter class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RunProcedureCallParameter ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Locates an already running instance of the procedure 
        Notes:   
        @return The handle of the already running instance of the procedure 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED HANDLE FindProcedure ():
        
        DEFINE VARIABLE hProcedure AS HANDLE NO-UNDO.
        
        ASSIGN hProcedure = SESSION:FIRST-PROCEDURE .
        
        DO WHILE VALID-HANDLE (hProcedure):
        
            IF hProcedure:FILE-NAME = THIS-OBJECT:ProcedureName THEN 
                RETURN hProcedure .  
            
            hProcedure = hProcedure:NEXT-SIBLING . 
        END.

        RETURN ? .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invokes/Starts the function associated with the IFunctionCallParameter 
                 object
        Notes:   
        @param plForceNewInstance Logical value indicating if a new Instance should be started (True/False/?)
        @param poContext An optional reference to a context (could be the Control that launches the function) 
        @return The HandleHolder that contains the reference to the running instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Progress.Lang.Object Invoke (plForceNewInstance AS LOGICAL,
                                               poContext AS Progress.Lang.Object):

        DEFINE VARIABLE hProcedure    AS HANDLE NO-UNDO .

        /* If there should only be a single instance of this Form,
           reactivate when there is already a Form instance */
        IF THIS-OBJECT:RunPersistent AND 
           THIS-OBJECT:ReactivateInternalProcedure > "":U AND 
           (THIS-OBJECT:AllowMultiple = FALSE OR plForceNewInstance = FALSE) THEN DO:
            hProcedure = THIS-OBJECT:FindProcedure () .

            IF VALID-HANDLE (hProcedure) THEN DO:
                
                IF THIS-OBJECT:ReactivateInternalProcedureParameter > "":U THEN 
                    RUN VALUE (THIS-OBJECT:ReactivateInternalProcedure) IN hProcedure (THIS-OBJECT:ReactivateInternalProcedureParameter).
                ELSE 
                    RUN VALUE (THIS-OBJECT:ReactivateInternalProcedure) IN hProcedure . 
    
                RETURN NEW Consultingwerk.HandleHolder (hProcedure) .            
            END. 
        END .

        IF THIS-OBJECT:UseCharacterParameterValue THEN DO:
            
            IF THIS-OBJECT:RunPersistent THEN DO:
                RUN VALUE (THIS-OBJECT:ProcedureName) PERSISTENT SET hProcedure (THIS-OBJECT:CharacterParameterValue). 
                
                IF THIS-OBJECT:InitializeInternalProcedure > "":U THEN DO:
                    IF THIS-OBJECT:InitializeInternalProcedureParameter > "":U THEN 
                        RUN VALUE (THIS-OBJECT:InitializeInternalProcedure) IN hProcedure (THIS-OBJECT:InitializeInternalProcedureParameter).
                    ELSE 
                        RUN VALUE (THIS-OBJECT:InitializeInternalProcedure) IN hProcedure . 
                END.
                
                RETURN NEW Consultingwerk.HandleHolder (hProcedure) .            
            END.
            ELSE DO: 
                RUN VALUE (THIS-OBJECT:ProcedureName) (THIS-OBJECT:CharacterParameterValue). 
    
                RETURN ? .                 
            END.
        END. 
        ELSE DO:
            IF THIS-OBJECT:RunPersistent THEN DO:
                RUN VALUE (THIS-OBJECT:ProcedureName)  PERSISTENT SET hProcedure . 
                
                IF THIS-OBJECT:InitializeInternalProcedure > "":U THEN DO:
                    IF THIS-OBJECT:InitializeInternalProcedureParameter > "":U THEN 
                        RUN VALUE (THIS-OBJECT:InitializeInternalProcedure) IN hProcedure (THIS-OBJECT:InitializeInternalProcedureParameter).
                    ELSE 
                        RUN VALUE (THIS-OBJECT:InitializeInternalProcedure) IN hProcedure . 
                END.
                
                RETURN NEW Consultingwerk.HandleHolder (hProcedure) .            
            END.
            ELSE DO: 
                RUN VALUE (THIS-OBJECT:ProcedureName) . 
    
                RETURN ? .                 
            END.
            
        END.
    
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns true when the function is currently active (runninng) 
        Notes:   
        @return True when the function is currently active (runninng)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsActive ():
        
        RETURN VALID-HANDLE (THIS-OBJECT:FindProcedure()) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: ReActivates the function 
        Notes:   
        @return True when the function was successfully reactivated
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ReActivate ():
        
        DEFINE VARIABLE hProcedure AS HANDLE NO-UNDO.
        
        /* If there should only be a single instance of this Form,
           reactivate when there is already a Form instance */
        IF THIS-OBJECT:RunPersistent AND 
           THIS-OBJECT:ReactivateInternalProcedure > "":U THEN DO:
               
            hProcedure = THIS-OBJECT:FindProcedure () .

            IF VALID-HANDLE (hProcedure) THEN DO:
                IF THIS-OBJECT:ReactivateInternalProcedureParameter > "":U THEN 
                    RUN VALUE (THIS-OBJECT:ReactivateInternalProcedure) IN hProcedure (THIS-OBJECT:ReactivateInternalProcedureParameter).
                ELSE 
                    RUN VALUE (THIS-OBJECT:ReactivateInternalProcedure) IN hProcedure . 
    
                RETURN TRUE .            
            END. 
        END .
        
        RETURN FALSE . 
        
    END METHOD.

END CLASS.
