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
    File        : LaunchFormCallParameter
    Purpose     : Function call parameter implementation that can launch 
                  a GUI for .NET form 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Nov 02 18:07:52 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                   FROM PROPATH .
USING Consultingwerk.Framework.*         FROM PROPATH . 
USING Consultingwerk.SmartFramework.*    FROM PROPATH .
USING Consultingwerk.Util.*              FROM PROPATH .
USING Consultingwerk.Windows.Framework.* FROM PROPATH .  
USING Progress.Lang.*                    FROM PROPATH .
USING System.Windows.Forms.*             FROM ASSEMBLY . 

CLASS Consultingwerk.Windows.Framework.LaunchFormCallParameter 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF    
    IMPLEMENTS IFunctionCallParameter: 
    
    {Consultingwerk/JsonSerializableProperty.i AllowMultiple LOGICAL} .
    {Consultingwerk/JsonSerializableProperty.i CharacterParameterValue CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ClassName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i FormStartType Consultingwerk.Windows.Framework.FormStartTypeEnum} .
    {Consultingwerk/JsonSerializableProperty.i UseCharacterParameterValue LOGICAL} .
    
    DEFINE VARIABLE lRetrying AS LOGICAL NO-UNDO INIT FALSE .    
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Default FormStartType
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY DefaultFormStartType AS FormStartTypeEnum NO-UNDO 
    GET.
    SET (INPUT arg AS FormStartTypeEnum):
        IF arg = FormStartTypeEnum:Default THEN 
            UNDO, THROW NEW Consultingwerk.Exceptions.InvalidParameterValueException ("DefaultFormStartType":U,
                                                                                      arg:ToString (),
                                                                                      "Consultingwerk.Windows.Framework.LaunchFormCallParameter":U) .            
        Consultingwerk.Windows.Framework.LaunchFormCallParameter:DefaultFormStartType = arg . 
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Editor Control used by the Function maintenance
                 Viewer 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY EditorControl AS CHARACTER NO-UNDO INIT "Consultingwerk.Windows.Framework.Menu.LaunchFormCallParameterControl":U 
    GET.
        
    /*------------------------------------------------------------------------------
        Purpose: Returns the image representing this Function Call Parameter
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ImageKey AS CHARACTER INITIAL "Consultingwerk/Windows/Framework/Images/FunctionCallParameter/window.png":U NO-UNDO 
    GET.
            
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InvokeStaticMethodCallParameter class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LaunchFormCallParameter ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LaunchFormCallParameter class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR STATIC LaunchFormCallParameter ():
        
        Consultingwerk.Windows.Framework.LaunchFormCallParameter:DefaultFormStartType = FormStartTypeEnum:SeparateForm .
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Locates the reference to a running Form instance                                                                       
        Notes:
        @return The reference to the running Form instance                                                                         
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED Progress.Windows.Form FindFormInstance ():
        
        RETURN FormHelper:FindRunningFormInstance (THIS-OBJECT:ClassName) .

    END METHOD.

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

        DEFINE VARIABLE oForm         AS Progress.Windows.Form NO-UNDO . 
        DEFINE VARIABLE oStartType    AS FormStartTypeEnum     NO-UNDO . 
        DEFINE VARIABLE oDialogResult AS DialogResult          NO-UNDO . 
        
        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN 
            FrameworkSettings:WaitStateManager:SetWaitState ("Launching Form"{&TRAN}) .
        
        /* If there should only be a single instance of this Form,
           reactivate when there is already a Form instance */
        IF THIS-OBJECT:AllowMultiple = FALSE OR plForceNewInstance = FALSE  THEN DO:
            oForm = FindFormInstance () .

            IF VALID-OBJECT (oForm) THEN DO:
                oForm:Activate () .
                
                RETURN oForm .
            END .
        END .
        
        IF THIS-OBJECT:UseCharacterParameterValue THEN 
            oForm = DYNAMIC-NEW (THIS-OBJECT:ClassName)
                                (THIS-OBJECT:CharacterParameterValue) .
        ELSE 
            oForm = DYNAMIC-NEW (THIS-OBJECT:ClassName) () .

        IF VALID-OBJECT (poContext) AND TYPE-OF (oForm, IReceiveMenuContext) THEN 
            CAST (oForm, IReceiveMenuContext):ReceiveMenuContext (poContext) . 

        /* Mike Fechner, Consultingwerk Ltd. 13.03.2014
           SCL-197 - allow the Form to validate if it's o.k. to launch the Form*/
        IF TYPE-OF (oForm, IValidateFormStart) THEN DO ON ERROR UNDO, THROW:
            IF CAST (oForm, IValidateFormStart):ValidateFormStart (poContext) = FALSE THEN DO:
                GarbageCollectorHelper:DeleteObject (oForm) .
                
                RETURN ? .      
            END.
            
            CATCH err AS Progress.Lang.Error:
                GarbageCollectorHelper:DeleteObject (oForm) .
                
                UNDO, THROW err .       
            END CATCH.
        END.
        
        oStartType = THIS-OBJECT:FormStartType .
            
        IF NOT VALID-OBJECT (oStartType) OR oStartType = FormStartTypeEnum:Default THEN 
           oStartType = Consultingwerk.Windows.Framework.LaunchFormCallParameter:DefaultFormStartType . 
        
        CASE oStartType:
            WHEN FormStartTypeEnum:SeparateForm THEN DO:
                oForm:Show () .
            END.
            WHEN FormStartTypeEnum:MdiChild THEN DO:
                oForm:MdiParent = Consultingwerk.Framework.FrameworkSettings:MdiContainer .
                oForm:Show () .
            END.
            WHEN FormStartTypeEnum:Dialog THEN DO:
                IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN 
                    FrameworkSettings:WaitStateManager:ClearWaitState ("Launching Form"{&TRAN}) .

                THIS-OBJECT:WaitForShowDialog (oForm, OUTPUT oDialogResult) .
            END.
        END CASE .
        
        RETURN oForm .

&IF DEFINED (RetryFormLaunchingOnProgressLangSysError) NE 0 &THEN       
        /* Mike Fechner, Consultingwerk Ltd. 16.01.2014
           Handling of SysError with no message - retry exactly once
           Ability to activate a workaround for OpenEdge core issues, where (still at one 
           customer) on OpenEdge 11.3.1 the usage of static methods/properties/events in 
           hybrid classes and/or the overriding of .NET event methods (On... methods) 
           using an ABL method may cause a Progress.Lang.SysError with no message when 
           starting the Form the first time */
        CATCH syserr AS Progress.Lang.SysError:
    
            IF syserr:NumMessages = 0 THEN DO ON ERROR UNDO, THROW:
                IF NOT lRetrying THEN DO:
                    ASSIGN lRetrying = TRUE .

                    IF VALID-OBJECT (oForm) THEN
                        DELETE OBJECT oForm .

                    RETURN THIS-OBJECT:Invoke (plForceNewInstance,
                                               poContext) .

                END.

                FINALLY:
                        ASSIGN lRetrying = FALSE .
                END FINALLY.
            END.
            ELSE
                UNDO, THROW syserr .
        END CATCH.
&ENDIF        
        FINALLY:
            IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN 
                FrameworkSettings:WaitStateManager:ClearWaitState ("Launching Form"{&TRAN}) .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns true when the function is currently active (runninng) 
        Notes:   
        @return True when the function is currently active (runninng)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsActive ():
        
        RETURN VALID-OBJECT (THIS-OBJECT:FindFormInstance ()) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: ReActivates the function 
        Notes:   
        @return True when the function was successfully reactivated
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ReActivate ():
        
        DEFINE VARIABLE oForm AS Progress.Windows.Form NO-UNDO . 
        
        oForm = FindFormInstance () .
 
        IF VALID-OBJECT (oForm) THEN DO:
            oForm:Activate () .
            RETURN TRUE .  
        END.

        RETURN FALSE . 
 
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Executes the WAIT-FOR ShowDialog for a DialogForm
        Notes:   avoid using WAIT-FOR in a non-void method to simplify compilation
                 on OpenEdge 10.2B
        @param poForm The DialogForm to show
        @param poDialogResult OUTPUT The DialogResult member that is the result of the ShowDialog method 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID WaitForShowDialog (poForm AS System.Windows.Forms.Form,
                                             OUTPUT poDialogResult AS DialogResult):
        
        WAIT-FOR poForm:ShowDialog () SET poDialogResult . 

    END METHOD .

END CLASS.
