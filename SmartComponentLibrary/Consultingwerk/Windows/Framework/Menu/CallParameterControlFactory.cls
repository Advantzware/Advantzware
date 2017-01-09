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
    File        : CallParameterControlFactory
    Purpose     : Factory class for the UI Controls used to editor the 
                  Call Parameter classes in the menu function maintenance
    Syntax      : Relies on a static property called "EditorControl" in the 
                  IFunctionCallParameter implementations
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Dec 04 20:05:12 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.Framework.Menu.ICallParameterControlFactory FROM PROPATH .
USING Consultingwerk.SmartFramework.Services.*                           FROM PROPATH .  
USING Progress.Lang.*                                                    FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.Framework.Menu.CallParameterControlFactory 
    IMPLEMENTS ICallParameterControlFactory: 

    /*------------------------------------------------------------------------------
        Purpose: Returns an instance of a CallParameterControl based on the type of
                 the CallParameter
        Notes:   
        @param poCallParameterType The class referenceof the CallParameter for which a UserControl should be provided 
        @return The specialized CallParameterControl instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Windows.Forms.UserControl GetCallParameterControl (poCallParameterType AS Progress.Lang.Class):
        
        DEFINE VARIABLE cUserControlClassName AS CHARACTER NO-UNDO.

        Consultingwerk.Assertion.ObjectAssert:IsValid (poCallParameterType) .


&IF NOT PROVERSION BEGINS "10.2":U &THEN
        ASSIGN cUserControlClassName = poCallParameterType:GetPropertyValue ("EditorControl":U) .
&ELSE
        CASE poCallParameterType:TypeName:
            WHEN "Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter":U THEN ASSIGN cUserControlClassName = Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter:EditorControl .           
            WHEN "Consultingwerk.SmartFramework.InvokeStaticMethodCallParameter":U  THEN ASSIGN cUserControlClassName = Consultingwerk.SmartFramework.InvokeStaticMethodCallParameter:EditorControl .
            WHEN "Consultingwerk.SmartFramework.RunProcedureCallParameter":U        THEN ASSIGN cUserControlClassName = Consultingwerk.SmartFramework.RunProcedureCallParameter:EditorControl .     
            WHEN "Consultingwerk.Windows.Framework.LaunchFormCallParameter":U       THEN ASSIGN cUserControlClassName = Consultingwerk.Windows.Framework.LaunchFormCallParameter:EditorControl .   
            WHEN "Consultingwerk.Windows.Framework.WebUriCallParameter":U           THEN ASSIGN cUserControlClassName = Consultingwerk.Windows.Framework.WebUriCallParameter:EditorControl .       
        END CASE . 
&ENDIF

        Consultingwerk.Assertion.Assert:NotNullOrEmpty (cUserControlClassName, "User Control Class Name":U) .

        RETURN InternalGetControl (cUserControlClassName).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Internal Method to handle the creation an caching of control instances
        Notes:   
        @param pcUserControlClassName The full type name of the user control class naem
        @return The CallParameterControl instance
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE System.Windows.Forms.UserControl InternalGetControl (pcUserControlClassName AS CHARACTER):
        
        DEFINE VARIABLE oUserControl AS System.Windows.Forms.UserControl NO-UNDO.
        
        oUserControl = DYNAMIC-NEW (pcUserControlClassName) ().
        
        RETURN oUserControl.

    END METHOD .
    
END CLASS.
