 /**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : Checkbox
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Sebastian Düngel / Consultingwerk Ltd.
    Created     : Wed Feb 19 10:46:58 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Web.*         FROM PROPATH. 
USING Consultingwerk.Web.Widgets.* FROM PROPATH.
USING Progress.Lang.*              FROM PROPATH.

CLASS Consultingwerk.Web.Widgets.Checkbox
    INHERITS Control
    IMPLEMENTS IDataBindable, IPerformInput: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the handle of the Buffer Field this GridColumn is bound to                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BufferField AS HANDLE NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the data type                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DataType AS CHARACTER NO-UNDO INIT "LOGICAL":U
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current screen value of the Checkbox                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ScreenValue AS CHARACTER NO-UNDO 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Checkbox class                                                                     
        Notes:        
        @param pcName The Name of the Component                                                                
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Checkbox (pcName AS CHARACTER):
        
        SUPER (INPUT pcName).
        
        ASSIGN THIS-OBJECT:Width = 100 
               THIS-OBJECT:Height = 21.
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Checkbox class                                                                      
        Notes:                                                                        
        @param pcName The Name of the Component                                     
        @param phBufferField The handle of the BufferField to bind to                           
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Checkbox (pcName AS CHARACTER,
                                 phBufferField AS HANDLE):
                                    
        THIS-OBJECT (pcName).
        
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBufferField ""BUFFER-FIELD"":U}.
        
        ASSIGN THIS-OBJECT:BufferField = phBufferField 
               THIS-OBJECT:DataType    = phBufferField:DATA-TYPE . 

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Assigns the current ScreenValue to the BufferField                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Assign ():
        
        IF NOT VALID-HANDLE (THIS-OBJECT:BufferField) THEN 
            RETURN . 
            
        IF THIS-OBJECT:BufferField:BUFFER-HANDLE:AVAILABLE THEN
            
            IF THIS-OBJECT:ScreenValue = "yes":U THEN 
                THIS-OBJECT:BufferField:BUFFER-VALUE = TRUE.
            ELSE
                THIS-OBJECT:BufferField:BUFFER-VALUE = FALSE.         

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs the Data Binding                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformBinding ():
        
        LOG-MANAGER:WRITE-MESSAGE (STRING (THIS-OBJECT:BufferField:BUFFER-VALUE)).
        
        IF NOT VALID-HANDLE (THIS-OBJECT:BufferField) THEN 
            RETURN . 
            
        IF THIS-OBJECT:BufferField:BUFFER-HANDLE:AVAILABLE THEN DO: 
            THIS-OBJECT:ScreenValue = STRING (THIS-OBJECT:BufferField:BUFFER-VALUE) .       
        END. 
        ELSE 
            THIS-OBJECT:ScreenValue = "FALSE":U .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs Input from the Web Browser for this Control
        Notes:   
        @return The current input value of the control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER PerformInput ():
        
        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
        
        ASSIGN cValue = WebUtilities:GetField (THIS-OBJECT:Name) .
        
        THIS-OBJECT:ScreenValue = cValue. 
        
        RETURN cValue .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs output for this Control to the web stream                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID PerformOutput ():
        
        DEFINE VARIABLE cChecked AS CHARACTER INIT "":U NO-UNDO.
        
        IF THIS-OBJECT:ScreenValue = "yes":U THEN 
            ASSIGN cChecked = 'checked="checked"':U.
        
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('<input type="checkbox" name="&5" &1 value="yes" style="top: &2px; left: &3px; width: &4px">':U,
                                        cChecked,
                                        THIS-OBJECT:Top,
                                        THIS-OBJECT:Left,
                                        THIS-OBJECT:Width,
                                        THIS-OBJECT:NAME), TRUE) .

    END METHOD.
END CLASS.