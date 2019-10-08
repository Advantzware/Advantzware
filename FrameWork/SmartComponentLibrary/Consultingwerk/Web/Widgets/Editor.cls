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
    File        : Editor
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Apr 10 23:35:59 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*             FROM PROPATH . 
USING Consultingwerk.Web.*         FROM PROPATH . 
USING Consultingwerk.Web.Widgets.* FROM PROPATH .
USING Progress.Lang.*              FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.Editor 
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
    DEFINE PUBLIC PROPERTY DataType AS CHARACTER NO-UNDO INIT "CHARACTER":U 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the column format                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Format AS CHARACTER NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current screen value of the Editor                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ScreenValue AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Editor class                                                                     
        Notes:            
        @param pcName The Name of the Control                                                                  
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Editor (pcName AS CHARACTER):
        SUPER (INPUT pcName).
        
        ASSIGN THIS-OBJECT:Width = 100 
               THIS-OBJECT:Height = 21.
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the GridColumn class                                                                      
        Notes:                                                                        
        @param pcName The Name of the Control
        @param phBufferField The handle of the BufferField to bind to                                                                  
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Editor (pcName AS CHARACTER,
                               phBufferField AS HANDLE):
                                   
        THIS-OBJECT (pcName).
        
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBufferField ""BUFFER-FIELD"":U}.
        
        ASSIGN THIS-OBJECT:BufferField = phBufferField 
               THIS-OBJECT:DataType    = phBufferField:DATA-TYPE
               THIS-OBJECT:Format      = phBufferField:FORMAT . 

    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Assigns the current ScreenValue to the BufferField                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Assign ():
        
        IF NOT VALID-HANDLE (THIS-OBJECT:BufferField) THEN 
            RETURN . 
            
        IF THIS-OBJECT:BufferField:BUFFER-HANDLE:AVAILABLE THEN  
            THIS-OBJECT:BufferField:BUFFER-VALUE = THIS-OBJECT:ScreenValue .        

    END METHOD .
        
    /*------------------------------------------------------------------------------
        Purpose:                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformBinding ():
        
        IF NOT VALID-HANDLE (THIS-OBJECT:BufferField) THEN 
            RETURN . 
            
        IF THIS-OBJECT:BufferField:BUFFER-HANDLE:AVAILABLE THEN DO: 
            IF THIS-OBJECT:Format > "":U THEN 
                THIS-OBJECT:ScreenValue = STRING (THIS-OBJECT:BufferField:BUFFER-VALUE,
                                                  THIS-OBJECT:Format) .
            ELSE . 
                THIS-OBJECT:ScreenValue = STRING (THIS-OBJECT:BufferField:BUFFER-VALUE) .       
        END. 
        ELSE 
            THIS-OBJECT:ScreenValue = "?":U .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs Input from the Web Browser for this Control
        Notes:   
        @return The current input value of the control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER PerformInput ():
        
        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
        
        ASSIGN cValue = WebUtilities:GetField (THIS-OBJECT:Name) .
        
        THIS-OBJECT:ScreenValue = cValue . 
        
        RETURN cValue .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs output for this Control to the web stream                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID PerformOutput ():
        
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('<textarea name="&1" style="top: &3px; left: &4px; width: &5px; height: &6px">&2</textarea>':U,
                                        THIS-OBJECT:Name,
                                        THIS-OBJECT:HtmlEncode (THIS-OBJECT:ScreenValue),
                                        THIS-OBJECT:Top,
                                        THIS-OBJECT:Left,
                                        THIS-OBJECT:Width,
                                        THIS-OBJECT:Height), TRUE) .

    END METHOD.

END CLASS.
