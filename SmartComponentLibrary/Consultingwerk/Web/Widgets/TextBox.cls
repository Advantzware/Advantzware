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
    File        : TextBox
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Apr 10 23:35:59 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.*         FROM PROPATH . 
USING Consultingwerk.Web.Widgets.* FROM PROPATH .
USING Progress.Lang.*              FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.TextBox 
    INHERITS Control
    IMPLEMENTS IDataBindable, IPerformInput: 

    /*------------------------------------------------------------------------------
        Purpose: Allows to define the autocomplete attribute on the textbox
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AutoComplete AS LOGICAL INITIAL ? NO-UNDO 
    GET.
    SET. 

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
    SET (pcValue AS CHARACTER):
        
        IF THIS-OBJECT:TextBoxType = TextBoxTypeEnum:TEXT THEN DO:
            CASE pcValue:
                WHEN "DATETIME":U OR WHEN "DATETIME-TZ":U THEN
                    ASSIGN THIS-OBJECT:TextBoxType = TextBoxTypeEnum:Datetime.
                
                WHEN "DATE":U THEN
                    ASSIGN THIS-OBJECT:TextBoxType = TextBoxTypeEnum:Date.
                     
                WHEN "DECIMAL":U OR WHEN "INT":U OR WHEN "INT64":U THEN
                    ASSIGN THIS-OBJECT:TextBoxType = TextBoxTypeEnum:Number.
                    
            END CASE.
        END.
             
    END. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the column format                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Format AS CHARACTER NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the TextBox should be read only
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ReadOnly AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current screen value of the TextBox                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ScreenValue AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the TextBoxType value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TextBoxType AS TextBoxTypeEnum NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Label class                                                                     
        Notes:                                                                        
        @param pcName The name for the Control                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TextBox (pcName AS CHARACTER):
        SUPER (INPUT pcName).
        
        ASSIGN THIS-OBJECT:Width = 100 
               THIS-OBJECT:Height = 21.
        
        THIS-OBJECT:TextBoxType = TextBoxTypeEnum:Text .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the GridColumn class                                                                      
        Notes:
        @param pcName The name for the Control                                                                        
        @param phBufferField The handle of the BufferField to bind to                                                                                    
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TextBox (pcName AS CHARACTER,
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
        
        DEFINE VARIABLE cAutoComplete AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cReadOnly     AS CHARACTER NO-UNDO.
        
        IF THIS-OBJECT:AutoComplete = TRUE THEN
            ASSIGN cAutoComplete = ' autocomplete="on"':U .
        ELSE  
            IF THIS-OBJECT:AutoComplete = FALSE THEN
                ASSIGN cAutoComplete = ' autocomplete="off"':U .

        IF THIS-OBJECT:ReadOnly = TRUE THEN
            ASSIGN cAutoComplete = ' readonly="readonly"':U .
        ELSE  
            ASSIGN cReadOnly = '':U .
        
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('<input type="&5" name="&6" style="top: &2px; left: &3px; width: &4px" value="&1"&7&8>':U,
                                        THIS-OBJECT:HtmlEncode (THIS-OBJECT:ScreenValue),
                                        THIS-OBJECT:Top,
                                        THIS-OBJECT:Left,
                                        THIS-OBJECT:Width,
                                        LC (THIS-OBJECT:TextBoxType:ToString()),
                                        THIS-OBJECT:Name,
                                        cAutoComplete,
                                        cReadOnly), TRUE) .

    END METHOD.

END CLASS.