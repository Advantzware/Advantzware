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
    File        : SmartDynamicWebViewer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Apr 10 23:29:22 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Web.Widgets.* FROM PROPATH .
USING Progress.Lang.*              FROM PROPATH .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Web.Widgets.SmartDynamicWebViewer 
    INHERITS SmartWebViewer: 

    {Consultingwerk/Framework/RepositoryServices/ttViewerDesign.i}       
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartDynamicWebViewer class                                                                      
        Notes:    
        @param pcName The name of the Control                                                                     
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartDynamicWebViewer (pcName AS CHARACTER):
        SUPER (INPUT pcName).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID InitializeComponent ():
        
        DEFINE VARIABLE oEditor  AS Editor  NO-UNDO . 
        DEFINE VARIABLE oLabel   AS Label   NO-UNDO . 
        DEFINE VARIABLE oTextBox AS TextBox NO-UNDO . 
        
        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
        
        TEMP-TABLE ttViewerDesign:READ-XML ("file":U,
                                            "Consultingwerk/SmartComponentsDemo/Web/customer.viewer":U,
                                            "empty":U, ?, ?) .
        
        ASSIGN hBuffer = THIS-OBJECT:Query:GET-BUFFER-HANDLE (1) .
        
        FOR EACH ttViewerDesign:

            CASE ttViewerDesign.ControlType:
                WHEN "System.Windows.Forms.TextBox":U THEN DO:
                    IF ttViewerDesign.ControlMultiline THEN DO:
                        IF Consultingwerk.Util.BufferHelper:HasField (hBuffer,
                                                                      ttViewerDesign.BindingFields) THEN 
                            oEditor = NEW Editor (ttViewerDesign.ControlName,
                                                  hBuffer:BUFFER-FIELD (ttViewerDesign.BindingFields)).
                        ELSE 
                            oEditor = NEW Editor (ttViewerDesign.ControlName) .
                            
                        ASSIGN oEditor:Left   = INTEGER (ENTRY (1, ttViewerDesign.ControlLocation))                  
                               oEditor:Top    = INTEGER (ENTRY (2, ttViewerDesign.ControlLocation))
                               oEditor:Width  = INTEGER (ENTRY (1, ttViewerDesign.ControlSize))
                               oEditor:Height = INTEGER (ENTRY (2, ttViewerDesign.ControlSize))
                               NO-ERROR .                                   
                                            
                        THIS-OBJECT:Controls:Add (oEditor) .
                    END.
                    ELSE DO:
                        IF Consultingwerk.Util.BufferHelper:HasField (hBuffer,
                                                                      ttViewerDesign.BindingFields) THEN 
                            oTextBox = NEW TextBox (ttViewerDesign.ControlName,
                                                    hBuffer:BUFFER-FIELD (ttViewerDesign.BindingFields)).
                        ELSE 
                            oTextBox = NEW TextBox (ttViewerDesign.ControlName) .

                        ASSIGN oTextBox:Left  = INTEGER (ENTRY (1, ttViewerDesign.ControlLocation))                  
                               oTextBox:Top   = INTEGER (ENTRY (2, ttViewerDesign.ControlLocation))
                               oTextBox:Width = INTEGER (ENTRY (1, ttViewerDesign.ControlSize))
                               NO-ERROR .                 
                        THIS-OBJECT:Controls:Add (oTextBox) .
                    END.
                END.
                WHEN "System.Windows.Forms.Label":U THEN DO:
                    oLabel = NEW Label (ttViewerDesign.ControlName) .
                    ASSIGN oLabel:Left = INTEGER (ENTRY (1, ttViewerDesign.ControlLocation))                  
                           oLabel:Top = INTEGER (ENTRY (2, ttViewerDesign.ControlLocation))
                           NO-ERROR .
            
                    oLabel:ScreenValue = ttViewerDesign.ControlText .                           
                           
                    THIS-OBJECT:Controls:Add (oLabel) .                 
                END.
                
            END CASE . 
        END .
        
    END METHOD.

END CLASS.
