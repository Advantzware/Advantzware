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
    File        : SmartWebViewer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Apr 10 23:26:17 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Web.*         FROM PROPATH .
USING Consultingwerk.Web.Widgets.* FROM PROPATH .
USING Progress.Lang.*              FROM PROPATH .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Web.Widgets.SmartWebViewer 
    INHERITS Control
    IMPLEMENTS IPerformInput, IRaiseEvents: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of Controls                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Controls AS ListControl NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the handle of the Query                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Query AS HANDLE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the title of the viewer                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ViewerTitle AS CHARACTER NO-UNDO 
	GET.
	SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartWebViewer class                                                                        
        Notes:
        @param pcName The name for the Control                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartWebViewer (pcName AS CHARACTER):
        SUPER (INPUT pcName).
        
        ASSIGN THIS-OBJECT:Controls  = NEW ListControl ()
               THIS-OBJECT:Height    = 400 .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeComponent ():
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs the input for this component 
        Notes:   
        @return Not used
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER PerformInput ():
        
        IF WebContext:REQUEST_METHOD = "POST":U THEN DO:
            {Consultingwerk/foreachABL.i Control oControl in this-object:Controls}
                IF TYPE-OF (oControl, IPerformInput) THEN 
                    CAST (oControl, IPerformInput):PerformInput () . 
            END.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs the output for this web component                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID PerformOutput ():

        IF NOT THIS-OBJECT:ViewerTitle = "":U THEN 
            THIS-OBJECT:OUTPUT (SUBSTITUTE ('<div id="&1_Title" class="SmartDataViewerTitle"><a>&2</a></div>':U, THIS-OBJECT:Name, THIS-OBJECT:ViewerTitle)) .

        THIS-OBJECT:OUTPUT (SUBSTITUTE ('<div id="&1" class="SmartDataViewer" style="height: &2px">':U, THIS-OBJECT:Name, THIS-OBJECT:Height), TRUE) .

        {Consultingwerk/foreachABL.i Control oControl in THIS-OBJECT:Controls}
            IF TYPE-OF (oControl, IDataBindable) THEN
                CAST (oControl, IDataBindable):PerformBinding () .

            oControl:PerformOutput () .
        END.

        THIS-OBJECT:OUTPUT ('</div>':U, TRUE) .


    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises events in the Child Controls that support the IRaiseEvents
                 Interface 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RaiseEvents ():
        
        IF WebContext:REQUEST_METHOD = "POST":U THEN DO:
            {Consultingwerk/foreachABL.i Control oControl in this-object:Controls}
                IF TYPE-OF (oControl, IRaiseEvents) THEN 
                    CAST (oControl, IRaiseEvents):RaiseEvents () . 
            END.
        END.
        
    END METHOD .

END CLASS.