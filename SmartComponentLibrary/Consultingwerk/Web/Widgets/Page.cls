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
    File        : Page
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 04 23:27:49 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.*        FROM PROPATH . 
USING Consultingwerk.Web.*         FROM PROPATH .
USING Consultingwerk.Web.Widgets.* FROM PROPATH .
USING Progress.Lang.*              FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.Page
    ABSTRACT 
    INHERITS Component 
    IMPLEMENTS IPage, IPageWithCustomJavaScript, IPageWithCustomStyleSheet: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of Controls                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Controls AS ListControl NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the comma delimited list of JavaScript libraries that 
                 need to be outputted with the Page 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CustomJavaScriptLibraries AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET . 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the comma delimited list of stlye sheet libraries that 
                 need to be outputted with the Page 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CustomStyleLibraries AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET .     
    
    /*------------------------------------------------------------------------------
        Purpose: The number of seconds until this page should expire from any chace
        Notes:   meta http-equiv expires
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Expires AS INTEGER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Indicates if this page will be left after processing the input
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LeavePageAfterInput AS LOGICAL NO-UNDO INIT FALSE 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the title of the Page object                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PageTitle AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets the Url for Post Requests to this page 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PostUrl AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET .   
     
    /*------------------------------------------------------------------------------
        Purpose:                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Page ():
        
        DEFINE VARIABLE cTypeName AS CHARACTER NO-UNDO.
        
        SUPER ().
    
        ASSIGN cTypeName = THIS-OBJECT:GetClass ():TypeName .
    
        ASSIGN THIS-OBJECT:Controls  = NEW ListControl () 
               THIS-OBJECT:PageTitle = cTypeName
               THIS-OBJECT:PostUrl   = ENTRY (NUM-ENTRIES (cTypeName, ".":U), cTypeName, ".":U).
        
    END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	    Purpose: Evaluates the custom libraries required by the Controls on the Page
	    Notes:   Javascript and CSS
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID EvaluateControlCustomLibraries ():

        DEFINE VARIABLE cLibraries AS CHARACTER NO-UNDO.
		
        {Consultingwerk/foreachABL.i Control oControl in THIS-OBJECT:Controls}
            IF TYPE-OF (oControl, IControlWithCustomJavaScript) THEN DO:
                
                ASSIGN cLibraries = CAST (oControl, IControlWithCustomJavaScript):CustomJavaScriptLibraries . 
                
                THIS-OBJECT:CustomJavaScriptLibraries = ListHelper:MergeLists (THIS-OBJECT:CustomJavaScriptLibraries, 
                                                                               cLibraries) .
            END.
        END.

        {Consultingwerk/foreachABL.i Control oControl in THIS-OBJECT:Controls nodefine}
            IF TYPE-OF (oControl, IControlWithCustomStyleSheet) THEN DO:
                
                ASSIGN cLibraries = CAST (oControl, IControlWithCustomStyleSheet):CustomStyleLibraries . 
                
                THIS-OBJECT:CustomStyleLibraries = ListHelper:MergeLists (THIS-OBJECT:CustomStyleLibraries, 
                                                                          cLibraries) .
            END.
        END.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Processes the browser input
        Notes:   
        @return Logical value indicating if this IPage object need to continue (output) after processing the browser input
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ProcessWebInput ():
        
        IF WebContext:REQUEST_METHOD = "POST":U THEN DO:
            {Consultingwerk/foreachABL.i Control oControl in this-object:Controls}
                IF TYPE-OF (oControl, IPerformInput) THEN 
                    CAST (oControl, IPerformInput):PerformInput () . 
            END.

            {Consultingwerk/foreachABL.i Control oControl in this-object:Controls nodefine}
                IF TYPE-OF (oControl, IRaiseEvents) THEN 
                    CAST (oControl, IRaiseEvents):RaiseEvents () . 
            END.
        END.
        
        THIS-OBJECT:EvaluateControlCustomLibraries() .
        
        RETURN NOT THIS-OBJECT:LeavePageAfterInput . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose:                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ProcessWebRequest ():
        
        {Consultingwerk/foreachABL.i Control oControl in THIS-OBJECT:Controls}
                
            oControl:PerformOutput () . 
            
/*            THIS-OBJECT:OUTPUT ("<br />", TRUE) .*/
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the Page class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC Page ():
    
        {Consultingwerk/foreachABL.i Control oControl in THIS-OBJECT:Controls}
            DELETE OBJECT oControl . 
        END.

    END DESTRUCTOR.

END CLASS.
