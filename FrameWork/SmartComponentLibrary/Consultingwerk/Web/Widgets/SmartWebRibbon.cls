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
    File        : SmartWebRibbon
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 05 20:46:57 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.*               FROM PROPATH . 
USING Consultingwerk.Web.*                FROM PROPATH .
USING Consultingwerk.Web.Widgets.*        FROM PROPATH .
USING Consultingwerk.Web.Widgets.Ribbon.* FROM PROPATH .
USING Progress.Lang.*                     FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.SmartWebRibbon 
    INHERITS Control
    IMPLEMENTS IPerformInput, IRaiseEvents: 

    /*------------------------------------------------------------------------------
        Purpose: Event raised when the user clicked on a Ribbon Tool 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ToolClick SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                  e AS ToolClickEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the Form that get's submitted by the Ribbon actions
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ActionForm AS CHARACTER INITIAL "myForm":U NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClickValue AS CHARACTER NO-UNDO INIT "":U
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the current selected RibbonTab                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CurrentTab AS RibbonTab NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets the list of RibbonGroups                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Tabs AS ListRibbonTab NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the TaskBarMenu                                                                
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TaskBarMenu AS TaskBarMenu NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartWebRibbon class                                                                        
        Notes:                                           
        @param pcName The name for the Control                             
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartWebRibbon (pcName AS CHARACTER):
        SUPER (pcName).
        
        ASSIGN 
            THIS-OBJECT:Tabs        = NEW ListRibbonTab () 
            THIS-OBJECT:TaskBarMenu = NEW TaskBarMenu () .
            
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs Input from the Web Browser for this Control
        Notes:   
        @return The current input value of the control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER PerformInput ():
        
        DEFINE VARIABLE cValue   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cPattern AS CHARACTER NO-UNDO.
        
        ASSIGN THIS-OBJECT:ClickValue = "":U
               cValue                 = WebUtilities:GetField ("ClickValue":U) 
               cPattern               = SUBSTITUTE ("&1.":U, THIS-OBJECT:Name) .
        
        IF cValue BEGINS cPattern THEN 
            THIS-OBJECT:ClickValue = SUBSTRING (cValue, LENGTH (cPattern) + 1) .

        RETURN THIS-OBJECT:ClickValue .        
        
    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Performs the output for this web component                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID PerformOutput ():
        
        DEFINE VARIABLE oLastToolSize         AS RibbonToolSize NO-UNDO .
        DEFINE VARIABLE oCurrentToolSize      AS RibbonToolSize NO-UNDO . 
        DEFINE VARIABLE iNormalSizeToolsCount AS INTEGER        NO-UNDO .

        IF NOT VALID-OBJECT (THIS-OBJECT:CurrentTab) AND THIS-OBJECT:Tabs:Count > 0 THEN 
            ASSIGN THIS-OBJECT:CurrentTab = THIS-OBJECT:Tabs:GetItem (1) . 
        
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('<div id="&1" class="SmartRibbonBar" actionForm="&2"><input type="hidden" class="ClickValue" name="ClickValue" />':U, 
                                        THIS-OBJECT:Name, THIS-OBJECT:ActionForm), TRUE) .
        THIS-OBJECT:OUTPUT ('    <div class="SmartRibbonInfo">':U, TRUE) . 
        THIS-OBJECT:OUTPUT ('        <div class="SmartRibbonTabContainer">':U, TRUE) .
        THIS-OBJECT:OUTPUT ('            <ul class="SmartRibbonTabs">':U, TRUE) .
        
        /* Tab Labels */
        {Consultingwerk/foreachABL.i RibbonTab oTab in THIS-OBJECT:Tabs}
            THIS-OBJECT:OUTPUT (SUBSTITUTE ('        <li id="Tab&1" class="&2"><a href="#">&3</a></li>':U, 
                                            oTab:Key,
                                            (IF THIS-OBJECT:CurrentTab = oTab THEN "SmartRibbonTab_active":U ELSE "Tab":U),
                                            oTab:Caption), TRUE) .
        END.

        THIS-OBJECT:OUTPUT ('             </ul>':U, TRUE) .
        THIS-OBJECT:OUTPUT ('        </div>':U, TRUE) .
        
        /* Right Info-Container */
        THIS-OBJECT:OUTPUT ('        <div class="TaskBarMenuContainer">':U, TRUE) .
        THIS-OBJECT:TaskBarMenu:PerformOutput() .
        THIS-OBJECT:OUTPUT ('        </div>':U, TRUE) .
        THIS-OBJECT:OUTPUT ('    </div>':U, TRUE) .
          
        /* Tab Contents */
        {Consultingwerk/foreachABL.i RibbonTab oTab in THIS-OBJECT:Tabs nodefine}

            THIS-OBJECT:OUTPUT (SUBSTITUTE ('    <div id="Tab&1Links" class="SmartRibbonLinks &2">':U,
                                            oTab:Key,
                                            (IF THIS-OBJECT:CurrentTab = oTab THEN "show":U ELSE "hidden":U)), TRUE) . 

            THIS-OBJECT:OUTPUT ('        <ul class="SmartRibbonGroups">':U, TRUE) . 
            
            {Consultingwerk/foreachABL.i RibbonGroup oGroup in oTab:Groups}

                THIS-OBJECT:OUTPUT ('            <li><ul class="SmartRibbonGroup">':U, TRUE) .

                ASSIGN iNormalSizeToolsCount = 0 . 

                {Consultingwerk/foreachABL.i ToolBase oTool in oGroup:Tools}
                
                    IF TYPE-OF (oTool, ButtonTool) THEN 
                        ASSIGN oCurrentToolSize = CAST (oTool, ButtonTool):Size .
                    
                    /* First tool is Normal or the last tool was Large */ 
                    IF oCurrentToolSize = RibbonToolSize:Normal AND 
                       (NOT VALID-OBJECT (oLastToolSize) OR oLastToolSize <> RibbonToolSize:Normal) THEN DO:  
                        THIS-OBJECT:OUTPUT ('<li><ul class="SmartRibbonLinksNormal">':U, TRUE) .
                    END.

                    IF oCurrentToolSize = RibbonToolSize:Normal THEN 
                        iNormalSizeToolsCount = iNormalSizeToolsCount + 1 . 

                    IF oCurrentToolSize <> RibbonToolSize:Normal AND 
                       oLastToolSize = RibbonToolSize:Normal THEN DO: 
                        ASSIGN iNormalSizeToolsCount = 0 . 
                        THIS-OBJECT:OUTPUT ('</ul></li>':U, TRUE) .
                    END.

                    IF iNormalSizeToolsCount > 3 THEN DO:
                        THIS-OBJECT:OUTPUT ('</ul></li>':U, TRUE) .
                        THIS-OBJECT:OUTPUT ('<li><ul class="SmartRibbonLinksNormal">':U, TRUE) .
                        
                        ASSIGN iNormalSizeToolsCount = 1 . 
                    END.
                    
                    oTool:PerformOutput () .

                
                    ASSIGN oLastToolSize = oCurrentToolSize .
                
                END. /* foreach ToolBase */

                IF iNormalSizeToolsCount > 0 THEN 
                    THIS-OBJECT:OUTPUT ('</ul></li>':U, TRUE) .
                    
                ASSIGN oLastToolSize = ? . 

                THIS-OBJECT:OUTPUT (SUBSTITUTE ('                <li class="SmartRibbonGroupFooter"><span>&1</span></li>':U,
                                                oGroup:Caption), TRUE) . 

                THIS-OBJECT:OUTPUT ('            </ul></li>':U, TRUE) .
            END. /* foreach RibbonGroup */
            
            THIS-OBJECT:OUTPUT ('        </ul>':U, TRUE) . 
            THIS-OBJECT:OUTPUT ('    </div>':U, TRUE) .
        END. /* foreach RibbonTab */


        THIS-OBJECT:OUTPUT ('</div>':U, TRUE) /* SmartRibbonBar */.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises Events from a Web Control
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RaiseEvents ():
        
        {Consultingwerk/foreachABL.i RibbonTab oTab in THIS-OBJECT:Tabs}
            {Consultingwerk/foreachABL.i RibbonGroup oGroup in oTab:Groups}
                {Consultingwerk/foreachABL.i ToolBase oTool in oGroup:Tools}
    
                    IF oTool:Key = THIS-OBJECT:ClickValue THEN DO:
                        THIS-OBJECT:OnToolClick (NEW ToolClickEventArgs (oTool)) .
                                                
                        RETURN .                         
                    END. 
                END.
            END.
        END.
        
        FINALLY:
            THIS-OBJECT:ClickValue = "":U .
        END FINALLY.            

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the ToolClick Event
        Notes:   
        @param e The ToolClikcEventArgs with the data for the ToolClick event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnToolClick (e AS ToolClickEventArgs):
        
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "ToolClick":U) .
        
        THIS-OBJECT:ToolClick:Publish (THIS-OBJECT, e) .

    END METHOD .

END CLASS.
