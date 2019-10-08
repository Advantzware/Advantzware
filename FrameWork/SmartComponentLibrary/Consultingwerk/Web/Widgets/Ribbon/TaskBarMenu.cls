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
    File        : TaskBarMenu
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 10 09:43:52 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.Widgets.* FROM PROPATH .  
USING Consultingwerk.Web.Widgets.Ribbon.* FROM PROPATH .  
USING Progress.Lang.*                     FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.Ribbon.TaskBarMenu
    INHERITS Component: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Caption of the TaskBarMenu 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Caption AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Caption of the TaskBarMenu 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TaskBarMenuItems AS ListTaskBarMenuItem NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TaskBarMenu class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TaskBarMenu ():
        SUPER ().
        
        TaskBarMenuItems = NEW ListTaskBarMenuItem () .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs the generation of the required html code                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformOutput ():
        
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('            <span>&1</span>':U, THIS-OBJECT:Caption)) .
        
        THIS-OBJECT:OUTPUT ('            <div class="TaskBarMenuLinks hidden">':U) .
        THIS-OBJECT:OUTPUT ('                <ul>':U) .
        
        {Consultingwerk/foreachABL.i TaskBarMenuItem oItem in this-object:TaskBarMenuItems}
            
            oItem:PerformOutput () . 

        END.
        
        THIS-OBJECT:OUTPUT ('                </ul>':U) .
        THIS-OBJECT:OUTPUT ('            </div>':U) .
        
    END METHOD.

END CLASS.
