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
    File        : IPage
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 04 22:50:54 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Web.*         FROM PROPATH .
USING Consultingwerk.Web.Widgets.* FROM PROPATH .
USING Progress.Lang.*              FROM PROPATH .

INTERFACE Consultingwerk.Web.Widgets.IPage:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the List of Controls                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Controls AS ListControl NO-UNDO 
    GET.
      
    /*------------------------------------------------------------------------------
        Purpose: The number of seconds until this page should expire from any chace
        Notes:   meta http-equiv expires
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Expires AS INTEGER NO-UNDO 
    GET.
    SET. 

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
     
    /*------------------------------------------------------------------------------
        Purpose: Processes the browser input
        Notes:   
        @return Logical value indicating if this IPage object need to continue (output) after processing the browser input
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ProcessWebInput ().

    /*------------------------------------------------------------------------------
        Purpose: Processes the Web Request using the Page object                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ProcessWebRequest ().

END INTERFACE.