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
    File        : triggerend.i
    Purpose     : Placeholder for custom code in trigger blocks

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 03 11:43:06 CEST 2010
    Notes       : Added by winkitmtk.w
    
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

&IF "{1}" <> "WINDOW-CLOSE" AND "{1}" <> "ROW-DISPLAY" &THEN
&IF DEFINED (winkitactive) NE 0 &THEN

FINALLY:
    
/*    Consultingwerk.Util.WidgetHelper:SetFrameBackgroundColor                                  */
/*        (NEW Consultingwerk.Util.SetFrameBackgroundColorParameter (FRAME {&FRAME-NAME}:HANDLE,*/
/*                                                                   17, /* UltraPanel BG */    */
/*                                                                   15,                        */
/*                                                                   15,                        */
/*                                                                   17,                        */
/*                                                                   TRUE)).                    */
    
    IF VALID-OBJECT (oForm) THEN DO:
        Consultingwerk.Util.UltraToolbarsHelper:RefreshTools (oForm:ToolbarsManager) .
        
        oForm:RefreshButtons () .
    
    END.
    

&IF "{1}" EQ "DEFAULT-ACTION" &THEN        
    /* Mike Fechner, Consultingwerk Ltd. 30.10.2012
       Flag the DEFAULT-ACTION Event as handled, so that the 
       RenderedBrowseControl will not attempt to APPLY the MOUSE-SELECT-DBLCLICK event
       for the same action */
    IF VALID-HANDLE (SELF) AND 
       SELF:TYPE = Consultingwerk.WidgetTypeEnum:Browse THEN DO:
        
        Consultingwerk.WindowIntegrationKit.Controls.WinKitControls:SetDefaultActionHandled (SELF) .
        
    END.
&ENDIF           
           
END.
&ENDIF
&ENDIF
