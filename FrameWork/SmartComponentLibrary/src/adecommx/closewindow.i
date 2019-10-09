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
    File        : src/adecommx/closewindow.i
    Purpose     : 

    Syntax      :

    Description : Cleans up the MdiChild Form an frees ressources.
                  Use before RUN disable_UI. !!!

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 03 22:00:31 CET 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

/* Mike Fechner, Consultingwerk Ltd. 08.07.2009
   Conditional compile rule: Will compile in OpenEdge release 10.2A and above,
   This rule will break when release 40.x will be reached. Note PROVERSION
   returns a string, so this is alpha comparison, not numeric */
&IF DEFINED (winkitactive) NE 0 &THEN
    IF VALID-OBJECT(oForm) THEN DO:

        /* When using Tab Folders, close those before closing the Form */
        IF TYPE-OF (CAST(oFormControl, System.Windows.Forms.Form), 
                    Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowTabFolderForm) THEN
            CAST (CAST(oFormControl, System.Windows.Forms.Form), 
                  Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowTabFolderForm):CloseTabs () .
            
        oForm:CloseFromCode () .
            
        IF NOT oForm:ShowAsDialog THEN DO:
            oFormControl:Dispose() .
    
            IF VALID-OBJECT(oForm) THEN
                DELETE OBJECT oForm .               

            oFormControl = ? .
            oForm = ? .                        
        END.    
        /* Mike Fechner, Consultingwerk Ltd. 14.02.2013
           to receive the focus after closing an Embedded ABL Dialog */
/*        ELSE DO:                                                                                                                   */
/*             Consultingwerk.Windows.API.Win32:PostMessage(Consultingwerk.Framework.FrameworkSettings:MdiContainer:Handle:ToInt32(),*/
/*                                                         Consultingwerk.Windows.API.WinUser:WM_LBUTTONDOWN,                        */
/*                                                         0,                                                                        */
/*                                                         0) .                                                                      */
/*                                                                                                                                   */
/*             Consultingwerk.Windows.API.Win32:PostMessage(Consultingwerk.Framework.FrameworkSettings:MdiContainer:Handle:ToInt32(),*/
/*                                                          Consultingwerk.Windows.API.WinUser:WM_SETFOCUS,                          */
/*                                                          0,                                                                       */
/*                                                          0) .                                                                     */
/*                                                                                                                                   */
/*             Consultingwerk.Windows.API.Win32:PostMessage(Consultingwerk.Framework.FrameworkSettings:MdiContainer:Handle:ToInt32(),*/
/*                                                         Consultingwerk.Windows.API.WinUser:WM_LBUTTONUP,                          */
/*                                                         0,                                                                        */
/*                                                         0) .                                                                      */
/*        END.                                                                                                                       */
    END.
&ENDIF    
