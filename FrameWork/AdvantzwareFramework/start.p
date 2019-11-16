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
    File        : start.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Aug 21 15:28:02 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.* FROM PROPATH .

DEFINE VARIABLE oForm      AS Consultingwerk.WindowIntegrationKit.Samples.SampleMDIContainer NO-UNDO . 
DEFINE VARIABLE oLocalizer AS Consultingwerk.Windows.Localization.ILocalizer                 NO-UNDO . 

DEFINE VARIABLE oF10KeyHandler AS Consultingwerk.Support.F10KeyMessageFilter                 NO-UNDO .  

/* ***************************  Main Block  *************************** */

/* Mike Fechner, Consultingwerk Ltd. 07.11.2011
   Load services using ServiceLoader */
DEFINE VARIABLE oLoader AS ServiceLoader NO-UNDO .

oLoader = NEW ServiceLoader () .
oLoader:Load ("Consultingwerk/WindowIntegrationKit/Samples/services.xml":U) .

DELETE OBJECT oLoader .

/* Mike Fechner, Consultingwerk Ltd. 04.03.2012
   Perform Infragistics UI Translation */
oLocalizer = NEW Consultingwerk.Windows.Localization.Infragistics.DE () .
oLocalizer:Localize() .
oLocalizer = ? . /* Let the GC do it's work when the ILocalizer is no longer required */

oForm = NEW Consultingwerk.WindowIntegrationKit.Samples.SampleMDIContainer () .

oF10KeyHandler = NEW Consultingwerk.Support.F10KeyMessageFilter () .
oF10KeyHandler:F10KeyPressed:Subscribe ("F10KeyHandler":U) . 

System.Windows.Forms.Application:AddMessageFilter (oF10KeyHandler ) .

/*WAIT-FOR System.Windows.Forms.Application:RUN (oForm) .*/
oForm:Show () . 

Consultingwerk.Design.WinKit.DesignManager:Close() .

CATCH fioex AS System.IO.FileNotFoundException:
    DEFINE VARIABLE cStack AS CHARACTER NO-UNDO.
    
    IF SESSION:ERROR-STACK-TRACE = TRUE THEN DO:
        cStack = ENTRY (NUM-ENTRIES (fioex:CallStack, CHR(10)), fioex:CallStack, CHR(10)) .
        
        IF ENTRY (1, cStack, " ":U) = "InitializeComponent":U THEN DO:
            Consultingwerk.Util.MessageFormHelper:ShowMessage 
                (SUBSTITUTE ("Error loading .NET Assembly:&1&1&2",
                             System.Environment:NewLine,
                             fioex:FileName),
                "WinKit Sample application",
                Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageError) .

            LEAVE .                     
        END.
    END.
    
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (fioex) .
END CATCH.

CATCH err AS Progress.Lang.Error:
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .		
END CATCH.

PROCEDURE F10KeyHandler:
    DEFINE INPUT PARAMETER sender AS System.Object                                 NO-UNDO . 
    DEFINE INPUT PARAMETER e      AS Consultingwerk.Support.F10KeyPressedEventArgs NO-UNDO . 
    
    /* Test if Message is for the currently focussed ABL Widget */
    IF VALID-HANDLE (FOCUS) AND FOCUS:HWND = e:HWnd THEN 
        APPLY "F10":U TO FOCUS .     
    
END PROCEDURE .   
