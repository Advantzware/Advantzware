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
    File        : EmbeddedWindowHelper
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Mar 27 12:27:03 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.WindowIntegrationKit.Forms.* FROM PROPATH . 
USING Progress.Lang.*                             FROM PROPATH .

CLASS Consultingwerk.WindowIntegrationKit.EmbeddedWindowHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE EmbeddedWindowHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Activates the EmbeddedWindowForm associated with a procedure handle                                                                       
        Notes:
        @param phProcedure The handle of the procedure that should be activated on the UI                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ActivateByProcedureHandle (phProcedure AS HANDLE):
        
        DEFINE VARIABLE oForm        AS IEmbeddedWindowForm   NO-UNDO . 
        DEFINE VARIABLE oFormControl AS Progress.Windows.Form NO-UNDO .
        
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phProcedure Consultingwerk.WidgetTypeEnum:Procedure} .
        
        oForm = DYNAMIC-FUNCTION ("getEmbeddedWindowForm":U IN phProcedure) NO-ERROR .
         
        IF VALID-OBJECT (oForm) THEN DO:
            oFormControl = CAST (CAST (oForm, Progress.Lang.Object), Progress.Windows.Form) . 
          
            Consultingwerk.Util.FormHelper:ActivateForm (oFormControl) .
        END.  

        FINALLY:
            ERROR-STATUS:ERROR = FALSE NO-ERROR  .        
        END FINALLY.
          
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Activates the EmbeddedWindowForm associated with an embedded handle                                                                      
        Notes:
        @param phWindow The handle of the window that should be activated on the UI   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ActivateByWindowHandle (phWindow AS HANDLE):
        
        DEFINE VARIABLE oForm        AS IEmbeddedWindowForm   NO-UNDO . 
        DEFINE VARIABLE oFormControl AS Progress.Windows.Form NO-UNDO .
        
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phWindow Consultingwerk.WidgetTypeEnum:Window} .
        
        oForm = WinKitForms:FromEmbeddedWindow (phWindow) .
         
        IF VALID-OBJECT (oForm) THEN DO:
            oFormControl = CAST (CAST (oForm, Progress.Lang.Object), Progress.Windows.Form) . 
          
            Consultingwerk.Util.FormHelper:ActivateForm (oFormControl) .
        END.  

        FINALLY:
            ERROR-STATUS:ERROR = FALSE NO-ERROR  .      
        END FINALLY.

    END METHOD.

END CLASS.
