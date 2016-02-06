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
    File        : AeroGlass
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Mar 10 18:56:49 CET 2010
    Notes       : http://www.codeproject.com/KB/vista/AeroGlassForms.aspx
                  http://www.mycsharp.de/wbb2/thread.php?threadid=80531
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

USING Progress.Lang.* FROM ASSEMBLY .

CLASS Consultingwerk.Windows.API.AeroGlass: 

    /*------------------------------------------------------------------------------
        Purpose: Extends the window frame into the client area.                                                                     
        Notes:   http://msdn.microsoft.com/en-us/library/aa969512(v=vs.85).aspx   
        @param hwnd The handle to the window in which the frame will be extended into the client area.
        @param margins A pointer to a MARGINS structure that describes the margins to use when extending the frame into the client area.                                                                     
    ------------------------------------------------------------------------------*/
    PROCEDURE DwmExtendFrameIntoClientArea EXTERNAL "dwmapi.dll":U:
      DEFINE INPUT PARAMETER hwnd  AS LONG.
      DEFINE INPUT PARAMETER margins AS MEMPTR. 
    END PROCEDURE .     

    /*------------------------------------------------------------------------------
        Purpose: Obtains a value that indicates whether Desktop Window Manager (DWM) 
                 composition is enabled.                                                                     
        Notes:   http://msdn.microsoft.com/en-us/library/aa969518(v=vs.85).aspx                                                                        
    ------------------------------------------------------------------------------*/
    PROCEDURE DwmIsCompositionEnabled EXTERNAL "dwmapi.dll":U:
      DEFINE OUTPUT PARAMETER lSupported AS MEMPTR. 
    END PROCEDURE .     

    /*------------------------------------------------------------------------------
        Purpose: Obtains a value that indicates whether Desktop Window Manager (DWM) 
                 composition is enabled.                                                                   
        Notes:   http://msdn.microsoft.com/en-us/library/aa969518(v=vs.85).aspx
        @return Logical value indicating if Aero glass effects are supported                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsGlassSupported ():
        
        DEFINE VARIABLE lSupported AS LOGICAL NO-UNDO.
        DEFINE VARIABLE mResult AS MEMPTR NO-UNDO .

        IF System.Environment:OSVersion:Version:Major < 6 THEN 
            RETURN FALSE . 

        SET-SIZE (mResult) = 1 . 

        RUN DwmIsCompositionEnabled (OUTPUT mResult) . 

        ASSIGN lSupported = GET-BYTE (mResult, 1) = 1 .

        RETURN lSupported . 

        FINALLY: 
            SET-SIZE (mResult) = 0 . 
        END FINALLY.    

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Extends the transparent region of a window / form                                                                        
        Notes:   http://msdn.microsoft.com/en-us/library/aa969512(v=vs.85).aspx
        @param piHwnd The HWND of the Window
        @param piLeft The number of pixels from the left border 
        @param piRight The number of pixels from the right border
        @param piTop The number of pixels form the Top border
        @param piBottom The number of pixels from the Bottom border                                                                         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SetGlassMargins (piHwnd   AS INTEGER,
                                               piLeft   AS INTEGER,
                                               piRight  AS INTEGER,
                                               piTop    AS INTEGER,
                                               piBottom AS INTEGER):
        
        DEFINE VARIABLE mmargins AS MEMPTR NO-UNDO.
        
        IF System.Environment:OSVersion:Version:Major < 6 THEN 
            RETURN . 
        
        /* build structure required for DwmExtendFrameIntoClientArea */
        SET-SIZE (mmargins) = 16 .     
            
        PUT-LONG (mmargins, 1) = piLeft . 
        PUT-LONG (mmargins, 5) = piRight . 
        PUT-LONG (mmargins, 9) = piTop . 
        PUT-LONG (mmargins, 13) = piBottom . 
            
        RUN DwmExtendFrameIntoClientArea (piHwnd, mmargins) . 
        
        FINALLY:
            SET-SIZE (mmargins) = 0 . 
        END FINALLY.

    END METHOD.

END CLASS.