/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *    OEHIVE                                                          *
 **********************************************************************/  
/*------------------------------------------------------------------------
    File        : Win32
    Purpose     : A wrapper to Win32 API access
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 06 00:07:05 CET 2010
    Notes       : Some of the methods here are are based on original proceudres
                  found at: http://www.oehive.org/win32api
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Assertion.*              FROM PROPATH .
USING Consultingwerk.Framework.*              FROM PROPATH . 
USING Consultingwerk.Windows.API.*            FROM PROPATH . 
USING Consultingwerk.Windows.API.Exceptions.* FROM PROPATH.
USING Microsoft.Win32.*                       FROM ASSEMBLY.
USING Progress.Lang.*                         FROM ASSEMBLY .

/* data types */
&GLOBAL-DEFINE HWND LONG
&GLOBAL-DEFINE BOOL LONG
&GLOBAL-DEFINE HINSTANCE LONG
&GLOBAL-DEFINE INT LONG
&GLOBAL-DEFINE INTSIZE 4

/* libraries */
&GLOBAL-DEFINE USER     "user32":U
&GLOBAL-DEFINE KERNEL   "kernel32":U
&GLOBAL-DEFINE SHELL    "shell32":U
&GLOBAL-DEFINE MAPI     "mapi32":U
&GLOBAL-DEFINE GDI      "gdi32":U
&GLOBAL-DEFINE MMEDIA   "winmm":U
&GLOBAL-DEFINE WINSPOOL "winspool.drv":U
&GLOBAL-DEFINE ADVAPI   "advapi32":U
&GLOBAL-DEFINE A A

/* menu manipulation */
&GLOBAL-DEFINE MF_BYPOSITION 1024
&GLOBAL-DEFINE MF_REMOVE     256

&SCOPED-DEFINE WM_SETREDRAW      0x000B

/* PeekMessage */
&SCOPED-DEFINE PM_NOREMOVE 0

CLASS Consultingwerk.Windows.API.Win32: 
    
    /*------------------------------------------------------------------------------
        Purpose: Disables the window ghosting feature for the calling GUI process. 
                 Window ghosting is a Windows Manager feature that lets the user minimize, 
                 move, or close the main window of an application that is not responding.
                 After calling DisableProcessWindowsGhosting, the ghosting feature is 
                 disabled for the duration of the process.
                 VOID WINAPI DisableProcessWindowsGhosting(void);                                                    
        Notes:   http://msdn.microsoft.com/en-us/library/ms648415(v=vs.85).aspx                                                                    
    ------------------------------------------------------------------------------*/    
    PROCEDURE DisableProcessWindowsGhosting EXTERNAL {&USER}:
    END PROCEDURE.    
    
    /*------------------------------------------------------------------------------
        Purpose: Redraws the menu bar of the specified window. If the menu bar changes 
                 after the system has created the window, this function must be called 
                 to draw the changed menu bar.
                 BOOL WINAPI DrawMenuBar(__in  HWND hWnd);                                                    
        Notes:   http://msdn.microsoft.com/en-us/library/windows/desktop/ms647633(v=vs.85).aspx                                                                    
    ------------------------------------------------------------------------------*/    
    PROCEDURE DrawMenuBar EXTERNAL {&USER}:
        DEFINE INPUT  PARAMETER hMenu      AS  LONG.
        DEFINE RETURN PARAMETER iRetCode   AS  LONG.
    END PROCEDURE.
    
    /*------------------------------------------------------------------------------
        Purpose: Retrieves a handle to the desktop window. The desktop window covers 
                 the entire screen. The desktop window is the area on top of which 
                 other windows are painted.    
        Notes:   http://msdn.microsoft.com/en-us/library/windows/desktop/ms633504%28v=vs.85%29.aspx                                                                   
    ------------------------------------------------------------------------------*/       
    PROCEDURE GetDesktopWindow EXTERNAL {&USER}:
        DEFINE RETURN PARAMETER piResult     AS LONG NO-UNDO.
    END PROCEDURE.    
    
    /*------------------------------------------------------------------------------
        Purpose: Determines the number of items in the specified menu.
                 int WINAPI GetMenuItemCount(__in_opt  HMENU hMenu);                                                    
        Notes:   http://msdn.microsoft.com/en-us/library/windows/desktop/ms647978(v=vs.85).aspx                                                                   
    ------------------------------------------------------------------------------*/       
    PROCEDURE GetMenuItemCount EXTERNAL {&USER}:
        DEFINE INPUT  PARAMETER hMenu    AS  LONG.
        DEFINE RETURN PARAMETER iCount   AS  LONG.
    END PROCEDURE.
    
    /*------------------------------------------------------------------------------
        Purpose: Retrieves a handle to the specified window's parent or owner.
                 HWND WINAPI GetParent(__in  HWND hWnd);                                                   
        Notes:   http://msdn.microsoft.com/en-us/library/windows/desktop/ms633510(v=vs.85).aspx                                                                 
    ------------------------------------------------------------------------------*/ 
    PROCEDURE GetParent EXTERNAL {&USER}:
        DEFINE INPUT  PARAMETER thishwnd   AS {&HWND}.
        DEFINE RETURN PARAMETER parenthwnd AS {&HWND}.
    END PROCEDURE.
    
    /*------------------------------------------------------------------------------
        Purpose: Enables the application to access the window menu (also known as the 
                 system menu or the control menu) for copying and modifying.                   
                 HMENU WINAPI GetSystemMenu(__in  HWND hWnd,
                                            __in  BOOL bRevert);                                 
        Notes:   http://msdn.microsoft.com/en-us/library/windows/desktop/ms647985(v=vs.85).aspx                                                                  
    ------------------------------------------------------------------------------*/     
    PROCEDURE GetSystemMenu EXTERNAL {&USER}:
        DEFINE INPUT  PARAMETER hwnd    AS {&HWND}.
        DEFINE INPUT  PARAMETER bRevert AS {&BOOL}.
        DEFINE RETURN PARAMETER hMenu   AS {&INT}.
    END PROCEDURE.
        
    /*------------------------------------------------------------------------------
        Purpose: Retrieves the name of the user or other security principal associated 
                 with the calling thread. You can specify the format of the returned name.                   
                 BOOLEAN WINAPI GetUserNameEx(__in     EXTENDED_NAME_FORMAT NameFormat,
                                              __out    LPTSTR lpNameBuffer,
                                              __inout  PULONG lpnSize);                                 
        Notes:   http://msdn.microsoft.com/en-us/library/windows/desktop/ms724435(v=vs.85).aspx                                                               
    ------------------------------------------------------------------------------*/   
    PROCEDURE GetUserNameExA EXTERNAL "Secur32.dll":U:
        DEFINE INPUT        PARAMETER NameFormat AS SHORT NO-UNDO . 
        DEFINE OUTPUT       PARAMETER phBuffer AS MEMPTR NO-UNDO . 
        DEFINE INPUT-OUTPUT PARAMETER piSize AS SHORT NO-UNDO . 
    END PROCEDURE .        
        
    /*------------------------------------------------------------------------------
        Purpose: Retrieves the identifier of the thread that created the specified window 
                 and, optionally, the identifier of the process that created the window.
                 DWORD WINAPI GetWindowThreadProcessId(_In_       HWND hWnd,
                                                       _Out_opt_  LPDWORD lpdwProcessId);                                 
        Notes:   http://msdn.microsoft.com/en-us/library/windows/desktop/ms633522(v=vs.85).aspx
    ------------------------------------------------------------------------------*/   
    PROCEDURE GetWindowThreadProcessId EXTERNAL {&USER}:
        DEFINE INPUT PARAMETER intWindowHwnd AS LONG NO-UNDO .
        DEFINE OUTPUT PARAMETER pProcessID   AS LONG NO-UNDO .
        DEFINE RETURN PARAMETER hResult      AS LONG NO-UNDO .
    END PROCEDURE.
        
    /*------------------------------------------------------------------------------
        Purpose: Dispatches incoming sent messages, checks the thread message queue 
                 for a posted message, and retrieves the message (if any exist).
                 BOOL WINAPI PeekMessage(_Out_     LPMSG lpMsg,
                                         _In_opt_  HWND hWnd,
                                         _In_      UINT wMsgFilterMin,
                                         _In_      UINT wMsgFilterMax,
                                         _In_      UINT wRemoveMsg);                                 
        Notes:   http://msdn.microsoft.com/en-us/library/windows/desktop/ms644943(v=vs.85).aspx
                 http://blog.excastle.com/2005/08/15/telling-windows-were-not-really-not-responding/
                 https://community.progress.com/technicalusers/f/19/p/9232/35351.aspx?Redirected=true#35351 
    ------------------------------------------------------------------------------*/   
    PROCEDURE PeekMessageA EXTERNAL {&USER}:
        DEFINE INPUT  PARAMETER lpmsg         AS MEMPTR.
        DEFINE INPUT  PARAMETER hWnd          AS LONG.
        DEFINE INPUT  PARAMETER wMsgFilterMin AS LONG.
        DEFINE INPUT  PARAMETER wMsgFilterMax AS LONG.
        DEFINE INPUT  PARAMETER wRemoveMsg    AS LONG.
        DEFINE RETURN PARAMETER lResult       AS LONG.
    END PROCEDURE.        
        
    /*------------------------------------------------------------------------------
        Purpose: Deletes a menu item or detaches a submenu from the specified menu. 
                 If the menu item opens a drop-down menu or submenu, RemoveMenu does 
                 not destroy the menu or its handle, allowing the menu to be reused. 
                 Before this function is called, the GetSubMenu function should retrieve 
                 a handle to the drop-down menu or submenu.                                    
                 BOOL WINAPI RemoveMenu(__in  HMENU hMenu,
                                        __in  UINT uPosition,
                                        __in  UINT uFlags);                
        Notes:   http://msdn.microsoft.com/en-us/library/windows/desktop/ms647994(v=vs.85).aspx                                                                  
    ------------------------------------------------------------------------------*/         
    PROCEDURE RemoveMenu EXTERNAL {&USER}:
        DEFINE INPUT  PARAMETER hMenu      AS  LONG.
        DEFINE INPUT  PARAMETER nPosition  AS  LONG.
        DEFINE INPUT  PARAMETER wFlags     AS  LONG.
        DEFINE RETURN PARAMETER iRetCode   AS  LONG.
    END PROCEDURE.

    /*------------------------------------------------------------------------------
        Purpose: The LockWindowUpdate function disables or enables drawing in the 
                 specified window. Only one window can be locked at a time.            
                 BOOL WINAPI PostMessage(__in_opt  HWND hWnd,
                                         __in      UINT Msg,
                                         __in      WPARAM wParam,
                                         __in      LPARAM lParam);                                                         
        Notes:   http://msdn.microsoft.com/en-us/library/dd145034(v=vs.85).aspx                                                                     
    ------------------------------------------------------------------------------*/
    PROCEDURE LockWindowUpdate EXTERNAL {&USER}:
        DEFINE INPUT  PARAMETER hWndLock AS LONG.
        DEFINE RETURN PARAMETER IsLocked AS LONG.
    END PROCEDURE.

    /*------------------------------------------------------------------------------
        Purpose: Places (posts) a message in the message queue associated with the 
                 thread that created the specified window and returns without waiting 
                 for the thread to process the message.            
                 BOOL WINAPI PostMessage(__in_opt  HWND hWnd,
                                         __in      UINT Msg,
                                         __in      WPARAM wParam,
                                         __in      LPARAM lParam);                                                         
        Notes:   http://msdn.microsoft.com/en-us/library/ms644944(VS.85).aspx                                                                     
    ------------------------------------------------------------------------------*/
    PROCEDURE PostMessageA EXTERNAL {&USER}:
        DEFINE INPUT PARAMETER hwnd         AS LONG.
        DEFINE INPUT PARAMETER umsg         AS LONG.
        DEFINE INPUT PARAMETER wparam       AS LONG.
        DEFINE INPUT PARAMETER lparam       AS LONG.
        DEFINE RETURN PARAMETER ReturnValue AS LONG.
    END.

    /*------------------------------------------------------------------------------
        Purpose: Sends the specified message to a window or windows. The SendMessage 
                 function calls the window procedure for the specified window and does 
                 not return until the window procedure has processed the message.
                 To send a message and return immediately, use the SendMessageCallback 
                 or SendNotifyMessage function. To post a message to a thread's message 
                 queue and return immediately, use the PostMessage or PostThreadMessage function.
                 LRESULT WINAPI SendMessage(__in  HWND hWnd,
                                            __in  UINT Msg,
                                            __in  WPARAM wParam,
                                            __in  LPARAM lParam);                                                    
        Notes:   http://msdn.microsoft.com/en-us/library/windows/desktop/ms644950(v=vs.85).aspx                                                                  
    ------------------------------------------------------------------------------*/ 
    PROCEDURE SendMessageA EXTERNAL {&USER}:
        DEFINE INPUT  PARAMETER hwnd   AS LONG NO-UNDO.
        DEFINE INPUT  PARAMETER wmsg   AS LONG NO-UNDO.
        DEFINE INPUT  PARAMETER wparam AS LONG NO-UNDO.
        DEFINE INPUT  PARAMETER lparam AS LONG NO-UNDO.
        DEFINE RETURN PARAMETER rc     AS LONG NO-UNDO.
    END PROCEDURE.

    /*------------------------------------------------------------------------------
        Purpose: Changes the parent window of the specified child window.
                 HWND WINAPI SetParent(_In_      HWND hWndChild,
                                       _In_opt_  HWND hWndNewParent);
        Notes:   http://msdn.microsoft.com/en-us/library/windows/desktop/ms633541%28v=vs.85%29.aspx                                                                
    ------------------------------------------------------------------------------*/ 
    PROCEDURE SetParent EXTERNAL {&USER}:
        DEFINE INPUT  PARAMETER hwndChild     AS LONG .
        DEFINE INPUT  PARAMETER hwndNewParent AS LONG .
        DEFINE RETURN PARAMETER hwndOldParent AS LONG .
    END PROCEDURE.

    /*------------------------------------------------------------------------------
        Purpose: Performs an operation on a specified file.
                 HINSTANCE ShellExecute(__in_opt  HWND hwnd,
                                        __in_opt  LPCTSTR lpOperation,
                                        __in      LPCTSTR lpFile,
                                        __in_opt  LPCTSTR lpParameters,
                                        __in_opt  LPCTSTR lpDirectory,
                                        __in      INT nShowCmd);                                                   
        Notes:   http://msdn.microsoft.com/en-us/library/bb762153(v=vs.85).aspx                                                                     
    ------------------------------------------------------------------------------*/
    PROCEDURE ShellExecuteA EXTERNAL "shell32":U:
        DEFINE INPUT PARAMETER HWND AS LONG.
        DEFINE INPUT PARAMETER lpOperation AS CHARACTER.
        DEFINE INPUT PARAMETER lpFile AS CHARACTER.
        DEFINE INPUT PARAMETER lpParameters AS CHARACTER.
        DEFINE INPUT PARAMETER lpDirectory AS CHARACTER.
        DEFINE INPUT PARAMETER nShowCmd AS LONG.
        DEFINE RETURN PARAMETER hInstance AS LONG.
    END.

    /*------------------------------------------------------------------------------
        Purpose: DisableProcessWindowsGhosting, disables the window ghosting feature 
                 for the calling GUI process. Window ghosting is a Windows Manager 
                 feature that lets the user minimize, move, or close the main window 
                 of an application that is not responding.
                 After calling DisableProcessWindowsGhosting, the ghosting feature is 
                 disabled for the duration of the process.
        Notes:   http://knowledgebase.progress.com/articles/Article/000035144
                 http://msdn.microsoft.com/en-us/library/ms648415(v=vs.85).aspx
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID DisableProcessWindowsGhosting ():

        RUN DisableProcessWindowsGhosting .
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Disables the close button of a .NET Form
        Notes:   
        @param poForm The Form to disable the close button for
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID DisableWindowClose (poForm AS System.Windows.Forms.Form):
        
        DEFINE VARIABLE iHwnd AS INTEGER NO-UNDO.

        ASSIGN iHwnd = poForm:Handle:ToInt32() .

        Win32:DisableWindowClose (iHwnd) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Disables the close button of a Progress Window Widget
        Notes:   
        @param phWindow The Window to disable the close button for
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID DisableWindowClose (phWindow AS HANDLE):
        
        DEFINE VARIABLE iParent AS INTEGER NO-UNDO.
        
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phWindow ""WINDOW"":U} .

        RUN GetParent (phWindow:HWND,
                       OUTPUT iParent).        

        Win32:DisableWindowClose (iParent) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Disables the close button of a Progress Window Widget
        Notes:   
        @param piHwnd The HWND of the Window (ABL or .NET) to disable the close button for
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID DisableWindowClose (piHwnd AS INTEGER):
        
        DEFINE VARIABLE hSysMenu   AS  INTEGER NO-UNDO.
        DEFINE VARIABLE hInstance  AS  INTEGER NO-UNDO.
        DEFINE VARIABLE iRetCode   AS  INTEGER NO-UNDO.
        DEFINE VARIABLE iCnt       AS  INTEGER NO-UNDO.
 
        /* Get handle to the window's system menu
           (Restore, Maximize, Move, close etc.) */
        RUN GetSystemMenu (piHwnd, 
                           INPUT  0,
                           OUTPUT hSysMenu).
 
        IF hSysMenu NE 0 THEN DO:
            /* Get System menu's menu count */
            RUN GetMenuItemCount (INPUT hSysMenu,
                                  OUTPUT iCnt).
 
            IF iCnt NE 0 THEN DO:
                /* Menu count is based on 0 (0, 1, 2, 3...) */
         
                /* remove the "close option" */
                RUN RemoveMenu  (INPUT hSysMenu, 
                                 INPUT iCnt - 1, 
                                 INPUT {&MF_BYPOSITION} + {&MF_REMOVE},
                                 OUTPUT iRetCode).
         
                /* remove the seperator */
                RUN RemoveMenu  (INPUT hSysMenu, 
                                 INPUT iCnt - 2, 
                                 INPUT {&MF_BYPOSITION} + {&MF_REMOVE},
                                 OUTPUT iRetCode).
         
                /* Force caption bar's refresh which 
                   will disable the window close ("X") button */
                RUN DrawMenuBar  (INPUT  piHwnd,
                                  OUTPUT iRetCode).
            END.
        END.                               

    END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Enables visual cues in the window
	    Notes:   Visual cues are by default only enabled when a Window has been 
	             launched using a keyboard action. This method can be used to enable
	             visual cues for windows launched by a mouse action
	    @param piHwnd The HWND of the Window
	------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID EnableVisualCues (piHwnd AS INTEGER):
		
        Win32:PostMessage(piHwnd,
                          WinUser:WM_UPDATEUISTATE,
                          65538,
                          0).

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Enables visual cues in the window
        Notes:   Visual cues are by default only enabled when a Window has been 
                 launched using a keyboard action. This method can be used to enable
                 visual cues for windows launched by a mouse action
        @param poControl The reference to the Control (e.g. Form)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID EnableVisualCues (poControl AS System.Windows.Forms.Control):
        
        Win32:PostMessage(poControl:Handle:ToInt32(),
                          WinUser:WM_UPDATEUISTATE,
                          65538,
                          0).

    END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Returns the path to a known application from the Windows Registry
	    Notes:   Reads registry keys from 
	             HKEY_LOCAL_MACHINE\Microsoft\Windows\CurrentVersion\App Paths\AcroRd32.exe
	             Throws an exception when the application is not found
	    @param pcAppName The name of the application to retrieve, e.g. AcroRd32.exe, excel.exe
	    @return The full path name of the application executable
	------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC CHARACTER GetApplicationCommandLine (pcAppName AS CHARACTER):
		
		DEFINE VARIABLE cRegKey   AS CHARACTER   NO-UNDO .
        DEFINE VARIABLE oRegkey   AS RegistryKey NO-UNDO .
		DEFINE VARIABLE cRegValue AS CHARACTER   NO-UNDO .
		
        Assert:NotNullOrEmpty (pcAppName) .

        ASSIGN cRegKey = SUBSTITUTE ("Software\Microsoft\Windows\CurrentVersion\App Paths\&1":U, 
                                     pcAppName)     

               oRegkey = Consultingwerk.Framework.Registry:FindRegistryKeyReadOnly ("HKEY_LOCAL_MACHINE":U, cRegKey) .		
		
		IF VALID-OBJECT (oRegkey) THEN 
		    ASSIGN cRegValue = oRegkey:GetValue ("":U) . 
		
        IF cRegValue > "":U THEN 
            RETURN cRegValue .		

        UNDO, THROW NEW InvalidApplicationNameException (SUBSTITUTE ("The application name &1 could not be located!"{&TRAN}, 
                                                                     pcAppName),
                                                         0,
                                                         pcAppName) .
	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Retrieves a handle to the desktop window. The desktop window covers 
                 the entire screen. The desktop window is the area on top of which 
                 other windows are painted.    
        Notes:   http://msdn.microsoft.com/en-us/library/windows/desktop/ms633504%28v=vs.85%29.aspx                                                                   
        @return The hwnd of the desktop window
    ------------------------------------------------------------------------------*/       
    METHOD PUBLIC STATIC INTEGER GetDesktopWindow ():

        DEFINE VARIABLE iDesktopHwnd AS INTEGER NO-UNDO.
        
        RUN GetDesktopWindow (OUTPUT iDesktopHwnd) .  
        
        RETURN iDesktopHwnd . 
    END.    
    
    /*------------------------------------------------------------------------------
        Purpose: Retrieves the name of the user or other security principal associated 
                 with the calling thread. You can specify the format of the returned name.
        Notes:   
        @param poNameFormat The ExtendedNameFormatEnum value representing the user identity to return 
        @return The user identity/name requested
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetUserName (poNameFormat AS ExtendedNameFormatEnum):
        
        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iLen AS INTEGER     NO-UNDO INIT 500 .
        DEFINE VARIABLE mptr AS MEMPTR NO-UNDO . 

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poNameFormat """ExtendedNameFormatEnum"":U"} .

        SET-SIZE (mptr) = 500 . 

        RUN GetUserNameExA (INTEGER(poNameFormat:GetValue()), OUTPUT mptr, INPUT-OUTPUT iLen) .

        IF iLen > 0 THEN
            cValue = TRIM (GET-STRING (mptr, 1)) .

        RETURN cValue .

        FINALLY:
            SET-SIZE (mptr) = 0 .         
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retrieves the identifier of the process that created the specified window 
                 DWORD WINAPI GetWindowThreadProcessId(_In_       HWND hWnd,
                                                       _Out_opt_  LPDWORD lpdwProcessId);            
        Notes:   http://msdn.microsoft.com/en-us/library/windows/desktop/ms633522(v=vs.85).aspx
        @param piHwnd A handle to the window
        @return The process Id that created the Window
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC INTEGER GetWindowThreadProcessId (piHwnd AS INTEGER):
		
		DEFINE VARIABLE nRet AS INTEGER NO-UNDO . 
		DEFINE VARIABLE nPid AS INTEGER NO-UNDO . 
		
        RUN GetWindowThreadProcessId (piHwnd, OUTPUT nPid, OUTPUT nRet).

        RETURN nPid . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: The LockWindowUpdate function disables or enables drawing in the 
                 specified window. Only one window can be locked at a time.            
                 BOOL WINAPI PostMessage(__in_opt  HWND hWnd,
                                         __in      UINT Msg,
                                         __in      WPARAM wParam,
                                         __in      LPARAM lParam);                                                         
        Notes:   http://msdn.microsoft.com/en-us/library/dd145034(v=vs.85).aspx                                                                     
        @param piHwnd The HWND of the Window to lock
        @param piLocked  If the function succeeds, the return value is nonzero. If the function fails, the return value is zero, indicating that an error occurred or another window was already locked.                                                                   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID LockWindowUpdate (piHwnd AS INTEGER,
                                                OUTPUT piLocked AS INTEGER):
        
        RUN LockWindowUpdate (piHwnd, OUTPUT piLocked) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: The LockWindowUpdate function disables or enables drawing in the 
                 specified window. Only one window can be locked at a time.            
                 BOOL WINAPI PostMessage(__in_opt  HWND hWnd,
                                         __in      UINT Msg,
                                         __in      WPARAM wParam,
                                         __in      LPARAM lParam);                                                         
        Notes:   http://msdn.microsoft.com/en-us/library/dd145034(v=vs.85).aspx                                                                     
        @param piHwnd The HWND of the Window to lock
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID LockWindowUpdate (piHwnd AS INTEGER):
        
        DEFINE VARIABLE iLocked AS INTEGER NO-UNDO.
        
        RUN LockWindowUpdate (piHwnd, OUTPUT iLocked) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: The LockWindowUpdate function disables or enables drawing in the 
                 specified window. Only one window can be locked at a time.            
                 BOOL WINAPI PostMessage(__in_opt  HWND hWnd,
                                         __in      UINT Msg,
                                         __in      WPARAM wParam,
                                         __in      LPARAM lParam);                                                         
        Notes:   http://msdn.microsoft.com/en-us/library/dd145034(v=vs.85).aspx                                                                     
        @param poControl The reference to the Control who's window should be locked
        @param piLocked  If the function succeeds, the return value is nonzero. If the function fails, the return value is zero, indicating that an error occurred or another window was already locked.                                                                   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID LockWindowUpdate (poControl AS System.Windows.Forms.Control,
                                                OUTPUT piLocked AS INTEGER):
        
        RUN LockWindowUpdate (poControl:Handle:ToInt32(), OUTPUT piLocked) .

    END METHOD.
    /*------------------------------------------------------------------------------
        Purpose: The LockWindowUpdate function disables or enables drawing in the 
                 specified window. Only one window can be locked at a time.            
                 BOOL WINAPI PostMessage(__in_opt  HWND hWnd,
                                         __in      UINT Msg,
                                         __in      WPARAM wParam,
                                         __in      LPARAM lParam);                                                         
        Notes:   http://msdn.microsoft.com/en-us/library/dd145034(v=vs.85).aspx
        @param poControl The reference to the Control who's window should be locked                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID LockWindowUpdate (poControl AS System.Windows.Forms.Control):
        
        DEFINE VARIABLE iLocked AS INTEGER NO-UNDO.
        
        RUN LockWindowUpdate (poControl:Handle:ToInt32(), OUTPUT iLocked) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Retrieves a message from the Windows Message Queue without removing
                 it. Useful to show that a process is still alive without handling
                 incoming events 
        Notes:   http://msdn.microsoft.com/en-us/library/windows/desktop/ms644943(v=vs.85).aspx
                 http://blog.excastle.com/2005/08/15/telling-windows-were-not-really-not-responding/
                 https://community.progress.com/technicalusers/f/19/p/9232/35351.aspx?Redirected=true#35351 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID PeekMessage ():
		
        DEFINE VARIABLE mMsg    AS MEMPTR  NO-UNDO .
        DEFINE VARIABLE lResult AS INTEGER NO-UNDO .
        
        SET-SIZE(mMsg) = 48. /* big enough for 64-bit */
        
        /* This call to PeekMessage will not remove posted messages from the
        ** message queue. The one side effect I am aware of is that messages
        ** that were sent from another process using SendMessage will be
        ** dispatched before PeekMessage returns, but the same is true of
        ** any code that processes messages. Since you have to process
        ** messages to tell Windows you're awake, this seems like an
        ** unavoidable (and relatively rare) side effect.
        */
        RUN PeekMessageA (mMsg, 0, 0, 0, {&PM_NOREMOVE}, OUTPUT lResult).

        FINALLY:
            SET-SIZE (mMsg) = 0 . 	
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Places (posts) a message in the message queue associated with the 
                 thread that created the specified window and returns without waiting 
                 for the thread to process the message.            
        Notes:   BOOL WINAPI PostMessage(__in_opt  HWND hWnd,
                                         __in      UINT Msg,
                                         __in      WPARAM wParam,
                                         __in      LPARAM lParam);                                                         
                 http://msdn.microsoft.com/en-us/library/ms644944(VS.85).aspx
        @param piHwnd A handle to the window whose window procedure is to receive the message
        @param piUmsg The message to be posted.
        @param piwParam Additional message-specific information.
        @param pilParam Additional message-specific information.
        @return If the function succeeds, the return value is nonzero.                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER PostMessage (piHwnd AS INTEGER,
                                              piUmsg AS INTEGER,
                                              piwParam AS INTEGER, 
                                              pilParam AS INTEGER):
        
        DEFINE VARIABLE piReturnValue AS INTEGER NO-UNDO.

        IF piHwnd > 0 THEN
            RUN PostMessageA (piHwnd, piUmsg, piwParam, pilParam, OUTPUT piReturnValue) .
        
        RETURN piReturnValue .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sends the specified message to a window or windows. The SendMessage 
                 function calls the window procedure for the specified window and does 
                 not return until the window procedure has processed the message.
        Notes:   To send a message and return immediately, use the SendMessageCallback 
                 or SendNotifyMessage function. To post a message to a thread's message 
                 queue and return immediately, use the PostMessage or PostThreadMessage function.
                 LRESULT WINAPI SendMessage(__in  HWND hWnd,
                                            __in  UINT Msg,
                                            __in  WPARAM wParam,
                                            __in  LPARAM lParam);                                                    
                 http://msdn.microsoft.com/en-us/library/windows/desktop/ms644950(v=vs.85).aspx 
        @param piHwnd A handle to the window whose window procedure is to receive the message
        @param piUmsg The message to be posted.
        @param piwParam Additional message-specific information.
        @param pilParam Additional message-specific information.
        @return If the function succeeds, the return value is nonzero.                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER SendMessage (piHwnd AS INTEGER,
                                              piUmsg AS INTEGER,
                                              piwParam AS INTEGER, 
                                              pilParam AS INTEGER):
        
        DEFINE VARIABLE piReturnValue AS INTEGER NO-UNDO.

        IF piHwnd > 0 THEN
            RUN SendMessageA (piHwnd, piUmsg, piwParam, pilParam, OUTPUT piReturnValue) .
        
        RETURN piReturnValue .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Changes the parent window of the specified child window.
        Notes:   
        @param piHwndChild A handle to the child window.
        @param piHwndNewParent A handle to the new parent window. If this parameter is NULL, the desktop window becomes the new parent window. If this parameter is HWND_MESSAGE, the child window becomes a message-only window.
        @return If the function succeeds, the return value is a handle to the previous parent window. If the function fails, the return value is NULL. 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC INTEGER SetParent (piHwndChild     AS INTEGER,
                                            piHwndNewParent AS INTEGER):
		
		DEFINE VARIABLE iHwndOldParent AS INTEGER NO-UNDO.
		
        RUN SetParent (piHwndChild, piHwndNewParent, OUTPUT iHwndOldParent) .
        
        RETURN iHwndOldParent .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Wrapper to sending the WM_SETREDRAW message to a Window (Control/Widget)
        Notes:   An application sends the WM_SETREDRAW message to a window to allow changes 
                 in that window to be redrawn or to prevent changes in that window from being 
                 redrawn. 
                 http://msdn.microsoft.com/en-us/library/dd145219(v=vs.85).aspx
        @param piHwnd The HWND of the Window to enable/disable screen repaint for
        @param plAllowRedraw True to allow update of window, False to disallow update of window
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SetRedraw (piHwnd AS INTEGER,
                                         plAllowRedraw AS LOGICAL):
        
        DEFINE VARIABLE iResult AS INTEGER NO-UNDO.

        IF plAllowRedraw THEN         
            RUN SendMessageA
                (piHwnd, {&WM_SETREDRAW}, 1, 0, OUTPUT iResult).
        ELSE 
            RUN SendMessageA
                (piHwnd, {&WM_SETREDRAW}, 0, 0, OUTPUT iResult).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs an operation on a specified file.                                                         
        Notes:   HINSTANCE ShellExecute(_In_opt_  HWND hwnd,
                                        _In_opt_  LPCTSTR lpOperation,
                                        _In_      LPCTSTR lpFile,
                                        _In_opt_  LPCTSTR lpParameters,
                                        _In_opt_  LPCTSTR lpDirectory,
                                        _In_      INT nShowCmd);
                 http://msdn.microsoft.com/en-us/library/ms644944(VS.85).aspx
        @param pcOperation The action to be performed. The set of available verbs depends on the particular file or folder. Generally, the actions available from an object's shortcut menu are available verbs.
        @param pcFile The file or object on which to execute the specified verb. To specify a Shell namespace object, pass the fully qualified parse name. Note that not all verbs are supported on all objects. For example, not all document types support the "print" verb. If a relative path is used for the lpDirectory parameter do not use a relative path for lpFile.
        @param pcParameter If lpFile specifies an executable file, the parameters to be passed to the application. The format of this string is determined by the verb that is to be invoked. If lpFile specifies a document file, lpParameters should be NULL.
        @param pcDirectory The default (working) directory for the action. If this value is NULL, the current working directory is used. If a relative path is provided at lpFile, do not use a relative path for lpDirectory.
        @param nShowCmd The flags that specify how an application is to be displayed when it is opened. If lpFile specifies a document file, the flag is simply passed to the associated application. It is up to the application to decide how to handle it. These values are defined in Winuser.h 
        @param piInstance OUTPUT If the function succeeds, it returns a value greater than 32. If the function fails, it returns an error value that indicates the cause of the failure. The return value is cast as an HINSTANCE for backward compatibility with 16-bit Windows applications. It is not a true HINSTANCE, however.
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC VOID ShellExecute (pcOperation AS CHARACTER,
                                            pcFile AS CHARACTER,
                                            pcParameter AS CHARACTER,
                                            pcDirectory AS CHARACTER,
                                            nShowCmd AS INTEGER,
                                            OUTPUT piInstance AS INTEGER):
        
        RUN ShellExecuteA (0, 
                           pcOperation, 
                           pcFile, 
                           pcParameter, 
                           pcDirectory, 
                           nShowCmd, 
                           OUTPUT piInstance) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs an operation on a specified file.                                                         
        Notes:   HINSTANCE ShellExecute(_In_opt_  HWND hwnd,
                                        _In_opt_  LPCTSTR lpOperation,
                                        _In_      LPCTSTR lpFile,
                                        _In_opt_  LPCTSTR lpParameters,
                                        _In_opt_  LPCTSTR lpDirectory,
                                        _In_      INT nShowCmd);
                 http://msdn.microsoft.com/en-us/library/ms644944(VS.85).aspx
        @param pcOperation The action to be performed. The set of available verbs depends on the particular file or folder. Generally, the actions available from an object's shortcut menu are available verbs.
        @param pcFile The file or object on which to execute the specified verb. To specify a Shell namespace object, pass the fully qualified parse name. Note that not all verbs are supported on all objects. For example, not all document types support the "print" verb. If a relative path is used for the lpDirectory parameter do not use a relative path for lpFile.
        @param pcParameter If lpFile specifies an executable file, the parameters to be passed to the application. The format of this string is determined by the verb that is to be invoked. If lpFile specifies a document file, lpParameters should be NULL.
        @param pcDirectory The default (working) directory for the action. If this value is NULL, the current working directory is used. If a relative path is provided at lpFile, do not use a relative path for lpDirectory.
        @param nShowCmd The flags that specify how an application is to be displayed when it is opened. If lpFile specifies a document file, the flag is simply passed to the associated application. It is up to the application to decide how to handle it. These values are defined in Winuser.h 
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC VOID ShellExecute (pcOperation AS CHARACTER,
                                            pcFile AS CHARACTER,
                                            pcParameter AS CHARACTER,
                                            pcDirectory AS CHARACTER,
                                            nShowCmd AS INTEGER):

        DEFINE VARIABLE iInstance AS INTEGER NO-UNDO.

        RUN ShellExecuteA (0, 
                           pcOperation, 
                           pcFile, 
                           pcParameter, 
                           pcDirectory, 
                           nShowCmd, 
                           OUTPUT iInstance) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Performs an operation on a specified file.                                                         
        Notes:   HINSTANCE ShellExecute(_In_opt_  HWND hwnd,
                                        _In_opt_  LPCTSTR lpOperation,
                                        _In_      LPCTSTR lpFile,
                                        _In_opt_  LPCTSTR lpParameters,
                                        _In_opt_  LPCTSTR lpDirectory,
                                        _In_      INT nShowCmd);
                 http://msdn.microsoft.com/en-us/library/ms644944(VS.85).aspx
        @param pcOperation The action to be performed. The set of available verbs depends on the particular file or folder. Generally, the actions available from an object's shortcut menu are available verbs.
        @param pcFile The file or object on which to execute the specified verb. To specify a Shell namespace object, pass the fully qualified parse name. Note that not all verbs are supported on all objects. For example, not all document types support the "print" verb. If a relative path is used for the lpDirectory parameter do not use a relative path for lpFile.
        @param pcParameter If lpFile specifies an executable file, the parameters to be passed to the application. The format of this string is determined by the verb that is to be invoked. If lpFile specifies a document file, lpParameters should be NULL.
        @param pcDirectory The default (working) directory for the action. If this value is NULL, the current working directory is used. If a relative path is provided at lpFile, do not use a relative path for lpDirectory.
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC VOID ShellExecute (pcOperation AS CHARACTER,
                                            pcFile AS CHARACTER,
                                            pcParameter AS CHARACTER,
                                            pcDirectory AS CHARACTER):

        DEFINE VARIABLE iInstance AS INTEGER NO-UNDO.

        RUN ShellExecuteA (0, 
                           pcOperation, 
                           pcFile, 
                           pcParameter, 
                           pcDirectory, 
                           1, 
                           OUTPUT iInstance) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Performs an operation on a specified file.                                                         
        Notes:   HINSTANCE ShellExecute(_In_opt_  HWND hwnd,
                                        _In_opt_  LPCTSTR lpOperation,
                                        _In_      LPCTSTR lpFile,
                                        _In_opt_  LPCTSTR lpParameters,
                                        _In_opt_  LPCTSTR lpDirectory,
                                        _In_      INT nShowCmd);
                 http://msdn.microsoft.com/en-us/library/ms644944(VS.85).aspx
        @param pcOperation The action to be performed. The set of available verbs depends on the particular file or folder. Generally, the actions available from an object's shortcut menu are available verbs.
        @param pcFile The file or object on which to execute the specified verb. To specify a Shell namespace object, pass the fully qualified parse name. Note that not all verbs are supported on all objects. For example, not all document types support the "print" verb. If a relative path is used for the lpDirectory parameter do not use a relative path for lpFile.
        @param pcParameter If lpFile specifies an executable file, the parameters to be passed to the application. The format of this string is determined by the verb that is to be invoked. If lpFile specifies a document file, lpParameters should be NULL.
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC VOID ShellExecute (pcOperation AS CHARACTER,
                                            pcFile AS CHARACTER,
                                            pcParameter AS CHARACTER):

        DEFINE VARIABLE iInstance AS INTEGER NO-UNDO.

        RUN ShellExecuteA (0, 
                           pcOperation, 
                           pcFile, 
                           pcParameter, 
                           "":U, 
                           1, 
                           OUTPUT iInstance) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Performs an operation on a specified file.                                                         
        Notes:   HINSTANCE ShellExecute(_In_opt_  HWND hwnd,
                                        _In_opt_  LPCTSTR lpOperation,
                                        _In_      LPCTSTR lpFile,
                                        _In_opt_  LPCTSTR lpParameters,
                                        _In_opt_  LPCTSTR lpDirectory,
                                        _In_      INT nShowCmd);
                 http://msdn.microsoft.com/en-us/library/ms644944(VS.85).aspx
        @param pcOperation The action to be performed. The set of available verbs depends on the particular file or folder. Generally, the actions available from an object's shortcut menu are available verbs.
        @param pcFile The file or object on which to execute the specified verb. To specify a Shell namespace object, pass the fully qualified parse name. Note that not all verbs are supported on all objects. For example, not all document types support the "print" verb. If a relative path is used for the lpDirectory parameter do not use a relative path for lpFile.
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC VOID ShellExecute (pcOperation AS CHARACTER,
                                            pcFile AS CHARACTER):

        DEFINE VARIABLE iInstance AS INTEGER NO-UNDO.

        RUN ShellExecuteA (0, 
                           pcOperation, 
                           pcFile, 
                           "":U, 
                           "":U, 
                           1, 
                           OUTPUT iInstance) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Performs an operation on a specified file.                                                         
        Notes:   HINSTANCE ShellExecute(_In_opt_  HWND hwnd,
                                        _In_opt_  LPCTSTR lpOperation,
                                        _In_      LPCTSTR lpFile,
                                        _In_opt_  LPCTSTR lpParameters,
                                        _In_opt_  LPCTSTR lpDirectory,
                                        _In_      INT nShowCmd);
                 http://msdn.microsoft.com/en-us/library/ms644944(VS.85).aspx
        @param piHwnd A handle to the parent window used for displaying a UI or error messages. This value can be NULL if the operation is not associated with a window
        @param pcOperation The action to be performed. The set of available verbs depends on the particular file or folder. Generally, the actions available from an object's shortcut menu are available verbs.
        @param pcFile The file or object on which to execute the specified verb. To specify a Shell namespace object, pass the fully qualified parse name. Note that not all verbs are supported on all objects. For example, not all document types support the "print" verb. If a relative path is used for the lpDirectory parameter do not use a relative path for lpFile.
        @param pcParameter If lpFile specifies an executable file, the parameters to be passed to the application. The format of this string is determined by the verb that is to be invoked. If lpFile specifies a document file, lpParameters should be NULL.
        @param pcDirectory The default (working) directory for the action. If this value is NULL, the current working directory is used. If a relative path is provided at lpFile, do not use a relative path for lpDirectory.
        @param nShowCmd The flags that specify how an application is to be displayed when it is opened. If lpFile specifies a document file, the flag is simply passed to the associated application. It is up to the application to decide how to handle it. These values are defined in Winuser.h 
        @param piInstance OUTPUT If the function succeeds, it returns a value greater than 32. If the function fails, it returns an error value that indicates the cause of the failure. The return value is cast as an HINSTANCE for backward compatibility with 16-bit Windows applications. It is not a true HINSTANCE, however.
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC VOID ShellExecute (piHwnd AS INTEGER,
                                            pcOperation AS CHARACTER,
                                            pcFile AS CHARACTER,
                                            pcParameter AS CHARACTER,
                                            pcDirectory AS CHARACTER,
                                            nShowCmd AS INTEGER,
                                            OUTPUT piInstance AS INTEGER):
        
        RUN ShellExecuteA (piHwnd, 
                           pcOperation, 
                           pcFile, 
                           pcParameter, 
                           pcDirectory, 
                           nShowCmd, 
                           OUTPUT piInstance) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs an operation on a specified file.                                                         
        Notes:   HINSTANCE ShellExecute(_In_opt_  HWND hwnd,
                                        _In_opt_  LPCTSTR lpOperation,
                                        _In_      LPCTSTR lpFile,
                                        _In_opt_  LPCTSTR lpParameters,
                                        _In_opt_  LPCTSTR lpDirectory,
                                        _In_      INT nShowCmd);
                 http://msdn.microsoft.com/en-us/library/ms644944(VS.85).aspx
        @param piHwnd A handle to the parent window used for displaying a UI or error messages. This value can be NULL if the operation is not associated with a window
        @param pcOperation The action to be performed. The set of available verbs depends on the particular file or folder. Generally, the actions available from an object's shortcut menu are available verbs.
        @param pcFile The file or object on which to execute the specified verb. To specify a Shell namespace object, pass the fully qualified parse name. Note that not all verbs are supported on all objects. For example, not all document types support the "print" verb. If a relative path is used for the lpDirectory parameter do not use a relative path for lpFile.
        @param pcParameter If lpFile specifies an executable file, the parameters to be passed to the application. The format of this string is determined by the verb that is to be invoked. If lpFile specifies a document file, lpParameters should be NULL.
        @param pcDirectory The default (working) directory for the action. If this value is NULL, the current working directory is used. If a relative path is provided at lpFile, do not use a relative path for lpDirectory.
        @param nShowCmd The flags that specify how an application is to be displayed when it is opened. If lpFile specifies a document file, the flag is simply passed to the associated application. It is up to the application to decide how to handle it. These values are defined in Winuser.h 
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC VOID ShellExecute (piHwnd AS INTEGER,
                                            pcOperation AS CHARACTER,
                                            pcFile AS CHARACTER,
                                            pcParameter AS CHARACTER,
                                            pcDirectory AS CHARACTER,
                                            nShowCmd AS INTEGER):

        DEFINE VARIABLE iInstance AS INTEGER NO-UNDO.

        RUN ShellExecuteA (piHwnd, 
                           pcOperation, 
                           pcFile, 
                           pcParameter, 
                           pcDirectory, 
                           nShowCmd, 
                           OUTPUT iInstance) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Performs an operation on a specified file.                                                         
        Notes:   HINSTANCE ShellExecute(_In_opt_  HWND hwnd,
                                        _In_opt_  LPCTSTR lpOperation,
                                        _In_      LPCTSTR lpFile,
                                        _In_opt_  LPCTSTR lpParameters,
                                        _In_opt_  LPCTSTR lpDirectory,
                                        _In_      INT nShowCmd);
                 http://msdn.microsoft.com/en-us/library/ms644944(VS.85).aspx
        @param piHwnd A handle to the parent window used for displaying a UI or error messages. This value can be NULL if the operation is not associated with a window
        @param pcOperation The action to be performed. The set of available verbs depends on the particular file or folder. Generally, the actions available from an object's shortcut menu are available verbs.
        @param pcFile The file or object on which to execute the specified verb. To specify a Shell namespace object, pass the fully qualified parse name. Note that not all verbs are supported on all objects. For example, not all document types support the "print" verb. If a relative path is used for the lpDirectory parameter do not use a relative path for lpFile.
        @param pcParameter If lpFile specifies an executable file, the parameters to be passed to the application. The format of this string is determined by the verb that is to be invoked. If lpFile specifies a document file, lpParameters should be NULL.
        @param pcDirectory The default (working) directory for the action. If this value is NULL, the current working directory is used. If a relative path is provided at lpFile, do not use a relative path for lpDirectory.
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC VOID ShellExecute (piHwnd AS INTEGER,
                                            pcOperation AS CHARACTER,
                                            pcFile AS CHARACTER,
                                            pcParameter AS CHARACTER,
                                            pcDirectory AS CHARACTER):

        DEFINE VARIABLE iInstance AS INTEGER NO-UNDO.

        RUN ShellExecuteA (piHwnd, 
                           pcOperation, 
                           pcFile, 
                           pcParameter, 
                           pcDirectory, 
                           1, 
                           OUTPUT iInstance) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Performs an operation on a specified file.                                                         
        Notes:   HINSTANCE ShellExecute(_In_opt_  HWND hwnd,
                                        _In_opt_  LPCTSTR lpOperation,
                                        _In_      LPCTSTR lpFile,
                                        _In_opt_  LPCTSTR lpParameters,
                                        _In_opt_  LPCTSTR lpDirectory,
                                        _In_      INT nShowCmd);
                 http://msdn.microsoft.com/en-us/library/ms644944(VS.85).aspx
        @param piHwnd A handle to the parent window used for displaying a UI or error messages. This value can be NULL if the operation is not associated with a window
        @param pcOperation The action to be performed. The set of available verbs depends on the particular file or folder. Generally, the actions available from an object's shortcut menu are available verbs.
        @param pcFile The file or object on which to execute the specified verb. To specify a Shell namespace object, pass the fully qualified parse name. Note that not all verbs are supported on all objects. For example, not all document types support the "print" verb. If a relative path is used for the lpDirectory parameter do not use a relative path for lpFile.
        @param pcParameter If lpFile specifies an executable file, the parameters to be passed to the application. The format of this string is determined by the verb that is to be invoked. If lpFile specifies a document file, lpParameters should be NULL.
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC VOID ShellExecute (piHwnd AS INTEGER,
                                            pcOperation AS CHARACTER,
                                            pcFile AS CHARACTER,
                                            pcParameter AS CHARACTER):

        DEFINE VARIABLE iInstance AS INTEGER NO-UNDO.

        RUN ShellExecuteA (piHwnd, 
                           pcOperation, 
                           pcFile, 
                           pcParameter, 
                           "":U, 
                           1, 
                           OUTPUT iInstance) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Performs an operation on a specified file.                                                         
        Notes:   HINSTANCE ShellExecute(_In_opt_  HWND hwnd,
                                        _In_opt_  LPCTSTR lpOperation,
                                        _In_      LPCTSTR lpFile,
                                        _In_opt_  LPCTSTR lpParameters,
                                        _In_opt_  LPCTSTR lpDirectory,
                                        _In_      INT nShowCmd);
                 http://msdn.microsoft.com/en-us/library/ms644944(VS.85).aspx
        @param piHwnd A handle to the parent window used for displaying a UI or error messages. This value can be NULL if the operation is not associated with a window
        @param pcOperation The action to be performed. The set of available verbs depends on the particular file or folder. Generally, the actions available from an object's shortcut menu are available verbs.
        @param pcFile The file or object on which to execute the specified verb. To specify a Shell namespace object, pass the fully qualified parse name. Note that not all verbs are supported on all objects. For example, not all document types support the "print" verb. If a relative path is used for the lpDirectory parameter do not use a relative path for lpFile.
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC VOID ShellExecute (piHwnd AS INTEGER,
                                            pcOperation AS CHARACTER,
                                            pcFile AS CHARACTER):

        DEFINE VARIABLE iInstance AS INTEGER NO-UNDO.

        RUN ShellExecuteA (piHwnd, 
                           pcOperation, 
                           pcFile, 
                           "":U, 
                           "":U, 
                           1, 
                           OUTPUT iInstance) .

    END METHOD . 
    
END CLASS.
