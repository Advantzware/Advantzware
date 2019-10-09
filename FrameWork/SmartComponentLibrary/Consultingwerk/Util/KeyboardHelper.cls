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
    File        : KeyboardHelper
    Purpose     : Provides support for working with .NET Key (board) Values
    Syntax      : Static methods only, Protected constructor to disallow 
                  instance creation
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 22 22:05:48 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Util.*     FROM PROPATH . 
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING System.Windows.Forms.*    FROM ASSEMBLY  .
USING Progress.Util.*           FROM ASSEMBLY .
&ENDIF
USING Progress.Lang.*           FROM PROPATH .

CLASS Consultingwerk.Util.KeyboardHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED KeyboardHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Translates a Progress MENU-ITEM Accelerator into a .NET
                 System.Windows.Forms.Shortcut enumeration member
        Notes:   This method used to reside in the UltraToolbarsHelper and has been
                 moved to the Keyboard helper for better reusability
                 Note, not all possible Progress Accelerators are available/supported
                 as .NET Shortcuts, http://blog.consultingwerk.de/consultingwerkblog/2011/10/create-custom-shortcuts-not-contained-in-the-system-windows-forms-shortcut-enumeration/
        @param pcAccelerator The ABL keyboard accelerator
        @return The System.Windows.Forms.Shortcut value that matches the ABL keyboard accelerator         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Windows.Forms.Shortcut ShortcutFromAccelerator (pcAccelerator AS CHARACTER):

        CASE pcAccelerator:

            WHEN "":U OR WHEN "?":U OR WHEN ? THEN RETURN System.Windows.Forms.Shortcut:None .

            WHEN "ALT-0":U THEN RETURN System.Windows.Forms.Shortcut:Alt0 .
            WHEN "ALT-1":U THEN RETURN System.Windows.Forms.Shortcut:Alt1 .
            WHEN "ALT-2":U THEN RETURN System.Windows.Forms.Shortcut:Alt2 .
            WHEN "ALT-3":U THEN RETURN System.Windows.Forms.Shortcut:Alt3 .
            WHEN "ALT-4":U THEN RETURN System.Windows.Forms.Shortcut:Alt4 .
            WHEN "ALT-5":U THEN RETURN System.Windows.Forms.Shortcut:Alt5 .
            WHEN "ALT-6":U THEN RETURN System.Windows.Forms.Shortcut:Alt6 .
            WHEN "ALT-7":U THEN RETURN System.Windows.Forms.Shortcut:Alt7 .
            WHEN "ALT-8":U THEN RETURN System.Windows.Forms.Shortcut:Alt8 .
            WHEN "ALT-9":U THEN RETURN System.Windows.Forms.Shortcut:Alt9 .

            WHEN "ALT-BACKSPACE":U THEN RETURN System.Windows.Forms.Shortcut:AltBksp .
            WHEN "ALT-F1":U THEN RETURN System.Windows.Forms.Shortcut:AltF1 .
            WHEN "ALT-F10":U THEN RETURN System.Windows.Forms.Shortcut:AltF10 .
            WHEN "ALT-F11":U THEN RETURN System.Windows.Forms.Shortcut:AltF11 .
            WHEN "ALT-F12":U THEN RETURN System.Windows.Forms.Shortcut:AltF12 .
            WHEN "ALT-F2":U THEN RETURN System.Windows.Forms.Shortcut:AltF2 .
            WHEN "ALT-F3":U THEN RETURN System.Windows.Forms.Shortcut:AltF3 .
            WHEN "ALT-F4":U THEN RETURN System.Windows.Forms.Shortcut:AltF4 .
            WHEN "ALT-F5":U THEN RETURN System.Windows.Forms.Shortcut:AltF5 .
            WHEN "ALT-F6":U THEN RETURN System.Windows.Forms.Shortcut:AltF6 .
            WHEN "ALT-F7":U THEN RETURN System.Windows.Forms.Shortcut:AltF7 .
            WHEN "ALT-F8":U THEN RETURN System.Windows.Forms.Shortcut:AltF8 .
            WHEN "ALT-F9":U THEN RETURN System.Windows.Forms.Shortcut:AltF9 .

            WHEN "CTRL-0":U THEN RETURN System.Windows.Forms.Shortcut:Ctrl0 .
            WHEN "CTRL-1":U THEN RETURN System.Windows.Forms.Shortcut:Ctrl1 .
            WHEN "CTRL-2":U THEN RETURN System.Windows.Forms.Shortcut:Ctrl2 .
            WHEN "CTRL-3":U THEN RETURN System.Windows.Forms.Shortcut:Ctrl3 .
            WHEN "CTRL-4":U THEN RETURN System.Windows.Forms.Shortcut:Ctrl4 .
            WHEN "CTRL-5":U THEN RETURN System.Windows.Forms.Shortcut:Ctrl5 .
            WHEN "CTRL-6":U THEN RETURN System.Windows.Forms.Shortcut:Ctrl6 .
            WHEN "CTRL-7":U THEN RETURN System.Windows.Forms.Shortcut:Ctrl7 .
            WHEN "CTRL-8":U THEN RETURN System.Windows.Forms.Shortcut:Ctrl8 .
            WHEN "CTRL-9":U THEN RETURN System.Windows.Forms.Shortcut:Ctrl9 .
            WHEN "CTRL-A":U THEN RETURN System.Windows.Forms.Shortcut:CtrlA .
            WHEN "CTRL-B":U THEN RETURN System.Windows.Forms.Shortcut:CtrlB .
            WHEN "CTRL-C":U THEN RETURN System.Windows.Forms.Shortcut:CtrlC .
            WHEN "CTRL-D":U THEN RETURN System.Windows.Forms.Shortcut:CtrlD .
            WHEN "CTRL-E":U THEN RETURN System.Windows.Forms.Shortcut:CtrlE .
            WHEN "CTRL-F":U THEN RETURN System.Windows.Forms.Shortcut:CtrlF .
            WHEN "CTRL-G":U THEN RETURN System.Windows.Forms.Shortcut:CtrlG .
            WHEN "CTRL-H":U THEN RETURN System.Windows.Forms.Shortcut:CtrlH .
            WHEN "CTRL-I":U THEN RETURN System.Windows.Forms.Shortcut:CtrlI .
            WHEN "CTRL-J":U THEN RETURN System.Windows.Forms.Shortcut:CtrlJ .
            WHEN "CTRL-K":U THEN RETURN System.Windows.Forms.Shortcut:CtrlK .
            WHEN "CTRL-L":U THEN RETURN System.Windows.Forms.Shortcut:CtrlL .
            WHEN "CTRL-M":U THEN RETURN System.Windows.Forms.Shortcut:CtrlM .
            WHEN "CTRL-N":U THEN RETURN System.Windows.Forms.Shortcut:CtrlN .
            WHEN "CTRL-O":U THEN RETURN System.Windows.Forms.Shortcut:CtrlO .
            WHEN "CTRL-P":U THEN RETURN System.Windows.Forms.Shortcut:CtrlP .
            WHEN "CTRL-Q":U THEN RETURN System.Windows.Forms.Shortcut:CtrlQ .
            WHEN "CTRL-R":U THEN RETURN System.Windows.Forms.Shortcut:CtrlR .
            WHEN "CTRL-S":U THEN RETURN System.Windows.Forms.Shortcut:CtrlS .
            WHEN "CTRL-T":U THEN RETURN System.Windows.Forms.Shortcut:CtrlT .
            WHEN "CTRL-U":U THEN RETURN System.Windows.Forms.Shortcut:CtrlU .
            WHEN "CTRL-V":U THEN RETURN System.Windows.Forms.Shortcut:CtrlV .
            WHEN "CTRL-W":U THEN RETURN System.Windows.Forms.Shortcut:CtrlW .
            WHEN "CTRL-X":U THEN RETURN System.Windows.Forms.Shortcut:CtrlX .
            WHEN "CTRL-Y":U THEN RETURN System.Windows.Forms.Shortcut:CtrlY .
            WHEN "CTRL-Z":U THEN RETURN System.Windows.Forms.Shortcut:CtrlZ .

            WHEN "F1":U THEN RETURN System.Windows.Forms.Shortcut:F1 .
            WHEN "F10":U THEN RETURN System.Windows.Forms.Shortcut:F10 .
            WHEN "F11":U THEN RETURN System.Windows.Forms.Shortcut:F11 .
            WHEN "F12":U THEN RETURN System.Windows.Forms.Shortcut:F12 .
            WHEN "F2":U THEN RETURN System.Windows.Forms.Shortcut:F2 .
            WHEN "F3":U THEN RETURN System.Windows.Forms.Shortcut:F3 .
            WHEN "F4":U THEN RETURN System.Windows.Forms.Shortcut:F4 .
            WHEN "F5":U THEN RETURN System.Windows.Forms.Shortcut:F5 .
            WHEN "F6":U THEN RETURN System.Windows.Forms.Shortcut:F6 .
            WHEN "F7":U THEN RETURN System.Windows.Forms.Shortcut:F7 .
            WHEN "F8":U THEN RETURN System.Windows.Forms.Shortcut:F8 .
            WHEN "F9":U THEN RETURN System.Windows.Forms.Shortcut:F9 .
            WHEN "INS":U THEN RETURN System.Windows.Forms.Shortcut:Ins .
            WHEN "DEL":U THEN RETURN System.Windows.Forms.Shortcut:Del .

            WHEN "CTRL-F1":U THEN RETURN System.Windows.Forms.Shortcut:CtrlF1 .
            WHEN "CTRL-F10":U THEN RETURN System.Windows.Forms.Shortcut:CtrlF10 .
            WHEN "CTRL-F11":U THEN RETURN System.Windows.Forms.Shortcut:CtrlF11 .
            WHEN "CTRL-F12":U THEN RETURN System.Windows.Forms.Shortcut:CtrlF12 .
            WHEN "CTRL-F2":U THEN RETURN System.Windows.Forms.Shortcut:CtrlF2 .
            WHEN "CTRL-F3":U THEN RETURN System.Windows.Forms.Shortcut:CtrlF3 .
            WHEN "CTRL-F4":U THEN RETURN System.Windows.Forms.Shortcut:CtrlF4 .
            WHEN "CTRL-F5":U THEN RETURN System.Windows.Forms.Shortcut:CtrlF5 .
            WHEN "CTRL-F6":U THEN RETURN System.Windows.Forms.Shortcut:CtrlF6 .
            WHEN "CTRL-F7":U THEN RETURN System.Windows.Forms.Shortcut:CtrlF7 .
            WHEN "CTRL-F8":U THEN RETURN System.Windows.Forms.Shortcut:CtrlF8 .
            WHEN "CTRL-F9":U THEN RETURN System.Windows.Forms.Shortcut:CtrlF9 .

            WHEN "CTRL-DEL":U OR WHEN "ALT-DEL":U THEN RETURN System.Windows.Forms.Shortcut:CtrlDel .
            WHEN "CTRL-INS":U OR WHEN "ALT-INS":U THEN RETURN System.Windows.Forms.Shortcut:CtrlIns .

            WHEN "SHIFT-DEL":U THEN RETURN System.Windows.Forms.Shortcut:ShiftDel .
            WHEN "SHIFT-INS":U THEN RETURN System.Windows.Forms.Shortcut:ShiftIns .

            WHEN "SHIFT-F1":U THEN RETURN System.Windows.Forms.Shortcut:ShiftF1 .
            WHEN "SHIFT-F10":U THEN RETURN System.Windows.Forms.Shortcut:ShiftF10 .
            WHEN "SHIFT-F11":U THEN RETURN System.Windows.Forms.Shortcut:ShiftF11 .
            WHEN "SHIFT-F12":U THEN RETURN System.Windows.Forms.Shortcut:ShiftF12 .
            WHEN "SHIFT-F2":U THEN RETURN System.Windows.Forms.Shortcut:ShiftF2 .
            WHEN "SHIFT-F3":U THEN RETURN System.Windows.Forms.Shortcut:ShiftF3 .
            WHEN "SHIFT-F4":U THEN RETURN System.Windows.Forms.Shortcut:ShiftF4 .
            WHEN "SHIFT-F5":U THEN RETURN System.Windows.Forms.Shortcut:ShiftF5 .
            WHEN "SHIFT-F6":U THEN RETURN System.Windows.Forms.Shortcut:ShiftF6 .
            WHEN "SHIFT-F7":U THEN RETURN System.Windows.Forms.Shortcut:ShiftF7 .
            WHEN "SHIFT-F8":U THEN RETURN System.Windows.Forms.Shortcut:ShiftF8 .
            WHEN "SHIFT-F9":U THEN RETURN System.Windows.Forms.Shortcut:ShiftF9 .

            WHEN "CTRL-SHIFT-F1":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftF1 .
            WHEN "CTRL-SHIFT-F10":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftF10 .
            WHEN "CTRL-SHIFT-F11":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftF11 .
            WHEN "CTRL-SHIFT-F12":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftF12 .
            WHEN "CTRL-SHIFT-F2":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftF2 .
            WHEN "CTRL-SHIFT-F3":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftF3 .
            WHEN "CTRL-SHIFT-F4":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftF4 .
            WHEN "CTRL-SHIFT-F5":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftF5 .
            WHEN "CTRL-SHIFT-F6":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftF6 .
            WHEN "CTRL-SHIFT-F7":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftF7 .
            WHEN "CTRL-SHIFT-F8":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftF8 .
            WHEN "CTRL-SHIFT-F9":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftF9 .

            WHEN "CTRL-SHIFT-A":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftA .
            WHEN "CTRL-SHIFT-B":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftB .
            WHEN "CTRL-SHIFT-C":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftC .
            WHEN "CTRL-SHIFT-D":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftD .
            WHEN "CTRL-SHIFT-E":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftE .
            WHEN "CTRL-SHIFT-F":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftF .
            WHEN "CTRL-SHIFT-G":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftG .
            WHEN "CTRL-SHIFT-H":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftH .
            WHEN "CTRL-SHIFT-I":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftI .
            WHEN "CTRL-SHIFT-J":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftJ .
            WHEN "CTRL-SHIFT-K":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftK .
            WHEN "CTRL-SHIFT-L":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftL .
            WHEN "CTRL-SHIFT-M":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftM .
            WHEN "CTRL-SHIFT-N":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftN .
            WHEN "CTRL-SHIFT-O":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftO .
            WHEN "CTRL-SHIFT-P":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftP .
            WHEN "CTRL-SHIFT-Q":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftQ .
            WHEN "CTRL-SHIFT-R":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftR .
            WHEN "CTRL-SHIFT-S":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftS .
            WHEN "CTRL-SHIFT-T":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftT .
            WHEN "CTRL-SHIFT-U":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftU .
            WHEN "CTRL-SHIFT-V":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftV .
            WHEN "CTRL-SHIFT-W":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftW .
            WHEN "CTRL-SHIFT-X":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftX .
            WHEN "CTRL-SHIFT-Y":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftY .
            WHEN "CTRL-SHIFT-Z":U THEN RETURN System.Windows.Forms.Shortcut:CtrlShiftZ .

            WHEN "DELETE-CHARACTER":U THEN RETURN System.Windows.Forms.Shortcut:Del .
    
        END CASE.
        
        ASSIGN pcAccelerator = REPLACE (pcAccelerator, "PAGE-DOWN":U,        "PageDown":U)
               pcAccelerator = REPLACE (pcAccelerator, "PAGE-UP":U,          "PageUp":U)
               pcAccelerator = REPLACE (pcAccelerator, "CURSOR-UP":U,        "Up":U)
               pcAccelerator = REPLACE (pcAccelerator, "CURSOR-DOWN":U,      "Down":U)
               pcAccelerator = REPLACE (pcAccelerator, "CURSOR-LEFT":U,      "Left":U)
               pcAccelerator = REPLACE (pcAccelerator, "CURSOR-RIGHT":U,     "Right":U)
               
               /* Mike Fechner, Consultingwerk Ltd. 21.07.2013
                  Handling of combinations with DELETE-CHARACTER, ALT-DEL, SHIFT-DEL */
               pcAccelerator = REPLACE (pcAccelerator, "DEL":U, "Delete":U)
               .
        
        IF NUM-ENTRIES (pcAccelerator, "-":U) = 2 THEN 
            RETURN KeyboardHelper:ShortcutFromKeyCombination (ENTRY (1, pcAccelerator, "-":U),
                                                              ENTRY (2, pcAccelerator, "-":U)) .
        ELSE IF NUM-ENTRIES (pcAccelerator, "-":U) = 3 THEN 
            RETURN KeyboardHelper:ShortcutFromKeyCombination (ENTRY (1, pcAccelerator, "-":U),
                                                              ENTRY (2, pcAccelerator, "-":U),
                                                              ENTRY (3, pcAccelerator, "-":U)) .
                                                              
        RETURN ? .
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Convert special key combinations (like ALT-A) into a 
                 System.Windows.Forms.Shortcut value                                                                      
        Notes:   See http://blog.consultingwerk.de/consultingwerkblog/2011/10/create-custom-shortcuts-not-contained-in-the-system-windows-forms-shortcut-enumeration/                                                                     
        @param pcKey1 The first ABL key label                                                                              
        @param pcKey2 The second ABL key label                                                                              
        @return The Shortcut Enum value                                                                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Windows.Forms.Shortcut ShortcutFromKeyCombination (pcKey1 AS CHARACTER, 
                                                                                   pcKey2 AS CHARACTER):
        RETURN ShortcutFromKeyCombination (pcKey1, 
                                           pcKey2,
                                           ?) .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Convert a single special key into a System.Windows.Forms.Shortcut value                                                                         
        Notes:   See http://blog.consultingwerk.de/consultingwerkblog/2011/10/create-custom-shortcuts-not-contained-in-the-system-windows-forms-shortcut-enumeration/
        @param poKey The System.Windows.Forms.Keys value to convert                                                                               
        @return The Shortcut Enum value                                                                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Windows.Forms.Shortcut ShortcutFromKey (poKey AS System.Windows.Forms.Keys):
        
        DEFINE VARIABLE iEnum   AS INTEGER     NO-UNDO.
        
        iEnum = Progress.Util.CastUtil:ToInt32 (poKey) .
        
        RETURN CAST (System.Enum:ToObject (Progress.Util.TypeHelper:GetType ("System.Windows.Forms.Shortcut":U),
                                           iEnum),
                     System.Windows.Forms.Shortcut) .    
            
    END METHOD.
         
    /*------------------------------------------------------------------------------
        Purpose: Convert special key combinations (like ALT-A) into a 
                 System.Windows.Forms.Shortcut value                                                                         
        Notes:   See http://blog.consultingwerk.de/consultingwerkblog/2011/10/create-custom-shortcuts-not-contained-in-the-system-windows-forms-shortcut-enumeration/
        @param pcKey1 The first ABL key label                                                                              
        @param pcKey2 The second ABL key label                                                                              
        @param pcKey3 The third ABL key label
        @return The Shortcut Enum value                                                                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Windows.Forms.Shortcut ShortcutFromKeyCombination (pcKey1 AS CHARACTER, 
                                                                                   pcKey2 AS CHARACTER,
                                                                                   pcKey3 AS CHARACTER):
        
        DEFINE VARIABLE cABL    AS CHARACTER   NO-UNDO INITIAL "ALT,SHIFT,CTRL":U.
        DEFINE VARIABLE cDotNet AS CHARACTER   NO-UNDO INITIAL "Alt,Shift,Control":U.
        DEFINE VARIABLE i       AS INTEGER     NO-UNDO.

        DEFINE VARIABLE oKey    AS Keys        NO-UNDO EXTENT 3 .
        DEFINE VARIABLE oEnum   AS System.Enum NO-UNDO .
        DEFINE VARIABLE iEnum   AS INTEGER     NO-UNDO.
        
        DO i = 1 TO NUM-ENTRIES (cABL):
            IF pcKey1 = ENTRY (i, cABL) THEN pcKey1 = ENTRY (i, cDotNet) .
            IF pcKey2 = ENTRY (i, cABL) THEN pcKey2 = ENTRY (i, cDotNet) .
            IF pcKey3 = ENTRY (i, cABL) THEN pcKey3 = ENTRY (i, cDotNet) .
        END.

        IF pcKey1 > "":U THEN oKey [1] = {Consultingwerk/parse-enum-ignorecase.i System.Windows.Forms.Keys pcKey1} . 
        IF pcKey2 > "":U THEN oKey [2] = {Consultingwerk/parse-enum-ignorecase.i System.Windows.Forms.Keys pcKey2} . 
        IF pcKey3 > "":U THEN oKey [3] = {Consultingwerk/parse-enum-ignorecase.i System.Windows.Forms.Keys pcKey3} . 

        IF VALID-OBJECT (oKey[2]) THEN 
            oEnum = Progress.Util.EnumHelper:Or (oKey [1], oKey[2]) .
        IF VALID-OBJECT (oKey[3]) THEN 
            oEnum = Progress.Util.EnumHelper:Or (oEnum, oKey[3]) .
        
        iEnum = Progress.Util.CastUtil:ToInt32 (oEnum) .
        
        RETURN CAST (System.Enum:ToObject (Progress.Util.TypeHelper:GetType ("System.Windows.Forms.Shortcut":U),
                                           iEnum),
                     System.Windows.Forms.Shortcut) .    
            
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the ABL Keyboard event label for the specified .NET KeyEventArgs 
        Notes:   
        @param poEventArgs The System.Windows.Forms.Keys to translate
        @return The ABL Keyboard Event Label or ? for an unknown/unmatches key
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER SpecialKeyToAblEventLabel (poEventArgs AS KeyEventArgs):
    
        IF NOT VALID-OBJECT (poEventArgs) THEN
            RETURN ? .
        
        RETURN KeyboardHelper:SpecialKeyToAblEventLabel (poEventArgs:KeyCode) .
    
    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the ABL Keyboard event label for the specified .NET Keys Enum value
        Notes:   
        @param poKeys The System.Windows.Forms.Keys to translate
        @return The ABL Keyboard Event Label or ? for an unknown/unmatches key
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER SpecialKeyToAblEventLabel (poKeys AS Keys):
        
        IF EnumHelper:AreEqual (poKeys, Keys:Back)        THEN RETURN "BACKSPACE":U . 
        IF EnumHelper:AreEqual (poKeys, Keys:Tab)         THEN RETURN "TAB":U .         
        IF EnumHelper:AreEqual (poKeys, Keys:Return)      THEN RETURN "RETURN":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:Enter)       THEN RETURN "RETURN":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:Escape)      THEN RETURN "END-ERROR":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:End)         THEN RETURN "END":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:Home)        THEN RETURN "HOME":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:Left)        THEN RETURN "CURSOR-LEFT":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:Up)          THEN RETURN "CURSOR-UP":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:Right)       THEN RETURN "CURSOR-RIGHT":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:Down)        THEN RETURN "CURSOR-DOWN":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:Delete)      THEN RETURN "DELETE-CHARACTER":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:Insert)      THEN RETURN "INSERT-MODE":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F1)          THEN RETURN "F1":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F2)          THEN RETURN "F2":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F3)          THEN RETURN "F3":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F4)          THEN RETURN "F4":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F5)          THEN RETURN "F5":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F6)          THEN RETURN "F6":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F7)          THEN RETURN "F7":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F8)          THEN RETURN "F8":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F9)          THEN RETURN "F9":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F10)         THEN RETURN "F10":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F11)         THEN RETURN "F11":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F12)         THEN RETURN "F12":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F13)         THEN RETURN "F13":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F14)         THEN RETURN "F14":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F15)         THEN RETURN "F15":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F16)         THEN RETURN "F16":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F17)         THEN RETURN "F17":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F18)         THEN RETURN "F18":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F19)         THEN RETURN "F19":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F20)         THEN RETURN "F20":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F21)         THEN RETURN "F21":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F22)         THEN RETURN "F22":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F23)         THEN RETURN "F23":U .       
        IF EnumHelper:AreEqual (poKeys, Keys:F24)         THEN RETURN "F24":U .       
        
        RETURN ? .

    END METHOD .
&ENDIF

END CLASS.
