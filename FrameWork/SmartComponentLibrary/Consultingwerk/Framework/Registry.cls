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
    File        : Registry
    Purpose     : Provides access to the Windows Registry
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed May 21 14:42:55 CEST 2008
    Notes       : This file is intended for GUI for .NET only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Framework.Exceptions.* FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING Microsoft.Win32.* FROM ASSEMBLY .
USING Progress.Lang.* FROM ASSEMBLY .
&ENDIF

CLASS Consultingwerk.Framework.Registry :

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Registry class
        Notes:   Disallow instance creation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE Registry ():
        SUPER ().

    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Deletes a single value from the windows registry
        Notes:
        @param pcRegistryKey The registry key
        @param pcSubKey The registry sub key
        @param pcProperty The property name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID DeleteRegistryValue (pcRegistryKey AS CHARACTER,
                                                   pcSubKey AS CHARACTER,
                                                   pcProperty AS CHARACTER):

        DEFINE VARIABLE regkey AS RegistryKey NO-UNDO .

        regKey = FindRegistryKey (pcRegistryKey, pcSubKey) .

        IF NOT VALID-OBJECT(regKey) THEN
            UNDO, THROW NEW UnknownRegistryException (pcRegistryKey + "~\":U + pcSubKey) .

        regKey:DeleteValue (pcProperty) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deletes a single key (and all child keys) from the windows registry
        Notes:
        @param pcRegistryKey The registry key
        @param pcSubKey The registry sub key
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID DeleteRegistryKey (pcRegistryKey AS CHARACTER,
                                                 pcSubKey AS CHARACTER):

        DEFINE VARIABLE regkey   AS RegistryKey NO-UNDO .
        DEFINE VARIABLE cKeyName AS CHARACTER   NO-UNDO .

        regKey = FindRegistryKey (pcRegistryKey, pcSubKey) .

        IF NOT VALID-OBJECT(regKey) THEN
            UNDO, THROW NEW UnknownRegistryException (pcRegistryKey + "~\":U + pcSubKey) .

        ASSIGN cKeyName = SUBSTRING (pcSubKey, R-INDEX (pcSubKey, "~\":U) + 1, -1, "CHARACTER":U).

        regKey = FindRegistryKey (pcRegistryKey,
                                  SUBSTRING (pcSubKey, 1, R-INDEX (pcSubKey, "~\":U) - 1, "CHARACTER":U)) .

        regKey:DeleteSubKeyTree (cKeyName) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets a value in the windows registry
        Notes:
        @param pcRegistryKey The registry key
        @param pcSubKey The registry sub key
        @param pcProperty The property name
        @param pcValue The property value to set
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SetRegistryValue (pcRegistryKey AS CHARACTER,
                                                pcSubKey AS CHARACTER,
                                                pcProperty AS CHARACTER,
                                                pcValue AS LONGCHAR):

        DEFINE VARIABLE regkey AS RegistryKey NO-UNDO .

        regKey = FindRegistryKey (pcRegistryKey, pcSubKey) .

        IF NOT VALID-OBJECT(regKey) THEN
            UNDO, THROW NEW UnknownRegistryException (pcRegistryKey + "~\":U + pcSubKey) .

        regKey:SetValue(pcProperty, pcValue) .

        {Consultingwerk/CatchSystemException.i}
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a windows registry value
        Notes:
        @param pcRegistryKey The registry key
        @param pcSubKey The registry sub key
        @param pcProperty The property name
        @return The registry value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LONGCHAR GetRegistryValue (pcRegistryKey AS CHARACTER,
                                                    pcSubKey AS CHARACTER,
                                                    pcProperty AS CHARACTER):

        DEFINE VARIABLE regkey AS RegistryKey NO-UNDO .

        regKey = FindRegistryKey (pcRegistryKey, pcSubKey) .

        IF VALID-OBJECT(regKey) AND VALID-OBJECT (regKey:GetValue(pcProperty)) THEN
            RETURN regKey:GetValue(pcProperty):ToString() .
        ELSE RETURN ? .

        {Consultingwerk/CatchSystemException.i}
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Locates a RegistryKey
        Notes:
        @param pcRegistryKey The registry key
        @param pcSubKey The registry sub key
        @return The specified RegistryKey
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC RegistryKey FindRegistryKey (pcRegistryKey AS CHARACTER,
                                                      pcSubKey AS CHARACTER):

        DEFINE VARIABLE reg AS RegistryKey NO-UNDO .
        DEFINE VARIABLE i   AS INTEGER NO-UNDO.

        ASSIGN pcSubKey = REPLACE(pcSubKey, "/":U, "~\":U) .

        reg = FindRegistry(pcRegistryKey) .

        IF NOT VALID-OBJECT(reg) THEN
            RETURN ? .

        DO i = 1 TO NUM-ENTRIES(pcSubKey, "~\":U):
            IF VALID-OBJECT (reg) THEN
                ASSIGN reg = reg:CreateSubKey(ENTRY(i, pcSubKey, "~\":U)) .
        END.

        RETURN reg.

        {Consultingwerk/CatchSystemException.i}

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Locates a RegistryKey for read only access
        Notes:
        @param pcRegistryKey The registry key
        @param pcSubKey The registry sub key
        @return The specified RegistryKey
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC RegistryKey FindRegistryKeyReadOnly (pcRegistryKey AS CHARACTER,
                                                              pcSubKey AS CHARACTER):

        DEFINE VARIABLE reg AS RegistryKey NO-UNDO .
        DEFINE VARIABLE i   AS INTEGER NO-UNDO.

        ASSIGN pcSubKey = REPLACE(pcSubKey, "/":U, "~\":U) .

        reg = FindRegistry(pcRegistryKey) .

        IF NOT VALID-OBJECT(reg) THEN
            RETURN ? .

        DO i = 1 TO NUM-ENTRIES(pcSubKey, "~\":U):
            IF VALID-OBJECT (reg) THEN
                ASSIGN reg = reg:OpenSubKey(ENTRY(i, pcSubKey, "~\":U), FALSE) .
        END.

        RETURN reg.

        {Consultingwerk/CatchSystemException.i}

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Locates a root registry key
        Notes:
        @param pcRegistryKey The registry key
        @return The specified RegistryKey
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC RegistryKey FindRegistry (pcRegistryKey AS CHARACTER):

        CASE pcRegistryKey:
            WHEN "ClassesRoot":U OR WHEN "HKEY_CLASSES_ROOT":U OR WHEN "CLASSES":U THEN
                RETURN Microsoft.Win32.Registry:ClassesRoot .
            WHEN "CurrentConfig":U OR WHEN "HKEY_CURRENT_CONFIG":U OR WHEN "CONFIG":U THEN
                RETURN Microsoft.Win32.Registry:CurrentConfig .
            WHEN "CurrentUser":U OR WHEN "HKEY_CURRENT_USER":U OR WHEN "USER":U THEN
                RETURN Microsoft.Win32.Registry:CurrentUser .
            WHEN "DynData":U OR WHEN "HKEY_DYN_DATA":U THEN
                RETURN Microsoft.Win32.Registry:DynData .
            WHEN "LocalMachine":U OR WHEN "HKEY_LOCAL_MACHINE":U OR WHEN "MACHINE":U THEN
                RETURN Microsoft.Win32.Registry:LocalMachine .
            WHEN "PerformanceData":U OR WHEN "HKEY_PERFORMANCE_DATA":U THEN
                RETURN Microsoft.Win32.Registry:PerformanceData .
            WHEN "Users":U OR WHEN "HKEY_USERS":U THEN
                RETURN Microsoft.Win32.Registry:Users .
            OTHERWISE
                UNDO, THROW NEW UnknownRegistryException (pcRegistryKey) .
        END CASE .

        {Consultingwerk/CatchSystemException.i}

    END METHOD .
&ENDIF
END CLASS.
