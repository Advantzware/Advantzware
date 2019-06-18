/**********************************************************************
 * Copyright (C) 2006-2019 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/

/* in order to build packages on Linux this code must be distributed as
 * xcode -- otherwise the OPSYS begins "win" test will fail...
 *
 * to make that work the plain text source is actually in lib/setpt3dir.x.p
 * and the xcoded lib/setpt3dir.p is created thus:
 * 
 *   mkdir -p /tmp/lib 2> /dev/null
 *   xcode -d /tmp lib/setpt3dir.p
 *   cp /tmp/lib/setpt3dir.p lib/setpt3dir2.p
 * 
 * (lib/setpt3dir.p is provided as plain text source just in case anyone is curious)
 *
 */


/*------------------------------------------------------------------------
    File        : set-protop-installdir.p
    Purpose     : Sets the Protop install directory in the Windows registry

    Syntax      :

    Description : First attempts to store this at
                  HKEY_LOCAL_SYSTEM\Software\White Star Software\Protop
                  When the user has no access there, it attempts to write to
                  HKEY_CURRENT_USER\Software\White Star Software\Protop

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jan 19 11:02:53 CET 2019
    Notes       : SCL-2433
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF OPSYS BEGINS "WIN":U AND (PROVERSION BEGINS "10.2B" OR INTEGER(ENTRY (1, PROVERSION, ".")) GE 11) &THEN
&SCOPED-DEFINE DotNetAccessible
&ENDIF

&IF DEFINED (DotNetAccessible) NE 0 &THEN
ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*   FROM PROPATH .
USING Microsoft.Win32.* FROM ASSEMBLY .
&ENDIF

DEFINE INPUT  PARAMETER pcInstallDir AS CHARACTER NO-UNDO.

/* ***************************  Main Block  *************************** */

&IF DEFINED (DotNetAccessible) NE 0 &THEN
DO ON ERROR UNDO, THROW:
    RUN setRegistryValue ("LocalMachine":U,
                          "Software~\White Star Software~\ProTop":U,
                          "installdir":U,
                          pcInstallDir) .

    /* Mike Fechner, Consultingwerk Ltd. 19.01.2019
       When the current user is not authorized to write to
       the HKEY_LOCAL_SYSTEM, we'll store this under HKEY_CURRENT_USER
       instead */
    CATCH uncaught AS System.UnauthorizedAccessException:
        RUN setRegistryValue ("User":U,
                              "Software~\White Star Software~\ProTop":U,
                              "installdir":U,
                              pcInstallDir) .
    END CATCH.
END.

CATCH err AS Progress.Lang.Error:
    MESSAGE "Unhandled Error setting ProTop installdir in the Windows Registry":U SKIP
            err:GetMessage(1)
        VIEW-AS ALERT-BOX.
END CATCH.

/**
 * Purpose: Locates a root registry key
 * Notes:
 * @param pcRegistryKey The registry key
 * @param (OUTPUT) The specified RegistryKey
 */
PROCEDURE findRegistry:

    DEFINE INPUT  PARAMETER pcRegistryKey AS CHARACTER   NO-UNDO .
    DEFINE OUTPUT PARAMETER poRegistryKey AS RegistryKey NO-UNDO .

    CASE pcRegistryKey:
        WHEN "ClassesRoot":U OR WHEN "HKEY_CLASSES_ROOT":U OR WHEN "CLASSES":U THEN DO:
            ASSIGN poRegistryKey = Microsoft.Win32.Registry:ClassesRoot .
            RETURN .
        END.
        WHEN "CurrentConfig":U OR WHEN "HKEY_CURRENT_CONFIG":U OR WHEN "CONFIG":U THEN DO:
            ASSIGN poRegistryKey = Microsoft.Win32.Registry:CurrentConfig .
            RETURN .
        END.
        WHEN "CurrentUser":U OR WHEN "HKEY_CURRENT_USER":U OR WHEN "USER":U THEN DO:
            ASSIGN poRegistryKey = Microsoft.Win32.Registry:CurrentUser .
            RETURN .
        END.
        WHEN "DynData":U OR WHEN "HKEY_DYN_DATA":U THEN DO:
            ASSIGN poRegistryKey = Microsoft.Win32.Registry:DynData .
            RETURN .
        END.
        WHEN "LocalMachine":U OR WHEN "HKEY_LOCAL_MACHINE":U OR WHEN "MACHINE":U THEN DO:
            ASSIGN poRegistryKey = Microsoft.Win32.Registry:LocalMachine .
            RETURN .
        END.
        WHEN "PerformanceData":U OR WHEN "HKEY_PERFORMANCE_DATA":U THEN DO:
            ASSIGN poRegistryKey = Microsoft.Win32.Registry:PerformanceData .
            RETURN .
        END.
        WHEN "Users":U OR WHEN "HKEY_USERS":U THEN DO:
            ASSIGN poRegistryKey = Microsoft.Win32.Registry:Users .
            RETURN .
        END.
        OTHERWISE
            UNDO, THROW NEW AppError (SUBSTITUTE ("Unknonw registry error &1":U, pcRegistryKey), 0) .
    END CASE .


END PROCEDURE.

/**
 * Purpose: Locates a RegistryKey
 * Notes:
 * @param pcRegistryKey The registry key
 * @param pcSubKey The registry sub key
 * @param (OUTPUT) The specified RegistryKey
 */
PROCEDURE findRegistryKey:

    DEFINE INPUT  PARAMETER pcRegistryKey  AS CHARACTER   NO-UNDO .
    DEFINE INPUT  PARAMETER pcSubKey       AS CHARACTER   NO-UNDO .
    DEFINE OUTPUT PARAMETER reg            AS RegistryKey NO-UNDO .

    DEFINE VARIABLE i   AS INTEGER NO-UNDO.

    ASSIGN pcSubKey = REPLACE(pcSubKey, "/":U, "~\":U) .

    RUN findRegistry(pcRegistryKey, OUTPUT reg) .

    IF NOT VALID-OBJECT(reg) THEN
        RETURN ? .

    DO i = 1 TO NUM-ENTRIES(pcSubKey, "~\":U):
        IF VALID-OBJECT (reg) THEN
            ASSIGN reg = reg:CreateSubKey(ENTRY(i, pcSubKey, "~\":U)) .
    END.

END PROCEDURE.

/**
 * Purpose: Sets a value in the windows registry
 * Notes:
 * @param pcRegistryKey The registry key
 * @param pcSubKey The registry sub key
 * @param pcProperty The property name
 * @param pcValue The property value to set
 */
PROCEDURE setRegistryValue:

    DEFINE INPUT  PARAMETER pcRegistryKey AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pcSubKey      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pcProperty    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pcValue       AS LONGCHAR  NO-UNDO.

    DEFINE VARIABLE regkey AS RegistryKey NO-UNDO .

    RUN findRegistryKey(pcRegistryKey, pcSubKey, OUTPUT regKey) .

    IF NOT VALID-OBJECT(regKey) THEN
        UNDO, THROW NEW AppError (SUBSTITUTE ("Invalid registry key: &1~\&2":U, pcRegistryKey, pcSubKey), 0) .

    regKey:SetValue (pcProperty, pcValue) .

END PROCEDURE.
&ENDIF
