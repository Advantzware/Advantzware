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
    File        : UserAccountControl
    Purpose     : Microsoft Active Directory User Account Control constants
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 12 14:51:01 CET 2012
    Notes       : http://support.microsoft.com/kb/305144/en
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.Ldap.* FROM PROPATH .  
USING Progress.Lang.*               FROM PROPATH .

CLASS Consultingwerk.Windows.Ldap.UserAccountControl: 

    DEFINE PUBLIC PROPERTY SCRIPT                         AS INTEGER INITIAL        1 NO-UNDO GET. /*     0x0001 */
    DEFINE PUBLIC PROPERTY ACCOUNTDISABLE                 AS INTEGER INITIAL        2 NO-UNDO GET. /*     0x0002 */
    DEFINE PUBLIC PROPERTY HOMEDIR_REQUIRED               AS INTEGER INITIAL        8 NO-UNDO GET. /*     0x0008 */  
    DEFINE PUBLIC PROPERTY LOCKOUT                        AS INTEGER INITIAL       16 NO-UNDO GET. /*     0x0010 */  
    DEFINE PUBLIC PROPERTY PASSWD_NOTREQD                 AS INTEGER INITIAL       32 NO-UNDO GET. /*     0x0020 */  
    DEFINE PUBLIC PROPERTY PASSWD_CANT_CHANGE             AS INTEGER INITIAL       64 NO-UNDO GET. /*     0x0040 */  
    DEFINE PUBLIC PROPERTY ENCRYPTED_TEXT_PWD_ALLOWED     AS INTEGER INITIAL      128 NO-UNDO GET. /*     0x0080 */  
    DEFINE PUBLIC PROPERTY TEMP_DUPLICATE_ACCOUNT         AS INTEGER INITIAL      256 NO-UNDO GET. /*     0x0100 */  
    DEFINE PUBLIC PROPERTY NORMAL_ACCOUNT                 AS INTEGER INITIAL      512 NO-UNDO GET. /*     0x0200 */ 
    DEFINE PUBLIC PROPERTY INTERDOMAIN_TRUST_ACCOUNT      AS INTEGER INITIAL     2048 NO-UNDO GET. /*     0x0800 */  
    DEFINE PUBLIC PROPERTY WORKSTATION_TRUST_ACCOUNT      AS INTEGER INITIAL     4096 NO-UNDO GET. /*     0x1000 */  
    DEFINE PUBLIC PROPERTY SERVER_TRUST_ACCOUNT           AS INTEGER INITIAL     8192 NO-UNDO GET. /*     0x2000 */  
    DEFINE PUBLIC PROPERTY DONT_EXPIRE_PASSWORD           AS INTEGER INITIAL    65536 NO-UNDO GET. /*    0x10000 */ 
    DEFINE PUBLIC PROPERTY MNS_LOGON_ACCOUNT              AS INTEGER INITIAL   131072 NO-UNDO GET. /*    0x20000 */ 
    DEFINE PUBLIC PROPERTY SMARTCARD_REQUIRED             AS INTEGER INITIAL   262144 NO-UNDO GET. /*    0x40000 */ 
    DEFINE PUBLIC PROPERTY TRUSTED_FOR_DELEGATION         AS INTEGER INITIAL   524288 NO-UNDO GET. /*    0x80000 */ 
    DEFINE PUBLIC PROPERTY NOT_DELEGATED                  AS INTEGER INITIAL  1048576 NO-UNDO GET. /*   0x100000 */    
    DEFINE PUBLIC PROPERTY USE_DES_KEY_ONLY               AS INTEGER INITIAL  2097152 NO-UNDO GET. /*   0x200000 */    
    DEFINE PUBLIC PROPERTY DONT_REQ_PREAUTH               AS INTEGER INITIAL  4194304 NO-UNDO GET. /*   0x400000 */    
    DEFINE PUBLIC PROPERTY PASSWORD_EXPIRED               AS INTEGER INITIAL  8388608 NO-UNDO GET. /*   0x800000 */    
    DEFINE PUBLIC PROPERTY TRUSTED_TO_AUTH_FOR_DELEGATION AS INTEGER INITIAL 16777216 NO-UNDO GET. /*  0x1000000 */   
    DEFINE PUBLIC PROPERTY PARTIAL_SECRETS_ACCOUNT        AS INTEGER INITIAL 67108864 NO-UNDO GET. /* 0x04000000 */  

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation 
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PRIVATE UserAccountControl ():
		SUPER ().
		
	END CONSTRUCTOR.

END CLASS.
