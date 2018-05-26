/*------------------------------------------------------------------------
    File        : po/ftppo.p
    Purpose     :  FTP exported file to ftp server
    Syntax      : 
    Description : 
    Author(s)   : wade Kaldawi
    Created     : 
    Notes       : This program is run from programs representing specific
                : po export formats such as po/po-gpex.p
                : Those programs pass in a hard-coded string for the input
                : parameter ip-ftp-where.  ip-exp-file is simply the file
                : to be ftp'd.  The po export program to run is determined
                : by a hard-coded list in porep/r-poprt.w using the value 
                : in vendor.po-export.
                : Configuration: nk1 poexport
                :                vendor vf1
                :                nk1 for partner, e.g. kiwi
                :                   sys-ctrl.name     = Kiwi = ip-ftp-where
                :                   sys-ctrl.char-fld = trilakes
                :                   sys-ctrl.desc     = "N:\rcode\kiwi\export"
                :                po/poexport.dat config
                          
                : ttConfig.exportFormat = ip-ftp-where
                : ttConfig.destName     = sys-ctrl.char-fld (via ip-ftp-where)
  ----------------------------------------------------------------------*/
DEF INPUT PARAM ip-exp-file AS cha NO-UNDO.
DEF INPUT PARAM ip-ftp-where AS cha NO-UNDO.

{sys/inc/var.i SHARED}

DEFINE STREAM sReadLog.
DEF VAR v-ftp-file AS cha NO-UNDO.
DEFINE VARIABLE cExec          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWinScpIniFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWinScpXmlLog AS CHARACTER NO-UNDO.
DEFINE VARIABLE lConfigBased AS LOG NO-UNDO.
DEFINE VARIABLE lConfigIdentified AS LOGICAL NO-UNDO.
DEFINE VARIABLE cSingleFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
DEFINE VARIABLE cReturnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cPoConfigDir AS CHARACTER NO-UNDO.

DEF TEMP-TABLE ttConfig FIELD exportFormat  AS CHAR
                      FIELD destName AS CHAR FORMAT "x(20)"
                      FIELD ftp-site AS CHAR FORMAT "x(30)"
                      FIELD ftp-user AS CHAR 
                      FIELD ftp-passwd AS CHAR FORMAT "x(12)"
                      FIELD ftp-mode AS CHAR 
                      FIELD ftp-software AS CHAR
                      FIELD ftp-dir AS CHAR 
                      FIELD ftp-binary AS CHAR
                      FIELD ftp-script AS CHAR
                      FIELD ftp-cmd AS CHAR
                      INDEX exportFormat exportFormat
                      INDEX destName IS PRIMARY destName.
                      
{sys/inc/poexport.i}

RELEASE sys-ctrl.

RUN sys/ref/nk1look.p (INPUT cocode,  "POConfigDir", "C" /* Character*/, 
    INPUT NO /* check by cust */, 
    INPUT YES /* use cust not vendor */,
    INPUT "" /* cust */, 
    INPUT "" /* ship-to*/,
    OUTPUT cReturnChar, 
    OUTPUT lRecFound).
IF lRecFound THEN 
    cPoConfigDir = cReturnChar  .
ELSE 
    cPoConfigDir  = ".\custfiles\EDIFiles\POs".
FUNCTION getWinScpFile RETURNS CHARACTER
    (  ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    DEF VAR cWinScpExe AS CHAR NO-UNDO.
    DEF VAR cExec      AS CHAR NO-UNDO.
    
    IF SEARCH("C:\Program Files\WinSCP\winscp.com") NE ? 
        OR SEARCH("WinSCP\winscp.com") NE ?
        OR SEARCH("C:\Program Files (x86)\WinSCP\winscp.com") NE ?
        THEN 
    DO:
        /* WinSCP folder is in resources */
        cExec = SEARCH("WinSCP\winscp.com").
        IF cExec EQ ? THEN 
            cExec = SEARCH("C:\Program Files\WinSCP\winscp.com").
        IF cExec EQ ? THEN 
            cExec = SEARCH("C:\Program Files (x86)\WinSCP\winscp.com").
          
        FILE-INFO:FILE-NAME = cExec.
        cExec = FILE-INFO:FULL-PATHNAME.
        
        cExec = '"' + cExec + '"'.
    END.
    cWinScpExe = cExec.          
    RETURN cWinScpExe.   /* Function return value. */

END FUNCTION.

cWinScpIniFile = SEARCH(cPoConfigDir + "\winscp.ini").
IF cWinScpIniFile EQ ? THEN 
    cWinScpIniFile = "".
ELSE 
DO:
    FILE-INFO:FILE-NAME = cWinScpIniFile.
    cWinScpIniFile = " /ini=" + FILE-INFO:FULL-PATHNAME.
END.

cWinScpXmlLog =ENTRY(1, ip-exp-file) + ".xml".
 
RUN load-config.

IF poexport-int EQ 0 THEN
  FIND FIRST sys-ctrl NO-LOCK
       WHERE sys-ctrl.company EQ cocode
         AND sys-ctrl.name    EQ ip-ftp-where
      NO-ERROR.
     
IF AVAIL sys-ctrl THEN
  RUN set-config-based.

/* For testing the ftp cmd */
IF poexport-int EQ 2 AND USERID("nosweat") EQ "ASI" THEN
    MESSAGE "Do you want to ftp the file?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
  TITLE "" UPDATE lSendTheFile AS LOGICAL.
ELSE
  lSendTheFile = YES. /* Production */

/* Keep track of whether hard-coded config was used */
lConfigIdentified = NO.


IF ip-ftp-where EQ "Corr-U-KraftII" THEN DO:
  lConfigIdentified = TRUE. 
  OUTPUT TO VALUE(cPoConfigDir + "\ftpcmd2.txt").    /* ftp text file */

  PUT UNFORMATTED 
      "open edi.ftp.sunclipse.com" SKIP   /* ftp server ip address */
      "fibreconttest"              SKIP   /* userid */
      "Must1356"                   SKIP   /* password */
      "cd /test/ckots/fibrecont"   SKIP
      "put " ip-exp-file           SKIP   /* file to transfer */
      "quit" .
  OUTPUT CLOSE.

  OS-COMMAND VALUE("ftp -v -i -s:"+ cPoConfigDir + "\ftpcmd2.txt").
END. /* Corr-U-KraftII */

ELSE
IF ip-ftp-where EQ "kiwi" THEN DO:
  OUTPUT TO VALUE(cPoConfigDir + "\ftpkiwi.txt").    /* ftp text file */

  IF INDEX(ip-exp-file,"TL_kiwi") > 0 THEN DO:
     lConfigIdentified = TRUE.
     PUT UNFORMATTED 
         "open ftp2.fivestarsheets.com" SKIP   /* ftp server ip address */
         "customer"          SKIP   /* userid */
         "custom3r"          SKIP   /* password */
         "put " ip-exp-file  SKIP   /* file to transfer */
         "quit" .
  END.
  ELSE DO:
     lConfigIdentified = TRUE.
     PUT UNFORMATTED 
         "open ftp.fivestarsheets.com" SKIP   /* ftp server ip address */
         "customer"                    SKIP   /* userid */
         "custom3r"                    SKIP   /* password */
         "put " ip-exp-file            SKIP   /* file to transfer */
         "quit" .
  END.
  OUTPUT CLOSE.

  OS-COMMAND VALUE("ftp -v -i -s:"+ cPoConfigDir + "\ftpkiwi.txt").
END. /* Kiwi */

 


/*IF ip-ftp-where EQ "Welsh" THEN DO:                              */
/*  OUTPUT TO VALUE(cPoConfigDir + "\ftpWelsh.txt").    /* ftp text file */   */
/*                                                                 */
/*  PUT UNFORMATTED                                                */
/*      "open ftp.corrchain.com" SKIP   /* ftp server ip address */*/
/*      "ctiasiwelsh"              SKIP   /* userid */             */
/*      "a1w2!2oiz"                   SKIP   /* password */        */
/*      "cd /asi/southcorr/"   SKIP                                */
/*      "put " ip-exp-file           SKIP   /* file to transfer */ */
/*      "quit" .                                                   */
/*  OUTPUT CLOSE.                                                  */
/*                                                                 */
/*  OS-COMMAND VALUE("ftp -v -i -s:"+ cPoConfigDir + "\ftpWelsh.txt").            */
/*END. /* Welsh */                                                 */
/*                                                                 */
/*ELSE                                                             */
IF AVAIL sys-ctrl THEN DO:

  IF ip-ftp-where EQ "CorSuply" THEN DO:
    OUTPUT TO VALUE(cPoConfigDir + "\ftpcorr.txt").    /* ftp text file */

    IF sys-ctrl.char-fld EQ "Southpak" THEN DO: /* South Pak */
      lConfigIdentified = TRUE.
      PUT UNFORMATTED                       
          "open cscftp1.corrsupp.com" SKIP  /* ftp server ip address */
          "southpak"                  SKIP  /* userid*/
          "s0uthru5"                  SKIP  /* password */
          "put " ip-exp-file          SKIP  /* file to transfer */
          "quit".
    END.
    ELSE 
    IF sys-ctrl.char-fld EQ "TrePaper" THEN DO: /* Tre Paper */
      lConfigIdentified = TRUE.
      PUT UNFORMATTED                       
          "open cscftp1.corrsupp.com" SKIP  /* ftp server ip address */
          "trepaper"                  SKIP  /* userid*/
          "tr3p4p"                    SKIP  /* password */
          "binary"                    SKIP  /* ASCII */
          "prompt"                    SKIP  /* turns off interactive mode */
          "put " ip-exp-file          SKIP  /* file to transfer */
          "quit".
    END.
    ELSE DO:                                /*CSC*/
      lConfigIdentified = TRUE.
      PUT UNFORMATTED 
          "open cscftp1.corrsupp.com" SKIP  /* ftp server ip address */
          "container"                 SKIP  /* userid*/
          "c0nt4ns3r"                 SKIP  /* password */
          "put " ip-exp-file          SKIP  /* file to transfer */
          "quit".
    END.
    OUTPUT CLOSE.
  END. /* CorSuply */

/*  ELSE                                                                 */
/*  IF ip-ftp-where EQ "HRMS" THEN DO:                                   */
/*    v-ftp-file = "c:\tmp\RFC" + STRING(TIME).                          */
/*                                                                       */
/*    OS-COPY VALUE(ip-exp-file) VALUE(v-ftp-file).                      */
/*                                                                       */
/*    OUTPUT TO VALUE(cPoConfigDir + "\ftphrms.txt").    /* ftp text file */        */
/*                                                                       */
/*    PUT UNFORMATTED                                                    */
/*        "open 216.127.228.181"       SKIP   /* ftp server ip address */*/
/*        "advance"                    SKIP   /* userid*/                */
/*        "phl20corr40"                SKIP   /* password */             */
/*        "cd /usr/lib/basic/WORK/xmt" SKIP                              */
/*        "put " v-ftp-file            SKIP   /* file to transfer */     */
/*        "quit" .                                                       */
/*    OUTPUT CLOSE.                                                      */
/*  END. /* HRMS */                                                      */

  ELSE
  IF ip-ftp-where EQ "Pratt" THEN DO:
    v-ftp-file = "c:\tmp\RFC" + STRING(TIME).

    OS-COPY VALUE(ip-exp-file) VALUE(v-ftp-file).

    OUTPUT TO VALUE(cPoConfigDir + "\ftppratt.txt").    /* ftp text file */
    lConfigIdentified = TRUE.
    PUT UNFORMATTED 
        "open ftp.lovebox.com"       SKIP   /* ftp server ip address */
        "mc8mtb0xhf"                    SKIP   /* userid*/
        "dlkxd992"                SKIP   /* password */
        "cd /aspire/mc8mtb0xhf/InBox" SKIP
        "put " v-ftp-file            SKIP   /* file to transfer */
        "quit" .
    OUTPUT CLOSE.
  END. /* Pratt */

  ELSE
  IF ip-ftp-where EQ "GP" THEN DO:
    IF LOOKUP(sys-ctrl.char-fld, "PremierPkg,Woodland,Trilakes,Michcor,ST.Clair,NStock") GT 0 THEN DO:
        lConfigIdentified = TRUE.
      OUTPUT TO VALUE(cPoConfigDir + "\ftpcmdgp.txt").   /* ftp text file */
  
  
      /* New Destination in 2015 */
      PUT UNFORMATTED 
          "option batch abort"   SKIP.
      PUT UNFORMATTED 
          "option confirm off"   SKIP.

      IF sys-ctrl.char-fld EQ "Woodland" THEN  /* Woodland */
      PUT UNFORMATTED "open WOODLNDM:WDFTP3@van-ftp.nubridges.net" SKIP.
  
      ELSE
      IF sys-ctrl.module EQ "T"          OR
         sys-ctrl.char-fld EQ "Trilakes" THEN /* Trilakes */
         PUT UNFORMATTED "open trilake:Triftp1@van-ftp.nubridges.net" SKIP.
  
      ELSE
      IF sys-ctrl.char-fld EQ "Michcor" THEN  /* Michcor */
      PUT UNFORMATTED "open michor:Micftp9@van-ftp.nubridges.net" SKIP.
  
      ELSE
      IF sys-ctrl.char-fld EQ "PremierPkg" THEN  /* PremierPkg */
        PUT UNFORMATTED "open PREMPKG:PREMFTP8@van-ftp.nubridges.net" SKIP.
  
      ELSE
      IF sys-ctrl.char-fld EQ "ST.Clair" THEN  /* ST.Clair */
       PUT UNFORMATTED "open STCLAIRPKG:STCLPFTP8@van-ftp.nubridges.net" SKIP.
  
      ELSE
      IF sys-ctrl.char-fld EQ "NStock" THEN  /* NStock */
         PUT UNFORMATTED "open NSTOCKBOX:NSTOCKFTP4@van-ftp.nubridges.net" SKIP.
      PUT UNFORMATTED
        "cd inbox"                 SKIP.       /* test or prod */
      PUT UNFORMATTED
          "put " ip-exp-file       SKIP .      /* file to transfer */
      PUT UNFORMATTED    "close" SKIP .     
      PUT UNFORMATTED "Exit" SKIP.   
  
  
      OUTPUT CLOSE.

    END. /* hard coded setups */
    ELSE IF lConfigBased THEN DO:
          FIND FIRST ttConfig 
              WHERE ttConfig.exportFormat EQ ip-ftp-where
                AND ttConfig.destName EQ sys-ctrl.char-fld
              NO-LOCK NO-ERROR.
            
          IF AVAIL ttconfig THEN DO:     
               OUTPUT CLOSE.
               RUN config-based-script.
          END.
    END.
        .
  END. /* GP  */
  ELSE
     IF ip-ftp-where EQ "Smurfit" THEN DO:
        OUTPUT TO VALUE(cPoConfigDir + "\ftpsmur.txt").    /* ftp text file */
        lConfigIdentified = TRUE.
        PUT UNFORMATTED 
            "open gwftpe.smurfit.com" SKIP  /* ftp server ip address */
            "ContainerServices"       SKIP  /* userid*/
            "5946KTAyTj"              SKIP  /* password */
            "cd /In"  SKIP
            "put " ip-exp-file        SKIP  /* file to transfer */
            "quit".
    
        OUTPUT CLOSE.
     END. /* Smurfit */
  ELSE
     IF ip-ftp-where EQ "CorrChoice" THEN DO:

        OUTPUT TO VALUE(cPoConfigDir + "\ftpcc.txt").    /* ftp text file */

        IF sys-ctrl.char-fld EQ "PremierPkg" THEN DO: /* PremierPkg */
            lConfigIdentified = TRUE.
           cExec = getWinScpFile().
           IF cExec NE ? AND cExec GT "" THEN DO:
             
              PUT UNFORMATTED 
                  "option batch abort"   SKIP.
              PUT UNFORMATTED 
                  "option confirm off"   SKIP.
              PUT UNFORMATTED "open ftp://premierpleky:C8S1f9zyWy@corrchoiceb2b.greif.com" SKIP.
              PUT UNFORMATTED
                "cd mcc"                 SKIP.       /* test or prod */
              PUT UNFORMATTED
                  "put " ip-exp-file       SKIP .      /* file to transfer */
              PUT UNFORMATTED    "close" SKIP .     
              PUT UNFORMATTED "Exit" SKIP.   
           END.
           ELSE
           PUT UNFORMATTED 
               "open corrchoiceb2b.greif.com" SKIP  /* ftp server ip address */
               "premierpleky"         SKIP     /* userid */
               "C8S1f9zyWy"           SKIP     /* password */
               "cd mcc"  SKIP
               "put " ip-exp-file     SKIP     /*file to transfer */
               "quit".
        END.
        ELSE IF sys-ctrl.char-fld EQ "NStock" THEN DO: /* NStock */
           lConfigIdentified = TRUE.
           PUT UNFORMATTED 
               "open corrchoiceb2b.greif.com" SKIP  /* ftp server ip address */
               "nstockbownoh"         SKIP     /* userid */
               "uBv56G1iqR"           SKIP     /* password */
               "cd cci"  SKIP
               "put " ip-exp-file     SKIP     /*file to transfer */
               "quit".       
        END. 
        ELSE IF sys-ctrl.char-fld EQ "McElroy" THEN DO: /* McElroy */
           lConfigIdentified = TRUE.
           PUT UNFORMATTED 
               "open corrchoiceb2b.greif.com" SKIP  /* ftp server ip address */
               "mcelroyperoh"         SKIP     /* userid */
               "AE2bHYoKQh"           SKIP     /* password */
               "cd opc"  SKIP
               "put " ip-exp-file     SKIP     /*file to transfer */
               "quit".       
        END.
        ELSE IF sys-ctrl.char-fld EQ "PkgSpec" THEN DO: /* Packaging Specialties */
           lConfigIdentified = TRUE.
           PUT UNFORMATTED 
               "open corrchoiceb2b.greif.com" SKIP  /* ftp server ip address */
               "packaginorse"         SKIP     /* userid */
               "H1j0v1HLMa"           SKIP     /* password */
               "cd mpm"  SKIP
               "put " ip-exp-file     SKIP     /*file to transfer */
               "quit".
        END. 
        OUTPUT CLOSE.
     END. /* Corrchoice */
   ELSE
     IF ip-ftp-where EQ "AlliFlutes" THEN DO:
         
         OUTPUT TO VALUE(cPoConfigDir + "\ftpaf.txt").    /* ftp text file */
         IF sys-ctrl.char-fld EQ "PremierPkg" THEN 
         DO: /* PremierPkg */
             lConfigIdentified = TRUE.
             cExec = getWinScpFile().
             IF cExec NE ? AND cExec GT "" THEN 
             DO:
             
                 PUT UNFORMATTED 
                     "option batch abort"   SKIP.
                 PUT UNFORMATTED 
                     "option confirm off"   SKIP.
                 PUT UNFORMATTED 
                     "open ftp://FluPremier850:j7aG3Gp79k@edi.schwarzdata.com" SKIP.
                 PUT UNFORMATTED
                     "put " ip-exp-file       SKIP .      /* file to transfer */
                 PUT UNFORMATTED    
                     "close" SKIP .     
                 PUT UNFORMATTED 
                     "Exit" SKIP.   
             END.
             ELSE
                 PUT UNFORMATTED 
                     "open edi.schwarzdata.com" SKIP  /* ftp server ip address */
                     "FluPremier850"         SKIP     /* userid */
                     "j7aG3Gp79k"           SKIP     /* password */
                     "put " ip-exp-file     SKIP     /*file to transfer */
                     "quit".
         END.        
         ELSE IF sys-ctrl.char-fld EQ "McElroy" THEN DO: /* McElroy */     
           lConfigIdentified = TRUE.  
           PUT UNFORMATTED 
               "open edi.schwarzdata.com" SKIP  /* ftp server ip address */
               "AlsMep850"         SKIP     /* userid */
               "7k6uQVf48m"           SKIP     /* password */              
               "put " ip-exp-file     SKIP     /*file to transfer */
               "quit".       
         END.
         ELSE IF sys-ctrl.char-fld EQ "Bell" THEN DO:  /* Bell */
                 lConfigIdentified = TRUE.
                 PUT UNFORMATTED 
                     "open edi.schwarzdata.com" SKIP  /* ftp server ip address */
                     "FrdmBell850"         SKIP     /* userid */
                     "QNB5xtnnFF"           SKIP     /* password */
                     /* "cd opc"  SKIP */
                     "put " ip-exp-file     SKIP     /*file to transfer */
                     "quit".     
         END. 
         ELSE IF sys-ctrl.char-fld EQ "Capitol" THEN  DO: /* Capitol Specialties */
             lConfigIdentified = TRUE.
             cExec = getWinScpFile().
             
             IF cExec NE ? AND cExec GT "" THEN 
             DO:
             
                 PUT UNFORMATTED 
                     "option batch abort"   SKIP.
                 PUT UNFORMATTED 
                     "option confirm off"   SKIP.
                 PUT UNFORMATTED 
                     "open ftp://FluCapCity850:kHCJ4cb5zg@edi.schwarzdata.com" SKIP.
                 PUT UNFORMATTED
                     "put " ip-exp-file       SKIP .      /* file to transfer */
                 PUT UNFORMATTED    
                     "close" SKIP .     
                 PUT UNFORMATTED 
                     "Exit" SKIP.   
             END.
             ELSE 
                 PUT UNFORMATTED 
                     "open edi.schwarzdata.com" SKIP  /* ftp server ip address */
                     "FluCapCity850"         SKIP     /* userid */
                     "kHCJ4cb5zg"           SKIP     /* password */
                     "put " ip-exp-file     SKIP     /*file to transfer */
                     "quit".     
         END.
         ELSE IF lConfigBased THEN DO:
             lConfigIdentified = TRUE.
            FIND FIRST ttConfig 
              WHERE ttConfig.exportFormat EQ ip-ftp-where
                AND ttConfig.destName EQ sys-ctrl.char-fld
              NO-LOCK NO-ERROR.
            
            IF AVAIL ttconfig THEN DO:     
               OUTPUT CLOSE.
               RUN config-based-script.
            END.
         END.
        OUTPUT CLOSE.
     END. /* AlliFlutes */
   
    /* ip-ftp-where was not listed */
    IF NOT lConfigIdentified THEN DO:
        
         IF lConfigBased THEN DO:
                lConfigIdentified = TRUE.
                FIND FIRST ttConfig 
                  WHERE ttConfig.exportFormat EQ ip-ftp-where
                    AND ttConfig.destName EQ sys-ctrl.char-fld
                  NO-LOCK NO-ERROR.
                
                IF AVAIL ttconfig THEN DO:     
                   OUTPUT CLOSE.
                   RUN config-based-script.
                END.
            
            OUTPUT CLOSE.
         END.
         ELSE 
           OS-COPY VALUE(ip-exp-file) VALUE("c:\tmp\ftpexppo.txt").
           
    END.
    IF NOT lConfigIdentified THEN DO:
    END.

  OS-COMMAND SILENT VALUE("@echo off").

  IF lSendTheFile THEN DO:
      IF lConfigBased AND lConfigIdentified THEN 
      DO:
          /* Preferred method - Based on poexport.dat */
          IF SEARCH(cPoConfigDir + "\" + ttConfig.ftp-script) EQ ? THEN 
          DO:
              MESSAGE "File to transfer was not found: " cPoConfigDir + "\" + ttConfig.ftp-script 
                  VIEW-AS ALERT-BOX. 
              RETURN.
          END. 
          IF getWinScpFile() EQ ? OR getWinScpFile() EQ "" THEN DO:
              MESSAGE "PO was not transfered - file transfer software 'WinScp' was not found."
              VIEW-AS ALERT-BOX.
              RETURN. 
          END.
          FIND FIRST ttConfig 
              WHERE ttConfig.exportFormat EQ ip-ftp-where
              AND ttConfig.destName EQ sys-ctrl.char-fld
              NO-LOCK NO-ERROR.
          IF AVAILABLE ttConfig THEN 
          DO:
              cExec = getWinScpFile().
              IF cWinScpIniFile GT "" THEN 
                  cExec = cExec + " " + "/ini=" + cWinScpIniFile + " " + "/xmllog=" + cWinScpXmlLog.  
                
              OS-COMMAND NO-WAIT VALUE(cExec + " /script="+ cPoConfigDir + "\" + ttConfig.ftp-script).
               
          END. 
      END.
      ELSE 
      DO:
          CASE ip-ftp-where:    

          /* Original method - hard-coded */
          WHEN "HRMS" THEN DO:
              
            /* OS-COMMAND VALUE("ftp -v -i -s:"+ cPoConfigDir + "\ftphrms.txt"). */ 
            IF AVAIL(sys-ctrl) /* AND sys-ctrl.int-fld EQ 1 */ 
                AND  getWinScpFile() NE ?
                THEN 
            DO:
                
                cExec = getWinScpFile().
                IF cWinScpIniFile GT "" THEN 
                    cExec = cExec + " " + "/ini=" + cWinScpIniFile + " " + "/xmllog=" + cWinScpXmlLog.  
                
                OS-COMMAND VALUE(cExec + " /script="+ cPoConfigDir + "\ftphrms.txt").
            END.
            ELSE
                OS-COMMAND VALUE("ftp -v -i -s:"+ cPoConfigDir + "\ftphrms.txt").
          END.
          WHEN "Pratt" THEN
            OS-COMMAND VALUE("ftp -v -i -s:"+ cPoConfigDir + "\ftppratt.txt").
      
          WHEN "CorSuply" THEN
            OS-COMMAND VALUE("ftp -v -i -s:"+ cPoConfigDir + "\ftpcorr.txt").
      
          WHEN "Smurfit" THEN
            OS-COMMAND VALUE("ftp -v -i -s:"+ cPoConfigDir + "\ftpsmur.txt").
      
          WHEN "GP" THEN DO:     
            
            IF AVAIL(sys-ctrl) AND sys-ctrl.int-fld EQ 1 
               AND  getWinScpFile() NE ?
               THEN 
            DO:
                cExec = getWinScpFile().
                IF cWinScpIniFile GT "" THEN 
                  cExec = cExec + " " + "/ini=" + cWinScpIniFile + " " + "/xmllog=" + cWinScpXmlLog.  
                
                OS-COMMAND VALUE(cExec + " /script="+ cPoConfigDir + "\ftpcmdgp.txt").
            END.
            ELSE
              OS-COMMAND VALUE("ftp -v -i -s:"+ cPoConfigDir + "\ftpcmdgp.txt").
          END.
          WHEN "CorrChoice" THEN DO:
            IF sys-ctrl.char-fld EQ "PremierPkg" AND cExec NE ? AND cExec NE "" THEN DO:      
              cExec = getWinScpFile().
              IF cWinScpIniFile GT "" THEN 
                  cExec = cExec + " " + "/ini=" + cWinScpIniFile + " " + "/xmllog=" + cWinScpXmlLog.          
              OS-COMMAND SILENT VALUE(cExec + " /script="+ cPoConfigDir + "\ftpcc.txt").
            END.
            ELSE
              OS-COMMAND VALUE("ftp -v -i -s:"+ cPoConfigDir + "\ftpcc.txt").
          END.
          WHEN "AlliFlutes" THEN 
            DO:
            
                IF (sys-ctrl.char-fld EQ "PremierPkg" OR sys-ctrl.char-fld EQ "Capitol" OR (lConfigBased AND cWinScpIniFile NE "" AND cWinScpIniFile NE ?)) AND cExec NE ? AND cExec NE "" THEN 
                DO:      
                    cExec = getWinScpFile().
                    
                    IF cWinScpIniFile GT "" THEN 
                        cExec = cExec + " " + "/ini=" + cWinScpIniFile + " " + "/xmllog=" + cWinScpXmlLog.          
                    OS-COMMAND SILENT VALUE(cExec + " /script="+ cPoConfigDir + "\ftpaf.txt").
                END.
                ELSE
                    OS-COMMAND VALUE("ftp -v -i -s:"+ cPoConfigDir + "\ftpaf.txt").
            END.
          WHEN "ipaper" THEN DO:     
            
            IF   getWinScpFile() NE ?
               THEN 
            DO:
                cExec = getWinScpFile().
                IF cWinScpIniFile GT "" THEN 
                  cExec = cExec + " " + "/ini=" + cWinScpIniFile + " " + "/xmllog=" + cWinScpXmlLog.  
                
                OS-COMMAND VALUE(cExec + " /script="+ cPoConfigDir + "\ftpip.txt").
            END.
            ELSE
              OS-COMMAND VALUE("ftp -v -i -s:"+ cPoConfigDir + "\ftpcmdip.txt").
          END.        
          WHEN "Welsh" THEN DO:
       
            IF cExec NE ? AND cExec NE "" THEN DO:      
              cExec = getWinScpFile().
              IF cWinScpIniFile GT "" THEN 
                  cExec = cExec + " " + "/ini=" + cWinScpIniFile + " " + "/xmllog=" + cWinScpXmlLog.    
                 
              OS-COMMAND SILENT VALUE(cExec + " /script="+ cPoConfigDir + "\ftpct.txt").
            END.
            ELSE
              OS-COMMAND VALUE("ftp -v -i -s:"+ cPoConfigDir + "\ftpcc.txt").
          END.      
        END CASE.
    END.
    
    RUN checkXmlLogResult.
  END. /* If lSendTheFile */
END. /* If avail sys-ctrl */


PROCEDURE checkXmlLogResult:
    DEFINE VARIABLE cLogLine AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lTransferSuccess AS LOGICAL NO-UNDO.
    lTransferSuccess = TRUE.
    IF SEARCH(cWinscpXmlLog) NE ? THEN DO:
        lTransferSuccess = FALSE.
        INPUT STREAM sReadLog FROM VALUE(cWinScpXmlLog).
        REPEAT:
            cLogLine = "".
            IMPORT STREAM sReadLog UNFORMATTED cLogLine.
            IF INDEX(cLogLine, "Result") GT 0 AND INDEX(cLogLine, "success") GT 0 THEN DO:
                IF INDEX(cLogLine, "true") GT 0 THEN DO:
                    lTransferSuccess = TRUE.
                END.
            END. 
        END. /* repeat */
        INPUT STREAM sReadLog CLOSE.

        IF NOT lTransferSuccess THEN 
           MESSAGE "Warning: The purchase order was not transmitted."
                   VIEW-AS ALERT-BOX.
        
    END.
END PROCEDURE.

PROCEDURE config-based-script:

        IF ttConfig.ftp-software EQ "FTP" THEN DO:
            
            
            OUTPUT TO VALUE(cPoConfigDir + "\" + ttConfig.ftp-script).    /* ftp text file */
            
            PUT UNFORMATTED 
                "open " + ttConfig.ftp-site  SKIP   /* ftp server ip address */
                ttConfig.ftp-user            SKIP   /* userid */
                ttConfig.ftp-passwd          SKIP.   /* password */

            IF ttConfig.ftp-binary GT "" THEN
              PUT UNFORMATTED
                ttConfig.ftp-binary          SKIP.

            IF ttConfig.ftp-dir GT "" THEN 
            PUT UNFORMATTED
                "cd " + ttConfig.ftp-dir     SKIP.            
           
            DO iCnt = 1 TO NUM-ENTRIES(ip-exp-file):
                cSingleFile = ENTRY(iCnt, ip-exp-file).

                PUT UNFORMATTED
                     ttConfig.ftp-cmd + " " + cSingleFile SKIP .   /* file to transfer */
            END. 
            
            PUT UNFORMATTED
                "quit" .


            OUTPUT CLOSE.
                        
            
        END. /* FTP */
    
        IF ttConfig.ftp-software EQ "winSCP" THEN 
        DO:
            cWinScpIniFile = SEARCH(cPoConfigDir + "\winscp.ini").
            cExec = getWinScpFile().
            IF cWinScpIniFile EQ ? THEN 
                cWinScpIniFile = "".
            ELSE 
            DO:
                FILE-INFO:FILE-NAME = cWinScpIniFile.
                cWinScpIniFile = " /ini=" + FILE-INFO:FULL-PATHNAME.
            END.
            
            OUTPUT TO VALUE(cPoConfigDir + "\" + ttConfig.ftp-script). 
            
    
            PUT UNFORMATTED 
                "option batch abort"                  SKIP.

            PUT UNFORMATTED 
                "option confirm off"                  SKIP.

            PUT UNFORMATTED "open " 
             + ttConfig.ftp-mode + ":" + ttConfig.ftp-user + ":" + ttConfig.ftp-passwd + "@" + ttConfig.ftp-site SKIP.

            IF ttConfig.ftp-binary GT "" THEN
              PUT UNFORMATTED
                ttConfig.ftp-binary                   SKIP.

            IF ttConfig.ftp-dir GT "" THEN 
                PUT UNFORMATTED
                    "cd " + ttConfig.ftp-dir          SKIP.            


            DO iCnt = 1 TO NUM-ENTRIES(ip-exp-file):
                cSingleFile = ENTRY(iCnt, ip-exp-file).

                PUT UNFORMATTED
                    ttConfig.ftp-cmd + " " + cSingleFile SKIP .   /* file to transfer */
            END. 
            
            PUT UNFORMATTED    
                "close"                               SKIP .     

            PUT UNFORMATTED 
                "Exit"                                SKIP.   

            OUTPUT CLOSE.


        END. /* WinScp */

   
END PROCEDURE.

PROCEDURE load-config:

  EMPTY TEMP-TABLE ttConfig.

  IF SEARCH(cPoConfigDir + "\poexport.dat") NE ? THEN DO:

      INPUT FROM VALUE(cPoConfigDir + "\poexport.dat").
      REPEAT:
          
          CREATE ttConfig.
          IMPORT ttConfig.exportFormat ttConfig.destName ttConfig.ftp-site ttConfig.ftp-user ttConfig.ftp-passwd
                 ttConfig.ftp-mode ttConfig.ftp-dir ttConfig.ftp-software
                 ttConfig.ftp-binary ttConfig.ftp-script ttConfig.ftp-cmd.
    
          IF ttConfig.exportFormat BEGINS "#" OR ttConfig.exportFormat EQ "" THEN
            DELETE ttConfig.
    
      END.
      INPUT CLOSE.
      
  END.
END PROCEDURE. /* load-config */

PROCEDURE set-config-based:

  FIND FIRST ttConfig WHERE ttConfig.exportFormat EQ ip-ftp-where
   AND ttConfig.destName EQ sys-ctrl.char-fld
  NO-LOCK NO-ERROR.
  

  IF AVAIL ttConfig THEN
    lConfigBased = TRUE.
  ELSE
    lConfigBased = FALSE.

END PROCEDURE. /* set-config-based */
