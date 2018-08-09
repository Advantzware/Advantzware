/*************************************************************************
    Programe Name :  Sys/ref/sys-ctrl.i                                  * 
    Author        :                                                      * 
    Date          :                                                      * 
    Desc          :  Include file for sys control paramaters             * 
                                                                         *
*************************************************************************/
DEF VAR name-fld-list AS CHAR NO-UNDO.

name-fld-list =
/*    1      2       3     4        5        6      7        8      9       10      11      12       13      14      15       16     17   18      19        20     21     22      23   24     25      26       27       28     29       30      31      32      33       34      35      36       37   38    39     40        41     42       43      44       45 */
"quoprint,invprint,cerun,msfcalc,2piececd,fastoe,setprint,poprint,relprint,bolfmt,chkfmt,addrelse,ackhead,jobcard,boxdesum,job qty,job#,bolwhse,closejob,relpost,pocost,loadtag,arexp,cemenu,ceroute,oeautopo,sht calc,ceround,bolcert,cerunc,cerunf,jobcardc,jobcardf,oecomm,oecarier,autopost,apinq,poqty,arinq,schedule,fgitem#,fgsetrec,bolpost,lastship,oereleas," +
"ap gl#,autoissu,quoprice,rmissue,custsize,poexport,cewhschg,fgpostgl,foamcost,logocolr,cepanel,sellpric,oepopup,oeclose,invdate,boldate,oeautofg,quosheet,fgemails,tspost,fgrecpt,pouom,fgwhsbin,cadcam,efbrowse,rmrecpt,fgbrowse,rmwhsbin,rmpostgl,sstransf,payvend,ou6brows,cewhatif,oereordr,oeship,fginvrec,inexport,cecopy,ceprint,ceprep,cecscrn,ceprice,relmerge," +
/* 46     47        48      49      50       51       52        53       54        55      56      57       58      59      60     61       62       63       64      65     66      67     68      69      70      71      72       73        74       75      76       77       78       79      80      81       82      83     84      85     86      87      88  */
/*  89       90       91     92      93      94       95    96   97     98      99       100       101      102     103    104      105       106    107   108       109       110       111      112       113       114      115       116       117    118      119      120       121     122        123        124         125     126     127     128        129  */
"stmtprint,TSFinish,appaper,cecunit,fgpost,corsuply,fgitemsf,GP,oeprep,celayout,1099misc,RFQPrint,oecredit,maxbreak,aptax,rmemails,sspostfg,bolfmtx,schdcard,TSTIME,FGReOrder,ARMEMO,APCheckFile,OEPrompt,SSBOLSCAN,INVCOPYS,REMOVED,BORELDATE,OEINQ,ECBROWSE,VENDXFER,CUSTXFER,ORDERXFER,MISCJOBCL,RMUnderOver,CEPREPPRICE,RELTYPE,SSMoveFG,CEMISC,BolPostTime,CEDeliveryZone," +
/* 130         131      132     133           134        135      136      137      138      139    140      141           142       143           144      145         146      147        148        149       150          151        152    153               154               155            156        157                   158         159       */
"BOLFreight,CESAMPLE,SSRMISSUE,CorrTrim,CustShipToImp,OEScreen,fgoecost,runship,InvStatus,AGEDAYS,FGPostCmp,AckMaster,ChkFmtACH,OeDateChange,SSBOLEMAIL,FGRecptUnit,FGBrowseIA,AlliFlutes,SSBOLPRINT,POScreen,SSScanVendor,BOLFMTTran,POStatus,BOLMaster,CEMarkupMatrixLookup,overwriteJobPlan,capacityPage,OEPriceMatrixCheck,BOLPartial,OEAutoDateUpdate".

DEFINE VARIABLE str-init AS CHARACTER EXTENT 159 NO-UNDO.
    
ASSIGN
 str-init[1]  = "ABox,10 Pitch,ContSrvc,CSC-EXCEL,TRILAKE-EXCEL,HOP," +
                "Triad,Fibre,Harwell,Pacific,Xprint,SouthPak,Midwest," +
                "Century,Concepts,Oracle,PremierX,Elite,Unipak,OTTPkg,Xprint30,StClair," +
                "Frankstn,Mirpkg,APC,FibreX,PPI,Fibre-Excel,Packrite,Nosco-Excel," + 
                "MSPACK-Excel,AllWest,Simkins,CCC,Soule,SouleMed,MWFIBRE,Loylang,Protagon," +
                "PeachTree,LoylangBSF,Premier-Excel,Oklahoma,Accord,Hughes,Sultana,CCC-Excel,Axis,Boss,Bell-Excel,Perform," +
                "Printers,quoprint 1,quoprint 2,quoprint 10,quoprint 20,quoprint10-CAN,Premier-excel-mci,quoprint 11,RFC"
 str-init[2]  = "Capitol,ASI,Beeler,Argrov,ILWalker,1/2 Page,Phoenix,Color,Interpac,Royal,Keystone," +
                "Livngstn,ContSrvc,Rudd,Premier,MultiWll,Imperial,PAC 1/2," +
                "Colonial,Clev 1/2,Triad,Danbury,TriState,Blueridg,Sonoco," +  
                "P&P,HOP,Empire,Brick,Acme,Allpkg,Valley,Century,Express," +
                "MaxPak,Fibre,Abox,Harwell,CorrCart,Chillic,Pacific,Xprint," +
                "Midwest,SouthPak,SouthPak-xl,Hughes,Hughes2,BoxTech,Concepts,Herman,ASIXprnt," +
                "MWBox,CentBox,Dayton,Oracle,RFC,PremierX,CSC,Frankstn,Mirpkg," +
                "CSCIN,Elite,Unipak,AllpkgX,RuddX,APC,Abox-Xp,Consbox,SonocoX," +
                "BlueRX,ImperiaX,ColoniaX,Indiana,ArgrovX,AgMach,HPB,PPI,ABC,Fibrex," +
                "TriLakes,Nosco,Nosco1,Southpakl,Androp,Packrite," +
                "Rosmar,Badger,Badger-Emailed,AllWest,Simkins,ACPI,ColorX,Loylang," +
                "ColonialLot#,LoylangJIT,Carded,CCCFGLot,CSC-GA,CCCFGL3,Adapt,Soule,SouleMed,CapCityIN," +
                "Peachtreefgl3,Peachtree,DEE,CSC-GASummary,TrilakesBroker,Accord,NStock,LoylangBSF,PremierS,Printers," +
                "Protagon,Protagon2,SoulePO,RFCX,Central,Bell,PrestigeLLB,Axis,Boss,CSCINStamp,PrystupExcel,Coburn," +
                "invprint 1,invprint 2,invprint 10,invprint 20,Lovepac,invprint10-CAN,Shamrock"
 str-init[3]  = "ASI,Clevelnd,McLean,Suthrlnd,HOP,Brick,Peachtre"
 str-init[4]  = "Foldware,Corrware"
 str-init[5]  = "D Pallet,Z Trailr"
 str-init[6]  = "ASI,Argrov,Beeler,ILWalker"
 str-init[7]  = "ASI,McLean"
 str-init[8]  = "16th's,32nd's,Century,Middlesx,RFC,Sonoco,Rudd,Brick,Fibre,Metro,"
              + "Pacific,Xprint,SouthPak,SouthPak-xl,CentBox,Xprint2,Oracle,"
              + "PremierX,CSC,Elite,OTTPkg,APC,ConsBox,FibreX,ASIXprnt,Valley,PPI,CSC-GA,"
              + "HPB,Indiana,MWFibre,Packrite,Allwest,ACPI,Badger,CCC,Soule,SouleMed,"
              + "Protagon,Protagon2,PremierCX,PeachTree,PremierXFGItems,Hughes,Boss,Bell,StClair,Sultana,"
              + "Coburn,poprint 1,poprint 2,poprint 10,poprint 20,Lovepac,POPrint10-CAN,LancoYork"
 str-init[9]  = "ASI,Argrov,Century,HOP,MultiWll,Sonoco,TriState,Fibre," +
                "Premier,Pacific,Xprint,CentBox,HOPX,Xprint2,Frankstn,APC,FibreX,Indiana,PPI," +
                "HPB,Keystone,Hughes,Allwest,Rosmar,Loylang,Carded,PremierX,CSC-GA,CardedX," +
                "Protagon,Peachtree,ACPI,Soule,NStock,Multicell,Prystup,Metro,StClair," +
                "Frank,Axis,Distributor,Accord,Lakeside,relprint 1,relprint 2,relprint 10,Midwest,RFC"
 str-init[10] = "ASI,1/2 Page,Royal,ContSrvc,Superior,Premier,Warren,PAC 1/2," +
                "Imperial,P&P,Triad,TriState,BlueRidg,BlueRidg2,Sonoco,HOP,CCC," +
                "Boxtech,Empire,Brick,AllPkg,Fibre,Maxpak,Oracle,Harwell,Century," +
                "Inland,Chillic,Coburn,Midwest,Pacific,Xprint,MidwestX,Hughes,HughesTags,Concepts," +
                "RFC,MWBox,Intrpack,Dayton,Xprint2,SouthPak,SouthPak-XL,PremierX,PremierPX," +
                "Frankstn,Mirpkg,CSCIN,Elite,ACPI,P&PX,Express,Lamar Pkg,OTTPkg,DEE,APC,ConsBox," +
                "Indiana,Michcor,FibreCI,Herman,PPI,PackRite,Hamilton,Trilakes,CapCityIN,TrilakesX,Fibrex," +
                "Allwest,COLOR,Badger,LoyLang,Carded,Metro,Nosco,CSC-GA,KDWILLSX,Peachtree,Protagon," +
                "PremierCX,PeachTreeBC,ACCORDBC,Soule,NSTOCK,TrilakesLot#,Multicell,MetroTags,AllPkg2,PremierBroker,SouleMed,Chillicothe," +
                "CapitolBC,CardedBC,Axis,BadgerSoldTo,PeachtreeLotPO,Lakeside,CSCINStamp,CCCWPP,Prystup-Excel,Printers,Printers2,RFCX," +
                "bolfmt 1,bolfmt 2,bolfmt 10,bolfmt 20,bolfmt10-CAN,PremierXFooter,Wingate-BOL,BOLfmt15"
 str-init[11] = "ASI,Laser,P&P,n,Raritan,ContSrvc,Royal,Triad,Rudd,"   +
                "Hartford,AIHalper,Brick,Fibre,Herman,ASILaser,TriadLas,"      +
                "Chillic,Middlesx,Hughes,ASI2000,Unipak,RFC,Inland,Prefered,"  +
                "Dayton,AllLaser,IPLaser,Trilakes,ADVLaser,ColorLas,Harwllas," +
                "Midwest,Argvlas,Oracle,Vineland,Lakelas,Imperial,IndianaL,"   +
                "Frankstn,Pacific,STCLaser,Woodland,Hamilton,PrePkgLS,FibreLsr," +
                "Action,CAPLasAL,ACPI,Carded,Adaptls,Soule,MidYork,ASSILaser,Laser" +
                "Protagon,AllLaserHP1536DN,ASIX,CustCorr,chkfmt 1,chkfmt 2,Lovepac,APChkFmt1"
 str-init[12] = "ASI,Bin/Tag,No Tags".
ASSIGN
 str-init[13] = "ASI,WesInd,ILWalker,HOP,Brick,Gulf,Pacific,Xprint,SouthPak,"   +
                "Century,Oracle,PremierX,Frankstn,Mirpkg,Unipak,OTTPkg,Shelby," +
                "RUDDX,APC,Imperial,Indiana,Fibrex,PPI,HPB,Albert,ContSvc,Triad,Packrite,Dee," +
                "Allwest,Simkins,HOPX,PremierCX,Accord,Soule,ACPI,Hughes,Carded,SouleUOM,Badger,Axis," +
                "3CPack,3CPackSD,Perform,ackhead 1,ackhead 2,ackhead 10,ackhead 20,ackhead10-CAN,Shamrock-Ack"
 str-init[14] = "10 Pitch,17 Pitch,Boxtech,Phoenix,TriState,Triad,RFC,HOP," +
                "Brick,Hartford"
 str-init[15] = "Inches,MM,Both"
 str-init[16] = ",Net Shts"
 str-init[17] = ",Order#,YYMMSEQ#,PLine&Order#,Estimate#"
 str-init[18] = "FG BIN,SHIPTO,FIFO,ShipFromWhse"
 str-init[19] = "Manually,FGPost,OrdClose,TS"
 str-init[20] = "Nothing,Invoice,BOL,BOL/TAG,BOL/REL"
 str-init[21] = "JobFile,Vendor,Vendor/MSH"
 str-init[22] = "ASI,Triad,BarOne,SSBarOne,SSLabel,CentBox"
 str-init[23] = "ASI,Sonoco,Inland,Excel"
 str-init[24] = "Corrware,Foldware,Both"
 str-init[26] = "Auto,Manual,AutoRM"
 str-init[27] = "QOH>QEst,AllItems,QAvail>0"
 str-init[28] = "Penny,Dollar"
 str-init[29] = ",Brick,Xprint,Unipak-XL,PremierPkg,ACPI,PremierPkgU,PremierPkgM,PremierBroker,CCC,LoyLang,CCCWPP,Prystup," +
                "bolcert 1,bolcert 2,BOLCERT10,CCC2"
 str-init[30] = "ASI,Clevelnd,McLean,Suthrlnd,Brick,Fibre,Protagon,cerunc 1,cerunc 2,cerunc 3"
 str-init[31] = "ASI,McLean,HOP,Dee,Fibre,cerunf 1,cerunf 2"
 str-init[32] = "XPRINT,Pacific,RFC,Hughes,HPB,MWFIBRE,ARTIOS,P&,MWBox," +
                "CSC,CSC-GA,PREMIER,Suthrlnd,United,MulticellGA,MCPartitions," +
                "TriLakes,TriLakes2,Spectrum,Michcor,CapCity,colonialPL," +
                "Allwest,LoyLang,Badger,Delta,PQP,RFC2,Peachtree,BlueRidg," +
                "Oklahoma,Protagon,TriState,Soule,Adapt,Freedman,BELL,VINELAND,Axis,Lakeside,PFS," +
                "jobcardc 1,jobcardc 2,Printers,Valley,jobcardc 20" 
 str-init[33] = "ASI,HOP,Fibre,Century,Interpac,FibreFC,HPB,Dayton,Livngstn,CentBox,Metro,Keystone,Frankstn,Colonial,Unipak,OTTPkg,Shelby,CCC,Indiana-XL,PPI,Accord,Knight,MidYork,Dee,Badger,Rosmar,Carded,Carded2,PackRite,Prystup,Knight***,Coburn," + /*MWFibre=Keystone*/
                "jobcardf 1,jobcardf 2,xml,Wingate"
 str-init[34] = "Manual,Matrix"
 str-init[35] = "ShipTo,Header"
 str-init[36] = "ShipTo,FGFile"
 str-init[37] = "Triad,Brick"
 str-init[38] = "JobQty,Net Shts"
 str-init[39] = "ASI,Fibre"
 str-init[40] = /*Planner,Kiwi,*/ "None,PlanDate,NoDate"
 str-init[41] = "Manual,%$$$$$$####A&&,@$$$$$$$$####A>,$$$$$$$$#####A>,None,Hold" /* %$$$$$$####A&&:Hughes,@$$$$$$$$####A>:Fibre*/
 str-init[42] = "Item,Set" 
 str-init[43] = ",Bin>Qty,AutoSelectShipFrom"
 str-init[44] = "Fibre,ASI,Stock/Custom" 
 str-init[45] = "Due Date,LastShip,DueDate+1Day" 
 str-init[46] = "Expense,Vendor,ExpVend,Asset" 
 str-init[47] = "JobClose,FGPost" 
 str-init[48] = "EA,M" 
 str-init[49] = "Gross,Net" 
 str-init[50] = "ASI,Hughes"
 .
ASSIGN
 str-init[51] = "None,CorrTrim,Alliance,HRMS,CorSuply,Corr-U-KraftII,GP," +
                "Vendor,Kiwi,Smurfit,CorrChoice,Pratt,AlliFlutes,iPaper,KiwiT,Liberty"  
 str-init[52] = "Percent,$/Pallet,$/MSF"
 str-init[53] = "AllItems,POOnly,None" 
 str-init[54] = "Sheet,Blank" 
 str-init[55] = "BLACK,RED"  
 str-init[56] = "" /*removed option with ticket 17756*/
 str-init[57] = "Matrix,LastPric"
 str-init[58] = ""
 str-init[59] = "Complete,OnHand=0"
 str-init[60] = "BOL,Current"    
 str-init[61] = "Current,Release"
 str-init[62] = "NonStock,LotCntrl,Avail<0,Any"
 str-init[63] = "SouthPak,Xprint,APC,StClair"  /* quosheet */
 str-init[64] = "None,Underrun,Receipts"  /* fgemails */ 
 str-init[65] = "Standard,Actual"  /* tspost */ 
 str-init[66] = "AUTOPOST,TSPOSTFG,Manual,LoadTag,TSPARTS"  /* fgrecpt */ 
 str-init[67] = "Stock,Purchase"  /* pouom */
 str-init[68] = "AUTOPOST,FGITEM"  /* fgwhsbin */
 str-init[69] = ",Artios,Arden,Artios2" /* cadcam */  
 str-init[70] = "Std,Std & Paper1/2" /* efbrowse */  
 str-init[71] = ",ByItem,POPUP,RMTAG" /* rmrecpt */
 str-init[72] = ",Bronze/Maroon" /* fgbrowse */
 str-init[73] = ",User Entered" /* fgbrowse */
 str-init[74] = ",REALONLY,ALLITEMS" /* rmpostgl */
 str-init[75] = "ItemFle,Pallet" /* SSTRANSF character value was ",Pallet,Case" */
 str-init[76] = "ASI,MAS90,WestInd" /* payvend */
 str-init[77] = "OEINQ,Order Entry" /* ou6brows */
 str-init[78] = ",PerMSF,CM%+Freight" /* cewhatif */
 str-init[79] = ",XOnOrder" /* oereordr */
 str-init[80] = "EstShipto,OEShipto" /* oeship */
 str-init[81] = "Inv/Item,AllItems" /* fginvrec */
 str-init[82] = ",CIT,ContSrvc,Goodman"  /*Inexport*/  
 str-init[83] = "Both,Corr,Fold,Neither" /* cecopy */
 str-init[84] = "Consolidate,Segment,Text" /* ceprint */
 str-init[85] = "Penny,Dollar,FiveDollar" /* ceprep */  
 str-init[86] = ",16th's,32nd's,Decimal" /* cecscrn */
 str-init[87] = "RunOnly,MR+Run" /* ceprice */
 str-init[88] = ",AllOrders,SameOrderOnly,SamePo#Only,AllOrders&ShipFromWhse,SamePO#&ShipFromWhse,SameOrder&SameShipFrom,SameOrder&SameShipFrom&SamePO,AllOrders&NotRunShip"  /* relmerge */       
 str-init[89] = ",ASI,ASIXprnt,Premier,ASIExcel,Loylang,Printers,Protagon,Badger,Soule,RFC,SouleMed,stmtprint 1,stmtprint 2,StdStatement10"  /*stmtprin*/
 str-init[90] = "All Machines,Last Machine,NO"  /*TSFinish*/
 str-init[91] = "MSF,PO UOM"  /*appaper*/
 str-init[92] = "FluteMtx,AUTOCALC"  /*appaper*/
 str-init[93] = ",Before,After,Both"  /*fgpost*/
 str-init[94] = ",CSC,Southpak,TrePaper"  /* corsuply */
 str-init[95] = "Blank,GrossSH"  /* fgitemsf */
 str-init[96] = ",Michcor,Trilakes,Woodland,PremierPkg,St.Clair,NStock,Blue,Freedman"  /* GP */
 str-init[97] = "DC Only,OE & DC"   /* oeprep */
 str-init[98] = "None,1/8"  /* celayout */
 str-init[99] = "Fibre"
 str-init[100] = "Customer,CustX"  /* 1099-MISC */
 str-init[101] = ",Skip Open Order Calc"
 str-init[102] = ""
 str-init[103] = "Item,Vendor,All,NoTax"   /* aptax */ 
 str-init[104] = "None,Overrun,Receipts"  /* rmemails */     
 str-init[105] = ",loadtag"
 str-init[106] = "Fibrex,BOLFMTX15"
 str-init[107] = ",Indiana,Dee"
 str-init[108] = "Workstation,Server"  
 str-init[109] = "Reorder Point"   
 str-init[110] = ",HOP,Xprint,AllWest,PremierPkg,SouleMed,Soule,StdCreditMemo10"  
 str-init[111] = ",Positive Pay,Positive Pay-knight"
 str-init[112] = ",DuplicateFGDayClient" 
 str-init[113] = ",Trailer" 
 str-init[114] = ",Overrun/Underrun"
 str-init[115] = ""
 str-init[116] = "Today,Release Date"
 str-init[117] = ",Invoice Price,Order Price"
 str-init[118] = ",Partitions"
 str-init[119] = ""
 str-init[120] = "" 
 str-init[121] = ""
 str-init[122] = "ASIJobCL,MidYorkJobCL,CentBoxJobCL,ASIJobCR,ACPIJobCor"
 str-init[123] = "UnderRuns and OverRun,OverRuns Only,UnderRuns Only"
 str-init[124] = "Profit,Cost Markup"
 str-init[128] = "BolCreation,Fixed Time"
 .

ASSIGN str-init[125] = "Ship Only,Invoice Only,Bill and Ship,Transfer Only"
       str-init[126] = "FG Lot#,Whs/Bin"
       str-init[127] = "S,I,M,O,N"
       str-init[129] = "SHIPTO,FreightClass"
       str-init[130] = "Shipto Delivery Zone,FGFreightClass"
       str-init[131] = "Partitions,Premier"
       str-init[132] = "JobRequired,ScanTagOnly"
       str-init[133] = ",Fibre,Welsh"
       str-init[134] = ",Premier"
       str-init[135] = ",Qty-Item,Item-Qty"
       str-init[136] = ",TransferCost"
       str-init[137] = ",Run&Ship Prompt,DefaultOnly"
       str-init[138] = ",Batch,One BOL Only"
       str-init[139] = ",Exclude Finance Charges"
       str-init[140] = ",NoAdjustments"
       str-init[141] = "3CPack,AckMaster 1,AckMaster 2"
       str-init[142] = "ASI,Badger"
       str-init[143] = ",Release Date,Promise Date,Order Line Due Date,Release Due Date"
       str-init[144] = ",Added,Deleted,OverUnder"
       str-init[145] = "Order Item Counts,Pallet Counts"
       str-init[146] = ",TEAL"
       str-init[147] = ",PremierPkg,McElroy,Bell,Capitol,Rudd,Trepaper,StClair"
       str-init[148] = ",NoWarning,OverShipWarning,UnderShipWarning,OverUnderShipWarning"
       str-init[149] = ",Job-Item,Item-Job"
       str-init[150] = ",RMLot"
       str-init[151] = "Xprint,bolfmt1,GPI"
       str-init[152] = ",Open,Hold"
       str-init[153] = "Trailer#,ShipTo,Indiana,StdBOLMaster"
       str-init[154] = "Square Feet,Board Cost,Factory Cost,Full Cost"
       str-init[155] = "Yes,No,Ask"
       str-init[156] = "Yes,No,Ask"
       str-init[157] = ",Block Entry"
       str-init[158] = "Order Quantity,Release Quantity"
       str-init[159] = "Transit Days,Dock Appt Days,Transit&Dock"
       .
	
IF PROGRAM-NAME(1) MATCHES "*windows/l-syschr.w*" THEN DO:
     ASSIGN
	   str-init[1]  = "quoprint 1,quoprint 2,quoprint 10,quoprint 11,quoprint 20,quoprint10-CAN"
       str-init[2]  = "invprint 1,invprint 2,invprint 10,invprint 20,invprint10-CAN"
       str-init[8]  = "poprint 1,poprint 2,poprint 10,poprint 20,POPrint10-CAN"
       str-init[9]  = "relprint 1,relprint 2,relprint 10"
       str-init[10] = "bolfmt 1,bolfmt 2,bolfmt 10,bolfmt 20,bolfmt10-CAN,BOLfmt15"
       str-init[11] = "chkfmt 1,chkfmt 2,APChkFmt1"
       str-init[13] = "ackhead 1,ackhead 2,ackhead 10,ackhead 20,ackhead10-CAN"
       str-init[29] = "bolcert 1,bolcert 2,BOLCERT10"
       str-init[30] = "cerunc 1,cerunc 2,cerunc 3"
       str-init[31] = "cerunf 1,cerunf 2"
       str-init[32] = "jobcardc 1,jobcardc 2,jobcardc 20"
       str-init[33] = "jobcardf 1,jobcardf 2"
       str-init[89] = "stmtprint 1,stmtprint 2,StdStatement10"  /*stmtprin*/
       str-init[141] = "AckMaster 1,AckMaster 2" /*"3CPack"*/
       str-init[151] = "bolfmt1"
       .
END.

FOR EACH ASI.item-spec FIELDS(CODE) WHERE
    ASI.item-spec.company = g_company AND
    ASI.item-spec.i-no = '' NO-LOCK  
    BREAK BY item-spec.CODE:

    IF FIRST-OF(item-spec.CODE) THEN
       str-init[58] = str-init[58] + 
                      IF str-init[58] <> "" THEN "," + item-spec.CODE 
                      ELSE item-spec.CODE.
END.


FOR EACH ASI.company  NO-LOCK :

    IF AVAIL ASI.company THEN DO:   
      ASSIGN
       str-init[119] = str-init[119] + 
                      IF str-init[119] <> "" THEN "," + company.company 
                      ELSE company.company
       str-init[120] = str-init[120] + 
                      IF str-init[120] <> "" THEN "," + company.company 
                      ELSE company.company
       str-init[121] = str-init[121] + 
                      IF str-init[121] <> "" THEN "," + company.company 
                      ELSE company.company.
    END.
END.
