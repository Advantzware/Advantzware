using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Web.Caching;
using ASINET;
using ASIDataNS;
using Progress.Open4GL.Proxy;

/// <summary>
/// Summary description for browsepo
/// </summary>
[System.ComponentModel.DataObject]
public class orderentry : AppServerConnect.AppServer
{
    public orderentry()
    {
        //
        // TODO: Add constructor logic here
        //
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsEntryIOrdDataSet SeOrderEntry(string prmCust, string prmUser, string prmAction, string prmPonum, string prmPartno, string prmPostatus, Int32 prmOrderNum, string prmFgitem, string prmEst, string prmJob, string prmJob2, Int32 prmQuote)
    {

        dsEntryIOrdDataSet dsEntryIOrd = new dsEntryIOrdDataSet();
        dsEntryIOrd = null;
        AppServerConnect();
        aoObject.EntryOrd(prmCust, prmUser, prmAction, prmPonum, prmPartno, prmPostatus, prmOrderNum, prmFgitem, prmEst, prmJob, prmJob2,prmQuote, ref dsEntryIOrd);
        AppServerDisconnect();

        return dsEntryIOrd;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsUpdateOrdEntryDataSet SelectOrderEntry(string prmUser, string prmAction, Int32 prmOrderNum, string prmCustomer, string prmUserid, string prmStat, string prmSold, DateTime prmOrdate, string prmSoldName, string prmDueCode, DateTime prmDueDate, string prmCustAddr, string prmSoldAddr, DateTime prmLastDate, string prmcustAddr2, string prmSoldAddr2, DateTime prmProdDate, string prmCity, string prmState, string prmZip, string prmSoldCity, string prmSoldState, string prmSoldZip, string prmPonum, string prmContact, decimal prmOverpct, decimal prmUnderpct, string prmTerms, string prmTermdscr, Int32 prmProd, string prmTaxgr, string prmFreight, string prmCarrier, string prmFob, string prmSman, string prmSname, string prmSman2, string prmSname2, string prmSman3, string prmSname3, string prmCtype, DateTime prmcExp, string prmCnum, string prmCauth, string prmCustName, string prmType, Int32 prmLine, string prmWhis, Int64 VRowid)
    {
        string cError = "";
        dsUpdateOrdEntryDataSet dsUpdateOrdEntry = new dsUpdateOrdEntryDataSet();
        dsUpdateOrdEntry = null;
        AppServerConnect();
        aoObject.OrdEntryUpdate(prmUser, prmAction, prmOrderNum, prmCustomer, prmUserid, prmStat, prmSold, prmOrdate, prmSoldName, prmDueCode, prmDueDate, prmCustAddr, prmSoldAddr, prmLastDate, prmcustAddr2, prmSoldAddr2, prmProdDate, prmCity, prmState, prmZip, prmSoldCity, prmSoldState, prmSoldZip, prmPonum, prmContact, prmOverpct, prmUnderpct, prmTerms, prmTermdscr, prmProd, prmTaxgr, prmFreight, prmCarrier, prmFob, prmSman, prmSname, prmSman2, prmSname2, prmSman3, prmSname3, prmCtype, prmcExp, prmCnum, prmCauth, prmCustName, prmType, prmLine, prmWhis, VRowid, ref dsUpdateOrdEntry, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsUpdateOrdEntry;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsUpdateItemDataSet SelectItemEntry(string prmUser, string prmAction, Int32 prmOrderNum, string prmItemNum, string prmItemName, Int32 prmLine)
    {

        dsUpdateItemDataSet dsUpdateItem = new dsUpdateItemDataSet();
        dsUpdateItem = null;
        AppServerConnect();
        aoObject.ItemUpdate(prmUser, prmAction, prmOrderNum, prmItemNum, prmItemName, prmLine, ref dsUpdateItem);
        AppServerDisconnect();

        return dsUpdateItem;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsViewItemEntryDataSet SelectViewEntry(string prmUser, string prmAction, Int32 prmOrderNum, Int32 prmLine, string prmEstimate, string prmItemNum, string prmPartNum, decimal prmQty, string prmItemName, string prmPartdscr, string prmPartdscr1, string prmPartdscr2, decimal prmPrice, string prmUom, string prmTax, string prmPoNum, string prmJob, decimal prmDiscount, string prmCode, DateTime prmReqDate, decimal prmTPrice, string prmPromCode, DateTime prmPromDate, decimal prmShip, Int32 prmCas, decimal prmPartial, Int32 prmUnit, Int32 prmEnum, Int32 prmPrevOrder, string prmSman, string prmSman2, string prmSman3, string prmType, decimal prmOver, decimal prmUnder, string prmVend, string prmManag, string prmLn, string prmSname, string prmSname2, string prmSname3)
    {
        string cError = "";
        dsViewItemEntryDataSet dsViewItemEntry = new dsViewItemEntryDataSet();
        dsViewItemEntry = null;
        AppServerConnect();
        aoObject.ViewItemEntry(prmUser, prmAction, prmOrderNum, prmLine, prmEstimate, prmItemNum, prmPartNum, prmQty, prmItemName, prmPartdscr, prmPartdscr1, prmPartdscr2, prmPrice, prmUom, prmTax, prmPoNum, prmJob, prmDiscount, prmCode, prmReqDate, prmTPrice, prmPromCode, prmPromDate, prmShip, prmCas, prmPartial, prmUnit, prmEnum, prmPrevOrder, prmSman, prmSman2, prmSman3, prmType, prmOver, prmUnder, prmVend, prmManag, prmLn, prmSname, prmSname2, prmSname3, ref dsViewItemEntry, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsViewItemEntry;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateEstEntry(string prmUser, string prmAction, string prmExt, Int32 prmOrderNum, string prmCustomer, string prmUserid, string prmStat, string prmSold, DateTime prmOrdate, string prmSoldName, string prmDueCode, DateTime prmDueDate, string prmCustAddr, string prmSoldAddr, DateTime prmLastDate, string prmcustAddr2, string prmSoldAddr2, DateTime prmProdDate, string prmCity, string prmState, string prmZip, string prmSoldCity, string prmSoldState, string prmSoldZip, string prmPonum, string prmContact, decimal prmOverpct, decimal prmUnderpct, string prmTerms, string prmTermdscr, Int32 prmProd, string prmTaxgr, string prmFreight, string prmCarrier, string prmFob, string prmSman, string prmSname, string prmSman2, string prmSname2, string prmSman3, string prmSname3, string prmCtype, DateTime prmcExp, string prmCnum, string prmCauth, string prmCustName, string prmType, Int32 prmLine, string prmWhis, Int64 VRowid, string prmJob, Int32 prmJob2, string prmEst, decimal prmSales1, decimal prmSales2, decimal prmSales3, decimal prmComm1, decimal prmComm2, decimal prmComm3, Int32 prmQuote)
    {
        string cError = "";
        
        AppServerConnect();
        aoObject.AddOrderValidate(prmUser, prmAction, prmExt, prmOrderNum, prmCustomer, prmUserid, prmStat, prmSold, prmOrdate, prmSoldName, prmDueCode, prmDueDate, prmCustAddr, prmSoldAddr, prmLastDate, prmcustAddr2, prmSoldAddr2, prmProdDate, prmCity, prmState, prmZip, prmSoldCity, prmSoldState, prmSoldZip, prmPonum, prmContact, prmOverpct, prmUnderpct, prmTerms, prmTermdscr, prmProd, prmTaxgr, prmFreight, prmCarrier, prmFob, prmSman, prmSname, prmSman2, prmSname2, prmSman3, prmSname3, prmCtype, prmcExp, prmCnum, prmCauth, prmCustName, prmType, prmLine, prmWhis, VRowid, prmJob, prmJob2, prmEst, prmSales1, prmSales2, prmSales3, prmComm1, prmComm2, prmComm3, prmQuote, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            return false;
        }
        else
        {
            return true;
        }
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsOrderEstUpdateDataSet SelectEstEntry(string prmUser, string prmAction, string prmExt, Int32 prmOrderNum, string prmCustomer, string prmUserid, string prmStat, string prmSold, DateTime prmOrdate, string prmSoldName, string prmDueCode, DateTime prmDueDate, string prmCustAddr, string prmSoldAddr, DateTime prmLastDate, string prmcustAddr2, string prmSoldAddr2, string prmProdDate, string prmCity, string prmState, string prmZip, string prmSoldCity, string prmSoldState, string prmSoldZip, string prmPonum, string prmContact, decimal prmOverpct, decimal prmUnderpct, string prmTerms, string prmTermdscr, Int32 prmProd, string prmTaxgr, string prmFreight, string prmCarrier, string prmFob, string prmSman, string prmSname, string prmSman2, string prmSname2, string prmSman3, string prmSname3, string prmCtype, string prmcExp, string prmCnum, string prmCauth, string prmCustName, string prmType, Int32 prmLine, string prmWhis, Int64 VRowid, string prmJob, Int32 prmJob2, string prmEst, decimal prmSales1, decimal prmSales2, decimal prmSales3, decimal prmComm1, decimal prmComm2, decimal prmComm3, Int32 prmQuote, Int32 prmQty, decimal prmPrice,string prmUom)
    {
        string cError = "";
        dsOrderEstUpdateDataSet dsOrderEstUpdate = new dsOrderEstUpdateDataSet();
        dsOrderEstUpdate = null;
        AppServerConnect();
        aoObject.OrderEstUpdate(prmUser, prmAction, prmExt, prmOrderNum, prmCustomer, prmUserid, prmStat, prmSold, prmOrdate, prmSoldName, prmDueCode, prmDueDate, prmCustAddr, prmSoldAddr, prmLastDate, prmcustAddr2, prmSoldAddr2, prmProdDate, prmCity, prmState, prmZip, prmSoldCity, prmSoldState, prmSoldZip, prmPonum, prmContact, prmOverpct, prmUnderpct, prmTerms, prmTermdscr, prmProd, prmTaxgr, prmFreight, prmCarrier, prmFob, prmSman, prmSname, prmSman2, prmSname2, prmSman3, prmSname3, prmCtype, prmcExp, prmCnum, prmCauth, prmCustName, prmType, prmLine, prmWhis, VRowid, prmJob, prmJob2, prmEst, prmSales1, prmSales2, prmSales3, prmComm1, prmComm2, prmComm3, prmQuote, prmQty,prmPrice,prmUom, ref dsOrderEstUpdate, out cError);
        AppServerDisconnect();
        if (cError != "")
        {            
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
            
        }
        
        return dsOrderEstUpdate;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsViewItemEstimateDataSet SelectViewItemEstimate(string prmUser, string prmAction, Int32 prmOrderNum, Int32 prmLine, string prmEstimate, string prmItemNum, string prmPartNum, decimal prmQty, string prmItemName, string prmPartdscr, string prmPartdscr1, string prmPartdscr2, decimal prmPrice, string prmUom, string prmTax, string prmPoNum, string prmJob, Int32 prmJob2, decimal prmDiscount, string prmCode, DateTime prmReqDate, decimal prmTPrice, string prmPromCode, DateTime prmPromDate, decimal prmShip, Int32 prmCas, decimal prmPartial, Int32 prmUnit, Int32 prmEnum, Int32 prmPrevOrder, string prmSman, string prmSman2, string prmSman3, string prmType, decimal prmOver, decimal prmUnder, string prmVend, string prmManag, string prmLn, string prmSname, string prmSname2, string prmSname3, decimal prmSpct, decimal prmSpct2, decimal prmSpct3, decimal prmComm, decimal prmComm2, decimal prmComm3, decimal prmCost, Int32 prmQno, string prmNewItemCreated)
    {
        string cError = "";
        dsViewItemEstimateDataSet dsViewItemEstimate = new dsViewItemEstimateDataSet();
        dsViewItemEstimate = null;
        AppServerConnect();
        aoObject.ViewItemEstimate(prmUser, prmAction, prmOrderNum, prmLine, prmEstimate, prmItemNum, prmPartNum, prmQty, prmItemName, prmPartdscr, prmPartdscr1, prmPartdscr2, prmPrice, prmUom, prmTax, prmPoNum, prmJob,prmJob2, prmDiscount, prmCode, prmReqDate, prmTPrice, prmPromCode, prmPromDate, prmShip, prmCas, prmPartial, prmUnit, prmEnum, prmPrevOrder, prmSman, prmSman2, prmSman3, prmType, prmOver, prmUnder, prmVend, prmManag, prmLn, prmSname, prmSname2, prmSname3, prmSpct, prmSpct2, prmSpct3, prmComm, prmComm2, prmComm3, prmCost,prmQno, prmNewItemCreated, ref dsViewItemEstimate, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsViewItemEstimate;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool AddViewItemEstimate(string prmUser, string prmAction, Int32 prmOrderNum, Int32 prmLine, string prmEstimate, string prmItemNum, string prmPartNum, decimal prmQty, string prmItemName, string prmPartdscr, string prmPartdscr1, string prmPartdscr2, decimal prmPrice, string prmUom, string prmTax, string prmPoNum, string prmJob, Int32 prmJob2, decimal prmDiscount, string prmCode, DateTime prmReqDate, decimal prmTPrice, string prmPromCode, DateTime prmPromDate, decimal prmShip, Int32 prmCas, decimal prmPartial, Int32 prmUnit, Int32 prmEnum, Int32 prmPrevOrder, string prmSman, string prmSman2, string prmSman3, string prmType, decimal prmOver, decimal prmUnder, string prmVend, string prmManag, string prmLn, string prmSname, string prmSname2, string prmSname3, decimal prmSpct, decimal prmSpct2, decimal prmSpct3, decimal prmComm, decimal prmComm2, decimal prmComm3, decimal prmCost, Int32 prmQno, string prmNewItemCreated)
    {
        string cError = "";
        dsViewItemEstimateDataSet dsViewItemEstimate = new dsViewItemEstimateDataSet();
        dsViewItemEstimate = null;
        AppServerConnect();
        aoObject.ViewItemEstimate(prmUser, prmAction, prmOrderNum, prmLine, prmEstimate, prmItemNum, prmPartNum, prmQty, prmItemName, prmPartdscr, prmPartdscr1, prmPartdscr2, prmPrice, prmUom, prmTax, prmPoNum, prmJob, prmJob2, prmDiscount, prmCode, prmReqDate, prmTPrice, prmPromCode, prmPromDate, prmShip, prmCas, prmPartial, prmUnit, prmEnum, prmPrevOrder, prmSman, prmSman2, prmSman3, prmType, prmOver, prmUnder, prmVend, prmManag, prmLn, prmSname, prmSname2, prmSname3, prmSpct, prmSpct2, prmSpct3, prmComm, prmComm2, prmComm3, prmCost, prmQno, prmNewItemCreated, ref dsViewItemEstimate, out cError);
        AppServerDisconnect();
       
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            return false;
        }
        else
        {
            return true;
        }
        
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGetCostDataSet SelectGetCost(string prmUser, string prmAction, decimal prmQty, string prmEstimate, string prmPartNum)
    {
        string cError = "";
        dsGetCostDataSet dsGetCost = new dsGetCostDataSet();
        dsGetCost = null;
        AppServerConnect();
        aoObject.GetCost(prmUser, prmAction, prmQty, prmEstimate, prmPartNum, ref dsGetCost, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsGetCost;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsOrderQuoteupDataSet SeOrderQuoteup(string prmAction, string prmUser, string prmComp, Int32 prmQuote)
    {        
        dsOrderQuoteupDataSet dsOrderQuoteup = new dsOrderQuoteupDataSet();
        dsOrderQuoteup = null;
        AppServerConnect();
        aoObject.OrderQuoteup(prmAction, prmUser, prmComp, prmQuote, ref dsOrderQuoteup);
        AppServerDisconnect();
        
        return dsOrderQuoteup;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select,true)]
    public dsTopListNotesDataSet ListTopOrderNotes(string prmAction, string prmUser, string prmRecKey, string prmHeader, string prmDate, string prmDept)
    {
        string cError = "";
        dsTopListNotesDataSet dsTopListNotes=new dsTopListNotesDataSet();
        dsTopListNotes=null;
        AppServerConnect();
        aoObject.TopListNotes(prmAction, prmUser, prmRecKey, prmHeader, prmDate, prmDept, ref dsTopListNotes, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsTopListNotes;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsTopViewNotesDataSet ViewTopOrderNotes(string prmAction, string prmUser, string prmRecKey, string prmHeader, DateTime prmNoteDate, string prmNoteTime, string prmUserId, string prmViewed, string prmDeptCode, string prmDeptName, Int32 prmForm, string prmNoteTitle, string prmNewNoteTitle, string prmNoteText, string prmEstimate)
    {
        string cError = "";
        dsTopViewNotesDataSet dsTopViewNotes = new dsTopViewNotesDataSet();
        dsTopViewNotes = null;
        AppServerConnect();
        aoObject.TopViewNotes(prmAction, prmUser, prmRecKey, prmHeader, prmNoteDate, prmNoteTime, prmUserId, prmViewed, prmDeptCode, prmDeptName, prmForm, prmNoteTitle, prmNewNoteTitle, prmNoteText, prmEstimate, ref dsTopViewNotes, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsTopViewNotes;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsTopEstFGItemDataSet ListTopSpecNotes(string prmAction, string prmUser, string prmEst,string prmItem)
    {
        string cError = "";
        dsTopEstFGItemDataSet dsTopEstFGItem = new dsTopEstFGItemDataSet();
        dsTopEstFGItem = null;
        AppServerConnect();
        aoObject.TopEstItem(prmAction, prmUser, prmEst,prmItem, ref dsTopEstFGItem, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>window.close()</script>");
        }
        return dsTopEstFGItem;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsTopListNotes2DataSet SelectSpecNotes(string prmAction, string prmUser, string prmRecKey, string prmHeader, string prmNoteDate, string prmNoteTime, string prmUserId, string prmViewed, string prmDeptCode, string prmDeptName, Int32 prmForm, string prmNoteTitle, string prmNewNoteTitle, string prmNoteText, string prmEstimate)
    {
        string cError = "";
        dsTopListNotes2DataSet dsTopListNotes = new dsTopListNotes2DataSet();
        dsTopListNotes = null;
        AppServerConnect();
        aoObject.TopSpecNotes(prmAction, prmUser, prmRecKey, prmHeader, prmNoteDate, prmNoteTime, prmUserId, prmViewed, prmDeptCode, prmDeptName, prmForm, prmNoteTitle, prmNewNoteTitle, prmNoteText, prmEstimate, ref dsTopListNotes, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsTopListNotes;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsTopAttachmentDataSet SelectAttach(string prmAction, string prmUser, string prmRecKey, string prmAttFile, string prmEst, string prmFgitem, string prmDate, string prmOpenWith, string prmSerchEst)
    {
        string cError = "";
        dsTopAttachmentDataSet dsTopAttachment = new dsTopAttachmentDataSet();
        dsTopAttachment = null;
        AppServerConnect();
        aoObject.TopAttached(prmAction, prmUser, prmRecKey, prmAttFile, prmEst, prmFgitem, prmDate, prmOpenWith, prmSerchEst, ref dsTopAttachment, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsTopAttachment;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsItemHistDetailDataSet SelectHistoryDetail(string prmAction, string prmUser, string prmItem, string prmCust)
    {
        string cError = "";
        dsItemHistDetailDataSet dsItemHistDetail = new dsItemHistDetailDataSet();
        dsItemHistDetail = null;
        AppServerConnect();
        aoObject.ItemHistoryDetail(prmAction, prmUser, prmItem, prmCust, ref dsItemHistDetail);
        AppServerDisconnect();

        return dsItemHistDetail;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsItemHistDisplayDataSet SelectHistory(string prmAction, string prmUser, string prmRowId, string prmText)
    {
        string cError = "";
        dsItemHistDisplayDataSet dsItemHistDisplay = new dsItemHistDisplayDataSet();
        dsItemHistDisplay = null;
        AppServerConnect();
        aoObject.ItemHistory(prmAction, prmUser, prmRowId, prmText, ref dsItemHistDisplay);
        AppServerDisconnect();

        return dsItemHistDisplay;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsItemPriceLevelDataSet SelectItemPriceLevel(string prmAction, string prmUser, Int32 prmOrder, Int32 prmLevel, string prmComp, Int32 prmLine)
    {
        string cError = "";
        dsItemPriceLevelDataSet dsItemPriceLevel = new dsItemPriceLevelDataSet();
        dsItemPriceLevel = null;
        AppServerConnect();
        aoObject.ItemPriceLevel(prmAction, prmUser, prmOrder, prmLevel, prmComp, prmLine, out cError, ref dsItemPriceLevel);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsItemPriceLevel;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool AddViewItemEstimateApp(string prmUser, string prmAction, Int32 prmOrderNum, Int32 prmLine, string prmEstimate, string prmItemNum, string prmPartNum, decimal prmQty, string prmItemName, string prmPartdscr, string prmPartdscr1, string prmPartdscr2, decimal prmPrice, string prmUom, string prmTax, string prmPoNum, string prmJob, Int32 prmJob2, decimal prmDiscount, string prmCode, DateTime prmReqDate, decimal prmTPrice, string prmPromCode, DateTime prmPromDate, decimal prmShip, Int32 prmCas, decimal prmPartial, Int32 prmUnit, Int32 prmEnum, Int32 prmPrevOrder, string prmSman, string prmSman2, string prmSman3, string prmType, decimal prmOver, decimal prmUnder, string prmVend, string prmManag, string prmLn, string prmSname, string prmSname2, string prmSname3, decimal prmSpct, decimal prmSpct2, decimal prmSpct3, decimal prmComm, decimal prmComm2, decimal prmComm3, decimal prmCost, Int32 prmQno, string prmNewItemCreated, ref string vError)
    {
        string cError = "";
        dsViewItemEstimateDataSet dsViewItemEstimate = new dsViewItemEstimateDataSet();
        dsViewItemEstimate = null;
        AppServerConnect();
        aoObject.ViewItemEstimate(prmUser, prmAction, prmOrderNum, prmLine, prmEstimate, prmItemNum, prmPartNum, prmQty, prmItemName, prmPartdscr, prmPartdscr1, prmPartdscr2, prmPrice, prmUom, prmTax, prmPoNum, prmJob, prmJob2, prmDiscount, prmCode, prmReqDate, prmTPrice, prmPromCode, prmPromDate, prmShip, prmCas, prmPartial, prmUnit, prmEnum, prmPrevOrder, prmSman, prmSman2, prmSman3, prmType, prmOver, prmUnder, prmVend, prmManag, prmLn, prmSname, prmSname2, prmSname3, prmSpct, prmSpct2, prmSpct3, prmComm, prmComm2, prmComm3, prmCost, prmQno, prmNewItemCreated, ref dsViewItemEstimate, out cError);
        AppServerDisconnect();

        if (cError != "")
        {
            vError = cError;
            //HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            return false;
        }
        else
        {
            return true;
        }

    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateEstEntryApp(string prmUser, string prmAction, string prmExt, Int32 prmOrderNum, string prmCustomer, string prmUserid, string prmStat, string prmSold, DateTime prmOrdate, string prmSoldName, string prmDueCode, DateTime prmDueDate, string prmCustAddr, string prmSoldAddr, DateTime prmLastDate, string prmcustAddr2, string prmSoldAddr2, DateTime prmProdDate, string prmCity, string prmState, string prmZip, string prmSoldCity, string prmSoldState, string prmSoldZip, string prmPonum, string prmContact, decimal prmOverpct, decimal prmUnderpct, string prmTerms, string prmTermdscr, Int32 prmProd, string prmTaxgr, string prmFreight, string prmCarrier, string prmFob, string prmSman, string prmSname, string prmSman2, string prmSname2, string prmSman3, string prmSname3, string prmCtype, DateTime prmcExp, string prmCnum, string prmCauth, string prmCustName, string prmType, Int32 prmLine, string prmWhis, Int64 VRowid, string prmJob, Int32 prmJob2, string prmEst, decimal prmSales1, decimal prmSales2, decimal prmSales3, decimal prmComm1, decimal prmComm2, decimal prmComm3, Int32 prmQuote, ref string vError)
    {
        string cError = "";

        AppServerConnect();
        aoObject.AddOrderValidate(prmUser, prmAction, prmExt, prmOrderNum, prmCustomer, prmUserid, prmStat, prmSold, prmOrdate, prmSoldName, prmDueCode, prmDueDate, prmCustAddr, prmSoldAddr, prmLastDate, prmcustAddr2, prmSoldAddr2, prmProdDate, prmCity, prmState, prmZip, prmSoldCity, prmSoldState, prmSoldZip, prmPonum, prmContact, prmOverpct, prmUnderpct, prmTerms, prmTermdscr, prmProd, prmTaxgr, prmFreight, prmCarrier, prmFob, prmSman, prmSname, prmSman2, prmSname2, prmSman3, prmSname3, prmCtype, prmcExp, prmCnum, prmCauth, prmCustName, prmType, prmLine, prmWhis, VRowid, prmJob, prmJob2, prmEst, prmSales1, prmSales2, prmSales3, prmComm1, prmComm2, prmComm3, prmQuote, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            vError = cError;
            //HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            return false;
        }
        else
        {
            return true;
        }
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsViewItemEstimateAppDataSet SelectViewItemEstimateApp(string prmUser, string prmAction, Int32 prmOrderNum, Int32 prmLine, string prmEstimate, string prmItemNum, string prmPartNum, decimal prmQty, string prmItemName, string prmPartdscr, string prmPartdscr1, string prmPartdscr2, decimal prmPrice, string prmUom, string prmTax, string prmPoNum, string prmJob, Int32 prmJob2, decimal prmDiscount, string prmCode, DateTime prmReqDate, decimal prmTPrice, string prmPromCode, DateTime prmPromDate, decimal prmShip, Int32 prmCas, decimal prmPartial, Int32 prmUnit, Int32 prmEnum, Int32 prmPrevOrder, string prmSman, string prmSman2, string prmSman3, string prmType, decimal prmOver, decimal prmUnder, string prmVend, string prmManag, string prmLn, string prmSname, string prmSname2, string prmSname3, decimal prmSpct, decimal prmSpct2, decimal prmSpct3, decimal prmComm, decimal prmComm2, decimal prmComm3, decimal prmCost, Int32 prmQno, string prmNewItemCreated, string prmMultiItem, string prmApp)
    {
        string cError = "";
        dsViewItemEstimateAppDataSet dsViewItemEstimateApp = new dsViewItemEstimateAppDataSet();
        dsViewItemEstimateApp = null;
        AppServerConnect();
        aoObject.ViewItemEstimateApp(prmUser, prmAction, prmOrderNum, prmLine, prmEstimate, prmItemNum, prmPartNum, prmQty, prmItemName, prmPartdscr, prmPartdscr1, prmPartdscr2, prmPrice, prmUom, prmTax, prmPoNum, prmJob, prmJob2, prmDiscount, prmCode, prmReqDate, prmTPrice, prmPromCode, prmPromDate, prmShip, prmCas, prmPartial, prmUnit, prmEnum, prmPrevOrder, prmSman, prmSman2, prmSman3, prmType, prmOver, prmUnder, prmVend, prmManag, prmLn, prmSname, prmSname2, prmSname3, prmSpct, prmSpct2, prmSpct3, prmComm, prmComm2, prmComm3, prmCost, prmQno, prmNewItemCreated, prmMultiItem, prmApp, ref dsViewItemEstimateApp, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsViewItemEstimateApp;
    }
}
