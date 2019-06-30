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
/// Summary description for ItemDetail
/// </summary>
[System.ComponentModel.DataObject]
public class QuoteDetail : AppServerConnect.AppServer
{
    public QuoteDetail()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsQuoteDetailDataSet SelectQuoteDetail(string prmUser, string prmItem)
    {

        dsQuoteDetailDataSet dsQuoteDetail = new dsQuoteDetailDataSet();
        dsQuoteDetail = null;
        AppServerConnect();
        aoObject.QuoteDetail(prmUser, prmItem, ref dsQuoteDetail);
        AppServerDisconnect();

        return dsQuoteDetail;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsDispQuoteItemsDataSet SelectQuoteItems(string prmUser, string prmAction, Int32 prmQuote, string prmPartNo, string prmItemNo, string prmPartDscr1, string prmPartDscr2, Int32 prmQty, decimal prmPrice, string prmUom, string prmDimensions, string prmBoard, string prmColor, Int32 prmLine,string prmStyle)
    {
        string cError = "";
        dsDispQuoteItemsDataSet dsDispQuoteItems = new dsDispQuoteItemsDataSet();
        dsDispQuoteItems = null;
        AppServerConnect();
        aoObject.QuoteItems(prmUser, prmAction, prmQuote, prmPartNo, prmItemNo, prmPartDscr1, prmPartDscr2, prmQty, prmPrice, prmUom, prmDimensions, prmBoard, prmColor,prmLine,prmStyle, ref dsDispQuoteItems, out cError);        
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsDispQuoteItems;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool SelectQuoteItemsValidate(string prmUser, string prmAction, Int32 prmQuote, string prmPartNo, string prmItemNo, string prmPartDscr1, string prmPartDscr2, Int32 prmQty, Decimal prmPrice, string prmUom, string prmDimensions, string prmBoard, string prmColor, Int32 prmLine, string prmStyle)
    {
        string cError = "";
        dsDispQuoteItemsDataSet dsDispQuoteItems = new dsDispQuoteItemsDataSet();
        dsDispQuoteItems = null;
        AppServerConnect();
        aoObject.QuoteItems(prmUser, prmAction, prmQuote, prmPartNo, prmItemNo, prmPartDscr1, prmPartDscr2, prmQty, prmPrice, prmUom, prmDimensions, prmBoard, prmColor, prmLine, prmStyle, ref dsDispQuoteItems, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
            return false;
        }
        else
        {
            return true;
        }
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsQuoteQtQtyItemsDataSet SelectQuoteQty(string prmUser, string prmAction, Int32 prmQuote, Int32 prmQty, decimal prmPrice, string prmUom, decimal prmProfit, Int32 prmRels, decimal prmMatCost, decimal prmLabCost, decimal prmFoCost, decimal prmVoCost, decimal prmTotLab, string prmDate, Int32 prmLine, string prmReckey, string prmRePrice)
    {
        string cError = "";
        dsQuoteQtQtyItemsDataSet dsQuoteQtQtyItems = new dsQuoteQtQtyItemsDataSet();
        dsQuoteQtQtyItems = null;
        AppServerConnect();
        aoObject.quote_qtqty(prmUser,  prmAction,  prmQuote, prmQty,  prmPrice,  prmUom,  prmProfit,  prmRels,  prmMatCost,  prmLabCost,  prmFoCost,  prmVoCost,  prmTotLab,  prmDate,  prmLine,  prmReckey,prmRePrice, ref dsQuoteQtQtyItems, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsQuoteQtQtyItems;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool SelectQuoteQtyValidate(string prmUser, string prmAction, Int32 prmQuote, Int32 prmQty, decimal prmPrice, string prmUom, decimal prmProfit, Int32 prmRels, decimal prmMatCost, decimal prmLabCost, decimal prmFoCost, decimal prmVoCost, decimal prmTotLab, string prmDate, Int32 prmLine, string prmReckey,string prmRePrice)
    {
        string cError = "";
        dsQuoteQtQtyItemsDataSet dsQuoteQtQtyItems = new dsQuoteQtQtyItemsDataSet();
        dsQuoteQtQtyItems = null;
        AppServerConnect();
        aoObject.quote_qtqty(prmUser, prmAction, prmQuote, prmQty, prmPrice, prmUom, prmProfit, prmRels, prmMatCost, prmLabCost, prmFoCost, prmVoCost, prmTotLab, prmDate, prmLine, prmReckey,prmRePrice, ref dsQuoteQtQtyItems, out cError);
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
    public dsQuoteQtChgItemsDataSet SelectQuoteCharge(string prmUser, string prmAction, Int32 prmQuote, Int32 prmQty, Int32 prmLine, Int32 prmSnum, Int32 prmBnum, string prmBill, string prmCharge, decimal prmPrepQty, decimal prmCost, decimal prmMkup, decimal prmAmtz, decimal prmAmt, decimal prmMatF, decimal prmMatM, decimal prmLabF, decimal prmLabM, string prmSimon, string prmReckey)
    {
        string cError = "";
        dsQuoteQtChgItemsDataSet dsQuoteQtChgItems = new dsQuoteQtChgItemsDataSet();
        dsQuoteQtChgItems = null;
        AppServerConnect();
        aoObject.quote_qtchg(prmUser, prmAction, prmQuote, prmQty, prmLine, prmSnum, prmBnum, prmBill, prmCharge, prmPrepQty, prmCost, prmMkup, prmAmtz, prmAmt, prmMatF, prmMatM, prmLabF, prmLabM, prmSimon, prmReckey, ref dsQuoteQtChgItems, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsQuoteQtChgItems;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsQuoteQtNotesItemsDataSet SelectQuoteNotes(string prmUser, string prmAction, Int32 prmQuote, string prmNote1, string prmNote2, string prmNote3, string prmNote4, string prmNote5)
    {
        string cError = "";
        dsQuoteQtNotesItemsDataSet dsQuoteQtNotesItems = new dsQuoteQtNotesItemsDataSet();
        dsQuoteQtNotesItems = null;
        AppServerConnect();
        aoObject.quote_notes(prmUser, prmAction, prmQuote, prmNote1,  prmNote2,  prmNote3,  prmNote4,  prmNote5, ref dsQuoteQtNotesItems, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsQuoteQtNotesItems;
    }
    
}


