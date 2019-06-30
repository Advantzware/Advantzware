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
/// Summary description for browsinvoice
/// </summary>
[System.ComponentModel.DataObject]
public class browsinvoice : AppServerConnect.AppServer
{
    public browsinvoice()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsBrwsInvDataSet SelectBrowsInvoice(string prmUser, string prmAction, string prmOrderNum, string prmInvoice, string prmCustomer, string prmItem, string prmAccount, string prmPart, string prmCustPo, string prmBOL, string prmEstimate, string prmDate, string prmOpen, string prmPaid)
    {

        dsBrwsInvDataSet dsBrwsInv = new dsBrwsInvDataSet();
        dsBrwsInv = null;
        AppServerConnect();
        aoObject.BrwsInv(prmUser, prmAction, prmOrderNum, prmInvoice, prmCustomer, prmItem, prmAccount, prmPart, prmCustPo, prmBOL, prmEstimate, prmDate, prmOpen, prmPaid, ref dsBrwsInv);
        AppServerDisconnect();

        return dsBrwsInv;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsViewInvDataSet selectViewInv(string prmUser, string prmAction, string prmOrderNum, Int32 prmInv)
    {

        dsViewInvDataSet dsViewInv = new dsViewInvDataSet();
        dsViewInv = null;
        AppServerConnect();
        aoObject.ViewInvoice(prmUser, prmAction, prmOrderNum, prmInv, ref dsViewInv);
        AppServerDisconnect();

        return dsViewInv;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsViewInvPrintDataSet ViewInvPrint(string prmAction, Int64 vRowid)
    {

        dsViewInvPrintDataSet dsViewInvPrint = new dsViewInvPrintDataSet();
        dsViewInvPrint = null;
        AppServerConnect();
        aoObject.ViewInvPrint(prmAction, vRowid, ref dsViewInvPrint);
        AppServerDisconnect();

        return dsViewInvPrint;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCreditStatusInvDataSet CreditStatusInv(string prmUser, string prmCust)
    {

        dsCreditStatusInvDataSet dsCreditStatusInv = new dsCreditStatusInvDataSet();
        dsCreditStatusInv = null;
        AppServerConnect();
        aoObject.CreditStatusInv(prmUser, prmCust, ref dsCreditStatusInv);
        AppServerDisconnect();

        return dsCreditStatusInv;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsbolInvDataSet BolInv(string prmUser, string prmAction, string prmOrderNum, string prmBol)
    {

        dsbolInvDataSet dsbolInv = new dsbolInvDataSet();
        dsbolInv = null;
        AppServerConnect();
        aoObject.bolInv(prmUser, prmAction, prmOrderNum, prmBol, ref dsbolInv);
        AppServerDisconnect();

        return dsbolInv;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsbolInvPrintDataSet BolInvPrint(string prmUser, string prmAction, Int64 vRowid)
    {

        dsbolInvPrintDataSet dsbolInvPrint = new dsbolInvPrintDataSet();
        dsbolInvPrint = null;
        string vFile;
        AppServerConnect();
        aoObject.bolInvPrint(prmUser, prmAction, vRowid, ref dsbolInvPrint, out vFile);
        AppServerDisconnect();



        return dsbolInvPrint;

    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsVendItemLookDataSet SelectVendItem(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsVendItemLookDataSet dsVendLook = new dsVendItemLookDataSet();
        dsVendLook = null;
        AppServerConnect();
        aoObject.VendItemLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsVendLook);
        AppServerDisconnect();
        return dsVendLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsItemPoLookDataSet SelectPoItem(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsItemPoLookDataSet dsPOLook = new dsItemPoLookDataSet();
        dsPOLook = null;
        AppServerConnect();
        aoObject.ItemPoLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsPOLook);
        AppServerDisconnect();
        return dsPOLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsVendorLookDataSet Selectvendor(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsVendorLookDataSet dsVendorLook = new dsVendorLookDataSet();
        dsVendorLook = null;
        AppServerConnect();
        aoObject.VendorLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsVendorLook);
        AppServerDisconnect();
        return dsVendorLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsSignBolPrintDataSet SignBolPrint(string prmAction, Int64 vRowid)
    {

        dsSignBolPrintDataSet dsSignBolPrint = new dsSignBolPrintDataSet();
        dsSignBolPrint = null;
        AppServerConnect();
        aoObject.SignBolPrint(prmAction, vRowid, ref dsSignBolPrint);
        AppServerDisconnect();

        return dsSignBolPrint;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsArInvoiceDataSet ArInvoice(string prmAction, string prmUser, string prmOrderNum, string prmInvoice, string prmCustomer, string prmItem, string prmPart, string prmCustPo, string prmBOL, string prmEstimate, string prmDate, string prmOpen, string prmPaid)
    {

        dsArInvoiceDataSet dsArInvoice = new dsArInvoiceDataSet();
        dsArInvoice = null;
        AppServerConnect();
        aoObject.ArInvoice(prmAction, prmUser, prmOrderNum, prmInvoice, prmCustomer, prmItem, prmPart, prmCustPo, prmBOL, prmEstimate, prmDate, prmOpen, prmPaid, ref dsArInvoice);
        AppServerDisconnect();

        return dsArInvoice;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsTopCustNoteDataSet SelectCustNotes(string prmAction, string prmUser, string prmRecKey, string prmHeader, string prmNoteDate, string prmNoteTime, string prmUserId, string prmViewed, string prmCode, string prmGroup, string prmNoteTitle, string prmNewNoteTitle, string prmNoteText)
    {
        string cError = "";
        dsTopCustNoteDataSet dsTopCustNote = new dsTopCustNoteDataSet();
        dsTopCustNote = null;
        AppServerConnect();
        aoObject.TopCustNotes(prmAction, prmUser, prmRecKey, prmHeader, prmNoteDate, prmNoteTime, prmUserId, prmViewed, prmCode, prmGroup, prmNoteTitle, prmNewNoteTitle, prmNoteText, ref dsTopCustNote, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsTopCustNote;
    }

}

