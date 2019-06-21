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
using System.Data.SqlClient;

using System.Net.Mail;



/// <summary>
/// Summary description for Order
/// </summary>
[System.ComponentModel.DataObject]
public class Order : AppServerConnect.AppServer
{
    public Order()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    //public dsOrderDataSet SelectOrder(string prmUser, string prmAction)
    //{

    //    dsOrderDataSet dsOrder= new dsOrderDataSet();
    //    dsOrder = null;
    //    AppServerConnect();
    //    aoObject.order_enq("", prmUser, prmAction, "", "", "", "", "", "", "", "", ref dsOrder);
    //    AppServerDisconnect();

    //    return dsOrder;
    //}
    

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsOrderDataSet SelectOrder(string prmUser, string prmCust, string prmAction, string prmPonum, string prmPartno, string prmPostatus, string prmOrderNum, string prmFgitem, string prmEst, string prmJob, string prmJob2, string prmQuote )
    {

        dsOrderDataSet dsOrder = new dsOrderDataSet();
        dsOrder = null;
        AppServerConnect();

        aoObject.order_enq(prmCust, prmUser, prmAction, prmPonum, prmPartno, prmPostatus, prmOrderNum, prmFgitem, prmEst, prmJob, prmJob2, prmQuote, ref dsOrder);
        AppServerDisconnect();
        return dsOrder;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsvieworderDataSet Selectvieworder(string prmUser, string prmOrderNum)
    {

        dsvieworderDataSet dsvieworder = new dsvieworderDataSet();
        dsvieworder = null;
        AppServerConnect();
        aoObject.vieworder(prmUser, prmOrderNum,ref dsvieworder);        
        AppServerDisconnect();

        return dsvieworder;
    }




    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsRelDataSet SelectRel(string prmUser, string prmOrderNum, string prmItemNum, string prmAction, Int64 vRowid, string Asi, string AShipTo, string AVia, string ASqty, string ApoNo, string AlotNo,  DateTime ADate, string sellPrice, string HeaderDueDate, string HeaderLastShipDate, string LineItemDueDate, string LineItemLastShipDate, string RelAllDt, string AllPo, string ShipAllDate, string Afrtpay, string Afob)
    {

        dsRelDataSet dsRel = new dsRelDataSet();

        //add a new DataSet that returns e-mail messages

        dsRel = null;
        string cError;
        string vMailto=null;
        string vSubject=null;
        string vBody=null;
        string vMailFrom = null;
        string vInternalUser = null;

        AppServerConnect();

        ////return DataSet full of e-mail messages
        aoObject.OeRelease(prmUser, prmOrderNum, prmItemNum, prmAction, vRowid, Asi, AShipTo, AVia, ASqty, ApoNo, AlotNo, ADate, sellPrice, HeaderDueDate, HeaderLastShipDate, LineItemDueDate, LineItemLastShipDate, RelAllDt, AllPo, ShipAllDate, Afrtpay, Afob, ref dsRel, out cError, out vMailto, out vSubject, out vBody, out vMailFrom, out vInternalUser);        
        AppServerDisconnect();       

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        conn.Open();
        string cmd = "select email_from, subject from email_alerts where program_name = 'Order Release'";
        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
        DataSet ds = new DataSet();
        da.Fill(ds);
        conn.Close();

        try
        {
            vMailFrom = Convert.ToString(ds.Tables[0].Rows[0][0]);
            vSubject = Convert.ToString(ds.Tables[0].Rows[0][1]);
        }
        catch
        {

        }
        finally
        {
            conn.Close();
        }
        
        
        if (vMailto != "")
        {
            try
            {
                if (vInternalUser == "no")
                {
                    MailMessage om = new MailMessage();
                    om.To.Add(vMailto);
                    om.From = new MailAddress(vMailFrom);
                    om.Subject = vSubject;
                    om.Body = vBody;
                    om.IsBodyHtml = true;
                    om.Priority = MailPriority.High;
                    SmtpClient smpt = new SmtpClient();
                    smpt.Send(om);
                }
            }
            catch { }
        }
                
        return dsRel;

    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateRelease(string prmUser, string prmOrderNum, string prmItemNum, string prmAction, Int64 vRowid, string Asi, string AShipTo, string AVia, string ASqty, string ApoNo, string AlotNo, DateTime ADate, string sellPrice, string HeaderDueDate, string HeaderLastShipDate, string LineItemDueDate, string LineItemLastShipDate, string RelAllDt, string AllPo, string ShipAllDate, string Afrtpay, string Afob)
    {
        
        dsRelDataSet dsRel = new dsRelDataSet();

        dsRel = null;
        string cError = "";
        string vMailto = null;
        string vSubject = null;
        string vBody = null;
        string vMailFrom = null;
        string vInternalUser = null;

        AppServerConnect();
        aoObject.OeRelease(prmUser, prmOrderNum, prmItemNum, prmAction, vRowid, Asi, AShipTo, AVia, ASqty, ApoNo, AlotNo, ADate, sellPrice, HeaderDueDate, HeaderLastShipDate, LineItemDueDate, LineItemLastShipDate, RelAllDt, AllPo, ShipAllDate, Afrtpay, Afob, ref dsRel, out cError, out vMailto, out vSubject, out vBody, out vMailFrom, out vInternalUser);        
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
    public String OrderMail(string prmUser, string prmAction, string prmOrderNum, string prmComp, string prmItem)
    {

        //add a new DataSet that returns e-mail messages

        string vMailto = null;
        string vMailFrom = null;
        string vSubject = null;
        string vBody = null;
        string vInternalUser = null;

        AppServerConnect();

        ////return DataSet full of e-mail messages
        aoObject.OrderMail(prmUser, prmAction, prmOrderNum, prmComp, prmItem, out vMailto, out vMailFrom, out vSubject, out vBody, out vInternalUser);
        AppServerDisconnect();

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        conn.Open();
        string cmd = "";
        if(vInternalUser == "no")
        cmd = "select email_from, subject from email_alerts where program_name = 'Add Order'";
        if (vInternalUser == "yes")
            cmd = "select email_from, subject from email_alerts where program_name = 'E Add Order'";
        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
        DataSet ds = new DataSet();
        da.Fill(ds);
        conn.Close();

        try
        {
            vMailFrom = Convert.ToString(ds.Tables[0].Rows[0][0]);
            vSubject = Convert.ToString(ds.Tables[0].Rows[0][1]);
        }
        catch
        {

        }
        finally
        {
            conn.Close();
        }
        
        if (vMailto != "")
        {
            try
            {                
                    MailMessage om = new MailMessage();
                    om.To.Add(vMailto);
                    om.From = new MailAddress(vMailFrom);
                    om.Subject = vSubject;
                    om.Body = vBody;
                    om.IsBodyHtml = true;
                    om.Priority = MailPriority.High;
                    SmtpClient smpt = new SmtpClient();
                    smpt.Send(om);
                
            }
            catch { }
        }
       return vMailto;

    }
    

    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    //public dsOrderFgDataSet SelectOrderFg(string prmOrderNum,string prmOrderOnHand)
    //{

    //    dsOrderFgDataSet dsOrderFg = new dsOrderFgDataSet();
    //    dsOrderFg = null;
    //    AppServerConnect();
    //    aoObject.OrderOnHand("001", "ATT1000", "", prmOrderNum,prmOrderOnHand, ref dsOrderFg);
    //    AppServerDisconnect();

    //    return dsOrderFg;
    //}

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsfgDataSet Selectfg()
    {

        dsfgDataSet dsfg = new dsfgDataSet();
        dsfg = null;
        AppServerConnect();
        aoObject.fgitem("001", "ATT1000", "", ref dsfg);
        AppServerDisconnect();

        return dsfg;
    }
    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    //public dsOrder1DataSet SelectOrder1()
    //{

    //    dsOrder1DataSet dsOrder1 = new dsOrder1DataSet();
    //    dsOrder1 = null;
    //    AppServerConnect();
    //    aoObject.Order("001", "ATT1000", "", ref dsOrder1);
    //    AppServerDisconnect();

    //    return dsOrder1;
    //}

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dscust1DataSet Selectcust()
    {

        dscust1DataSet dscust1 = new dscust1DataSet();
        dscust1 = null;
        AppServerConnect();
        aoObject.cust("001", "ATT1000", "", ref dscust1);
        AppServerDisconnect();

        return dscust1;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsestDataSet Selectest()
    {

        dsestDataSet dsest = new dsestDataSet();
        dsest = null;
        AppServerConnect();
        aoObject.est("001", "ATT1000", "", ref dsest);
        AppServerDisconnect();

        return dsest;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsjobDataSet Selectjob()
    {
        string prmUser = "";
        dsjobDataSet dsjob = new dsjobDataSet();
        dsjob = null;
        AppServerConnect();
        aoObject.job(prmUser, ref dsjob);
        AppServerDisconnect();

        return dsjob;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dspartDataSet Selectpart()
    {

        dspartDataSet dspart = new dspartDataSet();
        dspart = null;
        AppServerConnect();
        aoObject.part("001", "ATT1000", "", ref dspart);
        AppServerDisconnect();

        return dspart;
    }

    

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsOrdLookDataSet SelectOrderLook(string prmCust, string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsOrdLookDataSet dsOrdLook = new dsOrdLookDataSet();
        dsOrdLook = null;
        AppServerConnect();
        aoObject.OrdLook(prmCust, prmUser, prmAction, prmField, prmCondition, prmText, ref dsOrdLook);
        AppServerDisconnect();
        return dsOrdLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsCustomerDataSet SelectCustomer( string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsCustomerDataSet dsCustomer = new dsCustomerDataSet();
        dsCustomer = null;
        AppServerConnect();
        aoObject.CustLookup(prmAction, prmUser, prmField, prmCondition, prmText, ref dsCustomer);
        AppServerDisconnect();
        return dsCustomer;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsCustOrderLookDataSet SelectOrderCust(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsCustOrderLookDataSet dsCustOrderLook = new dsCustOrderLookDataSet();
        dsCustOrderLook = null;
        AppServerConnect();
        aoObject.CustOrderLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsCustOrderLook);
        AppServerDisconnect();
        return dsCustOrderLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsInvCustomerDataSet InvCustomer(string prmUser, string prmAction, string prmField, string prmCondition, string prmText, string prmOrderNum)
    {

        dsInvCustomerDataSet dsInvCustomer = new dsInvCustomerDataSet();
        dsInvCustomer = null;
        AppServerConnect();
        aoObject.InvCustLookup(prmAction, prmUser, prmField, prmCondition, prmText,prmOrderNum, ref dsInvCustomer);
        AppServerDisconnect();
        return dsInvCustomer;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsItemLookDataSet SelectFgItem(string prmAction, string prmUser, string prmCust, string prmStat, string prmField, string prmCondition, string prmText)
    {

        dsItemLookDataSet dsItemLook1 = new dsItemLookDataSet();
        dsItemLook1 = null;
        AppServerConnect();
        aoObject.ItemLook(prmAction, prmUser, prmCust, prmStat, prmField, prmCondition, prmText, ref dsItemLook1);
        AppServerDisconnect();
        return dsItemLook1;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsItemReportLookDataSet SelectItemReport(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmCust)
    {

        dsItemReportLookDataSet dsItemReportLook1 = new dsItemReportLookDataSet();
        dsItemReportLook1 = null;
        AppServerConnect();
        aoObject.ItemReportLook(prmAction, prmUser, prmField, prmCondition, prmText, prmCust, ref dsItemReportLook1);
        AppServerDisconnect();
        return dsItemReportLook1;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsInvItemLookDataSet InvFgItem(string prmUser, string prmAction, string prmField, string prmCondition, string prmText, string prmOrderNum)
    {

        dsInvItemLookDataSet dsInvItemLook = new dsInvItemLookDataSet();
        dsInvItemLook = null;
        AppServerConnect();
        aoObject.InvItemLook(prmAction, prmUser, prmField, prmCondition, prmText, prmOrderNum, ref dsInvItemLook);
        AppServerDisconnect();
        return dsInvItemLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsInvoiceLookDataSet InvNum(string prmUser, string prmAction, string prmField, string prmCondition, string prmText )
    {

        dsInvoiceLookDataSet dsInvoiceLook = new dsInvoiceLookDataSet();
        dsInvoiceLook = null;
        AppServerConnect();
        aoObject.InvNumLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsInvoiceLook);
        AppServerDisconnect();
        return dsInvoiceLook;
    }
    public dsBolNumLookDataSet BolNum(string prmUser, string prmAction, string prmField, string prmCondition, string prmText, string prmCustomer, string prmPosted)
    {

        dsBolNumLookDataSet dsBolNumLook = new dsBolNumLookDataSet();
        dsBolNumLook = null;
        AppServerConnect();
        aoObject.BolNumLook(prmAction, prmUser, prmField, prmCondition, prmText, prmCustomer,prmPosted,  ref dsBolNumLook);
        AppServerDisconnect();
        return dsBolNumLook;
    }


    /*[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsOrdLookDataSet SelectOder(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsOrdLookDataSet dsOrdLook = new dsOrdLookDataSet();
        dsOrdLook = null;
        AppServerConnect();
        aoObject.OrdLook(prmAction, "001", "ATT1000", "", prmField, prmCondition, prmText, ref dsOrdLook);
        AppServerDisconnect();
        return dsOrdLook;
    }*/

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsInvPartLookDataSet InvCustPartLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText, string prmOrderNum)
    {

        dsInvPartLookDataSet dsInvPartLook = new dsInvPartLookDataSet();
        dsInvPartLook = null;
        AppServerConnect();
        aoObject.InvPartLook(prmAction, prmUser,  prmField, prmCondition, prmText, prmOrderNum, ref dsInvPartLook);
        AppServerDisconnect();
        return dsInvPartLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsPartLookDataSet SelectCustPartLook(string prmCust, string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmItem)
    {

        dsPartLookDataSet dsPartLook = new dsPartLookDataSet();
        dsPartLook = null;
        AppServerConnect();
        aoObject.PartLook(prmCust, prmAction, prmUser, prmField, prmCondition, prmText,prmItem, ref dsPartLook);
        AppServerDisconnect();
        return dsPartLook;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsPoLookDataSet SelectCustPoLook(string prmCust, string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsPoLookDataSet dsPOLook = new dsPoLookDataSet();
        dsPOLook = null;
        AppServerConnect();
        aoObject.PoLook(prmCust, prmAction, prmUser, prmField, prmCondition, prmText, ref dsPOLook);
        AppServerDisconnect();
        return dsPOLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsInvPoLookDataSet InvCustPoLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText, string prmOrderNum)
    {

        dsInvPoLookDataSet dsInvPOLook = new dsInvPoLookDataSet();
        dsInvPOLook = null;
        AppServerConnect();
        aoObject.InvPoLook(prmAction, prmUser, prmField, prmCondition, prmText,prmOrderNum, ref dsInvPOLook);
        AppServerDisconnect();
        return dsInvPOLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsEbLookDataSet SelectEstimateLook(string prmCust, string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsEbLookDataSet dsEbLook = new dsEbLookDataSet();
        dsEbLook = null;
        AppServerConnect();
        aoObject.EbLook(prmCust, prmAction, prmUser, prmField, prmCondition, prmText, ref dsEbLook);
        AppServerDisconnect();
        return dsEbLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsInvEbLookDataSet InvEstimateLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText, string prmOrderNum)
    {

        dsInvEbLookDataSet dsInvEbLook = new dsInvEbLookDataSet();
        dsInvEbLook = null;
        AppServerConnect();
        aoObject.InvEbLook(prmAction, prmUser, prmField, prmCondition, prmText, prmOrderNum, ref dsInvEbLook);
        AppServerDisconnect();
        return dsInvEbLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsJobLookDataSet SelectJob1Look(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsJobLookDataSet dsJobLook = new dsJobLookDataSet();
        dsJobLook = null;
        AppServerConnect();
        aoObject.JobLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsJobLook);
        AppServerDisconnect();
        return dsJobLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsViewItemDataSet Selectview(string prmUser, string prmAction, string prmOrderNum,string prmItemNum)
    {

        dsViewItemDataSet dsViewItem = new dsViewItemDataSet();
        dsViewItem = null;
        AppServerConnect();
        aoObject.ViewItem(prmUser, prmAction, prmOrderNum,prmItemNum, ref dsViewItem);
        AppServerDisconnect();

        return dsViewItem;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsItemSelDataSet SelectviewSel(string prmUser, string prmAction, string prmOrderNum, string prmItemNum)
    {

        dsItemSelDataSet dsItemSel = new dsItemSelDataSet();
        dsItemSel = null;
        AppServerConnect();
        aoObject.ItemSel(prmUser, prmAction, prmOrderNum, prmItemNum, ref dsItemSel);
        AppServerDisconnect();

        return dsItemSel;
    }



    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    //public dsOrderFg1DataSet onhand(string prmAction, string prmItemNum)
    //{

    //    dsOrderFg1DataSet dsOrderFg1 = new dsOrderFg1DataSet();
    //    dsOrderFg1 = null;
    //    AppServerConnect();
    //    aoObject.on("001", "ATT1000", "", prmAction, prmItemNum ,ref dsOrderFg1);
    //    AppServerDisconnect();

    //    return dsOrderFg1;
    //}

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsHistDataSet history(string prmUser, string prmOrderNum)
    {

        dsHistDataSet dshist = new dsHistDataSet();
        dshist = null;
        AppServerConnect();
        aoObject.hist(prmUser, prmOrderNum, ref dshist);
        AppServerDisconnect();

        return dshist;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsMiscChargeDataSet MiscCharge(string prmUser, string prmAction, string prmOrderNum, string vLine)
    {

        dsMiscChargeDataSet dsMiscCharge = new dsMiscChargeDataSet();
        dsMiscCharge = null;
        AppServerConnect();
        aoObject.MiscCharge(prmUser, prmAction, prmOrderNum, vLine, ref dsMiscCharge);
        AppServerDisconnect();

        return dsMiscCharge;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsMiscCharge1DataSet MiscCharge1(string prmUser, string prmAction, string prmSell)
    {

        dsMiscCharge1DataSet dsMiscCharge1 = new dsMiscCharge1DataSet();
        dsMiscCharge1 = null;
        AppServerConnect();
        aoObject.MiscCharge1(prmUser, prmAction, prmSell, ref dsMiscCharge1);
        AppServerDisconnect();

        return dsMiscCharge1;
    }

    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    //public dsViewJobDataSet jobprod(string prmOrderNum)
   // {

     //   dsViewJobDataSet dsViewJob = new dsViewJobDataSet();
        //dsViewJob = null;
       // AppServerConnect();
    //    aoObject.ViewJob("001", "ATT1000", "", prmOrderNum, ref dsViewJob);
        //AppServerDisconnect();

  //      return dsViewJob;
//    }

    

    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    //public dsOrdItemDataSet FgItem(string prmOrderNum)
    //{

    //    dsOrdItemDataSet dsOrdItem = new dsOrdItemDataSet();
    //    dsOrdItem = null;
    //    AppServerConnect();
    //    aoObject.OrdItem("001", "", "", prmOrderNum, ref dsOrdItem);
    //    AppServerDisconnect();

    //    return dsOrdItem;
    //}

   // [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
   // public dsMatInfoDataSet MatInfo(string prmOrderNum)
   // {

     //   dsMatInfoDataSet dsMatInfo = new dsMatInfoDataSet();
     //   dsMatInfo = null;
     //   AppServerConnect();
     //   aoObject.MatInfo("001", "", "", prmOrderNum, ref dsMatInfo);
        //AppServerDisconnect();

        //return dsMatInfo;
    //}

    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    //public dsMachHrDataSet MachHr(string prmOrderNum)
    //{

    //    dsMachHrDataSet dsMachHr = new dsMachHrDataSet();
    //    dsMachHr = null;
    //    AppServerConnect();
    //    aoObject.MachHr("001", "", "", prmOrderNum ,ref dsMachHr);
    //    AppServerDisconnect();

    //    return dsMachHr;
    //}
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
        public dsInvoLookDataSet Invo(string prmUser, string prmAction, string prmField, string prmCondition, string prmText, string prmOrderNum)
    {

        dsInvoLookDataSet dsInvoLook = new dsInvoLookDataSet();
        dsInvoLook = null;
        AppServerConnect();
        aoObject.InvoLook(prmAction, prmUser, prmField, prmCondition, prmText, prmOrderNum, ref dsInvoLook);
        AppServerDisconnect();
        return dsInvoLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsBolLookDataSet Bol(string prmUser, string prmAction, string prmField, string prmCondition, string prmText, string prmOrderNum)
    {

        dsBolLookDataSet dsBolLook = new dsBolLookDataSet();
        dsBolLook = null;
        AppServerConnect();
        aoObject.BolLook(prmAction, prmUser, prmField, prmCondition, prmText, prmOrderNum, ref dsBolLook);
        AppServerDisconnect();
        return dsBolLook;
    }
   
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsRowsDataSet SelectRows(string prmUser, Int32 vLine)
    {

        dsRowsDataSet dsRows = new dsRowsDataSet();
        dsRows = null;
        AppServerConnect();
        aoObject.Rows(prmUser, vLine, ref dsRows);
        AppServerDisconnect();

        return dsRows;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsOrderTotDataSet OrderTot(string prmUser, string prmAction, string prmOrderNum, decimal prmWeight, decimal prmTax, decimal prmFreight, string prmFbill, decimal prmRevenue, decimal prmCost, decimal prmComm)
    {

        dsOrderTotDataSet dsOrderTot = new dsOrderTotDataSet();
        dsOrderTot = null;
        AppServerConnect();
        aoObject.OrderTot(prmUser, prmAction, prmOrderNum, prmWeight, prmTax, prmFreight, prmFbill, prmRevenue, prmCost, prmComm, ref dsOrderTot);
        AppServerDisconnect();

        return dsOrderTot;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsbrwsitemstatusDataSet BrwsItemInq(string prmUser, string prmActItem, string prmItem, string prmItemName, string prmCad, string prmEst, string prmStyle, string prmProcat)
    {

        dsbrwsitemstatusDataSet dsbrwsitemstatus = new dsbrwsitemstatusDataSet();
        dsbrwsitemstatus = null;
        AppServerConnect();
        aoObject.BrwsItemStatus(prmUser, prmActItem, prmItem, prmItemName, prmCad, prmEst, prmStyle, prmProcat, ref dsbrwsitemstatus);
        AppServerDisconnect();

        return dsbrwsitemstatus;
    }

         
  
     
 
      
      
    
   







}


