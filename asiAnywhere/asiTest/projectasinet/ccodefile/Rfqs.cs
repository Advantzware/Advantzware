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
/// Summary description for browsinvoice
/// </summary>
[System.ComponentModel.DataObject]
public class rfqs : AppServerConnect.AppServer
{
    public rfqs()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsListRfqsDataSet ListRfqs(string prmComp, string prmCust, string prmUser, string prmAction, Int32 vRfqNo, string prmPartno, string vPartDscr, string vStyle, string prmEst, string vLength, string vWidth, string vDepth)
    {
        string cError = "";
        dsListRfqsDataSet dsListRfqs = new dsListRfqsDataSet();
        dsListRfqs = null;
        AppServerConnect();
        aoObject.list_rfqs(prmComp, prmCust, prmUser, prmAction, vRfqNo, prmPartno, vPartDscr, vStyle, prmEst, vLength, vWidth, vDepth, ref dsListRfqs, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsListRfqs;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsRfqsLookDataSet RfqsLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText, string prmCustomer)
    {

        dsRfqsLookDataSet dsRfqsLook = new dsRfqsLookDataSet();
        dsRfqsLook = null;
        AppServerConnect();
        aoObject.RfqsLook(prmAction, prmUser, prmField, prmCondition, prmText, prmCustomer, ref dsRfqsLook);
        AppServerDisconnect();
        return dsRfqsLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsRfqItemDataSet Rfqitem(string prmComp, string prmUser, string prmAction, Int32 prmRfqNo, Int32 RfqSeqNo, Int32 RfqQty, string RfqStock, string RfqName, string RfqPartno, string Rfqstyle, string RfqProcat, Int32 RfqCol, Int32 RfqCoat, decimal RfqLength, decimal RfqWidth, decimal RfqDepth, string RfqBoard, decimal RfqCal, Int32 RfqQuantity, Int64 RfqRowid, Int32 lv_qty2, Int32 lv_qty3, Int32 lv_qty4, Int32 lv_qty5, Int32 lv_qty6, Int32 lv_qty7, Int32 lv_qty8, Int32 lv_qty9, Int32 lv_qty10, decimal lv_price_1, decimal lv_price_2, decimal lv_price_3, decimal lv_price_4, decimal lv_price_5, decimal lv_price_6, decimal lv_price_7, decimal lv_price_8, decimal lv_price_9, decimal lv_price_10, string lv_uom_1, string lv_uom_2, string lv_uom_3, string lv_uom_4, string lv_uom_5, string lv_uom_6, string lv_uom_7, string lv_uom_8, string lv_uom_9, string lv_uom_10, DateTime lv_date_1, DateTime lv_date_2, DateTime lv_date_3, DateTime lv_date_4, DateTime lv_date_5, DateTime lv_date_6, DateTime lv_date_7, DateTime lv_date_8, DateTime lv_date_9, DateTime lv_date_10, Int32 lv_delivery_1, Int32 lv_delivery_2, Int32 lv_delivery_3, Int32 lv_delivery_4, Int32 lv_delivery_5, Int32 lv_delivery_6, Int32 lv_delivery_7, Int32 lv_delivery_8, Int32 lv_delivery_9, Int32 lv_delivery_10, string RfqEstNo)
    {
        string cError = "";
        string vMailto = null;
        string vSubject = null;
        string vBody = null;
        string vFrom = null;
        dsRfqItemDataSet dsRfqItem = new dsRfqItemDataSet();
        dsRfqItem = null;
        AppServerConnect();
        aoObject.RfqItem(prmComp, prmUser, prmAction, prmRfqNo, RfqSeqNo, RfqQty, RfqStock, RfqName, RfqPartno, Rfqstyle, RfqProcat, RfqCol, RfqCoat, RfqLength, RfqWidth, RfqDepth, RfqBoard, RfqCal, RfqQuantity, RfqRowid, lv_qty2, lv_qty3, lv_qty4, lv_qty5, lv_qty6, lv_qty7, lv_qty8, lv_qty9, lv_qty10, lv_price_1, lv_price_2, lv_price_3, lv_price_4, lv_price_5, lv_price_6, lv_price_7, lv_price_8, lv_price_9, lv_price_10, lv_uom_1, lv_uom_2, lv_uom_3, lv_uom_4, lv_uom_5, lv_uom_6, lv_uom_7, lv_uom_8, lv_uom_9, lv_uom_10, lv_date_1, lv_date_2, lv_date_3, lv_date_4, lv_date_5, lv_date_6, lv_date_7, lv_date_8, lv_date_9, lv_date_10, lv_delivery_1, lv_delivery_2, lv_delivery_3, lv_delivery_4, lv_delivery_5, lv_delivery_6, lv_delivery_7, lv_delivery_8, lv_delivery_9, lv_delivery_10, RfqEstNo, ref dsRfqItem, out cError, out vMailto, out vBody, out vFrom, out vSubject);        

        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        else
        {
            if (prmAction == "AddRfqItem")
            {
                this.RfqMail(prmUser, "MailRfq", prmRfqNo, prmComp);
            }
        }        
        return dsRfqItem;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public string ValidateRfq(string prmComp, string prmUser, string prmAction, string RfqStock, string Rfqstyle, string RfqProcat, string RfqBoard)
    {
        string cError = "";      
        AppServerConnect();
        aoObject.ValidateRfq(prmComp, prmUser, prmAction, RfqStock, Rfqstyle, RfqProcat, RfqBoard, out cError);
        AppServerDisconnect();       
        return cError;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsRfqItemDscrDataSet RfqItemDscr(string prmUser, Int32 prmRfqNo, string prmPartNo)
    {

        dsRfqItemDscrDataSet dsRfqItemDscr = new dsRfqItemDscrDataSet();
        dsRfqItemDscr = null;
        AppServerConnect();
        aoObject.RfqItemDscr(prmUser, prmRfqNo, prmPartNo, ref dsRfqItemDscr);
        AppServerDisconnect();

        return dsRfqItemDscr;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsViewRfqDataSet ViewRfq(string prmUser, string prmAction, string prmExt, Int32 PrmRfqNo, DateTime prmReqdate, DateTime prmDuedate, string prmCustno, string prmShipname, string prmShipAddr, string prmShipAddr2, string prmShipcity, string prmShipstate, string prmShipzip, string prmSman, string prmSmanName, decimal prmComm, string prmFobcode, string prmChgmethod, Int32 prmWhmonth, string prmInst, Int64 VRowid)
    {
        string cError = "";
        dsViewRfqDataSet dsViewRfq = new dsViewRfqDataSet();
        dsViewRfq = null;
        AppServerConnect();
        aoObject.ViewRfq(prmUser, prmAction, prmExt, PrmRfqNo, prmReqdate, prmDuedate, prmCustno, prmShipname, prmShipAddr, prmShipAddr2, prmShipcity, prmShipstate, prmShipzip, prmSman, prmSmanName, prmComm, prmFobcode, prmChgmethod, prmWhmonth, prmInst, VRowid, ref dsViewRfq, out cError);
        AppServerDisconnect();        

        return dsViewRfq;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ViewRfqValidate(string prmUser, string prmAction, string prmExt, Int32 PrmRfqNo, DateTime prmReqdate, DateTime prmDuedate, string prmCustno, string prmShipname, string prmShipAddr, string prmShipAddr2, string prmShipcity, string prmShipstate, string prmShipzip, string prmSman, string prmSmanName, decimal prmComm, string prmFobcode, string prmChgmethod, Int32 prmWhmonth, string prmInst, Int64 VRowid)
    {
        string cError = "";
        dsViewRfqDataSet dsViewRfq = new dsViewRfqDataSet();
        dsViewRfq = null;
        AppServerConnect();
        aoObject.ViewRfq(prmUser, prmAction, prmExt, PrmRfqNo, prmReqdate, prmDuedate, prmCustno, prmShipname, prmShipAddr, prmShipAddr2, prmShipcity, prmShipstate, prmShipzip, prmSman, prmSmanName, prmComm, prmFobcode, prmChgmethod, prmWhmonth, prmInst, VRowid, ref dsViewRfq, out cError);
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
    public dsRfqSizeDataSet RfqSize(string prmAction, string prmUser, Int32 prmRfqNo, string PrmPartNo, string prmName, string prmStyle, string prmTab, decimal prmLen, decimal prmWid, decimal prmDep, decimal prmDust, decimal prmPanel, decimal prmTuck, string prmAdhesive, decimal prmKWid, decimal prmKLen, decimal prmLock, decimal prmGluela, decimal prmLin, decimal prmTWid, decimal prmTLen, string prmdscr, decimal prmSqin)
    {
        string cError = "";
        dsRfqSizeDataSet dsRfqSize = new dsRfqSizeDataSet();
        dsRfqSize = null;
        AppServerConnect();
        aoObject.RfqSize(prmAction, prmUser, prmRfqNo, PrmPartNo, prmName, prmStyle, prmTab, prmLen, prmWid, prmDep, prmDust, prmPanel, prmTuck, prmAdhesive, prmKWid, prmKLen, prmLock, prmGluela, prmLin, prmTWid, prmTLen, prmdscr, prmSqin, ref dsRfqSize, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsRfqSize;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsRfqItemSpecDataSet RfqItemSpec(string prmAction, string prmUser, Int32 prmRfqNo, string PrmPartNo, string prmItemName, string prmDscr, string prmDscr2, string prmDscr3, string prmEstimate, string prmStock, string prmPlate, string prmDie, string prmSample, string prmUpc, string prmSpc, string prmCat, string prmCatdscr)
    {

        dsRfqItemSpecDataSet dsRfqItemSpec = new dsRfqItemSpecDataSet();
        dsRfqItemSpec = null;
        AppServerConnect();
        aoObject.RfqItemSpec(prmAction, prmUser, prmRfqNo, PrmPartNo, prmItemName, prmDscr, prmDscr2, prmDscr3, prmEstimate, prmStock, prmPlate, prmDie, prmSample, prmUpc, prmSpc, prmCat, prmCatdscr, ref dsRfqItemSpec);
        AppServerDisconnect();
        return dsRfqItemSpec;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsRfqMaterialDataSet RfqMaterial(string prmComp, string prmCust, string prmUser, string prmAction, Int32 prmRfqNo, string PrmPartNo, Int64 MatRowid, string prmBoard, string prmBrdDscr, decimal prmCal, decimal prmGshwid, decimal prmGshlen, string prmLeaf1, decimal prmLeafw1, decimal prmLeafl1, string prmLeaf2, decimal prmLeafw2, decimal prmLeafl2, string prmLeaf3, decimal prmvLeafw3, decimal prmLeafl3, string prmLeaf4, decimal prmLeafw4, decimal prmLeafl4, string prmSpecdscr1, string prmSpecdscr2, string prmSpecdscr3, string prmSpecdscr4, string prmSpecdscr5, string prmSpecdscr6, string prmAdder1, string prmAdder2, string prmAdder3, string prmAdder4, string prmAdder5, string prmAdder6, string prmAdder7, string prmAdder8, string prmAdder9, string prmAdder10, string prmAdder11, string prmAdder12, string prmSpecno1, string prmSpecno2, string prmSpecno3, string prmSpecno4, string prmSpecno5, string prmSpecno6, string prmLeafdscr, string prmLeafdscr2, string prmLeafdscr3, string prmLeafdscr4, decimal prmSpecQty, decimal prmSpecQty2, decimal prmSpecQty3, decimal vSpecQty4, decimal prmSpecQty5, decimal prmSpecQty6)
    {
        string cError = "";
        dsRfqMaterialDataSet dsRfqMaterial = new dsRfqMaterialDataSet();
        dsRfqMaterial = null;
        AppServerConnect();
        aoObject.RfqMaterial(prmComp, prmCust, prmUser, prmAction, prmRfqNo, PrmPartNo, MatRowid, prmBoard, prmBrdDscr, prmCal, prmGshwid, prmGshlen, prmLeaf1, prmLeafw1, prmLeafl1, prmLeaf2, prmLeafw2, prmLeafl2, prmLeaf3, prmvLeafw3, prmLeafl3, prmLeaf4, prmLeafw4, prmLeafl4, prmSpecdscr1, prmSpecdscr2, prmSpecdscr3, prmSpecdscr4, prmSpecdscr5, prmSpecdscr6, prmAdder1, prmAdder2, prmAdder3, prmAdder4, prmAdder5, prmAdder6, prmAdder7, prmAdder8, prmAdder9, prmAdder10, prmAdder11, prmAdder12, prmSpecno1, prmSpecno2, prmSpecno3, prmSpecno4, prmSpecno5, prmSpecno6, prmLeafdscr, prmLeafdscr2, prmLeafdscr3, prmLeafdscr4, prmSpecQty, prmSpecQty2, prmSpecQty3, vSpecQty4, prmSpecQty5, prmSpecQty6, ref dsRfqMaterial, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsRfqMaterial;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsRfqPrintingDataSet RfqPrinting(string prmComp, string prmUser, string prmAction, Int32 prmRfqNo, Int32 RfqSeq, Int32 prmPcol, Int32 prmPass, Int32 prmCoat, string prmColdscr, Int32 prmIps1, Int32 prmIps2, Int32 prmIps3, Int32 prmIps4, Int32 prmIps5, Int32 prmIps6, Int32 prmIps7, Int32 prmvIps8, Int32 prmIps9, Int32 prmIps10, string prmIcode1, string prmIcode2, string prmIcode3, string prmIcode4, string prmIcode5, string prmIcode6, string prmIcode7, string prmIcode8, string prmIcode9, string prmIcode10, string prmCdscr1, string prmCdscr2, string prmCdscr3, string prmCdscr4, string prmCdscr5, string prmCdscr6, string prmCdscr7, string prmCdscr8, string prmCdscr9, string prmCdscr10, Int32 prmIper1, Int32 prmIper2, Int32 prmIper3, Int32 prmIper4, Int32 prmIper5, Int32 prmIper6, Int32 prmIper7, Int32 prmIper8, Int32 prmIper9, Int32 prmIper10, Int64 RfqPRowid)
    {
        string cError = "";
        dsRfqPrintingDataSet dsRfqPrinting = new dsRfqPrintingDataSet();
        dsRfqPrinting = null;
        AppServerConnect();
        aoObject.RfqPrinting(prmComp, prmUser, prmAction, prmRfqNo, RfqSeq, prmPcol, prmPass, prmCoat, prmColdscr, prmIps1, prmIps2, prmIps3, prmIps4, prmIps5, prmIps6, prmIps7, prmvIps8, prmIps9, prmIps10, prmIcode1, prmIcode2, prmIcode3, prmIcode4, prmIcode5, prmIcode6, prmIcode7, prmIcode8, prmIcode9, prmIcode10, prmCdscr1, prmCdscr2, prmCdscr3, prmCdscr4, prmCdscr5, prmCdscr6, prmCdscr7, prmCdscr8, prmCdscr9, prmCdscr10, prmIper1, prmIper2, prmIper3, prmIper4, prmIper5, prmIper6, prmIper7, prmIper8, prmIper9, prmIper10, RfqPRowid, ref dsRfqPrinting, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsRfqPrinting;


    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsRfqShippingDataSet RfqShipping(string prmComp, string prmUser, string prmAction, Int32 prmRfqNo, Int32 RfqSeq, string prmShipid, string prmName, string prmCarrier, string prmdscr, string prmCasno, string prmTrno, decimal prmWeight, decimal prmCasCost, decimal prmTrCost, Int32 prmCascnt, Int32 prmTrcnt, decimal prmCaslen, decimal prmTrlen, decimal prmCasWid, decimal prmTrwid, decimal prmCasdep, decimal prTrdep, Int32 prmCaspal, Int32 prmTrcas, decimal prmCaswt, Int64 RfqSRowid)
    {
        string cError = "";
        dsRfqShippingDataSet dsRfqShipping = new dsRfqShippingDataSet();
        dsRfqShipping = null;
        AppServerConnect();
        aoObject.RfqShip(prmComp, prmUser, prmAction, prmRfqNo, RfqSeq, prmShipid, prmName, prmCarrier, prmdscr, prmCasno, prmTrno, prmWeight, prmCasCost, prmTrCost, prmCascnt, prmTrcnt, prmCaslen, prmTrlen, prmCasWid, prmTrwid, prmCasdep, prTrdep, prmCaspal, prmTrcas, prmCaswt, RfqSRowid, ref dsRfqShipping, out cError);

        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsRfqShipping;


    }
    //public bool Custype(string prmAction, string vCompany, string vCustype, string vDscr, decimal vComm, decimal vDiscount)
    //{
    //    AppServerConnect();
    //    aoObject.Custype(prmAction, vCompany, vCustype, vDscr, vComm, vDiscount);
    //    AppServerDisconnect();
    //    return false;
    //}
    

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsRfqEstimateDataSet RfqEstimate(string prmComp, string prmLoc, string prmUser, string prmAction, Int32 prmRfqNo, string prmEstType, string prmEstNew, string prmSeqList)
    {        

        dsRfqEstimateDataSet dsRfqEstimate = new dsRfqEstimateDataSet();
        dsRfqEstimate = null;
        string cError;
        string vMailto = null;
        string vSubject = null;
        string vBody = null;
        string vMailFrom = null;
        string vInternalUser = null;

        AppServerConnect();
        aoObject.RfqEstimate(prmComp, prmLoc, prmUser, prmAction, prmRfqNo, prmEstType, prmEstNew, prmSeqList, ref dsRfqEstimate, out cError, out vMailto, out vSubject, out vBody, out vInternalUser);
        AppServerDisconnect();

        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");
            HttpContext.Current.Response.Write("<script>window.location.href='rfq_estimate.aspx'</script>");

        }
              
        if (vMailto != "")
        {

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            conn.Open();
            string cmd = "select email_from, subject from email_alerts where program_name = 'Request for Quote'";
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
        return dsRfqEstimate;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsListRfqitemDataSet SelectRfqitem(string prmComp, string prmUser, string prmAction, Int32 prmRfqNo, Int32 RfqSeqNo)
    {

        dsListRfqitemDataSet dsListRfqitem = new dsListRfqitemDataSet();
        dsListRfqitem = null;
        AppServerConnect();
        aoObject.ListRfqitem(prmComp, prmUser, prmAction, prmRfqNo, RfqSeqNo, ref dsListRfqitem);
        AppServerDisconnect();

        return dsListRfqitem;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public String RfqMail(string prmUser, string prmAction, Int32 prmRfqNo, string prmComp)
    {

        //add a new DataSet that returns e-mail messages

        string vMailto = null;
        string vMailFrom = null;
        string vSubject = null;
        string vBody = null;
        string vInternalUser = null;

        AppServerConnect();

        ////return DataSet full of e-mail messages
        aoObject.RfqMail(prmUser, prmAction, prmRfqNo, prmComp, out vMailto, out vMailFrom, out vSubject, out vBody, out vInternalUser);
        AppServerDisconnect();

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        conn.Open();
        string cmd = "";
        if (vInternalUser == "no")
            cmd = "select email_from, subject from email_alerts where program_name = 'Add Rfq'";
        if (vInternalUser == "yes")
            cmd = "select email_from, subject from email_alerts where program_name = 'E Add Rfq'";
        
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
        { }
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
    //public dsvalboardDataSet SelectValBoard(decimal RfqBoard, string prmComp)
    //{
    //    string cError = "";
    //    dsvalboardDataSet dsvalboard = new dsvalboardDataSet();
    //    dsvalboard = null;
    //    AppServerConnect();
    //    aoObject.ValBoard(RfqBoard, prmUser, ref dsvalboard, out cError);
    //    AppServerDisconnect();
    //    if (cError != "")
    //    {
    //        HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
    //        HttpContext.Current.Response.Write("<script>history.back()</script>");

    //    }
    //    return dsvalboard;
    //}
}


