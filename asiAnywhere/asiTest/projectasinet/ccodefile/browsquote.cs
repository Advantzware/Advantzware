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
using System.Net;

/// <summary>
/// Summary description for browsinvoice
/// </summary>
[System.ComponentModel.DataObject]
public class browsquote : AppServerConnect.AppServer
{
    public browsquote()
    {
        //
        // TODO: Add constructor logic here
        //
    }
  
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsBrowseQteDataSet SelectBrowsQuote(string prmUser, string prmAction, Int32 prmQuote, DateTime prmDate, string prmCustomer, string prmContact, string prmEstimate, string prmRfq, string prmPart)
    {

        dsBrowseQteDataSet dsBrowseQte = new dsBrowseQteDataSet();
        dsBrowseQte = null;
        AppServerConnect();
        aoObject.BrwsQuote(prmUser, prmAction, prmQuote, prmDate, prmCustomer, prmContact, prmEstimate, prmRfq, prmPart, ref dsBrowseQte);
        AppServerDisconnect();

        return dsBrowseQte;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsBrwsRfqQuoteDataSet SelectRfqBrowsQuote(string prmUser, string prmAction, Int32 prmQuote, DateTime prmDate, string prmCustomer, string prmContact, string prmEstimate, string prmRfq, string prmPart)
    {

        dsBrwsRfqQuoteDataSet dsBrowseRfqQte = new dsBrwsRfqQuoteDataSet();
        dsBrowseRfqQte = null;
        AppServerConnect();
        aoObject.BrwsRfqQuote(prmUser, prmAction, prmQuote, prmDate, prmCustomer, prmContact, prmEstimate, prmRfq, prmPart, ref dsBrowseRfqQte);
        AppServerDisconnect();

        return dsBrowseRfqQte;
    }
    
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsListQuotesDataSet SelectListQuotes(string prmUser, string prmAction, Int32 prmQuote)
    {

        dsListQuotesDataSet dsListQuotes = new dsListQuotesDataSet();
        dsListQuotes = null;
        AppServerConnect();
        aoObject.ListQuotes(prmUser, prmAction, prmQuote, ref dsListQuotes);
        AppServerDisconnect();

        return dsListQuotes;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsViewQuoteDataSet SelectViewQuotes(string prmUser, string prmAction, Int32 prmQuote, DateTime PrmDate, string prmCust, string prmEst, string prmRfq, DateTime prmDel, string prmBill, string prmBill2, string prmBill3, string prmBill4, string prmShipid, string prmShip, string prmShip2, string prmShip3, string prmShip4, string PrmContact, string prmSoldid, string prmSold, string prmSold2, string prmSold3, string prmSold4, string prmSman, string prmTerms, string prmCarr, string prmZone, string prmStat, string prmPart, string prmCarrdscr, string prmTermDscr, string prmSName, string prmZondesc)
    {
        string cError = "";
        dsViewQuoteDataSet dsViewQuote = new dsViewQuoteDataSet();
        dsViewQuote = null;
        AppServerConnect();
        aoObject.ViewQuote(prmUser, prmAction, prmQuote, PrmDate, prmCust, prmEst, prmRfq, prmDel, prmBill, prmBill2, prmBill3, prmBill4, prmShipid, prmShip, prmShip2, prmShip3, prmShip4, PrmContact, prmSoldid, prmSold, prmSold2, prmSold3, prmSold4, prmSman, prmTerms, prmCarr, prmZone, prmStat, prmPart, prmCarrdscr, prmTermDscr, prmSName, prmZondesc, ref dsViewQuote, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }

        return dsViewQuote;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateViewQuotes(string prmUser, string prmAction, Int32 prmQuote, DateTime PrmDate, string prmCust, string prmEst, string prmRfq, DateTime prmDel, string prmBill, string prmBill2, string prmBill3, string prmBill4, string prmShipid, string prmShip, string prmShip2, string prmShip3, string prmShip4, string PrmContact, string prmSoldid, string prmSold, string prmSold2, string prmSold3, string prmSold4, string prmSman, string prmTerms, string prmCarr, string prmZone, string prmStat, string prmPart, string prmCarrdscr, string prmTermDscr, string prmSName, string prmZondesc)
    {
        string cError = "";
        dsViewQuoteDataSet dsViewQuote = new dsViewQuoteDataSet();
        dsViewQuote = null;
        AppServerConnect();
        aoObject.ViewQuote(prmUser, prmAction, prmQuote, PrmDate, prmCust, prmEst, prmRfq, prmDel, prmBill, prmBill2, prmBill3, prmBill4, prmShipid, prmShip, prmShip2, prmShip3, prmShip4, PrmContact, prmSoldid, prmSold, prmSold2, prmSold3, prmSold4, prmSman, prmTerms, prmCarr, prmZone, prmStat, prmPart, prmCarrdscr, prmTermDscr, prmSName, prmZondesc, ref dsViewQuote, out cError);
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
    public dsQuotePrint1DataSet SelectPrintQuote(string prmAction, Int32 prmQuote, string prmUser)
    {

        dsQuotePrint1DataSet dsQuotePrint1 = new dsQuotePrint1DataSet();
        dsQuotePrint1 = null;
        string vFile = "";
        string vMailto = "";
        string vBody = "";
        string vMailFrom = null;
        string vSubject = null;
        string vInternalUser = null;

        AppServerConnect();
        aoObject.PrintQuote(prmAction, prmQuote, prmUser, ref dsQuotePrint1, out vFile, out vMailto, out vBody, out vInternalUser);
        AppServerDisconnect();

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        conn.Open();
        string cmd = "select email_from, subject from email_alerts where program_name = 'Print Quote'";
        string cmd2 = "select email from user_master where Username = '" + prmUser + "'";
        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
        SqlDataAdapter da2 = new SqlDataAdapter(cmd2, conn);
        DataSet ds = new DataSet();
        da.Fill(ds);
        DataSet ds2 = new DataSet();
        da2.Fill(ds2);
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
        
        return dsQuotePrint1;

    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsQuotePrint1DataSet SelectPrintQuoteMail(string prmAction, Int32 prmQuote, string prmUser, out string oFile, out string oMailto, out string oBody)
    {

        dsQuotePrint1DataSet dsQuotePrint1 = new dsQuotePrint1DataSet();
        dsQuotePrint1 = null;
        string vFile = "";
        string vMailto = "";
        string vBody = "";
        string vMailFrom = null;
        string vSubject = null;
        string vInternalUser = null;

        AppServerConnect();
        aoObject.PrintQuote(prmAction, prmQuote, prmUser, ref dsQuotePrint1, out vFile, out vMailto, out vBody, out vInternalUser);
        AppServerDisconnect();
        oFile = vFile;
        oMailto = vMailto;
        oBody = vBody;

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        conn.Open();
        string cmd = "select email_from, subject from email_alerts where program_name = 'Print Quote'";
        string cmd2 = "select email from user_master where Username = '" + prmUser + "'";
        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
        SqlDataAdapter da2 = new SqlDataAdapter(cmd2, conn);
        DataSet ds = new DataSet();
        da.Fill(ds);
        DataSet ds2 = new DataSet();
        da2.Fill(ds2);
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
                //if (vInternalUser == "yes")
                //{

                    MailMessage om = new MailMessage();  
                    om.To.Add(vMailto);
                    om.From = new MailAddress(vMailFrom);
                    om.Subject = vSubject;
                    om.Body = vBody;
                    om.IsBodyHtml = true;
                    Attachment attachment = new Attachment("C:/inetpub/wwwroot/pdfs/" + vFile);
                    om.Attachments.Add(attachment);
                    om.Priority = MailPriority.High;
                    SmtpClient smpt = new SmtpClient();
                    smpt.Send(om);

                    
               // }
            }
            catch { }
        }

        return dsQuotePrint1;

    }   

}

