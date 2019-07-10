using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

using Outlook = Microsoft.Office.Interop.Outlook;
using System.Collections.Generic;
using System.Data.SqlClient;

using Microsoft.Office.Interop.Outlook;
using OutlookApp = Microsoft.Office.Interop.Outlook.Application;
 



public partial class ViewQuote : System.Web.UI.Page
{
    protected void Page_PreRender(object sender, EventArgs e)
    {
        try
        {
            if (Session["list_rfq_rfq_nos"] != null)
            {
                LinkButton b2quote = (LinkButton)Master.FindControl("list_quote");
                b2quote.Visible = false;
                HtmlGenericControl rfqlist = (HtmlGenericControl)this.Page.Master.FindControl("listquoteh");
                rfqlist.Attributes.Add("style", "display:none");
            }
        }
        catch
        {
        }
    }

    protected void Page_Load(object sender, EventArgs e)
    {
        Session["qty_quote_grid1_index"] = null;
        Session["miscprpquote_grid1_index"] = null;
        if (Session["list_rfq_rfq_nos"] == null)
        {
            try
            {
                LinkButton bquote = (LinkButton)Master.FindControl("ImageButton1");
                bquote.Visible = false;
                HtmlGenericControl rfqlist = (HtmlGenericControl)this.Page.Master.FindControl("rfqlist");
                rfqlist.Attributes.Add("style", "display:none");
                Hyperlink1.Visible = false;
            }
            catch { }
        }

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "";
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
            {                
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                              
                string vUserId = UserLogin.UserName;
                string vPage = "ViewQuote.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                //lblComp.Text = PrmComp;
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");

                }
            }
            //Label name = (Label)Master.FindControl("lbl_page");
            //name.Text = "View Job";
        if (!Page.IsPostBack)
        {
            GridView1.Visible = true;
            FormView2.Visible = true;
            if (Session["viewQuote_formview2_index"] != null)
                GridView1.SelectedIndex = Convert.ToInt32(Session["viewQuote_formview2_index"]);
            else
                GridView1.SelectedIndex = 0;
        }
           /* ImageButton viewquote = (ImageButton)Master.FindControl("view_quote");
            viewquote.ImageUrl = "~/Images/viewquote1.jpg";*/
        }

    protected void UpdateButton_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        Label quote = (Label)FormView1.FindControl("vQuoteTextBox");
        TextBox date = (TextBox)FormView1.FindControl("vDateTextBox");
        TextBox cust = (TextBox)FormView1.FindControl("vCustTextBox");
        TextBox est = (TextBox)FormView1.FindControl("vEstimateTextBox");
        TextBox rfq = (TextBox)FormView1.FindControl("vRfqTextBox");
        TextBox del = (TextBox)FormView1.FindControl("vDelDateTextBox");
        TextBox bill = (TextBox)FormView1.FindControl("vBillTextBox");
        TextBox bill2 = (TextBox)FormView1.FindControl("vBill2TextBox");
        TextBox bill3 = (TextBox)FormView1.FindControl("vBill3TextBox");
        TextBox bill4 = (TextBox)FormView1.FindControl("vBill4TextBox");
        TextBox shipid = (TextBox)FormView1.FindControl("vShipidTextBox");
        TextBox ship = (TextBox)FormView1.FindControl("vShipTextBox");
        TextBox ship2 = (TextBox)FormView1.FindControl("vShip2TextBox");
        
        TextBox ship3 = (TextBox)FormView1.FindControl("vShip3TextBox");
        TextBox ship4 = (TextBox)FormView1.FindControl("vShip4TextBox");
        TextBox contact = (TextBox)FormView1.FindControl("vContactTextBox");
        TextBox soldid = (TextBox)FormView1.FindControl("vSoldIdTextBox");
        TextBox sold = (TextBox)FormView1.FindControl("vSoldTextBox");
        TextBox sold2 = (TextBox)FormView1.FindControl("vSold2TextBox");
        TextBox sold3 = (TextBox)FormView1.FindControl("vSold3TextBox");
        TextBox sold4 = (TextBox)FormView1.FindControl("vSold4TextBox");
        TextBox sman = (TextBox)FormView1.FindControl("vSmanTextBox");
        TextBox terms = (TextBox)FormView1.FindControl("vTermsTextBox");
        TextBox carr = (TextBox)FormView1.FindControl("vCarrierTextBox");
        TextBox zone = (TextBox)FormView1.FindControl("vDelZoneTextBox");
        Label stat = (Label)FormView1.FindControl("vStatTextBox");
        TextBox sname = (TextBox)FormView1.FindControl("vSnameTextBox");
        TextBox termdscr = (TextBox)FormView1.FindControl("vTermDscrTextBox");
        TextBox carrdscr = (TextBox)FormView1.FindControl("vCarrdscrTextBox");
        TextBox zdscr = (TextBox)FormView1.FindControl("vZondescTextBox");

       

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmQuote"].DefaultValue = quote.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDate"].DefaultValue = date.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = cust.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEst"].DefaultValue = est.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRfq"].DefaultValue = rfq.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDel"].DefaultValue = del.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBill"].DefaultValue = bill.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBill2"].DefaultValue = bill2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBill3"].DefaultValue = bill3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBill4"].DefaultValue = bill4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmShipid"].DefaultValue = shipid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmShip"].DefaultValue = ship.Text.Trim();
        ObjectDataSource1.SelectParameters["prmShip2"].DefaultValue = ship2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmShip3"].DefaultValue = ship3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmShip4"].DefaultValue = ship4.Text.Trim();
        ObjectDataSource1.SelectParameters["PrmContact"].DefaultValue = contact.Text.Trim();

        ObjectDataSource1.SelectParameters["prmSoldid"].DefaultValue = soldid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSold"].DefaultValue = sold.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSold2"].DefaultValue = sold2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSold3"].DefaultValue = sold3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSold4"].DefaultValue = sold4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSman"].DefaultValue = sman.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTerms"].DefaultValue = terms.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCarr"].DefaultValue = carr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmZone"].DefaultValue = zone.Text.Trim();
        ObjectDataSource1.SelectParameters["prmStat"].DefaultValue = stat.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmPart"].DefaultValue = upc.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCarrdscr"].DefaultValue = carrdscr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTermDscr"].DefaultValue = termdscr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSName"].DefaultValue = sname.Text.Trim();
        ObjectDataSource1.SelectParameters["prmZondesc"].DefaultValue = zdscr.Text.Trim();
        FormView1.ChangeMode(FormViewMode.ReadOnly);

    }
    protected void Add_viewquote(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

       
        TextBox date = (TextBox)FormView1.FindControl("vDateTextBox");
        TextBox cust = (TextBox)FormView1.FindControl("vCustTextBox");
        TextBox est = (TextBox)FormView1.FindControl("vEstimateTextBox");
        TextBox rfq = (TextBox)FormView1.FindControl("vRfqTextBox");
        TextBox del = (TextBox)FormView1.FindControl("vDelDateTextBox");
        TextBox bill = (TextBox)FormView1.FindControl("vBillTextBox");
        TextBox bill2 = (TextBox)FormView1.FindControl("vBill2TextBox");
        TextBox bill3 = (TextBox)FormView1.FindControl("vBill3TextBox");
        TextBox bill4 = (TextBox)FormView1.FindControl("vBill4TextBox");
        TextBox shipid = (TextBox)FormView1.FindControl("vShipidTextBox");
        TextBox ship = (TextBox)FormView1.FindControl("vShipTextBox");
        TextBox ship2 = (TextBox)FormView1.FindControl("vShip2TextBox");

        TextBox ship3 = (TextBox)FormView1.FindControl("vShip3TextBox");
        TextBox ship4 = (TextBox)FormView1.FindControl("vShip4TextBox");
        TextBox contact = (TextBox)FormView1.FindControl("vContactTextBox");
        TextBox soldid = (TextBox)FormView1.FindControl("vSoldIdTextBox");
        TextBox sold = (TextBox)FormView1.FindControl("vSoldTextBox");
        TextBox sold2 = (TextBox)FormView1.FindControl("vSold2TextBox");
        TextBox sold3 = (TextBox)FormView1.FindControl("vSold3TextBox");
        TextBox sold4 = (TextBox)FormView1.FindControl("vSold4TextBox");
        TextBox sman = (TextBox)FormView1.FindControl("vSmanTextBox");
        TextBox terms = (TextBox)FormView1.FindControl("vTermsTextBox");
        TextBox carr = (TextBox)FormView1.FindControl("vCarrierTextBox");
        TextBox zone = (TextBox)FormView1.FindControl("vDelZoneTextBox");
        TextBox sname = (TextBox)FormView1.FindControl("vSnameTextBox");
        TextBox termdscr = (TextBox)FormView1.FindControl("vTermDscrTextBox");
        TextBox carrdscr = (TextBox)FormView1.FindControl("vCarrdscrTextBox");
        TextBox zdscr = (TextBox)FormView1.FindControl("vZondescTextBox");


        browsquote brw = new browsquote();

        bool check = brw.ValidateViewQuotes(UserLogin.UserName, "AddValdate", 0, Convert.ToDateTime("07/07/2010"), cust.Text.Trim(), est.Text.Trim(), rfq.Text.Trim(), Convert.ToDateTime("07/07/2010"), bill.Text.Trim(), bill2.Text.Trim(), bill3.Text.Trim(), bill4.Text.Trim(), shipid.Text.Trim(), ship.Text.Trim(), ship2.Text.Trim(), ship3.Text.Trim(), ship4.Text.Trim(), contact.Text.Trim(), soldid.Text.Trim(), sold.Text.Trim(), sold2.Text.Trim(), sold3.Text.Trim(), sold4.Text.Trim(), sman.Text.Trim(), terms.Text.Trim(), carr.Text.Trim(), zone.Text.Trim(), "", "", "", "", "", "");
              
        string value = Convert.ToString(check);
        if (value == "True")
        {

            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";

            ObjectDataSource1.SelectParameters["prmDate"].DefaultValue = date.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = cust.Text.Trim();
            ObjectDataSource1.SelectParameters["prmEst"].DefaultValue = est.Text.Trim();
            ObjectDataSource1.SelectParameters["prmRfq"].DefaultValue = rfq.Text.Trim();
            ObjectDataSource1.SelectParameters["prmDel"].DefaultValue = del.Text.Trim();
            ObjectDataSource1.SelectParameters["prmBill"].DefaultValue = bill.Text.Trim();
            ObjectDataSource1.SelectParameters["prmBill2"].DefaultValue = bill2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmBill3"].DefaultValue = bill3.Text.Trim();
            ObjectDataSource1.SelectParameters["prmBill4"].DefaultValue = bill4.Text.Trim();
            ObjectDataSource1.SelectParameters["prmShipid"].DefaultValue = shipid.Text.Trim();
            ObjectDataSource1.SelectParameters["prmShip"].DefaultValue = ship.Text.Trim();
            ObjectDataSource1.SelectParameters["prmShip2"].DefaultValue = ship2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmShip3"].DefaultValue = ship3.Text.Trim();
            ObjectDataSource1.SelectParameters["prmShip4"].DefaultValue = ship4.Text.Trim();
            ObjectDataSource1.SelectParameters["PrmContact"].DefaultValue = contact.Text.Trim();

            ObjectDataSource1.SelectParameters["prmSoldid"].DefaultValue = soldid.Text.Trim();
            ObjectDataSource1.SelectParameters["prmSold"].DefaultValue = sold.Text.Trim();
            ObjectDataSource1.SelectParameters["prmSold2"].DefaultValue = sold2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmSold3"].DefaultValue = sold3.Text.Trim();
            ObjectDataSource1.SelectParameters["prmSold4"].DefaultValue = sold4.Text.Trim();
            ObjectDataSource1.SelectParameters["prmSman"].DefaultValue = sman.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTerms"].DefaultValue = terms.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCarr"].DefaultValue = carr.Text.Trim();
            ObjectDataSource1.SelectParameters["prmZone"].DefaultValue = zone.Text.Trim();


            ObjectDataSource1.SelectParameters["prmCarrdscr"].DefaultValue = carrdscr.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTermDscr"].DefaultValue = termdscr.Text.Trim();
            ObjectDataSource1.SelectParameters["prmSName"].DefaultValue = sname.Text.Trim();
            ObjectDataSource1.SelectParameters["prmZondesc"].DefaultValue = zdscr.Text.Trim();
            
            FormView1.ChangeMode(FormViewMode.ReadOnly);
             Response.Write("<script>window.open('quote_item_popup.aspx','QuoteLookupWindow','width=500,height=500, scrollbars=1, toolbars=1,statusbar=1,resizeable=1');</script>");
        }
        
    }

    protected void PrintButtonClick(object sender, EventArgs e)
    {
        if (!Request.Browser.Browser.Contains("Safari"))
            Response.Write("<script>window.open('PrintQuote.aspx'); target='_blank'</script>");
        else
            Response.Redirect("BrowseQuote.aspx"); 
    }

    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["viewQuote_formview2_index"] = GridView1.SelectedIndex;
    }
    protected void FormView1_ondatabound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            GridView1.Visible = false;
            FormView2.Visible = false;
            TextBox date = (TextBox)FormView1.FindControl("vDateTextBox");
            date.Text = Convert.ToString( System.DateTime.Now.Date.ToShortDateString());
            date.Focus();
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox date = (TextBox)FormView1.FindControl("vDateTextBox");
            date.Focus();
        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
          try
            {
                Label sql = (Label)FormView1.FindControl("vQuoteLabel");
                Session["quote_no"] = sql.Text;
            }
            catch { }
        }
    }
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        
    }
    protected void delete_button_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
    }

    protected void Formview2_onbatabound(object sender, EventArgs e)
    {
        if (FormView2.CurrentMode == FormViewMode.Edit)
        {
            AddNewButton.Visible = false;
            TextBox desc = (TextBox)FormView2.FindControl("vPartDscr1TextBox");
            desc.Focus();
        }
        if (FormView2.CurrentMode == FormViewMode.Insert)
        {
            AddNewButton.Visible = false;
            TextBox desc = (TextBox)FormView2.FindControl("vPartNoTextBox");
            desc.Focus();
        }
        if (FormView2.CurrentMode == FormViewMode.ReadOnly)
        {

            Label line = (Label)FormView2.FindControl("vLineLabel");
            Label qty = (Label)FormView2.FindControl("vQtyLabel");
            try
            {
                Session["MiscprpQuote_main_qty"] = qty.Text.Trim();
                Session["viewquote_qty_lineno"] = line.Text;
            }
            catch { }
            if (GridView1.Rows.Count > 0)
                AddNewButton.Visible = false;
            else
                AddNewButton.Visible = true;
        }
    }

    protected void Formview2_update_button_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox desc = (TextBox)FormView2.FindControl("vPartDscr1TextBox");
        TextBox desc2 = (TextBox)FormView2.FindControl("vPartDscr2TextBox");
        TextBox size = (TextBox)FormView2.FindControl("vSizeTextBox");
        TextBox board = (TextBox)FormView2.FindControl("vIdscrTextBox");
        TextBox col = (TextBox)FormView2.FindControl("vIcoldscrTextBox");
              

        ObjectDataSource_item.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource_item.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource_item.SelectParameters["prmPartDscr1"].DefaultValue = desc.Text.Trim();
        ObjectDataSource_item.SelectParameters["prmPartDscr2"].DefaultValue = desc2.Text.Trim();
        ObjectDataSource_item.SelectParameters["prmDimensions"].DefaultValue = size.Text.Trim();
        ObjectDataSource_item.SelectParameters["prmBoard"].DefaultValue = board.Text.Trim();
        ObjectDataSource_item.SelectParameters["prmColor"].DefaultValue = col.Text.Trim();

        FormView2.ChangeMode(FormViewMode.ReadOnly);
        Response.Write("<script>window.location.href='ViewQuote.aspx';</script>");

    }

    protected void Formview2_insert_button_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox part = (TextBox)FormView2.FindControl("vPartNoTextBox");
        TextBox item = (TextBox)FormView2.FindControl("vInoTextBox");
        TextBox desc = (TextBox)FormView2.FindControl("vPartDscr1TextBox");
        TextBox desc2 = (TextBox)FormView2.FindControl("vPartDscr2TextBox");
        TextBox style = (TextBox)FormView2.FindControl("vStyleTextBox");

        TextBox qty = (TextBox)FormView2.FindControl("vQtyTextBox");
        TextBox price = (TextBox)FormView2.FindControl("vPriceTextBox");
        TextBox uom = (TextBox)FormView2.FindControl("vUomTextBox");
        TextBox size = (TextBox)FormView2.FindControl("vSizeTextBox");
        TextBox board = (TextBox)FormView2.FindControl("vIdscrTextBox");
        TextBox col = (TextBox)FormView2.FindControl("vIcoldscrTextBox");
        

        QuoteDetail quo = new QuoteDetail();
        bool check = quo.SelectQuoteItemsValidate(UserLogin.UserName, "AddValdate", Convert.ToInt32(Session["quote_no"]), part.Text.Trim(), item.Text.Trim(), desc.Text.Trim(), desc2.Text.Trim(), 0, 0, uom.Text.Trim(), "", "", "", 0, "");
              
        string value = Convert.ToString(check);
        if (value == "True")
        {

            ObjectDataSource_item.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource_item.SelectParameters["prmAction"].DefaultValue = "Add";
            ObjectDataSource_item.SelectParameters["prmPartNo"].DefaultValue = part.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmItemNo"].DefaultValue = HiddenField1.Value;
            ObjectDataSource_item.SelectParameters["prmPartDscr1"].DefaultValue = desc.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmPartDscr2"].DefaultValue = desc2.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmStyle"].DefaultValue = style.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmQty"].DefaultValue = qty.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmPrice"].DefaultValue = price.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmUom"].DefaultValue = uom.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmDimensions"].DefaultValue = size.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmBoard"].DefaultValue = board.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmColor"].DefaultValue = col.Text.Trim();

            FormView2.ChangeMode(FormViewMode.ReadOnly);
            Response.Write("<script>window.location.href='ViewQuote.aspx';</script>");
        }
    }

    protected void Formview2_deletebutton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource_item.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource_item.SelectParameters["prmAction"].DefaultValue = "Delete";
        Session["viewQuote_formview2_index"] = null;
        Response.Write("<script>window.location.href='ViewQuote.aspx';</script>");
    }
    protected void AddNewButton_Click(object sender1, EventArgs e)
    {
        FormView2.ChangeMode(FormViewMode.Insert);
    }

    protected void formview1_cancel_click(object sender, EventArgs e)
    {
        GridView1.Visible = true;
        FormView2.Visible = true;
    }
    protected void EmailButtonClick(object sender, EventArgs e)
    {
        string vFiles = "";
        string vMailto = "";
        string vBody = "";
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        browsquote browsquote = new browsquote();
        DataSet ds_brows = new DataSet();
        ds_brows = browsquote.SelectPrintQuoteMail("PrintQuote", Convert.ToInt32(Session["quote_no"]), UserLogin.UserName, out vFiles,out vMailto, out vBody );
        if (vFiles != "")
        {
            string path2 = @"/pdfs/" + vFiles;
            Response.Write(path2);
            if (!Request.Browser.Browser.Contains("Safari"))
                Response.Write("<script>window.open('PrintReport.aspx?file=" + vFiles + "'); target='_blank'</script>");
           // Response.Redirect(path2);
        }
    //    Outlook.Application objApp = new Outlook.Application();
    //    Outlook.MailItem mail = null;
    //    mail = (Outlook.MailItem)objApp.CreateItem(Outlook.OlItemType.olMailItem);
    //    //The CreateItem method returns an object which has to be typecast to MailItem 
    //    //before using it.
    //    mail.Attachments.Add((object)@"C:\tmp\Account.xml",
    //   Outlook.OlAttachmentType.olEmbeddeditem,
    //1, (object)"Attachment");
    //    //The parameters are explained below
    //    mail.To = "me@abc.com;test@def.com";
    //    mail.Display(true);        
    }


    
}

