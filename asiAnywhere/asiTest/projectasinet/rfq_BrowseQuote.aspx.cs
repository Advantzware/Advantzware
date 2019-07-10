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

public partial class BrowseQuote : System.Web.UI.Page
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
       
        
        if (Session["User"] != null)
        {
            UserClass UserLogin = (UserClass)Session["User"];
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            //ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

            UserClass.CheckLogin(Page);
            string vUserId = UserLogin.UserName;
            string vPage = "BrowseQuote.aspx";
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

        try
        {
            GridView1.SelectedIndex = Convert.ToInt32(Session["brows_quote_index"]);
            if (Session["brows_quote_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["quote_no"] = GridView1.SelectedRow.Cells[1].Text;
            }
            txt_contact.Attributes.Add("onkeypress", "return clickButton(event, '" + btnSearch.ClientID + "')");
            txt_cust.Attributes.Add("onkeypress", "return clickButton(event, '" + btnSearch.ClientID + "')");
            txt_est.Attributes.Add("onkeypress", "return clickButton(event, '" + btnSearch.ClientID + "')");
            txt_fdate.Attributes.Add("onkeypress", "return clickButton(event, '" + btnSearch.ClientID + "')");
            txt_part.Attributes.Add("onkeypress", "return clickButton(event, '" + btnSearch.ClientID + "')");
            txt_quote.Attributes.Add("onkeypress", "return clickButton(event, '" + btnSearch.ClientID + "')");
            txt_rfq.Attributes.Add("onkeypress", "return clickButton(event, '" + btnSearch.ClientID + "')");
        }
        catch
        {

        }
        
        /*ImageButton browsquote = (ImageButton)Master.FindControl("ImageButton1");
        browsquote.ImageUrl = "~/Images/browsequote1.jpg";*/
    }
    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["brows_quote_index"] = GridView1.SelectedIndex;
        Session["quote_no"] = GridView1.SelectedRow.Cells[1].Text;
    }
    protected void customerval2(object sender, EventArgs e)
    {
        //if (txt_cust.Text.Trim() != "")
        //{
        //    Session["customer_fglookup_val"] = txt_cust.Text;
        //}
        //else
        //{
        //    Session["customer_fglookup_val"] = null;
        //}
    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmQuote"].DefaultValue = txt_quote.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDate"].DefaultValue = txt_fdate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustomer"].DefaultValue = txt_cust.Text.Trim();

        ObjectDataSource1.SelectParameters["prmContact"].DefaultValue = txt_contact.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEstimate"].DefaultValue = txt_est.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRfq"].DefaultValue = txt_rfq.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPart"].DefaultValue = txt_part.Text.Trim();
        
        //Response.Write(openinvoice.Value);
        //Response.Write(paidinvoice.Value);
         
        Session["prmAction2"] = "Search";
        Session["prmQuote"] = txt_quote.Text.Trim();
        Session["prmDate"] = txt_fdate.Text.Trim();
        Session["prmCustomer"] = txt_cust.Text.Trim();
        Session["prmContact"] = txt_contact.Text.Trim();
        Session["prmEstimate"] = txt_est.Text.Trim();
        Session["prmRfq"] = txt_rfq.Text.Trim();
        
        Session["prmPart"] = txt_part.Text.Trim();
        

        //Session["Arindex"] = null;
        
    }
    protected void btn_reset_Click(object sender, EventArgs e)
    {
        string str = "";

        txt_quote.Text = str.ToString();
        txt_fdate.Text = str.ToString();
        txt_cust.Text = str.ToString();

        txt_contact.Text = str.ToString();
        txt_est.Text = str.ToString();
        txt_rfq.Text = str.ToString();
        txt_part.Text = str.ToString();
        ;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "select";
        
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmQuote"].DefaultValue = txt_quote.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDate"].DefaultValue = txt_fdate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustomer"].DefaultValue = txt_cust.Text.Trim();

        ObjectDataSource1.SelectParameters["prmContact"].DefaultValue = txt_contact.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEstimate"].DefaultValue = txt_est.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRfq"].DefaultValue = txt_rfq.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPart"].DefaultValue = txt_part.Text.Trim();

        //Session["prmAction2"] = "select";
        //Session["prmInvoice"] = txt_inv.Text.Trim();
        //Session["prmCustomer"] = txt_cust.Text.Trim();
        //Session["prmItem"] = txt_item.Text.Trim();
        //Session["prmPart"] = txt_part.Text.Trim();
        //Session["prmCustPo"] = txt_po.Text.Trim();
        //Session["prmBOL"] = txt_bol.Text.Trim();
        //Session["prmEstimate"] = txt_est.Text.Trim();
        //Session["prmDate"] = txt_date.Text.Trim();
        //Session["prmOpen"] = openinvoice.Value.Trim();
        //Session["prmPaid"] = paidinvoice.Value.Trim();

        //Session["Arindex"] = null;
        //Session["index2"] = null;
        //Session["item"] = null;
        //Session["line"] = null;
        //Session["index3"] = null;
        //Session["index4"] = null;

        //Page_Load(sender, e);
    }
}
