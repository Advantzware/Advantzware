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

public partial class BrowseQuoteaspx : System.Web.UI.Page
{
    protected void page_PreRender(object sender, EventArgs e)
    {
      

    }
    protected void Page_Load(object sender, EventArgs e)
    {
        Session["qty_quote_grid1_index"] = null;
        Session["viewQuote_formview2_index"] = null;
        Session["miscprpquote_grid1_index"] = null;
        Session["list_rfq_rfq_nos"] = null;
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["list_rfq_rfq_nos"] == null)
        {
            LinkButton bquote = (LinkButton)Master.FindControl("ImageButton1");
            bquote.Visible = false;
            HtmlGenericControl rfqlist = (HtmlGenericControl)this.Page.Master.FindControl("rfqlist");
            rfqlist.Attributes.Add("style", "display:none");
        }
        if(!Page.IsPostBack)
        {
            if (Session["prmEstimate_quote_list"] != null)
            {
                txt_est.Text = Convert.ToString(Session["prmEstimate_quote_list"]);
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
            }  
        }
        
        if (Session["User"] != null)
        {            
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
                Session["MiscprpQuote_main_qty"] = ((Label)GridView1.SelectedRow.FindControl("LabelQty")).Text;
                Session["viewquote_qty_lineno"] = ((Label)GridView1.SelectedRow.FindControl("Labeline")).Text;
            }
            
        }
        catch
        {

        }

        
        
        /*ImageButton browsquote = (ImageButton)Master.FindControl("list_quote");
        browsquote.ImageUrl = "~/Images/browsequote1.jpg";*/
    }
    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["brows_quote_index"] = GridView1.SelectedIndex;
        Session["quote_no"] = GridView1.SelectedRow.Cells[1].Text;
        Session["MiscprpQuote_main_qty"] = ((Label)GridView1.SelectedRow.FindControl("LabelQty")).Text;
        Session["viewquote_qty_lineno"] = ((Label)GridView1.SelectedRow.FindControl("Labeline")).Text;
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

        Session["prmAction2_quote_list"] = "Search";
        Session["prmQuote_quote_list"] = txt_quote.Text.Trim();
        Session["prmDate_quote_list"] = txt_fdate.Text.Trim();
        Session["prmCustomer_quote_list"] = txt_cust.Text.Trim();
        Session["prmContact_quote_list"] = txt_contact.Text.Trim();
        Session["prmEstimate_quote_list"] = txt_est.Text.Trim();
        Session["prmRfq_quote_list"] = txt_rfq.Text.Trim();

        Session["prmPart_quote_list"] = txt_part.Text.Trim();
        

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

        Session["prmAction2_quote_list"] = "select";
        Session["prmQuote_quote_list"] = txt_quote.Text.Trim();
        Session["prmDate_quote_list"] = txt_fdate.Text.Trim();
        Session["prmCustomer_quote_list"] = txt_cust.Text.Trim();
        Session["prmContact_quote_list"] = txt_contact.Text.Trim();
        Session["prmEstimate_quote_list"] = txt_est.Text.Trim();
        Session["prmRfq_quote_list"] = txt_rfq.Text.Trim();

        Session["prmPart_quote_list"] = txt_part.Text.Trim();
        //Page_Load(sender, e);
    }
}
