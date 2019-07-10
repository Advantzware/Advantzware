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

public partial class MasterPage6 : System.Web.UI.MasterPage
{
    protected void Page_Load(object sender, EventArgs e)
    {
        string fname = this.Page.GetType().Name.ToString();

        if (fname == "rfq_browsequote_aspx")
        {
            rfqlist.Attributes.Add("class", "selected");
        }
        else if (fname == "browsequote_aspx")
        {
            listquoteh.Attributes.Add("class", "selected");
        }
        else if (fname == "viewquote_aspx")
        {
            viewquoteh.Attributes.Add("class", "selected");
        }
        else if (fname == "qty_quote_aspx")
        {
            qtyheadh.Attributes.Add("class", "selected");
        }
        else if (fname == "miscprpquote_aspx")
        {
            prepmisch.Attributes.Add("class", "selected");
        }
        else 
        {
            noteshead.Attributes.Add("class", "selected");
        }
        

        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;


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

                lblComp.Text = PrmComp;
                //Response.Write(vCanRun);
                if (vCanRun == true)
                {
                    list_quote.Visible = true;
                    //list_rfqs.Visible = true;

                }

                if (vCanRun == false)
                {
                    //lnklist_rfqs.Visible = false;
                    list_quote.Visible = false;

                }


                string vUserIdview_quote = UserLogin.UserName;
                string vPageview_quote = "ViewQuote.aspx";
                string aUsersview_quote = null;
                string PrmCompview_quote = null;
                bool vCanCreateview_quote = false;
                bool vCanRunview_quote = false;
                bool vCanUpdateview_quote = false;
                bool vCanDeleteview_quote = false;

                func1 f1view_quote = new func1();
                //Response.Write(Page);
                f1view_quote.CheckProgramPermissions(vPageview_quote, vUserIdview_quote, ref  vCanCreateview_quote, ref  vCanRunview_quote, ref  vCanUpdateview_quote, ref  vCanDeleteview_quote, ref  PrmCompview_quote, ref  aUsersview_quote);

                lblComp.Text = PrmCompview_quote;
                //Response.Write(vCanRun);
                if (vCanRunview_quote == true)
                {
                    //lnkrfq_item.Visible = true;
                    view_quote.Visible = true;

                }

                if (vCanRunview_quote == false)
                {
                    //lnkrfq_item.Visible = false;
                    view_quote.Visible = false;

                }

                string vUserIdprint_quote = UserLogin.UserName;
                string vPageprint_quote = "PrintQuote.aspx";
                string aUsersprint_quote = null;
                string PrmCompprint_quote = null;
                bool vCanCreateprint_quote = false;
                bool vCanRunprint_quote = false;
                bool vCanUpdateprint_quote = false;
                bool vCanDeleteprint_quote = false;

                func1 f1print_quote = new func1();
                //Response.Write(Page);
                f1print_quote.CheckProgramPermissions(vPageprint_quote, vUserIdprint_quote, ref  vCanCreateprint_quote, ref  vCanRunprint_quote, ref  vCanUpdateprint_quote, ref  vCanDeleteprint_quote, ref  PrmCompprint_quote, ref  aUsersprint_quote);

                lblComp.Text = PrmCompprint_quote;
                //Response.Write(vCanRun);
                if (vCanRunprint_quote == true)
                {
                    //lnkprint_quote.Visible = true;

                    //print_quote.Visible = true;

                }

                if (vCanRunprint_quote == false)
                {
                    
                    //print_quote.Visible = false;

                }

                

            }
        }
    }

    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }
    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {
        string sLoginURL =

ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" +"Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void lnk_viewquote_Click(object sender, EventArgs e)
    {
        
    }
   
    
    protected void lnk_viewqte_Click(object sender, EventArgs e)
    {
       
            Response.Redirect("ViewQuote.aspx");

    }
    protected void lnk_quantitiesqte_Click(object sender, EventArgs e)
    {
        Response.Redirect("qty_quote.aspx");
    }
    protected void lnk_miscprp_Click(object sender, EventArgs e)
    {
        Response.Redirect("MiscprpQuote.aspx");
    }
    protected void lnk_quotenotes_Click(object sender, EventArgs e)
    {
        Response.Redirect("QuoteNotes.aspx");
    }
    
    protected void print_quote_Click(object sender, EventArgs e)
    {        
        Response.Redirect("QuotePrint.aspx");
    }
    
    protected void list_quote_Click(object sender, EventArgs e)
    {
       
            Session["brwsquote"] = Session["quote"];
            Response.Redirect("BrowseQuote.aspx");
        
    }
    protected void ImageButton_Click(Object sender, EventArgs e)
    {
        Response.Redirect("rfq_BrowseQuote.aspx");
    }
    
}
