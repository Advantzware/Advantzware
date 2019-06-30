
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class load_recr_journal : System.Web.UI.Page
{

   protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Session["Rowuser"] = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "Recr_gljrn_list.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;

            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;
                if (Session["load_general_ledger_jrnl"] != null)
                    jurl_TextBox.Text = Convert.ToString(Session["load_general_ledger_jrnl"]);
                if (Session["load_general_ledger_freq"] != null)
                    frenq_TextBox.Text = Convert.ToString(Session["load_general_ledger_freq"]);
                
            }
        } //  ! Page.IsPostBack


        try
        {
            if (Session["load_journal_index1"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["load_journal_reckey_rec"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
            }
        }
        catch
        {
            
        }
        try
        {
            if (Session["load_journal_index1"] != null)
            {
                GridView1.SelectedIndex = Convert.ToInt32(Session["load_journal_index1"]);
                Session["load_journal_reckey_rec"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
            }
        }
        catch
        {
            
        }
        

        try
        {
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {           
        }
        

    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }



    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);
        GridView1.PageSize = Convert.ToInt32(Session["gridsize"]);
    }


    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["load_journal_index1"] = GridView1.SelectedIndex;
        Session["load_journal_reckey_rec"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
        LoadButton.Visible = true;
    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        Session["load_general_ledger_jrnl"] = jurl_TextBox.Text;
        Session["load_general_ledger_freq"] = frenq_TextBox.Text.Trim();
        
       
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmjrn_no"].DefaultValue = jurl_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcb_freq"].DefaultValue = frenq_TextBox.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmPosted"].DefaultValue = Convert.ToString(Session["cust_invoice_posted"]);

        Session["load_journal_index1"] = null;
    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        jurl_TextBox.Text = "";
        frenq_TextBox.Text = "";

        Session["load_general_ledger_jrnl"] = jurl_TextBox.Text;
        Session["load_general_ledger_freq"] = frenq_TextBox.Text.Trim();
        
        
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmjrn_no"].DefaultValue = jurl_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcb_freq"].DefaultValue = frenq_TextBox.Text.Trim();
       // ObjectDataSource1.SelectParameters["prmPosted"].DefaultValue = Convert.ToString(Session["vendor_invoice_posted"]);
        Session["load_journal_index1"] = null;
    }
    protected void lnk_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_recr_gljrn.aspx");
    }
    protected void load_viewjournals_Click(object sender, EventArgs e)
    {
        Response.Redirect("load_recr_journal.aspx");
    }
    protected void lnk_list_click(object sender, EventArgs e)
    {
        Response.Redirect("recr_gljrn_list.aspx");
    }

     protected void img_btn_exit_click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }

     protected void img_btn_add_click(object sender, EventArgs e)
     {
         Session["receapp_invoice_add_button"] = "add";
         Response.Redirect("view_gljrn_list.aspx");

     }

     protected void loadButton_Click(object sender, EventArgs e)
     {
         if (HiddenField2.Value == "Yes")
         {
            UserClass UserLogin = (UserClass)Session["User"];

            try
            {
                ledger con = new ledger();
                DataSet ds = new DataSet();
               // ds = con.RecrGeneralLedgerlist("LoadRecr", "", UserLogin.UserName, 0, Convert.ToString(HiddenField1.Value) , 0, 0, 0, 0, "", "", "", "", Convert.ToString(Session["load_journal_reckey_rec"]));
            }
            catch { }
         }

         
         //Response.Write("<script>window.location.href='load_recr_apinv.aspx'</script>");
     }
    

}
