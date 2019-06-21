
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class rel_ord_shipnote : System.Web.UI.Page
{

   protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Session["Rowuser"] = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "ReleaseOrd_list.aspx";
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
                //lblUser.Text = UserLogin.UserName;
                //if(Session["loadrecr_invoice_vendor"] != null)
                //    vendor_TextBox.Text = Convert.ToString(Session["loadrecr_invoice_vendor"]);
                //if(Session["loadrecr_invoice_freq"] != null)
                //    frequ_TextBox.Text = Convert.ToString(Session["loadrecr_invoice_freq"]);
                
            }
        } //  ! Page.IsPostBack      
        

        
        

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
        //ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);
        //GridView1.PageSize = Convert.ToInt32(Session["gridsize"]);
    }

    protected void FormView1_OnDataBound(object sender, EventArgs e)
    {

        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            

        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox ship1 = (TextBox)FormView1.FindControl("shipnot1TextBox");
            ship1.Focus();            
        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            //try
            //{
                        
            //Label reckey = (Label)FormView1.FindControl("reckeyTextBox");

            //Session["Release_ord_reckey_rec"] = reckey.Text.Trim();
            
            //}
            //catch { }
            

        }
    }

    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        TextBox ship1 = (TextBox)FormView1.FindControl("shipnot1TextBox");
        TextBox ship2 = (TextBox)FormView1.FindControl("shipnot2TextBox");
        TextBox ship3 = (TextBox)FormView1.FindControl("shipnot3TextBox");
        TextBox ship4 = (TextBox)FormView1.FindControl("shipnot4TextBox");
        

        UserClass UserLogin = (UserClass)Session["User"];

        try
        {
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "UpdateNotes";
            ObjectDataSource1.SelectParameters["prmship1"].DefaultValue = ship1.Text.Trim();
            ObjectDataSource1.SelectParameters["prmship2"].DefaultValue = ship2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmship3"].DefaultValue = ship3.Text.Trim();
            ObjectDataSource1.SelectParameters["prmship4"].DefaultValue = ship4.Text.Trim();

            FormView1.ChangeMode(FormViewMode.ReadOnly);
        }
        catch { }
       
    }

    
    protected void lnk_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_release_order.aspx");
    }

    protected void load_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("rel_ord_shipnote.aspx");
    }
    protected void lnk_list_click(object sender, EventArgs e)
    {
        Response.Redirect("ReleaseOrd_list.aspx");
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
         Session["release_ord_add_button"] = "add";
         Response.Redirect("view_release_order.aspx");

     }    
    

}
