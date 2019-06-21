
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class payterm_list : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        //Session["customer_list_soldto"] = null;
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "payterm_list.aspx";
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
                //    UserClass UserLogin = (UserClass)Session["User"]; 
                lblUser.Text = UserLogin.UserName;
                Session["customer_user_id"] = UserLogin.UserName;
                if (Session["payterms_list_prmterms"] != null)
                    terms_TextBox.Text = Convert.ToString(Session["payterms_list_prmterms"]);
                if (Session["payterms_list_prmname"] != null)
                    name_TextBox.Text = Convert.ToString(Session["payterms_list_prmname"]);
                               

            }
                       

        } //  ! Page.IsPostBack


        try
        {
            if (Session["payterms_list_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["payterms_list_vend_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reckeyLabel")).Text;
            }
        }
        catch
        {
            
        }
        try
        {
            if (Session["payterms_list_index"] != null)
            {
                GridView1.SelectedIndex = Convert.ToInt32(Session["payterms_list_index"]);

                Session["payterms_list_vend_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reckeyLabel")).Text;
            }
        }
        catch
        {
            
        }
        Session["Rowuser"] = UserLogin.UserName;

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

    }


    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["payterms_list_index"] = GridView1.SelectedIndex;

        Session["payterms_list_vend_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reckeyLabel")).Text;

    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmtcode"].DefaultValue = terms_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdscr"].DefaultValue = name_TextBox.Text.Trim();
        

        Session["payterms_list_prmterms"] = terms_TextBox.Text;
        Session["payterms_list_prmname"] = name_TextBox.Text;
      


        Session["payterms_list_index"] = null;
        Session["payterms_list_vend_reckey"] = null;
    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        terms_TextBox.Text = "";
        name_TextBox.Text = "";
        
        
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmtcode"].DefaultValue = terms_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdscr"].DefaultValue = name_TextBox.Text.Trim();
        

        Session["payterms_list_prmterms"] = terms_TextBox.Text;
        Session["payterms_list_prmname"] = name_TextBox.Text;
       


        Session["payterms_list_index"] = null;
        Session["payterms_list_vend_reckey"] = null;
    }
    protected void lnk_viewvend_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_payterm.aspx");
    }
    
   
    protected void img_btn_add_click(object sender, EventArgs e)
    {
        Session["payterms_list_add_new_payterms"] = "addpayterms";
        Response.Redirect("view_payterm.aspx");
    }
    

}
