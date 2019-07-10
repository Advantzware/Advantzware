
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class gl_account_list : System.Web.UI.Page
{

   protected void Page_Load(object sender, System.EventArgs e)
    {
       
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "gl_account_list.aspx";
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
            lblUser.Text = UserLogin.UserName;
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

        


        try
        {
            if (Session["gl_account_list_index1"] == null)
            {
                GridView1.SelectedIndex = 0;

                Session["gl_account_list_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                Session["top_list_notes_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;               
                
            }
        }
        catch
        {
            
        }
        try
        {
            if (Session["gl_account_list_index1"] != null)
            {
                GridView1.SelectedIndex = Convert.ToInt32(Session["gl_account_list_index1"]);

                Session["gl_account_list_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                Session["top_list_notes_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;                
                
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
        Session["gl_account_list_index1"] = GridView1.SelectedIndex;

        Session["gl_account_list_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
        Session["top_list_notes_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;

       
    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        Session["gl_account_list_prmacc"] = act_TextBox.Text;
        Session["gl_account_list_prmactdscr"] = dscr_TextBox.Text.Trim();
        Session["gl_account_list_prmactype"] = type_TextBox.Text.Trim();
       
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmact"].DefaultValue = act_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmactdscr"].DefaultValue = dscr_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmactype"].DefaultValue = type_TextBox.Text.Trim();

        Session["gl_account_list_index1"] = null;
    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        act_TextBox.Text = "";
        dscr_TextBox.Text = "";
        type_TextBox.Text = "";

        Session["gl_account_list_prmacc"] = act_TextBox.Text;
        Session["gl_account_list_prmactdscr"] = dscr_TextBox.Text;
        Session["gl_account_list_prmactype"] = type_TextBox.Text.Trim();
        

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmact"].DefaultValue = act_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmactdscr"].DefaultValue = dscr_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmactype"].DefaultValue = type_TextBox.Text.Trim();

        Session["gl_account_list_index1"] = null;
    }
    protected void lnk_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_gl_account.aspx");
    }
    protected void load_viewjournals_Click(object sender, EventArgs e)
    {
        Response.Redirect("distribute_account.aspx");
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
         Session["gl_account_list_add_button"] = "add";
         Response.Redirect("view_gl_account.aspx");

     }
    

}
