
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class view_gl_inquiry : System.Web.UI.Page
{

   protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            lblUser.Text = UserLogin.UserName;
        }

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "gl_inquiry.aspx";
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




        try
        {
            if (Session["gl_viewinquiry_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["top_glview_inquiry_reckey"] = ((Label)GridView1.SelectedRow.FindControl("invoiceLabel")).Text;
                
            }
        }
        catch
        {
            
        }
        try
        {
            if (Session["gl_viewinquiry_index"] != null)
            {
                GridView1.SelectedIndex = Convert.ToInt32(Session["gl_viewinquiry_index"]);
                Session["top_glview_inquiry_reckey"] = ((Label)GridView1.SelectedRow.FindControl("invoiceLabel")).Text;

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
        /*year_TextBox.Text = "0";
        opbal_TextBox.Text = "0.00";
        clbal_TextBox.Text = "?";
        bepdrn_TextBox.Text = "0";
        endpdrn_TextBox.Text = "12";*/


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

    }


    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["gl_viewinquiry_index"] = GridView1.SelectedIndex;
        Session["top_glview_inquiry_reckey"] = ((Label)GridView1.SelectedRow.FindControl("invoiceLabel")).Text;
    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
               
       
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
       
    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
       
    }

    protected void lnk_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_gl_inquiry.aspx");
    }
    protected void load_viewjournals_Click(object sender, EventArgs e)
    {
        Response.Redirect("load_recr_journal.aspx");
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

     

     protected void FormView2_PreRender(object sender, EventArgs e)
     {
         
     }
     protected void inl_list_inquiry(object sender, EventArgs e)
     {
         Response.Redirect("gl_inquiry.aspx");
     }

    
    

}
