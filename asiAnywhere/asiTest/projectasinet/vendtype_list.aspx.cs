
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class vendtype_list : System.Web.UI.Page
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
            string vPage = "vendtype_list.aspx";
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
                if (Session["vendtype_prmvtype"] != null)
                    vendtype_TextBox.Text = Convert.ToString(Session["vendtype_prmvtype"]);
                if (Session["vendtype_list_prmdscr"] != null)
                    dscr_TextBox.Text = Convert.ToString(Session["vendtype_list_prmdscr"]);
                
               

            }
                       

        } //  ! Page.IsPostBack


        
        Session["Rowuser"] = UserLogin.UserName;

        try
        {
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch { }
        
        try
        {
            if (Session["vendtype_code_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["vendtype_new_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reckeyLabel")).Text;
            }
        }
        catch
        {

        }
        try
        {
            if (Session["vendtype_code_index"] != null)
            {
                GridView1.SelectedIndex = Convert.ToInt32(Session["vendtype_code_index"]);

                Session["vendtype_new_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reckeyLabel")).Text;
            }
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
        Session["vendtype_code_index"] = GridView1.SelectedIndex;

        Session["vendtype_new_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reckeyLabel")).Text;

    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "GridSearch";
        ObjectDataSource1.SelectParameters["prmvtype"].DefaultValue = vendtype_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmvdscr"].DefaultValue = vendtype_TextBox.Text.Trim();
        Session["vendtype_prmvtype"] = vendtype_TextBox.Text;
        Session["vendtype_list_prmdscr"] = dscr_TextBox.Text;        
    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        vendtype_TextBox.Text = "";
        dscr_TextBox.Text = "";

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "GridSelect";
        ObjectDataSource1.SelectParameters["prmvtype"].DefaultValue = vendtype_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmvdscr"].DefaultValue = dscr_TextBox.Text.Trim();


        Session["vendtype_prmvtype"] = vendtype_TextBox.Text;
        Session["vendtype_list_prmdscr"] = dscr_TextBox.Text;
        

        
    }
    protected void lnk_viewvend_Click(object sender, EventArgs e)
    {
        Response.Redirect("vendtype_viewlist.aspx");
    }
    
    
    protected void img_btn_add_click(object sender, EventArgs e)
    {
        Session["vend_type_add_new_vend"] = "addnewvend";
        Response.Redirect("vendtype_viewlist.aspx");
    }
    

}
