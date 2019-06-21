
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class ReleaseOrd_list : System.Web.UI.Page
{

   protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
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
                lblUser.Text = UserLogin.UserName;
                if (Convert.ToString(Session["Release_ord_posting"]) == "Yes")
                    CheckBox1.Checked = true;
                else
                {
                    CheckBox1.Checked = false;
                    Session["Release_ord_posting"] = "No";
                }
                
                ObjectDataSource1.SelectParameters["prmPosted"].DefaultValue = Convert.ToString(Session["Release_ord_posting"]);

            }


        } //  ! Page.IsPostBack


        try
        {
            if (Session["Release_ord_index1"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["Release_ord_reckey_rec"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                Session["Release_ord_r_no"] = ((Label)GridView1.SelectedRow.FindControl("Label1")).Text; 
                
            }
        }
        catch
        {
            
        }
        try
        {
            if (Session["Release_ord_index1"] != null)
            {
                GridView1.SelectedIndex = Convert.ToInt32(Session["Release_ord_index1"]);
                Session["Release_ord_reckey_rec"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                Session["Release_ord_r_no"] = ((Label)GridView1.SelectedRow.FindControl("Label1")).Text; 
                
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
        Session["Release_ord_index1"] = GridView1.SelectedIndex;
        Session["Release_ord_reckey_rec"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
        Session["Release_ord_r_no"] = ((Label)GridView1.SelectedRow.FindControl("Label1")).Text; 
        
    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        Session["Release_ord_rellno"] = release_TextBox.Text;
        Session["Release_ord_ordno"] = ordno_TextBox.Text.Trim();
        Session["Release_ord_pono"] = cstpo_TextBox.Text;
        Session["Release_ord_custno"] = cust_TextBox.Text;
        Session["Release_ord_ino"] = fgitm_TextBox.Text;
        Session["Release_ord_job"] = job_TextBox.Text.Trim();
        Session["Release_ord_job2"] = job2_TextBox.Text;
        
        string vpost = "No";
        if (CheckBox1.Checked)
            vpost = "Yes";
        else
            vpost = "No";
        Session["Release_ord_posting"] = vpost;
        
       
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmrellno"].DefaultValue = release_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmordno"].DefaultValue = ordno_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcustno"].DefaultValue = cust_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmino"].DefaultValue = fgitm_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpono"].DefaultValue = cstpo_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmjobno"].DefaultValue = job_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmjobno2"].DefaultValue = job2_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPosted"].DefaultValue = vpost;

        Session["Release_ord_index1"] = null;
    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        release_TextBox.Text = "";
        ordno_TextBox.Text = "";
        cstpo_TextBox.Text = "";
        cust_TextBox.Text = "";
        fgitm_TextBox.Text = "";
        job_TextBox.Text = "";
        job2_TextBox.Text = "";

        Session["Release_ord_rellno"] = release_TextBox.Text;
        Session["Release_ord_ordno"] = ordno_TextBox.Text.Trim();
        Session["Release_ord_pono"] = cstpo_TextBox.Text;
        Session["Release_ord_custno"] = cust_TextBox.Text;
        Session["Release_ord_ino"] = fgitm_TextBox.Text;
        Session["Release_ord_job"] = job_TextBox.Text.Trim();
        Session["Release_ord_job2"] = job2_TextBox.Text;
        string vpost = "No";
        if (CheckBox1.Checked)
            vpost = "Yes";
        else
            vpost = "No";
        Session["Release_ord_posting"] = vpost;

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmrellno"].DefaultValue = release_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmordno"].DefaultValue = ordno_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcustno"].DefaultValue = cust_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmino"].DefaultValue = fgitm_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpono"].DefaultValue = cstpo_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmjobno"].DefaultValue = job_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmjobno2"].DefaultValue = job2_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPosted"].DefaultValue = vpost;
        
    }
    protected void lnk_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_release_order.aspx");
    }

    protected void load_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("rel_ord_shipnote.aspx");
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
